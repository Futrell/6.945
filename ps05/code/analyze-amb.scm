;;;; Analyzing interpreter with AMB.
;;;   Execution procedures take environment 
;;;   and two continuations: SUCCEED and FAIL

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define analyze
  (make-generic-operator 1 'analyze
    (lambda (exp)
      (cond ((application? exp) (analyze-application exp))
	    (else (error "Unknown expression type" exp))))))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(defhandler analyze analyze-self-evaluating self-evaluating?)


(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(defhandler analyze analyze-quoted quoted?)


(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(defhandler analyze analyze-variable variable?)

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-compound-procedure vars bproc env)
	       fail))))

(defhandler analyze analyze-lambda lambda?)


(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
	     (lambda (pred-value pred-fail)
	       (if (true? pred-value)
		   (cproc env succeed pred-fail)
		   (aproc env succeed pred-fail)))
	     fail))))

(defhandler analyze analyze-if if?)

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env succeed fail)
      (proc1 env
	     (lambda (proc1-value proc1-fail)
	       (proc2 env succeed proc1-fail))
	     fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (if (null? exps) (error "Empty sequence"))
  (let ((procs (map analyze exps)))
    (loop (car procs) (cdr procs))))

(defhandler analyze
  (lambda (exp)
    (analyze-sequence (begin-actions exp)))
  begin?)

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
	     (lambda (proc proc-fail)
	       (get-args aprocs env
			 (lambda (args args-fail)
			   (execute-application proc
						args
						succeed
						args-fail))
			 proc-fail))
	     fail))))

(define (get-args aprocs env succeed fail)
  (cond ((null? aprocs) (succeed '() fail))
	((null? (cdr aprocs))
	 ((car aprocs) env
	  (lambda (arg fail)
	    (succeed (list arg) fail))
	  fail))
	(else
	 ((car aprocs) env
	  (lambda (arg fail)
	    (get-args (cdr aprocs) env
		      (lambda (args fail)
			(succeed (cons arg args)
				 fail))
		      fail))
	  fail))))

(define execute-application
  (make-generic-operator 4 'execute-application
    (lambda (proc args succeed fail)
      (error "Unknown procedure type" proc))))

(defhandler execute-application
  (lambda (proc args succeed fail)
    (succeed (apply-primitive-procedure proc args) fail))
  strict-primitive-procedure?)

(defhandler execute-application
  (lambda (proc args succeed fail)
    ((procedure-body proc)
     (extend-environment (procedure-parameters proc)
			 args
			 (procedure-environment proc))
     succeed
     fail))
  compound-procedure?)

;;; There are two useful kinds of assignments in AMB
;;; interpreters.  Undoable assignments and permanent
;;; assignments.  The default is the undoable assignment.

(define (analyze-undoable-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
	     (lambda (new-val val-fail)
	       (let ((old-val (lookup-variable-value var env)))
		 (set-variable-value! var new-val env)
		 (succeed 'OK
		   (lambda ()
		     (set-variable-value! var old-val env)
		     (val-fail)))))
	     fail))))

(defhandler analyze
  analyze-undoable-assignment
  assignment?)

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
	     (lambda (new-val val-fail)
	       (define-variable! var new-val env)
	       (succeed var val-fail))
	     fail))))

(defhandler analyze analyze-definition definition?)


;;; AMB, itself!

(define (analyze-amb exp)
  (let ((aprocs (map analyze (amb-alternatives exp))))
    (lambda (env succeed fail)
      (let loop ((alts aprocs))
	(if (null? alts)
	    (fail)
	    ((car alts) env
	                succeed
			(lambda ()
			  (loop (cdr alts)))))))))

(defhandler analyze analyze-amb amb?)


;;; Macros (definitions are in syntax.scm)

(defhandler analyze
  (lambda (exp)
    (analyze (cond->if exp)))
  cond?)

(defhandler analyze
  (lambda (exp)
    (analyze (let->combination exp)))
  let?)
