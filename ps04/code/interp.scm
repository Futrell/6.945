;;; Core of extended Scheme interpreter
;;;
;;; See also structure and predicate definitions in rtdata.scm and
;;; syntax.scm

(declare (usual-integrations eval apply))

(define (default-eval expression environment)
  (cond ((application? expression)
	 (apply (eval (operator expression) environment)
		(operands expression)
		environment))
	(else
	 (error "Unknown expression type" expression))))

(define (default-apply procedure operands calling-environment)
  (error "Unknown procedure type" procedure))

(define eval
  (make-generic-operator 2 'eval default-eval))

(defhandler eval
  (lambda (expression environment) expression)
  self-evaluating?)

(defhandler eval lookup-variable-value variable?)

(defhandler eval
  (lambda (expression environment)
    (text-of-quotation expression))
  quoted?)

(defhandler eval
  (lambda (expression environment)
    (make-compound-procedure
     (lambda-parameters expression)
     (lambda-body expression)
     environment))
  lambda?)

(defhandler eval
  (lambda (expression environment)
    (if (eval (if-predicate expression) environment)
	(eval (if-consequent expression) environment)
	(eval (if-alternative expression) environment)))
  if?)

(defhandler eval
  (lambda (expression environment)
    (eval (cond->if expression) environment))
  cond?)

(defhandler eval
  (lambda (expression environment)
    (eval (let->combination expression) environment))
  let?)

(defhandler eval
  (lambda (expression environment)
    (evaluate-sequence (begin-actions expression)
		       environment))
  begin?)

(define (evaluate-sequence actions environment)
  (cond ((null? actions)
	 (error "Empty sequence"))
	((null? (rest-exps actions))
	 (eval (first-exp actions) environment))
	(else
	 (eval (first-exp actions) environment)
	 (evaluate-sequence (rest-exps actions) environment))))

(defhandler eval
  (lambda (expression environment)
    (define-variable! (definition-variable expression)
      (eval (definition-value expression) environment)
      environment)
    (definition-variable expression))
  definition?)

(defhandler eval
  (lambda (expression environment)
    (set-variable-value! (assignment-variable expression)
      (eval (assignment-value expression) environment)
      environment))
  assignment?)

(define apply
  (make-generic-operator 3 'apply default-apply))

(defhandler apply
  (lambda (procedure operands calling-environment)
    (define (evaluate-list operands)
      (cond ((null? operands) '())
	    ((null? (rest-operands operands))
	     (list (eval (first-operand operands)
			 calling-environment)))
	    (else
	     (cons (eval (first-operand operands)
			 calling-environment)
		   (evaluate-list (rest-operands operands))))))
    (display "Now applying ")
    (display procedure)
    (display " to ")
    (display operands)
    (newline)
    (apply-primitive-procedure procedure
      (evaluate-list operands)))
  strict-primitive-procedure?)

(defhandler apply
  (lambda (procedure operands calling-environment)
    (display "Now applying ")
    (display procedure)
    (display " to ")
    (display operands)
    (newline)
    (if (not (= (length (procedure-parameters procedure))
		(length operands)))
	(error "Wrong number of operands supplied"))
    (let ((arguments
	   (map (lambda (parameter operand)
		  (evaluate-procedure-operand parameter
					      operand
					      calling-environment))
		(procedure-parameters procedure)
		operands)))
      (eval (procedure-body procedure)
	    (extend-environment
	     (map procedure-parameter-name
		  (procedure-parameters procedure))
	     arguments
	     (procedure-environment procedure)))))
  compound-procedure?)

(define evaluate-procedure-operand
  (make-generic-operator 3
			 'evaluate-operand
			 (lambda (parameter operand environment)
			   (eval operand environment))))

(define procedure-parameter-name
  (make-generic-operator 1 'parameter-name (lambda (x) x)))
