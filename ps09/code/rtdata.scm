;;; -*- Mode:Scheme -*- 

(declare (usual-integrations))

(define the-unspecified-value (list 'the-unspecified-value))

(define (true? x)
  (if x true false))

(define (false? x)
  (if x false true))

;;; Primitive procedures are inherited from Scheme.

(define strict-primitive-procedure? procedure?)
(define apply-primitive-procedure apply)


;;; Compound procedures
#|
More information on MIT scheme records at:
http://groups.csail.mit.edu/mac/projects/scheme/documentation/scheme_11.html#SEC110

(define-record-type <record-name>
    (<record-constructor> <fieldname0> ...)
    <predicate>
  (<fieldname0> <fieldname0-accessor>)
  ...)
|#

(define-record-type compound-procedure
    (make-compound-procedure vars bproc env)
    compound-procedure?
  (vars  procedure-parameters)
  (bproc procedure-body)
  (env   procedure-environment))

(define-record-type actor-procedure
    (make-actor-procedure% vars bproc env task-queue runnable)
    actor-procedure?
  (vars  actor-parameters)
  (bproc actor-body)
  (env   actor-environment)
  (task-queue get-actor-task-queue set-actor-task-queue!)
  (runnable actor-runnable? set-actor-runnable!))

(define (make-actor-procedure vars bproc env)
  (make-actor-procedure% vars bproc env (queue:make) #f))

;;; An ENVIRONMENT is a chain of FRAMES, made of vectors.

(define (extend-environment variables values base-environment)
  (if (fix:= (length variables) (length values))
      (vector variables values base-environment)
      (if (fix:< (length variables) (length values))
	  (error "Too many arguments supplied" variables values)
	  (error "Too few arguments supplied" variables values))))

(define (environment-variables env) (vector-ref env 0))
(define (environment-values env) (vector-ref env 1))
(define (environment-parent env) (vector-ref env 2))

(define the-empty-environment (list '*the-empty-environment*))

(define (lookup-variable-value var env)
  (let plp ((env env))
    (if (eq? env the-empty-environment)
	(lookup-scheme-value var)
	(let scan
	    ((vars (vector-ref env 0))
	     (vals (vector-ref env 1)))
	  (cond ((null? vars) (plp (vector-ref env 2)))
		((eq? var (car vars)) (car vals))
		(else (scan (cdr vars) (cdr vals))))))))

;;; Extension to make underlying Scheme values available to interpreter

(define (lookup-scheme-value var)
  (lexical-reference generic-evaluation-environment var))

(define (define-variable! var val env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable -- DEFINE" var) ;should not happen.
      (let scan
	  ((vars (vector-ref env 0))
	   (vals (vector-ref env 1)))
	(cond ((null? vars)
	       (double-check-lock
		(lambda () (null? vars)) ; check that vars is STILL NULL
		(lambda () 
		  (vector-set! env 0 (cons var (vector-ref env 0)))
		  (vector-set! env 1 (cons val (vector-ref env 1)))) ; do this stuff
		(lambda ()
		  (define-variable! var val env)))) ; try again until
                                        ; it works
	      ((eq? var (car vars))
               (double-check-lock
                (lambda () (eq? var (car vars)))
                (lambda ()
		  (set-car! vals val))
                (lambda ()
		  (define-variable! var val env))))
	      (else
	       (scan (cdr vars) (cdr vals)))))))

(define (set-variable-value! var val env)
  (let plp ((env env))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(let scan
	    ((vars (vector-ref env 0))
	     (vals (vector-ref env 1)))
	  (cond ((null? vars) (plp (vector-ref env 2)))
		((eq? var (car vars))
                 (double-check-lock
                  (lambda () (eq? var (car vars)))
                  (lambda ()
		    (set-car! vals val))
                  (lambda () (plp env))))
		(else (scan (cdr vars) (cdr vals))))))))


