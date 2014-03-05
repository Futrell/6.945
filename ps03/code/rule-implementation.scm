(declare (usual-integrations))

;;; A rule is a procedure constructed from a pattern and a consequent
;;; procedure.  A rule takes data that the rule may apply to and a
;;; success and failure continuation.  The consequent procedure binds
;;; the variables named in the pattern.  It produces the result of the
;;; rule as a function of the values of those variables that matched
;;; the data presented.

(define (make-rule pattern consequent)
  (let ((match-procedure (match:->combinators pattern))
	(bound-variables (procedure-bound-variables consequent)))
    (define (the-rule data succeed fail)
      (or (match-procedure (list data) '()
 	    ;; A matcher-success continuation
	    (lambda (dict n)
	      (define (matched-value name)
		(match:value
		 (or (match:lookup name dict)
		     (error "Handler asked for unknown name"
			    name dict))))
	      (and (= n 1)
		   (let ((result
			  (apply consequent
				 (map matched-value
				      bound-variables))))
		     (and result
			  (succeed result
				   (lambda () #f)))))))
	  (fail)))
    the-rule))

(define-syntax rule
  (sc-macro-transformer
   (lambda (form use-env)
     (let ((pattern (cadr form))
	   (handler-body (caddr form)))
       `(make-rule 
	 ,(close-syntax pattern use-env)
	 ,(compile-handler handler-body use-env
			   (match:pattern-names pattern)))))))

(define (compile-handler form env names)
  ;; See magic in utils.scm
  (make-lambda names env
    (lambda (env*) (close-syntax form env*))))

#|
(pp (syntax '(rule '(* (? a) (? b))
		   (and (expr<? a b)
			`(* ,a ,b)))
	    (the-environment)))

; (make-rule '(* (? a) (? b))
;  (lambda (b a)
;    (and (expr<? a b)
;         (list '* a b))))
;Unspecified return value
|#
