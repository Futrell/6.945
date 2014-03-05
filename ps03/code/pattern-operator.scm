(define (make-pattern-operator #!optional rules)
  (define (operator self . arguments)
    (define (succeed value fail) value)
    (define (fail)
      (error "No applicable operations" self arguments))
    (try-rules arguments (entity-extra self) succeed fail))
  (make-entity operator (if (default-object? rules) '() rules)))

(define (attach-rule! operator rule)
  (set-entity-extra! operator
   (cons rule (entity-extra operator))))

;;; Pattern-directed factorial, with and without the rule macro.

#|
(define factorial (make-pattern-operator))

(attach-rule! factorial (rule '(0) 1))

(attach-rule! factorial
 (rule `((? n ,positive?))
       (* n (factorial (- n 1)))))

(factorial 10)
;Value: 3628800
|#

#|
(define factorial (make-pattern-operator))

(attach-rule! factorial
 (make-rule '((? n))
  (lambda (n) (* n (factorial (- n 1))))))

(attach-rule! factorial
 (make-rule '(0) (lambda () 1)))

(factorial 10)
;Value: 3628800
|#

#|
 (define quad
   (make-pattern-operator
    (list
     (rule 
      `((? a) (? b) (? c) (? x))
      (+ (* a (expt x 2))
	 (* b x)
	 c))

     (rule
      `((? a) (? x) (? x) + (? b) (? x) + (? c))
      (+ (* a (expt x 2))
	 (* b x)
	 c)))))

 (quad 1 2 3 4)
 ;Value: 27

 (quad 1 4 4 '+ 2 4 '+ 3)
 ;Value: 27
|#

#|
 (define frob
   (make-pattern-operator))

 (attach-rule! frob
  (rule
   '(a (?? x) (?? y) (?? x) c)
   (and (<= (length y) 2)
        y)))

 (apply frob '(a b b b b b b c))
 ;Value: (b b)
|#
