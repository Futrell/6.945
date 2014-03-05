;;; 6.945 Problem Set 3
;;; Richard Futrell
;;; futrell@mit.edu

(load "load.scm")

;;; Problem 3.1
;;;
;;; Forcing the success function to print the result and then return
;;; #f allows us to see all the matches for the following
;;; reason. The multiplicity of matches comes from an or expression in
;;; match:segment. The first term calls the success function; if that
;;; returns something that is true, then the or expression terminates
;;; returning that value. If the success function returns something
;;; false, then the or expression then evalates the next term, which
;;; in this case means to try matching with a longer segment. Forcing
;;; the success function to return false ensures that we will go
;;; around this loop as much as possible.

;;; Problem 3.2 
;;;

(define (match:choice . alternatives)
  (define (choice-match data dictionary succeed)
    (and (pair? data) 
	 (let loop ((matchers alternatives))
	   (and (not (null? matchers)) ; #f if out of matchers
		(or (let ((success-args
			   ((car matchers) data dictionary 
			    (lambda args args))))
		      (and success-args 
			   (let ((success-d (car success-args)))
			     (succeed success-d 1))))
		    (loop (cdr matchers)))))))
  choice-match)

(define (match:choice? pattern)
  (and (list? pattern)
       (eq? (car pattern) '?:choice)))

(define (match:list? pattern)
  (and (list? pattern)
       (or (null? pattern)
	   (not (memq (car pattern) '(? ?? ?:choice))))))

(defhandler match:->combinators 
  (lambda (pattern) 
    (apply match:list (map match:->combinators pattern)))
  match:list?)

(defhandler match:->combinators
  (lambda (pattern) 
    (apply match:choice (map match:->combinators (cdr pattern))))
  match:choice?)

#| Tests

((match:->combinators '(?:choice a b (? x) c)) '(z) '() (lambda (d n)
							  `(succeed
							    ,d)))

;Value: (succeed ((x z)))

((match:->combinators
  `((? y) (?:choice a b (? x ,string?) (? y ,symbol?) c)))
 '((z z))
 '()
 (lambda (d n) `(succeed ,d)))

;Value: (succeed ((y z)))

((match:->combinators `(?:choice b (? x ,symbol?)))
 '(b) '()
 (lambda (x n)
   (pp `(succeed ,x))
   #f))
(succeed ())
(succeed ((x b)))
;Value: #f

((matcher '((?:choice + -) (? a) (? b))) '(+ 1 2))
;Value: ((b 2) (a 1))

((matcher '((?:choice + -) (? a) (? b))) '(- 1 2))
;Value: ((b 2) (a 1))

((matcher '(?:choice)) 'b)
;Value: #f

((matcher '(?:choice (?:choice a b) c)) 'b)
;Value: ()

|# 

;;; Problem 3.3
;;;
;;; We'll store the match procedures bound by pletrec in the same
;;; dictionary that holds variable bindings. The "matcher" procedure
;;; will strip the pletrec-bound procedures out of the dictionary
;;; before giving it to the end user. 

(define pletrec-bound-matcher-sigil "*pletrec-bound-matcher:")

(define (tap x) ; useful for debugging
  (display x)
  (newline)
  x)

(define (pletrec-identifier name)
  ;; Mark the variable names of pletrec-generated matchers so they do
  ;; not interfere with the rest of the variable namespace, and so
  ;; they can be stripped out if needed.
  (symbol pletrec-bound-matcher-sigil name))

(define (pletrec-identifier? name-expr-pair)
  (and (pair? name-expr-pair)
       (let ((name (car name-expr-pair)))
	 (string-prefix? pletrec-bound-matcher-sigil (string name)))))

(define (match:ref name)
  (define (ref-match data dictionary succeed)
    (and (pair? data) 
	 (let ((match-procedure-pair (match:lookup 
				      (pletrec-identifier name)
				      dictionary)))
           (if match-procedure-pair
	       (let ((match-procedure (cadr match-procedure-pair)))
		 (match-procedure data dictionary succeed))
	       (error "?:ref referred to an unbound name.")))))
  ref-match)

(define (match:pletrec bindings body) 
  (define (bind-matchers-into-dictionary dictionary bindings)
    (define (compile-binding name-expr-pair)
      (let ((name (car name-expr-pair))
	    (expr (cadr name-expr-pair)))
	(list (pletrec-identifier name)
	      (match:->combinators expr))))
    (append (map compile-binding bindings) dictionary))
  (define (pletrec-match data dictionary succeed)
    (let ((new-dictionary
	   (bind-matchers-into-dictionary dictionary bindings)))
      ((match:->combinators body) data new-dictionary succeed)))
  pletrec-match)

(define (match:pletrec? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '?:pletrec)))

(define (match:ref? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '?:ref)))

(define (match:list? pattern) ; Ugh!
  (and (list? pattern)
       (or (null? pattern)
           (not (memq (car pattern) '(?
				      ?? 
				      ?:choice
				      ?:pletrec
				      ?:ref))))))

(defhandler match:->combinators
  (lambda (pattern)
    (apply match:list (map match:->combinators pattern)))
  match:list?)

(defhandler match:->combinators 
  (lambda (pattern)
    (apply match:pletrec (cdr pattern)))
  match:pletrec?)

(defhandler match:->combinators 
  (lambda (pattern)
    (apply match:ref (cdr pattern)))
  match:ref?)

(define (remove-pletrec-matchers-from-dictionary dictionary)
  (filter (lambda (name-expr-pair) 
	    (not (pletrec-identifier? name-expr-pair)))
	  dictionary))

(define (matcher pattern) ; redefine so it only returns bound
			  ; variables, not pletrec-bound stuff
  (let ((match-combinator (match:->combinators pattern)))
    (lambda (datum)
      (match-combinator (list datum)
                        '()
                        (lambda (dictionary n)
                          (and (= n 1)
                               (remove-pletrec-matchers-from-dictionary
				dictionary)))))))

#| Tests

((matcher '(?:pletrec ((hello (? a))) (?:ref hello))) 'cat)
;Value: ((a cat))

((matcher '(?:pletrec ((one (? a)) (two (? b))) 
		      ((?:ref one) (?:ref two))))
 '(cat dog))
;Value: ((b dog) (a cat))

((matcher '(?:pletrec ((odd-even-etc 
			(?:choice () (1 (?:ref even-odd-etc))))
		       (even-odd-etc 
			(?:choice () (2 (?:ref odd-even-etc)))))
           (?:ref odd-even-etc))) '(1 (2 (1 (2 (1 ()))))))
;Value: ()

((matcher '(?:pletrec ((odd-even-etc
                        (?:choice () (1 (?:ref even-odd-etc))))
                       (even-odd-etc
                        (?:choice () (2 (?:ref odd-even-etc)))))
           (?:ref odd-even-etc))) '(1 (2 (2 (2 (1 ()))))))
;Value: #f

((matcher '(?:pletrec ((odd-even-etc
(?:choice () (1 (?:ref even-odd-etc))))
                       (even-odd-etc
                        (?:choice () (2 (?:ref odd-even-etc)))))
           (?:ref odd-even-etc))) '())
;Value: ()

((matcher '(?:pletrec ((cat dog)) 
		      (?:pletrec ((foo bar)) 
				 ((?:ref cat) (?:ref foo)))))
 '(dog bar))

((matcher '(?:pletrec () cat)) 'cat)
;Value: ()

|#

;;; Problem 3.4
;;;
;;; Suppose we did not check (expr<? a b). Then it would be possible
;;; to apply the commutative law over and over again without end. But
;;; if we enforce (expr<? a b), it will only apply to that a and b
;;; once; after that, (expr<? a b) will fail and it will not apply.

;;; Problem 3.5
;;;
;;; The total ordering imposed by the commutative law enforces that
;;; numerical quantities will be the leftmost in the expression, and
;;; thus adjacent to each other. So the numerical simplification rule
;;; can be expressed with a segment matcher for material following the
;;; numbers, and we don't have to use a segment matcher for material
;;; preceding the numbers, or for material between the numbers. 
;;;
;;; If we did not have the total ordering, we could express the
;;; numerical simplification rules with three segment variables
;;; before, between, and after the two numbers to be operated on. Each
;;; of those segment variables would have to try potentially many
;;; possible segmentations for each possible segmentation provided by
;;; the other segment variables. It would take a long time for this
;;; kind of rule to find a match in a long expression.

;;; Problem 3.6
;;;
;;; Instead of using the commutative law to implement a bubble sort,
;;; we can explicitly do a recursive sort of the expression after each
;;; rule application. If we use an O(NlogN) sorting algorithm we can
;;; get the expressions sorted in that time. However, we might end up
;;; applying the sorting algorithm so often that it ends up slower
;;; than the gradual O(N^2) bubble sort in the current system.

;;; Problem 3.7
;;;
;;; How to collect like terms:

(define algebra-3
  (rule-simplifier
   (list

    ;; Sums

    (rule `(+ (? a)) a)

    (rule `(+ (?? a) (+ (?? b)) (?? c))
          `(+ ,@a ,@b ,@c))

    (rule `(+ (?? a) (? y) (? x) (?? b))
          (and (expr<? x y)
               `(+ ,@a ,x ,y ,@b)))


    ;; Products

    (rule `(* (? a)) a)

    (rule `(* (?? a) (* (?? b)) (?? c))
          `(* ,@a ,@b ,@c))

    (rule `(* (?? a) (? y) (? x) (?? b))
          (and (expr<? x y)
	              `(* ,@a ,x ,y ,@b)))


    ;; Distributive law

    (rule `(* (?? a) (+ (?? b)) (?? c))
          `(+ ,@(map (lambda (x) `(* ,@a ,x ,@c)) b)))


    ;; Numerical simplifications

    (rule `(+ 0 (?? x)) `(+ ,@x))

    (rule `(+ (? x ,number?) (? y ,number?) (?? z))
          `(+ ,(+ x y) ,@z))


    (rule `(* 0 (?? x)) 0)

    (rule `(* 1 (?? x)) `(* ,@x))

    (rule `(* (? x ,number?) (? y ,number?) (?? z))
          `(* ,(* x y) ,@z))

    
    ;; Collection of like terms

    ;; ax+bx = (a+b)x if a and b are numbers 
    (rule `(+ (?? before) 
	      (* (? a ,number?) (?? x)) 
	      (?? between) ; inefficient :(
	      (* (? b ,number?) (?? x)) 
	      (?? after))
	  `(+ ,@before (* ,(+ a b) ,@x) ,@between ,@after)) 

    ;; x+x = 2x
    (rule `(+ (?? before) 
	      (? x) 
	      (? x)  ; commutative law ensures adjacency :)
	      (?? after))
	  `(+ ,@before (* 2 ,x) ,@after))

    ;; x+ax = (a+1)x
    (rule `(+ (?? before) 
	      (? x) 
	      (?? between) 
	      (* (? a) (? x)) 
	      (?? after))
	  `(+ ,@before (* ,(+ a 1) ,x) ,@between ,@after))

    )))


#| Tests

(algebra-3 '(+ (* 4 x) (* 3 x)))
;Value: (* 7 x)

(algebra-3 '(+ (* 4 y) (* 9 x) (* 3 y)))
;Value: (+ (* 7 y) (* 9 x))

(algebra-3 '(+ (* 2 x y) (* 3 x y)))
;Value: (* 5 x y)

(algebra-3 '(+ (* 2 x y) (* 3 x y)))
;Value: (* 5 x y)

(algebra-3 '(+ x x x x x))
;Value: (* 5 x)

(algebra-3 '(+ y (* x -2 w) (* x 4 y) (* w x) z (* 5 z) (* x w) (* x y 3)))
;Value: (+ y (* 6 z) (* 7 x y))

(algebra-3 '(+ (* a (+ x y)) (* a (+ y x))))
;Value: (+ (* 2 a x) (* 2 a y))

(algebra-3 '(+ x y (* x 3)))
;Value: (+ y (* 4 x))

(algebra-3 '(+ (* x 6) (* x 5) (* 4 y)))
;Value: (+ (* 4 y) (* 11 x))

|#
