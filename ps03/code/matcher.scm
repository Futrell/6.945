;;;; Matcher based on match combinators, CPH/GJS style.
;;;     Idea is in Hewitt's PhD thesis (1969).

(declare (usual-integrations))

;;; There are match procedures that can be applied to data items.  A
;;; match procedure either accepts or rejects the data it is applied
;;; to.  Match procedures can be combined to apply to compound data
;;; items.

;;; A match procedure takes a list containing a data item, a
;;; dictionary, and a success continuation.  The dictionary
;;; accumulates the assignments of match variables to values found in
;;; the data.  The success continuation takes two arguments: the new
;;; dictionary, and the number of items absorbed from the list by the
;;; match.  If a match procedure fails it returns #f.

;;; Primitive match procedures:

(define (match:eqv pattern-constant)
  (define (eqv-match data dictionary succeed)
    (and (pair? data)
	 (eqv? (car data) pattern-constant)
	 (succeed dictionary 1)))
  eqv-match)

(define (match:element variable restrictions)
  (define (ok? datum)
    (every (lambda (restriction)
	     (restriction datum))
	   restrictions))
  (define (element-match data dictionary succeed)
    (and (pair? data)
	 (ok? (car data))
	 (let ((vcell (match:lookup variable dictionary)))
	   (if vcell
	       (and (equal? (match:value vcell) (car data))
		    (succeed dictionary 1))
	       (succeed (match:bind variable
				    (car data)
				    dictionary)
			1)))))
  element-match)


;;; Support for the dictionary.

(define (match:bind variable data-object dictionary)
  (cons (list variable data-object) dictionary))

(define (match:lookup variable dictionary)
  (assq variable dictionary))

(define (match:value vcell)
  (cadr vcell))

(define (match:segment variable)
  (define (segment-match data dictionary succeed)
    (and (list? data)
	 (let ((vcell (match:lookup variable dictionary)))
	   (if vcell
	       (let lp ((data data)
			(pattern (match:value vcell))
			(n 0))
		 (cond ((pair? pattern)
			(if (and (pair? data)
				 (equal? (car data) (car pattern)))
			    (lp (cdr data) (cdr pattern) (+ n 1))
			    #f))
		       ((not (null? pattern)) #f)
		       (else (succeed dictionary n))))
	       (let ((n (length data)))
		 (let lp ((i 0))
		   (if (<= i n)
		       (or (succeed (match:bind variable
						(list-head data i)
						dictionary)
				    i)
			   (lp (+ i 1)))
		       #f)))))))
  segment-match)

(define (match:list . match-combinators)
  (define (list-match data dictionary succeed)
    (and (pair? data)
	 (let lp ((lst (car data))
		  (matchers match-combinators)
		  (dictionary dictionary))
	   (cond ((pair? matchers)
		  ((car matchers)
		   lst
		   dictionary
		   (lambda (new-dictionary n) ; gets called when car
					; matcher succeeds only
		     (if (> n (length lst))
			 (error "Matcher ate too much."
				n))
		     (lp (list-tail lst n)
			 (cdr matchers)
			 new-dictionary))))
		 ((pair? lst) #f)
		 ((null? lst)
		  (succeed dictionary 1))
		 (else #f)))))
  list-match)

;;; Syntax of matching is determined here.

(define (match:element? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '?)))

(define (match:segment? pattern)
  (and (pair? pattern)
       (eq? (car pattern) '??)))

(define (match:variable-name pattern) (cadr pattern))
(define (match:restrictions pattern) (cddr pattern))

(define (match:list? pattern)
  (and (list? pattern)
       (or (null? pattern)
	   (not (memq (car pattern) '(? ??))))))

(define match:->combinators
  (make-generic-operator 1 'eqv match:eqv))

(defhandler match:->combinators
  (lambda (pattern)
    (match:element
     (match:variable-name pattern)
     (match:restrictions pattern)))
  match:element?)

(defhandler match:->combinators
  (lambda (pattern) (match:segment (match:variable-name pattern)))
  match:segment?)

(defhandler match:->combinators
  (lambda (pattern)
    (apply match:list (map match:->combinators pattern)))
  match:list?)

(define (matcher pattern)
  (let ((match-combinator (match:->combinators pattern)))
    (lambda (datum)
      (match-combinator (list datum)
			'()
			(lambda (dictionary n)
			  (and (= n 1)
			       dictionary))))))

#|
(define (report-success dict n)
  (assert (= n 1))
  `(succeed ,dict))

((match:->combinators '(a ((? b) 2 3) 1 c))
 '((a (1 2 3) 1 c))
 '()
  report-success)
;Value: (succeed ((b 1)))

((match:->combinators '(a ((? b) 2 3) (? b) c))
 '((a (1 2 3) 2 c))
 '()
  report-success)
;Value: #f

((match:->combinators '(a ((? b) 2 3) (? b) c))
 '((a (1 2 3) 1 c))
 '()
  report-success)
;Value: (succeed ((b 1)))


((match:->combinators '(a (?? x) (?? y) (?? x) c))
 '((a b b b b b b c))
 '()
 (lambda (dict n)
   (assert (= n 1))
   (pp `(succeed ,dict))
   #f))
(succeed ((y (b b b b b b)) (x ())))
(succeed ((y (b b b b)) (x (b))))
(succeed ((y (b b)) (x (b b))))
(succeed ((y ()) (x (b b b))))
;Value: #f

((matcher '(a ((? b) 2 3) (? b) c))
 '(a (1 2 3) 1 c))
;Value: ((b 1))
|#

;;; Nice pattern inspection procedure that will be used by the
;;; pattern-directed invocation system.

(define (match:pattern-names pattern)
  (let loop ((pattern pattern) (names '()))
    (cond ((or (match:element? pattern)
               (match:segment? pattern))
           (let ((name
		  (match:variable-name pattern)))
             (if (memq name names)
                 names
                 (cons name names))))
          ((list? pattern)
           (let elt-loop
	       ((elts pattern) (names names))
             (if (pair? elts)
                 (elt-loop (cdr elts)
			   (loop (car elts) names))
                 names)))
          (else names))))

#|
 (match:pattern-names '((? a) (?? b)))
 ;Value: (b a)
|#
