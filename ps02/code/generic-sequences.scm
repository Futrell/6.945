;;;;    Generic sequence operator definitions

;;; First we declare the operators we want to be generic.
;;;  Each declaration specifies the arity (number of arguments)
;;;  and the default operation, if necessary.

(define sequence:null
  (make-generic-operator 1))

(define sequence:ref
  (make-generic-operator 2))

(define sequence:size
  (make-generic-operator 1))

(define sequence:type
  (make-generic-operator 1))

(define sequence:null?
  (make-generic-operator 1))

(define sequence:equal?
  (make-generic-operator 2))

(define sequence:set!
  (make-generic-operator 3))

(define sequence:subsequence
  (make-generic-operator 3))


;;; sequence:append takes multiple arguments.  It is defined in terms
;;; of a generic binary append that takes two sequences.

(define (sequence:append . sequences)
  (if (null? sequences)
      (error "Need at least one sequence for append"))
  (let ((type? (sequence:type (car sequences))))
    (if (not (for-all? (cdr sequences) type?))
	(error "All sequences for append must be of the same type"
	       sequences))
    (fold-right generic:binary-append
		(sequence:null (sequence:type (car sequences)))
		sequences)))

(define generic:binary-append (make-generic-operator 2))

;;; Implementations of the generic operators.

(define (any? x) #t)
(define (constant val) (lambda (x) val))
(define (is-exactly val) (lambda (x) (eq? x val)))

(defhandler sequence:null (constant "")    (is-exactly string?))
(defhandler sequence:null (constant '())   (is-exactly list?))
(defhandler sequence:null (constant #())   (is-exactly vector?))

(defhandler sequence:ref string-ref         string? exact-integer?)
(defhandler sequence:ref list-ref           list?   exact-integer?)
(defhandler sequence:ref vector-ref         vector? exact-integer?)

(defhandler sequence:size string-length     string?)
(defhandler sequence:size length            list?)
(defhandler sequence:size vector-length     vector?)

(defhandler sequence:type (constant string?)     string?)
(defhandler sequence:type (constant list?)       list?)
(defhandler sequence:type (constant vector?)     vector?)


(define (vector-null? v) (= (vector-length v) 0))

(defhandler sequence:null? string-null?     string?)
(defhandler sequence:null? null?            list?)
(defhandler sequence:null? vector-null?     vector?)


;;; To assign to the ith element of a list:

(define (list-set! list i val)
  (cond ((null? list)
	 (error "List does not have enough elements" i))
	((= i 0) (set-car! list val))
	(else (list-set! (cdr list) (- i 1) val))))

(defhandler sequence:set! string-set!   string? exact-integer? any?)
(defhandler sequence:set! list-set!     list?   exact-integer? any?)
(defhandler sequence:set! vector-set!   vector? exact-integer? any?)


(defhandler sequence:subsequence
		  substring
		  string?  exact-integer?  exact-integer?)
(defhandler sequence:subsequence
		  sublist
		  list?  exact-integer?  exact-integer?)
(defhandler sequence:subsequence
		  subvector
		  vector?  exact-integer?  exact-integer?)


(define (vector-append v1 v2)
  (let ((n1 (vector-length v1))
	(n2 (vector-length v2)))
    (make-initialized-vector (+ n1 n2)
			     (lambda (i)
			       (if (< i n1)
				   (vector-ref v1 i)
				   (vector-ref v2 (- i n1)))))))

(defhandler generic:binary-append string-append  string? string?)
(defhandler generic:binary-append append         list?   list?)
(defhandler generic:binary-append vector-append  vector? vector?)
