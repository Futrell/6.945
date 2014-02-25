(load "load.scm")

;;; Problem 2.1
;;;
;;; First we'll write some stuff that will be useful later. 
;;; Most usefully, we want to have a procedure that takes a procedure
;;; and modifies it so that it takes any sequence, turns it into a
;;; list and operates on it, then returns a sequence of the same type
;;; that was passed in.

(define sequence:as-list (make-generic-operator 1 'as-list))

(defhandler sequence:as-list string->list string?)
(defhandler sequence:as-list (lambda (x) x) list?)
(defhandler sequence:as-list vector->list vector?)

(define (list-replace lst index replacement)
  ;; Replace the item at index in lst with replacement.
  (if (null? lst)
      lst
      (if (eq? index 0) ; 0-indexing
	  (cons replacement (cdr lst))
	  (cons (car lst) 
		(list-replace (cdr lst)
			      (- index 1)
			      replacement)))))

(define (get-sequences-from-args args arg-index-of-sequence)
  ;; Extract sequences from arguments to a procedure. 
  ;; If arg-index-of-sequence is 'all-but-first, take all arguments
  ;; except the first. (This is useful for map, for-each, and filter).
  (if (eq? arg-index-of-sequence 'all-but-first)
      (cdr args)
      (list (list-ref args arg-index-of-sequence))))

(define (to-list-and-back procedure arg-index-of-sequence)
  ;; Convert the sequence(s) in position arg-index-of-sequence to a list,
  ;; perform the procedure on it, then convert it back to the type of
  ;; the first sequence. If there are multiple sequences in the
  ;; arguments to procedure, you can pass in 'all-but-first as
  ;; arg-index-of-sequence.
  (lambda args
    (let* ((sequences (get-sequences-from-args args
					       arg-index-of-sequence))
	   (sequence-type (sequence:type (car sequences)))
	   (result-as-list 
	    (apply
	     (operate-on-list procedure arg-index-of-sequence) args)
	    ))
      (apply sequence:construct (cons sequence-type
				      result-as-list)))))

(define (operate-on-list procedure arg-index-of-sequence)
  ;; Convert the sequence(s) in position arg-index-of-sequence to
  ;; list(s), perform procedure on them, and return the result, which
  ;; might be of any type.
  (define (replace-sequences-with-lists args arg-index-of-sequence)
    (let* ((sequences (get-sequences-from-args args
					       arg-index-of-sequence))
	   (sequences-as-lists (map sequence:as-list sequences)))
      (if (eq? arg-index-of-sequence 'all-but-first)
	  (cons (car args) sequences-as-lists)
	  (let ((sequence-as-list (car sequences-as-lists)))
	    (list-replace args arg-index-of-sequence
			  sequence-as-list)))))
  (lambda args
    (let ((new-args (replace-sequences-with-lists args arg-index-of-sequence)))
      (apply procedure new-args))))

(define (switch-arg-order procedure indices)
  ;; Change the order of the arguments of a procedure.
  (lambda args
    (apply procedure (switch-order args indices))))

(define (switch-order lst indices)
  ;; Change the order of items in a list.
  (if (null? indices)
      '()
      (cons (list-ref lst (car indices))
	    (switch-order lst (cdr indices)))))

#| Tests

(switch-order '(a b c) '(2 1 0))
;Value: (c b a)

(switch-order '(a b c) '(1 2 0))
;Value: (b c a)

(define new-filter (switch-arg-order filter '(1 0)))
(new-filter '(1 2 3 4 5) (lambda (x) (> x 3)))
;Value: (4 5)

(list-replace '(a b c d) 2 "hi")
;Value: (a b "hi" d)

|#

;;; Now we can start filling in the specs.

;;; sequence:construct

(define *type-constructor-table*
  (make-eq-hash-table))

(define (add-type-constructor name constructor)
  (hash-table/put! *type-constructor-table* name constructor))

(define (get-type-constructor x)
  (hash-table/get *type-constructor-table* x #f))

(add-type-constructor list? list)
(add-type-constructor list list)
(add-type-constructor 'list list)
(add-type-constructor vector? vector)
(add-type-constructor vector vector)
(add-type-constructor 'vector vector)
(add-type-constructor string? string)
(add-type-constructor string string)
(add-type-constructor 'string string)

(define (sequence:construct sequence-type . args)
  (let ((constructor (get-type-constructor sequence-type)))
    (if (not constructor)
	(error "Unknown type passed to sequence:construct")
	(apply constructor args))))

#| Tests

(sequence:construct list? 1 2 3 4 5)
;Value: (1 2 3 4 5)

(sequence:construct 'vector 1 2 3 4 5)
;Value: #(1 2 3 4 5)

(sequence:construct string 'a 'b 'c 'd)
;Value: "abcd"

|#

;;; sequence:equal?

(define (sequence:equal? s1 s2)
  (if (sequence:same-size? s1 s2)
      (fold-right (lambda (x y) (and x y)) 
		  #t
		  (map (lambda (x y) (equal? x y))
		       (sequence:as-list s1)
		       (sequence:as-list s2)))
      #f))


#| Tests

(sequence:equal? '(1 2 3) '(1 2 3))
;Value: #t

(sequence:equal? '(1 2 3) '(1 2 3 4))
;Value: #f

(sequence:equal? #(1 2 3) #(1 2 3))
;Value: #t

(sequence:equal? #(1 2 3) #(1 2 3 4))
;Value: #f

(sequence:equal? "abc" "abc")
;Value: #t

(sequence:equal? "abc" "abcd")
;Value: #f

(sequence:equal? '(1 2 3) #(1 2 3))
;Value: #t

(sequence:equal? "cat" #(#\c #\a #\t))
;Value: #t

|# 

;;; sequence:generate

(define (range from to)
  (if (= from to)
      '()
      (cons from (range (+ from 1) to))))

(define sequence:generate 
  (make-generic-operator 3 'generate))

(define (eager-generate desired-type n proc)
  (apply sequence:construct 
	 (cons desired-type (map proc (range 0 n)))))

(defhandler sequence:generate eager-generate 
  (lambda (x) (eq? (get-type-constructor x) list)) 
  exact-integer? 
  procedure?)
(defhandler sequence:generate eager-generate 
  (lambda (x) (eq? (get-type-constructor x) vector)) 
  exact-integer? 
  procedure?)
(defhandler sequence:generate eager-generate 
  (lambda (x) (eq? (get-type-constructor x) string)) 
  exact-integer?
  procedure?)

#| Tests

(sequence:generate list 5 (lambda (x) x))
;Value: (0 1 2 3 4)

(sequence:generate list 0 (lambda (x) x))
;Value: ()

(sequence:generate vector? 5 (lambda (x) x))
;Value: #(0 1 2 3 4)

(sequence:generate 'string 5 (lambda (x) (+ x 1)))
;Value: "12345"

|# 

;;; sequence:map

(define (sequence:same-size? . sequences)
  (apply = (map sequence:size sequences)))

(define (sequence:map proc . sequences)
  (if (apply sequence:same-size? sequences)
      (apply (to-list-and-back map 'all-but-first)
	     (cons proc sequences))
      (error "Sequences must be the same size.")))

#| Tests

(sequence:map (lambda (x y) (+ x y)) '(1 2 3) '(4 5 6))
;Value: (5 7 9)

(sequence:map (lambda (x y) (+ x y)) #(1 2 3) #(4 5 6))
;Value: #(5 7 9)

(sequence:map (lambda (x) x) "abc")
;Value: "abc"

(sequence:map (lambda (x y) (+ x y)) '(1 2 3) #(4 5 6)))
;Value: (5 7 9)

|#

;;; sequence:for-each

(define (sequence:for-each proc . sequences)
  (if (apply sequence:same-size? sequences)
      (apply (operate-on-list for-each 'all-but-first)
	     (cons proc sequences))
      (error "Sequences must be the same size.")))

#| Tests

(sequence:for-each (lambda (x y) (display (list x y))) '(1 2 3) '(4 5 6))
(1 4)(2 5)(3 6)
;Unspecified return value

(sequence:for-each (lambda (x y) (display (list x y))) #(1 2 3) #(4 5 6))
(1 4)(2 5)(3 6)
;Unspecified return value

(sequence:for-each (lambda (x y) (display (list x y))) "abc" "def")
(a d)(b e)(c f)
;Unspecified return value

|#

;;; sequence:filter

(define sequence:filter
  (make-generic-operator 2 'filter))

(define generic-filter
  (to-list-and-back (switch-arg-order filter '(1 0)) 0))

(defhandler sequence:filter (switch-arg-order filter '(1 0))
  list? procedure?)
(defhandler sequence:filter generic-filter vector? procedure?)
(defhandler sequence:filter generic-filter string? procedure?)

#| Tests

(sequence:filter '(1 10 100 1000) (lambda (x) (> x 50)))
;Value: (100 1000)

(sequence:filter #(1 10 100 1000) (lambda (x) (> x 50)))
;Value: #(100 1000)

(sequence:filter "abcd" (lambda (x) (or (eqv? x #\b)
					(eqv? x #\d))))
;Value: "bd"

|#


;;; sequence:get-index

(define sequence:get-index
  (make-generic-operator 2))

(define (list-get-index sequence pred)
  (define (get-index sequence n)
    (if (null? sequence)
	#f
	(let ((first-element (car sequence)))
	  (if (pred first-element)
	      n
	      (get-index (cdr sequence) (+ n 1))))))
  (get-index sequence 0))

(define generic-get-index (operate-on-list list-get-index 0))

(defhandler sequence:get-index list-get-index list? procedure?)
(defhandler sequence:get-index generic-get-index vector? procedure?)
(defhandler sequence:get-index generic-get-index string? procedure?)

#| Tests

(sequence:get-index '(1 10 100) (lambda (x) (> x 50)))
;Value: 2

(sequence:get-index #(1 10 100) (lambda (x) (> x 50)))
;Value: 2

(sequence:get-index "abc" (lambda (x) (equal? x #\c)))
;Value: 2

|#
    
 
;;; sequence:get-element

(define (sequence:get-element sequence pred)
  (let ((filtered-sequence (sequence:filter sequence pred)))
    (if (sequence:null? filtered-sequence)
	#f
	(sequence:ref filtered-sequence 0))))


#| Tests

(sequence:get-element '(1 10 100) (lambda (x) (> x 50)))
;Value: 100

(sequence:get-element #(1 10 100) (lambda (x) (> x 50)))
;Value: 100

(sequence:get-element "abc" (lambda (x) (equal? x #\b)))
;Value: #\b

|# 


;;; sequence:fold-right and sequence:fold-left

(define sequence:fold-right
  (make-generic-operator 3))

(defhandler sequence:fold-right fold-right 
  procedure? any? list?)
(defhandler sequence:fold-right (operate-on-list fold-right 2)
  procedure? any? vector?)
(defhandler sequence:fold-right (operate-on-list fold-right 2)
  procedure? any? string?)

#| Tests

(sequence:fold-right list 'end '(a b c))
;Value: (a (b (c end)))

(sequence:fold-right list 'end #(1 2 3))
;Value: (1 (2 (3 end)))

(sequence:fold-right list 'end "abc")
;Value: (#\a (#\b (#\c end)))

|#

(define sequence:fold-left
  (make-generic-operator 3))

(defhandler sequence:fold-left fold-left 
  procedure? any? list?)
(defhandler sequence:fold-left (operate-on-list fold-left 2)
  procedure? any? vector?)
(defhandler sequence:fold-left (operate-on-list fold-left 2)
  procedure? any? string?)

#| Tests

(sequence:fold-left list 'end '(a b c))
;Value: (((end a) b) c)

(sequence:fold-left list 'end #(1 2 3))
;Value: (((end 1) 2) 3)

(sequence:fold-left list 'end "abc")
;Value: (((end #\a) #\b) #\c)

|#

;;; Problem 2.2
;;;
;;; First let's write the full set of coercions we need. 

(define sequence:as-list (make-generic-operator 1 'as-list))
(defhandler sequence:as-list (lambda (x) x) list?)
(defhandler sequence:as-list vector->list vector?)
(defhandler sequence:as-list string->list string?)

(define (string->vector str)
  (list->vector (string->list str)))

(define sequence:as-vector (make-generic-operator 1 'as-vector))
(defhandler sequence:as-vector list->vector list?)
(defhandler sequence:as-vector (lambda (x) x) vector?)
(defhandler sequence:as-vector string->vector string?)

(define (vector->string vec)
  (list->string (vector->list vec)))

(define sequence:as-string (make-generic-operator 1 'as-string))
(defhandler sequence:as-string list->string list?)
(defhandler sequence:as-string vector->string vector?)
(defhandler sequence:as-string (lambda (x) x) string?)

;;; In general, any string can be a vector or a list, but not any
;;; vector or list can be a string, which is only a sequence of
;;; characters. So in combining types vector and string, it would be
;;; wise for the output to be the more general form, since the less
;;; general one might not work. Below I will adopt the convention:
;;;                   list > vector > string
;;; When sequences of two types are mixed, the returned type will be
;;; the one to the left on the hierarchy.
;;;
;;; All we have to do to implement this for sequence:append is modify
;;; the dispatch table for binary-append. And we need to get rid of
;;; the type checking in sequence:append.
;;; TODO: Modify the specs

(define (compose-1st-arg f g)
  (lambda (x y) (f (g x) y)))

(define (compose-2nd-arg f g)
  (lambda (x y) (f x (g y))))

(define (sequence:append . sequences)
  (if (null? sequences)
      (error "Need at least one sequence for append"))
  (fold-right generic:binary-append
	      (sequence:null (sequence:type (car sequences)))
	      sequences))

(defhandler generic:binary-append 
  (compose-1st-arg append string->list)  
  string? list?)

(defhandler generic:binary-append 
  (compose-2nd-arg append string->list)  
  list? string?)

(defhandler generic:binary-append 
  (compose-1st-arg vector-append string->vector)  
  string? vector?)

(defhandler generic:binary-append 
  (compose-2nd-arg vector-append string->vector)  
  vector? string?)

(defhandler generic:binary-append 
  (compose-1st-arg append vector->list)
  vector? list?)

(defhandler generic:binary-append 
  (compose-2nd-arg append vector->list)
  list? vector?)

#| Tests

(sequence:append '(a b c) "def")
;Value: (a b c #\d #\e #\f)

(sequence:append '(a b c) #(1 2 3))
;Value: (a b c 1 2 3)

(sequence:append "abc" #(4 5 6))
;Value: #(#\a #\b #\c 4 5 6)

|# 

;;; Problem 2.3
;;;
;;; Blah blah blah about whether this is a good idea it's an OK idea. TODO

;;; If an operation takes a target type, then it should be entered
;;; into the dispatch table with incremented arity, and when the
;;; operation is called without the target type, the argument slot
;;; typically filled by the target type will be a sentinel value.

;;; We'll have to keep a registry of operations that have target
;;; types, so that defhandler knows what to do.

(define *target-type-registry*
  (make-eq-hash-table)) ; using this as a mutable set

(define (has-target-type? operator)
  (hash-table/get *target-type-registry* operator #f))

(define (make-generic-operator arity #!optional name
			       default-operation has-target-type)
  (let ((record (make-operator-record arity)))
    (define (operator . arguments)
      (if (not (or (= (length arguments) arity)
		   (and (not (default-object? has-target-type))
			(= (length arguments) (- arity 1)))))
          (error "Wrong number of arguments for generic operator"
                 (if (default-object? name) operator name)
                 arity arguments))
      (apply (or (let per-arg
                     ((tree (operator-record-tree record))
                      (args (if 
			     (and (not (default-object?
					 has-target-type))
				  (= (length arguments) (- arity 1)))
			     (cons 'default-target-type arguments)
			     arguments)))
                   (let per-pred ((tree tree))
                     (and (pair? tree)
                          (if ((caar tree) (car args))
                              (if (pair? (cdr args))
                                  (or (per-arg (cdar tree) (cdr args))
                                      (per-pred (cdr tree)))
                                  (cdar tree))
                              (per-pred (cdr tree))))))
                 (if (default-object? default-operation)
                     (lambda args
                       (error "No applicable methods for generic operator"
                              (if (default-object? name) operator
				  name)
                              args))
                     default-operation))
             arguments))

    (hash-table/put! *generic-operator-table* operator record)
    (hash-table/put! *target-type-registry* operator #t)
    operator))

;;; We don't have to modify defhandler. But if we don't, we have to
;;; change the way we define functions that have a target type. Here
;;; is how we would declare the functions that we have worked with up
;;; to now:

(define (has-default-target-type? x) (eq? x 'default-target-type))

(define generic:binary-append ; it's arity 3 now because of the target
			      ; type!
  (make-generic-operator 3 'binary-append append 'has-target-type))

(defhandler generic:binary-append append
  has-default-target-type? list? list?)
(defhandler generic:binary-append 
  (lambda args (display "In vector world") (apply append (cdr args)))
  (lambda (x) (eq? x vector)) list? list?)

#| Tests

(generic:binary-append '(1 2 3) '(4 5 6))
;Value: (1 2 3 4 5 6)

(generic:binary-append vector '(1 2 3) '(4 5 6))
In vector world
;Value: (1 2 3 4 5 6)

|# 

;;; Alternatively, we can modify def-handler so that when only two
;;; predicates are given, a first predicate 'default-target-type is
;;; inferred. This means (a) the signature in defhandler better
;;; matches the actual function calls, and (b) we don't have to
;;; tediously type has-default-target-type? for all these handlers.

(define (defhandler operator handler . argument-predicates)
  (let* ((record 
	  (let ((record (hash-table/get *generic-operator-table*
					operator 
					#f)))
	    (if record
		record
		(error "Operator not known: " operator))))
	 (arity (length argument-predicates))
	 (desired-arity (operator-record-arity record))
	 (new-argument-predicates
	  (if (= arity desired-arity)
	      argument-predicates
	      (if (and (= arity (- desired-arity 1))
		       (has-target-type? operator))
		  (cons has-default-target-type? argument-predicates)
		  (error "Incorrect operator arity:" operator)))))
    (set-operator-record-tree! record
      (bind-in-tree new-argument-predicates
                    handler
                    (operator-record-tree record))))
  operator)
 
(define generic:binary-append ; still arity 3
  (make-generic-operator 3 'binary-append append 'has-target-type))   

;;; Now this is how we define what we had before:
(defhandler generic:binary-append append list? list?)

;;; And this defines the version that will return a vector:
(defhandler generic:binary-append 
  (lambda args (display "In vector world") (apply append (cdr args)))
  (lambda (x) (eq? x vector)) list? list?)

;;; Now I can call (generic:binary-append vector '(1 2 3) '(4 5 6))
;;; and do whatever I want to do to make it a vector. Now we have to
;;; redefine sequence:append to pass the right arguments to
;;; generic:binary-append:

(define (sequence:append . sequences)
  (if (null? sequences)
      (error "Need at least one sequence for append"))
  (let ((first-item-is-type? (get-type-constructor (car sequences))))
    (let ((binary-append (if first-item-is-type?
			     (lambda (x y)
			       (generic:binary-append (car sequences)
						      x y))
			     generic:binary-append))
	  (new-sequences (if first-item-is-type?
			     (cdr sequences)
			     sequences)))
      (fold-right binary-append 
		  (sequence:null (sequence:type (car new-sequences)))
		  new-sequences))))

#| Tests

(generic:binary-append '(1 2 3) '(4 5 6))
;Value: (1 2 3 4 5 6)

(generic:binary-append vector '(1 2 3) '(4 5 6))
In vector world
;Value: (1 2 3 4 5 6)

(sequence:append '(1 2 3) '(4 5 6) '(7 8 9))
;Value: (1 2 3 4 5 6 7 8 9)

(sequence:append vector '(1 2 3) '(4 5 6) '(7 8 9))
In vector worldIn vector worldIn vector world
;Value: (1 2 3 4 5 6 7 8 9)

|#

;;; These tests do not show useful uses of this mechanism, but in
;;; general it can be used to dispatch to functions that work on
;;; well-chosen intermediate types, rather than just dispatching to a
;;; version of the function that prints something.  In general though,
;;; this is just a  special case of variable arity functions, where
;;; here the optional argument is a target type. 

;;; Problem 2.4
;;;
;;; Variable arity operations blah blah blah blah blah. TODO


;;; Problem 2.5
;;;
;;; TODO write stuff

#| Tests of Louis's idea
;1 ]=> (list<? '(1 2 3 4) '(2 1 3 4))

;Value: #t

1 ]=> (list<? '(2 1 3 4) '(1 2 3 4))

|#

;;; We can implement the total ordering of types by making the default
;;; operation one where the disparate types of the arguments are
;;; compared. Since a single function serves as the default operation,
;;; we do not have to enter N^2 entries into the table; we only have
;;; to specify the operations for like types and maybe some special
;;; cases of disarate types.

;;; Let's implement type ordering in a data-directed manner:

(define type-ordering
  (list null? boolean? char? number? symbol? string? vector? list?))

(define (different-types-less? one two)
  (define (get-type-position item)
    (let ((result (sequence:get-index type-ordering 
				      (lambda (type?) (type? item)))))
      (if result 
	  result
	  (error "Object of unknown type passed to generic:less?"))))
  (< (get-type-position one) (get-type-position two)))

;;; Now we can define generic:less? which will go to
;;; different-types-less? when arguments do not satisfy any signature
;;; registered with defhandler. 

(define generic:less? ; Use different-types-less? as default
  (make-generic-operator 2 'less? different-types-less?))

(define (list<? list-1 list-2)
  (let ((len-1 (length list-1)) (len-2 (length list-2)))
    (cond ((< len-1 len-2) #t)
	  ((> len-1 len-2) #f)
	  ;; Invariant: equal lengths
	  (else
	   (let prefix<? ((list-1 list-1) (list-2 list-2))
	     (cond ((null? list-1) #f) ; same
		   ((generic:less? (car list-1) (car list-2)) #t)
		   ((generic:less? (car list-2) (car list-1)) #f)
		   (else (prefix<? (cdr list-1) (cdr list-2)))))))))

(define (vector<? vector-1 vector-2)
  (list<? (sequence:as-list vector-1) (sequence:as-list vector-2)))

(define (boolean<? boolean-1 boolean-2) ; #f < #t
  (and (not (eq? boolean-1 boolean-2))
       boolean-2))

(define (null<? null-1 null-2) #f)

(defhandler generic:less? null<? null? null?)
(defhandler generic:less? boolean<? boolean? boolean?)
(defhandler generic:less? char<? char? char?)
(defhandler generic:less? symbol<? symbol? symbol?)
(defhandler generic:less? string<? string? string?)
(defhandler generic:less? vector<? vector? vector?)
(defhandler generic:less? list<? list? list?)

#| Tests

(generic:less? '(1 2 3 4) '(2 1 3 4))
;Value: #f

(generic:less? '(2 1 3 4) '(1 2 3 4))
;Value: #f

(generic:less? #f '(1 2 3 4))
;Value: #t

(generic:less? #f #t)
;Value: #t

(generic:less? 'a #(1 2 3))
;Value: #t

|# 

;;; Problem 2.6
;;;
;;; TODO write stuff

;;; Problem 2.7
;;;
;;; Now we can integrate streams into the generic sequence
;;; operators. 

(define stream? stream-pair?)

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

;;; Easy ones
(defhandler sequence:as-list stream->list stream?)
(defhandler sequence:null (constant '()) (is-exactly stream?))
(defhandler sequence:null? stream-null? stream?)
(defhandler sequence:ref stream-ref stream? exact-integer?)
(defhandler sequence:size stream-length stream?)
(defhandler sequence:type (constant stream?) stream?)

#| Tests

(sequence:as-list (stream 0 1 2 3 4))
;Value: (0 1 2 3 4)

(sequence:as-list (integers-starting-from 0)) 
;Don't do this!

(sequence:null stream?)
;Value: '()

(sequence:null? (stream))
;Value: #t

(sequence:ref (stream 0 1 2 3 4) 1)
;Value: 1

(sequence:ref (integers-starting-from 0) 1)
;Value: 1

(sequence:size (stream 0 1 2 3 4))
;Value: 5

(sequence:size (integers-starting-from 0))
;Don't do this!

(sequence:type (integers-starting-from 0))
;Value: #[compiled-procedure 207 ("stream" #x1) #x3 #x2f161]

|# 

;;; Add constructors
(add-type-constructor stream? stream)
(add-type-constructor stream stream)
(add-type-constructor 'stream stream)

#| Tests

(stream->list (sequence:construct stream 0 1 2 3 4))
(0 1 2 3 4)

|#

;;; sequence:equal? uses sequence:map so we don't need to reimplement.
;;; sequence:set! is not applicable to streams.
;;; sequence:subsequence
(define (stream-subsequence s start end)
  (apply stream (stream-tail (stream-head s (+ start end)) start)))

(defhandler sequence:subsequence stream-subsequence 
  stream? exact-integer? exact-integer?)

#| Tests

(stream->list (sequence:subsequence (stream 0 1 2 3 4) 0 2))
;Value: (0 1)

(stream->list (sequence:subsequence (integers-starting-from 0) 0 2))
;Value: (0 1)

|#

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
		   (stream-append (stream-cdr s1) s2))))

(defhandler generic:binary-append string-append  string? string?)
(defhandler generic:binary-append append         list?   list?)
(defhandler generic:binary-append vector-append  vector? vector?)
(defhandler generic:binary-append stream-append  stream? stream?)


;;; sequence:generate
(define (stream-range from to)
  (if (= from to)
      '()
      (cons-stream from (stream-range (+ from 1) to))))

(define (stream-generate desired-type n proc)
  (stream-map proc (stream-range 0 n)))

(defhandler sequence:generate stream-generate 
  (is-exactly stream?) exact-integer? procedure?)

#| Tests

(stream->list (sequence:generate stream? 5 (lambda (x) x)))
;Value: (0 1 2 3 4)

|# 

;;; map and for-each already work for finite streams. See discussion
;;; below. 

;;; sequence:filter -- see discussion for how this works with infinite
;;; streams!
(define (stream-filter pred stream)
  (if (stream-null? stream)
      '()
      (let ((first-item (stream-car stream)))
	(if (pred first-item)
	    (cons-stream first-item 
			 (stream-filter pred (stream-cdr stream)))
	    (stream-filter pred (stream-cdr stream))))))
      
(defhandler sequence:filter (switch-arg-order stream-filter '(1 0))
  stream? procedure?)

#| Tests

(stream->list (sequence:filter (stream 0 1 2 3) (lambda (x) (< x 3))))
;Value: (0 1 2)

(stream-head (sequence:filter (integers-starting-from 0) 
			      (lambda (x) (< x 3))) 2)
;Value: (0 1)

(stream-head (sequence:filter (integers-starting-from 0) 
			      (lambda (x) (< x 3))) 10)
;Don't do this!

|# 

;;; sequence:get-index
(define (stream-get-index sequence pred)
  (define (get-index sequence n)
    (if (null? sequence)
        #f
        (let ((first-element (stream-car sequence)))
          (if (pred first-element)
              n
              (get-index (stream-cdr sequence) (+ n 1))))))
  (get-index sequence 0))

(defhandler sequence:get-index stream-get-index stream? procedure?)

#| Tests

(sequence:get-index (stream 0 1 2 3 4) (lambda (x) (equal? x 2)))
;Value: 2

(sequence:get-index (integers-starting-from 0) (lambda (x) (equal? x
								   2)))
;Value: 2

(sequence:get-index (integers-starting-from 3) (lambda (x) (equal? x
                                                                   2)))
;Don't do this!

|# 

;;; sequence:get-element uses sequence:filter so it already works.

#| Tests 

(sequence:get-element (stream 0 1 2 3 4) (lambda (x) (equal? x 2)))
;Value: 2

(sequence:get-element (integers-starting-from 0) (lambda (x) (equal? x
                                                                   2)))
;Value: 2

(sequence:get-element (integers-starting-from 3) (lambda (x) (equal? x
                                                                   2)))
;Don't do this!

|#

;;; sequence:fold-right and sequence:fold-left have to be pretty smart
;;; to deal correctly with infinite streams. But they work fine with
;;; finite streams. For now I am going to just implement this using
;;; lists as an intermediate representation. 
(defhandler sequence:fold-right (operate-on-list fold-right 2)
  procedure? any? stream?)
(defhandler sequence:fold-left (operate-on-list fold-left 2)
  procedure? any? stream?)

#| Tests

(sequence:fold-right list 'end (stream 'a 'b 'c))
;Value: (a (b (c end)))

(sequence:fold-left list 'end (stream 'a 'b 'c))
;Value: (((end a) b) c)

|#


;;; The generic dispatch system here is dangerously general.

;;; The task of integrating streams with other sequences so that
;;; generic functions can be used was easy. But the task of doing this
;;; *well* is more difficult, because new operator functions have to be
;;; written to respect the lazy evaluation of streams. For example,
;;; sequence:filter for strings could have just coerced the stream to
;;; a list, filtered, and returned a stream of the resulting list; but
;;; that would be inefficient and would take infinite memory if passed
;;; an infinite stream.

;;; Infinite streams also raise some tricky issues. For example, if I
;;; do this:

#| Test
(sequence:get-element (integers-starting-from 4) (lambda (x) (= x 3)))
|#

;;; I will get not #f, but nothing, as it will try to evaluate
;;; forever. For sequence:filter, sequence:fold, and their ilk to
;;; determine when to stop evaluating an infinite stream is a hard
;;; task. By adding stream operations for these generic functions, we
;;; have introduced the danger that a user might unwittingly pass in
;;; an infinite stream and break her computer. We should amend the
;;; spec sheet to tell users not to do this.

