;;;;           Most General Generic-Operator Dispatch

(declare (usual-integrations))

;;; Generic-operator dispatch is implemented here by a discrimination
;;; list, where the arguments passed to the operator are examined by
;;; predicates that are supplied at the point of attachment of a
;;; handler (by DEFHANDLER).

;;; To be the correct branch all arguments must be accepted by
;;; the branch predicates, so this makes it necessary to
;;; backtrack to find another branch where the first argument
;;; is accepted if the second argument is rejected.  Here
;;; backtracking is implemented by OR.

(define (make-generic-operator arity #!optional name default-operation)
  (let ((record (make-operator-record arity)))
    (define (operator . arguments)
      (if (not (= (length arguments) arity))
          (error "Wrong number of arguments for generic operator"
		 (if (default-object? name) operator name)
		 arity arguments))
      (apply (or (let per-arg
                     ((tree (operator-record-tree record))
                      (args arguments))
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
			      (if (default-object? name) operator name)
			      args))
		     default-operation))
             arguments))

    (hash-table/put! *generic-operator-table* operator record)
    operator))

(define *generic-operator-table*
  (make-eq-hash-table))

(define (get-operator-record operator)
  (hash-table/get *generic-operator-table* operator #f))

(define (make-operator-record arity) (cons arity '()))
(define (operator-record-arity record) (car record))
(define (operator-record-tree record) (cdr record))
(define (set-operator-record-tree! record tree) (set-cdr! record tree))

(define (defhandler operator handler . argument-predicates)
  (let ((record
         (let ((record (hash-table/get *generic-operator-table* operator #f))
               (arity (length argument-predicates)))
           (if record
               (begin
                 (if (not (= arity (operator-record-arity record)))
                     (error "Incorrect operator arity:" operator))
                 record)
               (error "Operator not known" operator)))))
    (set-operator-record-tree! record
      (bind-in-tree argument-predicates
		    handler
		    (operator-record-tree record))))
  operator)

;;; An alias used in some old code
(define assign-operation defhandler)


(define (bind-in-tree keys handler tree)
  (let loop ((keys keys) (tree tree))
    (let ((p.v (assq (car keys) tree)))
      (if (pair? (cdr keys))
          (if p.v
              (begin
                (set-cdr! p.v
                          (loop (cdr keys) (cdr p.v)))
                tree)
              (cons (cons (car keys)
                          (loop (cdr keys) '()))
                    tree))
          (if p.v
              (begin
                (warn "Replacing a handler:" (cdr p.v) handler)
                (set-cdr! p.v handler)
                tree)
              (cons (cons (car keys) handler)
                    tree))))))

#|
;;; Demonstration of handler tree structure.
;;; Note: symbols were used instead of procedures

(define foo (make-generic-operator 3 'foo))
;Value: foo

(pp (get-operator-record foo))
(3)

(defhandler foo 'abc 'a 'b 'c)

(pp (get-operator-record foo))
(3 (a (b (c . abc))))

(defhandler foo 'abd 'a 'b 'd)

(pp (get-operator-record foo))
(3 (a (b (d . abd) (c . abc))))

(defhandler foo 'aec 'a 'e 'c)

(pp (get-operator-record foo))
(3 (a (e (c . aec))
      (b (d . abd)
	 (c . abc))))

(defhandler foo 'dbd 'd 'b 'd)

(pp (get-operator-record foo))
(3 (d (b (d . dbd)))
   (a (e (c . aec))
      (b (d . abd)
	 (c . abc))))
|#
