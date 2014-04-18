;;; 6.945 Problem Set 8
;;; Richard Futrell
;;; futrell@mit.edu

;;; Problem 1
;;;
;;; The time and space complexity is O(N^2) in the number of terminal
;;; nodes of the tree. This is because each call to append makes a
;;; copy of the fringe and walks down part of it, and append is called
;;; at least once for each node. 

;;; Problem 2
;;;
;;; A. Suppose the first element of stream2 takes infinitely long to
;;; compute. In that case if it is not thunkified before being passed
;;; to stream-append-deferred, then that first element will be
;;; evaluated by cons and we will be in trouble. 
;;;
;;; B. We delay the walk and build up an even stream as ans.

(define (lazy-fringe subtree)
  (define (walk subtree ans-thunk)
    (cond ((pair? subtree)
	   (walk (car subtree)
		 (lambda () (walk (cdr subtree) ans-thunk))))
	  ((null? subtree) (ans-thunk))
	  (else (cons-stream subtree (ans-thunk)))))
  (walk subtree (lambda () '())))

#| Tests

(lazy-same-fringe? '((a b) c ((d)) e (f ((g h))))
                   '(a b c ((d) () e) (f (g (h)))))
;Value: #t

(lazy-same-fringe?
 '((a b) c ((d)) e (f ((g h))))
 '(a b c ((d) () e) (g (f (h)))))

;Value: #f

|#

;;; Problem 3
;;;
;;; We want to put resume-thunk in its own thunk so that the call to
;;; set! can change the function that is called from the outside. If
;;; resume-thunk were not in its own thunk, then the call to set!
;;; would not change the function when it is called outside the scope
;;; of the definition.


;;; Problem 4
;;;

(define (make-coroutine todo) 
  (let ((todo-future #f))
    (lambda (supplicant)      
      (let ((resume-point supplicant))
	(define (return value)
	    (set! resume-point      
		  (call-with-current-continuation
		    (lambda (k)
		         (set! todo-future k)
			    (resume-point value)))))
	(if todo-future             
	        (todo-future supplicant)
		    (todo return)))))) 

(define next-value  call-with-current-continuation)

(define *done* (list '*done*))

(define (done? x) (eq? x *done*))

(define (list-iterator list)
  (make-coroutine 
   (lambda (return) 
     (for-each return list)
     (return *done*))))

;;; The bug seems to appear only when there are two iterators. But
;;; really it exists when there is just one. Observe:

#|
(define foo (list-iterator '(a b)))
;Value: foo

(next-value foo)
;Value: a

(define cat (next-value foo))
;Value: cat

cat
;Value: b

(define dog (next-value foo))
;Value 2: (*done*) ; hey wait a minute...

dog
;Unbound variable: dog
|#

;;; The fact that dog is unbound suggests that the define was never
;;; executed successfully; when we saw value (*done*), that was passed
;;; through a continuation to the REPL, short-circuiting the
;;; define. But what exactly is that continuation and where does it
;;; come from? Let's figure it out by calling next-value from
;;; somewhere other than the REPL.

#|
(define (call-then-print thunk name)
  (let ((result (thunk)))
    (pp name)
    result))

(call-then-print (lambda () (next-value foo)) 'one)
one
;Value: a

(call-then-print (lambda () (next-value foo)) 'two)
two
;Value: b

(call-then-print (lambda () (next-value foo)) 'three)
one
;Value 2: (*done*)

(call-then-print (lambda () (next-value foo)) 'four)
one
;Value 2: (*done*)

|#

;;; So *done* is being sent to the continuation that was current the
;;; first time we called next. This explains why we get ((*done*) a)
;;; in the example in the handout: The *done* is sent via the
;;; continuation to the point where we first called next-value; after
;;; jumping to that point, we proceed to get the first elements from
;;; the rest of the iterators in the let expression.

;;; So why is *done* sent to the initial continuation? First let's see
;;; what the coroutines really look like.

;;; make-coroutine reduces to this with initial values todo-future=#f,
;;; resume-point=supplicant:

(lambda (todo)
  (lambda (supplicant) 
    (if todo-future 
	(todo-future supplicant)
	(todo (lambda (value) 
		(set! resume-point
		      (call/cc
		       (lambda (k)
			 (set! todo-future k)
			 (resume-point value)))))))))

;;; plugging in the buggy list-iterator as todo, we get:

(lambda (supplicant) 
  (if todo-future 
      (todo-future supplicant) ; THEN CLAUSE: todo-future is a
			       ; continuation that sends supplicant
			       ; off to be set as the resume-point in
			       ; the else-clause.
      (begin ; ELSE CLAUSE: We go here directly the first time
	     ; next-value is called; afterwards, we jump into this
	     ; clause from the then-clause.
	(for-each (lambda (value) 
		    (set! resume-point ;gets (resume-point value)
			  (call/cc
			   (lambda (k)
			     (set! todo-future k)
			     (resume-point value)))))
		  list)
	*done*) 
      ))

;;; The bug happens because when we use the continuation in the
;;; then-clause to jump into the else-clause, we are jumping into a
;;; state where the underlying continuation of the function is the
;;; one from when that else clause was first evaluated, i.e the first
;;; time next-value was called. So when we return *done* from the
;;; function we are returning it into that continuation, rather than
;;; the continuation from the most recent time we called
;;; next-value. 

;;; Problem 5

(load "conspire")

(define (make-pipe) ; a pipe is a queue and a lock
  (let ((queue (queue:make))
	(lock (conspire:make-lock)))
    (list queue lock)))

(define (make-locked-procedure proc lock)
  (define (locked-procedure . arguments)
    (conspire:acquire-lock lock)
    (let ((result (apply proc arguments)))
      (conspire:unlock lock)
      result))
  locked-procedure)

(define (pipe-empty? pipe)
  (let ((queue (car pipe))
	(lock (cadr pipe)))
    (let ((empty? (make-locked-procedure 
		   queue:empty? 
		   lock)))
      (empty? queue))))
    
(define (pipe-writer pipe)
  (let ((queue (car pipe))
	(lock (cadr pipe)))
    (define (write-pipe thing)
      (queue:add-to-end! queue thing))
    (make-locked-procedure write-pipe lock)))

(define (pipe-reader pipe)
  (let ((queue (car pipe))
	(lock (cadr pipe)))
    (define (read-pipe)
      (define safe-get-first 
	(make-locked-procedure queue:get-first lock))
      (conspire:switch-threads ; block until something's there
       (lambda () (not (pipe-empty? pipe))))
      (safe-get-first queue))
    read-pipe))

(define *done* (list '*done*))

(define (tap x)
  (pp x)
  x)

(define (piped-same-fringe? tree1 tree2)
  (let ((p1 (make-pipe)) (p2 (make-pipe)))
    (let ((thread1
           (conspire:make-thread
            conspire:runnable
            (lambda ()
              (piped-fringe-generator tree1 (pipe-writer p1)))))
          (thread2
           (conspire:make-thread
            conspire:runnable
            (lambda ()
              (piped-fringe-generator tree2 (pipe-writer p2)))))
          (f1 (pipe-reader p1))
          (f2 (pipe-reader p2)))
      (let lp ((x1 (f1)) (x2 (f2)))
        (cond ((and (eq? x1 *done*) (eq? x2 *done*)) #t)
              ((or (eq? x1 *done*) (eq? x2 *done*)) #f)
              ((eq? x1 x2) (lp (f1) (f2))) ; read from pipe-readers
              (else #f))))))

(define (piped-fringe-generator tree return)
  (define (lp tree)
    (cond ((pair? tree)
           (lp (car tree))
           (lp (cdr tree)))
          ((null? tree) unspecific)
          (else (return tree))))
  (lp tree)
  (return *done*))

#| Tests

(with-time-sharing-conspiracy
 (lambda ()
   (piped-same-fringe?
    '((a b) c ((d)) e (f ((g h))))
    '(a b c ((d) () e) (f (g (h)))))
   ))
;Value: #t

(with-time-sharing-conspiracy
 (lambda ()
   (piped-same-fringe?
    '((a b) c ((d)) e (f ((g h))))
    '(a b c ((d) () e) (g (f (h)))))
   ))
;Value: #f

(with-time-sharing-conspiracy
 (lambda ()
   (piped-same-fringe?
    '((a b) c ((d)) e (f ((g h))))
    '(a b c ((d) () e) (g (f ))))
   ))
;Value: #f

|#

;;; Problem 6

(define (make-threaded-filter generator)
  (let ((pipe (make-pipe)))
    (conspire:make-thread conspire:runnable
			  (lambda ()
			    (generator (pipe-writer pipe))))
    (pipe-reader pipe)))
    
(define (tf-piped-fringe-generator tree)
  (lambda (yield) 
    (define (lp tree)
      (cond ((pair? tree)
	     (lp (car tree))
	     (lp (cdr tree)))
	    ((null? tree) unspecific)
	    (else
	     (yield tree))))
    (lp tree)
    (yield *done*)))


(define (tf-piped-same-fringe? tree1 tree2)
  (let ((f1 (make-threaded-filter (tf-piped-fringe-generator tree1)))
	(f2 (make-threaded-filter (tf-piped-fringe-generator tree2))))
    (let lp ((x1 (f1)) (x2 (f2)))
      (cond ((and (eq? x1 *done*) (eq? x2 *done*)) #t)
	    ((or  (eq? x1 *done*) (eq? x2 *done*)) #f)
	    ((eq? x1 x2) (lp (f1) (f2)))
	    (else #f)))))

#|
(with-time-sharing-conspiracy
 (lambda ()
   (tf-piped-same-fringe?
    '((a b) c ((d)) e (f ((g h))))
    '(a b c ((d) () e) (f (g (h)))))
   ))
;Value: #t

(with-time-sharing-conspiracy
 (lambda ()
   (tf-piped-same-fringe?
    '((a b) c ((d)) e (f ((g h))))
    '(a b c ((d) () e) (g (f (h)))))
   ))
;Value: #f

(with-time-sharing-conspiracy
 (lambda ()
   (tf-piped-same-fringe?
    '((a b) c ((d)) e (f ((g h))))
    '(a b c ((d) () e) (g (f ))))
   ))
;Value: #f
|#

