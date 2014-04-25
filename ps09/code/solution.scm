;;; 6.945 Problem Set 9
;;; Richard Futrell
;;; futrell@mit.edu

;;; Problem 1
;;;
;;; The a and b values each descend monotonically but are
;;; interleaved. The time between a values is determined by the time
;;; it takes to decrement i from 100 to zero with the buzz
;;; function. The time between b values gets slower since it is the
;;; time it takes for the buzz function to go from 100*(n-i) to 0 as i
;;; decreases.

;;; Problem 2
;;;
;;; a. Better to evaluate the arguments strictly. If a programmer
;;; wants to make a function with delayed arguments she can make that
;;; function take a thunk. On the other hand if arguments are lazily
;;; evaluated and the programmer wants strict evaluation, it is less
;;; clear what she should do. Also, unevaluated arguments which might
;;; be mutable become scary to reason about when you don't know when
;;; exactly in what order function bodies will be evaluated.
;;;
;;; b. Just move the argument evaluation into the thunk:

(defhandler apply
  (lambda (actor operands calling-environment)
    (if (not (= (length (actor-parameters actor))
                (length operands)))
        (error "Wrong number of operands supplied"))
    (add-to-tasks! actor
                   (lambda ()
                     (let ((arguments
                            (map (lambda (parameter operand)
                                   (evaluate-procedure-operand parameter
                                                               operand
                                                               calling-environment))
                                 (actor-parameters actor)
                                 operands)))
                       (eval (actor-body actor)
                             (extend-environment
                              (map procedure-parameter-name
                                   (actor-parameters actor))
                              arguments
                              (actor-environment actor))))))
    'actor-applied)
  actor-procedure?)

#| Tests

eval> (foo 10)
actor-applied
(b 10)
(b 9)
(b 8)
(b 7)
(a 10)
(b 6)
(b 5)
(a 9)
(b 4)
(a 8)
(b 3)
(a 7)
(b 2)
(a 6)
(a 5)
(b 1)
(a 4)
(a 3)
(a 2)
(a 1)

eval> (fib1 10 write-line)
actor-applied
55

eval> (fib2 10 write-line)
actor-applied
55

|#

;;; c. Better than providing a "beta expression" would be the
;;; mechanism seen in problem set 4, where arguments could be tagged
;;; as lazy.

;;; Problem 3
;;;
;;; Double-check-lock takes three thunks: check, do, and if-not. It
;;; evaluates (check) and if it is true, it evaluates the expression
;;; (do) for its side effects, then ((lambda () 'ok)) to return 'ok.
;;; If (check) is false, it calls if-not, which is the procedure for
;;; trying again recursively until (check) is true. It is needed
;;; because when setting or defining a variable, that variable might
;;; have been set or defined by another thread in the time between
;;; when this thread called set! and when it got to the relevant part
;;; of the set! procedure.

;;; Problem 4
;;;
;;; a. fib1 is slow because when each fib1 thread that is not ready to
;;; return is busy calling itself in order to wait; since the threads
;;; aren't running on multiple cores, this takes time.
;;;
;;; b. 55 gets passed to the continuation c twice in the case when
;;; check-if-done is called twice. Usually check-if-done gets called
;;; in one of two locations: either after x is set or after y is
;;; set; and it isn't called again because it either returns #f if the
;;; other variable isn't set yet, or returns x+y into the continuation
;;; if the other variable is set. But if the second call to
;;; check-if-done happens after the first one has started but before
;;; it gets to the part where it either returns #f or continues, then
;;; it proceeds and has the potential to send the result into the
;;; continuation twice.
;;;
;;; To fix this, we would need to prevent check-if-done from being
;;; entered from multiple threads at the same time:

   (define fib3
       (alpha (n c)
	 (if (< n 2)
	     (c n)
	     (let ((x 'not-ready) (y 'not-ready))
	       (define check-if-done
		 (lambda ()
                   (atomically
                    (lambda ()
                      (if (boolean/or (eq? x 'not-ready)
                                      (eq? y 'not-ready))
                          #f
                          (c (+ x y)))))))
	       (fib3 (- n 1)
		     (lambda (v)
                       (set! x v)
		       (check-if-done)))
	       (fib3 (- n 2)
		     (lambda (v) ; what does this accomplish?
                       (set! y v)
                       (check-if-done)))))))

;;; I ran (fib3 10 write-line) 30 times and didn't get any duplicate 55s. 

;;; Problem 5
;;;

(define *future-unevaluated* (list 'unevaluated))

(define (make-future value done?)
  (vector value done?))

(define (future-value future)
  (atomically
   (lambda ()
     (vector-ref future 0))))

(define (future-done? future)
  (atomically
   (lambda ()
     (vector-ref future 1))))

(define (set-future-value! future value)
  (atomically
   (lambda ()
     (vector-set! future 0 value))))

(define (set-future-done! future)
  (atomically
   (lambda ()
     (vector-set! future 1 #t))))

(define (future proc-taking-continuation) 
  (let ((done? #f) (value *future-unevaluated*))
    (let ((future-object (make-future value done?)))
      (let ((actor
             (alpha ()
                    (proc-taking-continuation
                     (lambda (x)
                       (set-future-value! future-object x)
                       (set-future-done! future-object))))))
        (actor))
      future-object)))

(define (wait future continuation) 
  (define continue-if-ready
    (alpha ()
           (if (future-done? future)
               (continuation (future-value future))
               (continue-if-ready))))
  (continue-if-ready))
  
#| Tests

eval> (define (test-not-hanging thunk)
          (thunk)
          (write-line "if this came out immediately, then it's not hanging"))

eval> (define catf (future (lambda (k) (k 'cat))))
catf

eval> catf
#(cat #t)

eval> (test-not-hanging (lambda () (future (lambda (k) (k 'cat)))))
"if this came out immediately, then it's not hanging"
#!unspecific

eval> (wait catf write-line)
cat

eval> (test-not-hanging (lambda () (wait catf write-line)))
"if this came out immediately, then it's not hanging"
#!unspecific
cat

(fib 10 write-line)
actor applied
55

eval> (test-not-hanging (lambda () (fib 10 write-line)))
"if this came out immediately, then it's not hanging"
#!unspecific
55

eval> (define (test-wait-not-hanging thunk)
        (call-with-current-continuation
         (lambda (return)
           (define long-running-actor (future (lambda (k) (k (thunk)))))
           (wait long-running-actor return)
           (write-line "You should see this immediately."))))

eval> (test-wait-not-hanging (lambda () (fib 7 write-line)))
"You should see this immediately."
#!unspecific
actor-applied
13

|#

;;; Waiting on a future multiple times doesn't seem to cause a
;;; problem:

#|

eval>  (define (test-wait-twice-not-hanging thunk)
        (call-with-current-continuation
         (lambda (return)
           (define long-running-actor (future (lambda (k) (k (thunk)))))
           (wait long-running-actor return)
           (wait long-running-actor return)
           (write-line "You should see this immediately."))))
test-wait-twice-not-hanging

eval> (test-wait-twice-not-hanging (lambda () (fib 8 write-line)))
"You should see this immediately."
#!unspecific
actor-applied
actor-applied
21

|#
