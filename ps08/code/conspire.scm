;;;;  CONSPIRE: Time Sharing in Scheme
;;;     "Processes scheming together
;;;       constitute a conspiracy"

;;; The essence of this system is that the state of a
;;; thread is specified by its continuation.  To switch
;;; threads we need to make a continuation, store it
;;; for the scheduler, and then retrieve a thread from
;;; the scheduler and start it running.  The thread has
;;; an identity, even though its continuation changes
;;; from time to time.

;;; A running thread can block itself until some
;;; predicate thunk becomes true by calling
;;; conspire:switch-threads with the predicate.

(define (conspire:switch-threads runnable?)
  (conspire:save-current-thread runnable?
	conspire:start-next-thread))

(define (conspire:save-current-thread runnable? after-save)
  (call-with-current-continuation
   (lambda (current-continuation)
     (without-interrupts
      (lambda ()
	(conspire:set-continuation!
	 *running-thread* current-continuation)
	(conspire:add-to-schedule!
	 runnable? *running-thread*)))
     (after-save))))

(define (conspire:start-next-thread)
  ((conspire:continuation
    (without-interrupts
     (lambda ()
       (set! *running-thread*
	     (conspire:get-runnable-thread-from-schedule!))
       *running-thread*)))
   unspecific))


;;; A thread can explicitly yield control, remaining
;;; runnable.

(define (conspire:thread-yield)
  (conspire:switch-threads conspire:runnable))

(define conspire:runnable (lambda () #t))

;;; A thread can kill itself by starting some other thread
;;; without saving itself for rescheduling.

(define (conspire:kill-current-thread)
  (conspire:start-next-thread))

(define (conspire:kill-other-threads threads)
  (without-interrupts
   (lambda ()
     (for-each conspire:delete-from-schedule! threads))))

;;; A thread can make another thread and continue running.
;;; The thunk specified is the work order for the new thread. 
;;; When the thunk returns the thread kills itself.

(define (conspire:make-thread runnable? thunk)
  (call-with-current-continuation
   (lambda (current-continuation)
     (within-continuation *root-continuation*
       (lambda ()
	 (call-with-current-continuation
	  (lambda (new-continuation)
	    (current-continuation
	     (without-interrupts
	      (lambda ()
		(let ((new-thread
		       (conspire:make-new-thread
			new-continuation)))
		  (conspire:add-to-schedule!
		     runnable? new-thread)
		  new-thread))))))
	 (thunk)
	 (conspire:kill-current-thread))))))


;;; A simple scheduler is just round-robin.  

(define (conspire:add-to-schedule! runnable? thread)
  (queue:add-to-end! *thread-queue*
		     (cons runnable? thread)))

(define (conspire:get-runnable-thread-from-schedule!)
  (if (not (queue:empty? *thread-queue*))
      (let lp ((first (queue:get-first *thread-queue*)))
	(if ((car first))		; runnable?
	    (cdr first)
	    (begin
	      (queue:add-to-end! *thread-queue* first)
	      (lp (queue:get-first *thread-queue*)))))
      (error "No current thread")))

(define (conspire:delete-from-schedule! thread)
  (let ((entry
	 (find-matching-item
	     (queue:front-ptr *thread-queue*)
	   (lambda (entry)
	     (eq? (cdr entry) thread)))))
    (if entry
	(queue:delete-from-queue! *thread-queue*
				  entry))))

;;; We use the queue design similar to SICP Section 3.3.2

(define-record-type queue
    (queue:make-record front-ptr rear-ptr)
    queue?
  (front-ptr queue:front-ptr queue:set-front-ptr!)
  (rear-ptr  queue:rear-ptr  queue:set-rear-ptr!))

(define (queue:make)
  (queue:make-record '() '()))

(define (queue:empty? queue)
  (null? (queue:front-ptr queue)))

(define (queue:get-first queue)
  (if (null? (queue:front-ptr queue))
      (error "get-first called with an empty queue" queue)
      (let ((first (car (queue:front-ptr queue)))
	    (rest (cdr (queue:front-ptr queue))))
	(queue:set-front-ptr! queue rest)
	(if (null? rest)
	    (queue:set-rear-ptr! queue '()))
	first)))

(define (queue:add-to-end! queue item)
  (let ((new-pair (cons item '())))
    (cond ((null? (queue:front-ptr queue))
	   (queue:set-front-ptr! queue new-pair)
	   (queue:set-rear-ptr! queue new-pair))
	  (else
	   (set-cdr! (queue:rear-ptr queue) new-pair)
	   (queue:set-rear-ptr! queue new-pair))))
  'done) 

(define (queue:delete-from-queue! queue item)
  (queue:set-front-ptr! queue
			(delq item
			      (queue:front-ptr queue)))
  (if (pair? (queue:front-ptr queue))
      (queue:set-rear-ptr! queue
			   (last-pair (queue:front-ptr queue)))
      (queue:set-rear-ptr! queue '()))
  'done)

(define-record-type conspire:thread
    (conspire:make-new-thread continuation)
    conspire:thread?
  (continuation conspire:continuation
		conspire:set-continuation!))


;;; Startup: have to make queue and first process

(define (with-conspiracy thunk)
  (fluid-let ((*running-thread*
	       (conspire:make-new-thread unspecific))
	      (*thread-queue* (queue:make))
	      (*root-continuation*))
    (call-with-current-continuation
     (lambda (k)
       (set! *root-continuation* k)
       (thunk)))))

(define *running-thread*)

(define *thread-queue*)

(define *root-continuation*)

#|
;;; An elementary example:

(define (loop n)
  (let lp ((i 0))
    (if (< global-counter 1)
	'done
	(begin (set! global-counter (- global-counter 1))
	       (if (= i n)
		   (begin (write-line `(,n ,global-counter))
			  (conspire:thread-yield)
			  (lp 0))
		   (lp (+ i 1)))))))

(define global-counter)

(with-conspiracy
    (lambda ()
      (set! global-counter 200)
      (conspire:make-thread conspire:runnable (lambda () (loop 31)))
      (conspire:make-thread conspire:runnable (lambda () (loop 37)))
      (repl/start (push-repl (nearest-repl/environment))
		  "; Entering conspiracy")))
			     
(pp *thread-queue*)
#[queue 4]
(front-ptr
 ((#[compound-procedure 6 conspire:runnable] . #[conspire:thread 7])
  (#[compound-procedure 6 conspire:runnable] . #[conspire:thread 5])))
(rear-ptr
 ((#[compound-procedure 6 conspire:runnable] . #[conspire:thread 5])))

(conspire:thread-yield)
(31 168)
(37 130)
;Unspecified return value

;;; Got back to repl.

(conspire:thread-yield)
(31 98)
(37 60)
;Unspecified return value

(conspire:thread-yield)
(31 28)
;Unspecified return value

(conspire:thread-yield)
;Unspecified return value

(pp *thread-queue*)
#[queue 4]
(front-ptr ())
(rear-ptr ())

(abort->previous)			; Get out of repl.
|#

;;; Preemptive scheduling.

(define conspire:quantum 10) 

(define conspire:running? #f)

;;; This is an MIT Scheme specific detail.  register-timer-event is
;;; the MIT Scheme mechanism for delivering a timer interrupt -- when
;;; the time specified by its first argument expires, it invokes the
;;; second argument.

(define (start-time-sharing)
  (let lp ()
    (if *debugging-time-sharing* (display "."))
    (if conspire:running?
	(begin
	  (register-timer-event conspire:quantum
				lp)
	  (conspire:thread-yield))))
  'done)

(define *debugging-time-sharing* #f)


(define (with-time-sharing-conspiracy thunk)
  (fluid-let ((conspire:running? #t))
    (with-conspiracy
	(lambda ()
	  (start-time-sharing)
	  (thunk)))))

(define (conspire:null-job)
  (conspire:thread-yield)
  (if (queue:empty? *thread-queue*)
      'done
      (conspire:null-job)))

#|
;;; Our elementary example, again

(define (loop n)
  (let lp ((i 0))
    (if (< global-counter 1)
	'done
	(begin (set! global-counter (- global-counter 1))
	       (if (= i n)
		   (begin (write-line `(,n ,global-counter))
			  (lp 0))
		   (lp (+ i 1)))))))


(define global-counter)

(with-time-sharing-conspiracy
    (lambda ()
      (set! global-counter 100000)
      (conspire:make-thread conspire:runnable (lambda () (loop 5555)))
      (conspire:make-thread conspire:runnable (lambda () (loop 4444)))
      (conspire:null-job)))

(5555 94444)
(5555 88888)
(5555 83332)
(5555 77776)
(4444 71412)
(4444 66967)
(4444 62522)
(4444 58077)
(4444 53632)
(4444 49187)
(4444 44742)
(5555 39853)
(5555 34297)
(5555 28741)
(5555 23185)
(5555 17629)
(4444 9782)
(4444 5337)
(4444 892)
;Value: done
|#

;;; Interlocks

(define-record-type conspire:lock
    (conspire:make-lock-cell state)
    conspire:lock?
  (state conspire:lock-state conspire:set-lock-state!))

(define (conspire:make-lock)
  (conspire:make-lock-cell #f))

(define (test-and-set-lock?! cell)
  (if (not (conspire:lock? cell))
      (error "Bad lock"))
  (without-interrupts
   (lambda ()
     (if (eq? (conspire:lock-state cell) #f)
	 (begin (conspire:set-lock-state! cell #t)
		#t)
	 #f))))

(define (conspire:unlock cell)
  (conspire:set-lock-state! cell #f))

(define (conspire:acquire-lock lock)
  (if (test-and-set-lock?! lock)
      'OK
      (conspire:switch-threads
       (lambda () (test-and-set-lock?! lock)))))

#|
;;; Our elementary example again:

(define global-counter-lock (conspire:make-lock))

(define (loop n)
  (let lp ((i 0))
    (let delaylp ((k 100))
      (if (> k 0)
	  (delaylp (- k 1))))
     (conspire:acquire-lock global-counter-lock)
     (if (< global-counter 1)
	 (begin
	   (conspire:unlock global-counter-lock)
	   'done)
	 (begin (set! global-counter (- global-counter 1))		     
		(if (= i n)
		    (begin (write-line `(,n ,global-counter))
			   (conspire:unlock global-counter-lock)
			   (lp 0))
		    (begin
		      (conspire:unlock global-counter-lock)
		      (lp (+ i 1)))))))
  (write-line `(,n terminating)))

(define global-counter)

(set! conspire:quantum 5)

(with-time-sharing-conspiracy
    (lambda ()
      (set! global-counter 100000)
      (conspire:make-thread conspire:runnable (lambda () (loop 999)))
      (conspire:make-thread conspire:runnable (lambda () (loop 1000)))
      (conspire:null-job)))
|#
