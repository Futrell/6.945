;;;; Tasks are scheuled with queues.  

(declare (usual-integrations))

;;; There are queues of tasks for individual actors, and 
;;; there is a queue for runnable actors (those who have 
;;; non-empty task queues).  A task is a thunk.

(define runnable-actors)
(define (init-actors)
  (set! runnable-actors (queue:make)))

(define (run-one)
  (if (queue:empty? runnable-actors)
      'nothing-to-do
      (let ((actor (queue:get-first! runnable-actors)))
	(if (MITscheme-continuation? actor)
	    (lambda () (actor 'go))
	    (let ((actor-task-queue (get-actor-task-queue actor)))
	      (if (queue:empty? actor-task-queue)
		  (begin ;; Nothing for this actor to do.
		    (set-actor-runnable! actor #f)
		    'try-again)
		  (let ((task (queue:get-first! actor-task-queue)))
		    ;; If there are more tasks for this actor, 
                    ;; requeue it.
		    (if (queue:empty? actor-task-queue)
			(set-actor-runnable! actor #f)
			(queue:add-to-end! runnable-actors actor))
		    ;; Return task to execute
		    task)))))))

(define (run)
  (let lp ((to-do (atomically run-one)))
    (cond ((eq? to-do 'try-again)
	   (lp (atomically run-one)))
	  ((eq? to-do 'nothing-to-do)
	   to-do)
	  (else
	   (to-do)
           (root-continuation 'go)))))

(define (add-to-runnable! actor)
  (atomically
   (lambda ()
     (if (actor-runnable? actor)
	 'already-runnable
	 (begin (set-actor-runnable! actor #t)
		(queue:add-to-end! runnable-actors actor)
		'made-runnable)))))

(define (add-to-tasks! actor task)
  (atomically
    (lambda ()
      (queue:add-to-end! (get-actor-task-queue actor) task)))
  (add-to-runnable! actor)
  'task-added)

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

(define (queue:get-first! queue)
  (if (null? (queue:front-ptr queue))
      (error "get-first! called with an empty queue" queue)
      (atomically
       (lambda ()
	 (let ((first (car (queue:front-ptr queue)))
	       (rest (cdr (queue:front-ptr queue))))
	   (queue:set-front-ptr! queue rest)
	   (if (null? rest)
	       (queue:set-rear-ptr! queue '()))
	   first)))))

(define (queue:add-to-end! queue item)
  (let ((new-pair (cons item '())))
    (atomically
     (lambda ()
       (cond ((null? (queue:front-ptr queue))
	      (queue:set-front-ptr! queue new-pair)
	      (queue:set-rear-ptr! queue new-pair))
	     (else
	      (set-cdr! (queue:rear-ptr queue) new-pair)
	      (queue:set-rear-ptr! queue new-pair))))))
  'done)

