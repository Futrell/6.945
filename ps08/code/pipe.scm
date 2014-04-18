; make-pipe
; (pipe-writer p): makes a writer pw for pipe p. (pw x) writes x to p. 
; (pipe-reader p): makes a reader pr for pipe p. (pr) reads from p.


(define *done* (list '*done*))

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

; return will be pipe-writer
; 
