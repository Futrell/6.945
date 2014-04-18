(define (try-two-ways thunk1 thunk2)
  (let ((value) (done? #f))
    (let ((thread1
	   (conspire:make-thread
	    conspire:runnable
	    (lambda ()
	      (set! value (thunk1))
	      (set! done? #t))))
	  (thread2
	   (conspire:make-thread
	    conspire:runnable
	    (lambda ()
	      (set! value (thunk2))
	      (set! done? #t)))))

      (conspire:switch-threads
       (lambda () done?))

      (conspire:kill-other-threads
       (list thread1 thread2))

      value)))

(define (test n1 n2)
  (with-conspiracy
      (lambda ()
	(try-two-ways
	 (lambda ()
	   (let lp ((n n1))
	     (if (= n 0)
		 'a-done
		 (begin
		   (if (= (remainder n 100000) 0)
		       (begin (display 'a)
			      (conspire:thread-yield)))
		   (lp (- n 1))))))
	 (lambda ()
	   (let lp ((n n2))
	     (if (= n 0)
		 'b-done
		 (begin
		   (if (= (remainder n 100000) 0)
		       (begin (display 'b)
			      (conspire:thread-yield)))
		   (lp (- n 1))))))))))

#|
(test 1000000 1200000)
ababababababababababab
;Value: a-done

(test 1200000 1000000)
babababababababababaa
;Value: b-done
|#

(define (test1 n1 n2)
  (with-time-sharing-conspiracy
      (lambda ()
	(try-two-ways
	 (lambda ()
	   (let lp ((n n1))
	     (if (= n 0)
		 'a-done
		 (begin
		   (if (= (remainder n 100000) 0)
		       (display 'a))
		   (lp (- n 1))))))
	 (lambda ()
	   (let lp ((n n2))
	     (if (= n 0)
		 'b-done
		 (begin
		   (if (= (remainder n 100000) 0)
		       (display 'b))
		   (lp (- n 1))))))))))

#|
(test1 1000000 1200000)
baabbaabbaabbaabbaabb
;Value: a-done

(test1 1200000 1000000)
babaabbaabbaabbaabbaa
;Value: b-done
|#

;;; Interesting example

;;; Suppose we want to search a list, that
;;; may be infinite (circular).	 We could
;;; use the fast algorithm, but sometimes
;;; go into an infinite loop, or we could
;;; use the slow algorithm that marks the
;;; list (with a hash table) but always
;;; works.  If the statistics are right,
;;; a better strategy is to time-share the
;;; two methods and take the one which
;;; finishes first:

(define (safe-mem? item lst)
  (let ((table (make-eq-hash-table)))
    (let lp ((lst lst))
      (if (pair? lst)
	  (if (hash-table/get table lst #f)
	      #f			;circular
	      (if (eq? item (car lst))
		  #t
		  (begin
		    (hash-table/put! table lst #t)
		    (lp (cdr lst)))))
	  #f))))

(define (unsafe-mem? item lst)
  (let lp ((lst lst))
    (if (pair? lst)
	(if (eq? item (car lst))
	    #t
	    (lp (cdr lst)))
	#f)))

#|
(define foo (list 'a 'b 'c 'd))
;Value: foo

(begin (set-cdr! (last-pair foo) foo) 'foo)
;Value: foo

(unsafe-mem? 'b foo)
;Value: #t

(unsafe-mem? 'e foo)
;Quit!

(safe-mem? 'b foo)
;Value: #t

(safe-mem? 'e foo)
;Value: #f
|#

(define (mem? item lst)
  (with-time-sharing-conspiracy
      (lambda ()
	(try-two-ways
	 (lambda ()
	   (unsafe-mem? item lst))
	 (lambda ()
	   (safe-mem? item lst))))))

#|
(mem? 'b foo)
;Value: #t

(mem? 'e foo)
;Value: #f
|#
