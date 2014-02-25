;;;; Scheme Regular Expression Language Implementation -- regexp.scm

(define (r:dot) ".")
(define (r:bol) "^")
(define (r:eol) "$")

(define (r:quote string)
  (r:seq
   (list->string
    (append-map (lambda (char)
                  (if (memv char chars-needing-quoting)
                      (list #\\ char)
                      (list char)))
                (string->list string)))))

(define chars-needing-quoting
  '(#\. #\[ #\\ #\^ #\$ #\*))

(define (r:char-from string)
  (case (string-length string)
    ((0) (r:seq))
    ((1) (r:quote string))
    (else
     (bracket string
              (lambda (members)
                (if (lset= eqv? '(#\- #\^) members)
                    '(#\- #\^)
                    (quote-bracketed-contents members)))))))

(define (r:char-not-from string)
  (bracket string
           (lambda (members)
             (cons #\^ (quote-bracketed-contents members)))))

(define (bracket string procedure)
  (list->string
   (append '(#\[)
           (procedure (string->list string))
           '(#\]))))

(define (quote-bracketed-contents members)
  (let ((optional
         (lambda (char) (if (memv char members) (list char) '()))))
    (append (optional #\])
            (remove (lambda (c)
		      (memv c chars-needing-quoting-in-brackets))
		    members)
            (optional #\^)
            (optional #\-))))

(define chars-needing-quoting-in-brackets
  '(#\] #\^ #\-))

;;; Means of combination for patterns

(define (r:seq . exprs)
  (string-append "\\(" (apply string-append exprs) "\\)"))

;;; An extension to POSIX basic regular expressions.
;;; Supported by GNU grep and possibly others.
(define (r:alt . exprs)
  (if (pair? exprs)
      (apply r:seq
             (cons (car exprs)
                   (append-map (lambda (expr)
                                 (list "\\|" expr))
                               (cdr exprs))))
      (r:seq)))


(define (r:repeat min max expr)
  (apply r:seq
         (append (make-list min expr)
                 (if (eqv? max min)
                     '()
                     (if max
                         (make-list (- max min)
                                    (r:alt expr ""))
                         (list expr "*"))))))

;;; Using system's grep.
(define (write-bourne-shell-grep-command expr filename)
  (display (bourne-shell-grep-command-string expr filename)))

(define (bourne-shell-grep-command-string expr filename)
  (string-append "grep -e "
                 (bourne-shell-quote-string expr)
                 " "
                 filename))

;;; Works for any string without newlines.
(define (bourne-shell-quote-string string)
  (list->string
   (append (list #\')
           (append-map (lambda (char)
                         (if (char=? char #\')
                             (list #\' #\\ char #\')
                             (list char)))
                       (string->list string))
           (list #\'))))


;;; This is MIT/Scheme specific and compatible with grep for the
;;; purposes of this code.

(load-option 'synchronous-subprocess)

(define (r:grep expr filename)
  (display expr)
  (newline)
  (let ((port (open-output-string)))
    (and (= (run-shell-command
             (bourne-shell-grep-command-string expr filename)
             'output port)
            0)
	 (r:split-lines (get-output-string port)))))

(define (r:grep* expr filename)
  (let ((port (open-output-string)))
    (display expr)
    (newline)
    (run-synchronous-subprocess "grep"
                                (list expr filename)
                                'output
                                port)
    (r:split-lines (get-output-string port))))


(define (r:split-lines string)
  (reverse
   (let ((end (string-length string)))
     (let loop ((i 0) (lines '()))
       (if (< i end)
	   (let ((j
		  (substring-find-next-char string i end #\newline)))
	     (if j
		 (loop (+ j 1)
		       (cons (substring string i j) lines))
		 (cons (substring string i end) lines)))
	   lines)))))

#|
;;; For example...

(pp (r:grep (r:seq (r:quote "a") (r:dot) (r:quote "c")) "tests.txt"))
("[00]. abc"
 "[01]. aac"
 "[02]. acc"
 "[03]. zzzaxcqqq"
 "[10]. catcatdogdog"
 "[12]. catcatcatdogdogdog")
;Unspecified return value

;;; And...

(pp (r:grep (r:alt (r:quote "foo") (r:quote "bar") (r:quote "baz"))
	    "tests.txt"))
("[05]. foo" "[06]. bar" "[07]. foo bar baz quux")
;Unspecified return value


(pp (r:grep (r:repeat 3 5 (r:alt (r:quote "cat") (r:quote "dog")))
	    "tests.txt"))
("[09]. catdogcat"
 "[10]. catcatdogdog"
 "[11]. dogdogcatdogdog"
 "[12]. catcatcatdogdogdog"
 "[13]. acatdogdogcats"
 "[14]. ifacatdogdogs"
 "[15]. acatdogdogsme")
;Unspecified return value

(pp
 (r:grep (r:seq " "
		(r:repeat 3 5 (r:alt (r:quote "cat") (r:quote "dog")))
		(r:eol)) 
         "tests.txt"))
("[09]. catdogcat" "[10]. catcatdogdog" "[11]. dogdogcatdogdog")
;Unspecified return value

(pp
 (r:grep
  (let ((digit 
	 (r:char-from "0123456789")))
    (r:seq (r:bol)
	   (r:quote "[")
	   digit
	   digit
	   (r:quote "]")
	   (r:quote ".")
	   (r:quote " ")
	   (r:char-from "ab")
	   (r:repeat 3 5 (r:alt "cat" "dog"))
	   (r:char-not-from "def")
	   (r:eol)))
  "tests.txt"))
("[13]. acatdogdogcats")
;Unspecified return value
|#
