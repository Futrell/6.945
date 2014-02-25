;;; 6.945 Problem Set 1
;;; 
;;; Richard Futrell
;;; futrell@mit.edu

(load "regexp.scm")

;;; Problem 1.1
;;;
;;; Here are the definitions for * and +, and also some test cases and
;;; code for running them.

(define (r:* expr)
  ;; Repeat an expression unboundedly.
  (r:repeat 0 #f expr))

(define (r:+ expr)
  ;; Repeat an expression at least once.
  (r:repeat 1 #f expr))

(define (r:? expr) ; Bonus!
  ;; Repeat an expression 0 or 1 times.
  (r:repeat 0 1 expr))

(define r:test-cases
  ;; Test cases for regular expressions, represented as 3-tuples where
  ;; the first item is the regular expression, the second is a test
  ;; string, and the last item is whether the expression should match
  ;; to the string or not.
  ;; These are a mixture of tests from the Perl unit tests, the
  ;; problem set examples, and my own invention.
  '(((r:quote "abc") "abc" #t)
    ((r:quote "abc") "xbc" #f)
    ((r:quote "abc") "abx" #f)
    ((r:quote "abc") "ababc" #t)
    ((r:quote ".?{|t+") ".?{|t+" #t) ; test BRE vs ERE quoting
    ((r:quote ".?{|t+") "a{" #f)
    ((r:quote ".?{|t+") "ttt" #f)
    ((r:repeat 0 #f (r:seq (r:alt (r:quote "cat") (r:quote "dog"))
			   (r:quote "qux"))) "dogquxdogqux" #t)
    ((r:seq (r:quote "cat") (r:repeat 0 #f (r:quote "dog")) (r:quote "qux")) "catdogqux" #t)
    ((r:seq (r:quote "cat") (r:repeat 0 #f (r:quote "dog")) (r:quote "dogqux")) "catdogqux" #t)
    ((r:seq (r:quote "cat") (r:repeat 1 #f (r:quote "dog")) (r:quote "dogqux")) "catdogdogqux" #t)
    ((r:seq (r:quote "cat") (r:repeat 1 #f (r:quote "dog")) (r:quote "dogqux")) "catdogcar" #f)
    ((r:seq (r:bol) (r:quote "abc") (r:eol)) "abc" #t)
    ((r:seq (r:bol) (r:quote "abc") (r:eol)) "aabc" #f)
    ((r:bol) "abc" #t)
    ((r:eol) "abc" #t)
    ((r:seq (r:quote "a") (r:dot) (r:quote "c")) "abc" #t)
    ((r:seq (r:quote "a") (r:dot) (r:quote "c")) "axc" #t)
    ((r:seq (r:quote "a") (r:repeat 0 #f (r:dot)) (r:quote "c")) "ac" #t)
    ((r:seq (r:quote "a") (r:char-from "bc") (r:quote "d")) "abc" #f)
    ((r:seq (r:quote "a") (r:char-from "bc") (r:quote "d")) "acd" #t)
    ((r:seq (r:quote "a") (r:char-from "bcd") (r:quote "e")) "ace" #t)
    ((r:seq (r:quote "a") (r:char-from "-b")) "a-" #t)
    ((r:seq (r:quote "a") (r:char-from "]") (r:quote "b")) "a]b" #t)
    ((r:seq (r:quote "a") (r:char-not-from "bc") (r:quote "d")) "aed" #t)
    ((r:seq (r:quote "a") (r:char-not-from "bc") (r:quote "d")) "abd" #f)
    ((r:seq (r:quote "a") (r:char-not-from "-bc") (r:quote "d")) "a-c" #f)
    ((r:seq (r:eol) (r:quote "b")) "b" #f)
    ((r:seq (r:quote "a") (r:* (r:quote "b")) (r:quote "bc")) "abbbbc" #t)
    ((r:seq (r:quote "a") (r:repeat 4 5 (r:quote "b")) (r:quote "bc")) "abbbbc" #f)
    ((r:seq (r:quote "a") (r:alt (r:quote "b") (r:quote "c")) (r:quote
							       "d")) "abd" #t)
    ((r:seq (r:quote "a") (r:alt (r:quote "b") (r:quote "c")) (r:quote
                                                               "d")) "aed" #f)
    ((r:* (r:alt (r:+ (r:quote "a")) (r:quote "b"))) "ab" #t)
    ((r:+ (r:alt (r:+ (r:quote "a")) (r:quote "b"))) "aaaa" #t)
    ((r:* (r:char-not-from "ab")) "cde" #t)
    ((r:seq (r:alt (r:quote "a") (r:quote "b") (r:quote "c") 
		   (r:quote "d") (r:quote "e")) (r:quote "f")) "ef" #t)
    ((r:seq (r:quote "a") (r:* (r:char-from "bc"))
	    (r:* (r:quote "c"))) "abc" #t)
    ((let ((digit
	    (r:char-from "0123456789")))
       (r:seq (r:bol)
	      (r:quote "[")
	      digit
	      digit
	      (r:quote "]")
	      (r:quote ".")
	      (r:quote " ")
	      (r:char-from "ab")
	      (r:repeat 3 5 (r:alt (r:quote "cat") (r:quote "dog")))
	      (r:char-not-from "def")
	      (r:eol))) "[09]. acatdogdogcats" #t)
    ((let ((digit
	    (r:char-from "0123456789")))
       (r:seq (r:bol)
	      (r:quote "[")
	      digit
	      digit
	      (r:quote "]")
	      (r:quote ".")
	      (r:quote " ")
	      (r:char-from "ab")
	      (r:repeat 3 5 (r:alt (r:quote "cat") (r:quote "dog")))
	      (r:char-not-from "def")
	      (r:eol))) "[10]. ifacatdogdogs" #f)
    ((let ((digit
	    (r:char-from "0123456789")))
       (r:seq (r:bol)
	      (r:quote "[")
	      digit
	      digit
	      (r:quote "]")
	      (r:quote ".")
	      (r:quote " ")
	      (r:char-from "ab")
	      (r:repeat 3 5 (r:alt (r:quote "cat") (r:quote "dog")))
	      (r:char-not-from "def")
	      (r:eol))) "[11]. ifacatdogdogsme" #f)
    ))

(define (eval-regexp quoted-expression)
  (eval quoted-expression user-initial-environment))

(define (r:run-tests test-cases grep-proc eval-proc)
  ;; Test the regular expressions for the given test cases, printing
  ;; out cases where they fail, and printing nothing if all tests
  ;; succeed.
  ;; Arguments:
  ;;    test-cases: List of 3-tuple test cases,
  ;;    grep-proc: The procedure to grep with, i.e. r:grep or r:egrep
  ;;    eval-proc: The procedure to evaluate and compile the quoted
  ;;    Scheme regular expression in the test case.
  ;;
  (define temp-filename "tmptests.txt")
  
  (define (write-to-file filename string)
    (let ((outport (open-output-file filename)))
      (display string outport)
      (newline outport)
      (close-output-port outport)))

  (define (run-test test-case)
    (let ((test-string (cadr test-case))
	  (test-query (car test-case)))
      (begin
	(write-to-file temp-filename test-string)
	(grep-proc (eval-proc test-query) temp-filename))))

  (define (test-case-correct? result test-case)
    (let ((expected-result (caddr test-case)))
      (if (not expected-result)
	  (not result)
	  (not (not result)))))

  (define (check-test-case test-case)
    (let ((result (run-test test-case)))
      (if (test-case-correct? result test-case)
	  #t
	  (begin
	    (display "******************* Test failed *****************")
	    (newline)
	    (display "Case: ") (display test-case)
	    (newline)
	    #f))))

  (define (all lst)
    (reduce (lambda (x y) (and x y)) #t lst))
  
  (all (map check-test-case test-cases)))

;#| Tests

(r:run-tests r:test-cases r:grep eval-regexp)
;Value: #t

;|# 

;;; Problem 1.2
;;;
;;; a. Louis's suggestion is to replace the expression (r:alt expr "")
;;; in r:repeat with (r:repeat 0 1 expr). But if this were the case
;;; then (r:repeat 0 1 expr) would evaluate to (r:repeat 0 1
;;; expr). Ha-ha.
;;;
;;; b. Bonnie's proposal is better for two reasons:
;;; (1) It produces more modular. The procedure for r:? will be
;;; separate from re:repeat and available for reuse elsewhere.
;;;
;;; (2) Also, Bonnie's solution will produce shorter regular
;;; expressions. Alyssa's suggestion would require repeating the
;;; expression x at least n*(m-n) times in order to evaluate (r:repeat
;;; n m x). For large values of n or m this could produce expressions
;;; that are too long for the shell to evaluate!
;;;
;;; c. Ben's proposal uses the built-in machinery of Basic Regular
;;; Expressions. It produces shorter expressions, which will likely
;;; run faster since they take advantage of interval expressions,
;;; which are built into the grep utility and thus (presumably)
;;; relatively optimized. It does not use the symbol ?, which is only
;;; part of extended regular expressions, so it will be available on
;;; systems that can handle BREs or EREs. 
;;; 
;;; d. Here is the code for interval expressions:

(define (r:repeat min max expr)
  (define (number->string-if-true item)
    (if item
	(number->string item)
	""))
  (string-append (r:seq expr)
		 "\\{" ; this is BRE syntax only, watch out!
		 (number->string-if-true min)
		 ","
		 (number->string-if-true max)
		 "\\}"))

(define (r:repeat-n-times n expr)
  (r:repeat n n expr))


;#| Tests

(define r:test-cases
  (append r:test-cases
	  '(((r:seq (r:bol) (r:repeat-n-times 3 (r:quote "cat")) (r:eol)) "catcatcat" #t)
	    ((r:seq (r:bol) (r:repeat-n-times 3 (r:quote "cat")) (r:eol)) "catcat" #f)
	    ((r:seq (r:bol) (r:repeat-n-times 3 (r:quote "cat")) (r:eol)) "catcatcatcat" #f))))

(r:run-tests r:test-cases r:grep eval-regexp)
;Value: #t

;|#

;;; Problem 1.3
;;;
;;; In order to make backreferences work, we want to reduce nesting to
;;; that implied by the expression r:alt, r:repeat, r:repeat-n-times, 
;;; and user-supplied r:seq (except at the top level). For instance
;;; where the code currently generates "\(\(a\).\(c\)\)" we just want
;;; "a.c". 
;;;
;;; Let's start by remaking the regular expression components.
;;; The procedures here serve to take the scheme s-expression regular
;;; expressions and introduce the conceptual confusions that will be
;;; necessary to compile to BREs or EREs. The procedures now return
;;; lists which will have to be "compiled" later to the actual BRE or
;;; ERE strings. This representation makes it easy to code up the
;;; logic about when to add parentheses.
;;;
;;; Some subtle points are noted in the comments to the code.

(define (r:dot) (list 'dot))
(define (r:bol) (list 'bol))
(define (r:eol) (list 'eol))

(define (r:quote string)
  (list 'quote-string string))

(define (r:char-from string)
  (list 'char-from string))

(define (r:char-not-from string)
  (list 'char-not-from string))

(define (r:seq . exprs)  
  (define (capture-if-needed expr)
    (if (eq? (car expr) 'alt) ; If an alt is inside a sequence, then
			      ; it needs to be grouped, which in BREs
			      ; and EREs implies capturing.
	(r:capture expr)
	expr))
  (cons 'seq (map capture-if-needed exprs)))

(define (r:capture group)
  (list 'capture group))

(define (r:alt . exprs) ; This will only be grouped/captured when it
			; is embedded in a seq. If I group/capture it
			; here, then we might end up capturing it twice.
   (cons 'alt exprs))

(define (unit? expr)
  ;; Determine whether expr is a unit: a captured group, a bracket
  ;; expression, an anchor, an interval expression, 
  (let ((expr-tag (car expr)))
    (or (eq? expr-tag 'capture) 
	(eq? expr-tag 'char-from)
	(eq? expr-tag 'char-not-from)
	(eq? expr-tag 'bol)
	(eq? expr-tag 'eol)
	(eq? expr-tag 'dot)
	(eq? expr-tag 'backref)
	(and (eq? expr-tag 'quote-string)
	     (eq? (string-length (cadr expr)) 1)))))

(define (r:repeat min max expr) ; Only capture/group if the thing
				; repeated is more than one character
				; or not already grouped.
  (let ((expr (if (unit? expr)
		  expr
		  (r:capture expr))))
    (list 'repeat min max expr)))

(define (r:repeat-n-times n expr) ; Let's reuse r:repeat as much as
				  ; possible so we don't have to
				  ; repeat ourselves; also by not
				  ; using * and +, we avoid having to
				  ; escape them or not.
  (r:repeat n n expr))

(define (r:* expr)
  (r:repeat 0 #f expr))

(define (r:+ expr)
  (r:repeat 1 #f expr))


;;; Now we define how to compile these expressions to BREs by defining
;;; procedures seq, capture, etc., which produce strings. This means
;;; that the expression passed to r:grep and its ilk has to be first
;;; compiled into a BRE string using (r:compile).


(define (dot) ".")
(define (bol) "^")
(define (eol) "$")

(define (quote-string string)
  (list->string
   (append-map (lambda (char)
		 (if (memv char chars-needing-quoting)
		     (list #\\ char)
		     (list char)))
	       (string->list string))))

(define (char-from string)
  (case (string-length string)
    ((0) "")
    ((1) (quote-string string))
    (else
     (bracket string
              (lambda (members)
                (if (lset= eqv? '(#\- #\^) members)
                    '(#\- #\^)
                    (quote-bracketed-contents members)))))))

(define (char-not-from string)
  (bracket string
           (lambda (members)
             (cons #\^ (quote-bracketed-contents members)))))

(define (alt . exprs)
  (if (pair? exprs)
      (apply string-append
	     (cons (car exprs)
		   (append-map (lambda (expr)
				 (list "\\|" expr))
			       (cdr exprs))))
      ""))

(define (repeat-n-times n expr)
  (string-append expr
                 "\\{"
                 (number->string n)
                 "\\}"))

(define (repeat min max expr)
  (define (number->string-if-true item)
    (if item
        (number->string item)
	""))
  (string-append expr
                 "\\{"
                 (number->string-if-true min)
                 ","
                 (number->string-if-true max)
                 "\\}"))

(define (seq . exprs)
  (apply string-append exprs))

(define (capture group)
  (string-append "\\(" group "\\)"))

(define (r:compile-bre expression)
  (eval expression user-initial-environment)) ; For now!

(define (eval-bre quoted-expression)
  (r:compile-bre (eval quoted-expression user-initial-environment)))

;#| Tests

(r:run-tests r:test-cases r:grep eval-bre)
;Value: #t

;|#

;;; Here is an example that is representative of how this works.

(r:seq (r:quote "a") (r:alt (r:quote "b") (r:quote "c")) (r:quote "d"))

;Value: (seq 
;	(quote-string "a") 
;	(capture (alt (quote-string "b") (quote-string "c"))) 
;	(quote-string "d"))

;;; That is the intermediate representation which clearly delineates
;;; the part to be parenthesized (in 'capture). Evaluating that will
;;; produce the desired BRE. To do it all in one step:

(r:compile-bre (r:seq (r:quote "a") (r:alt (r:quote "b") (r:quote "c"))
		   (r:quote "d")))

;Value: "a\\(b\\|c\\)d"

;;; In this case we first create the alt-expression b|c, and only put
;;; it in the capture procedure when it is embedded in a sequence. 
;;; If we made alt a group upon its creation, we might create
;;; an unnecessary top-level group if the expression were just a|b,
;;; or we might cause double capturing when that is embedded within
;;; another expression.
;;;
;;; If there were no parentheses then the meaning would be "ab or
;;; cd". If b|c were not adjacent to "a" and "d", then the expression
;;; would just be "b\|c" with no parentheses, avoiding a needless
;;; capturing group. This is easy because the procedure r:seq detects
;;; alt-expressions and groups them.

;;; Problem 1.4
;;;
;;; Now we can implement backreferences with r:backref, which
;;; refers back to an expression that was captured with r:alt,
;;; r:repeat, r:* or r:+ if they modified sequences, or segments
;;; explicitly captured with r:capture. 

(define (r:backref number)
  (list 'backref number))

(define (backref number)
  (string-append "\\\\" (number->string number)))

;#| Tests

(define r:test-cases
  (append r:test-cases
          '(((r:seq (r:capture (r:+ (r:dot))) (r:quote " hello ") (r:backref 1))
	     "cat hello cat" #t)
	    ((r:seq (r:+ (r:seq (r:quote "m") (r:dot)))
		    (r:quote " hello ")
		    (r:backref 1)
		    (r:eol))
	     "mimi hello mi" #t)
	    ((r:seq (r:+ (r:seq (r:quote "m") (r:dot)))
		    (r:quote " hello ")
		    (r:backref 1)
		    (r:eol))
	     "mimi hello mimi" #f)
	    ((r:seq (r:capture (r:quote "one")) 
		    (r:quote "two")
		    (r:capture (r:quote "three"))
		    (r:backref 2) (r:backref 1))
	     "onetwothreethreeone" #t)
	    ((r:seq (r:repeat-n-times 3 (r:alt (r:quote "cat")
					       (r:quote "dog")))
		    (r:backref 1))
	     "catcatcatcat" #t)
	    ((r:seq (r:repeat-n-times 3 (r:alt (r:quote "cat")
					       (r:quote "dog")))
		    (r:backref 1))
	     "catcatcatdog" #f)
	    ((r:seq (r:repeat-n-times 3 (r:capture (r:dot)))
		    (r:backref 1))
	     "catt" #t))))

(r:run-tests r:test-cases r:grep eval-bre)

;|#

;;; Problem 1.5
;;;
;;; a. EREs add a number of constructs to BREs and have different quoting
;;; conventions. EREs include the symbols ? and + and | for
;;; alternation (though these are present in BREs for GNU
;;; grep). Furthermore, in BREs, special characters except . have a special
;;; meaning when preceded by \; in EREs, they have a special meaning
;;; UNLESS they are preceded by \ (or in brackets).
;;;
;;; By using interval expressions to translate r:+ and r:*, we avoid
;;; having to deal with ? and + and their quoting issues, other than
;;; to add them to the list of characters that must be quoted. To
;;; translate to EREs, all we need to do is modify what characters
;;; need to be quoted and how they are quoted (adding vs. omitting
;;; the \). Fortunately, since GNU grep allows expressions with | for
;;; BREs, we have already handled that.
;;;
;;; b. The architecture above allows us to "compile" to both EREs and
;;; BREs from the initial scheme expression. There are two compilation
;;; steps. First, the user-specified expression, composed using the
;;; procedures prefixed with r:, generates a list-based intermediate
;;; representation which adds capturing groups that BRE and ERE syntax
;;; force upon us, and generally enforces the constraints of that
;;; syntax. The lists of the intermediate representation, when
;;; evaluated as code, return BRE syntax. 
;;;
;;; In order to compile to EREs and BREs, we change the last step by
;;; making ERE versions of all the translation procedures, then
;;; evaluating the intermediate representation in an environment with
;;; the ERE procedures rather than the BRE procedures.
;;;
;;; Compilation to further regular expression syntaxes is possible in
;;; this way. Some particularly divergent syntaxes, such as Python
;;; which allows for non-capturing groups, would require modifying
;;; both the first compilation step and the second one in this way. 
;;; 
;;; I'm pretty much stabbing in the dark about how to make
;;; environments like this in Scheme. The internet also tells me that
;;; this style of using environments as first-class objects is
;;; "heresy", but the arguments against it seem to be based on the
;;; ability to do speed optimizations.
;;;
;;; c. Here is the implementation:
;;;
;;; First some stuff in common between EREs and BREs:

(define (dot) ".")
(define (bol) "^")
(define (eol) "$")

(define (quote-string-in-env env)
  (lambda (string)
    (list->string
     (append-map (lambda (char)
		   (if (memv char (environment-lookup env
						      'chars-needing-quoting))
		       (list #\\ char)
		       (list char)))
		 (string->list string)))))

(define (char-from-in-env env)
  (lambda (string)
    (case (string-length string)
      ((0) "")
      ((1) (quote-string string))
      (else
       (bracket string
		(lambda (members)
		  (if (lset= eqv? '(#\- #\^) members)
		      '(#\- #\^)
		      ((environment-lookup env
					   'quote-bracketed-contents)
		       members))))))))


(define (char-not-from-in-env env)
  (lambda (string)
  (bracket string
           (lambda (members)
             (cons #\^ ((environment-lookup env
					    'quote-bracketed-contents)
			members))))))


(define (seq . exprs)
  (apply string-append exprs))


;;; BRE-Specific Definitions

(define bre-env (extend-top-level-environment user-initial-environment))

(environment-define bre-env 'chars-needing-quoting '(#\. #\[ #\\ #\^
						     #\$ #\*))
(environment-define bre-env 'chars-needing-quoting-in-brackets '(#\]
								 #\^
								 #\-))

(environment-define bre-env 'quote-string
		    (quote-string-in-env bre-env))

(environment-define bre-env 'char-from
		    (char-from-in-env bre-env))

(environment-define bre-env 'char-not-from
		    (char-not-from-in-env bre-env))

(environment-define bre-env 'alt
		    (lambda exprs
  (if (pair? exprs)
      (apply string-append
	     (cons (car exprs)
		   (append-map (lambda (expr)
				 (list "\\|" expr))
			       (cdr exprs))))
      "")))

(environment-define bre-env 'repeat
		    (lambda (min max expr)
  (define (number->string-if-true item)
    (if item
        (number->string item)
	""))
  (string-append expr
                 "\\{"
                 (number->string-if-true min)
                 ","
                 (number->string-if-true max)
                 "\\}")))

(environment-define bre-env 'capture
		    (lambda (group)
  (string-append "\\(" group "\\)")))

(environment-define bre-env 'backref
		    (lambda (number)
  (string-append "\\\\" (number->string number))))

(define (r:compile-bre expression)
  (eval expression bre-env)) 

;#| Tests

(r:run-tests r:test-cases r:grep eval-bre)
;Value: #t

;|# 


;;; ERE-Specific Definitions

(define ere-env (extend-top-level-environment
		 user-initial-environment))

(environment-define ere-env 'chars-needing-quoting '(#\. #\[ #\\ #\^
						     #\$ #\* #\? #\+
						     #\|))

(environment-define ere-env 'quote-string
		    (quote-string-in-env ere-env))

(environment-define ere-env 'char-from
		    (char-from-in-env ere-env))

(environment-define ere-env 'char-not-from
		    (char-not-from-in-env ere-env))


(environment-define ere-env 'alt
		    (lambda exprs
  (if (pair? exprs)
      (apply string-append
	     (cons (car exprs)
		   (append-map (lambda (expr)
				 (list "|" expr))
			       (cdr exprs))))
      "")))

(environment-define ere-env 'repeat
		    (lambda (min max expr)
  (define (number->string-if-true item)
    (if item
        (number->string item)
	""))
  (string-append expr
                 "{"
                 (number->string-if-true min)
                 ","
                 (number->string-if-true max)
                 "}")))

(environment-define ere-env 'capture
		    (lambda (group)
  (string-append "(" group ")")))

(environment-define ere-env 'backref
		    (lambda (number)
  (string-append "\\" (number->string number))))

(define (r:compile-ere expression)
  (eval expression ere-env)) 

(define (bourne-shell-egrep-command-string expr filename)
  (string-append "egrep -e "
                 (bourne-shell-quote-string expr)
                 " "
                 filename))

(define (r:egrep expr filename)
  (display expr)
  (newline)
  (let ((port (open-output-string)))
    (and (= (run-shell-command
             (bourne-shell-egrep-command-string expr filename)
             'output port)
            0)
         (r:split-lines (get-output-string port)))))

(define (eval-ere quoted-expression)
  (r:compile-ere (eval quoted-expression user-initial-environment)))

;#| Tests

(r:run-tests r:test-cases r:egrep eval-ere)
;Value: #t

;;; Some BRE and ERE-related test cases are listed in the initial
;;; definition of the test cases.

;|#
