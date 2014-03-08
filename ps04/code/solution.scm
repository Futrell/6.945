;;; 6.945 Problem Set 4
;;; Richard Futrell
;;; futrell@mit.edu

;;; Problem 4.1

(defhandler apply
  (lambda (procedure-vector operands calling-environment)
    (define (map-procs procs data)
      (if (null? procs)
	  '()
	  (cons (apply (car procs) data calling-environment)
		(map-procs (cdr procs) data))))
    (define (vector-map-procs procs data)
      (list->vector (map-procs (vector->list procs) data)))
    (vector-map-procs procedure-vector operands))
  vector?)

#| Tests

eval> ((vector cos sin) .6)
#(.8253356149096783 .5646424733950354)

eval> ((vector + -) 3 4)
#(7 -1)

eval> ((vector) 3)
#()

eval> (vector (cos .6) (sin .6))
#(.8253356149096783 .5646424733950354)

|#

;;; Problem 4.2

;;; Suppose I have defined a function "hello" and have a list called
;;; "cats". Now suppose I call (map hello cats) inside our interpreter
;;; (which I'll call Bizarro-Scheme for this problem set, as opposed
;;; to Scheme). Then map will evaluate to Scheme's underlying map
;;; function, hello will evaluate to the Bizarro-Scheme's compound
;;; procedure representation, and cats will evaluate to its list. But
;;; Scheme's map function expects its first argument to be a function,
;;; not a Bizarro-Scheme compound function representation, so it will
;;; die. On the other hand, if I call (map + '(1 2 3) '(4 5 6)) in
;;; Bizarro-Scheme, it will work fine since + will be evaluated to
;;; the primitive function. Example:

#| Example

eval> (map (lambda (x) x) '(1 2 3))
;The object #[compound-procedure 39] is not applicable.

eval> (map + '(1 2 3) '(4 5 6))
(5 7 9)

|#

;;; We can fix this by making the Bizarro-Scheme compound function
;;; representation compatible with Scheme's (i.e. represent it as 
;;; #[compound-procedure n] where we would have to calculate the correct
;;; value of n given already existing Scheme procedures).

;;; Problem 4.3

;;; A. First let's make the new -, +, *, and /:

(define (make-numerical-procedure-taking-unbound-variables proc name base)
  (define (reduce-args args so-far) ; evaluate when possible
    (if (null? args)
	(if (equal? so-far base)
	    '()
	    (list so-far))
	(let ((first-thing (car 
			    args)))
	  (if (number? first-thing)
	      (reduce-args (cdr args) (proc first-thing so-far))
	      (if (or (symbol? first-thing) 
			   (pair? first-thing))
		  (if (equal? so-far base)
		      (cons first-thing (reduce-args (cdr args) base))
		      (cons so-far 
			    (cons first-thing (reduce-args (cdr args)
							   base))))
		  (error "bad arguments to" name))))))

  (lambda args
    (if (every number? args)
	(apply proc args '())
	(cons name (reduce-args args base)))))

(define + (make-numerical-procedure-taking-unbound-variables + '+ 0))
(define - (make-numerical-procedure-taking-unbound-variables - '- 0))
(define * (make-numerical-procedure-taking-unbound-variables * '* 1))
(define / (make-numerical-procedure-taking-unbound-variables / '/ 1))

#| Tests

(+ 1 2)
;Value: 3

(+ 1 'a)
;Value: (+ 1 a)

(+ 1 (+ 3 'a))
;Value: (+ 1 (+ 3 a))

(+ (* 2 3) (* 4 5))
;Value: 26

(+ (* 'a 3) (* 4 5))
;Value: (+ (* a 3) 20)

(+ 1 2 3 (+ 4 'a) 5 6 (+ 7 'b) 8 9)
;Value: (+ 6 (+ 4 a) 11 (+ 7 b) 17) ; pass this into algebra-3 if you
;want something simpler :)

|#

;;; Now we modify eval to take advantage of the new numerical
;;; operations.

(define ALLOW-SELF-EVALUATING-SYMBOLS #t)

(define (lookup-variable-value var env)
  (let plp ((env env))
    (if (eq? env the-empty-environment)
	(if (environment-bound? generic-evaluation-environment var)
	    (lookup-scheme-value var)
	    the-unspecified-value)
        (let scan
            ((vars (vector-ref env 0))
             (vals (vector-ref env 1)))
          (cond ((null? vars) (plp (vector-ref env 2)))
                ((eq? var (car vars)) (car vals))
                (else (scan (cdr vars) (cdr vals))))))))

(defhandler eval
  (lambda (var env)
    (let ((value (lookup-variable-value var env))) 
      (if (not (eq? value the-unspecified-value))
	  value
	  (if ALLOW-SELF-EVALUATING-SYMBOLS
	      var
	      (error "Unbound variable:" var)))))
  variable?)

#| Tests

eval> (+ 1 2)
3

eval> (+ 1 a)
(+ 1 a)

eval> (+ 1 (+ 3 a))
(+ 1 (+ 3 a))

eval> (+ (* 2 3) (* 4 5))
26

eval> (+ (* a 3) (* 4 5))
(+ (* a 3) 20)

eval> (+ 1 2 3 (+ 4 a) 5 6 (+ 7 b) 8 9)
(+ 6 (+ 4 a) 11 (+ 7 b) 17)

eval> (/ 3 (* 100 (- 4 b)))
(/ 3 (* 100 (- 4 b)))

|# 

;;; B. Now we augment apply to allow literal functions.

(defhandler apply
  (lambda (procedure operands calling-environment)
    (cons procedure operands))
  symbol?)

#| Tests

eval> (+ (f 3) (* 4 5))
(+ (f 3) 20)

eval> (define (foo x) x)
eval> (+ (foo 3) 4)
7

eval> (+ (foo c) 4)
(+ c 4)

eval> f
f

|#

;;; Problem 4.4
;;; 
;;; A. Kons gives us an easy implementation of even streams which
;;; allows us to use existing functions like car and cdr rather than
;;; having to know that an object is a stream, requiring the use of
;;; stream-car etc. In many ways, the lazy kons seems more basic than
;;; the strictly evaluated cons: Pairs often exist only to be
;;; traversed by car-cdr recursion, and in that setting it makes
;;; perfect sense to posit that the next value is only computed upon
;;; calling car against it; storing all future values is unnecessary
;;; in this very common setting. With kons, that same iteration
;;; paradigm works with infinite sequences; with cons, only finite
;;; sequences can be traversed this way. 

;;; Here is an illustration of the apparent basicness of kons. 
;;; In a previous assignment, I made a generic sequence:map operator
;;; which converted strings, vectors, etc., to lists, mapped a
;;; function over the lists, and returned them as the appropriate
;;; type; but this way of implementing sequence:map would fail for
;;; infinite streams since the stream->list step would not
;;; terminate. On the other hand, if I converted everything to a kons
;;; list or a stream behind the scenes, then everything would work
;;; fine. Kons lists are a more general data structure for iteration
;;; than cons lists.

;;; B. The problem is the mutually recursive definitions of the
;;; variables y and dy, where dy is invoked in the definition of y
;;; though it hasn't been defined yet, which produces this error:

#| Example
(ref-stream (solve (lambda (x) x) 1 0.001) 1000)
;Unbound variable: dy
|#

;;; We can redefine integral to take the parameter (integrand lazy memo)
;;; instead of integrand, and then it works, giving 2.717. More
;;; generally this should be applied to all the stream arguments of
;;; functions taking streams.


;;; Problem 4.5

;;; A. Here is a case where there is an advantage to not memoizing in
;;; kons. Suppose we have a stream of items and we want to get the
;;; googleplexth one. If kons is memoized, (ref-stream items
;;; <googleplex>) will generate and store the items zero through
;;; googleplex, and fill the memory of the machine far before we get
;;; the item we want. But if kons is not memoized, (ref-stream items
;;; <googleplex>) will take a long time, but will not fill up my
;;; memory.

;;; B. We consider qons, a potential way to define kons:

#|
(define (qons (a lazy memo) (d lazy memo))
  (cons a d))
|#

;;; Since cons is strict, it will evaluate a and d in order to return
;;; the construct from qons, thus nullifying their laziness. 

;;; C. If we used kons instead of cons, we would require potentially
;;; unbounded memory since we are dragging around environments. Also,
;;; lazy evaluation is potentially dangerous when programs have state,
;;; resulting in the result of an operation like (car x), where x has
;;; been created by kons, dependent on when the car procedure is
;;; invoked; this is especially bad in programs with I/O where order
;;; matters. More generally, it is easy to simulate kons in a system
;;; with cons, by using thunks, but it is less straightforward to
;;; simulate cons in a system where kons is basic.

;;; Problem 4.6
;;;
;;; Keyword arguments are useful to me because I forget which
;;; positional arguments correspond to which parameters when the
;;; number of arguments exceeds 3. Sometimes I even forget the order
;;; for map! What if we could pass in arguments by name instead of by
;;; order? It turns out that's easy to set up in Bizarro-Scheme
;;; (though not totally additive).

;;; The keyword syntax I make here is this: I can pass an argument in
;;; the form (: name value), in which case value is taken to be the
;;; value of the parameter named name.

(define (operand-is-keyword? operand)
  (and (pair? operand)
       (eq? (car operand) ':)))

(define (keyword-name exp)
  (cadr exp))

(define (keyword-value exp)
  (caddr exp))

(define (strip-keyword-syntax keyword-operands)
  (map cdr keyword-operands))

(define (read-keyword-arguments parameters 
				operands
				calling-environment)
  ;; Go through the remaining parameters and match them up with their
  ;; corresponding keyword-marked operands.
  (if (null? parameters)
      '()
      (let ((parameter (car parameters)))
	(let ((parameter-operand-pair 
	       (assq parameter operands)))
	  (if (not parameter-operand-pair)
	      (error "No keyword found for" parameter))
	  (let ((variable (car parameter-operand-pair))
		(value (evaluate-procedure-operand 
			parameter
			(cadr parameter-operand-pair)
			calling-environment)))
	    (cons (list variable value)
		  (read-keyword-arguments (cdr parameters)
					  operands
					  calling-environment)))))))

(define (read-arguments parameters operands calling-environment)
  (if (null? parameters)
      '()
      (let ((operand (car operands)) 
	    (parameter (car parameters)))
	(if (operand-is-keyword? operand)
	    (read-keyword-arguments parameters 
				    (strip-keyword-syntax operands)
				    calling-environment)
	    (cons (list parameter 
			(evaluate-procedure-operand parameter
						    operand
						    calling-environment))
		  (read-arguments (cdr parameters)
				  (cdr operands)
				  calling-environment))))))

(defhandler apply
  (lambda (procedure operands calling-environment)
    (if (not (= (length (procedure-parameters procedure))
                (length operands)))
        (error "Wrong number of operands supplied"))
    (let ((vars-and-vals 
	   (read-arguments (procedure-parameters procedure)
			   operands
			   calling-environment)))
      (let ((variables (map car vars-and-vals))
	    (values (map cadr vars-and-vals)))
	(eval (procedure-body procedure)
	      (extend-environment variables 
				  values 
				  (procedure-environment
				   procedure))))))
  compound-procedure?)

#| Test

eval> (define (bake-cake sweetener how-long-in-oven oven-temperature
  height icing-color)
  (display "Baking a cake ")
  (display height)
  (display " inches high, using ")
  (display sweetener)
  (display ", for ")
  (display how-long-in-oven)
  (display " minutes at ")
  (display oven-temperature)
  (display " degrees, with ")
  (display icing-color)
  (display " icing.")
  (newline)
  'cake)

eval> (bake-cake (: sweetener 'sucralose) (: height 5) (: icing-color 'mauve)
                 (: how-long-in-oven 15) (: oven-temperature 350))
Baking a cake 5 inches high, using sucralose, for 15 minutes at 350
degrees, with mauve icing.
cake

eval> (bake-cake 'sucralose 15 350 5 'green)
Baking a cake 5 inches high, using sucralose, for 15 minutes at 350
degrees, with green icing.
cake

eval> (bake-cake 'sucralose 15 350 (: icing-color 'purple) (: height
100))
Baking a cake 100 inches high, using sucralose, for 15 minutes at 350
degrees, with purple icing\
.
cake

eval> (bake-cake 'sucralose 15 350 (: icing-color 'purple) (: honk
100))
;No keyword found for height

|#

