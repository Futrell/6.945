;;; 2-7-14
;;; We use LISP because it is easy to demonstrate certain things.
;;; It is dangerous; we are interested in dangerous programming.

(define (compose f g)
  (lambda (x) (f (g x))))

(define (identity x) x)

(define (iterate f n)
  (if (= n 0)
      identity 
      (compose f (iterate f (- n 1)))))

(define (map f list)
  (if (null? lst)
      '()
      (cons (f (car lst))
	    (map f (cdr list)))))

(define foo
  '(define compose
     (lambda (f g)
       (lambda (x)
	 (f (g x))))))

(car foo)
;Value: define

(cadr foo)
;Value: compose

(define (mangle expression)
  (if (and (eq? (car expression) 'define)
	   (pair? (addr expression))
	   (eq? (caaddr expression) 'lambda))
      (mangle
       (cons 'define
	     (cons (cons (cadr expression)
		   (cadr (caddr expression)))
	     (cddr (caddr expression)))))
  expression))

;;; 2-10-14
;;; Today is a flame day. We will get in fights. Why I don't like any
;;; computer languages.
;;; Take naive FORTRAN and Lisp quadratic functions--shouldn't they
;;; return 2 values? We could do this:

(define (quadratic a b c)
  (/ ((amb + -)
      (- b)
      (sqrt (- (expt b 2) (* 4 a c))))
     (* 2 a)))

;;; Amb returns one or the other but not both. We'll go over how to
;;; make things like this soon.

;;; APL failed because it depended on an APL keyboard. It was based on
;;; square arrays of numbers. 

;;; COBOL has no user-defined functions. This is feature, not a
;;; bug, because the designers don't trust the users. The language is
;;; for financial transactions. The auditor should be able to see
;;; everything. 

;;; Why I'm not happy about languages: What are they forcing me to do?
;;; In engineering you have a machine that takes inputs and makes
;;; outputs which might not come out at the same time and they might
;;; go into other boxes. 

;;; Some languages try to get around this: Erlang, which has an async
;;; thing, Scala too, but they're all pretty kludgerous. The only
;;; sensible way to descibe something like this is with a wiring
;;; diagram of some sort.

;;; We want to be able to use intermediate values. Streams and
;;; differential equations work but are kludges too.

;;; It would be nice to annotate dimensions and units, which are
;;; better than types. Haskell people are teaching us about monads
;;; which are a way to pipe information around with metadata, such as
;;; an entire memory of operations applied, called a "store" or
;;; something like that. 

;;; Problems
;;; Concurrency and Parallelism: We will look at Actors and
;;; Propagators--but these require you to write more stuff!
;;; Iteroperability of programming languages.

;;; Every problem forces ontological commitments, which is the
;;; problem. Scheme doesn't force as many commitments.


;;; 2-12-14
;;; Today I'll talk about glue. 

;;; Here's a circuit glued together from primitive pieces:

(parallel-parallel
 (t-network (resistor -) 
	    (capacitor -)
	    (resistor -))
 (t-network (capacitor -)
	    (resistor -)
	    (capacitor -)))

;;; t-network is a combinator; parallel-parallel is another one.

;;; You can easily construct all ways you can paste together
;;; mathematical functions with just two combinators.
(K x y) -> x
(S x y z) -> (x z (y z))

;;; But it takes enormous numbers of S and K to say anything. On the
;;; other hand, lambda calculus is convenient.

((lambda (x y) 
   -) 
 -)

;;; This differs in that it gives names to the subexpressions. Turner
;;; invented a computer language called Supercombinators where he
;;; augmented S and K with a few useful things so he can translate
;;; into supercombinators. Names in the sense of bound variables in
;;; quantifier expressions. 

;;; Usually we think of lambda calculus used to make functions rather
;;; than as a naming convention mechanism. For example you can say a
;;; resistor is a procedure of two terminals--a bunch of names
;;; creating new names such that when I apply them to other things it
;;; wires them in.

;;; So what are some GOOD combinators for making mix-and-match parts?
;;; Here's a beautiful set, which is not actually fully
;;; adequate. Adopting a particular set of combinators can lock you
;;; in. 

(define (compose f g)
  (assert (= (get-arity f) 1))
  (let ((n (get-arity g)))
    (define (the-composition . args) ; Like (lambda args ...), *args
      (assert (= (length args) n))
      (f (apply g args)))
    (restrict-arity the-composition n)))

#|
((compose (lambda (x)
	    (list 'foo x))
	  (lambda (x)
	    (list 'bar x)))
 'z)
;Value: (foo (bar z))

|#

(define (parallel-combine combine t1 t2)
  (let ((n1 (get-arity t1))
	(n2 (get-arity t2)))
    (define (the-combination . args)  ; Always put defines at beginning of a block
      (assert (= (length args) n1))
      (combine (apply t1 args)
	       (apply t2 args)))
    (assert (= n1 n2))
    (restrict-arity the-combination n1)))

;;; Now what if there are many arguments coming in... n+m arguments
;;; coming in. Pass n of them one way and m of then another way then
;;; combine them. This is basically what you do in tensors: split
;;; arguments into two groups, process separately, and combine. In the
;;; mathematical world, t_1 and t_2 must be linear functions and the
;;; combiner must be multiply.

;(define (spread-combine combine t1 t2)
;  ... missed it)

;;; What about throwing away some arguments?
;;; A big box that takes input 0,...,4, puts them into f, and throws
;;; away the i'th argument.

;;; Currying. Curry-argument taking an argument index, or a bunch of
;;; indices. 

;;; (permute-arguments 1 2 0 3)

;;; Sticky notes. To restrict the arity of a procedure, put a sticky
;;; note on it. Look it up when you call get-arity. 

;;; 2-14-14
;;;
;;; More wild and dangerous programming.
;;; Diamonds are pretty but you can always add more mud to a ball of
;;; mud.

(define symbolic-arithmetic-1
  (make-arithmetic-1 'symbolic numeric-arithmetic
		     (lambda (name base-operation)
		       (lambda args (cons name args))))) ; An
					; operation (+ x y z) will now
					; return the list (+ x y z).

(install-arithmetic! symbolic-arithmetic-1) ; Replace all arithmetic
					; operators with their new
					; meanings from the package we
					; just made.

;;; To simplify, just change the cons to a symbolic simplifier. 
;;; But unsimplified it's still useful for debugging. But it's bad
;;; because you're changing the meanings of things undernearth your
;;; program so you can't prove things--this is where I get into
;;; arguments with Haskell types.
;;; Q. Couldn't you define new types to do this.
;;; A. It requires a lot of ceremony to do that.

;;; What if I want to combine symbolic and numerical? Let's add sticky
;;; notes onto procedures. 

(define numeric-arithmetic
  (make-arithmetic 'numeric raw-scheme-arithmetic number?
		   (lambda (name base-operation)
			   (annotate (lambda args (apply-operation ; Do this to the arguments
						   base-operation
						   args))
				     'applicability ; Make a sticky note:
					; applicability = all args are numbers
				     (all-args (operation-arity
						base-operation)
					       number?)))))

;;; The annotation says all arguments are numbers.

(define (symbolic-arithmetic base-arithmetic)
  (make-arithmetic 'symbolic base-arithmetic symbolic?
		   (lambda (name base-operation)
		     (annotate (lambda args (cons name args)) ; or simplify
			       'applicability ; Applicability is if
					; any argument is symbolic.
			       (any-arg (operation-arity
					 base-operation)
					symbolic?
					(in-domain-predicate
					 base-arithmetic))))))

;;; Now combine the arithmetics and do dispatch on the operations. 

(define (add-2-arithmetics a1 a2)
  (make-arithmetic 'add
		   (list a1 a2)
		   (disjoin (in-domain-predicate a1)
			    (in-domain-predicate a2))
		   (lambda (name op1 op2)
		     (annotate (lambda args
				 (operation-dispatch op1 op2 args
						     naem))
			       'applicability
			       (applicability-union op1 op2)))))

(define (operation-dispatch op1 op2 args name)
  (cond ((is-operation-applicabile? op1 args)
	 (apply-operation op1 args))
	((is-operation-applicable? op2 args)
	 (apply-operation op2 args))
	(else
	 (error "Inapplicable operator:" name))))

;;; This is pretty cool but I'm not there yet!

(define (pure-function-arithmetic range-arithmetic)
  (make-arithmetic 'pure-function range-arithmetic procedure?
		   (lambda (name range-operation)
		     (annoate (lambda functions
				(lambda args
				  (apply-operation range-operation
						   (map (lambda
							    (function)
							  (apply
							   function
							   args))
							functions))))
			      'applicability
			      (all-args (operation-arity
					 range-operation)
					(procedure?))))))

(install-arithmetic!
 (extend-arithmeic combined-arithmetic
		   pure-function-arithmetic))

(* 'b ((+ cos sin) (+ 3 'a)))

;;; Now you can do b(sin+cos)(a+3)
;;; But what does (f+1)(x) mean? We have to build that in too.

;;; Now what if I have (* 'b ((+ 'c cos sin) (+ 3 'a)))-- is c' a
;;; function? We need to define literal-function to mark it if it is.

;;; Have we found Heaven? 
(define foo (make-generic-operation 'foo 2))
(add-to-generic-operation! foo
			   (all-args 2 number?)
			   (lambda (a b) (+ a b)))
(add-to-generic-operation! foo
			   (any-args 2 symbol? number?)
			   (lambda (a b) (list '+ a b)))

(foo 1 2)
;Value: 3

(foo 1 'a)
;Value: (+ 1 a)

;;; Here's a trivial version of make-generic-operation
(define (make-generic-operation name arity)
  (let ((metadata (make-oepration-metadata name arity)))
    (let ((operation (lambda args (generic-dispatch metadata args))))
      (set-operation-metadata! operation
			       metadata)
      operation)))

;;; Then an add-to! and a dispatch procedure.
(let ((g (generic-arithmetic numeric-arithmetic)))
  (add-to-generic-arithmetic! g numeric-arithmetic)
  (install-arithmetic! g))

;;; But now there's dependence on the order in which rules are added
;;; to the generic operations. 
;;; This is a sense of the game we're playing, not the detailed
;;; technique. 

      
;;; 2/18/14
;;;
;;; More generic mechanisms, and then more Wednesday.
;;; Generic operators as exaptation.
;;; How to built systems to **increase the chance** of exaptation?
;;; But we want to not replace the old functionality--while the
;;; extension to new things may not do what you would
;;; think. Augmenting rather than replacing.
;;;
;;; Example: The idea of a user. Extended to daemons (but this is a
;;; bad exaptation).
;;;
;;; This is all for research code, not industry code, where
;;; programmers are interchangeable.
;;;

;;; Can we make D such that ((D cos) 3) etc?
;;; We have x and f(x) and g(f(x)).
;;; Invent a new datatype which is $x + \Delta x$.
;;; Now expand f to work on that kind of object.
;;; It produces $f(x) + f'(x) \Delta x$. We assume every primitive
;;; object knows how to do this. 
;;; Also $g(f(x)) + g'(f(x))f'(x)\Delta x$.
;;; So a derivative operator starts with an $x$ and adds an arbitrary
;;; 1 to it, shoves it through the system, and extracts the last
;;; term. 
;;; The generic operator trick defines a chain rule whenever we know
;;; the derivatives of all primitive functions.

'(+ (literal-number 'a) 1)

(assign-operation 'type quaternion?)
;;; You should use def-handler.

;;; 2/19/14
;;;
;;; Generic operations implementation. SICP chapter 2. 
;;; Generic selectors: (real-part z) etc.
(define (real-part z)
  ((rectangular? z)
   (real-part-rectangular (contents z)))
  ((polar? z)
   (real-part-polar (contents z)))
  (else
   (error "?"))) ; etc.

;;; Imagine a table:
;;;               rect       polar 
;;; realpart
;;; imagpart
;;; mag
;;; etc .
;;;
;;; You select the row, and dispatch selects the column.
;;; This is quite equivalent to OO message passing when dispatch is on
;;; types.
;;;
;;; How can we do this as flexibly as possible?
;;; I like to write stuff like this:

(defhandler + m+m matrix? matrix?)
(defhandler * s*v scalar? vector?)

;;; How can we implement such a thing effectively?
;;; Store argument tests in a trie.
;;; An alist is a list of key-value pairs. Store the trie as recursive
;;; alists. 

;;; We want be able to do:
(define +bin 
  (make-generic-operator 2 '+ +)) ; arity 2, name '+, default +

;;; Within that definition:
(define (operator . arguments)
  (if (not (acceptable-arglist? arguments arity))
      (error)
      (if (default-object? name) operator name)
      (arity arguments))
  (apply (find-handler arguments) arguments))

;;; Then find-handler is the trie search.
;;; (let symbol ((v1 e1) (v2 e2)) e) ->
;;; ((lambda (v1 v2) e) e1 e2)

(define *generic-operator-table*
  (make-eq-hash-table))

;;; bind-in-tree is nasty.
;;; How much it cost to do this in performance? Let's look at some
;;; alternatives.
;;; Tagging works well for one-argument but is lousy for
;;; multiple-argument systems.


;;; 2-21-14
;;;
;;; We talked about ways of attaching handlers to operators. 

(f 1 2)

;;; f can be interpreted by many different handlers. What if we have
;;; e.g. one that can do it fast but not reliably vs one that does it
;;; slow but reliably or some that don't know if they can do it. How
;;; could you manage this ensemble? But we won't do that today. Having
;;; a language where there is one symbol f is good to the reader but
;;; not good for figuring out what you really want done. What kind of
;;; a language would allow various pieces of a machine to negotiate
;;; about how to do something? We need a more nuanced way of
;;; specifying the job we want done.
;;;
;;; We'll talk about pattern matching. 

(= (* ?x ?x) :y)

;;; If I give you y can you give me the x that when multiplied by
;;; itself gives y? That's more powerful than the word sqrt. 

(= (* 3 ?x) ?y) ; numbers which have 3 as a factor

(= (* 3 (?x integer?) ?y) ; integer multiples of 3

;;; Logic is a beautiful example: people trying to understand how
;;; smart people think. 
(rule ((?a)
       (implies (?a) (?b)))
       (?b))

(know (implies (human @x)
	       (mortal @x))

(know (human Socrates)) ; we should be able to then derive that
			; Socrates is mortal.

;;; Logic is a good way to describe how it appears that smart people
;;; think but how they really get their answers we don't know. We have
;;; to separate how we describe a thing from how a thing really
;;; works. A coin falling is one half a t squared--no, that's how we
;;; describe it, not what's really happening.

;;; Q. Prolog? A. Only backwards chaining. Only chronological
;;; backtracking. An attempt to use a logical basis for a rather
;;; limited language. It has both good and bad characteristics: the
;;; worst is that it has no nice means of abstraction.

;;; In Mathematica, MACSYMA etc., there are pattern-matching
;;; substitution rules:

(* ?a (+ ?b ?c)) ; rewrite as:
(+ (* ?a ?b) (* ?a ?c))

;;; Soon we will have programs doing term rewriting. Things like
;;; transcendental functions are hard to do well. In general, you
;;; can't tell whether an algebraic expression is 0. 

;;; Today I'll show you the evolution of pattern matching, and
;;; tomorrow an exceedingly excellent method. In general we're
;;; generalizing the notion of equality. 

;;; BTW you probably made a horrible mistake in the pset:
;;; eq? eqv? equal? =
;;; Some of you used eq? which is a terrible mistake to use
;;; w.r.t. numbers. It happened to work for small numbers but it
;;; doesn't work if they're big. For numbers, use =. 
;;; eq? is pointer equality! Only use it for symbols!
;;; equal? is recursive equality of data structures.

(define (equal? a b)
  (cond ((pair? a)
	 (if (pair? b)
	     (and (equal? (car a) (car b))
		  (equal? (cdr a) (cdr b)))
	     #f))
	((pair? b) #f)
	(else (eqv? a b))))

;;; Now let's add in "don't cares"

(define (match? a b)
  (cond ((variable? a) #t) ; This is the new part
	((pair? a)
	 (if (pair? b)
	     (and (match? (car a) (car b))
		  (match? (cdr a) (cdr b)))
	     #f))
	((pair? b) #f)
	((pair? b) #f)
	(else (eqv? a b))))

;;; Now we need to catch things and name them

(define (match? a b)
  (define (walk a b dict) ; walk will return the dict
    (cond ((variable? a) ; if it's a variable, bind it
	   (if (named-variable? a)
	       (bind a b dict)
	       dict))
	  ((pair? a) ; if a is a pair,
	   (if (pair? b) ; and if b is a pair, walk down the cars
	       (let ((ndict
		      (walk (car a) (car b) dict)))
		 (and ndict ; if we haven't failed yet, walk down the
			    ; cdrs
		      (walk (cdr a) (cdr b) ndict)))
	       #f)) ; fail if a is a pair but b is not
	  ((eqv? a b) dict) ; if a and b are not pairs and equivalent
	  (else #f))) ; otherwise fail
  (walk a b '())) ; build up from the empty list

(define (named-variable? a)
  (and (pair? (cdr a))
       (symbol? (cadr a))))

(define (variable-name a)
  (cadr a))

(define (bind var value dict)
  (cons (list (variable-name var) value) dict))

;;; Walk the data structure and drag with me a dictionary. Haskell
;;; people should see this as a state monad. Instead of matching a
;;; don't-care we now match a don't-care with a name.

;;; Now let's assume the varaibles are named and two instances of the
;;; same nameed variable must have the same variable

(define (match? a b)
  (define walk a b dict)
  (cond ((variable? a)
	 (let ((vcell (lookup a dict)))
	   (if vcell
	       (if (equal? (value vcell) b)
		   dict
		   #f)
	       (bind a b dict))))
	  ((pair? a) ; if a is a pair,
	   (if (pair? b) ; and if b is a pair, walk down the cars
	       (let ((ndict
		      (walk (car a) (car b) dict)))
		 (and ndict ; if we haven't failed yet, walk down the
			    ; cdrs
		      (walk (cdr a) (cdr b) ndict)))
	       #f)) ; fail if a is a pair but b is not
	  ((eqv? a b) dict) ; if a and b are not pairs and equivalent
	  (else #f))) ; otherwise fail
  (walk a b '())) ; build up from the empty list

;;; Now the hard part: suppose you want to match substructure. For
;;; example, the associative law for addition. 

(+ (??as (+ (?? bs)) (?? cs))) ; want `(+ ,@as ,@bs ,@cs)

;;; Now we need segment variables, but there are many ways to match a
;;; segment. Now you will see a complexification, tomorrow a
;;; simplification. Building on what already have:

(define (match? a b)
  (define (walk a b dict cont)
    (cond 
     ((variable? a)
      (match-variable a b dict cont))
     (eqv? a b) (cont dict))
    ((and (pair? a) (pair? b))
     (match-list a b dict walk cont))
    (else #f))
  (walk a b '() (lambda (dict) dict)))

;;; We use a continuation which is a procedure which will return the
;;; result. 

; ...

(define (match-segment a b dict cont)
  (let try-seg ((end b))
    (or (cont ; Make sure that the rest will be OK with this match
	 (bind a (make-segment b end) dict) end)
	(and (pair? end)
	     (try-seg (cdr end))))))

#|

(pp (match? '(a (?? x) b (?? y) c)
	    '(a a b b b b b b c)))
((y #((b b b b b c) (c)))
 (x #((a b b b b b b c) (b b b b b b c))))

|#

;;; 2-24-2014
;;;
;;; Today I'll show you an elegant way to organize matching. I got the
;;; idea from Carl Hewitt's 1969 PhD thesis at MIT. 

;;; Make match combinators: A procedure like this:
(lambda (data dictionary succeed)
  (if ...
      #f
      (succeed new-dictionary)))

;;; Remember syntax for matching is already determined.
(define (match:element? pattern) ; element variables
  (and (pair? pattern)
       (eq? (car pattern) '?)))

(define (match:segment? pattern) ; segment variables, which match a
				  ; sublist of something
  (and (pair? pattern)
       (eq? (car pattern '??))))

(define (match:variable-name pattern)
  (cadr pattern))

(define (match:list? pattern)
  (and (list? pattern)
       (or (null? pattern)
	   (not (memq (car pattern) '(? ??)))))) ; it was bad to link
					; this bit of information with
					; the previous code using ?
					; and ??. Now we have to
					; maintain this
					; correlation. 
;;; The thing that decides whether a pattern is a pattern matching a
;;; list knows that there are other patterns in the world--that's the
;;; problem. 

;;; Now I'll make a generic operation that compiles a match expression
;;; to a combination of those combinators.
(define match:->combinators
  (make-generic-operator 1 match:eqv))

(defhandler match:->combinators
  (lambda (pattern)
    (match:element (match:variable-name pattern)))
  match:element?) ; produces the combinator

(defhandler match:->combinators
  (lambda (pattern)
    (apply match:list
	   (map match:->combinators pattern)))
  match:list?)

(define (matcher pattern)
  (let ((match-combinator
	 (match:->combinators pattern)))
    (lambda (datum)
      (match-combinator datum '()
			(lambda (dictionary) dictionary)))))

(define (match:eqv pattern-constant)
  (define (eqv-match data dictionary succeed)
    (and (eqv? data pattern-constant)
	 (succeed dictionary)))
  eqv-match)

;;; `(a ,b c)
;;; quoted but with b evaluated rather than quoted
;;; `(a ,b ,@(map foo '(a b c)) d)
;;; ??
;;; ` is shorthand for:
;;; (list 'a b 'd)
;;; so something like `(a b c) is (list 'a 'b 'c) not '(a b c).

;;; Now you can do stuff like pattern-directed invocation:
(fact 0) = 1
(fact n) = (* n (fact (- n 1))) ; need to add a guard enforcing n > 0





