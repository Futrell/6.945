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

;;; 2-26-2014
;;;
;;; The nice things about rules is that they're somewhat
;;; additive. Next time we'll talk about why this approach is
;;; inadequate.

(define algebra-1
  (rule-simplifier
   (list
    ;; Associative law
    (rule '(+ (? a) (+ (? b) (? c)))
	  `(+ (+ ,a ,b) ,c))

    ;; Commutative law
    (rule '(* (? b) (? a))
	  (and (expr<? a b) ; an arbitrary total ordering
	       `(* ,a ,b)))

    ;; Distributive law
    (rule '(* (? a) (+ (? b) (? c)))
	  `(+ (* ,a ,b) (* ,a ,c))))))

;;; Now you can make a bigger system! 

(define algebra-2
  (rule-simplifier
   (list
    (rule `(+ (? a)) a)
    (rule `(+ (?? a) (? y) (? x) (?? b))
	  (and (expr<? x y)
	       `(+ ,@a ,x ,y ,@b)))
    (rule `(* (? a)) a)
    (rule `(* (?? a) (* (?? b)) (?? c))
	  `(* ,@a ,@b ,@c))
    ;; etc. 

    (rule `(+ 0 (?? x)) `(+ ,@x))
    (rule `(+ (? x ,number?) (? x ,number?) (?? z))
	  `(+ ,(+ x y) ,@z))

    (rule `(* 0 (?? x)) 0)
    (rule `(* 1 (?? x)) `(* ,@x))
    )))

;;; Use the total ordering to make numerical expressions bubble to the
;;; left together so they can be simplified. 

(define derivates
  (rule-simplifier
   (list (rule `(d/d (? u) (? x))
	       (and (not (recursive-member? x u))
		    0))
	 (rule `(d/d (? x) (? x))
	       1)
	 (rule `(d/d (+ (? u) (? v)) (? x))
	       `(+ (d/d ,u ,x)
		   (d/d ,v ,x)))
	 (rule `(d/d (* (? u) (? v)) (? x))
	       `(+ (* (d/d ,u ,x) ,v)
		   (* ,u (d/d ,v ,x))))
	 (rule `(d/d (expt (? u) (? n ,integer?))
		     (? x))
	       `(* ,n
		   (expt ,u ,(- n 1))
		   (d/d ,u ,x))))))

(define (recursive-member? x u)
  (cond ((equal? x u) #t)
	((pair? u)
	 (or (recursive-member? x (car u))
	     (recursive-member? x (cdr u))))
	(else #f)))

;;; Now how do I make this work?

(define (rule-simplifier the-rules)
  (define (simplify-expression expression)
    (let ((subexpressions-simplified
	   (if (list? expression)
	       (map simplify-expression expression)
	       (expression)))
	  ;; ones the subexps are simplified we resimplify the
	  ;; expression that contains them
	  (try-rules subexpressions-simplified
		     the-rules
		     ;; if any rule applies we must resimplify
		     (lambda (result fail) ;; oh I've got a new
		       ;; result, now I have to go around the loop
		       ;; simplifying that too. Also, what if it's the
		       ;; case that the recursive simplifying
		       ;; expression here fails? Need a more powerful
		       ;; function.
		       (simplify-expression result))
		     ;; if no rules applies we're done
		     (lambda () ; if no rule applied, just return that
		       subexpression-simplified))))
    (rule-memoize simplify-expression))
		     
(define (try-rules data rules succeed fail)
  (let per-rule ((rules rules)) ; Q. why overwrite the name here?
				; A. whatever
    (if (null? rules)
	(fail) ; if no more rules, call the failure continuation
	((car rules) data succeed (lambda ()
				    (per-rule (cdr rules)))))))

;;; Macros are one of the best ways to confuse yourself.
;;; In many languages macros are just substitution, but variable
;;; capture is a problem. Hygienic macros are like procedures in that
;;; they have local context, to prevent variable capture. A much more
;;; powerful macro system is syntactic closures. 

#| 
(syntax '(rule ... ...))
(make-rule ...
	   (lambda (b a)
	     ...))
|#

(define-syntax rule
  (sc-macro-transformer
   (lambda (form use-env)
     (let ((patern (cadr form))
	   (handler-body (caddr form))) ; stuff to prevent capture
       ...))))

;;; Get out of macro world as fast as possible!
(define (make-rule pattern consequent)
  (let ((match-procedure
	 (match:->combinators pattern))
	(bound-variables
	 (procedure-bound-variables consequent)))
    (define (the-rule data succeed fail)
      (or (match-procedure (list data) '() ; if it succeeds I get a
					; new dictionary where I have
					; to look up the value for
					; every bound variable in the
					; dictionary; make that into
					; an argument which I apply to
					; the consequent to get the
					; result ...
)))))

;;; The only reason we use macros is to produce syntactic sugar. 

;;; 2-28-14
;;;
;;; Symbolic integration, which is not useful but is good for showing
;;; off and problem sets. But it elicits a deep technique. 

;;; SIN symbolic integrator: Joel Moses.
;;; Derivative Divides Heuristic:
;;; I(x) = \int c \rho(u(x)) u'(x) dx
;;; let y=u(x) so dy = u'(x)dx, then I(x) = cJ(u(x)) where 
;;; J(y) = \int \rho(y)dy

(define (integration-by-parts)
  (rule `(int (? x) (product (? u) (? dv/dx)))
	(and (occurs? x u)
	     (occurs x dv/sx)
	     (let ((v (internal-integrate
		       `(int ,x ,dv/dx))))
	       (and v
		    (let* ((du/dx (derivative u/x)))
		      ...))))))

;;; This depends on semantic match: you integrate by parts if you can
;;; see something as (int (? x) (product (?u) (? dv/dx)).

;;; Suppose I want to match quadratics: I might have to imagine terms.
(define match-quadratic
  (rule `(sum (product
	       (expt (? var ,non-numeric?) 2)
	       (? a ,non-zero?))
	      (sum (product (? var) (? b))
		   (? c))) 
	(and (not (occurs? var a)) ; what does occurs? mean?
	     (not (occurs? var b))
	     (not (occurs? var c))
	     (list var a b c))))

(match-quadratic '(+ (* 2 (expt x 2)) (* 3 x) 4))
;Value: (x 2 3 4)

(match-quadratic '(+ (expt x 2) (* 3 x) 4)) ; need to imagine the 1
;Value: (x 1 3 4) 

(match-quadratic '(+ (* 3 x) 4))
;Value: #f

(define perfect-square?
  (rule `(sum (product
	       (expt (? var ,non-numeric?) 2)
	       (? a ,non-zero?))
	      (sum (product (? var) (? b))
		   (? c)))
	(and (not (occurs? var a))
	     (not (occurs? var b))
	     (not (occurs? var c))
	     (zero? ; so it's a quadratic with this constraint: the
		    ; discriminant turns out to be 0. 
	      (simplify
	       `(- (expt ,b 2)
		   (* 4 ,a ,c))))
	     (pp (list a b c var))
	     #f)))

(perfect-square? `(* (+ x y) (- x y)))
;Value: #f

(perfect-square? `(* (+ x y) (+ x y)))
(1 (* 2 y) (expt y 2) x)
(1 (* 2 x) (expt x 2) y)
;Value: #f

;;; Next week we go to evaluator land. How do you make a problem that
;;; fits your problem?

;;; 3-3-2014
;;;
;;; Eval is the universal machine.

(define eval
  (lambda (expr env) ; env is a mapping of names to their values
    (cond ((number? exp) exp)
	  ((symbol? exp) (lookup exp env))
	  ((eq? (car expr) 'quote) (cadr expr))
	  ((eq? (car expr) 'cond) (eval-clauses (cdr expr) env))
	  ((eq? (car expr) 'lambda) (list 'procedure 
					  (cadr expr)
					  (caddr exp)
					  env)) ; lexical scoping
					    ; happens here
	  (else (apply (eval (car expr) env)
		       (values (cdr expr) env))))))

(define apply
  (lambda (proc args)
    (cond ((primitive? proc) 
	   (papply proc args))
	  ((eq? (car proc) 'procedure)
	   (eval (caddr proc) (extend (cadddr procedure) ; old environment
				      (cadr proc) ; args from lambda
				      args)))
	  (else (error "Unknown procedure applied.")))))

(define values
  (lambda (exprs env)
    (cond ((null? exprs) 
	   '())
	  ((else (cons (eval 9car exprs) env)
		 (values (cdr exprs) env))))))

(define eval-clauses
  (lambda (clauses env)
    (cond ((eq? (caar clauses) 'else)
	   (eval (cadar clauses) env))
	  ((not (eq? (eval (caar clauses) env) #f)) 
	   (eval (cadar clauses) env))
	  (else 
	   (eval-clauses (cdr clauses) env)))))

(define extend
  (lambda (base-env vars vals)
    (cons (make-frame vars vals) base-env)))

(define make-frame
  (lambda (vars vals)
    (cond ((null? vars)
	   (cond ((null? vals)
		  '()) ; base case
		 (else (error "Too many arguments."))))
	  ((null? vals)
	   (error "Too few arguments."))
	  (else
	   (cons (cons (car vals) (car vals))
		 (make-frame (cdr vars) (cdr vals)))))))

(define lookup
  (lambda (var env)
    (cond ((null? env)
	   (error "Unbound variable."))
	  (else ((lambda (binding) ; this is a let form
		   (cond (binding
			  (cdr binding))
			 (else (lookup var (cdr env)))))
		 (assq var (car env)))))))

(define assq
  (lambda (var bindings)
    (cond ((null? bindings)
	   #f)
	  ((eq? var (caar bindings))
	   (car bindings))
	  (else (assq var (cdr bindings))))))

(define primitive?
  (lambda (proc)
    (memq proc (list car cdr pair? cons eq? memq cadr ...))))

(define papply
  (lambda (proc args)
    (cond ((eq? proc car) ; not quote car!
	   (car (car args)))
	  ((eq? proc cdr)
	   (cdr (car args)))
	  ...)))

;;; What does this mean? Really, what does a program mean ever? 

(define fact
  (lambda (n)
    (cond ((= n 0) 1)
	  (else (* n (fact (- n 1)))))))

;;; This is an equation in the name fact which has the property that
;;; one of its solutions is the method of computing factorials. 

K = (lambda (fact)
      (lambda (n)
	(cond ((= n 0) 1)
	      (else (* n (fact (- n 1)))))))

;;; I'm asking for a fixpoint of K. The true factorial procedure ! is
;;; a thing that has the property that ! = (K !). Because if you pass !
;;; into K then it's the fact in lambda (fact). 

;;; Say we have B, bottom, the useless procedure that always gives an
;;; error. (K B) is a good factorial procedure for n=0. 
;;; Now if I have (K (K B)), that'll give me the value for n=1.
;;; (K (K (K B))) is good up to n=2.

;;; I'll give you a procedure Y.
Y = (lambda (f)
      ((lambda (x) (f (x x)))
       (lambda (x) (f (x x)))))

(Y K) = (K (K (K (K ...)))) = (K (Y K)) = !

;;; This one works only in normal order. The website has one for
;;; applicative order.



;;; 3-5-2014

;;; Easy way to see what's going on with Y operators and such:
((lambda (g) (g g 20))
 (lambda (f n)
   (cond ((< n 2) n)
	 (else (+ f f (- n 2))
	       (f f (- n 1))))))
;Value: 6765

(define sigma
  (lambda (f lo hi)
    (define lp
      (lambda (i sum)
	(if (> i hi)
	    sum
	    (lp (+ i 1) (+ sum (f i))))))
    (lp lo 0)))

(define sum-powers 
  (lambda (lo hi n)
    (define nth-power 
      (lambda (x) (expt x n)))
    (sigma nth-power lo hi)))

;;; This demonstrates static lexical binding: the n has the same value
;;; in sum-powers and in nth-power. Where does that occur in an
;;; interpreter? And then I'll show you alternatives.

;;; The critical part is the part of eval where we evaluate a lambda
;;; expression to make a procedure. You capture the environment of the
;;; lambda expression when you make do make:

(list 'PROCEDURE (cadr exp) (caddr exp) env)

;;; So the n in sum-powers is given its value when the function is
;;; DEFINED, not when it's called. Then in apply, you have:

(extend (cadddr proc) (cadr proc) args)

;;; Now for some other ideas. How about this?

(define nth-power
  (lambda (x) (expt x n)))

(define sum-powers
  (lambda (lo hi n)
    (sigma nth-power lo hi)))

;;; In that case the n comes not from the place where nth-power is
;;; defined, but rather from where it's called. Early LISPs did this
;;; and APL too. "Dynamic binding". Is this more modular? Before
;;; arguing, I'll show you how to do it, and how we can do both if we
;;; want using "fluid variables".

;;; New rules inside eval:
(list 'PROCEDURE
      (cadr exp)
      (caddr exp))

(apply (eval (car exp) env)
       (values (cdr exp) env)
       env) ; pass in the environment too

;;; Then within apply you have:
(eval (caddr proc)
      (extend env
	      (cadr proc)
	      args))

;;; OK so why is this a bad idea? Suppose the library routine were the
;;; following:

(define sigma
  (lambda (f lo hi)
    (define lp
      (lambda (n sum)
	(if (> n hi)
	    sum
	    (lp (+ n 1) (sum f n)))))
    (lp lo 0)))

;;; In that case the internal n would be captured. Aagh! 
;;; Dynamic binding leads to referential opacity.
;;; Things depend upon names that you can't know.

;;; Now why is that ever useful? It's good when you want something to
;;; be controlled by an extent of time, such as in an operating system
;;; when you want to know when files are opened or close. Also,
;;; exception handlers. 

;;; Here's a simpler bit of code using static lexical binding, using
;;; currying.

(define nth-power
  (lambda (n)
    (lambda (x) expt x n)))

(define sum-powers
  (lambda (lo hi n)
    (sigma (nth-power n) lo hi)))

;;; (Argument about Haskell)

;;; Let's go down a different path: continuations, which represent
;;; everything that will happen in the future. 

(+ (* 3 4) 5)

;;; Rewrite that as:

(** 3 4
    (lambda (v)
      (++ v 5 k))) ; k is the continuation

;;; The value of this one goes to the k. Instead of returnign the
;;; value directly it calls a continuation which gets a value, adds
;;; it to 5, and then calls ITS Continuation function. We're escaping
;;; from expression world. Once you get this you can do all sorts of
;;; things like write an entire operating system which requires stuff
;;; like interrupts. 

;;; Let's define call/cc or call-with-current-continuation:

(+ 1 (call/cc 
      (lambda (k) 
	(+ 2 (k 3)))))
;Value: 4

;;; At the top level is something that adds 1 to something. Call/cc
;;; says capture that continuation and give it the name k. Call k on 3
;;; later. The result is 4. (k 3) says take that 3 and put it in the
;;; place of call/cc: make that the thing that the continuation
;;; accepts. 

((lambda (map)
   (call/cc
    (lambda (k)
      (map map ; ha ha that's funny
	   (lambda (x)
	     (cond ((negative? x) (k 'bad))
		   (else (sqrt x))))
	   '(1 9 25 64)))))
 (lambda (me proc lst)
   (cond ((null? lst) lst)
	 (else
	  (cons (proc (car lst))
		(me me proc (cdr lst))))))
;Value: (1 3 5 8) 

;;; But if I "pass in" '(1 9 -35 64) I get the output 'bad. 
;;; The continuation is functioning like break. 
;;; But a continuation may be called more than once.

(define r #f)

(+ 1 (call/cc
      (lambda (k)
	(set! r k)
	(+ 2 (k 3)))))
;Value: 4

(r 5)
;Value: 6

(+ 3 (r 5))
;Value: 6

;;; The future of the computation has been captured so it can be used
;;; more than once. Useful for generate-and-test. This gives you
;;; dangerous power.

(define eval
  (lambda (exp env)
    (eval-k exp env (lambda (x) x))))

;;; Add this in eval-k:
((eq? (car exp) 'CALL/CC)
 (eval-k (cadr exp) env ; the argument of call/cc
	 (lambda (proc) (apply-k proc (list k k)))))

;;; I've changed the evaluator's shape which has consequences. How can
;;; I prevent inventing other extensions to the evaluator when I add
;;; more things?

;;; It turns out continuations are universal. In Haskell there's a
;;; paper called "The mother of all monads". It argues that
;;; continuations give you everything you can do with a monad. 

;;; 3-7-14
;;; 
;;; Today we'll talk about compilation. Next Friday Guy Steele will
;;; talk about parallel random number generation. 
;;;
;;; A computer is just an interpreter for some language. But direct
;;; interpretation isn't necessarily the best way to do things. You
;;; can also translate and then interpret. Source code -> object code
;;; -> "hardware". From the point of view of a Lisp interpreter, the
;;; interpreter is the "hardware" (a virtual machine). 
;;;
;;; How do you organize a compiler? A compiler takes your source code
;;; as a constant and pushes it through the interpreter to determine
;;; what it would do, and then produces symbolic results of that
;;; sort. A basic idea is constant folding. In this case the constant
;;; is your program.

(+ 1 3) => 4
(+ 1 'a) => (+ 1 a)

;;; This is symbolic evaluation, and you're left knowing what you have
;;; left to do. The compiler also gets to throw away parts that are
;;; not necessary. Basically it's a kind of algebraic simplification.
;;;
;;; Today we'll talk about compiling to combinators. Let's do that
;;; today for something like a general interpreter. 
;;;
;;; You want to transform code in some source language into something
;;; that has a standardized interface. Separate analysis from
;;; execution.

(define (eval exp env)
  ((analyze exp) env)) 

(define analyze ; produces an execution procedure which takes an
		; environment and does it.
  (make-generic-operator 1
			 (lambda (exp)
			   (cond ((application? exp)
				  (analyze-application exp))
				 (else (error "Unknown exp type"))))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(defhandler analyze analyze-self-evaluating self-evaluating?)

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(defhandler analyze analyze-quoted quoted?)

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
	(cproc (analyze (if-consequent exp)))
	(aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
	  (cproc env)
	  (aproc env)))))

(defhandler analyze analyze-if if?)

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
	(bproc (analyze (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(defhandler analyze analyze-lambda lambda?)

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
	first-proc
	(loop (sequentially first-proc (car rest-procs))
	      (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (loop (car procs) (cdr procs))))

(defhandler analyze analyze-sequence begin?)
  
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp))) ; first have to evaluate to
					; get the procedure
	(aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env) 
			   (map (lambda (aproc) (aproc env))
				aprocs)))))

(define (execute-application proc args) ; should be generic
  (cond ((promitive-procedure? proc)
	 (apply-primitive-procedure proc args))
	((compound-procedure? ... etc))))

;;; If you just take the elementary interpreter I showed you and time
;;; it and then use this one you get a factor of 10 improvement.
;;;
;;; We're transforming the language into a different languages which
;;; is a composition of functions only. Separation of analysis from
;;; execution.
;;;
;;; Let's also talk about normal vs. applicative order. Normal order
;;; is lazy. We have to be careful to not believe in one thing or the
;;; other, but to be able to do both.
;;;
;;; Strict evaluation: Applicative order.
;;;     Lisp, Scheme, ML, Python, Ruby, OCAML
;;;
;;; Call by object sharing: Assignment does not change anything
;;; outside the local scope. However, modifying a data structure in
;;; local scope will change that structure outside the scope. Data
;;; push.
;;;
;;; Non-strict = lazy = normal order
;;;     All languages have nonstrict conditionals. And is a macro that
;;;     turns into nested ifs. 
;;; 
;;; Call-by-name was the default in ALGOL 60, and it also had
;;; call-by-value. Say we have (+ fact (- n 4) 1). Then the (- n 4)
;;; part has to carry an environment with it representing the value of
;;; n; this is stored in a thunk. Haskell and R are like this. Macro
;;; expansions are also like this.
;;;
;;; Normal order advantages: It terminates. Certain things don't
;;; produce error conditions. 
;;;
;;; Normal order disadvantages: Arbitrarily bad space
;;; complexity. State can't be incorporated smoothly, so it's hard to
;;; deal with IO devices. Easier to simulate normal order in an
;;; applicative language than it is to simulate applicative order in a
;;; normal language. 
;;;
;;; I can prove to you as a joke that the universe cares about this. 

double = (lambda (x) (+ x x))
(amb u v) = u or v but I don't know which
(double (amb -1 1)) = ?

;;; In pure normal order, I take the expression which has no free
;;; variables except amb, plug it into x for double, each one being
;;; separately evaluated, the possibilites are {-2, 0, 2}, with a
;;; probability of 1/4, 1/2, 1/4. 
;;;
;;; In applicative order the possibilities are {-2, 2}. 
;;; 
;;; Now think about the quantum two-slit experiment. In normal order
;;; I'd see a distribution with lots of mass between the slits. In
;;; applicative order I'd see a dip between them. So maybe the
;;; universe runs on applicative order. There are also proofs that the
;;; universe is two's complement. Ha, ha.
;;;

;;; 3-10-14
;;;
;;; Today is amb day. 
;;; In microtubules in cells, you have a generator and a tester. You
;;; can think of this as a noise filter but messages can be sent back
;;; to the generator so it knows what to avoid. 
;;; 
;;; Now you have something that doesn't know the goal, and something
;;; that doesn't know how to generate alternatives. 
;;;
;;; Example: Square roots. THere are two solutions. Let's make a
;;; square root function that provides an answer and a complaint
;;; department. 
;;;
;;; McCarthy invented the amb operator: (amb 1 2 3) is one of those
;;; but I don't know which. 

(list (amb 1 2 3) (amb 'a 'b))

;;; There are six possibilities. It's very easy to write a program
;;; that is exponentially long in time this way. 

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
	(require (= (+ (* i i) (* j j) (* k k)))
		 (list i j k))))))

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (require p) (if (not p) (amb))) ; amb of no args is a
					; contradiction

;;; Constraint satisfaction.

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
    ...etc)
    (require (distinct? (list baker cooper ...etc)))
    (require (not (= baker 5)))
    ...etc
    (list (list 'baker baker) ...etc)))

;;; Now (multiple-dwelling) works!
;;; SEND+MORE=MONEY: What are the values of the letters?

(define (send-more-money)
  (let ((m 1)
	(s (amb 2 3 4 5 6 7 8 9)))
    ...etc))

;;; Amb-evaluator in SICP produces depth-first chronological
;;; backtracking, a lousy way to do it. Hitting an amb of no arguments
;;; (a constradiction) doesn't give you enough information to know
;;; what not to do again. 

;;; Now I'll show you some of the evil. Last time we did compilation
;;; to combinators like this:

(lambda (env) doit)

;;; But now I have to do:

(lambda (env success fail)) ; fail is complaint department 1

;;; The only way it returns a value is by doing:

(succeed value 
	 (lambda () complaint-action)) ; complaint department 2

;;; The (lambda (env success fail) ...) will either call success with
;;; the result or call fail. (But how do you do with continuations or
;;; monads so we don't have to change our compilation? Whenever you
;;; have to make plumbing like this, you can do it with monads or
;;; continuations.)

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
	     fail)))

;;; This is the same stuff we've seen before but twisted into a
;;; continuation passing style. 

(define (analyze-amb exp)
  (let ((aprocs
	 (map analyze (amb-alternatives exp))))
    (lambda (env succeed fail)
      (let loop ((alts aprocs))
	(if (null? alts)
	    (fail) ; I ran out of alternatives so I'm calling fail.
	    ((car alts) env ; Call the first one with the environment
	     succeed 
	     (lambda () ; If someone doesn't like the result they can
			; call this, which goes around the loop with
			; the rest of the alternatives.
	       (loop (cdr alts)))))))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
	 ;; success cont. for calling a
	 (lambda (a-value fail2) ; if a succeeds, it gets the value a
				 ; which we're going to discard
				 ; because it's a sequence of things
				 ; being done. Then I call b but b
				 ; gets the success for the whole
				 ; thing and the failure coming from
				 ; the failure of a. So if we have
				 ; (require (not (= smith 4))) and
				 ; (require (> cooper whatever)), then
				 ; we say that if a later one hits a
				 ; contradiction, then I'll call the
				 ; failure continuation of the first
				 ; guy which says give me a different
				 ; choice for where smith might live.
	   (b env succeed fail2))
	 ;; failure cont. for calling a
	 fail))) ; if a doesn't like its environment it will call this
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
	first-proc
	(loop (sequentially first-proc
			    (car rest-procs))
	      (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
	(error "empty sequence"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
	(aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
	     (lambda (proc fail2)
	       (get-args aprocs env
			 (lambda (args fail3)
			   (execute-application
			    proc args succeed fail3))
			 fail2))
	     fail1))))

;;; Q. Isn't this just as bad as GOTO? A. It's easy to write macros
;;; turning gotos and local assignments into one that has no gotos and
;;; only procedure calls. We wrote this as a reply to Dijkstra. So
;;; GOTO is not the problem. The problem is how you structure your
;;; program.

;;; We start with functional programs: no side effects. 
;;; Assignments (mutators) are special because there's a moment before
;;; and after. This creates linear time.
;;; Nondeterministic search (amb): branching of time into possible
;;; futures. 
;;; Concurrency: Now you have a lattice. 

;;; 3-17-14
;;;
;;; Today I'll show how we can make a depth-first amb in Scheme using
;;; continuations. I want to do this:

(define elementary-backtrack-test
  (lambda ()
    (let ((x (amb 1 2 3)))
      (let ((y (amb 'a 'b)))
	(let ((z (amb #t #f)))
	  (pp (list x y z)))))
    (amb)))

(with-depth-first-schedule elementary-backtrack-test)
(1 a #t)
(1 a #f)
; etc.

;;; I'll introduce amb as a syntactic object by means of a macro.

(define-syntax amb
  (sc-macro-transformer
   (lambda (form uenv) ; take an expr that begins with amb and produce
		       ; an amb-list which has a list of lambda () alternatives.
     `(amb-list 
       (list
	,@(map (lambda (arg)
		 `(lambda ()
		    ,(close-syntax arg uenv))) ; this is a syntactic
					; closure. 
	       (cdr form)))))))

;;; Here's what's funny about macros.

(defmacro push! (item place)
  `(set! ,place (cons ,item ,place)))

;;; But we get inadvertent capture:

(let ((cons '()))
  (push! 'short cons)
  (push! 'long cons)
  cons)

;;; that expands to nonsense:

(let ((cons '()))
  (set! cons (cons 'short cons))
  (set! cons (cons 'long cons))
  cons)

;;; On the other hand, a hygienic macro system automatically knows
;;; there are things that are local to the macro def and things that
;;; are part of the user's environment. It will automatically rename
;;; the user's let-defined cons to cons.1 or something. 

;;; Here's another possible macro system that people use, which uses
;;; "...":

(define-syntax amb
  (syntax-rules
      ((amb alternative ...)
       (amb-list (list (lambda () alternative)
		       ...)))))


;;; Now here's how we can embed amb in the real thing.

(define (amb-list alternatives)
  (call-with-current-continuation ; remember what the amb returns to
   (lambda (k)
     (add-to-search-schedule
      (map (lambda (alternative)
	     (lambda () ; delay evaluation
	       (within-continuation k
				    alternative)))
	   alternatives))
     (yield)))) 

(define (yield)
  (if (stack&queue-empty? *search-schedule*)
      (*top-level* #f)
      ((pop! *search-schedule*))))

(define (add-to-depth-first-search-schedule alts)
  (for-each (lambda (alt)
	      (push! *search-schedule* alt))
	    (reverse alts)))

(define (add-to-breadth-first-search-schedule alts)
  (for-each (lambda (alt)
	      (add-to-end! *search-schedule* alt))
	    alts))

(define (with-depth-first-schedule thunk)
  (call-with-current-continuation
   (lambda (k)
     (fluid-let
	 ((add-to-search-schedule
	   add-to-depth-first-search-schedule)
	  (*search-schedule*
	   (empty-search-schedule))
	  (*top-level k))
       (thunk)))))

;;; Now you can do the backtracking code from the beginning,
;;; depth-first or breadth-first. Consider that if there's side
;;; effects, breadth-first makes no sense at all. 

;;; Two kinds of assignments: one you backtrack over and one that is
;;; permanent. mit-scheme's set! is persistent. However, I could
;;; prouce an alternative amb-set!

(define-syntax amb-set!
  (sc-macro-transformer
   (lambda (form uenv)
     (compile-amb-set (cadr form)
		      (caddr form)
		      uenv))))

(define (compile-amb-set var val-expr uenv)
  (let ((var (close-syntax var uenv))
	(val (close-syntax val-expr uenv)))
    `(let ...)))

;;; With amb I can write a program that doesn't know there's more than
;;; one answer. It's a way of factoring out some loops. On the other
;;; hand I might want to collect all the results of some amb
;;; thing. Make amb-collect-values. The moral of the story is the
;;; power of continuations. We'll deal with this a lot in upcoming
;;; weeks. How do you make a timesharing system? Coroutines? 

;;; 3-19-14
;;;
;;; Today more continuations and interesting fancy control structures.
;;; Starting with the same fringe problem.

(define (fringe tree)
  (cond ((pair? tree)
	 (append (fringe (car tree)) ; append together the lists of
				     ; terminal elements created in
				     ; the else condition
		 (fringe (cdr tree)))) 
	((null? tree)
	 '())
	(else
	 (list tree)))) ; make a list of all terminal elements

(define (append l1 l2)
  (if (pair? l1)
      (cons (car l1) (append (cdr l1) l2))
      l2))

;;; This defines the concept of fringe for today. 

(fringe '((a b) c ((d)) e (f ((g h)))))
;Value: (a b c d e f g h)

;;; What I just wrote is very inefficient. The call to append is
;;; turning this into an N^2 process. Here's an O(N) process:

(define (fringe subtree)
  (define (walk subtree ans)
    (cond ((pair? subtree)
	   (walk (car subtree)
		 (walk (cdr subtree)
		       ans)))
	  ((null? subtree) ans)
	  (else (cons subtree ans))))
  (walk subtree '()))

;;; The real question is the same-fringe problem: how do I tell
;;; whether two trees are the same? Here's the stupid way:

(define (same-fringe? tree1 tree2)
  (equal? (fringe tree1) (fringe tree2)))

;;; What if the trees are infinite? Then this is very stupid. 

(define (lazy-fringe subtree)
  (cond ((pair? subtree)
	 (append-deferred
	  (lazy-fringe (car subtree))
	  (lambda ()
	    (lazy-fringe (cdr subtree)))))
	((null? subtree) the-empty-sgtream)
	(else (stream subtree))))

(define (lazy-same-fringe? tree1 tree2)
  (let loop ((f1 (lazy-fringe tree1))
	     (f2 (lazy-fringe tree2)))
    (cond ((and (stream-null? f1)
		(stream-null? f2))
	   #t)
	  ((or (stream-null? f1)
	       (stream-null? f2))
	   #f)
	  ((eq? (stream-car f1)
		(stream-car f2))
	   (loop (stream-cdr f1)
		 (stream-cdf f2)))
	  (else #f))))

(define (append-deferred stream1 stream2-thunk)
  (if (stream-pair? stream1)
      (cons-stream
       (stream-car stream1)
       (append-deferred (stream-cdr stream1)
			stream2-thunk))
      (stream2-thunk)))

(define the-empty-stream (stream))

;;; Why does stream2 have to be a thunk? What would go wrong? If I
;;; didn't do that, then the very first element of stream2 would be
;;; computed, which might take infinitely long! 

;;; Let's make coroutines: a producer and a consumer. Make one for
;;; each fringe. I tried to simplify it last night and can't figure
;;; out the bug I got. The first step to enlightenment is confusion.

(define *done* (list '*done)) ; a common trick: a sentinel value. No
			      ; one can type it in. 

(define (coroutine-fringe-generator tree)
  (define (resume-thunk)
    (walk tree (lambda () *done*)))
  (define (walk subtree continue)
    (cond ((null? subtree)
	   (continue))
	  ((pair? subtree)
	   (walk (car subtree)
		(lambda ()
		  (walk (cdr subtree)
			continue))))
	  (else
	   (set! resume-thunk continue) ; an assignment!
	   subtree)))
  (lambda () (resume-thunk))) ; this works because we did
			      ; assignment. Whenever we hit the
			      ; function that this procedure returns,
			      ; we will get the updated value of
			      ; resume-thunk.

(define (coroutine-same-fringe? tree1 tree2)
  (let ((f1 (coroutine-fringe-generator tree1))
	(f2 (coroutine-fringe-generator tree2)))
    (let loop ((x1 (f1)) (x2 (f2)))
      (cond ((and (eq? x1 *done*) (eq? x2 *done))
	     #t)
	    ((or (eq? x1 *done*) (eq? x2 *done*))
	     #f)
	    ((eq? x1 x2)
	     (loop (f1) (f2)))
	    (else #f)))))

;;; This works well but has that nasty assignment! If you really want
;;; to be functional you can use the zipper.

;;; Here's where I got myself into trouble. I wrote a coroutine
;;; manager using call/cc. I want to abstract out what you just saw so
;;; I don't have to make a generator for every data structure. 

(define (make-coroutine his-job)
  (let ((resume-thunk) (k_yield))
    (define (my-job value)
      (call-with-current-continuation
       (lambda (k_his-job)) 
       (set! resume-thunk
	     (lambda ()
	       (k_his-job unspecific)))
       (k_yield value))))
  ...)

;;; This thing took me hours to debug, it's a mess! It lets you do:

(define (coroutine-fringe-generator tree)
  (lambda (return)
    (define (loop tree)
      (cond ((pair? tree)
	     (loop (car tree))
	     (loop (cdr tree)))
	    ((null? tree) unspecific)
	    (else 
	     (return tree)))) ; this return causes something to return
			      ; up in the calling function, but the
			      ; next time I call f1, I have to call
			      ; this return to return again to the
			      ; caller of the lambda procedure. 
    (define (initial-generation-coroutine-thunk)
      (loop tree)
      (return *done*))
    initial-generation-coroutine-thunk))

;;; Here's my disastrous attempt to simplify this.

(define (make-coroutine todo)
  (let ((continue #f))
    (lambda (supplicant) 
      (let ((resume-point supplicant))
	(define (return value)
	  (set! resume-point
		(call-with-current-continuation
		 (lambda (k)
		   (set! continue k)
		   (resume-point value)))))
	(if continue
	    (continue supplicant)
	    (todo return))))))

(define next-value call-with-current-continuation)

(define (list-iterator list)
  (make-coroutine
   (lambda (return)
     (for-each return list)
     *done*))) ; we found the trouble: this should be (return *done*)!

(define foo (list-iterator '(1 2 3)))

(next-value foo) ; it takes the continuation of this expression and
		 ; passes it in as the supplicant such that the
		 ; supplicant is the first resume point. It's going to
		 ; continue that in which case it has the property
		 ; that the supplicant will get the appropriate value.
; 1

(next-value foo)
; 2

(next-value foo)
; 3

(next-value foo)
; (*done*)

;;; Here's the problem:

(let ((foo (list-iterator '(1 2 3)))
      (bar (list-iterator '(a b c))))
  (let loop ((v (next-value foo))
	     (w (next-value bar)))
    (pp (list v w)) ; it works without this!
    (if (and (not (done? v)) (not (done? w)))
	(loop (next-value foo)
	      (next-value bar))
	'done)))


;;; 3-21-14
;;;
;;; Probabilistic programming with A. Radul.

;;; 3-31-14
;;; I was sick.

;;; 4-2-14
;;;
;;; Yesterday we talked about propagators. It may be appropriate to
;;; have multiple sources of information. Earlier I talked about
;;; degeneracy in biological systems. Today I'll talk about
;;; information coming from multiple sources. I'll start with
;;; intervals. Given a stopwatch, ruler, and barometer, how many ways
;;; can you measure the height of a building? 

;;; Drop the barometer and measure how long it takes.

(define (fall-duration t h)
  (compound-propagator t
		       (lambda ()
			 (let ((g (make-cell))
			       (one-half (make-cell))
			       (t^2 (make-cell))
			       (gt^2 (make-cell)))
			   ((constant (make-interval 9.789 9.832)) g)
			   ((constant (make-interval 1/2 1/2))
			    one-half)
			   (squarer t t^2)
			   (multiplier g t^2 gt^2)
			   (multiplier one-half gt^2 h)))))

(define fall-time (make-cell))
(define building-height (make-cell))

(fall-duration fall-time building-height)

(add-content fall-time (make-interval 2.9 3.1))
(run)
(content building-height)
#(interval 41.163 47.263)


;;; Use similar triangles: 

(define (similar-triangles s-ba h-ba s h)
  (compound-propagator (list s-ba h-ba s)
		       (lambda ()
			 (let ((ratio (make-cell)))
			   (divider h-ba s-ba ratio)
			   (multiplier s ratio h)))))

(define barometer-height (make-cell))
(define barometer-shadow (make-cell))
(define building-height (make-cell))
(define building-shadow (make-cell))
(similar-triangles barometer-shadow barometer-height
		   building-shadow building-height)

(add-content building-shadow
	     (make-interval 54.9 55.1))
...

(run)
(content building-height)
#(interval 44.514 48.978)

;;; then you can combine the results:

(define fall-time (make-cell))
(fall-duration fall-time building-height)
...
(content building-height)
;;; and you get a better interval out.

;;; Let's keep intervals: all positive and all finite. 

(define (mul-interval x y)
  (make-interval
   (* (interval-low x) (interval-low y))
   (* (interval-high x) (interval-high y))))

(define multiplier
  (function->propagator-constructor mul-interval))

(define (make-cell)
  (let ((neighbors '()) (content nothing))
    (define (new-neighbor neighbors)
      (begin
	(set! neighbors
	      (cons new-neighbor neighbors))))
    (define (add-content increment)
      (cond ((nothing? increment) 'ok)
	    ((nothing? content) 
	     (set! content increment)
	     (alert-propagators neighbors))
	    (else
	     (let ((new-range
		    (intersect-intervals content increment)))
	       (cond ((equal? new-range content) 'ok)
		     ((empty-interval? new-range)
		      (error "Inconsistency!"))
		     (else (set! content new-range)
			   (alert-propagators neighbors)))))))
    (define (me message)
      (cond ((eq? message 'new-neighbor!)
	     new-neighbor!)
	    ((eq? message 'add-content)
	     add-content)
	    ((eq? message 'content)
	     content)
	    (else (error "Unknown message!"))))
    me))

;;; If I know c is the sum of a and b and if I know c and a I can
;;; compute b. How can we do that?

;;; You can use propagators to enforce constraints if the output of
;;; one leads into the input of another whose output is input to the
;;; first one. 

(define (product x y total)
  (multiplier x y total)
  (divider total x y)
  (divider total y x))

(define (quadratic x x^2)
  (squarer x x^2)
  (sqrter x^2 x))

;;; Build up a pile of cells for the barometer problem. Now when we
;;; combine the measurements of building-height from fall-time and
;;; similar-triangles, we get a more exact estimate of
;;; barometer-height and fall-time! Now we need a way to enforce that
;;; stuff like the gravitational constant is truly constant and can't
;;; be updated...

;;; The earliest stuff in constraint propagation that was impressive
;;; was Ivan Sutherland's computer graphics. But the best is David
;;; Waltz's PhD thesis about what are the possible line drawings you
;;; can have. 

;;; Anyway you can also trade your stopwatch for the building plans.

(add-content building-height (make-interval 45 45))
(run)
(content barometer-height)
#(interval .3 .30328)

;;; This is partial information. Another kind of partial information
;;; is symbolic information where you know some variables but not
;;; others and combine them using unification. 

(define (make-cell)
  (let ((neighbors '()) (content nothing))
    (define (new-neighbor neighbors)
      (begin
	(set! neighbors
	      (cons new-neighbor neighbors))))

    (define (add-content increment)
      (let ((answer (merge content increment)))
	(cond ((eq? answer content) 'ok)
	      ((contradictory? answer)
	       (error "bad"))
	      ...)))

    (define (me message)
      (cond ((eq? message 'new-neighbor!)
	     new-neighbor!)
	    ((eq? message 'add-content)
	     add-content)
	    ((eq? message 'content)
	     content)
	    (else (error "Unknown message!"))))
    me))

(define merge (make-generic-operator 2 'merge
				     (lambda (content increment)
				       ...)))

(define (the-contradiction (list 'contradiction)))
(define (contradictory? x) (eq? x the-contradiction))

(defhandler 'merge
  (lambda (content increment) content)
  any? nothing?)

(define generic-+ (make-generic-operator 2 '+ +))
(define adder
  (function->propagator-constructor generic-+))

;;; This was the second step in the development of the propagator
;;; world. The next step is going to be carrying provenance or
;;; dependencies. This lets you do things like temporarily forget what
;;; the superintendant told you. You can do very optimal search. 

;;; 4-4-2014
;;;
;;; Today I am out of battery. 

;;; 4-7-2014
;;;
;;; So far there have been 3 propagator lectures: simple mechanism,
;;; partial information, and dependency tracking. Today is about
;;; hypothetical reasoning. 

(define (kick-out! premise)
  (if (premise-in? premise)
      (alert-all-propagators)) ; nasty!
  (mark-premise-out premise))

;;; When there's a contradiction we have the "nogood set": the set of
;;; premises that can't be true together. Having this lets us avoid
;;; checking premises that depend on a nogood set. 

(define (check-consistent! v&s)
  (if (contradictory? v&s)
      (process-nogood! (v&s-support v&s))))

(define (process-nogood! nogood) 
  (abort-process `(contradiction ,nogood))) ; we'll fix this up today.

;;; We want "dependency-directed backtracking". 
;;; 1. Avoiding known nogoods,
;;; 2. Avoiding recomputing things that do not depend on a nogood.

(define (process-nogood! nogood)
  (process-one-contradiction nogood))

;;; Introduce a new idea called a hypothetical premise. This is
;;; retractable without the user or any outside process getting
;;; involved. 

(define (process-one-contradiction nogood)
  (let ((hypotheses (filter hypothetical? nogood))) ; are there
					; hypotheticals in there?
    (if (null? hypotheses) ; if not, can't do anything
	(abort-process `(contradiction ,nogood))
	(begin (kick-out! (car hypotheses)) ; if so, choose to kick
					; out one hypothesis. 
	       (for-each
		(lambda (premise) ; for every no good premise, tell
				  ; that premise which nogood it is
				  ; associated with: what it can't
				  ; live with. Every premise has to be
				  ; told which premises it can't
				  ; coexist with at that point.
		  (assimilate-nogood! premise nogood))
		nogood)))))

(define (assimilate-nogood! premise new-nogood)
  (let ((item (delq premise new-nogood))
	(set (premise-nogoods premise)))
    (if (any (lambda (old) (subset? old item)) set)
	#f
	(let ((subsumed
	       (filter (lambda (old) (subset? item old))
		       set)))
	  (set-premise-nogoods! premise
				(set-adjoin
				 (set-difference set subsumed)
				 item))))))


;;; Now interesting things are happening. What does it mean to say
;;; forbid or require? 

(define (require cell)
  ((constant #t) cell))

(define (forbid cell)
  ((constant #f) cell))

(define (require-distinct cells)
  (for-each-distinct-pair
   (lambda (c1 c2)
     (let ((p (make-cell)))
       (=? c1 c2 p)
       (forbid p)))
   cells))

(define (one-of values output-cell)
  (let ((cells
	 (map (lambda (value)
		(let ((cel (make-cell)))
		  ((constant value) cell)
		  cell))
	      values)))
    (one-of-the-cells cells output-cell)))

;;; one-of-the-cells makes a chain of prop networks that choose cells
;;; cell using amb devices to make binary choices. 

;;; Here's the real guts of the matter. What goes on with the amb
;;; device? It's a binary-amb which makes hypotheticals for the belief
;;; whether something is true or false. 

(define (binary-amb cell)
  (let ((true-premise (make-hypothetical))
	(false-premise (make-hypothetical)))
    (define (amb-choose)
      (let ((reasons-against-true
	     (filter all-premises-in? ; here is where you avoid
				      ; enormous amounts of search. 
		     (premise-nogoods true-premise))) ; get the nogoods
	    (reasons-against-false
	     (filter all-premises-in?
		     (premise-nogoods false-premise))))
	(cond ((null? reasons-against-true)
	       (kick-out! false-premise) ; it would be nice if these
					; changes were themselves
					; propagated in a distributed
					; rather than done centrally
					; here. 
	       (bring-in! true-premise))
	      ((null? reasons-against-false)
	       (kick-out! true-premise)
	       (bring-in! false-premise))
	      (else ; this amb must fail. Resolution step. 
	       (kick-out! true-premise) ; make both of them false
	       (kick-out! false-premise)
	       (process-contradictions
		(pairwise-union
		 reasons-against-true ; then take the reasons against
				      ; true and the reasons against
				      ; false and make up new reasons
				      ; that are by themselves
				      ; contradictory. 
		 reasons-against-false)))))) 
    ((constant
      (make-tms
       (list (supported #t (list true-premise))
	     (supported #f (list false-premise)))))
     cell)
    (propagator cell amb-choose)))

;;; Suppose there are two known nogoods: {A, B, C} and {~A, E, F}. The
;;; first are reasons A cannot be true and the second are reasons A
;;; cannot be false. The two sets cancel giving {B, C, E, F} which
;;; together can't be true because it implies the contradiction about
;;; A: this is the cut rule. 

(define (process-contradictions nogoods)
  (process-one-contradiction
   (car (sort-by nogoods
		 (lambda (nogood)
		   (length (filter hypothetical? nogood)))))))


;;; So we have a naive, implicit, distributed SAT solver as part of
;;; the programming language now. Nogoods are associated with the
;;; associated premise. The part where you avoid doing something bad
;;; is in the binary amb.

;;; Next time we'll make a complete operating system.

;;; 4-9-2014
;;;
;;; Today we will make an operating system in a Lisp. People do it.
;;; Some of the code in the problem set will break your brain. Page
;;; 5 has the biggest brain buster rlm has ever seen. 
;;;
;;; CONSPIRE: Processes scheming together constitute a conspiracy. 
;;;
;;; Suppose I want to find membership in a circular list. 

(define (unsafe-mem? item lst)
  (let lp ((lst lst))
    (if (eq? item (car lst))
	#t
	(lp (cdr list)))
    #f))

(begin (set-cdr! last-pair foo) foo) 'foo)

(unsafe-mem? 'b foo)
; Value: #t

(unsafe-mem? 'e foo)
;Quit!

(define (safe-mem? item lst)
  (let ((table (make-eq-hash-table)))
    (let lp ((lst lst))
      (if (pair? lst)
	  (if (hash-table/get table lst #f) ; never seen this before
	      #f
	      (if (eq? item (car lst))
		  #t
		  (begin
		    (hash-table/put! ; leave a crumb
		     table lst #t)
		    (lp (cdr lst)))))
	  #f))))

;;; This is a lot slower than unsafe-mem?. So I don't want to do it
;;; all the time. I want to write this:

(define (mem? item lst)
  (try-two-ways
   (lambda ()
     (unsafe-mem? item lst))
   (lambda ()
     (safe-mem? item lst))))

;;; This motivates timesharing. Sometimes I can get better behavior by
;;; doing things in a degenerate way. Sometimes the algorithmic
;;; complexity of two algorithms running together is better than
;;; either one alone. 

(define (try-two-ways thunk1 thunk2)
  (with-time-sharing-conspiracy
   (lambda ()
     (let ((value) (done? #f))
       (let ((thread1
	      (conspire:make-thread
	       conspire:runnable ; means wait until done? becomes #t
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
	  (lambda () done?)) ; current thread not allowed to run till
			     ; done becomes true

	 (conspire:kill-threads ; fall through and kill both threads
	  (list thread1 thread2))

	 value)))))

;;; Let's talk about the notion of time in the program. Assignment
;;; introduces time. Amb introduces branching in time. Now when we add
;;; concurrency we're dealing with time as a partial order. There's no
;;; comparability in time between threads except when they meet. This
;;; is horrifying. 

(define (conspire:switch-threads runnable?)
  (conspire:save-current-thread runnable? ; pass in the test; call
					; this stuff x
				conspire:start-next-thread)) ; this is after-save

(define (conspire:save-current-thread
	 runnable? after-save)
  (call/cc ; get the current continuation, which is the thing where x returns
   (lambda (cc) ; put the continuation here
     (without-interrupts ; in a context where nothing else can happen,
			 ; in order to protect the assignments
      (lambda ()
	(conspire:set-continuation! ; set the continuation OF THE
				    ; THREAD to the current continuation
	 *running-thread* cc)
	(conspire:add-to-schedule! 
	 runnable? *running-thread*))) ; associate the running thread
				       ; with the runnability test
     (after-save)))) ; which is conspire:start-next-thread

(define (conspire:start-next-thread)
  ((conspire:continuation
    (without-interrupts
     (lambda ()
       (set! *running-thread*
	     (conspire:get-runnable-thread-from-schedule))  ; run it
       *running-thread*)))
   unspecific))
    
;;; So how do I make a thread?

(define (conspire:make-thread runnable? thunk)
  (call/cc
   (lambda (cc)
     (within-continuation *root-continuation*
			  (lambda ()
			    (call/cc
			     (lambda (new-c) ; the beginning of the
					; new thread--you start it by
					; calling this continuation
			       (cc
				(without-interrupts
				 (lambda ()
				   (let 
				       ((new-thread ; first physically
					 ; make the thread with the
					 ; new continuation in it
					 (conspire:make-new-thread
					  new-c)))
				     (conspire:add-to-schedule! ; add
					; it to the scheduler if it's
					; runnable. 
				      runnable? new-thread)
				     new-thread)))))) ; return the new
					; thread, wich will come out
					; of the call to cc and it
					; will be the thing returned
					; when I call the current
					; continuation with it
			    (thunk)
			    (conspire:kill-current-thread))))))

;;; I want the thread-maker to return that new thread. 

(define (conspire:add-to-schedule!
	 runnable? thread)
  (queue:add-to-end! *thread-queue* ; just add the runnable test and
				    ; the thread to the end of the queue
		     (cons runnable? thread)))

(define (conspire:get-runnable-thread-from-schedule!)
  (if (not (queue:empty? *thread-queue*))
      (let lp ((first (queue:get-first
		       *thread-queue*)))
	(if ((car first)) ; do the runnable test, if it's true
	    (cdr first) ; then return the cdr which is the thread
	    (begin ; otherwise rotate it to the end
	      (queue:add-to-end! *thread-queue*
				 first)
	      (lp (queue:get-first
		   *thread-queue*)))))
      (error "no current thread")))

(define (conspire:delete-from-schedule! thread)
  (let ((entry
	 (find-matching-item
	  (queue:front-ptr *thread-queue*)
	  (lambda (entry)
	    (eq? (cdr entry) thread)))))
    (if entry
	(queue:delete-from-queue!
	 *thread-queue* entry))))

;;; OK, now let's get it running. 

(define (with-conspiracy thunk)
  (fluid-let ((*running-thread*
	       (conspire:make-new-thread
		unspecific))
	      (*thread-queue* (queue:make))
	      (*root-continuation*))
    (call/cc
     (lambda (k)
       (set! *root-continuation* k)
       (thunk)))))

(define *running-thread*)
(define *thread-queue*)
(define *root-continuation*)

(define (conspire:thread-yield)
  (conspire:switch-threads
   conspire:runnable))

(define conspire:runnable (lambda () #t))

(define (conspire:kill-current-thread)
  (without-interrupts
   (lambda ()
     (conspire:start-next-thread))))

;;; Let's do some simple examples. First cooperative multiprocessing
;;; them preemptive. 

(define (loop n)
  (let lp ((i 0))
    (if (< global-counter 1)
	'done
	(begin
	  (set! global-counter
		(- global-counter 1))
	  (if (= i n)
	      (begin
		(write-lin
		 `(,n ,global-counter))
		(conspire:thread-yield)
		(lp 0))
	      (lp (+ i 1)))))))



;;; 4-11-14

;;; Today a strategy for solving equations. 

;;; In solving equations you look for a variable you can isolate then
;;; make a substitution into the rest of the equations. If they're
;;; nonlinear then you often hit a wall. Above quadratic, you're
;;; screwed and have to use numerical methods. This method is Gaussian
;;; elimination. We don't really know of any other methods. 

(set! *tms-present* #f)

(pp (solve-incremental
     (list (make-equation
	    '(+ (* 3 x) y -7) ; represents the residual: if it's not
			      ; satisfied, this is the error you
			      ; get. The answer should be 0.
	    (list 'A)) ; give the equation a name
	   (make-equation
	    '(+ (* 3 x) (- y) -5)
	    (list 'B)))
     '(x y))) ; solve for x and y

;;; That was 3x+y=7 and 3x-y=5. 

(pp (solve-incremental
     (list (make-equation
	    '(+ x y z 1) ; x + y + z + 1 = 0
	    (list 'A))
	   (make-equation
	    '(+ x y 2) ; x + y + 2 = 0
	    (list 'B))
	   (make-equation
	    (+ x 1) ; x + 1 = 0
	    (list 'C)))
     '(x y z)))

(() ; unsolveable equations
 () ; free variables
 (((= z 1) (A B C))
  ((= y -1) (B C))
  ((= x -1) (C))) ; only depends on C!
 ()) ; hopeless variables

;;; What about 3x+y-7=0 and 3x+y-5=0? Then you get

(contradiction (B A))
(((2 (B A)))  ; leftover hopeless equation: 2 = 0
 ()
 (((= x (+ 7/3 whatever)))) ; some random substitution the machine
			    ; made that wasn't useful. 
 (y))

;;; These programs can solve some nonlinear systems.

(pp (solve-incremental
     (list (make-equation
	    '(- 3 (+ x y))
	    (list 'A))
	   (make-equation
	    '(- 5 (- x y))
	    (list 'B))
	   (make-equation
	    '(- 3 (+ (* (sqrt x) z) ; multiplication so can't do
				    ; matrix stuff; it's got squares
				    ; and square roots and stuff too
		     (square y)))
	    (list 'C)))
     '(x y z)))

(()
 ()
 (((= z 1) (C B A))
  ((= y -1) (B A))
  ((= x 4) (B A)))
 ())

;;; When you solve x and y out of the linear part then this becomes a
;;; linear equation in z. There are really 2 solutions but we won't
;;; worry about that now since we can use amb to handle it. 

(pp (solve-incremental
     (list (make-equation
	    '(+ (* (+ a b) (- a c)) c)
	    (list 'A))
	   (make-equation
	    '(- 3 (+ a b))
	    (list 'B)))
     '(a b c)))

(()
 (c) ; c is a free variable
 (((= b (+ 3 (* -2/3 c))) (A B))
  ((= a (* 2/3 c)) (A B)))
 ())


;;; How do we get this fail-soft behavior? 

;;; 1. choose a variable to eliminate
;;; 2. look for an eq for which that variable can be isolated
;;; 3. isolate and make substitution for that variable
;;; 4. eliminate from remaining equations
;;; 5. use the substitution to eliminate var from other substitutions
;;; (this is what people usually forget)
;;; 6. if more vars and equations, go back to 1.

(define (solve-incremental equations variables)
  (let lp ((residual-eqs equations) ; I have the initial equations
	   (residual-vars variables)
	   (substitutions '()) 
	   (hopeless-vars '()))
    (if (or (null? residual-vars) ; we are done
	    (null? residual-eqs))
	(list (residual-eqs ; result
	       residual-vars
	       substitutions
	       hopeless-vars)
	      (isolate-var 
	       (car residual-vars) ; something stupid: pick out a
				   ; random variable (the first
				   ; one). A smarter strategy is to
				   ; pick one that has good properties
				   ; such as that it occurs in many
				   ; places. 
	       (sort residual-eqs fewer-variables?) ; sort the
					; residual equations so we
					; look at the ones with the
					; fewest variables first
	       (lambda (new-substitution equation-used) ; if it works
					; I do this. This is a
					; contintuation function. It
					; says it did work and I got a
					; new substitution. SUCCESS
					; CONTINUATION.
		 (lp (flush-tautologies
		      (next-equations
		       new-substitution
		       (delete equation-used ; delete that from the
					; residuals
			       residual-eqs)))
		     (cdr residual-vars) ; ones I haven't used yet
		     (cons new-substitution
			   (next-substitutions
			    new-substitution ; use the new
					; substitution to eliminate
					; the variable: this goes into
					; the substitutions in the loop.
			    substitutions))
		     hopeless-vars)) ; add nothing to hopeless-vars
	       (lambda () ; otherwise I do this. FAILURE CONTINUATION.
		 (lp residual-eqs
		     (cdr residual-vars) ; cdr off the residual variables
		     substitutions 
		     (cons (car residual-vars) ; add this variable to
					; the hopeless
					; variables. Really, this
					; isn't good enough since
					; there's an order
					; dependence. 
			   hopeless-vars))))))))

(define (fewer-variables? eqn1 eqn2)
  (< (length (equation-variables eqn1)
	     (equation-variables eqn2))))

(define (flush-tautologies equations)
  (filter (lambda (eqn)
	    (let ((expr (equation-expression eqn)))
	      (not (and (number? expr)
			(= expr 0)))))
	  equations))

(define (next-equations substitution equations)
  (map (lambda (equation)
	 (backsubstitute-equation
	  substitution
	  equation))
       equations))

;; next-substitutions is the same thing for substitutions

;;; here's isolating the variables:

(define (isolate-var var eqs succeed fail)
  (let lp ((eqns-to-scan eqs))
    (cons ((null? eqns-to-scan) (fail)) ; call the failure continuation
	  ((occurs? var (car eqns-to-scan))
	   (isolatable? var (car eqns-to-scan)
			(lambda (value) ; SUCCESS CONTINUATION
			  (succeed ; call the passed-in success
				   ; continuation with the
				   ; substitution specified by the
				   ; value which will be supplied by isolatable?
			   (make-substitution var value
					      (equation-justifications
					       (car eqns-to-scan)))
			   (car eqns-to-scan)))
			(lambda () ; FAILURE CONTINUATION
			  (lp (cdr eqns-to-scan)))))
	  (else (lp (cdr eqns-to-scan))))))

(define (occurs? var expr) ; recursive membership
  (or (equal? var expr)
      (and (pair? expr)
	   (or (occurs? var (car expr))
	       (occurs? var (cdr expr))))))
			    
;;; Now the real nastiness! 

(define (isolatable? var eqn succeed fail)
  (let lp ((expr (equation-expression eqn)))
    (cond ((equal? var expr) (succeed 0))
	  ((positive-power? expr)
	   (lp (car (operands expr))))
	  ((product? expr)
	   (var-in-product var expr succeed fail))
	  ((sum? expr)
	   (var-in-sum var expr succeed fail))
	  (else (fail)))))

(define (positive-power? expr)
  (and (expt? expr)
       (number? (cadr (operands expr)))
       (> (cadr (operands expr)) 0)))

(define (var-in-product var expr succeed fail)
  (let lp ((factors (operands expr)))
    (if (pair? factors)
	(let ((ff (car factors)))
	  (cond ((and (symbol? ff)
		      (equal? var ff))
		 (succeed 0))
		((and (positive-power? ff)
		      (equal? var 
			      (car (operands ff))))
		 (succeed 0))
		(else 
		 (lp (cdr factors)))))
	(fail))))

(define (var-in-sum var expr succeed fail)
  'something-horrible)

(define (backsubstitute-substitution ; wow, long names!
	 new-substitution substitution)
  (make-substitution
   (substitution-variable substitution)
   (substitute
    (substitution-expression new-substitution)
    (substitution-variable new-substitution)
    (substitution-expression substitution))
   (list-union ; new set of dependencies
    (substitution-justifications substitution)
    (substitution-justifications new-substitution))))

(define (backsubstitute-equation
	 substitution quation)
  ; same sort of thing 
  )

(define (make-substitution var value justs)
  (list (list '= var (simplify value))
	justs))

;;; What to do about the nasty order dependence?

;;; Dependency tracking is something we should be doing in all
;;; code. We want to have order trails. 

;;; Q. Is this really continuation-passing style without using
;;; call/cc?
;;; A. It is the same thing, we're just not using the UNDERLYING
;;; continuations. 



;;; 4-14-2014

;;; Logic is not the answer to any important problem. 

;;; A = S -> G
;;; B = ~S -> E
;;; C = ~G -> ~E
;;; Therefore G?

;;; To identify invalid arguments, find an example of substitutions
;;; for words for which you get a false conclusion from true
;;; premises. This is the "method of interpretation". 

;;; Suppose we have binary boolean functions. There are 16 of them. 
;;; So for the problem above, write out the full table for S, E, G, A,
;;; B, C. That's 2^N--you'll always have exponentials, but we can do
;;; better. How can we do something more human-oriented? Like assuming
;;; something then getting a contradiction? 

;;; 1. S -> G {1}
;;; 2. ~S -> E {2}
;;; 3. ~G -> E {3}
;;; 4. ~G {4} ; hypothesis
;;; 5. ~E MP 3,4 {3 4}
;;; 6. ~~S MT 2,5 {2 3 4}
;;; 7. S NE 6 {2 3 4}
;;; 8. ~S MT 1,4 {1 4}
;;; 9. G RAA 7,8;4 {1 2 3}

;;; RAA actually subtracts a premise (4) as indicated by the
;;; semicolon. 

;;; When we have quantifiers and variables it becomes a lot harder to
;;; justify things. Next time we'll talk about unification and after
;;; that about the formal rules that are appropriate. 

;;; Sentential rules.
;;; 0. Introductino of a premise
;;; n A premise {n}

;;; 1 Modus Ponens
;;; n A->B --- d1
;;; m A    --- d2
;;; -------------
;;; o B   (MP n m) d1 U d2

;;; 2. Modus Tollens
;;; 3. Conditional proof
;;; n A premise {n}
;;; m B ---     d
;;; ---------------------
;;; o A->B (CP m n) d - {n}

;;; This is the deduction theorem: a way of removing a dependency
;;; n. If I believe A and conclude B, then I can deduce A->B (it's ok
;;; if A turns out to be false because F->T). A needs to be a premise
;;; otherwise you can't remove the dependency. 

;;; 4. Simplification, 5. Adjunction, 6. Addition, ...
;;; Reductio ad Absurdum
;;; n premise {n}
;;; m --- d1
;;; o --- d2
;;; ----------------
;;; p ~A (RAA m n o) d1 U d2 - {n}

;;; Separation of Cases

;;; 4-16-14
;;;
;;; J. A. Robinson (1965): A Machine-oriented logic based on the
;;; resolution principle. 
;;;
;;; Say we have f(x, y, z). Without knowing x, y, z, we know f(A, y,
;;; z) or f(x, B, z) or f(x, y, C) are substitution instances of the
;;; original. f(A, B, z) etc. are substitution instances of two
;;; things. Finally we have f(A, B, C) which is the substitution
;;; instances of those last 3. This is a partial order. 
;;;
;;; Suppose I have f(x, y). If I know x and y are the same I know it's
;;; f(x, x) or f(A, x) or f(B, x) or f(x, A) or f(x, B). Then f(A, A),
;;; f(A, B) or f(B, B). These increase in information; they are not
;;; necessarily true. 
;;;
;;; Suppose you have a predicate P ("ugly") and a function f ("'s
;;; mother"). P(x) is parent of P(f(y)) and P(g(y)) and P(h(y,
;;; z)). P(f(y)) -> P(f(f(w))). 
;;;
;;; Unification is a way to gather information together. Both the
;;; pattern and the thing it's matching against are patterns that both
;;; have information and I want to merge the information. 

(define (unify-test p1 p2 expected)
  (equal? expected (unify p1 p2 '())))

(unify-test '((? x) (? x)) '(3 4) #f)
(unify-test '((? x) 4) '(3 (? x)) #f)
(unify-test '((? x) 4) '((? y) (? y))
	    '((y 4) (x (? y))))

(unify-test '(p (f a) (g (? x)))
	    '(p (? x) (? y))
	    '((y (g (? x))) (x (f a))))

(define (unify p1 p2 dict)
  (cond ((equal? p1 p2) dict)
	((variable? p1)
	 (extend-if-possible p1 p2 dict))
	((variable? p2)
	 (extend-if-possible p2 p1 dict))
	((and (pair? p1) (pair? p2))
	 (let ((dict 
		(unify (car p1)
		       (car p2)
		       dict)))
	   (if dict
	       (unify (cdr p1)
		      (cdr p2)
		      dict)
	       #f)))
	(else #f)))

(define (extend-if-possible var val dict)
  (let ((binding1 (lookup var dict)))
    (cond (binding1
	   (unify (value binding1) val dict)) ; recursive
	  ((variable? val)
	   (let ((binding2
		  (lookup val dict)))
	     (if binding2
		 (unify var
			(value binding2)
			dict)
		 (bind var val dict))))
	  ((depends-on? val var dict) #f)
	  (else (bind var bal dict)))))

;;; Suppose (? x) is matched against (f (? x)). That will produce (f
;;; (f (f (f ...)))). depends-on? catches these cases and just says we
;;; can't solve these cases (you could in principle solve for the
;;; fixpoint). 

(define (depends-on? expr var dict) 
  (define (tree-walk e)
    (cond ((variable? e)
	   (if (same-var? var e)
	       #t
	       (let ((binding
		      (lookup e dict)))
		 (if binding
		     (tree-walk
		      (value binding))
		     #f))))
	  ((pair? e)
	   (or (tree-walk (car e))
	       (tree-walk (cdr e))))
	  (else #f)))
  (tree-walk expr))

;;; Now you write a thing so when you backsubstitute you get the most
;;; informative result. Or you can think of this as solving equations.

(define (unify t1 t2)
  (let lp ((xs (list t1))
	   (ys (list t2))
	   (dict '()))
    (cond ((and (null? xs) (null? ys)) dict)
	  ((or (null? xs) (null? ys)) #f)
	  (else
	   (let ((x (unify:value (car xs) dict))
		 (y (unify:value (car ys) dict)))
	     (cond ((equal? x y)
		    (lp (cdr xs) (cdr ys) dict))
		   ((variable? x)
		    (and (not (occurs-in? x y))
			 (lp (cdr xs) (cdr ys)
			     (bind x y dict))))
		   ((variable? y)
		    (and (not (occurs-in? y x))
			 (lp cdr xs) (cdr ys)
			 (bind y x dict))))
	     ((and (pair? x) (pair? y))
	      (lp (append x (cdr xs)
			  ...))))))))

;;; Now let's go back to the partial orders. How to find the MOST
;;; GENERAL COMMON SPECIFICATION? Plotkin (1970) also talks about the
;;; least general (LGG). This is the dual of the unification
;;; algorithm. 

(pp (lgg '(is-a Rover dog) '(is-a Spot dog) '()))
((is-a (? x4) dog)
 (((rover spot) (? x4))))

;;; There's another tricky thing we do called hash-cons. 

;;; McCarthy invented LISP to make a simple version of Goedel's
;;; incompleteness theorem. We have x and #x which is the number of
;;; x. Then #(x y z 2) = 2^#x * 3^#y * 5^#z * 7^#w. This is a big
;;; number. I can get car of it: the number of x is the number of
;;; powers of 2. That's Goedel's system, but McCarthy wanted to make
;;; new numbers. Natural numbers are described by Peano's axioms: 0 is
;;; a number. If x is a natural number then s(x) is too. If two
;;; numbers have the same successor then they are the same. 0 is not
;;; successor of any number. If something is true of 0 and that
;;; thing's being true of n implies it's true of n+1.

;;; McCarthy says for any two sexprs there is their cons (that's like
;;; their successor). If the cons is the same then the car and the cdr
;;; are the same. Symbols are not the cons of anything. Finally an
;;; induction rule. 

;;; hash-cons: If I've consed these two things together I want to get
;;; the same thing when I do it again. You can make a canonical copy:
;;; then the two copies are in the same memory address.

;;; 4-18-14
;;;
;;; P = George's mother is ugly
;;; R = Harry's mother is ugly
;;; Q = George's mother is more stupid than Harry.
;;; S = Harry's mother is more stupid than George.
;;; Let (Um x) = x's mother is ugly
;;; Then P = (Um George) and R = (Um Harry)
;;; Let (MS x y) = x's mother is more stupid than y
;;; So Q = (MS George Harry)
;;;    S = (MS Harry George)
;;;
;;; (ugly x) = x is ugly
;;; (mother x) ; assume everyone has a unique mother
;;; P = (ugly (mother George))
;;; Q = (ugly (mother Harry))
;;; T = (nice (mother George))
;;;
(more x y) ;= x is more than y
(stupidity x) ;= the stupidity of x
(and (ugly (mother George))
     (more (stupidity (mother George))
           (stupidity Harry)))

;;; What about quotation? 
;;; E.g. George says "the morning star is Venus"
;;;      The morning star is the evening star.
;;; Not -> George says "the evening star is Venus"
;;; You can't substitute equals for equals inside a quotation.
;;; We won't talk about this now.

;;; How about "there is no one stupider than George"?

(not (Exists (x)
	     (more (stupidity x)
		   (stupidity George))))

(All (x)
     (implies (more (stupidity x)
		    (stupidity Harry))
	      (ugly x)))

;;; Exists is the OR of all possible things that could have been
;;; substituted for x. All is the AND of all those things. 
;;; These are binding constructs that produce new variables. 

;;; OK so what is a legitimate argument now that we have these things?

;;; All (x) man(x) -> mortal(x)
;;; man(Socrates)
;;; ------------------
;;; mortal(Socrates)

;;; We use the method of interpretation: don't want to deduce a false
;;; result from true antecedents. 

;;; All(x) Every(y) P(x,y) NOT-> Every(y) All(x) P(x, y)
;;; Every(x) All(y) P(x,y) -> All(x) Every(y) P(x, y)
;;; The first one is not true for integers P(x,y) = x <= y
;;; For the second one, there is no interpretation that is wrong. If
;;; the consequent is false then there is an x0 for which there is no
;;; y that makes p true. 

#|

1. All y [(greek y) -> (human y)] premise {1}
2. All x [(human x) -> (mortal x)] premise {2}
3. (greek *G) premise {3}
4. (greek *G) -> (human *G) (US 1 *G y) {1} ; universal specialization
5. (human *G) (MP 4 3) {1 3}
6. (human *G) -> (mortal *G) (US 2 *G x) {2}
7. (mortal *G) (MP 6 5) {1 2 3}
8. (greek *G) -> (mortal *G) (CP 7 3) {1 2} ; conditional proof
9. All z [(greek z) -> (mortal z)] (UG 8 z *G) {1 2} ; universal generalization

|#

;;; Universal generalization. Substitute an arbitrary value for a
;;; constant. Suppose I'd written:
;;;
;;; 10. All z mortal(z) (UG 7 z *G) {1 2 3}
;;;
;;; It's not fine because it depends on 3, which was a premise. 
;;; But we could do this (redundantly):
;;;
;;; 13. All z [(human z) -> (mortal z)] (UG 6 z *G) {2}
;;;
;;; Rule:
;;; n    P               ---          d
;;; -----------------------------------
;;; m   All x S[x;t;P]  (UG n x t)    d
;;; (substituting x for t in P)
;;;
;;; Universal specification:
;;; n   All x P               ---          d
;;; -----------------------------------
;;; m   S[x;t;P]             (UG n t x)    d
;;;
;;; With restrictions.
;;;
;;; How about existential generalization? 
;;; (greek Socrates) -> Exists x (greek x)
;;; (greek (mother Socrates)) -> Exists x (greek x)
;;; 
;;; All x (> (+ x 1) x)
;;; NOT-> Exists y (All x (> y x))
;;;
;;; The problem is the original predicate contained a variable. So
;;; there's a restriction on generalization.
;;; Existential Generalization has a rule: t is a term which contains
;;; no variables and x may not appear in P.
;;; 
;;; n      P                 ------     d
;;; -------------------------------------
;;; m  Exists x S[x;t;P]   (EG n x t)   d
;;;
;;; Existential specification:
;;;
;;; n  Exists x P            ------     d
;;; -------------------------------------
;;; m  S[x;t;P]            (ES n t x)   d
;;;
;;; Restriction: ...
;;;
;;; Suppose we know:
;;; Exists x (horse x)
;;; All y [(horse y) -> (animal y)]
;;; ?-> Exists z (animal z)
;;; 
;;; Consider one of the horses that is asserted to exist. It must be
;;; an animal hence there must be an animal. 
;;;
;;; Exists x (horse x) NOT-> (horse Socrates)
;;; So ES must produce a brand new constant.
;;; The choice for existential has to depend on existing variables.
;;; All x Exists y P(x, y) -> P(x*, f(x*)) where f(x*) is the y I can
;;; choose. This was invented by Skolem. When you specify the exists
;;; you have to manufacture a function of all things that have already
;;; been established as arbitrary individuals. 
;;;
;;; Restriction on ES:
;;; In this rule t is a term composed of a new function symbol applied
;;; to the list of the arbitrary individuals occuring in P.
;;;
;;; Jeynes on probability.
;;; 

;;; 4-23-14
;;;
;;; Robert McIntyre will argue today.
;;;
;;; 
