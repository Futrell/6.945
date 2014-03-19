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
