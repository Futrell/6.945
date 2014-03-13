;;; 6.945 Problem Set 5
;;; Richard Futrell
;;; futrell@mit.edu

;;; Problem 5.1
;;;
(defhandler execute-application
  (lambda (proc args)
    (let ((body (procedure-body proc))
	  (proc-env (procedure-environment proc))
	  (parameters (procedure-parameters proc)))
      (let ((new-env 
	     (if (list? parameters)
		 (extend-environment parameters
				     args
				     proc-env)
		 (extend-environment (list parameters)
				     (list args)
				     proc-env))))
	    (body new-env))))
  compound-procedure?)

#| Tests

eval> (define cat (lambda args args))
eval> (cat 1 2 3)
(1 2 3)

eval> (define (dog . args) args)
eval> (dog 1 2 3)
(1 2 3)

eval> (define (kitten a) a)
eval> (kitten '(1 2 3))
(1 2 3)

|#

;;; Problem 5.2
;;;
;;; TODO
(define (infix? exp) (tagged-list? exp 'infix))

(define (infix-exp exp) (cadr exp))

(define (analyze-infix exp)
  (analyze (infix-to-scheme (infix-exp exp))))

(define (infix-to-scheme string)
  (define infix-operators 
    ; associate infix operators to scheme procedures
    (list '+ '+)
    (list '- '-)
    (list '* '*)
    (list '/ '/)
    (list '^ 'expt))

  (define functions
    (list 'sqrt 'sqrt))

  (define associativity
    (list '^ 'right)
    (list '* 'left)
    (list '/ 'left)
    (list '+ 'left)
    (list '- 'left))

  (define precedence
    (list '^ 4)
    (list '* 3)
    (list '/ 3)
    (list '+ 2)
    (list '- 2))

  (define (lookup x alist)
    (let ((result (assq x alist)))
      (if result
	  (cadr result)
	  (error "Couldn't find item" x))))

  (define (read-token string)
    
    
 
  
  
;;; Problem 5.3
;;; 
;;; Let's modify multiple-dwelling to print out all possible
;;; solutions:

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    ;(require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (display (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith)))
    (newline)
    (require #f)))
 
;;; So there are 4 possibilities when I leave out the requirement that
;;; Fletcher and Smith not live on adjacent floors:

#| 
((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))
((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3))
((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
((baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1))
|#

;;; The order of requirements does not affect the answer since the
;;; requirements are effectively a big conjunction and thus
;;; commutative. (Unless checking a requirement involves mutation.)
;;; But the order does affect the time to find an answer. For example,
;;; if the time to check all requirements were equal, it would be best
;;; to order them such that the requirements that come first are the
;;; ones that potentially eliminate the most options. Otherwise all
;;; the operations done to check the less-restrictive requirements
;;; will be done pointlessly. When the requirements take different
;;; amounts of time to check it is a tricker to figure out the optimal
;;; ordering.

;;; In the multiple dwellings problem there are 5*5*5*5*5 = 3125
;;; possible mappings of individuals to floors, but only 5! = 120
;;; mappings of the 5 individuals to distinct floors. Generating 
;;; 3125 - 120 forms only in order to throw them away is
;;; wasteful. Let's restructure the procedure this way, where we apply
;;; the restrictions for each person before starting to explore
;;; assignments for the next person. This is faster, but boy is it
;;; more annoying to write!

(define (multiple-dwelling)
  (let ((cooper (amb 1 2 3 4 5)))
    (require (not (= cooper 1)))

    (let ((fletcher (amb 1 2 3 4 5)))
      (require (not (= fletcher cooper)))

      (require (not (= fletcher 5)))
      (require (not (= fletcher 
		       1)))

      (require (not (= (abs (- fletcher cooper)) 1)))

      (let ((smith (amb 1 2 3 4 5)))
	(require (not (= smith fletcher)))
	(require (not (= smith cooper)))

	(require (not (= (abs (- fletcher cooper)) 1)))

	(let ((miller (amb 1 2 3 4 5)))
	  (require (not (= miller cooper)))
	  (require (not (= miller fletcher)))
	  (require (not (= miller smith)))

	  (require (> miller cooper))

	  (let ((baker (amb 1 2 3 4 5)))
	    (require (not (= baker cooper)))
	    (require (not (= baker fletcher)))
	    (require (not (= baker smith)))
	    (require (not (= baker miller)))

	    (require (not (= baker 5)))
	    
	    (list (list 'baker baker)
		  (list 'cooper cooper)
		  (list 'fletcher fletcher)
		  (list 'miller miller)
		  (list 'smith smith))))))))

#| Tests

;;; Amb-Eval input:
(multiple-dwelling)

;;; Starting a new problem
;;; Amb-Eval value:
((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))

|#

;;; Problem 5.4 
;;;

(define (yacht-puzzle)
  (define daughters 
    (list (list 'mary-ann 'moore)
	  (list 'melissa 'barnacle)
	  (list 'gabrielle (amb 'downing 'hall 'parker))
	  (list 'lorna (amb 'downing 'hall 'parker))
	  (list 'rosalind (amb 'downing 'hall 'parker))))

  (define (every pred lst) ; have to redefine this
    (if (null? lst)
	#t
	(if (pred (car lst))
	    (every pred (cdr lst))
	    #f)))

  (define (consistent pairs)
    ;; check that all pairs that have x in the car have y in the cadr.
    (if (null? pairs)
	#t
	(let ((first-pair (car 
			   pairs)))
	  (if (every (lambda (pair)
		       (if (eq? (car pair) (car first-pair))
			   (eq? (cadr pair) (cadr first-pair))
			   #t))
		 (cdr pairs))
	      (consistent (cdr pairs))
	      #f))))
	
  (define (father-of daughter)
    (let ((result (assq daughter daughters)))
      (if result
	  (cadr result)
	  (error "lookup failure:" daughter))))

  (let ((yachts (list (list 'barnacle (father-of 'gabrielle))
		      (list 'moore (father-of 'lorna))
		      (list 'hall (father-of 'rosalind))
		      (list 'downing (father-of 'melissa))
		      (list (father-of 'gabrielle) 'parker))))

    (require (distinct (map cadr daughters)))
    (require (every distinct yachts))
    (require (consistent yachts))
	   
    (newline)
    (display daughters)
    (newline)
    (require #f)))

#| Test

;;; Amb-Eval input:
(yacht-puzzle)

;;; Starting a new problem
((mary-ann moore) (melissa barnacle) (gabrielle hall) (lorna downing)
 (rosalind parker))
;;; There are no more values of (yacht-puzzle)

|#

;;; So Lorna's father is Downing. We can figure out how many solutions
;;; there are if we don't know Mary Ann's father by changing (list
;;; 'mary-ann 'moore) to (list 'mary-ann (amb 'moore 'downing 'hall
;;; 'parker)) and adding 'moore to all the amb lists for the other
;;; daughters except Melissa. Then there are two solutions:

#| 
;;; Amb-Eval input:
(yacht-puzzle)

;;; Starting a new problem
((mary-ann moore) (melissa barnacle) (gabrielle hall) (lorna downing)
 (rosalind parker))

((mary-ann hall) (melissa barnacle) (gabrielle moore) (lorna parker)
 (rosalind downing))
;;; There are no more values of (yacht-puzzle)
|#


;;; Problem 5.5 
;;;
;;; First we make set!!.

(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
        (vproc env
             (lambda (new-val val-fail)
	       (set-variable-value! var new-val env)
	       (succeed 'OK val-fail))
	     fail))))

(defhandler analyze
  analyze-permanent-assignment
  permanent-assignment?)

#| Tests

;;; Amb-Eval input:
(define count 0)
(let ((x (amb 'a 'b 'c))
      (y (amb 'a 'b 'c)))
  (set!! count (+ count 1))
  (require (not (eq? x y)))

;;; Starting a new problem
;;; Amb-Eval value:
(a b 2)

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
(a c 3)

|#

;;; To show we didn't mess up set!, here's the same thing but with
;;; nonpermanent set!; it should give a count of 1 in all cases.

#|

;; Amb-Eval input:
(define count 0)
(let ((x (amb 'a 'b 'c))
      (y (amb 'a 'b 'c)))
  (set!! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))

;;; Starting a new problem
;;; Amb-Eval value:
count

;;; Amb-Eval input:
  (list x y count))

;;; Starting a new problem
;;; Amb-Eval value:
(a b 1)

|#

;;; Now we want to make if-fail. 

(define (if-fail? exp) (tagged-list? exp 'if-fail))

(define (if-fail-predicate exp) (cadr exp))
(define (if-fail-alternative exp) (caddr exp))

(define (analyze-if-fail exp)
  (let ((pproc (analyze (if-fail-predicate exp)))
        (aproc (analyze (if-fail-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
	     succeed
	     (lambda ()
	       (aproc env succeed fail))))))

(defhandler analyze
  analyze-if-fail
  if-fail?)

#| Tests

;;; Amb-Eval input:
(if-fail (let ((x (amb 1 3 5 8)))
           (require (even? x))
           x)
         'all-odd)

;;; Starting a new problem
;;; Amb-Eval value:
all-odd

;;; Amb-Eval input:
(if-fail (let ((x (amb 1 3 5 8)))
           (require (even? x))
           x)
         'all-odd)

;;; Starting a new problem
;;; Amb-Eval value:
8

|#

;;; Now if we do this, we get the set of all prime sum pairs from the
;;; lists, as expected:

#|

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))

(let ((pairs '()))
  (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
             (set!! pairs (cons p pairs))
             (amb))
           pairs))

;;; Amb-Eval input:
(let ((pairs '()))
  (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
             (set!! pairs (cons p pairs))
             (amb))
           pairs))

;;; Starting a new problem
;;; Amb-Eval value:
((8 35) (3 110) (3 20))

|# 

