;;;;      Diffie-Hellman Key Agreement & ElGamal Cryptosystem 

;;;  Alyssa and Ben, who have never interacted before, have an open
;;;  conversation -- anyone, including Eve, the evesdropper, may hear
;;;  the complete conversation.  At the end of the conversation Alyssa
;;;  and Ben will posess a shared secret that no one else, including
;;;  Eve, can know.  How is this possible?

;;;  See the breakthrough paper by Whitfield Diffie and Martin
;;;  Hellman, in "New directions in cryptography", 1976

;;;  The trick:
;;;    Everyone knows two publicly advertised numbers a, p.
;;;    Alyssa chooses a secret number, Sa, that she remembers.
;;;    Ben chooses a secret number, Sb, that he remembers.
;;;    Alyssa computes Pa = a^Sa (mod p) and announces Pa.
;;;    Ben computes Pb = a^Sb (mod p) and announces Pb.
;;;    Alyssa computes x = Pb^Sa (mod p) = a^(Sb*Sa) (mod p).
;;;    Ben computes y = Pa^Sb (mod p) = a^(Sa*Sb) (mod p).
;;;      but x=y, so Alyssa and Ben share a secret.

;;;  The reason why Eve cannot obtain the same secret x=y is that for
;;;  sufficiently large p, Sa, Sb, given the public information, a, p,
;;;  a^Sa (mod p), and a^Sb (mod p), there is no efficient algorithm
;;;  known to get a^(Sa*Sb) (mod p).  This appears to be as hard as
;;;  the "discrete logarithm problem".

;;;  The base, a, is called the primitive root.  Usually a=2 or a=5.
;;;  The modulus, p, must be a prime.  The choice of prime is usually
;;;  important for making the system hard to crack.  Usually one
;;;  chooses a "safe prime" of the form p=2*q+1 where q is also prime.

;;;  The ElGamal public-key cryptosystem is based on the Diffie-Hellman
;;;  key-agreement protocol.  Each receiver chooses a secret key, S.
;;;  The receiver publishes a public key, which gives a, p, and
;;;  P=a^S(mod p).  The receiver also supplies a procedure that the
;;;  sender can call with the ciphertext.  Given a message, m, the
;;;  sender chooses his own secret, T, and composes a ciphertext that
;;;  has two components, x=a^T(mod p) and y=m*P^T(mod p).  The
;;;  Diffie-Hellman shared secret is x^S(mod p)=P^T(mod p).  So the
;;;  receiver decrypts the message by computing m=(y/x^S)(mod p).


(define (eg-receiver dh-system)
  (let ((k (dh-system-size dh-system))
	(p (dh-system-prime dh-system)))
    (let ((my-secret (random-k-digit-number k))
	  (mod-expt (exptmod p))
	  (mod-* (modular p *))
	  (mod-inv (inversemod p)))
      (let ((advertised-number
	     (mod-expt (dh-system-primitive-root dh-system) my-secret)))
	(let ((public-key
	       (eg-make-public-key dh-system advertised-number))
	      (decryption-procedure
	       (lambda (ciphertext)
		 (let ((x (eg-ciphertext-x ciphertext))
		       (y (eg-ciphertext-y ciphertext)))
		   (let ((m (mod-* y (mod-inv (mod-expt x my-secret)))))
		     (integer->string m))))))
	  (eg-make-receiver public-key decryption-procedure))))))


;;; Data abstractions supporting the ElGamal system

(define (public-dh-system k)
  (list k (random-k-digit-prime k) 2))

(define (dh-system-size system) (car system))

(define (dh-system-prime system) (cadr system))

(define (dh-system-primitive-root system) (caddr system))


(define (eg-make-ciphertext x y) (cons x y))

(define (eg-ciphertext-x c) (car c))

(define (eg-ciphertext-y c) (cdr c))


(define (eg-make-public-key dh-system advertised-number)
  (cons dh-system advertised-number))

(define (eg-public-key-system key) (car key))

(define (eg-public-key-number key) (cdr key))


(define (eg-make-receiver public-key decryption-procedure)
  (cons public-key decryption-procedure))

(define (eg-receiver-public-key receiver) (car receiver))

(define (eg-receiver-decryption-procedure receiver)
  (cdr receiver))

;;; For example.

(define dh-system
  (public-dh-system 100))

(define Alyssa
  (eg-receiver dh-system))


(define (Eve receiver)
  (let ((receiver-public-key
	 (eg-receiver-public-key receiver))
	(receiver-decryption-procedure
	 (eg-receiver-decryption-procedure receiver)))
    (let ((my-spying-procedure
	   (lambda (ciphertext)
	     (write ciphertext)
	     (newline)
	     (receiver-decryption-procedure ciphertext))))
      (eg-make-receiver receiver-public-key
			my-spying-procedure))))

 
;;; The rest of this file is code you may find useful.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Problem 2: Raising a Number to a Power 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (slow-exptmod n)
  (let ((*mod (modular n *)))
    (define (em a b)
      (if (= b 0)
          1
          (*mod a (em a (- b 1)))))
    em))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Problem 4: Prime Numbers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define test-factors
  (lambda (n k)
    (cond ((>= k n) #t)
          ((= (remainder n k) 0) #f)
          (else (test-factors n (+ k 1))))))

(define slow-prime?
  (lambda (n)
    (if (< n 2)
        #f
        (test-factors n 2))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Useful routines for converting message 
;;; strings to and from integers.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (join-numbers list radix)
  ;; Takes a list of numbers (i_0 i_1 i_2 ... i_k)
  ;; and returns the number 
  ;;    n = i_0 + i_1*radix + i_2*radix^2 + ... i_k*radix^k + radix^(k+1)
  ;; The last term creates a leading 1, which allows us to distinguish
  ;; between lists with trailing zeros.
  (if (null? list)
      1
      (+ (car list) (* radix (join-numbers (cdr list) radix)))))

;;; test cases
;;;(join-numbers '(3 20 39 48) 100) ;-> 148392003
;;;(join-numbers '(5 2 3 5 1 9) 10) ;-> 1915325
;;;(join-numbers '() 10) ;-> 1

(define (split-number n radix)
  ;; Inverse of join-numbers.  Takes a number n generated by
  ;; join-numbers and converts it to a list (i_0 i_1 i_2 ... i_k) such
  ;; that
  ;;    n = i_0 + i_1*radix + i_2*radix2 + ... i_k*radix^k + radix^(k+1)
  (if (<= n 1)
      '()
      (cons (remainder n radix)
            (split-number (quotient n radix) radix))))

;;; test cases
;;;(split-number (join-numbers '(3 20 39 48) 100) 100) ;-> (3 20 39 48)
;;;(split-number (join-numbers '(5 2 3 5 1 9) 10) 10)  ;-> (5 2 3 5 1 9)
;;;(split-number (join-numbers '() 10) 10) ; -> ()

;;; For this problem set, we don't need the ability to deal with
;;; leading zeros, so these two definitions will suffice.

(define (split-number n radix)
  (if (< n 1) '()
      (cons (remainder n radix)
	    (split-number (quotient n radix) radix))))

(define (join-numbers digits radix)
  (reduce 
   + 0
   (map 
    (lambda (place digit)
      (* digit (expt radix place)))
    (iota (length digits)) digits)))
;;; Test cases are similar to above except that they don't have
;;; leading ones.

(define chars->bytes
  ;; Takes a list of 16-bit Unicode characters (or 8-bit ASCII
  ;; characters) and returns a list of bytes (numbers in the range
  ;; [0,255]).  Characters whose code value is greater than 255 are
  ;; encoded as a three-byte sequence, 255 <low byte> <high byte>.
  (lambda (chars)
    (if (null? chars)
        '()
        (let ((c (char->integer (car chars))))
          (if (< c 255)
              (cons c (chars->bytes (cdr chars)))
              (let ((lowbyte (remainder c 256))
                    (highbyte  (quotient c 256)))
                (cons 255
		      (cons lowbyte
			    (cons highbyte
				  (chars->bytes (cdr chars)))))))))))

;;; test cases
;;;(chars->bytes (string->list "hello")) ; -> (104 101 108 108 111)
;;;(chars->bytes (string->list "\u0000\u0000\u0000")) ; -> (0 0 0)
;;;(chars->bytes (string->list "\u3293\u5953\uabab"))
;;;      -> (255 147 50 255 83 89 255 171 171)


(define bytes->chars
  ;; Inverse of chars->bytes.  Takes a list of integers that encodes a
  ;; sequence of characters, and returns the corresponding list of
  ;; characters.  Integers less than 255 are converted directly to the
  ;; corresponding ASCII character, and sequences of 255 <low-byte>
  ;; <high-byte> are converted to a 16-bit Unicode character.
  (lambda (bytes)
    (if (null? bytes)
        '()
        (let ((b (car bytes)))
          (if (< b 255)
              (cons (integer->char b)
                    (bytes->chars (cdr bytes)))
              (let ((lowbyte (cadr bytes))
                    (highbyte (caddr bytes)))
                (cons (integer->char (+ lowbyte (* highbyte 256)))
                      (bytes->chars (cdddr bytes)))))))))

;;; test cases
;;;(bytes->chars '(104 101 108 108 111)) ; -> (#\h #\e #\l #\l #\o)
;;;(bytes->chars '(255 147 50 255 83 89 255 171 171))
;;;        -> (#\u3293 #\u5953 #\uabab)

(define (string->integer string)
  ;; returns an integer representation of an arbitrary string. 
  (join-numbers (chars->bytes (string->list string)) 256))

;;; test cases
;;;(string->integer "hello, world")
;;;(string->integer "")
;;;(string->integer "April is the cruelest month")
;;;(string->integer "\u0000\u0000\u0000")


(define (integer->string integer)
  ;; inverse of string->integer.  Returns the string corresponding to
  ;; an integer produced by string->integer.
  (list->string (bytes->chars (split-number integer 256))))

;;; test cases
;;;(integer->string (string->integer "hello, world"))
;;;(integer->string (string->integer ""))
;;;(integer->string (string->integer "April is the cruelest month"))
;;;(integer->string (string->integer "\u0000\u0000\u0000"))
;;;(integer->string (string->integer "\u3293\u5953\uabab"))
;;;(integer->string (string->integer "   lol   "))

(eg-send-message "Hi there." Alyssa)
;Value: "Hi there."

(eg-send-message "12345678901234567890123456789012345678901" Alyssa)
; 41 characters maximum

(define Alyssa (Effective-Eve Alyssa))

(eg-send-message "Don't let Eve see this!" Alyssa)
