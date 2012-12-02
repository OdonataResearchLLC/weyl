;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*- 
;;; ===========================================================================
;;;				 Lisp Numbers 
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; lisp-numbers.lisp,v 1.6 1995/05/24 17:42:03 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.6")

;; Contains a bunch of routines that allow one to use the Weyl generic
;; functions on Lisp numbers.  There are a number of special
;; arithmetic routines here also.  This is in not in LISP-SUPPORT
;; because it is more convenient to put write this code in the WEYLI
;; package. 

;; The Lisp number domain is a special field whose elements are
;; represented as Lisp numbers.  This class is unique.

(defgeneric numerator (number)
  (:documentation
   "Return the numerator of the number.")
  (:method ((number integer)) number)
  (:method ((number ratio)) (common-lisp:numerator number)))

(defgeneric denominator (number)
  (:documentation
   "Return the denominator of the number.")
  (:method ((number integer)) 1)
  (:method ((number ratio)) (common-lisp:denominator number)))

(defgeneric factorial (number)
  (:documentation
   "Return the factorial of the number."))

(defmethod factorial ((n integer))
  (labels ((fact (n)
	     (if (cl:< n 2) 1
		 (cl:* n (fact (cl:1- n))))))
    (if (cl:minusp n)
	(error "Illegal argument to factorial: ~D" n)
	(fact n))))

(defgeneric pochhammer (number1 number2)
  (:documentation
   "Return the falling, or lower, factorial."))

(defmethod pochhammer ((n integer) (k integer))
  (cond ((cl:minusp k)
	 (error "Illegal arguments to Pochhammer: (~D, ~D)"
		n k))
	((cl:zerop k) 1)
	(t (let ((ans n))
	     (loop for i upfrom 1 below k
		   do (setq ans (cl:* ans (cl:+ n i))))
	     ans))))

(defgeneric combinations (number1 number2)
  (:documentation
   "Return a combination of the upper and lower factorial.")
  (:method ((number1 integer) (number2 integer))
    (common-lisp:/ (pochhammer (+  number1 (- number2) 1) number2) (factorial number2))))

(defun faster-isqrt (n)
  "Argument n must be a non-negative integer"
  (let (n-len-quarter n-half n-half-isqrt init-value q r iterated-value)
    (cond
      ((> n 24)
       ;; theoretically (> n 7) ,i.e., n-len-quarter > 0
       (setq n-len-quarter (ash (integer-length n) -2))
       (setq n-half (ash n (- (ash n-len-quarter 1))))
       (setq n-half-isqrt (faster-isqrt n-half))
       (setq init-value (ash (1+ n-half-isqrt) n-len-quarter))
       (multiple-value-setq (q r) (cl:floor n init-value))
       (setq iterated-value (ash (+ init-value q) -1))
       (if (eq (logbitp 0 q) (logbitp 0 init-value)) ; same sign
	   ;; average is exact and we need to test the result
	   (let ((m (- iterated-value init-value)))
	     (if (> (* m m) r)
		 (- iterated-value 1)
		 iterated-value))
	   ;; average was not exact, we take value
	   iterated-value))
      ((> n 15) 4)
      ((> n  8) 3)
      ((> n  3) 2)
      ((> n  0) 1)
      ((> n -1) 0)
      (t nil))))

(defun integer-nth-root (x n)
  (cond ((cl:zerop x) x)
	((cl:plusp x)
	 (let ((n-1 (cl:- n 1))
	       (root (ash 1 (cl:truncate (integer-length x) n)))
	       new-root)
	   (loop for root^n-1 = (cl:expt root n-1)
		 do (setq new-root
			  (cl:round (cl:+ (cl:* n-1 root root^n-1) x)
                                    (* n root^n-1)))
                 (if (cl:= new-root root)
                     (return new-root)
                     (setq root new-root)))))
	((oddp n)
	 (- (integer-nth-root (cl:- x) n)))
	(t nil)))

;;; FIXME : Move the prime number related routines to a separate file
;;; and implement recent algorithms.

(defvar *pointbound* 2147483629 
  "Should be largest prime that fits in a word")

(defvar *big-primes* ()
  "List of large primes by decending size")

;; These two really should be in GFP, but because of LUCID brain damage,
;; they have to be here to avoid warnings.

(defun reduce-modulo-integer (value modulus)
  (unless (cl:zerop modulus)
    (setq value (cl:rem value modulus)))
  (if (cl:< value 0) (cl:+ value modulus)
      value))

(defun expt-modulo-integer (base expt modulus)  
  (%funcall (repeated-squaring
             #'(lambda (a b) (reduce-modulo-integer (cl:* a b) modulus))
             1)
            base expt)) 

(defgeneric prime? (number)
  (:documentation
   "Return true if the number is prime."))

(defmethod prime? ((p integer))
  (and (cl:> p 1)
       (or (cl:< p 14.)
	   (and (cl:= 1 (expt-modulo-integer 13. (1- p) p))
		(cl:= 1 (expt-modulo-integer 3 (1- p) p))))
       (null (cdr (setq p (factor p))))
       (cl:= 1 (cdar p))))

;; Rabin's probabilistic primality algorithm isn't used here because it
;; isn't much faster than the simple one for numbers about the size of a
;; word.
(defun find-smaller-prime (p)
  "Finds biggest prime less than fixnum p"
  (if (evenp p) (setq p (1- p)))
  (loop for pp = (cl:- p 2) then (cl:- pp 2) until (cl:< pp 0)
	when (prime? pp)
        do (return pp)))

;; Return the next prime less than its argument, and that fits into a
;; word.  
(defun newprime (&optional p)
  (if (null p) *pointbound*
      (do ((pl *big-primes* (cdr pl)))
          ((null pl) (setq p (find-smaller-prime p))
           (setq *big-primes* (nconc *big-primes* (list p)))
           p)
        (if (cl:< (car pl) p) (return (car pl))))))

;; Computes a list of primes whose product is greater than the given limit.
(defun choice-primes (limit &optional
                      (prime-list
                       (list (find-smaller-prime
                              most-positive-fixnum))))
  (let ((p (car prime-list)))
    (if (< limit p)
        prime-list
        (choice-primes (ceiling limit p)
                       (cons (newprime p) prime-list)))))

;; Computes (mod a b) symmetric around 0.  a and b are assumed to be
;; lisp integers.
(defun sym-mod (a b)
  (let* ((b (cl:abs b))
	 (c (cl:mod a b)))
    (if (cl:> c (cl:floor (cl:/ b 2)))
        (cl:- c b)
        c)))

(defun repeated-squaring (mult one)
  (lambda (base exp)
    (if (cl:zerop exp) one
        (let ((prod one))
          (loop
           (if (oddp exp)
               (setq prod (%funcall mult prod base)))
           (setq exp (cl:truncate exp 2))
           (if (cl:zerop exp)
               (return prod))
           (setq base (%funcall mult base base)))))))

(defgeneric power-of? (number &optional opt-number)
  (:documentation
   "Return true if number is a power of opt-number."))

(defmethod power-of? ((m integer) &optional n)
  (cond ((typep n 'integer)
	 (loop for test = n then (cl:* test n)
	       for i upfrom 1
	       do (cond ((cl:= test m)
			 (return (values n i)))
			((cl:> test m)
			 (return nil)))))
	(t (error "Haven't implemented the rest of the cases"))))

(defvar *factor-method* 'simple-integer-factor)

(defmacro count-multiple-integer-factors (N divisor)
  `(loop with i = 0
    do (multiple-value-bind (quo rem) (cl:truncate ,N ,divisor)
         (when (not (cl:zerop rem))
           (if (not (cl:zerop i))
               (push (cons ,divisor i) ans))
           (return t))
         (setq ,N quo)
         (incf i))))

(defun uniformize-factor-list (ans)
  (loop for pairs on (sort ans #'(lambda (a b) (< (first a) (first b))))
	when (or (null (rest pairs))
		 (not (cl:= (first (first pairs))
			    (first (second pairs)))))
        collect (first pairs)
	else do (incf (rest (second pairs)))))

(defgeneric factor (number)
  (:documentation
   "Return the factors of the number."))

(defmethod factor ((N integer))
  (let ((*factor-method* *factor-method*)
	ans factors)
    (when (cl:minusp N)
      (push (cons -1 1) ans)
      (setq N (cl:- N)))
    (count-multiple-integer-factors N 2)
    (count-multiple-integer-factors N 3)
    (count-multiple-integer-factors N 5)
    (unless (cl:= N 1)
      (loop
	(multiple-value-setq (N factors) (%funcall *factor-method* N))
	(setq ans (append factors ans))
	(if (cl:= N 1) (return t))))
    (uniformize-factor-list ans)))

(defun all-divisors (n)
  (let ((factors (factor n)))
    (loop with divisors = (list 1)
	  for (prime . times) in factors
	  do (loop for i from 1 to times
		   appending (loop for divisor in divisors
				   collect (* divisor (cl:expt prime i)))
                   into temp
		   finally (setq divisors (append temp divisors)))
          finally (return (sort divisors #'cl:<)))))

;; In general each factorization method should return just one factor.

(defvar *skip-chain-for-3-and-5* (circular-list 4 2 4 2 4 6 2 6))

(defun simple-integer-factor (N)
  (let ((increments *skip-chain-for-3-and-5*)
	(divisor 7)
	ans)
    (flet ((simple-integer-factor-internal (N)
	     (let ((limit (cl:isqrt N)))
	       (loop 
                (cond ((cl:= N 1)
                       (return (values N ans)))
                      ((cl:> divisor limit)
                       (return (values 1 (cons (cons N 1) ans))))
                      (t (count-multiple-integer-factors N divisor)))
                (setq divisor (cl:+ divisor (pop increments)))))))      
      (setq *factor-method* #'simple-integer-factor-internal)
      (simple-integer-factor-internal N))))

(defun fermat-integer-factor (N)
  (loop for x = (1+ (cl:isqrt N)) then (+ x 1)
	for w = (cl:- (cl:* x x) N)
	for y = (cl:isqrt w)
	do (when (cl:zerop (cl:- w (cl:* y y)))
	     (let ((u (cl:+ x y))
		   (v (cl:- x y)))
	       (return (if (1? v)
			   (values 1 (list (cons u 1)))
			   (values u (factor v))))))))
	
#| Knuth's addition-subtraction version of Fermat's algorithm |

(defun fermat-integer-factor (N)
  (let* ((isqrt-N (cl:isqrt N))
	 (x (1+ (* 2 isqrt-N)))
	 (y 1)
	 (r (- (* isqrt-N isqrt-N) N)))
    (loop
      (cond ((= r 0)
	     (return 
	       (let ((f (/ (+ x y -2) 2))
		     (g (/ (- x y) 2)))
		 (if (= g 1)
		     (values 1 (list (cons f 1)))
		     (values 1 (append (factor f) (factor g)))))))
	    ((< r 0)
	     (incf r x)
	     (incf x 2)))
      (decf r y)
      (incf y 2))))

(defun list-of-primes (N)
  (cons 2
	(loop for p upfrom 3 by 2 below N
	      when (prime? p) collect p)))

(defun make-integer-GCD-list (max-prime size-limit)
  (let ((GCD-list ()))
    (loop for p in (list-of-primes max-prime)
	  with prod = 1 and prime-list = ()
	  do (setq prod (* prod p))
	     (cond ((> prod size-limit)
		    (push (list (/ prod p) prime-list)
			  GCD-list)
		    (setq prod p)
		    (setq prime-list (list p)))
		   (t (push p prime-list))))
    GCD-list))

	    
||#

(defun totient (x)
  (do ((factors (factor x) (rest factors))
       (totient 1 (cl:* totient
                        (cl:- (cl:expt (caar factors) (cdar factors))
                              (cl:expt (caar factors) (1- (cdar factors)))))))
      ((null factors)
       totient)))

(defgeneric sin (number)
  (:documentation
   "Return the sine of the number.")
  (:method ((number number)) (common-lisp:sin number)))

(defgeneric cos (number)
  (:documentation
   "Return the cosine of the number.")
  (:method ((number number)) (common-lisp:cos number)))

(defgeneric tan (number)
  (:documentation
   "Return the tangent of the number.")
  (:method ((number number)) (common-lisp:tan number)))

(defgeneric asin (number)
  (:documentation
   "Return the arc-sine of the number.")
  (:method ((number number)) (common-lisp:asin number)))

(defgeneric acos (number)
  (:documentation
   "Return the arc-cosine of the number.")
  (:method ((number number)) (common-lisp:acos number)))

(defgeneric atan (number1 &optional number2)
  (:documentation
   "Return the arc-tangent of the number1 or optionally
number1/number2."))

;;; FIXME : This is a simple example of the motivation for predicate
;;; dispatching as described in AITR-2001-006.
(defmethod atan ((number1 number) &optional number2)
  (cond
    ((null number2)
     (common-lisp:atan number1))
    ((numberp number2)
     (common-lisp:atan number1 number2))
    (t (atan (coerce number1 (domain-of number2)) number2))))

(defgeneric sinh (number)
  (:documentation
   "Return the hyperbolic sine of the number.")
  (:method ((number number)) (common-lisp:sinh number)))

(defgeneric cosh (number)
  (:documentation
   "Return the hyperbolic cosine of the number.")
  (:method ((number number)) (common-lisp:cosh number)))

(defgeneric tanh (number)
  (:documentation
   "Return the hyperbolic tangent of the number.")
  (:method ((number number)) (common-lisp:tanh number)))

(defgeneric asinh (number)
  (:documentation
   "Return the hyperbolic arc sine of the number.")
  (:method ((number number)) (common-lisp:asinh number)))

(defgeneric acosh (number)
  (:documentation
   "Return the hyperbolic arc cosine of the number.")
  (:method ((number number)) (common-lisp:acosh number)))

(defgeneric atanh (number)
  (:documentation
   "Return the hyperbolic arc tangent of the number.")
  (:method ((number number)) (common-lisp:atanh number)))

(defgeneric exp (number)
  (:documentation
   "Return the exponential of the number.")
  (:method ((number number)) (common-lisp:exp number)))

;;; FIXME : The functions log2 and log need to be merged. This
;;; provides another simple example of the motivation for predicate
;;; dispatching as described in AITR-2001-006.
(defgeneric log2 (number base)
  (:documentation
   "Return the base logarithm of the number.")
  (:method ((number number) (base number)) (common-lisp:log number base)))

(defgeneric log (number)
  (:documentation
   "Return the natural logarithm of the number.")
  (:method ((number number)) (common-lisp:log number)))

(defgeneric signum (number)
  (:documentation
   "Returns a numerical value that indicates whether number is
negative, zero or positive.")
  (:method ((number number)) (common-lisp:signum number)))
