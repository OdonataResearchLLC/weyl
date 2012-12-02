;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-

;; bigfloat.lisp,v 1.3 1994/10/21 18:16:34 rz Exp

;; Arbitrary Precision Real Arithmetic System
;;  by Tateaki  Sasaki
;;  The UNIVERSITY OF UTAH,  MARCH 1979

;; For design philosophy and characteristics of this system, see T. Sasaki, 
;; ``An Arbitrary Precision Real Arithmetic Package In Reduce,''
;;  Proceedings of Eurosam '79, Marseille (FRANCE), June 1979.

;; For Implementation notes and using this system, see T. Sasaki, 
;; ``Manual For Arbitrary Precision Real Arithmetic System in Reduce,' 
;; Operating Report of Utah Symbolic Computation Group

;;  In order to speed up this system, you have only to rewrite four
;; routines (DECPREC!, INCPREC!, PRECI!, AND ROUND!LAST)
;; machine-dependently.

;;  This function constructs an internal representation of a number
;; 'n' composed of the mantissa MT and the exponent EP with the base
;; 10.  The magnitude of the number thus constructed is hence
;; MT*10^EP.

;; **** CAUTION!  MT and EP are integers.  So, EP denotes the order of
;;                the last figure in 'N', WHERE order(N)=K if 10**K <=
;;                ABS(N) < 10**(K+1), with the exception order(0)=0. 
;;

;; The number 'n' is said to be of precision 'k' if its mantissa is a
;; k-figure number.  MT and EP are any integers (positive or
;; negative).  So, you can handle any big or small numbers.  in this
;; sense, 'BF' denotes a big-floating-point number.  Hereafter, an
;; internal representation of a number constructed by MAKE-BIGFLOAT is
;; referred to as a big-float representation.
 
(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.3")

;;; FIXME : The precision should be specified in a class allocated
;;; slot.
(eval-when (:compile-toplevel :load-toplevel)
  (proclaim '(special *real-precision*)))

(defsubst make-bigfloat (domain mantissa exponent)
  (make-instance 'bigfloat :domain domain
		 :mantissa mantissa :exponent exponent))

;; This function returns t if x is a big-float representation, else it
;; returns NIL.
(defun bigfloatp (x)
  (eql (class-name (class-of x)) 'bigfloat))

;;; Routines For Converting A Big-Float Number
 
;; This function converts a number N to an equivalent number the
;; precision of which is decreased by K.
(defun decprec! (number k)
  (with-slots (exponent mantissa) number
    (make-bigfloat *domain*
		   (cl:truncate mantissa (cl:expt 10 k))
		   (cl:+ exponent k))))
 
;;  This function converts a number N to an equivalent 
;;       number the precision of which is increased by K.
(defun incprec! (number k)
  (with-slots (exponent mantissa) number
    (make-bigfloat *domain*
		   (cl:* mantissa (cl:expt 10 k))
		   (cl:- exponent k))))
 
;; This function converts a number to an equivalent number of precision
;; k by rounding or adding '0's.
(defun conv!mt (number k)
  (unless (bigfloatp number)
    (error "Expected ~S to be a bigfloat" number))
  (unless (and (integerp k) (cl:> k 0))
    (error "Expected ~S to be a positive integer" k))
  (cond ((cl:zerop (setq k (cl:- (preci! number) k)))
	 number)
	((cl:< k 0) (incprec! number (cl:- k)))
	(t (round!last (decprec! number (1- k))))))
 
;; This function converts a number 'n' to an equivalent number having
;; the exponent k by rounding 'n' or adding '0's to 'n'.
(defun conv!ep (nmbr k)
  (unless (bigfloatp nmbr) 
    (error "Invalid argument to conv!ep: ~S" nmbr))
  (unless (integerp k)
    (error "Invalid second argument to conv!ep: ~S" k))
  (cond ((cl:zerop (setq k (cl:- k (bigfloat-exponent nmbr))))
	 nmbr)
	((cl:< k 0) (incprec! nmbr (cl:- k)))
	(t (round!last (decprec! nmbr (cl:- k 1))))))
 
;; This function returns a given number N unchanged if its precision
;; is not greater than K, else it cuts off its mantissa at the (K+1)th
;; place and returns an equivalent number of precision K.
;;  **** CAUTION!  NO ROUNDING IS MADE.                  
(defun cut!mt (nmbr k)
  (declare (fixnum k))
  (unless (bigfloatp nmbr)
    (error "Invalid argument to cut!mt: ~S" nmbr))
  (unless (and (integerp k) (cl:> k 0))
    (error "Invalid precision to cut!mt: ~D" k))  
  (if (cl:plusp (setq k (cl:- (preci! nmbr) k)))
      (decprec! nmbr k)
      nmbr)) 

;; This function returns a given number N unchanged if its exponent is
;; not less than K, else it cuts off its mantissa and returns an
;; equivalent number of exponent K.
;;  **** CAUTION!  NO ROUNDING IS MADE.                  
(defun cut!ep (nmbr k)
  (unless (bigfloatp nmbr)
    (error "Invalid argument to cut!ep: ~S" nmbr))
  (unless (integerp k)
    (error "Invalid precision to cut!ep: ~D" k))  
  (if (not (cl:> (setq k (cl:- k (bigfloat-exponent nmbr))) 0))
      nmbr
      (decprec! nmbr k)))
 
;; This function counts the precision of a bigfloat 'n'.
;;
;; FIXTHIS:  The 1+ below is total kludge.  This whole package needs
;; to be rewritten sometime soon.  
(defun preci! (nmbr)
  (let* ((mt (1+ (cl:abs (bigfloat-mantissa nmbr))))
	 (len (integer-length mt)))
    (values
     (if (cl:< len 10)
	 (cl:ceiling (cl:log mt 10))
	 (cl:ceiling
	  (cl:+ (cl:/ (cl:+ len -10) #.(cl:log 10 2))
                (cl:log (cl:ash mt (cl:- 10 len)) 10)))))))
 
;; This function counts the order of a bigfloat 'n'.
;; **** ORDER(N)=K IF 10**K <= ABS(N) < 10**(K+1) 
;; ****     WHEN N IS NOT 0, AND ORDER(0)=0.      
(defun order! (nmbr)
  (if (cl:zerop (bigfloat-mantissa nmbr)) 0
      (cl:+ (preci! nmbr) (bigfloat-exponent nmbr) -1)))

(defun convert-number->characters (number)
  (if (cl:zerop number) (list #\0)
      (let ((chars ()))
	(loop with n = number
	      and digit
	      while (not (cl:zerop n))
	      do (multiple-value-setq (n digit) (cl:truncate n 10))
              (push (digit-char digit) chars))
	chars)))

;; This function rounds a number N at its last place.
(defun round!last (nmbr)
  (let ((abs-nmbr (cl:abs (bigfloat-mantissa nmbr)))
	n)
    (setq n (if (cl:< (rem abs-nmbr 10) 5)
		(cl:truncate abs-nmbr 10)
		(1+ (cl:truncate abs-nmbr 10))))
    (if (cl:minusp (bigfloat-mantissa nmbr)) (setq n (cl:- n)))
    (make-bigfloat *domain*  n (cl:1+ (bigfloat-exponent nmbr)))))

;; This function rounds a number N at the (K+1)th place and returns an
;; equivalent number of precision K if the precision of N is greater
;; than K, else it returns the given number unchanged.
(defun round!mt (nmbr k)
  (unless (bigfloatp nmbr)
    (error "Invalid argument to round!mt: ~S" nmbr))
  (unless (and (integerp k) (not (cl:minusp k)))
    (error "Invalid precision to round!mt: ~D" k))  
  (cond ((cl:minusp (setq k (cl:- (preci! nmbr) k 1)))
	 nmbr)
	((equal k 0) (round!last nmbr))
	(t (round!last (decprec! nmbr k)))))

;; This function rounds a number N and returns an equivalent number
;; having the exponent K if the exponent of N is less than K, else it
;; returns the given number unchanged.
(defun round!ep (nmbr k)
  (unless (bigfloatp nmbr)
    (error "Invalid argument to cut!ep: ~S" nmbr))
  (unless (integerp k)
    (error "Invalid precision to cut!ep: ~D" k))  
  (cond ((cl:< (setq k (cl:- (cl:1- k) (bigfloat-exponent nmbr))) 0)
	 nmbr)
	((equal k 0) (round!last nmbr))
	(t (round!last (decprec! nmbr k)))))

(defmethod print-object ((number bigfloat) stream)
  (setq number (round!mt number (cl:- *REAL-PRECISION* 2)))
  (with-slots (mantissa exponent) number 
    (let ((u (convert-number->characters (cl:abs mantissa)))
	  k)
      (flet ((bfprin1 (u)
	       (when (cl:minusp mantissa)
		 (push #\- u))
	       ;; Suppress trailing zeroes
	       (loop for v on (reverse u)
		     while (and (char= (first v) #\0)
				(if (rest v) (char/= (second v) #\.)
				    t))
		     finally (setq u (nreverse v)))
	       ;; Now print the number
	       (loop for char in u
		     do (write-char char stream))))
	(or (rest u) (push #\0 (rest u)))
	(push #\. (rest u))
	(bfprin1 u)
	(princ "B" stream)
	(setq u (convert-number->characters (cl:abs (setq k (order! number)))))
	(setq u (cons (if (cl:< k 0) #\- #\+)
		      u))
	(loop for char in u
	      do (write-char char stream))
	number))))

;;; Routines for reading/printing numbers
 
;; This function reads a long number N represented by a list in a way
;; described below, and constructs a big-float representation of N.
;; Using this function, you can input any long floating-point numbers
;; without difficulty.  L is a list of integers, the first element of
;; which gives the order of N and all the next elements when
;; concatenated give the mantissa of N.
;;    **** ORDER(N)=K IF 10**K <= ABS(N) < 10**(K+1).

(defun read!lnum (l)
  (loop for q in l
	unless (integerp q)
        do (error "Invalid argument to read!lnum: ~S" q))
  (loop for term in (rest l)
	for k = (cl:ceiling (integer-length term) #.(cl:log 10 2))
	with mt = 0
	and ep = (1+ (first l))
	do (setq mt (cl:+ (cl:* mt (cl:expt 10 k)) (cl:abs term)))
        (setq ep (cl:- ep k))
	finally (return 
		  (make-bigfloat *domain*
                                 (if (cl:plusp (second l)) mt (cl:- mt))
                                 ep))))

;; This function reads a long number N represented by a list in a way
;; described below, and constructs a big-float representation of N.
;; Using this function, you can input any long floating-point numbers
;; without difficulty.  L is a list of integers, the first element of
;; which gives the order of N and all the next elements when
;; concatenated give the mantissa of N.
;;  **** ORDER(N)=K IF 10**K <= ABS(N) < 10**(K+1).       

(defun read!num (n)
  (let ((exponent 0))
    (multiple-value-bind (integer j)
	(parse-integer n :junk-allowed t)
      (unless integer
	(setq integer 0))
      (cond ((char= (aref n j) #\.)
	     (multiple-value-bind (fraction i)
		 (parse-integer n :start (1+ j) :junk-allowed t)
	       (setq integer (cl:+ (cl:* integer
                                         (cl:expt 10 (cl:- i j 1)))
                                   fraction))
	       (decf exponent (cl:- i j 1)))))
      (make-bigfloat *domain* integer exponent))))

(defgeneric convert-to-bigfloat (number)
  (:documentation
   "Return the bigfloat representation of the number."))

(defmethod convert-to-bigfloat ((x rational-integer))
  (make-bigfloat (domain-of x) (integer-value x) 0))

(defmethod convert-to-bigfloat ((x rational-number))
  (let ((domain (domain-of x)))
    (/ (make-bigfloat domain (qo-numerator x) 0)
       (make-bigfloat domain (qo-denominator x) 0))))

(defmethod convert-to-bigfloat ((x floating-point-number))
  (let ((domain (domain-of x))
	(float (fp-value x)))
    (multiple-value-bind (mantissa expt sign) (integer-decode-float float)
      (if (cl:minusp expt)
	  (/ (make-bigfloat domain (cl:* mantissa sign) 0)
	     (make-bigfloat domain (cl:expt (float-radix float) (cl:- expt)) 0))
	  (* (make-bigfloat domain (cl:* mantissa sign) 0)
	     (make-bigfloat domain (cl:expt (float-radix float) expt) 0))))))

;;;  Arithmetic manipulation routines

(defun bf-abs (nmbr)
  (if (cl:> (bigfloat-mantissa nmbr) 0) nmbr
      (make-bigfloat *domain*
                     (cl:- (bigfloat-mantissa nmbr))
                     (bigfloat-exponent nmbr))))

(defun bf-minus (nmbr)
  (make-bigfloat *domain*
                 (cl:- (bigfloat-mantissa nmbr))
                 (bigfloat-exponent nmbr)))
 
(defun bf-plus (n1 n2)
  (let ((e1 (bigfloat-exponent n1)) (e2 (bigfloat-exponent n2)))
    (cond ((cl:= e1 e2)
	   (make-bigfloat *domain*
                          (cl:+ (bigfloat-mantissa n1) (bigfloat-mantissa n2))
                          e1))
	  ((cl:> e1 e2)
	   (make-bigfloat *domain*
                          (cl:+ (bigfloat-mantissa (incprec! n1 (cl:- e1 e2)))
                                (bigfloat-mantissa n2))
                          e2))
	  (t (make-bigfloat *domain*
                            (cl:+ (bigfloat-mantissa n1)
                                  (bigfloat-mantissa (incprec! n2 (cl:- e2 e1))))
                            e1))))) 

(defun bf-difference (n1 n2)
  (let ((e1 (bigfloat-exponent n1)) (e2 (bigfloat-exponent n2)))
    (cond ((cl:= e1 e2)
	   (make-bigfloat *domain*
                          (cl:- (bigfloat-mantissa n1) (bigfloat-mantissa n2))
                          e1))
	  ((cl:> e1 e2)
	   (make-bigfloat *domain*
                          (cl:- (bigfloat-mantissa (incprec! n1 (cl:- e1 e2)))
                                (bigfloat-mantissa n2))
                          e2))
	  (t 
	   (make-bigfloat *domain*
                          (cl:- (bigfloat-mantissa n1)
                                (bigfloat-mantissa (incprec! n2 (cl:- e2 e1))))
                          e1)))))

(defun bf-times (n1 n2)
  (make-bigfloat *domain*
                 (cl:* (bigfloat-mantissa n1) (bigfloat-mantissa n2))
                 (cl:+ (bigfloat-exponent n1) (bigfloat-exponent n2)))) 
 
(defun bf-quotient (n1 n2 k)
  (round!mt
   (with-slots (mantissa exponent) (conv!mt n1 (cl:+ k (preci! n2) 1))
     (make-bigfloat *domain*
                    (cl:truncate mantissa (bigfloat-mantissa n2))
                    (cl:- exponent (bigfloat-exponent n2))))
   k)) 
 
;; This function calculates the kth power of 'n'. The result will
;; become a long number if abs(k) >> 1.                             
(defun bf-expt (number k precision)
  (if (cl:< k 0)
      (/ (make-bigfloat *domain* 1 0)
	 (bf-expt number (cl:- k) precision))
      (%funcall (repeated-squaring
                 #'(lambda (a b) (round!mt (bf-times a b) precision))
                 (make-bigfloat *domain* 1 0))
                number k)))

;; This function calculates the integer quotient of 'n1' and 'n2',
;; just as the quotient" for integers does.
(defun bf-floor (n1 n2)
  (let ((e1 (bigfloat-exponent n1))
	(e2 (bigfloat-exponent n2)))
    (cond ((cl:= e1 e2)
	   (make-bigfloat *domain*
                          (cl:truncate (bigfloat-mantissa n1) (bigfloat-mantissa n2))
                          0))
	  ((cl:> e1 e2)
	   (bf-floor (incprec! n1 (cl:- e1 e2)) n2))
	  (t  (bf-floor n1 (incprec! n2 (cl:- e2 e1)))))))

(defun bf-integer-part (num)
  (with-slots (exponent mantissa) num
    (if (cl:zerop exponent) mantissa
	(cl:* mantissa (cl:expt 10 exponent)))))

;; This returns a lisp integer as its first return value  (perhaps
;; this should be a floating point integer)?

(defgeneric floor1 (number)
  (:documentation
   "Return the quotient truncated towards negative infinity."))

(defmethod floor1 ((number bigfloat))
  (let ((domain (domain-of number))
	quo)
    (bind-domain-context domain
      (setq quo (cut!ep number 0))
      (values (bf-integer-part quo)
	      (bf-difference number quo)))))

(defmethod floor2 ((number bigfloat) modulus)
  (let ((domain (domain-of number))
	quo)
    (bind-domain-context domain
      (setq modulus (coerce modulus (domain-of number)))
      (setq quo (bf-floor number modulus))
      (values (bf-integer-part quo)
	      (bf-difference number (bf-times quo modulus))))))

(defgeneric ceiling1 (number)
  (:documentation
   "Return the quotient truncated towards positive infinity."))

(defmethod ceiling1 ((number bigfloat))
  (let ((domain (domain-of number))
	quo)
    (bind-domain-context domain
      (setq quo (cut!ep number 0))
      (unless (eql quo number)
	(setq quo (+ 1 quo)))
      (values (bf-integer-part quo)
	      (bf-difference number quo)))))

(defmethod ceiling2 ((number bigfloat) modulus)
  (let ((domain (domain-of number))
	quo)
    (bind-domain-context domain
      (setq modulus (coerce modulus (domain-of number)))
      (setq quo (bf-floor number modulus))
      (unless (eql quo (cut!ep quo 0))
	(setq quo (+ 1 quo)))
      (values (bf-integer-part quo)
	      (bf-difference number (bf-times quo modulus))))))

(defgeneric round1 (number)
  (:documentation
   "Return the quotient rounded to the nearest integer."))

(defmethod round1 ((number bigfloat))
  (let ((domain (domain-of number))
	quo)
    (bind-domain-context domain
      (setq quo (floor (+ (coerce 1/2 domain) number))) 
      (values quo (bf-difference number (coerce quo domain))))))

(defmethod round2 ((number bigfloat) modulus)
  (let ((domain (domain-of number))
	quo)
    (bind-domain-context domain
      (setq modulus (coerce modulus domain))
      (setq quo (bf-floor (+ number (* (coerce 1/2 domain) modulus))
			  modulus))
      (values quo
	      (bf-difference
	       number
	       (bf-times (coerce quo domain) modulus))))))

(defgeneric truncate1 (number)
  (:documentation
   "Return a quotient that has been truncated towards zero."))

(defmethod truncate1 ((num bigfloat))
  (if (plus? num)
      (floor1 num)
      (ceiling1 num)))

(defmethod truncate2 ((num bigfloat) modulus)
  (if (plus? num)
      (floor2 num modulus)      
      (ceiling2 num modulus)))

;;;  Arithmetic predicates
 
(defun bf-binary= (n1 n2)  
  (with-slots ((e1 exponent)) n1
    (with-slots ((e2 exponent)) n2
      (and (cl:=  e1 e2)
	   (cl:= (bigfloat-mantissa n1) (bigfloat-mantissa n2))))))
 
(defun bf-binary>= (n1 n2)
  (with-slots ((e1 exponent)) n1
    (with-slots ((e2 exponent)) n2
      (cond ((cl:=  e1 e2)
	     (cl:>= (bigfloat-mantissa n1) (bigfloat-mantissa n2)))
	    ((cl:> e1 e2)
	     (cl:> (bigfloat-mantissa (incprec! n1 (cl:- e1 e2)))
                   (bigfloat-mantissa n2)))
	    ((cl:>= (bigfloat-mantissa n1)
                    (bigfloat-mantissa (incprec! n2 (cl:- e2 e1))))
	     t)
	    (t nil)))))

(defun bf-binary> (n1 n2)
  (with-slots ((e1 exponent)) n1
    (with-slots ((e2 exponent)) n2
      (cond ((cl:=  e1 e2)
	     (cl:> (bigfloat-mantissa n1) (bigfloat-mantissa n2)))
	    ((cl:> e1 e2)
	     (cl:> (bigfloat-mantissa (incprec! n1 (cl:- e1 e2)))
                   (bigfloat-mantissa n2)))
	    ((cl:> (bigfloat-mantissa n1)
                   (bigfloat-mantissa (incprec! n2 (cl:- e2 e1))))
	     t)
	    (t nil)))))
 
(defun bf-integerp (x)
  (and (bigfloatp x)
       (not (cl:minusp (bigfloat-exponent x)))))

;; Elementary Constants
 
;; This function returns the value of constant CNST of the precision
;; K, if it was calculated previously with, at least, the precision K,
;; else it returns :NOT-FOUND.
(defun get!const (cnst k)
  (unless (atom cnst)
    (error "Invalid argument to get!const: ~S" cnst))
  (unless (and (integerp k) (> k 0))
    (error "Invalid precision to get!const: ~D" k))  
  (let ((u (get cnst 'save!c)))
    (cond ((or (null u) (cl:< (car u) k)) nil)
	  ((equal (car u) k) (cdr u))
	  (t (round!mt (cdr u) k))))) 
 
;; This function saves the value of constant CNST for the later use.
(defun save!const (cnst nmbr)
  (unless (atom cnst)
    (error "Invalid constant for save!const: ~S" cnst))
  (unless (bigfloatp nmbr)
    (error "Invalid argument to  save!const: ~S" nmbr))
  (setf (get cnst 'save!c) (cons (preci! nmbr) nmbr)))

;; This function sets the value of constant CNST.  CNST is the name of
;; the constant.  L is a list of integers, which represents the value
;; of the constant in the way described in the function READ!LNUM.
(defmacro set!const (constant digits)
  `(progn (save!const ',constant (read!lnum ',digits))
    ',constant))
 
(set!const !pi
           (0 31415926535897932384626433832795028841971693993751058209749))
 
(set!const !e
           (0 27182818284590452353602874713526624977572470936999595749669))

(defmacro define-bfloat-constant (name &body form)
  `(defun ,name (prec)
    (let ((u (get!const ',name prec)))
      (when (eq u :not-found)
        (setq u ,@form)
        (save!const ',name u))
      u)))

;;  This function calculates the value of  `pi' with the precision K by       
;; using Machin's identity:           
;;          PI = 16*ATAN(1/5) - 4*ATAN(1/239).         
;; The calculation is performed mainly on integers.  
(defun bf-pi-machin (k)
  (let* ((k+3 (+ k 3))
	 s
	 (ss (cl:truncate (expt 10 k+3) 5))
	 (n ss)
	 (m 1)
	 (x -25)
	 u)
    (loop while (not (cl:zerop n)) do
      (setq n (cl:truncate n x))
      (setq ss (+ ss (cl:truncate n (setq m (+ m 2))))))
    (setq s (setq n (cl:truncate (expt 10 k+3) 239)))
    (setq x (- (expt 239 2)))
    (setq m 1)
    (loop while (not (cl:zerop n)) do
      (setq n (cl:truncate n x))
      (setq s (+ s (cl:truncate n (setq m (+ m 2))))))
    (setq u (round!mt (make-bigfloat *domain* (- (* 16 ss) (* 4 s)) (- k+3))
		      k))
    (save!const '!pi u)
    u))

;; This function calculates the square root of X with the precision K,
;; by Newton's iteration method.
(defun bf-sqrt (x k)
  (if (0? x) (coerce 0 *domain*)
      (let* ((k2 (+ k 2))
	     (ncut (- k2 (cl:truncate (1+ (order! x)) 2)))
	     (half (coerce 1/2 *domain*))
	     (dcut (make-bigfloat *domain* 10 (- ncut)))
	     (dy (make-bigfloat *domain* 20 (- ncut)))
	     (nfig 1)
	     (y0 (conv!mt x 2))
	     y u)
	(setq y0 (if (cl:zerop (rem (bigfloat-exponent y0) 2))
		     (make-bigfloat *domain*
		       (+ 3 (* 2 (cl:truncate (bigfloat-mantissa y0) 25)))
		       (cl:truncate (bigfloat-exponent y0) 2))
		     (make-bigfloat *domain*
		       (+ 10 (* 2 (cl:truncate (bigfloat-mantissa y0) 9)))
		       (cl:truncate (- (bigfloat-exponent y0) 1) 2))))
	(loop while (or (< nfig k2)
			(> (bf-abs dy) dcut))
	      do (if (> (setq nfig (* 2 nfig)) k2)
		     (setq nfig k2))
		 (setq u (bf-quotient x y0 nfig))
		 (setq y (bf-times (bf-plus y0 u) half))
		 (setq dy (bf-difference y y0))
		 (setq y0 y))
	(round!mt y k)))) 
 
;; This function calculates the value of 'PI', with the precision K, by
;; the arithmetic-geometric mean method.  (R. Brent, JACM vol.23, #2,
;; pp.242-251(1976).)
(defun bf-pi-agm (k)
  (let* ((n 1)
	 (k2 (+ k 2))
	 (u (coerce 1/4 *domain*))
	 (half (coerce 1/2 *domain*))
	 (dcut (make-bigfloat *domain* 10 (- k2)))
	 (x (coerce 1 *domain*))
	 (y (bf-quotient x (bf-sqrt (coerce 2 *domain*) k2) k2))
	 v)
    (loop while (> (bf-abs (bf-difference x y)) dcut) do
          (setq v x)
          (setq x (bf-times (bf-plus x y) half))
          (setq y (bf-sqrt (cut!ep (bf-times y v) (- k2)) k2))
          (setq v (bf-difference x v))
          (setq v (bf-times (bf-times v v) (coerce n *domain*)))
          (setq u (bf-difference u (cut!ep v (- k2))))
          (setq n (* 2 n)))
    (setq v (cut!mt (bf-expt (bf-plus x y) 2 k2) k2))
    (setq u (bf-quotient v (bf-times (coerce 4 *domain*) u) k))
    (save!const '!pi u)
    u))

(defun bf-pi (precision)
  (cond ((cl:< precision 20)
	 (round!mt (make-bigfloat *domain* 314159265358979323846 -20)
		   precision))
	((get!const '!pi precision))
	((cl:< precision 1000) (bf-pi-machin precision))
	(t (bf-pi-agm precision))))

(defgeneric pi-value (domain)
  (:documentation
   "Return the value of PI with the proper precision."))

(defmethod pi-value ((domain real-numbers))
  (bind-domain-context domain
    (bf-pi *REAL-PRECISION*)))

(defun bf-e (precision)
  (cond ((not (> precision 20))
	 (round!mt (make-bigfloat *domain* 271828182845904523536 -20)
		   precision))
	(t (let* ((u (get!const '!e precision))
		  (k2 (+ precision 2))
		  (m 1)
		  (n (expt 10 k2))
		  (ans 0))
	     (when (null u)
	       (loop while (not (cl:zerop n))
		     do (incf ans (setq n (cl:truncate n (incf m)))))
	       (setq ans (+ ans (* 2 (expt 10 k2))))
	       (setq u (round!mt (make-bigfloat *domain* ans (- k2))
				 precision))
	       (save!const '!e u))
	     u))))

(defgeneric e-value (domain)
  (:documentation
   "Return the value of e with the proper precision."))

;; This function calculates the value of 'E', the base of the natural
;; logarithm, with precision K, by summing the Taylor series for
;; EXP(X=1).
(defmethod e-value ((domain real-numbers))
  (bind-domain-context domain
    (bf-e *REAL-PRECISION*)))

;;;  Elementary Functions.

(defun bf-exp (x k)
  (cond ((0? x) (coerce 1 *domain*))
	(t (let* ((k2 (+ k 2))
		  (one (coerce 1 *domain*))
		  (y (bf-abs x))
		  (m (floor y))
		  (q (coerce m *domain*))
		  (r (bf-difference y q))
		  yq yr)
	     (setq yq (if (cl:zerop m) one
			  (bf-expt (bf-e k2) m k2)))
	     (cond ((0? r) (setq yr one))
		   (t (let ((j 0) (n 0)
			    (dcut (make-bigfloat *domain* 10 (- k2)))
			    (ri one) (tm one)
			    fctrial)
			(setq yr one)
			(setq m 1)
			(loop while (> tm dcut) do
                              (setq fctrial
                                    (coerce
                                     (setq m (* m (setq j (1+ j)))) *domain*))
                              (setq ri (cut!ep (bf-times ri r) (- k2)))
                              (setq n (max 1 (+ (- k2 (order! fctrial))
                                                (order! ri))))
                              (setq tm (bf-quotient ri fctrial n))
                              (setq yr (bf-plus yr tm))
                              (cond ((cl:zerop (rem j 10))
                                     (setq yr (cut!ep yr (- k2)))))))))
	     (setq y (cut!mt (bf-times yq yr) (1+ k)))
	     (if (minus? x) (bf-quotient one y k)
		 (round!last y)))))) 

;; This function calculates the value of the exponential function at
;; the point 'x', with the precision k, by summing terms of the Taylor
;; series for exp(z), 0 < z < 1.
(defmethod exp ((number bigfloat))
  (bind-domain-context (domain-of number)
    (bf-exp number *REAL-PRECISION*)))

(defun bf-log (x k)
  (when (not (plus? x))
    (error "Invalid argument to log: ~S" x))
  (unless (and (integerp k) (> k 0))
    (error "Invalid precision to log: ~D" k))
  (cond ((= x 1)
	 (coerce 0 *domain*))
	(t (let* ((m 0)
		  (k2 (+ k 2))
		  (one (coerce 1 *domain*))
		  (ee (bf-e k2))
		  (es (bf-exp (coerce 0.1 *domain*) k2))
		  sign l y z)
	     (cond ((> x one) (setq sign one) (setq y x))
		   (t (setq sign (bf-minus one))
		      (setq y (bf-quotient one x k2))))
	     (cond ((< y ee)
		    (setq z y))
		   (t 
		    (cond ((cl:zerop
			    (setq m (cl:truncate (* (order! y) 23) 10)))
			   (setq z y))
			  (t (setq z (bf-quotient y (bf-expt ee m k2) k2))))
		    (loop while (> z ee) do
                          (setq m (1+ m))
                          (setq z (bf-quotient z ee k2)))))
	     (setq l (coerce m *domain*))
	     (setq y (coerce 0.1 *domain*))
	     (loop while (> z es) do
                   (setq l (bf-plus l y))
                   (setq z (bf-quotient z es k2)))
	     (setq z (bf-difference z one))
	     (prog (n dcut tm zi)
                (setq n 0)
                (setq y (setq tm (setq zi z)))
                (setq z (bf-minus z))
                (setq dcut (make-bigfloat *domain* 10 (- k2)))
                (setq m 1)
                (loop while (> (bf-abs tm) dcut) do
                      (setq zi (cut!ep (bf-times zi z) (- k2)))
                      (setq n (max 1 (+ k2 (order! zi))))
                      (setq tm
                            (bf-quotient zi (coerce (setq m (1+ m)) *domain*)
                                         n))
                      (setq y (bf-plus y tm))
                      (cond ((cl:zerop (rem m 10))
                             (setq y (cut!ep y (- k2)))))))
	     (setq y (bf-plus y l))
	     (round!mt (bf-times sign y) k)))))
 
;; This function calculates log(x), the value of the logarithmic
;; function at the point 'x', with the precision k, by solving x =
;; exp(y) by Newton's method.
;;  x > 0, k is a positive integer                        
#+Ignore
(defun bf-log-newton (x k)
  (when (not (plus? x))
    (error "Invalid argument to log: ~S" x))
  (unless (and (integerp k) (> k 0))
    (error "Invalid precision to log: ~D" k))
  (cond ((bf-equal x (coerce 1 *domain*))
	 (coerce 0 *domain*))
	(t (let* ((k2 (+ k 2))
		  m (one (coerce 1 *domain*))
		  (ee (bf-e (+ k2 2)))
		  sign y z)
	     (cond ((> x one)
		    (setq sign one)
		    (setq y x))
		   (t (setq sign (bf-minus one))
		      (setq y (bf-quotient one x k2))))
	     (if (< y ee)
		 (setq m 0 z y)
		 (cond ((cl:zerop
			 (setq m (cl:truncate (cl:* (order! y) 23) 10)))
			(setq z y))
		       (t (setq z (bf-quotient y (bf-expt ee m k2) k2))
			  (loop while (> z ee) do
                                (setq m (1+ m))
                                (setq z (bf-quotient z ee k2))))))
	     (let ((nfig 0) (n 0)
		   (dcut (make-bigfloat *domain* 10 (- k2)))
		   dx
		   (dy (make-bigfloat *domain* 20 (- k2)))
		   x0)
	       (setq y (bf-quotient (bf-difference z one)
				    (coerce 1.72 *domain*) 2))
	       (setq nfig 1)
	       (loop while (or (< nfig k2) (> (bf-abs dy) dcut)) do
                     (cond
                       ((> (setq nfig (* 2 nfig)) k2)
                        (setq nfig k2)))
                     (setq x0 (exp* y nfig))
                     (setq dx (bf-difference z x0))
                     (setq n (max 1 (+ nfig (order! dx))))
                     (setq dy (bf-quotient dx x0 n))
                     (setq y (bf-plus y dy))))
	     (setq y (bf-plus (coerce m *domain*) y))
	     (round!mt (bf-times sign y) k)))))

(defmethod-sd log2 ((x bigfloat) (base bigfloat))
  (let ((k2 (+ 2 *REAL-PRECISION*)))
    (bind-domain-context domain
      (bf-quotient (bf-log x k2) (bf-log base k2) (- k2 2)))))

;; This function calculates log(x) by summing terms of the     
;; Taylor series for LOG(1+Z), 0 < Z < 0.10518. 
(defmethod log ((x bigfloat))
  (bind-domain-context (domain-of x)
    (bf-log x *REAL-PRECISION*)))

(defun bf-cos (x k)
  (unless (and (integerp k) (> k 0))
    (error "Invalid precision to cos: ~D" k))
  (cond ((0? x) (coerce 1 *domain*))
	(t (when (minus? x)
	     (setq x  (- x)))
	   (let* ((k2 (+ k 2))
		  (m (preci! x))
		  (pi4 (/ (bf-pi (+ k2 m)) 4))
		  sign q r y)
	     (cond ((< x pi4)
		    (setq m 0)
		    (setq r x))
		   (t (setq m (floor (setq q (floor x pi4))))
		      (setq r (- x (* q pi4)))))
	     (setq sign (coerce 1 *domain*))
	     (cond ((not (< m 8)) (setq m (rem m 8))))
	     (cond ((not (< m 4))
		    (setq sign (- sign))
		    (setq m (- m 4))))
	     (cond ((not (< m 2)) (setq sign (- sign))))
	     (cond ((equal m 1)
		    (setq r (cut!mt (- pi4 r) k2))
		    (bf-times sign (bf-sin r k)))
		   ((equal m 2)
		    (setq r (cut!mt r k2))
		    (bf-times sign (bf-sin r k)))
		   (t (when (= m 3)
			(setq r (cut!mt (- pi4 r) k2)))
		      (let ((j 0) (n 0)
			    (dcut (make-bigfloat *domain* 10 (- k2)))
			    fctrial ri tm)
			(setq y (setq ri (setq tm (coerce 1 *domain*))))
			(setq r (- (cut!ep (* r r) (- k2))))
			(setq m 1)
			(loop while (> (bf-abs tm) dcut) do
                              (setq j (+ j 2))
                              (setq fctrial
                                    (coerce
                                     (setq m (* m j (- j 1))) *domain*))
                              (setq ri (cut!ep (* ri r) (- k2)))
                              (setq n (max 1 (+ (- k2 (order! fctrial))
                                                (order! ri))))
                              (setq tm (bf-quotient ri fctrial n))
                              (setq y (+ y tm))
                              (cond ((equal (rem j 20) 0)
                                     (setq y (cut!ep y (- k2)))))))
		      (round!mt (* sign y) k)))))))

(defun bf-sin (x k)
  (cond ((0? x) (coerce 0 *domain*))
	((minus? x) (bf-minus (bf-sin (bf-minus x) k)))
	(t (let* ((k2 (+ k 2))
		  (m (preci! x))
		  (pi4 (bf-times (bf-pi (+ k2 m)) (coerce 1/4 *domain*)))
		  sign q r y)
	     (cond ((< x pi4)
		    (setq m 0)
		    (setq r x))
		   (t (setq m (floor (setq q (bf-floor x pi4))))
		      (setq r (bf-difference x (bf-times q pi4)))))
	     (setq sign (coerce 1 *domain*))
	     (cond ((not (< m 8)) (setq m (rem m 8))))
	     (cond ((not (< m 4))
		    (setq sign (bf-minus sign))
		    (setq m (- m 4))))
	     (cond ((equal m 1)
		    (setq r (cut!mt (bf-difference pi4 r) k2))
		    (bf-times sign (bf-cos r k)))
		   ((equal m 2)
		    (setq r (cut!mt r k2))
		    (bf-times sign (bf-cos r k)))
		   (t (unless (equal m 0)
			(setq r (cut!mt (bf-difference pi4 r) k2)))
		      (let* ((ncut (- k2 (min 0 (1+ (order! r)))))
			     (dcut (make-bigfloat *domain* 10 (- ncut)))
			     (tm r) (ri r) (j 1) n fctrial)
			(setq y r)
			(setq r (bf-minus (cut!ep (bf-times r r) (- ncut))))
			(setq m 1)
			(loop while (> (bf-abs tm) dcut) do
                              (setq j (+ j 2))
                              (setq fctrial
                                    (coerce
                                     (setq m (* m j (1- j))) *domain*))
                              (setq ri (cut!ep (bf-times ri r) (- ncut)))
                              (setq n (max 1 (+ (- k2 (order! fctrial)) (order! ri))))
                              (setq tm (bf-quotient ri fctrial n))
                              (setq y (bf-plus y tm))
                              (cond ((cl:zerop (rem j 20))
                                     (setq y (cut!ep y (- ncut)))))))
		      (round!mt (bf-times sign y) k)))))))

;; This function calculates sin(x), the value of the sine function at
;; the point 'x', with the precision k, by summing terms of the Taylor
;; series for:   sin(z), 0 < Z < PI/4.    
(defmethod sin ((number bigfloat))
  (bind-domain-context (domain-of number)
    (bf-sin number *REAL-PRECISION*)))

;; This function calculates cos(x), the value of the cosine function at
;; the point 'x', with the precision k, by summing terms of the Taylor
;; series for:   cos(z), 0 < Z < PI/4.    
(defmethod cos ((number bigfloat))
  (bind-domain-context (domain-of number)
    (bf-cos number *REAL-PRECISION*)))

(defun bf-tan (x k)
  (unless (and (integerp k) (> k 0))
    (error "Invalid precision to tan: ~D" k))
  (cond ((0? x) (coerce 0 *domain*))
	((minus? x) (bf-minus (bf-tan (bf-minus x) k)))
	(t (let* ((k2 (+ k 2))
		  (one (coerce 1 *domain*))
		  (m (preci! x))
		  (pi4 (bf-times (bf-pi (+ k2 m)) (coerce 1/4 *domain*)))
		  sign q r)
	     (cond ((< x pi4)
		    (setq m 0)
		    (setq r x))
		   (t (setq m (floor (setq q (bf-floor x pi4))))
		      (setq r (bf-difference x (bf-times q pi4)))))
	     (cond ((not (< m 4)) (setq m (rem m 4))))
	     (setq sign (if (< m 2) one (bf-minus one)))
	     (cond ((or (= m 1) (= m 3))
		    (setq r (bf-difference pi4 r))))
	     (setq r (cut!mt r k2))
	     (cond ((or (equal m 0) (equal m 3))
		    (setq r (bf-sin r k2))
		    (setq q (bf-difference one (bf-times r r)))
		    (setq q (bf-sqrt (cut!mt q k2) k2))
		    (bf-times sign (bf-quotient r q k)))
		   (t (setq r (bf-sin r k2))
		      (setq q (bf-difference one (bf-times r r)))
		      (setq q (bf-sqrt (cut!mt q k2) k2))
		      (bf-times sign (bf-quotient q r k))))))))

;; This function calculates tan(x), the value of the tangent function
;; at the point 'x', with the precision k, by calculating       
;;          sin(x)  or  cos(x) = sin(pi/2-x).       
(defmethod tan ((number bigfloat))
  (bind-domain-context (domain-of number)
    (bf-tan number *REAL-PRECISION*)))

(defun bf-atan (x k)
  (unless (and (integerp k) (> k 0))
    (error "Invalid precision to atan: ~D" k))
  (cond ((0? x) (coerce 0 *domain*))
	((minus? x) (bf-minus (bf-atan (bf-minus x) k)))
	(t (let* ((k2 (+ k 2))
		  (one (coerce 1 *domain*))
		  (pi4 (bf-times (bf-pi k2) (coerce 1/4 *domain*)))
		  y z)
	     (cond ((= x 1)
		    (round!mt pi4 k))
		   ((> x one)
		    (round!mt
		     (bf-difference (bf-plus pi4 pi4)
				    (bf-atan (bf-quotient one x k2) (+ k 1)))
		     k))
		   ((< x (coerce 0.42 *domain*))
		    (let* ((m 1) (n 0)
			   (ncut (- k2 (min 0 (+ (order! x) 1))))
			   (dcut (make-bigfloat *domain* 10 (- ncut)))
			   (zi x)
			   (tm x))
		      (setq y tm)
		      (setq z (bf-minus (cut!ep (bf-times x x) (- ncut))))
		      (loop while (> (bf-abs tm) dcut) do
                            (setq zi (cut!ep (bf-times zi z) (- ncut)))
                            (setq n (max 1 (+ k2 (order! zi))))
                            (setq tm (bf-quotient
                                      zi (coerce (setq m (+ m 2)) *domain*)
                                      n))
                            (setq y (bf-plus y tm))
                            (cond ((cl:zerop (rem m 20))
                                   (setq y (cut!ep y (- ncut)))))))
		    (round!mt y k))
		   (t (setq y (bf-plus one (cut!mt (bf-times x x) k2)))
		      (setq y (bf-plus one (bf-sqrt y k2)))
		      (setq y (bf-atan (bf-quotient x y k2) (+ k 1)))
		      (round!mt (bf-times y (coerce 2 *domain*)) k)))))))

;; this function calculates atan(x), the value of the arctangent
;; function at the point 'x', with the precision k, by summing terms
;; of the Taylor series for atan(z)  if  0 < z < 0.42.  
;;   otherwise the following identities are used!  
;;       atan(x) = pi/2 - atan(1/x)  if  1 < x  and 
;;       atan(x) = 2*atan(x/(1+sqrt(1+x**2)))       
;;             if  0.42 <= x <= 1.                     
;; the answer is in the range [-pi/2, pi/2).    
(defmethod atan ((number bigfloat) &optional base)
  (when base
    (error "Two argument atan not implemented yet"))
  (bind-domain-context (domain-of number)
    (bf-atan number *REAL-PRECISION*)))

(defun bf-asin (x k)
  (when (or (> (bf-abs x) (coerce 1 *domain*)))
    (error "Invalid argument to asin: ~S" x))
  (unless (and (integerp k) (> k 0))
    (error "Invalid precision to asin: ~D" k))
  (cond ((minus? x) (bf-minus (bf-asin (bf-minus x) k)))
	(t (let ((k2 (+ k 2))
		 (one (coerce 1 *domain*)))
	     (cond ((< (bf-difference one x)
		       (make-bigfloat *domain* 10 (- k2)))
		    (round!mt (bf-times (bf-pi (1+ k)) (coerce 1/2 *domain*))
			      k))
		   (t (bf-atan
		       (bf-quotient x (bf-sqrt (cut!mt (- 1 (* x x)) k2) k2)
				    k2)
		       k))))))) 

;; This function calculates asin(x), the value of the arcsine function
;; at the point 'x', with the precision k, by calculating        
;;          atan(x/sqrt(1-x**2))  
;; The answer is in the range <-pi/2 , pi/2>.  
(defmethod asin ((number bigfloat))
  (bind-domain-context (domain-of number)
    (bf-asin number *REAL-PRECISION*)))

(defun bf-acos (x k)
  (when (or (> (bf-abs x) (coerce 1 *domain*)))
    (error "Invalid argument to acos: ~S" x))
  (unless (and (integerp k) (> k 0))
    (error "Invalid precision to acos: ~D" k))
  (let ((k2 (+ k 2))
	y)
    (cond ((< (bf-abs x) (make-bigfloat *domain* 50 (- k2)))
	   (round!mt (bf-times (bf-pi (+ k 1)) (coerce 1/2 *domain*))
		     k))
	  (t (setq y (bf-quotient
		      (bf-sqrt (cut!mt
				(bf-difference (coerce 1 *domain*)
					       (bf-times x x))
				k2) k2)
		      (bf-abs x)
		      k2))
	     (if (minus? x)
		 (round!mt (bf-difference (bf-pi (+ k 1)) (bf-atan y k))
			   k)
		 (bf-atan y k))))))

;; This function calculates acos(x), the value of the arccosine
;; function at the point 'x', with the precision k, by calculating        
;;          atan(sqrt(1-x**2)/x)  if  x > 0  or      
;;          atan(sqrt(1-x**2)/x) + pi  if  x < 0.    
;; the answer is in the range [0 , pi).        
(defmethod acos ((number bigfloat))
  (bind-domain-context (domain-of number)
    (bf-acos number *REAL-PRECISION*)))

;; this function calculates arcsin(x), the value of the arcsine
;; function at the point 'x', with the precision k, by solving
;;    x = sin(y)  if  0 < x <= 0.72,  or       
;;    sqrt(1-x**2) = sin(y)  if  0.72 < x,     
;; by Newton's iteration method.               
;; the answer is in the range [-pi/2, pi/2).
#+Ignore
(defun bf-asin-newton (x k)
  (when (or (> (bf-abs x) (coerce 1 *domain*)))
    (error "Invalid argument to asin: ~S" x))
  (unless (and (integerp k) (> k 0))
    (error "Invalid precision to asin: ~D" k))
  (cond ((0? x) (coerce 0 *domain*))
	((minus? x) (bf-minus (bf-asin-newton (bf-minus x) k)))
	(t (let* ((k2 (+ k 2))
		  (dcut (make-bigfloat *domain* 10 (+ (- k2) (order! x) 1)))
		  (one (coerce 1 *domain*))
		  (pi2 (bf-times (bf-pi (+ k2 2)) (coerce 1/2 *domain*)))
		  y)	       
	     (cond ((< (- 1 x) dcut)
		    (round!mt pi2 k))
		   ((> x (coerce 0.72 *domain*))
		    (setq y (cut!mt (bf-difference one (bf-times x x)) k2))
		    (setq y (bf-asin-newton (bf-sqrt y k2) k))
		    (round!mt (bf-difference pi2 y) k))
		   (t (let ((nfig 1)
			    (n 0)
			    (dy one)
			    cx dx x0)
			(setq y x)
			(loop while (or (< nfig k2)
					(> (bf-abs dy) dcut))
			      do (cond ((> (setq nfig (* 2 nfig)) k2)
					(setq nfig k2)))
				 (setq x0 (bf-sin y nfig))
				 (setq cx (bf-sqrt (cut!mt (- 1 (* x0 x0))
							   nfig)
					     nfig))
				 (setq dx (- x x0))
				 (setq n (max 1 (+ nfig (order! dx))))
				 (setq dy (bf-quotient dx cx n))
				 (setq y (bf-plus y dy)))
			(round!mt y k))))))))
 
;; This function calculates arccos(x), the value of the arccosine
;; function at the point 'x', with the precision k, by calculating
;;    arcsin(sqrt(1-x**2))  if  x > 0.72  and    
;;    pi/2 - arcsin(x)  otherwise.
;; The answer is in the range [0, pi).          
#+ignore
(defun bf-acos-newton (x k)
  (when (or (> (bf-abs x) (coerce 1 *domain*)))
    (error "Invalid argument to acos: ~S" x))
  (unless (and (integerp k) (> k 0))
    (error "Invalid precision to acos: ~D" k))
  (cond ((bf-<= x (coerce 0.72 *domain*))
         (round!mt
	  (bf-difference
	   (bf-times (bf-pi (+ k 1)) (coerce 1/2 *domain*))
	   (bf-asin-newton x k))
	  k))
	(t (bf-asin-newton
	    (bf-sqrt
	     (cut!mt (bf-difference (coerce 1 *domain*) (* x x))
		     (+ k 2))
	     (+ k 2))
	    k))))
 
;; This function calculates arctan(x), the value of the arctangent
;; function at the point 'x', with the precision k, by calculating
;;     arcsin(x/sqrt(1+x**2))
;; The answer is in the range [-pi/2, pi/2).  
#+Ignore
(defun bf-atan-newton (x k)
  (unless (and (integerp k) (> k 0))
    (error "Invalid precision to atan: ~D" k))
  (cond ((minus? x) (bf-minus (bf-atan-newton (bf-minus x) k)))
	(t (bf-asin-newton
	    (bf-quotient x (bf-sqrt (cut!mt (+ 1 (* x x)) (+ k 2))
				    (+ k 2))
			 (+ k 2))
	    k))))

(defmethod-sd expt ((x bigfloat) (y bigfloat))
  (bind-domain-context domain
    (cond ((bf-integerp y) (expt x (floor y)))
	  ((minus? y)
	   (/ 1 (expt x (bf-minus y))))
	  (t (let ((n *REAL-PRECISION*)
		   (xp (cl:abs x))
		   yp) 
	       (cond ((bf-integerp (bf-times y (coerce 2 domain)))
		      (setq xp (incprec! xp 1)) 
		      (setq yp (round!mt
				(bf-times (expt xp (floor y))
					  (bf-sqrt xp (+ n 1)))
				n)))
		     (t (setq yp (bf-exp (* y (bf-log xp (1+ n))) n))))
	       (cond ((minus? x) (bf-minus yp)) (t yp)))))))
