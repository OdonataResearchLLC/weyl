;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			         Numbers
;;; ===========================================================================
;;; (c) Copyright 1991,1993 Cornell University

;;; numbers.lisp,v 1.14 1995/05/24 17:42:07 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.14")

;; The Rational Integers  (Z)

(define-domain-element-classes rational-integers
    rational-integer)

(defun rational-integers-print-object (d stream)
  (declare (ignore d))
  #+Genera
  (format stream "~'bZ~")
  #-Genera
  (princ "Z"  stream))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator rational-integers ()
    (let ((d (make-instance 'rational-integers
                            :print-function 'rational-integers-print-object)))
      (assert-ordered-domain d)
      (assert-gcd-domain d)
      (assert-unique-factorization-domain d)
      (with-slots (zero one) d
        (setq zero (make-element d 0))
        (setq one (make-element d 1)))
      d)
    :predicate #'(lambda (d) (eql (class-name (class-of d)) 'rational-integers))))

(defmethod print-object ((n rational-integer) stream)
  (princ (integer-value n) stream))

(defmethod make-element ((domain rational-integers) (x integer) &rest ignore)
  (declare (ignore ignore))
  (make-instance 'rational-integer :domain domain :value x))

(defmethod weyl:make-element ((domain rational-integers) (x integer)
			      &rest args)
  (when (not (null args))
    (error "Too many arguments to MAKE-ELEMENT of ~S: ~S"
	   domain (cons x args)))
  (make-instance 'rational-integer :domain domain :value x))


(defmethod coerce ((x integer) (domain rational-integers))
  (make-element domain x))

;; The Rational numbers  (Q)

(define-domain-element-classes rational-numbers
  rational-number rational-integer)

(defun rational-numbers-print-object (d stream)
  (declare (ignore d))
  #+Genera
  (format stream "~'bQ~")
  #-Genera
  (princ "Q"  stream))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator rational-numbers ()
    (let ((domain (make-instance 'rational-numbers
                                 :print-function 'rational-numbers-print-object))
          (Z (get-rational-integers)))
      (make-homomorphism Z
                         #'(lambda (x)
                             (make-element domain (integer-value x)))
                         domain)
      domain)
    :predicate #'(lambda (d)
                   (eql (class-name (class-of d)) 'rational-numbers))))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator quotient-field ((ring rational-integers))
    (make-rational-numbers)))

(defmethod get-quotient-field ((ring rational-integers))
  (get-rational-numbers))

(defmethod print-object ((ratfun rational-number) stream)
  (with-numerator-and-denominator (numerator denominator) ratfun
    (cond ((1? denominator)
	   (prin1 numerator stream))
	  (t (prin1 numerator stream)
	     (princ "/" stream)
	     (prin1 denominator stream)))))

(defmethod make-element ((qf rational-numbers) (num integer) &rest ignore)
  (declare (ignore ignore))
  (make-instance 'rational-integer :domain qf :value num))

(defmethod weyl:make-element ((qf rational-numbers) (num integer) &rest args)
  (when (not (null args))
    (error "Too many arguments to MAKE-ELEMENT of ~S: ~S"
	   qf (cons num args)))
  (make-instance 'rational-integer :domain qf :value num))

(defmethod coerce ((elt integer) (domain rational-numbers))
  (make-element domain elt))

(defmethod make-element ((qf rational-numbers) (num ratio) &rest ignore)
  (declare (ignore ignore))
  (make-instance 'rational-number :domain qf
		 :numerator (cl:numerator num)
		 :denominator (cl:denominator num)))

(defmethod weyl:make-element ((qf rational-numbers) (num ratio) &rest args)
  (when (not (null args))
    (error "Too many arguments to MAKE-ELEMENT of ~S: ~S"
	   qf (cons num args)))
  (make-instance 'rational-number :domain qf
		 :numerator (cl:numerator num)
		 :denominator (cl:denominator num)))

(defmethod coerce ((elt ratio) (domain rational-numbers))
  (make-element domain elt))

(defmethod make-quotient-element
    ((qf rational-numbers) (numerator integer) (denominator integer))
  (cond ((cl:= denominator 1)
	 (make-instance 'rational-integer :domain qf :value numerator))
	(t (make-instance 'rational-number :domain qf
			  :numerator numerator
			  :denominator denominator))))

;; The Real numbers (R)

(define-domain-element-classes real-numbers
  floating-point-number bigfloat rational-integer rational-number)

(defun real-numbers-print-object (d stream)
  (declare (ignore d))
  #+Genera
  (format stream "~'bR~")
  #-Genera
  (princ "R"  stream))

;; For these two there is only one domain.  (We could also implement
;; several integer domains if we wanted later.)

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator real-numbers ()
    (let ((domain (make-instance 'real-numbers
                                 :print-function 'real-numbers-print-object))
          (Q (get-rational-numbers)))
      (make-homomorphism Q
                         #'(lambda (x)
                             (make-element domain
                                           (if (typep x 'rational-integer)
                                               (integer-value x)
                                               (cl:/ (qo-numerator x)
                                                     (qo-denominator x)))))
                         domain)
      domain)
    :predicate #'(lambda (d) (eql (class-name (class-of d)) 'real-numbers))))

(defvar *floating-point-precision* 16.
  "Precision below which to use the machine floating point representation")

(defvar *real-precision* *floating-point-precision*
  "The default precision when creating a Real number")

(defmethod print-object ((z floating-point-number) stream)
  (format stream "~D" (fp-value z)))

(defmethod make-element ((domain real-numbers) (x integer) &rest ignore)
  (declare (ignore ignore))
  (make-instance 'rational-integer :domain domain :value x))

(defmethod weyl:make-element ((domain real-numbers) (x integer) &rest args)
  (when (not (null args))
    (error "Too many arguments to MAKE-ELEMENT of ~S: ~S"
	   domain (cons x args)))
  (make-instance 'rational-integer :domain domain :value x))

(defmethod coerce ((elt integer) (domain real-numbers))
  (make-element domain elt))

(defmethod make-element ((domain real-numbers) (x ratio) &rest ignore)
  (declare (ignore ignore))
  (make-instance 'rational-number :domain domain
		 :numerator (cl:numerator x)
		 :denominator (cl:denominator x)))

(defmethod weyl:make-element ((domain real-numbers) (x ratio) &rest args)
  (when (not (null args))
    (error "Too many arguments to MAKE-ELEMENT of ~S: ~S"
	   domain (cons x args)))
  (make-instance 'rational-number :domain domain
		 :numerator (cl:numerator x)
		 :denominator (cl:denominator x)))

(defmethod coerce ((elt ratio) (domain real-numbers))
  (make-element domain elt))

;; The following method is needed so that routines that return ratios
;; will work.
(defmethod make-quotient-element
    ((domain real-numbers) (numerator integer) (denominator integer))
  (cond ((cl:= denominator 1)
	 (make-instance 'rational-integer :domain domain :value numerator))
	(t (make-instance 'rational-number :domain domain
			  :numerator numerator
			  :denominator denominator))))

(defmethod make-element ((domain real-numbers) (x float) &rest ignore)
  (declare (ignore ignore))
  (make-instance 'floating-point-number :domain domain
		 :value x))

(defmethod weyl:make-element ((domain real-numbers) (x float) &rest args)
  (when (not (null args))
    (error "Too many arguments to MAKE-ELEMENT of ~S: ~S"
	   domain (cons x args)))
  (make-instance 'floating-point-number :domain domain
		 :value x))

(defmethod coerce ((elt float) (domain real-numbers))
  (make-element domain elt))

(defmethod zero ((domain real-numbers))
  (weyl:make-element domain 0))

(defmethod one ((domain real-numbers))
  (weyl:make-element domain 1))

;; The Complex numbers (C)

(define-domain-element-classes complex-numbers
  complex-number)

(defun complex-numbers-print-object (d stream)
  (declare (ignore d))
  #+Genera
  (format stream "~'bC~")
  #-Genera
  (princ "C"  stream))

;; For these two there is only one domain.  (We could also implement
;; several integer domains if we wanted later.)

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator complex-numbers ()
    (let ((domain (make-instance 'complex-numbers
                                 :print-function 'complex-numbers-print-object))
          (R (get-real-numbers)))
      (make-homomorphism R
                         #'(lambda (x)
                             (make-element domain
                                           (cond ((typep x 'rational-integer)
                                                  (integer-value x))
                                                 ((typep x 'rational-number)
                                                  (cl:/ (qo-numerator x)
                                                        (qo-denominator x)))
                                                 ((typep x 'floating-point-number)
                                                  (fp-value x))
                                                 (t x))))
                         domain)
      domain)
    :predicate #'(lambda (d) (eql (class-name (class-of d)) 'complex-numbers))))

(defmethod print-object ((z complex-number) stream)
  (with-slots ((x real) (y imag)) z
    (cond ((0? y)
	   (princ x stream))
	  ((0? x)
	   (if (1? y)
	       (format stream "i")
	       (format stream "~S i" y)))
	  (t (if (1? y)
		 (format stream "~S + i" x)
		 (format stream "~S + ~S i" x y))))))

(defmethod make-element ((domain complex-numbers) (x integer) &rest args)
  (if (or (null args) (0? (first args)))
      (make-instance 'rational-integer :domain domain :value x)
      (make-instance 'complex-number :domain domain
		     :realpart x
		     :imagpart (first args))))

(defmethod weyl:make-element ((domain complex-numbers) (x integer) &rest args)
  (cond ((or (null args)
	     (and (0? (first args)) (null (rest args))))
	 (make-instance 'rational-integer :domain domain :value x))
	((null (rest args))
	 (if (and (cl:numberp (first args))
		  (not (cl:complexp (first args))))
	     (make-instance 'complex-number :domain domain
			    :realpart x
			    :imagpart (first args))
	     (error "Wrong type of arguments to MAKE-ELEMENT of ~S: ~S"
		    domain (cons x args))))
	(t (error "Too many arguments to MAKE-ELEMENT of ~S: ~S"
		  domain (cons x args)))))

(defmethod make-element ((domain complex-numbers) (x ratio) &rest args)
  (if (or (null args) (0? (first args)))
      (make-instance 'rational-number :domain domain
		     :numerator (cl:numerator x)
		     :denominator (cl:denominator x))
      (make-instance 'complex-number :domain domain
		     :realpart x
		     :imagpart (first args))))

(defmethod weyl:make-element ((domain complex-numbers) (x ratio) &rest args)
  (cond ((or (null args)
	     (and (0? (first args))
		  (null (rest args))))
	 (make-instance 'rational-number :domain domain
			:numerator (cl:numerator x)
			:denominator (cl:denominator x)))
	((null (rest args))
	 (if (and (cl:numberp (first args))
		  (not (cl:complexp (first args))))
	     (make-instance 'complex-number :domain domain
			    :realpart x
			    :imagpart (first args))
	     (error "Wrong type of arguments to MAKE-ELEMENT of ~S: ~S"
		    domain (cons x args))))
	(t (error "Too many arguments to MAKE-ELEMENT of ~S: ~S"
		  domain (cons x args)))))

(defmethod make-element ((domain complex-numbers) (x float) &rest args)
  (if (or (null args) (0? (first args)))
      (make-instance 'floating-point-number :domain domain
		     :value x)
      (make-instance 'complex-number :domain domain
		     :realpart x
		     :imagpart (first args))))

(defmethod weyl:make-element ((domain complex-numbers) (x float) &rest args)
  (cond ((or (null args)
	     (and (0? (first args))
		  (null (rest args))))
	 (make-instance 'floating-point-number :domain domain
			:value x))
	((null (rest args))	 
	 (if (and (cl:numberp (first args))
		  (not (cl:complexp (first args))))
	     (make-instance 'complex-number :domain domain
			    :realpart x
			    :imagpart (first args))
	     (error "Wrong type of arguments to MAKE-ELEMENT of ~S: ~S"
		    domain (cons x args))))
	(t (error "Too many arguments to MAKE-ELEMENT of ~S: ~S"
		  domain (cons x args)))))

(defmethod make-element
    ((domain complex-numbers) (x cl:complex) &rest ignore)
  (declare (ignore ignore))
  (let ((real (cl:realpart x))
	(imag (cl:imagpart x)))
    (if (0? imag)
	(make-element domain real)
	(make-instance 'complex-number :domain domain
		       :realpart real :imagpart imag))))

(defmethod weyl:make-element
    ((domain complex-numbers) (x cl:complex) &rest args)
  (cond ((null args)
	 (make-element domain x))
	(t (error "Too many arguments to MAKE-ELEMENT of ~S: ~S"
		  domain (cons x args)))))

(defmethod realpart ((x number))
  (cl:realpart x))

(defmethod imagpart ((x number))
  (cl:imagpart x))

(defmacro def-realimag-part ((x type) real-body imag-body)
  `(progn
    (defmethod realpart ((,x ,type))
      (let ((domain (domain-of ,x)))
        (if (or (typep domain 'complex-numbers)
                (typep domain 'non-strict-domain))
            ,real-body
            (error "Don't know what \"realpart\" means in ~S"
                   domain))))
    (defmethod imagpart ((,x ,type))
      (let ((domain (domain-of ,x)))
        (if (or (typep domain 'complex-numbers)
                (typep domain 'non-strict-domain))
            ,imag-body
            (error "Don't know what \"imagpart\" means in ~S"
                   domain))))))

(def-realimag-part (x rational-integer)
    (make-element domain (integer-value x))
  (zero domain))

(def-realimag-part (x rational-number)
    (make-element domain (cl:/ (qo-numerator x) (qo-denominator x)))
  (zero domain))

(def-realimag-part (x floating-point-number)
    (make-element domain (fp-value x))
  (zero domain))

(def-realimag-part (x bigfloat)
    (make-bigfloat domain (bigfloat-mantissa x) (bigfloat-exponent x))
  (zero domain))

(defmethod realpart ((x complex-number))
  (let ((real (cn-realpart x))
	(domain (domain-of x)))
    (cond ((typep real 'number)
	   (make-element domain real))
	  (t (make-bigfloat domain
			    (bigfloat-mantissa real)
			    (bigfloat-exponent real))))))

(defmethod imagpart ((x complex-number))
  (let ((imag (cn-imagpart x))
	(domain (domain-of x)))
    (cond ((typep imag 'number)
	   (make-element domain imag))
	  (t (make-bigfloat domain
			    (bigfloat-mantissa imag)
			    (bigfloat-exponent imag))))))

(defgeneric conjugate (number)
  (:documentation
   "Return the conjugate value of the number."))

(defmethod conjugate  ((x number))
  (cl:conjugate x))

(defmethod conjugate ((x rational-integer))
  (cond ((or (typep (domain-of x) 'complex-numbers)
	     (typep (domain-of x) 'non-strict-domain))
	 x)
	(t (error "Don't know what \"conjugate\" means in ~S"
		  (domain-of x)))))

(defmethod conjugate ((x rational-number))
  (cond ((or (typep (domain-of x) 'complex-numbers)
	     (typep (domain-of x) 'non-strict-domain))
	 x)
	(t (error "Don't know what \"conjugate\" means in ~S"
		  (domain-of x)))))

(defmethod conjugate ((x floating-point-number))
  (cond ((or (typep (domain-of x) 'complex-numbers)
	     (typep (domain-of x) 'non-strict-domain))
	 x)
	(t (error "Don't know what \"conjugate\" means in ~S"
		  (domain-of x)))))

(defmethod conjugate ((x bigfloat))
  (cond ((or (typep (domain-of x) 'complex-numbers)
	     (typep (domain-of x) 'non-strict-domain))
	 x)
	(t (error "Don't know what \"conjugate\" means in ~S"
		  (domain-of x)))))

(defmethod conjugate ((x complex-number))
  (make-instance 'complex-number :domain (domain-of x)
		 :realpart (cn-realpart x)
		 :imagpart (- (cn-imagpart x))))

(defmethod abs ((x number))
  (cl:abs x))

(defmethod abs ((x rational-integer))
  (make-element (domain-of x) (cl:abs (integer-value x))))

(defmethod abs ((x rational-number))
  (make-instance 'rational-number
		 :domain (domain-of x)
		 :numerator (abs (qo-numerator x))     
		 :denominator (qo-denominator x)))

(defmethod abs ((z floating-point-number))
  (make-element (domain-of z) (cl:abs (fp-value z))))

(defmethod abs ((number bigfloat))
  (bind-domain-context (domain-of number)
    (bf-abs number)))

(defmethod abs ((z complex-number))
  (let ((x (cn-realpart z))
	(y (cn-imagpart z)))
    (make-element (domain-of z) (cl:sqrt (+ (* x x) (* y y))))))

(defgeneric phase (number)
  (:documentation
   "Return the phase of the number."))

(defmethod phase ((x number))
  (cl:phase x))

(defmethod phase ((x rational-integer))
  (zero (domain-of x)))

(defmethod phase ((x rational-number))
  (zero (domain-of x)))

(defmethod phase ((z floating-point-number))
  (zero (domain-of z)))

(defmethod phase ((number bigfloat))
  (zero (domain-of number)))

(defmethod phase ((z complex-number))
  (let ((x (cn-realpart z))
	(y (cn-imagpart z)))
    (make-element (domain-of z) (atan y x))))

(defgeneric random-constant (domain &optional height)
  (:documentation
   "Return a random constant."))

(defmethod random-constant ((domain numeric-domain) &optional height)
  (random domain height))

(defvar *default-random-height* most-positive-fixnum)

(defmethod random ((domain rational-integers) 
                   &optional (height *default-random-height*))
  (make-element domain (cl:random height)))

(defmethod random ((domain rational-numbers) 
                   &optional (height *default-random-height*))
  (make-element domain (/ (if (cl:zerop (cl:random 2))
                              (cl:random height)
                              (cl:- (cl:random height)
                          (cl:random height))))))

(defun random-floating-number (height)
  (let ((num (cl:+ (float (cl:random height))
                   (cl:/ (float (cl:random height))
                         (float (cl:random height))))))
    (if (cl:zerop (cl:random 2)) num (cl:- num))))

(defmethod random ((domain real-numbers) 
                   &optional (height *default-random-height*))
  (make-element domain (random-floating-number height)))

(defmethod random ((domain complex-numbers)
                   &optional (height *default-random-height*))
  (make-instance 'complex-number :domain domain
                 :realpart (random-floating-number height)
                 :imagpart (random-floating-number height)))

(defmethod height ((x number))
  (cl:abs x))

(defmethod height ((x rational-integer))
  (make-element (get-real-numbers) (cl:abs (integer-value x))))

(defmethod height ((x rational-number))
  (make-element (get-real-numbers) (cl:max (cl:abs (qo-numerator x))     
                                           (qo-denominator x))))

(defmethod height ((z floating-point-number))
  (make-element (get-real-numbers) (cl:abs (fp-value z))))

;; FIXTHIS I think this is buggy!
(defmethod height ((number bigfloat))
  (bind-domain-context (domain-of number)
    (bf-abs number)))

(defmethod height ((z complex-number))
  (let ((x (cn-realpart z))
	(y (cn-imagpart z)))
    (make-element (get-real-numbers) (cl:max (cl:abs x) (cl:abs y)))))

(defgeneric convert-to-lisp-number (number)
  (:documentation
   "Return a lisp representation of the number."))

(defmethod convert-to-lisp-number ((x number))
  x)

(defmethod convert-to-lisp-number ((x numeric))
  x)

(defmethod convert-to-lisp-number ((x rational-integer))
  (integer-value x))

(defmethod convert-to-lisp-number ((x rational-number))
  (cl:/ (qo-numerator x) (qo-denominator x)))

(defmethod convert-to-lisp-number ((x floating-point-number))
  (fp-value x))

(defmethod convert-to-lisp-number ((x bigfloat))
  x)

(defun parse-numeric-obj (num)
  ;;(declare (values num type domain))
  (cond ((typep num 'number)
	 (values num
		 (if (typep num 'integer) 'integer (cl:type-of num))
		 nil))
	((typep num 'rational-integer)
	 (values (integer-value num) 'rational-integer (domain-of num)))
	((typep num 'rational-number)
	 (values (cl:/ (qo-numerator num)
                       (qo-denominaTor num))
		 'rational-number
		 (domain-of num)))
	((typep num 'numeric)
	 (values num (class-name (class-of num)) (domain-of num)))
	(t (error "~S is not a numeric object" num))))

(defmethod numerator ((n rational-integer))
  (cond ((or (field? (domain-of n))
	     (typep (domain-of n) 'non-strict-domain))
	 n)
	(t (error "Don't know what \"numerator\" means in ~S"
		  (domain-of n)))))

(defmethod denominator ((n rational-integer))
  (cond ((or (field? (domain-of n))
	     (typep (domain-of n) 'non-strict-domain))
	 (one (domain-of n)))
	(t (error "Don't know what \"denominator\" means in ~S"
		  (domain-of n)))))

(defmethod 0? (x)
  (declare (ignore x))
  nil)

(defmethod 0? ((x number))
  (cl:zerop x))

(defmethod 0? ((x rational-integer))
  (cl:zerop (integer-value x)))

(defmethod 0? ((x rational-number))
  nil)

(defmethod 0? ((x floating-point-number))
  (cl:zerop (fp-value x)))

(defmethod 0? ((number bigfloat))
  (equal (bigfloat-mantissa number) 0))

(defmethod 0? ((x complex-number))
  (and (0? (realpart x)) (0? (imagpart x))))

(defmethod 1? (x)
  (declare (ignore x))
  nil)

(defmethod 1? ((x number))  
  (= x 1))

(defmethod 1? ((x rational-integer))  
  (eql (integer-value x) 1))

(defmethod 1? ((x rational-number))
  nil)

(defmethod 1? ((x floating-point-number))
  (cl:= 1.0 (fp-value x)))

(defmethod 1? ((number bigfloat))
  (and (equal (bigfloat-mantissa number) 1)
       (eql (bigfloat-exponent number) 0)))

(defmethod 1? ((x complex-number))
  (and (1? (realpart x)) (0? (imagpart x))))

(defmethod minus ((x number))
  (cl:- x))

(defmethod minus ((x rational-integer))
  (make-element (domain-of x) (cl:- (integer-value x))))

(defmethod minus ((x rational-number))
  (make-quotient-element (domain-of x)
			 (cl:- (qo-numerator x))
			 (qo-denominator x)))

(defmethod minus ((x floating-point-number))
  (make-element (domain-of x) (cl:- (fp-value x))))

(defmethod minus ((number bigfloat))
  (bind-domain-context (domain-of number)
    (bf-minus number)))

(defmethod minus ((x complex-number))
  (make-element (domain-of x) (- (cn-realpart x)) (- (cn-imagpart x))))

(defmethod minus? ((x number))
  (cl:minusp x))

(defmethod minus? ((x cl:complex))
  nil)

(defmethod minus? ((x rational-integer))
  (cl:minusp (integer-value x)))

(defmethod minus? ((x rational-number))
  (cl:minusp (qo-numerator x)))

(defmethod minus? ((x floating-point-number))
  (cl:minusp (fp-value x)))

(defmethod minus? ((x bigfloat))
  (cl:minusp (bigfloat-mantissa x)))

(defgeneric plus? (number)
  (:documentation
   "Return true if the number is positive."))

(defmethod plus? ((x number))
  (cl:plusp x))

(defmethod plus? ((x cl:complex))
  (not (cl:zerop x)))

(defmethod plus? ((x rational-integer))
  (cl:plusp (integer-value x)))

(defmethod plus? ((x rational-number))
  (cl:plusp (qo-numerator x)))

(defmethod plus? ((x floating-point-number))
  (cl:plusp (fp-value x)))

(defmethod plus? ((x bigfloat))
  (cl:plusp (bigfloat-mantissa x)))

(defgeneric integer? (number)
  (:documentation
   "Return true if the number is an integer."))

(defmethod integer? ((x number))
  (cl:integerp x))

(defmethod integer? ((x numeric))
  nil)

(defmethod integer? ((x rational-integer))
  t)

(defmethod recip ((x number))
  (cl:/ x))

(defmethod recip ((x rational-integer))
  (let ((x-val (integer-value x))
	(domain (domain-of x)))
    (cond ((or (eql x-val 1) (eql x-val -1))
	   x)
	  ((or (field? domain)
	       (typep domain 'non-strict-domain))
	   (make-element domain (cl:/ 1 x-val)))
	  (t 
	   (error "Trying to take the reciprocal of the rational integer ~S"
		  x)))))

;; recip of a rational integer is covered by QUOTIENT-ELEMENT

(defmethod recip ((x floating-point-number))
  (when (0? x)
    (error "Error: Attempt take reciprocal of zero: ~S" x))
  (make-element (domain-of x) (cl:/ (fp-value x))))

(defmethod recip ((z complex-number))
  (when (0? z)
    (error "Error: Attempt take reciprocal of zero: ~S" z))
  (let ((x (realpart z))
	(y (imagpart z))
	denom)
    (setq denom (+ (* x x) (* y y)))
    (make-element (domain-of z)
                  (convert-to-lisp-number (/ x denom))
                  (convert-to-lisp-number (/ (- y) denom)))))

(defgeneric sqrt (number)
  (:documentation
   "Return the square root of the number."))

(defmethod sqrt ((x number))
  (cl:sqrt  x))

(defmethod sqrt ((x integer))
  (let* ((n (cl:abs x))
	 (root (faster-isqrt n)))
    (unless (cl:= n (cl:* root root))
      (setq root (cl:sqrt n)))
    (if (minus? x) (cl:complex 0 root)
	root)))

(defmethod sqrt ((x rational-integer))
  (let ((domain (domain-of x)))
    (cond ((complete-set? domain)
	   (make-element domain (cl:sqrt (integer-value x))))
	  ((minus? (integer-value x))
	   (error "Can't take the sqrt of a negative number: ~S" x))
	  (t (let* ((n (integer-value x))
		    (root (faster-isqrt n)))
	       (cond ((cl:= n (cl:* root root))
		      (make-element domain root))
		     (t (error "~S does not have a sqrt in ~S"
			       x domain))))))))

(defmethod sqrt ((x rational-number))
  (let ((domain (domain-of x)))
    (cond ((complete-set? domain)
	   (make-element domain (cl:sqrt (cl:/ (qo-numerator x)
                                               (qo-denominator x)))))
	  (t (let* ((n (qo-numerator x))
		    (d (qo-denominator x))
		    (n-root (faster-isqrt n))
		    (d-root (faster-isqrt d)))
	       (cond ((and (cl:= n (cl:* n-root n-root))
			   (cl:= d (cl:* d-root d-root)))
		      (make-quotient-element domain n-root d-root))
		     (t (error "~S does not have a sqrt in ~S"
			       x domain))))))))

(defmethod sqrt ((x floating-point-number)) 
  (make-element (domain-of x) (cl:sqrt (fp-value x))))

(defmethod sqrt ((number bigfloat))
  (bind-domain-context (domain-of number)
    (bf-sqrt number *REAL-PRECISION*)))

(defmethod sqrt ((number complex-number))
  (let* ((x (cn-realpart number))
	 (y (cn-imagpart number))
	 (mag (/ (sqrt (+ (* x x) (* y y))) 2)))
    (make-element (domain-of number)
                  (sqrt (+ mag (/ x 2)))
                  (if (plus? y)
                      (sqrt (- mag (/ x 2)))
                      (- (sqrt (- mag (/ x 2))))))))

;; The following idea was stolen from the Mindy implementation of Dylan.
(defmacro with-contagion ((x y) &body body)
  (let (x-val y-val)
    (if (atom x) (setq x-val x)
	(setq x-val (second x)
	      x (first x)))
    (if (atom y) (setq y-val y)
	(setq y-val (second y)
	      y (first y)))
    `(multiple-value-bind (,x ,y) (contagion ,x-val ,y-val)
      ,@body)))

;; If this changes, you should also change the numeric-numeric part of
;; def-binary-coercions in morphisms.lisp
(defmacro define-binary-contagions (binary-op &key (numeric-numeric? t)
                                    (number-numeric? t))
  `(progn
    ,@(when number-numeric?
            `((defmethod ,binary-op ((.x. number) (.y. numeric))
                (with-contagion (.x. .y.) (,binary-op .x. .y.)))
              (defmethod ,binary-op ((.x. numeric) (.y. number))
                (with-contagion (.x. .y.) (,binary-op .x. .y.)))))
    ,(when numeric-numeric?
           `(defmethod ,binary-op ((.x. numeric) (.y. numeric))
             (let ((x-domain (domain-of .x.))
                   (y-domain (domain-of .y.)))
               (cond ((eql x-domain y-domain)
                      (when (eql (class-of .x.) (class-of .y.))
                        (error "No applicable contagion method for ~S and ~S" .x. .y.))
                      (with-contagion (.x. .y.) (,binary-op .x. .y.)))
                     ((typep (domain-of .x.) 'non-strict-domain)
                      (,binary-op .x. (coerce .y. (domain-of .x.))))
                     ((typep (domain-of .y.) 'non-strict-domain)
                      (,binary-op (coerce .x. (domain-of .y.)) .y.))
                     (t (call-next-method))))))))

(defgeneric contagion (number1 number2)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod contagion ((x number) (y number))
  (values x y))

(defmethod contagion ((x number) (y numeric))
  (values (coerce x (domain-of y)) y))

(defmethod contagion ((x numeric) (y number))
  (values x (coerce y (domain-of x))))

(defmethod-sd contagion ((x numeric) (y numeric))
  (values x y))

(defmethod-sd contagion ((x rational-integer) (y rational-number))
  (values (make-instance 'rational-number :domain domain
			 :numerator (integer-value x)
			 :denominator 1)
	  y))

(defmethod-sd contagion ((x rational-integer) (y floating-point-number))
  (values (make-element domain (float (integer-value x))) y))

(defmethod-sd contagion ((x rational-integer) (y bigfloat))
  (values (convert-to-bigfloat x) y))

(defmethod-sd contagion ((x rational-integer) (y complex-number))
  (values (make-instance 'complex-number :domain domain
			 :realpart x
			 :imagpart 0)
	  y))

(defmethod-sd contagion ((x rational-number) (y rational-integer))
  (values x
	  (make-instance 'rational-number :domain domain
			 :numerator (integer-value y)
			 :denominator 1)))

(defmethod-sd contagion ((x rational-number) (y floating-point-number))
  (values (make-element domain (cl:/ (float (qo-numerator x))
				     (qo-denominator x)))
	  y))

(defmethod-sd contagion ((x rational-number) (y bigfloat))
  (values (convert-to-bigfloat x) y))

(defmethod-sd contagion ((x rational-number) (y complex-number))
  (values (make-instance 'complex-number :domain domain
			 :realpart (cl:/ (qo-denominator x)
					 (qo-numerator x))
			 :imagpart 0)
	  y))

(defmethod-sd contagion ((x floating-point-number) (y rational-integer))
  (values x (make-element domain (float (integer-value y)))))

(defmethod-sd contagion ((x floating-point-number) (y rational-number))
  (values x
	  (make-element domain (cl:/ (float (qo-numerator y))
				     (qo-denominator y)))))

(defmethod-sd contagion ((x floating-point-number) (y bigfloat))
  (values (convert-to-bigfloat x) y))

(defmethod-sd contagion ((x floating-point-number) (y complex-number))
  (values (make-instance 'complex-number :domain domain
			 :realpart (fp-value x)
			 :imagpart 0.0)
	  y))

(defmethod-sd contagion ((x bigfloat) (y rational-integer))
  (values x (make-element domain (float (integer-value y)))))

(defmethod-sd contagion ((x bigfloat) (y rational-number))
  (values x
	  (make-element domain (cl:/ (float (qo-numerator y))
				     (qo-denominator y)))))

(defmethod-sd contagion ((x bigfloat) (y floating-point-number))
  (values (convert-to-bigfloat x) y))

(defmethod-sd contagion ((x bigfloat) (y complex-number))
  (values (make-instance 'complex-number :domain domain
			 :realpart x
			 :imagpart 0.0)
	  y))

(defmethod-sd contagion ((x complex-number) (y rational-integer))
  (values x (make-instance 'complex-number :domain domain
			   :realpart (integer-value y)
			   :imagpart 0)))

(defmethod-sd contagion ((x complex-number) (y rational-number))
  (values x
	  (make-instance 'complex-number :domain domain
                         :realpart (cl:/ (qo-numerator y)
                                         (qo-denominator y))
                         :imagpart 0)))

(defmethod-sd contagion ((x complex-number) (y floating-point-number))
  (values (make-instance 'complex-number :domain domain
			 :realpart (fp-value x)
			 :imagpart 0.0)
	  y))

(defmethod-sd contagion ((x complex-number) (y bigfloat))
  (values (make-instance 'complex-number :domain domain
			 :realpart y
			 :imagpart 0.0)
	  y))

;; These routines don't really need the contagion tools. 

(defmethod binary= ((x number) (y number))
  (cl:= x y))

(defmethod binary= ((x number) (y numeric))
  (cl:= x (convert-to-lisp-number y)))

(defmethod binary= ((x numeric) (y number))
  (cl:= (convert-to-lisp-number x) y))

;; Unless otherwise, checked two number are never equal!
(defmethod binary= ((x numeric) (y numeric))
  nil)

(defmethod-sd binary= ((x rational-integer) (y rational-integer))
  (eql (integer-value x) (integer-value y)))

(defmethod-sd binary= ((x floating-point-number) (y floating-point-number) )
  (cl:= (fp-value x) (fp-value y)))

(defmethod-sd binary= ((x bigfloat) (y bigfloat))
  (bf-binary= x y))

(defmethod-sd binary= ((x complex-number) (y complex-number))
  (and (= (cn-realpart x) (cn-realpart y))
       (= (cn-imagpart x) (cn-imagpart y))))

(define-binary-contagions binary>
    :number-numeric? nil)

(defmethod binary> ((x number) (y number))
  (cl:> x y))

(defmethod binary> ((x number) (y numeric))
  (cl:> x (convert-to-lisp-number y)))

(defmethod binary> ((x numeric) (y number))
  (cl:> (convert-to-lisp-number x) y))

(defmethod-sd binary> ((x rational-integer) (y rational-integer))
  (if (ordered-domain? domain)
      (cl:> (integer-value x) (integer-value y))
      (call-next-method)))

(defmethod-sd binary> ((x rational-number) (y rational-number))
  (if (ordered-domain? domain)
      (cl:> (cl:* (qo-numerator x) (qo-denominator y))
	      (cl:* (qo-numerator y) (qo-denominator x)))
      (call-next-method)))

(defmethod-sd binary> ((x floating-point-number) (y floating-point-number))
  (if (ordered-domain? domain)
      (cl:> (fp-value x) (fp-value y))
      (call-next-method)))

(define-binary-contagions binary>=
    :number-numeric? nil)

(defmethod binary>= ((x number) (y number))
  (cl:>= x y))

(defmethod binary>= ((x number) (y numeric))
  (cl:>= x (convert-to-lisp-number y)))

(defmethod binary>= ((x numeric) (y number))
  (cl:>= (convert-to-lisp-number x) y))

(defmethod-sd binary>= ((x rational-integer) (y rational-integer))
  (if (ordered-domain? domain)
      (cl:>= (integer-value x) (integer-value y))
      (call-next-method)))

(defmethod-sd binary>= ((x rational-number) (y rational-number))
  (if (ordered-domain? domain)
      (cl:>= (cl:* (qo-numerator x) (qo-denominator y))
             (cl:* (qo-numerator y) (qo-denominator x)))
      (call-next-method)))

(defmethod-sd binary>= ((x floating-point-number) (y floating-point-number))
  (if (ordered-domain? domain)
      (cl:>= (fp-value x) (fp-value y))
      (call-next-method)))

(define-binary-contagions max-pair)

(defmethod max-pair ((x number) (y number))
  (if (cl:> x y) x y))

;; Added the following two methods. Look at the related note for
;; min-pair.                             -- Sekhar 8/3/94

(defmethod max-pair :around ((x number) (y t))
  (cond ((eql x *positive-infinity*) x)
	((eql x *negative-infinity*) y)
	(t (call-next-method))))

(defmethod max-pair :around ((x t) (y number))
  (cond ((eql y *positive-infinity*) y)
	((eql y *negative-infinity*) x)
	(t (call-next-method))))

(defmethod-sd max-pair ((x rational-integer) (y rational-integer))
  (if (ordered-domain? domain)
      (if (cl:>= (integer-value x) (integer-value y))
	  x y)
      (call-next-method)))

(defmethod-sd max-pair ((x rational-number) (y rational-number))
  (if (ordered-domain? domain)
      (if (cl:>= (cl:* (qo-numerator x) (qo-denominator y))
		 (cl:* (qo-numerator y) (qo-denominator x)))
	  x y)
      (call-next-method)))

(defmethod-sd max-pair ((x floating-point-number) (y floating-point-number))
  (if (ordered-domain? domain)
      (if (cl:>= (fp-value x) (fp-value y))
	  x y)
      (call-next-method)))

(define-binary-contagions min-pair)

(defmethod min-pair ((x number) (y number))
  (if (cl:> x y) y x))

;; Added the following two methods to fix a problem occurred while
;; while exponentiating a tpower-series by an integer. -- Sekhar 8/3/94

(defmethod min-pair :around ((x number) (y t))
  (cond ((eql x *positive-infinity*) y)
	((eql x *negative-infinity*) x)
	(t (call-next-method))))

(defmethod min-pair :around ((x t) (y number))
  (cond ((eql y *positive-infinity*) x)
	((eql y *negative-infinity*) y)
	(t (call-next-method))))

(defmethod-sd min-pair ((x rational-integer) (y rational-integer))
  (if (ordered-domain? domain)
      (if (cl:<= (integer-value x) (integer-value y))
	  x y)
      (call-next-method)))

(defmethod-sd min-pair ((x rational-number) (y rational-number))
  (if (ordered-domain? domain)
      (if (cl:<= (cl:* (qo-numerator x) (qo-denominator y))
                 (cl:* (qo-numerator y) (qo-denominator x)))
	  x y)
      (call-next-method)))

(defmethod-sd min-pair ((x floating-point-number) (y floating-point-number))
  (if (ordered-domain? domain)
      (if (cl:<= (fp-value x) (fp-value y))
	  x y)
      (call-next-method)))

(define-binary-contagions plus)

(defmethod plus ((x number) (y number))
  (cl:+ x y))

(defmethod-sd plus ((x rational-integer) (y rational-integer))
  (make-instance 'rational-integer :domain domain
		 :value (cl:+ (integer-value x) (integer-value y))))

(defmethod-sd plus ((x rational-number) (y rational-number))
  (make-element domain
		(cl:+ (cl:/ (qo-numerator x) (qo-denominator x))
		      (cl:/ (qo-numerator y) (qo-denominator y)))))

(defmethod-sd plus ((x floating-point-number) (y floating-point-number))
  (make-element domain (cl:+ (fp-value x) (fp-value y))))

(defmethod-sd plus ((x bigfloat) (y bigfloat))
  (bind-domain-context domain
    (round!mt (bf-plus x y) *REAL-PRECISION*)))

(defmethod-sd plus ((x complex-number) (y complex-number))
  (make-element domain
		(+ (cn-realpart x) (cn-realpart y))
		(+ (cn-imagpart x) (cn-imagpart y))))

(define-binary-contagions difference)

(defmethod difference ((x number) (y number))
  (cl:- x y))

(defmethod-sd difference ((x rational-integer) (y rational-integer))
  (make-instance 'rational-integer :domain domain
		 :value (cl:- (integer-value x) (integer-value y))))

(defmethod-sd difference ((x rational-number) (y rational-number))
  (make-element domain
		(cl:- (cl:/ (qo-numerator x) (qo-denominator x))
		      (cl:/ (qo-numerator y) (qo-denominator y)))))

(defmethod-sd difference ((x floating-point-number) (y floating-point-number))
  (make-element domain (cl:- (fp-value x) (fp-value y))))

(defmethod-sd difference ((x bigfloat) (y bigfloat))
  (bind-domain-context domain
    (round!mt (bf-difference x y) *REAL-PRECISION*)))

(defmethod-sd difference ((x complex-number) (y complex-number))
  (make-element domain
		(- (cn-realpart x) (cn-realpart y))
		(- (cn-imagpart x) (cn-imagpart y))))

(define-binary-contagions times)

(defmethod times ((x number) (y number))
  (cl:* x y))

(defmethod-sd times ((x rational-integer) (y rational-integer))
  (make-instance 'rational-integer :domain domain
		 :value (cl:* (integer-value x) (integer-value y))))

(defmethod-sd times ((x rational-number) (y rational-number))
  (make-element domain
		(cl:* (cl:/ (qo-numerator x) (qo-denominator x))
		      (cl:/ (qo-numerator y) (qo-denominator y)))))

(defmethod-sd times ((x floating-point-number) (y floating-point-number))
  (make-element domain (cl:* (fp-value x) (fp-value y))))

(defmethod-sd times ((x bigfloat) (y bigfloat))
  (bind-domain-context domain
    (round!mt (bf-times x y) *REAL-PRECISION*)))

(defmethod-sd times ((x complex-number) (y complex-number))
  (let ((x-real (cn-realpart x))
	(x-imag (cn-imagpart x))
	(y-real (cn-realpart y))
	(y-imag (cn-imagpart y)))
    (make-element domain
                  (- (* x-real y-real) (* x-imag y-imag))
                  (+ (* x-real y-imag) (* x-imag y-real)))))

(define-binary-contagions quotient)

(defmethod quotient ((x number) (y number))
  (cl:/ x y))

;; Changed this method so that if x|y then we get an integer. Made this
;; change to fix a problem occurred while exponentiating a tpower-series
;; by an integer.                               -- Sekhar 8/3/94
(defmethod-sd quotient ((x rational-integer) (y rational-integer))
  (if (1? y) x
      (let ((quo (cl:/ (integer-value x) (integer-value y))))
        (cond ((typep quo 'integer)
               (make-element domain quo))
              ((or (field? domain)
                   (typep domain 'non-strict-domain))
               (make-element domain quo))
              (t (call-next-method))))))

(defmethod-sd quotient ((x rational-number) (y rational-number))
  (make-element domain
		(cl:/ (cl:* (qo-numerator x) (qo-denominator y))
		      (cl:* (qo-numerator y) (qo-denominator x)))))

(defmethod-sd quotient ((x floating-point-number) (y floating-point-number))
  (make-element domain (cl:/ (fp-value x) (fp-value y))))

(defmethod-sd quotient ((x bigfloat) (y bigfloat))
  (bind-domain-context domain
    (round!mt (bf-quotient x y *REAL-PRECISION*)
	      *REAL-PRECISION*)))

(defmethod-sd quotient ((x complex-number) (y complex-number))
  (let* ((x-real (cn-realpart x))
	 (x-imag (cn-imagpart x))
	 (y-real (cn-realpart y))
	 (y-imag (cn-imagpart y))
	 (norm (+ (* y-real y-real) (* y-imag y-imag))))
    (make-element (domain-of x)
		  (/ (+ (* x-real y-real) (* x-imag y-imag)) norm)
		  (/ (- (* x-imag y-real) (* x-real y-imag)) norm))))

(defmethod expt ((n number) (e number))
  (cl:expt n e))

(defmethod expt ((n integer) (e ratio))
  (let* ((num (cl:numerator e))
	 (den (cl:denominator e))
	 (nn (abs n))
	 (root (integer-nth-root nn den)))
    (setq root
	  (if (and root (cl:= nn (cl:expt root den)))
	      (cl:expt root num)
	      (cl:expt nn e)))
    (cond ((cl:minusp n)
	   (if (cl:evenp den)
	       (cl:complex 0 root)
	       (cl:- root)))
	  (t root))))

(defmethod expt ((n ratio) (e ratio))
  (cl:/ (expt (cl:numerator n) e)
        (expt (cl:denominator n) e)))

(defmethod expt ((n rational-integer) (e integer))
  (let ((domain (domain-of n)))
    (cond ((1? n) (one domain))
	  ((cl:minusp e)
	   (if (or (field? domain)
		   (typep domain 'non-strict-domain))
	       (make-quotient-element domain 1
				      (cl:expt (integer-value n) (cl:- e)))
	       (error "Raising ~D to a negative power ~D" n e)))
	  (t (if (eql (integer-value n) -1)
		 (if (oddp e) (- (one domain)) (one domain))
		 (make-element (domain-of n)
			       (cl:expt (integer-value n) e)))))))

(defmethod-sd expt ((n rational-integer) (e rational-integer))
  (cond ((1? n) (one domain))
	((cl:minusp (integer-value e))
	 (if (or (field? domain)
		 (typep domain 'non-strict-domain))
	     (make-quotient-element domain 1 (integer-value n))
	     (error "Raising ~D to a negative power ~D" n e)))
	(t (if (eql (integer-value n) -1)
	       (if (oddp (integer-value e)) (- (one domain)) (one domain))
	       (make-element (domain-of n)
                             (cl:expt (integer-value n) (integer-value e)))))))

(defmethod expt ((n rational-integer) (e ratio))
  (let* ((domain (domain-of n))
	 (nn (integer-value n))
	 (abs-nn (cl:abs nn))
	 (num (cl:numerator e))
	 (den (cl:denominator e))
	 (root (integer-nth-root abs-nn den)))
    (setq root 
	  (cond ((cl:= abs-nn (cl:expt root den))
		 (cl:expt root num))
		((complete-set? domain)
		 (cl:expt nn e))
		(t (error "Can't compute ~S to the ~S power in ~S"
			  n e domain))))
    (cond ((cl:minusp nn)
	   (cond ((cl:oddp den)
		  (make-element domain (cl:- root)))
		 ((complete-set? domain)
		  (make-element domain (cl:complex 0 root)))
		 (t (error "Can't compute ~S to the ~S power in ~S"
			   n e domain))))
	  (t (make-element domain root)))))

(defmethod expt ((n rational-integer) (e rational-number))
  (let* ((domain (domain-of n))
	 (nn (integer-value n))
	 (abs-nn (cl:abs nn))
	 (num (qo-numerator e))
	 (den (qo-denominator e))
	 (root (integer-nth-root abs-nn den)))
    (setq root 
	  (cond ((cl:= abs-nn (cl:expt root den))
		 (cl:expt root num))
		((complete-set? domain)
		 (cl:expt nn e))
		(t (error "Can't compute ~S to the ~S power in ~S"
			  n e domain))))
    (cond ((cl:minusp nn)
	   (cond ((cl:oddp den)
		  (make-element domain (cl:- root)))
		 ((complete-set? domain)
		  (make-element domain (cl:complex 0 root)))
		 (t (error "Can't compute ~S to the ~S power in ~S"
			   n e domain))))
	  (t (make-element domain root)))))

(defmethod expt ((x rational-number) (y ratio))
  (/ (expt (numerator x) y)
     (expt (denominator x) y)))

(defmethod expt ((x rational-number) (y rational-number))
  (/ (expt (numerator x) y)
     (expt (denominator x) y)))

(defmethod expt ((x floating-point-number) (y number))
  (make-element (domain-of x) (cl:expt (fp-value x) y)))

(defmethod expt ((x floating-point-number) (y rational-integer))
  (make-element (domain-of x) (cl:expt (fp-value x) (integer-value y))))

(defmethod expt ((x floating-point-number) (y rational-number))
  (make-element (domain-of x)
		(cl:expt (fp-value x)
                         (cl:/ (numerator y) (denominator y)))))

(defmethod expt ((x floating-point-number) (y floating-point-number))
  (make-element (domain-of x) (cl:expt (fp-value x) (fp-value y))))

(defmethod expt ((number bigfloat) (k integer))
  (cond ((eql k 0) (make-bigfloat (domain-of number) 1 0))
	((eql k 1) number)
	(t (let ((domain (domain-of number)))
	     (bind-domain-context domain
	       (bf-expt number k *REAL-PRECISION*))))))

;; FIXTHIS: exponentiation of complex numbers needs to be improved 
(defmethod expt ((x complex-number) (y integer))
  (cond ((minusp y) (recip (expt x (- y))))
	((zerop y) (one (domain-of x)))
	((1? y) x)
	(t (let ((half (expt x (ash y -1))))
	     (if (oddp y) (* half half x)
		 (* half half))))))

;; The first value returned by TRUNCATE is an integer for numbers, and
;; is returned in the domain of the first argument.  The second value
;; is returned in the domain of the second argument.

(defmethod truncate1 ((a number))
  (cl:truncate a))

(defmethod truncate1 ((a rational-integer))
  (values a (zero (domain-of a))))

(defmethod truncate1 ((a rational-number))
  (multiple-value-bind (q r)
      (cl:truncate (qo-numerator a) (qo-denominator a))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of a) r))))

(defmethod truncate1 ((a floating-point-number))
  (multiple-value-bind (q r) (cl:truncate (fp-value a))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of a) r))))

(defmethod truncate1 ((a complex-number))
  (error "Improper numeric argument"))

(defmethod truncate2 ((a number) (b number))
  (cl:truncate a b))

(defmethod truncate2 ((a numeric) (b numeric))
  (multiple-value-bind (q r) (cl:truncate (convert-to-lisp-number a)
					  (convert-to-lisp-number b))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of b) r))))

(defmethod truncate2 ((a numeric) (b complex-number))
  (error "Improper numeric argument"))

(defmethod truncate2 ((a complex-number) (b numeric))
  (error "Improper numeric argument"))

(defmethod floor1 ((a number))
  (cl:floor a))

(defmethod floor1 ((a rational-integer))
  (values a (zero (domain-of a))))

(defmethod floor1 ((a rational-number))
  (multiple-value-bind (q r)
      (cl:floor (qo-numerator a) (qo-denominator a))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of a) r))))

(defmethod floor1 ((a floating-point-number))
  (multiple-value-bind (q r) (cl:floor (fp-value a))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of a) r))))

(defmethod floor1 ((a complex-number))
  (error "Improper numeric argument"))

(defmethod floor2 ((a number) (b number))
  (cl:floor a b))

(defmethod floor2 ((a numeric) (b numeric))
  (multiple-value-bind (q r) (cl:floor (convert-to-lisp-number a)
                                       (convert-to-lisp-number b))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of b) r))))

(defmethod floor2 ((a numeric) (b complex-number))
  (error "Improper numeric argument"))

(defmethod floor2 ((a complex-number) (b numeric))
  (error "Improper numeric argument"))

(defmethod ceiling1 ((a number))
  (cl:ceiling a))

(defmethod ceiling1 ((a rational-integer))
  (values a (zero (domain-of a))))

(defmethod ceiling1 ((a rational-number))
  (multiple-value-bind (q r)
      (cl:ceiling (qo-numerator a) (qo-denominator a))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of a) r))))

(defmethod ceiling1 ((a floating-point-number))
  (multiple-value-bind (q r) (cl:ceiling (fp-value a))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of a) r))))

(defmethod ceiling1 ((a complex-number))
  (error "Improper numeric argument"))

(defmethod ceiling2 ((a number) (b number))
  (cl:ceiling a b))

(defmethod ceiling2 ((a numeric) (b numeric))
  (multiple-value-bind (q r) (cl:ceiling (convert-to-lisp-number a)
                                         (convert-to-lisp-number b))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of b) r))))

(defmethod ceiling2 ((a numeric) (b complex-number))
  (error "Improper numeric argument"))

(defmethod ceiling2 ((a complex-number) (b numeric))
  (error "Improper numeric argument"))

(defmethod round1 ((a number))
  (cl:round a))

(defmethod round1 ((a rational-integer))
  (values a (zero (domain-of a))))

(defmethod round1 ((a rational-number))
  (multiple-value-bind (q r)
      (cl:round (qo-numerator a) (qo-denominator a))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of a) r))))

(defmethod round1 ((a floating-point-number))
  (multiple-value-bind (q r) (cl:round (fp-value a))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of a) r))))

(defmethod round1 ((a complex-number))
  (error "Improper numeric argument"))

(defmethod round2 ((a number) (b number))
  (cl:round a b))

(defmethod round2 ((a numeric) (b numeric))
  (multiple-value-bind (q r) (cl:round (convert-to-lisp-number a)
                                       (convert-to-lisp-number b))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of b) r))))

(defmethod round2 ((a numeric) (b complex-number))
  (error "Improper numeric argument"))

(defmethod round2 ((a complex-number) (b numeric))
  (error "Improper numeric argument"))

(defmethod remainder ((a number) (b number))
  (cl:rem a b))

(defmethod-sd remainder ((a rational-integer) (b rational-integer))
  (make-element domain (cl:rem (integer-value a) (integer-value b))))

(defgeneric even? (number)
  (:documentation
   "Return true if the number is even."))

(defmethod even? ((a number))
  (cl:evenp a))

(defmethod even? ((a rational-integer))
  (cl:evenp (integer-value a)))

(defgeneric oddp? (number)
  (:documentation
   "Return true if the number is odd."))

(defmethod oddp? ((a number))
  (cl:evenp a))

(defmethod oddp? ((a rational-integer))
  (cl:evenp (integer-value a)))

(defmethod binary-gcd ((a integer) (b integer))
  (cl:gcd a b))

;; Do we really need this???
(defmethod binary-gcd ((a float) (b float))
  a)

;; All this extra stuff is because this method and lcm below override
;; the definitions in morphisms.lisp

(defmethod binary-gcd ((a numeric) (b numeric))
  (let ((a-domain (domain-of a)) (b-domain (domain-of b)))
    (cond ((eql a-domain b-domain) (one a-domain))
          ((typep (domain-of a) 'non-strict-domain)
           (gcd a (coerce b (domain-of a))))
          ((typep (domain-of b) 'non-strict-domain)
           (gcd (coerce a (domain-of b)) b))
          (t (call-next-method)))))

(defmethod-sd binary-gcd ((a rational-integer) (b rational-integer))
  (make-element domain (cl:gcd (integer-value a) (integer-value b))))

(defmethod binary-lcm ((a integer) (b integer))
  (cl:* (cl:/ a (cl:gcd a b)) b))

(defmethod binary-lcm ((a numeric) (b numeric))
  (let ((a-domain (domain-of a)) (b-domain (domain-of b)))
    (cond ((eql a-domain b-domain) (* a b))
          ((typep (domain-of a) 'non-strict-domain)
           (gcd a (coerce b (domain-of a))))
          ((typep (domain-of b) 'non-strict-domain)
           (gcd (coerce a (domain-of b)) b))
          (t (call-next-method)))))

(defmethod-sd binary-lcm ((a rational-integer) (b rational-integer))
  (let ((a (integer-value a))
	(b (integer-value b)))
    (make-element domain (cl:* (cl:/ a (cl:gcd a b)) b))))

(defun extended-gcd* (a b)
  (if (= b 0)
      (values a 1 0)
      (multiple-value-bind (d x y) (extended-gcd* b (mod a b))
	(values d y (cl:- x (cl:* (cl:floor a b) y))))))

(defgeneric extended-gcd (numerator denominator)
  (:documentation
   "Return the greatest common denominator."))

(defmethod extended-gcd ((a integer) (b integer))
  (multiple-value-bind (d x y) (extended-gcd* (abs a) (abs b))
    (values (cl:* (signum a) x) (cl:* (signum b) y) d)))

;; Some single argument functions

(defmethod sin ((x floating-point-number))
  (make-element (domain-of x) (cl:sin (fp-value x))))

(defmethod cos ((x floating-point-number))
  (make-element (domain-of x) (cl:cos (fp-value x))))

(defmethod tan ((x floating-point-number))
  (make-element (domain-of x) (cl:tan (fp-value x))))

(defmethod asin ((x floating-point-number))
  (make-element (domain-of x) (cl:asin (fp-value x))))

(defmethod acos ((x floating-point-number))
  (make-element (domain-of x) (cl:acos (fp-value x))))

(defmethod sinh ((x floating-point-number))
  (make-element (domain-of x) (cl:sinh (fp-value x))))

(defmethod cosh ((x floating-point-number))
  (make-element (domain-of x) (cl:cosh (fp-value x))))

(defmethod tanh ((x floating-point-number))
  (make-element (domain-of x) (tanh (fp-value x))))

(defmethod asinh ((x floating-point-number))
  (make-element (domain-of x) (cl:asinh (fp-value x))))

(defmethod acosh ((x floating-point-number))
  (make-element (domain-of x) (cl:acosh (fp-value x))))

(defmethod atanh ((x floating-point-number))
  (make-element (domain-of x) (cl:atanh (fp-value x))))

(defmethod exp ((x floating-point-number))
  (make-element (domain-of x) (cl:exp (fp-value x))))

(defmethod log ((x floating-point-number))
  (make-element (domain-of x) (cl:log (fp-value x))))
