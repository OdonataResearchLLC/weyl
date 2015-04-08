;;; -*- Mode:Lisp; Package:Weyli; Syntax:Common-Lisp; Base:10; Lowercase:T -*-
;;; ===========================================================================
;;;				  GF(p)
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; gfp.lisp,v 1.8 1995/05/24 17:42:01 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.8")

(define-domain-element-classes GFp GFp-element)

(defgeneric number-of-elements (domain)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod number-of-elements ((domain GFp))
  (characteristic domain))

(defmethod number-of-elements ((domain GFq))
  (expt (characteristic domain) (field-degree domain)))

(defgeneric make-GFp-domain (number number~)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod make-GFp-domain ((characteristic integer) (degree integer))
  (cond ((= degree 1)
	 (let ((domain (make-instance 'gfp :characteristic characteristic
				      :print-function 'GFp-print-object))
	       (Z (get-rational-integers)))
	   (make-homomorphism
            Z
            (lambda (x)
              (make-element domain
                            (if (cl:integerp x) x
                                (integer-value x))))
            domain)
	   domain))
 	(t (error "Can't do GF(~D^~D) yet" characteristic degree)
	   ;; This is where GFq domains are to be defined.
	   )))

(defun GFp-print-object (d stream)
  #+Genera
  (format stream "~'bGF~(~D)" (characteristic d))
  #-Genera
  (format stream "GF(~D)" (characteristic d)))

(defmethod make-element ((domain GFp) (value integer) &rest ignore)
  (declare (ignore ignore))
  (let ((modulus (characteristic domain)))
    (make-instance 'GFp-element
		   :domain domain
		   :value (reduce-modulo-integer value modulus))))

;; Could have more error checking
(defmethod weyl::make-element ((domain GFp) (value integer) &rest ignore)
  (declare (ignore ignore))
  (make-element domain value))

(defvar *print-modulus* t)

(defmethod print-object ((x GFp-element) stream)
  (with-slots (value domain) x
    (if *print-modulus*
	(format stream "~D(~D)" value (characteristic domain))	
	(format stream "~D" value))))

(defun compute-inverse (value modulus)
  (let ((a1 modulus)
	(a2 (if (cl:< value 0) (cl:+ value modulus) value))
	(y1 0)
	(y2 1)
	q)
    (loop
     (if (eql a2 1) (return (values y2 y1)))
     (if (cl:zerop a2)
         (error "Inverse of zero divisor -- ~d modulo ~d"
                value modulus))
     (setq q (truncate a1 a2))
     (psetq a1 a2 a2 (cl:- a1 (cl:* a2 q)))
     (psetq y1 y2 y2 (cl:- y1 (cl:* y2 q))))))

(defmethod coerce ((value ratio) (domain GFp))
  (make-element
   domain
   (cl:* (cl:numerator value)
         (compute-inverse (cl:denominator value)
                          (characteristic domain)))))

(defmethod coerce ((value rational-integer) (domain GFp))
  (make-element domain (integer-value value)))

(defmethod coerce ((value integer) (domain GFp))
  (make-element domain value))

(defmethod coerce ((element gfp-element) (domain general-expressions))
  (coerce (gfp-value element) domain))

(defmethod-sd binary= ((x GFp-element) (y GFp-element))
  (with-slots ((v1 value) (d1 domain)) x
    (with-slots ((v2 value) (d2 domain)) y
      (and (eq d1 d2) (eql v1 v2)))))

(defmethod 0? ((x GFp-element))
  (with-slots (value) x
    (cl:zerop value)))

(defmethod 1? ((x GFp-element))
  (with-slots (value) x
    (eql value 1)))

;; The following three methods make finite fields behave like quotient fields

(defmethod make-quotient-element ((domain GFp) (a GFp-element) (b GFp-element))
  (unless (eql domain (domain-of a))
    (error "~S should be an element of ~S" a domain))
  (unless (eql domain (domain-of b))
    (error "~S should be an element of ~S" b domain))
  (with-slots ((v1 value)) a
    (with-slots ((v2 value)) b
      (with-slots (characteristic) domain
	(make-element domain
                      (cl:* v1 (compute-inverse v2 characteristic)))))))

(defmethod numerator ((a GFp-element))
  a)

(defmethod denominator ((a GFp-element))
  (make-element (domain-of a) 1))

(defmethod minus ((x GFp-element))
  (with-slots (value domain) x
    (with-slots (characteristic) domain
      (if (eql 2 characteristic) x
	  (make-element domain (cl:- characteristic value))))))

;;; There is no such thing as a negative number in finite fields.
(defmethod minus? ((x GFp-element))
  nil)

(defmethod plus? ((x GFp-element))
  (not (0? x)))

(defmethod-sd plus ((a GFp-element) (b GFp-element))
  (make-element domain (cl:+ (gfp-value a) (gfp-value b))))

(defmethod-sd difference ((a GFp-element) (b GFp-element))
  (make-element domain (cl:- (gfp-value a) (gfp-value b))))

(defmethod-sd times ((a GFp-element) (b GFp-element))
  (make-element domain (cl:* (gfp-value a) (gfp-value b))))

;; Takes the inverse of an integer N mod P.  Solve N*X + P*Y = 1.  N
;; is guaranteed to be less than P, since in the case where P is a
;; fixnum, N is also assumed to be one.

(defmethod recip ((x GFp-element))
  (with-slots (value domain) x
    (with-slots (characteristic) domain
      (make-element domain (reduce-modulo-integer
			    (compute-inverse value characteristic)
			    characteristic)))))

(defmethod expt ((x GFp-element) (e integer))
  (with-slots (value domain) x
    (cond ((eql 1 value) x)
	  ((cl:minusp e)
	   (error "Raising ~D to a negative power ~D" x e))
	  (t (make-element domain
	       (expt-modulo-integer value e (characteristic domain)))))))

(defmethod quotient ((a GFp-element) (b GFp-element)) 
  (with-slots ((v1 value) (d1 domain)) a
    (with-slots ((v2 value) (d2 domain)) b
      (cond ((eq d1 d2)
	     (with-slots (characteristic) d1
	       (make-element d1
		 (cl:* v1 (compute-inverse v2 characteristic)))))
	    (t (error "Taking the quotient of elements of ~
		       different fields: ~S, ~S"
		      a b))))))

(defmethod remainder ((a GFp-element) (b GFp-element))
  (error "Computing the remainder of ~D by ~D"
	 a b))

(defmethod binary-gcd ((a GFp-element) (b GFp-element))
  (with-slots ((d1 domain)) a
    (with-slots ((d2 domain)) b
      (cond ((eq d1 d2) (make-element d1 1))
	    (t (error "Taking the GCD of elements of different fields: ~S, ~S"
		      a b))))))

(defmethod binary-lcm ((a GFp-element) (b GFp-element))
  (with-slots ((d1 domain)) a
    (with-slots ((d2 domain)) b
      (cond ((eq d1 d2) (make-element d1 1))
	    (t (error "Taking the LCM of elements of different fields: ~S, ~S"
		      a b))))))

(defmethod random ((domain GFp) &optional height)
  (declare (ignore height))
  (make-element domain (cl:random (characteristic domain))))

(defmethod height ((x GFp-element))
  (make-element (get-real-numbers) (characteristic (domain-of x))))

(defgeneric multiplicative-order (element)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod multiplicative-order ((a GFp-element))
  (with-slots (value domain) a
    (with-slots ((p  characteristic)) domain
      (cond ((not (eql 1 (cl:gcd value p)))
	     *positive-infinity*)
	    ((let ((group-order (totient p)))
	       (do ((factors (factor group-order)
			     (rest factors))
		    (order group-order))
		   ((null factors)
		    order)
		 (do ((i 0 (cl:1+ i)))
		     ((cl:= i (cdar factors)))
		   (setq order (cl:/ order (caar factors)))
		   (when (not (eql 1 (expt-modulo-integer value order p)))
		     (setq order (cl:* order (caar factors)))
		     (return t))))))))))

;; GF(2^n)
(defvar *GF2-irreducible-polynomials*
  '(#O7 #O13 #O23 #O45 #O103 #O211 #O435 #O1021 #O2011 #O4005 #O10123
    #O20033 #O42103 #O100003 #O210013))

(defmethod make-GFp-domain ((characteristic (eql 2)) (degree integer))
  (cond ((= degree 1)
	 (make-instance 'gfp :characteristic characteristic))
	((< degree (+ (length *GF2-irreducible-polynomials*) 2))
	 (let* ((mask (ash 1 degree))
		(field (1- mask))
		(min-poly (logand (nth (- degree 2) *GF2-irreducible-polynomials*)
				  field))
                domain Z)
           (setq domain
                 (make-instance 'GF2^n 
                                :degree degree
                                :reduction-table
                                (loop for i below degree
                                      for x^n = min-poly then (ash x^n 1)
                                      collect
                                      (if (cl:zerop (logand mask x^n)) x^n
                                          (setq x^n (logxor (logand field x^n) min-poly))))
                                :characteristic characteristic
				:print-function 'GF2^n-print-object))
           (setq Z (get-rational-integers))
           (make-homomorphism Z #'(lambda (x)
                                    (coerce (integer-value x) domain))
                              domain)
           domain))
	(t (error "Table doesn't go far enough: 2^~D" degree))))

(defun GF2^n-print-object (domain stream)
  #+Genera
  (format stream "~'bGF~(2^~D)" (field-degree domain))
  #-Genera
  (format stream "GF(2^~D)" (field-degree domain)))

(defclass GF2^n-element (GFp-element) 
  ())

(defmethod print-object ((elt GF2^n-element) stream)
  (format stream "~V,'0B(2^~D)"
	  (field-degree (domain-of elt)) (GFp-value elt)
          (field-degree (domain-of elt))))

(defmethod make-element ((domain GF2^n) (value integer) &rest ignore)
  (declare (ignore ignore))
  (make-instance 'GF2^N-element
		 :domain domain
		 :value (logand (1- (ash 1 (field-degree domain))) value)))

;; Could have more error checking
(defmethod weyl::make-element ((domain GF2^n) (value integer) &rest ignore)
  (declare (ignore ignore))
  (make-element domain value))

(defmethod coerce ((value integer) (domain GF2^n))
  (make-element domain (if (zerop value) 0 1)))

(defmethod coerce ((value ratio) (domain GF2^n))
  (make-element domain (if (zerop value) 0 1)))

(defmethod multiplicative-order ((a GF2^n-element))
  (let ((group-size (1- (number-of-elements (domain-of a)))))
    (loop for order in (all-divisors group-size)
	  do (when (1? (expt a order))
	       (return order)))))

(defmethod-sd plus ((a GF2^n-element) (b GF2^n-element))
  (make-element domain (logxor (gfp-value a) (gfp-value b))))

(defmethod-sd times ((a GF2^n-element) (b GF2^n-element))
  (let ((x (Gfp-value a))
	(y (GFp-value b))
	(degree (field-degree domain))
	(acc 0) answer)
    (loop while (not (cl:zerop y)) do
          (when (not (cl:zerop (cl:logand 1 y)))
            (setq acc (cl:logxor acc x)))
          (setq x (cl:ash x 1))
          (setq y (cl:ash y -1)))
    (setq answer (cl:logand (cl:1- (cl:ash 1 degree)) acc))
    (loop for hi-bits = (cl:ash acc (cl:- degree))
          then (cl:ash hi-bits -1)
	  for poly in (GFp-reduction-table domain)
	  while (not (cl:zerop hi-bits))
	  do (unless (cl:zerop (cl:logand 1 hi-bits))
	       (setq answer (cl:logxor answer poly))))
    (make-instance 'GF2^N-element :domain domain :value answer)))    

(defmethod expt ((base GF2^n-element) (expt integer))
  (%funcall (repeated-squaring #'times (make-element (domain-of base) 1))
            base expt))

(defmethod recip ((x GF2^n-element))
  (let ((domain (domain-of x)))
    (expt x (cl:- (cl:expt 2 (field-degree domain)) 2))))

(defmethod-sd quotient ((x GF2^n-element) (y GF2^n-element))
  (* x (recip y)))

;; GF(m)

;; This domain is the union of all Z/mZ for all m.

(define-domain-element-classes GFm GFm-element)

(defun make-gfm-domain ()
  (let ((domain (make-instance 'gfm))
	(Z (get-rational-integers)))
    (make-homomorphism Z #'(lambda (x)
			     (make-element domain (integer-value x) 0))
		       domain)
    domain))

(defmethod make-element ((domain GFm) value &rest rest)
  (let ((modulus (first rest)))
    (make-instance 'GFm-element
		   :domain domain
		   :value (reduce-modulo-integer value modulus)
		   :modulus modulus)))

;; Could have more error checking
(defmethod weyl::make-element ((domain GFm) value &rest rest)
  (%apply #'make-element domain value rest))

(defmethod print-object ((x GFm-element) stream)
  (with-slots (value modulus) x
    (format stream "~D(~D)" value modulus)))

(defmethod coerce ((value integer) (domain GFm))
  (make-element domain value 0))

(defmethod coerce ((elt GFp-element) (domain GFm))
  (with-slots ((v1 value) (d1 domain)) elt
    (make-element domain v1 (characteristic d1))))

(defmethod-sd binary= ((x GFm-element) (y GFm-element))
  (with-slots ((v1 value) (m1 modulus) (d1 domain)) x
    (with-slots ((v2 value) (m2 modulus) (d2 domain)) y
      (and (eq d1 d2) (eql v1 v2) (eql m1 m2)))))

(defmethod 0? ((x GFm-element))
  (with-slots (value) x
    (cl:zerop value)))

(defmethod 1? ((x GFm-element))
  (with-slots (value) x
    (eql value 1)))

(defmethod minus ((x GFm-element))
  (with-slots (value modulus domain) x
    (if (eql 2 modulus) x
	(make-element domain (cl:- modulus value) modulus))))

;;; There is no such thing as a negative number in finite fields.
(defmethod minus? ((x GFm-element))
  nil)

(defmethod plus? ((x GFm-element))
  (not (0? x)))

(defmethod-sd plus ((a GFm-element) (b GFm-element))
  (with-slots ((v1 value) (m1 modulus) (d1 domain)) a
    (with-slots ((v2 value) (m2 modulus) (d2 domain)) b
      (cond ((not (eql d1 d2))
	     (error "~S and ~S are not from the same domain" a b))
	    ((eql m1 m2)
	     (make-element d1 (cl:+ v1 v2) m1))
	    (t (make-element d1 (cl:+ v1 v2) (cl:lcm m1 m2)))))))

(defmethod-sd difference ((a GFm-element) (b GFm-element))
  (with-slots ((v1 value) (m1 modulus) (d1 domain)) a
    (with-slots ((v2 value) (m2 modulus) (d2 domain)) b
      (cond ((not (eql d1 d2))
	     (error "~S and ~S are not from the same domain" a b))
	    ((eql m1 m2)
	     (make-element d1 (cl:- v1 v2) m1))
	    (t (make-element d1 (cl:- v1 v2) (cl:lcm m1 m2)))))))

(defmethod-sd times ((a GFm-element) (b GFm-element))
  (with-slots ((v1 value) (m1 modulus) (d1 domain)) a
    (with-slots ((v2 value) (m2 modulus) (d2 domain)) b
      (cond ((not (eql d1 d2))
	     (error "~S and ~S are not from the same domain" a b))
	    ((eql m1 m2)
	     (make-element d1 (cl:* v1 v2) m1))
	    (t (make-element d1 (cl:* v1 v2) (cl:lcm m1 m2)))))))

(defmethod plus ((a GFm-element) (b integer))
  (with-slots ((v1 value) (m1 modulus) (d1 domain)) a
    (make-element d1 (cl:+ v1 (reduce-modulo-integer b m1)) m1)))

(defmethod plus ((a integer) (b GFm-element))
  (with-slots ((v1 value) (m1 modulus) (d1 domain)) b
    (make-element d1 (cl:+ (reduce-modulo-integer a m1) v1) m1)))

(defmethod difference ((a GFm-element) (b integer))
  (with-slots ((v1 value) (m1 modulus) (d1 domain)) a
    (make-element d1 (cl:- v1 (reduce-modulo-integer b m1)) m1)))

(defmethod difference ((a integer) (b GFm-element))
  (with-slots ((v1 value) (m1 modulus) (d1 domain)) b
    (make-element d1 (cl:- (reduce-modulo-integer a m1) v1) m1)))

(defmethod times ((a GFm-element) (b integer))
  (with-slots ((v1 value) (m1 modulus) (d1 domain)) a
    (make-element d1 (cl:* v1 (reduce-modulo-integer b m1)) m1)))

(defmethod times ((a integer) (b GFm-element))
  (with-slots ((v1 value) (m1 modulus) (d1 domain)) b
    (make-element d1 (cl:* (reduce-modulo-integer a m1) v1) m1)))

;;; Takes the inverse of an integer N mod P.  Solve N*X + P*Y = 1.  N
;;; is guaranteed to be less than P, since in the case where P is a
;;; fixnum, N is also assumed to be one.

(defmethod recip ((x GFm-element))
  (with-slots (value modulus domain) x
    (make-element domain (reduce-modulo-integer (compute-inverse value modulus)
						modulus)
		  modulus)))

(defmethod expt ((x GFm-element) (e integer))
  (with-slots (value modulus domain) x
    (cond ((eql 1 value) x)
	  ((cl:minusp e)
	   (error "Raising ~D to a negative power ~D" x e))
	  (t (make-element domain (expt-modulo-integer value e modulus)
			   modulus)))))

(defmethod-sd  quotient ((a GFm-element) (b GFm-element)) 
  (with-slots ((v1 value) (m1 modulus)) a
    (with-slots ((v2 value) (m2 modulus)) b
      (make-element domain (cl:* v1 (compute-inverse v2 m2)) m1))))
  
(defmethod remainder ((a GFm-element) (b GFm-element))
  (error "Computing the remainder of ~D by ~D" a b))

(defmethod binary-gcd ((a GFm-element) (b GFm-element))
  (declare (ignore b))
  (with-slots ((m1 modulus) (d1 domain)) a
    (make-element d1 1 m1)))

(defmethod-sd binary-lcm ((a GFm-element) (b GFm-element))
  (with-slots ((m1 modulus)) a
    (make-element domain 1 m1)))

(defmethod multiplicative-order ((a GFm-element))
  (with-slots (value modulus) a
    (cond ((not (eql 1 (cl:gcd value modulus)))
	   *positive-infinity*)
	  ((let ((group-order (totient modulus)))
	     (do ((factors (factor group-order)
			   (rest factors))
		  (order group-order))
		 ((null factors)
		  order)
	       (do ((i 0 (cl:1+ i)))
		   ((cl:= i (cdar factors)))
		 (setq order (cl:/ order (caar factors)))
		 (when (not (eql 1 (expt-modulo-integer value order modulus)))
		   (setq order (cl:* order (caar factors)))
		   (return t)))))))))

;; These are the guys that actually create the finite fields.
(defun make-finite-field* (size)
  (cond ((null size)
	 (make-gfm-domain))
	((prime? size)
	 (make-GFp-domain size 1))
	(t (let* ((s (factor size))
		  (char (first (first s)))
		  (degree (rest (first s))))
	     (if (null (rest s))
		 (make-Gfp-domain char degree)
		 (error "Finite fields of size ~S=~S don't exist" size s))))))

(defun make-finite-field (&optional size)
  (add-domain #'false (make-finite-field* size)))

;; This is slightly inefficient, but who cares...  I want to localize
;; the knowledge of how to create domains in the MAKE-...* functions.
(defun get-finite-field (&optional size)
  (cond ((null size)
	 (add-domain #'(lambda (d) (eql (class-name (class-of d)) 'GFm))
	   (make-finite-field* size)))
	((prime? size)
	 (add-domain #'(lambda (d)
		         (and (eql (class-name (class-of d)) 'GFp)
			      (eql (characteristic d) size)))
	   (make-finite-field* size)))
        ((null (rest (factor size)))
         (add-domain #'(lambda (d)
                         (and (eql (class-name (class-of d)) 'GF2^n)
                              (eql (cl:expt (characteristic d)
                                            (field-degree d))
                                   size)))
	   (make-finite-field* size)))
	(t (error "Can't do algebraic extensions yet"))))

(defgeneric get-factor-ring (ring ideal)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod get-factor-ring ((ring rational-integers) (ideal ideal))
  (cond ((eql (ring-of ideal) ring)
	 (get-finite-field (first (generators-of ideal))))
	(t (error "Don't know how to compute ~S/~S" ring ideal))))

(defgeneric use-chinese-remainder (remainders)
  (:documentation
   "The purpose of this method is unknown."))

;; Use Chinese remainder theorem to compute the result given a list
;; of remainders which are GFp elements.
(defmethod use-chinese-remainder ((remainders list))
  (let* ((gfm (get-finite-field '()))
	 (x (car remainders))
	 (remainders (cdr remainders))
	 (p1 (if (typep (domain-of x) 'GFp)
		 (coerce (characteristic (domain-of x)) gfm)
		 (coerce (modulus x) gfm)))
	 p1inv p2)
    (setq x (coerce x gfm))
    (loop for k2 in remainders do
          (setq p1inv (recip (with-slots ((v value)) p1
                               (coerce (coerce v (domain-of k2)) gfm))))
          (setq p2 (if (typep (domain-of k2) 'GFp)
                       (coerce (characteristic (domain-of k2)) gfm)
                       (coerce (modulus k2) gfm)))
          (setq k2 (coerce k2 gfm))
          (setq x (make-element gfm
                                (value (+ x (* (* p1inv (- k2 x)) p1)))
                                (value (* p1 p2))))
          (setq p1 (* p1 p2)))
    x))

(defgeneric compute-result (result)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod compute-result ((result GFm-element))
  (with-slots ((v value) (modulus modulus)) result
    (if (> v (floor modulus 2)) (- v modulus) v)))
