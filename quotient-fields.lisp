;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;				  Quotient Field Routines
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; quotient-fields.lisp,v 1.10 1995/05/24 17:42:10 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.10")

(defgeneric make-quotient-field (field)
  (:documentation
   "The purpose of this method is unknown.")
  (:method ((ring field)) ring))

(defgeneric get-quotient-field (field)
  (:documentation
   "The purpose of this method is unknown.")
  (:method ((ring field)) ring))

;;; These two methods are actually given in rational-numbers.lisp when
;;; the functions they call are defined.

;;;(defmethod make-quotient-field ((ring rational-integers))
;;;  (make-rational-numbers))

;;;(defmethod get-quotient-field ((ring rational-integers))
;;;  (get-rational-numbers))

(defgeneric make-quotient-element (domain numerator denominator)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod make-quotient-element
    ((domain quotient-field) numerator denominator)
  (make-instance 'quotient-element :domain domain
		 :numerator numerator :denominator denominator))

(define-domain-creator quotient-field ((ring ring))
  (progn
    (unless (gcd-domain? ring)
      (error "Can only create quotient fields of GCD domains: ~S"
             ring))
    (let ((qf (make-instance 'quotient-field :ring ring
			     :print-function 'quotient-field-print-object)))
      (with-slots (zero one) qf
        (setq zero (make-quotient-element qf (zero ring) (one ring)))
        (setq one (make-quotient-element qf (one ring) (one ring))))
      (make-homomorphism ring #'(lambda (x)
				  (make-quotient-element qf x (one ring)))
			 qf)
      qf))
  :predicate #'(lambda (d)
	         (and (typep d 'quotient-field) (eql (qf-ring d) ring))))

(defun quotient-field-print-object (qf stream)
  (with-slots (ring) qf
    (format stream "QF(~S)" ring)))

(defmethod coerce ((qe quotient-element) (d general-expressions))
  (let ((num (coerce (numerator qe) d))
	(den (coerce (denominator qe) d)))
    (setq den (if (number? den) (recip den)
		  (make-ge-expt d den (make-element d -1))))
    (cond ((1? num) den)
	  ((1? den) num)
	  (t (simplify (make-ge-times d (list num den)))))))

(defmethod print-object ((ratfun quotient-element) stream)
  (with-numerator-and-denominator (numerator denominator) ratfun
    (cond ((1? denominator)
	   (prin1 numerator stream))
	  (t (princ "(" stream)
	     (prin1 numerator stream)
	     (princ ")/(" stream)
	     (prin1 denominator stream)
	     (princ ")" stream)))))

(defmethod numerator ((r quotient-element))
  (qo-numerator r))

(defmethod denominator ((r quotient-element))
  (qo-denominator r))

(defmethod zero ((qf quotient-field))
  (with-slots (zero) qf
    zero))

(defmethod one ((qf quotient-field))
  (with-slots (one) qf
    one))

(defmethod 0? ((r quotient-element))
  (with-slots (numerator) r
    (0? numerator)))

(defmethod 1? ((r quotient-element))
  (with-slots (numerator denominator) r
    (and (1? numerator)
	 (1? denominator))))

(defgeneric height (object)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod height ((r quotient-element))
  (max (height (numerator r)) (height (denominator r))))

(defmethod minus ((r quotient-element))
  (let ((domain (domain-of r)))
    (with-numerator-and-denominator (numerator denominator) r
      (make-quotient-element domain (minus numerator) denominator))))

(defgeneric minus? (object)
  (:documentation
   "Return true if the object is negative."))

(defmethod minus? ((r quotient-element))
  (minus? (qo-numerator r)))

(defun quotient-reduce* (qf num &optional den)
  (with-slots (ring) qf
    (when (null den)
      (setq den (one ring))))
  (if (0? num) (zero qf)
      (let ((common-gcd (gcd num den)))
	(unless (1? common-gcd)
	  (setq num (/ num common-gcd)
		den (/ den common-gcd)))
	(when (minus? den)
	  (setq num (minus num)
		den (minus den)))
	(make-quotient-element qf num den))))

(defgeneric quotient-reduce (field numerator &optional denominator)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod quotient-reduce ((qf quotient-field) num &optional den)
  (with-slots (ring) qf
    (when (not (eql (domain-of num) ring))
      (error "The numerator's domain, ~S, is not the ring of the quotient field ~S"
	     (domain-of num) ring))
    (when (not (eql (domain-of den) ring))
      (error "The denominator's domain, ~S, is not the ring of the quotient field ~S"
	     (domain-of den) ring))
    (quotient-reduce* qf num den)))

(defmethod-sd plus ((r1 quotient-element) (r2 quotient-element))
  (with-numerator-and-denominator (n1 d1) r1
    (with-numerator-and-denominator (n2 d2) r2
      (cond ((0? n1) r2)
	    ((0? n2) r1)
	    (t (quotient-reduce* domain
				 (+ (* n1 d2) (* n2 d1))
				 (* d1 d2)))))))

(defmethod-sd difference ((r1 quotient-element) (r2 quotient-element))
  (with-numerator-and-denominator (n1 d1) r1
    (with-numerator-and-denominator (n2 d2) r2
      (cond ((0? n1)
	     (make-quotient-element domain (- n2) d1))
	    ((0? n2) r1)
	    (t (quotient-reduce* domain
				 (- (* n1 d2) (* n2 d1))
				 (* d1 d2)))))))

(defmethod-sd times ((r1 quotient-element) (r2 quotient-element))
  (with-numerator-and-denominator (n1 d1) r1
    (with-numerator-and-denominator (n2 d2) r2
      (let (common-gcd)
	(cond ((and (1? n1) (1? d1))
	       r2)
	      ((and (1? n2) (1? d2))
	       r1)
	      (t (setq common-gcd (gcd n1 d2))
		 (if (not (1? common-gcd))
		     (setq n1 (/ n1 common-gcd)
			   d2 (/ d2 common-gcd)))
		 (setq common-gcd (gcd n2 d1))
		 (if (not (1? common-gcd))
		     (setq n2 (/ n2 common-gcd)
			   d1 (/ d1 common-gcd)))
		 (setq d1 (* d1 d2)
		       n1 (* n1 n2))
		 (if (minus? d1)
		     (setq d1 (minus d1) n1 (minus n1)))
		 (make-quotient-element domain n1 d1)))))))

(defmethod-sd quotient ((r1 quotient-element) (r2 quotient-element))
  (with-numerator-and-denominator (n1 d1) r1
    (with-numerator-and-denominator (n2 d2) r2
      (let (common-gcd)
	(cond ((and (1? n1) (1? d1))
	       (make-quotient-element domain d2 n2))
	      ((and (1? n2) (1? d2))
	       r1)
	      (t (setq common-gcd (gcd n1 n2))
		 (if (not (1? common-gcd))
		     (setq n1 (/ n1 common-gcd)
			   n2 (/ n2 common-gcd)))
		 (setq common-gcd (gcd d1 d2))
		 (if (not (1? common-gcd))
		     (setq d2 (/ d2 common-gcd)
			   d1 (/ d1 common-gcd)))
		 (setq n1 (* n1 d2)
		       d1 (* d1 n2))
		 (if (minus? d1)
		     (setq d1 (minus d1) n1 (minus n1)))
		 (make-quotient-element domain n1 d1)))))))

(defmethod recip ((r1 quotient-element))
  (with-numerator-and-denominator (n1 d1) r1
    (if (minus? n1)
	(setq n1 (minus n1) d1 (minus d1)))
    (make-quotient-element (domain-of r1) d1 n1)))

(defun expt-quotient (domain quo exp)
  (with-numerator-and-denominator (n1 d1) quo
    (if (minus? exp)
	(if (minus? n1)
	    	(make-quotient-element
		  domain (expt (minus d1) (- exp)) (expt (minus n1) (- exp)))
		(make-quotient-element
		  domain (expt d1 (- exp)) (expt n1 (- exp))))
	(make-quotient-element domain (expt n1 exp) (expt d1 exp)))))

(defmethod expt ((r1 quotient-element) (exp integer))
  (expt-quotient (domain-of r1) r1 exp))

(defmethod expt ((r1 quotient-element) (exp rational-integer))
  (expt-quotient (domain-of r1) r1 (integer-value exp)))

(defmethod-sd binary-gcd ((r1 quotient-element) (r2 quotient-element))
  (with-numerator-and-denominator (n1 d1) r1
    (with-numerator-and-denominator (n2 d2) r2
      (make-quotient-element domain (gcd n1 n2) (lcm d1 d2)))))

(defmethod coerce (x (domain quotient-field))
  (let ((temp (coercible? x (qf-ring domain))))
    (if temp (make-quotient-element domain temp (one (qf-ring domain)))
	(call-next-method))))

(defmethod coerce ((x quotient-element) (domain field))
  (let ((num (coercible? (numerator x) domain)))
    (if num (/ num (coerce (denominator x) domain))
        (call-next-method))))
