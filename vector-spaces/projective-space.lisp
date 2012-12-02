;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;				  Projective Spaces
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; projective-space.lisp,v 1.5 1995/05/24 17:42:09 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.5")

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator projective-space ((domain field) dimension)
    (make-instance 'projective-space
                   :coefficient-domain domain
                   :dimension dimension
                   :print-function 'projective-space-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'projective-space)
                        (eql (coefficient-domain-of d) domain)
                        (eql (dimension-of d) dimension)))))

(defun projective-space-print-object (domain stream)
  (format stream #+Genera "P~D(~S)" #-Genera "P^~D(~S)"
	  (dimension-of domain)
	  (coefficient-domain-of domain)))

(defun make-projective-space-element (domain value)
  (make-instance 'projective-space-element :domain domain :value value))

(defmethod make-element ((domain projective-space) value &rest values)
  (let ((dim (dimension-of domain))
	(num-values (1+ (length values)))
	(coeff-domain (coefficient-domain-of domain))
	array)
    (setq values (cons value values))
    (cond ((eql dim num-values)
	   (setq array (make-array (1+ dim)))
	   (loop for i below dim
		 for v in values do
	     (setf (aref array i) (coerce v coeff-domain)))
	   (setf (aref array dim) (one coeff-domain))
	   (make-projective-space-element domain array))
	  ((eql dim (1- num-values))
	   (setq array (make-array (1+ dim)))
	   (loop for i below dim
		 for v in values 
		 with denom = (coerce (first (last values)) coeff-domain) do
	     (setf (aref array i) (/ (coerce v coeff-domain) denom)))
	   (setf (aref array dim) (one coeff-domain))
	   (make-projective-space-element domain array))
	  (t (error "Wrong number of vector elements in ~S" domain)))))

(defmethod ref ((vect projective-space-element) &rest args)
  (aref (tuple-value vect) (first args)))

(defmethod vector-set-ref
    ((vect projective-space-element) new-value &rest args)
  (setf (aref (tuple-value vect) (first args)) new-value))

;; Create an affine space corresponding to 
(defmethod make-affine-space ((space projective-space) &optional n)
  (let* ((dim (dimension-of space))
	 (range-space (make-vector-space (coefficient-domain-of space) dim))
	 homo)
    (when (null n)
      (setf n dim))
    (labels ((project (vector)
	       (loop with denom = (ref vector n)
		     for i below (1+ dim)
		     unless (= i n)
		       collect (/ (ref vector i) denom)))
	     (map-fun (vector)
	       (%apply #'make-element range-space (project vector))))
      (setq homo (make-morphism space #'map-fun range-space))
      (values (morphism-range homo)
	      (morphism-map homo)))))

