;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			      Polynomial Domain Tools
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; poly-tools.lisp,v 1.4 1995/05/24 17:42:08 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.4")

;; Things conditionalized by GEHASH would require hash tables that
;; work with GE-EQUAL.

(defmethod initialize-instance :after
    ((domain variable-hash-table) &rest plist)
  (declare (ignore plist))
  (with-slots (variable-hash-table variable-table variables) domain
    #+GEHASH
    (setq variable-hash-table (make-hash-table :test #'equal))
    (setq variable-table (make-array (list (max (length variables) 1) 2)))
    (setq variable-hash-table 
	  (loop for var in variables
		for i upfrom 0
		collect (list var i)
		do (setf (aref variable-table i 0) var)))))

(defgeneric variable-index (domain variable)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod variable-index ((domain variable-hash-table) (variable symbol))
  (setq variable (coerce variable *general*))
  (loop for (var index) in (variable-hash-table domain)
	do (when (ge-equal variable var)
	     (return index)))
  #+GEHASH
  (gethash variable (variable-hash-table domain)))

(defmethod variable-index
    ((domain variable-hash-table) (variable general-expression))  
  (loop for (var index) in (variable-hash-table domain)
	do (when (ge-equal variable var)
	     (return index)))
  #+GEHASH
  (gethash variable (variable-hash-table domain)))

(defgeneric variable-symbol (domain order)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod variable-symbol ((domain variable-hash-table) (order-number number))
  (aref (variable-index-table domain) order-number 0))

;;(defmethod variable-symbol ((domain variable-hash-table) (poly polynomial))
;;  (aref (variable-index-table domain) (poly-order-number (poly-form poly)) 0))

(defgeneric get-variable-number-property (domain order property)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod get-variable-number-property
    ((domain variable-hash-table) order-number property)
  (%getf (aref (variable-index-table domain) order-number 1) property))

(defgeneric set-variable-number-property (domain order property value)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod set-variable-number-property
    ((domain variable-hash-table) order-number property value)
  (setf (%getf (aref (variable-index-table domain) order-number 1) property)
	value))

(defsetf get-variable-number-property set-variable-number-property)

(defmethod get-variable-property
    ((domain variable-hash-table) variable property)
  (setq variable (coerce variable *general*))
  (get-variable-number-property domain (variable-index domain variable)
				property))

(defmethod set-variable-property
    ((domain variable-hash-table) variable property value)  
  (setq variable (coerce variable *general*))
  (set-variable-number-property domain (variable-index domain variable)
				property value))

;; Defined in general, which is loaded first.
;;(defsetf get-variable-property set-variable-property)

(defgeneric add-new-variable (ring variable)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod add-new-variable ((ring variable-hash-table) variable)
  (with-slots (variables variable-hash-table variable-table) ring
    (let ((vars (different-kernels (coerce variable *general*)
				   variables)))
      (setq vars
	    (loop for var in vars
		  unless (member var variables :test #'ge-equal)
                  collect var))
      (unless (null vars)
	(let* ((count (length variables))
	       (array (make-array (list (cl:+ count (length vars)) 2))))
	  (setq variables (append variables vars))
	  (copy-array-contents variable-table array)
	  (setq variable-table array)
	  #-GEHASH
	  (setq variable-hash-table
		(nconc variable-hash-table
		       (loop for var in vars
			     for cnt upfrom count
			     do (setf (aref variable-table cnt 0) var)
			     collect (list var cnt))))
	  #+GEHASH ;; If we had General expression hash tables
	  (loop for var in vars
		for cnt upfrom count
		do (setf (gethash var variable-table) cnt)))))
    ring))

(defmethod zero ((domain caching-zero-and-one))
  (with-slots (zero) domain
    zero))

(defmethod one ((domain caching-zero-and-one))
  (with-slots (one) domain
    one))
