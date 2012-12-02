;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;				    Vector Space
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; vector.lisp,v 1.11 1995/06/05 20:38:00 rick Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.11")

;; Useful macro until everything is ANSI compliant
(defmacro loop-vector-bind (index vars-vectors &body body)
  (let ((cnt 0) vectors limit var-bindings)
    (setq var-bindings
	  (loop for (var vect) in vars-vectors
		for vector = (intern (format nil ".VV~D." (incf cnt)))
		do (push (list var vector) vectors)
		collect `(,vector ,vect)
		finally (setq vectors (nreverse vectors))))
    (cond ((atom index)
	   (when (null index)
	     (setq index '.I.))
	   (setq limit `(min ,@(loop for (nil vect) in vectors
				     collect `(array-dimension ,vect 0)))))
	  (t (setq limit (second index))
	     (setq index (first index))))	  
    `(let ,var-bindings
       (declare (optimize (safety 1)))
       (loop for ,index fixnum below ,limit
	     ,@(loop for (var vect) in vectors
		     append `(for ,var = (svref ,vect ,index)))
	     do ,@body))))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator free-module ((domain ring) dimension)
    (make-instance 'free-module
                   :coefficient-domain domain
                   :dimension dimension
                   :print-function 'free-module-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'free-module)
                        (eql (coefficient-domain-of d) domain)
                        (eql (dimension-of d) dimension)))))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator vector-space ((domain field) dimension)
    (make-instance 'vector-space
                   :coefficient-domain domain
                   :dimension dimension
                   :print-function 'free-module-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'vector-space)
                        (eql (coefficient-domain-of d) domain)
                        (eql (dimension-of d) dimension)))))

(defun free-module-print-object (domain stream)
  (format stream #+Genera "~S~D" #-Genera "~S^~D"
	  (coefficient-domain-of domain)
	  (dimension-of domain)))

(define-domain-element-classes free-module free-module-element)
(define-domain-element-classes vector-space vector-space-element)

(defmethod make-element ((domain free-module) (value vector) &rest values)
  (declare (ignore values))
  (make-element-free-module-vector domain value))

(defun make-element-free-module-vector (domain value)
  (let ((dim (dimension-of domain)))
    (unless (eql (array-dimension value 0) dim)
      (error "Wrong number of vector elements in ~S" domain))
    (make-instance (first (domain-element-classes domain))
		   :domain domain :values value)))

(defmethod make-element ((domain free-module) value &rest values)
  (let ((dim (dimension-of domain)))
    (unless (eql (1- dim) (length values))
      (error "Wrong number of vector elements in ~S" domain))
    (make-instance (first (domain-element-classes domain))
		   :domain domain
		   :values (%apply #'vector value values))))

(defmethod weyl::make-element ((domain free-module) value &rest values)
  (let ((dim (dimension-of domain))
	(coef-domain (coefficient-domain-of domain)))
    (cond ((typep value 'vector)
	   (unless (and (eql (array-dimension value 0) dim)
			(null values))
	     (error "Wrong number of vector elements in ~S" domain))
	   (make-instance (first (domain-element-classes domain))
	     :domain domain
	     :values (%apply #'vector
			    (loop for i fixnum below (length value)
				  collect (coerce (aref value i) coef-domain)))))
	  (t (unless (eql (1- dim) (length values))
	       (error "Wrong number of vector elements in ~S" domain))
	     (make-instance (first (domain-element-classes domain)) 
	       :domain domain
	       :values (%apply #'vector
			      (coerce value coef-domain)
			      (loop for v in values
				    collect (coerce v coef-domain))))))))

(defmethod print-object ((elt free-module-element) stream)
  (print-free-module-element elt stream))

(defun print-free-module-element (elt stream)
  (let* ((domain (domain-of elt))
	(dim (if (typep domain 'dimensional-domain)
		 (dimension-of (domain-of elt))
		 (array-dimension (tuple-value elt) 0))))
    (write-char #\< stream)
    (unless (0? dim)
      (print-object (ref elt 0) stream)
      (loop for i upfrom 1 below dim
	    do (princ ", " stream)
	       (print-object (ref elt i) stream)))
    (write-char #\> stream)))

(defmethod dimensions ((v vector-space-element))
  (list (dimension-of (domain-of v))))

(defmethod 0? ((v free-module-element))
  (loop for i fixnum below (dimension-of (domain-of v))
	do (unless (0? (ref v i))
	     (return nil))
	finally (return t)))


(defmethod zero ((domain free-module))
  (let ((dim (dimension-of domain))
	(zero (zero (coefficient-domain-of domain))))
    (make-instance (first (domain-element-classes domain))
		   :domain domain
		   :values (make-array dim :initial-element zero))))

(defmethod list-of-variables ((v free-module-element) &optional ignore)
  (declare (ignore ignore))
  (loop for i fixnum below (dimension-of (domain-of v))
	with list
	do (setq list (list-of-variables (ref v i) list))
	finally (return list)))

(defmethod-sd max-pair ((v1 free-module-element) (v2 free-module-element))
  (let* ((dim (dimension-of domain))
	 (ans (make-array dim)))
    (loop-vector-bind (i dim) ((e1 (tuple-value v1)) 
			       (e2 (tuple-value v2)))
      (setf (svref ans i) (max e1 e2)))
    (make-element domain ans)))

(defmethod-sd min-pair ((v1 free-module-element) (v2 free-module-element))
  (let* ((dim (dimension-of domain))
	 (ans (make-array dim)))
    (loop-vector-bind (i dim) ((e1 (tuple-value v1))
			       (e2 (tuple-value v2)))
      (setf (svref ans i) (min e1 e2)))
    (make-element domain ans)))

(defmethod-sd plus ((v1 free-module-element) (v2 free-module-element))
  (let* ((dim (dimension-of domain))
	 (ans (make-array dim)))
    (loop-vector-bind (i dim)
	((e1 (tuple-value v1))
	 (e2 (tuple-value v2)))
      (setf (svref ans i) (+ e1 e2)))
    (make-element domain ans)))

(defmethod minus ((vector free-module-element))
  (let* ((vector-space (domain-of vector))
	 (dim (dimension-of vector-space))
	 (ans (make-array dim)))
    (loop-vector-bind (i dim) ((e (tuple-value vector)))
      (setf (svref ans i) (minus e)))
    (make-element vector-space ans)))

(defmethod-sd difference ((v1 free-module-element) (v2 free-module-element))
  (let* ((dim (dimension-of domain))
	 (ans (make-array dim)))
    (loop-vector-bind (i dim)
	((e1 (tuple-value v1))
	 (e2 (tuple-value v2)))
      (setf (svref ans i) (- e1 e2)))
    (make-element domain ans)))

;; The checks to see if the scalar is a free-module element are
;; necessary because we can multiple two quaternion's together.  There
;; are some complications here.  --RZ 11/2/94

(defmethod times :around (scalar (vector free-module-element))
  (let ((coeff-domain (coefficient-domain-of (domain-of vector)))
	(coerced-scalar nil))
    (cond ((and *coerce-where-possible*
		;; Don't clobber the arg, the next cluase needs it
		(setq coerced-scalar (coercible? scalar coeff-domain)))
	   (multiply-vector-by-scalar vector coerced-scalar))
	  ((typep scalar 'free-module-element)
	   (call-next-method))
	  (t (multiply-vector-by-scalar vector scalar)))))

;; The :around methods for this method and the next are not really
;; needed since they are chosen in preference other similar methods
;; since they have a more specialized first argument.  Nonetheless,
;; I'm leaving the :around's here for symmetry and emphasis.
;;  --RZ 7/12/94
(defmethod times :around ((vector free-module-element) scalar)
  (let ((coeff-domain (coefficient-domain-of (domain-of vector)))
	(coerced-scalar nil))
    (cond ((and *coerce-where-possible*
		;; Don't clobber the arg, the next cluase needs it
		(setq coerced-scalar (coercible? scalar coeff-domain)))
	   (multiply-vector-by-scalar vector coerced-scalar))
	  ((typep scalar 'free-module-element)
	   (call-next-method))
	  (t (multiply-vector-by-scalar vector scalar)))))

(defmethod quotient :around ((vector free-module-element) scalar)
  (let ((coeff-domain (coefficient-domain-of (domain-of vector)))
	(coerced-scalar nil))
    (cond ((and (not (numberp scalar))
		(eql (domain-of scalar) coeff-domain))
	   (multiply-vector-by-scalar vector (/ scalar)))
	  ((and *coerce-where-possible*
		;; Don't clobber the arg, the next cluase needs it
		(setq coerced-scalar (coercible? scalar coeff-domain)))
	   (multiply-vector-by-scalar vector (/ coerced-scalar)))
	  (t (call-next-method scalar vector)))))

(defun multiply-vector-by-scalar (vector scalar)
  (let* ((vector-space (domain-of vector))
	 (dim (dimension-of vector-space))
	 (ans (make-array dim)))
    (loop-vector-bind (i dim) ((e (tuple-value vector)))
      (setf (svref ans i) (* e scalar)))
    (make-element vector-space ans)))

(defmethod-sd dot-product ((v1 free-module-element) (v2 free-module-element))
  (loop for i fixnum upfrom 1 below (dimension-of domain)
	with ans = (* (ref v1 0) (ref v2 0))
	do (setq ans (+ ans (* (ref v1 i) (ref v2 i))))
	finally (return ans)))

(defmethod-sd inner-product ((v1 free-module-element) (v2 free-module-element))
  (dot-product v1 v2))

(defmethod cross-product (v1 v2)
  (error "CROSS-PRODUCT product is not implemented for elements of ~S and ~S"
	 (domain-of v1) (domain-of v2)))

(defmethod cross-product ((v1 free-module-element) (v2 free-module-element))
  (let ((domain (domain-of v1))
	a b)
    (cond ((and (eql domain (domain-of v2))
		(= 3 (dimension-of domain)))
	   (setq a (tuple-value v1)
		 b (tuple-value v2))
	   (make-element domain 
			 (- (* (aref a 1) (aref b 2))
			    (* (aref a 2) (aref b 1)))
			 (- (* (aref a 2) (aref b 0))
			    (* (aref a 0) (aref b 2)))
			 (- (* (aref a 0) (aref b 1))
			    (* (aref a 1) (aref b 0)))))
	  (t (call-next-method)))))

(defmethod tilde (vect)
  (error "TILDE is not implemented for elements of ~S" (domain-of vect)))

(defmethod tilde ((vect free-module-element))
  (cond ((= 3 (dimension-of (domain-of vect)))
	 (let ((matrix-space (get-matrix-space
			      (coefficient-domain-of (domain-of vect))))
	       (v1 (ref vect 0))
	       (v2 (ref vect 1))
	       (v3 (ref vect 2))
	       (zero (zero (coefficient-domain-of (domain-of vect)))))
	   (make-element matrix-space 
			 (make-array (list 3 3) 
				     :initial-contents
				     `((,zero ,(- v3) ,v2) 
				       (,v3   ,zero   ,(- v1))
				       (,(- v2) ,v1   ,zero))))))
	(t (call-next-method))))

(defmethod derivation ((vector free-module-element))
  (let* ((vector-space (domain-of vector))
	 (coef-domain (coefficient-domain-of vector-space)))
    (cond ((member 'deriv (list-operations coef-domain))
	   (let* ((vector-space (domain-of vector))
		  (dim (dimension-of vector-space))
		  (ans (make-array dim)))
	     (loop-vector-bind (i dim) ((e (tuple-value vector)))
	       (setf (svref ans i) (deriv e)))
	     (make-element vector-space ans)))
	  (t (error "Derivation is not a legal operation for domain ~S~%"
		    vector-space)))))

(defmethod deriv ((vector free-module-element) &rest vars)
  (let* ((vector-space (domain-of vector))
	 (dim (dimension-of vector-space))
	 (ans (make-array dim)))
    (loop-vector-bind (i dim) ((e (tuple-value vector)))
      (setf (svref ans i) (%apply #'deriv e vars)))
    (make-element vector-space ans)))

;; v1 - v2 has no negative components
(defmethod-sd dominates ((v1 lisp-vector) (v2 lisp-vector))
  (loop	with dimension = (dimension-of domain)
        and vect1 = (tuple-value v1) and vect2 = (tuple-value v2)
        for i below dimension
	when (< (aref vect1 i) (aref vect2 i))
	  do (return nil)
        finally (return t)))

(defmethod-sd disjoint ((v1 lisp-vector) (v2 lisp-vector))
  (loop	with dimension = (dimension-of domain)
        and vect1 = (tuple-value v1) and vect2 = (tuple-value v2)
        for i below dimension
	when (not (or (0? (aref vect2 i)) (0? (aref vect1 i))))
	 do (return nil)
        finally (return t)))

(defmethod substitute ((values list) (variables list) (v free-module-element)
		       &rest ignore)
  (declare (ignore ignore))
  (let* ((dim (dimension-of (domain-of v)))
	 (ans (make-array dim)))
    (loop-vector-bind (i dim) ((e (tuple-value v)))
      (setf (svref ans i) (substitute values variables e)))
    (make-element (domain-of v) ans)))


;; Should the absolute value of a vector be defined?  The phase?

(defmethod conjugate ((vector free-module-element))
  (let* ((vector-space (domain-of vector))
	 (dim (dimension-of vector-space))
	 (ans (make-array dim)))
    (loop-vector-bind (i dim)
      ((e (tuple-value vector)))
      (setf (aref ans i) (conjugate e)))
    (make-element vector-space ans)))

(defmethod realpart ((vector free-module-element))
  (let* ((vector-space (domain-of vector))
	 (dim (dimension-of vector-space))
	 (ans (make-array dim)))
    (loop-vector-bind (i dim)
      ((e (tuple-value vector)))
      (setf (aref ans i) (realpart e)))
    (make-element vector-space ans)))

(defmethod imagpart ((vector free-module-element))
  (let* ((vector-space (domain-of vector))
	 (dim (dimension-of vector-space))
	 (ans (make-array dim)))
    (loop-vector-bind (i dim)
      ((e (tuple-value vector)))
      (setf (aref ans i) (imagpart e)))
    (make-element vector-space ans)))


;;; the dimension of a vector is the dimension of it's domain
(defmethod dimension-of ((v vector-space-element))
  (dimension-of (domain-of v)))
