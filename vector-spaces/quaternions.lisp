;;; -*- Mode:Lisp; Package: WEYLI; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;				  Quaternions
;;;
;;;
;;; based on "Applications of quaternions to computations with rotation",
;;;   Eugene Salamin.
;;; Contains quaternions over a field and unit quaternions.
;;;
;;; Not yet implemented: homomorphism between unit quaternions and SO(3)
;;;
;;; ===========================================================================

;;; (c) Copyright 1989, 1993 Cornell University

;;; quaternions.lisp,v 1.9 1995/05/24 17:42:09 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.9")

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator quaternion-domain ((domain field))
    (make-instance 'quaternion-domain
                   :coefficient-domain domain
                   :dimension 4
                   :print-function 'quaternion-domain-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'quaternion-domain)
                        (eql (coefficient-domain-of d) domain)))))

(defun quaternion-domain-print-object (domain stream)
  (format stream "Quat(~S)" (coefficient-domain-of domain)))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator unit-quaternion-domain ((domain field))
    (make-instance 'unit-quaternion-domain
                   :coefficient-domain domain
                   :dimension 4
                   :print-function 'unit-quaternion-domain-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'unit-quaternion-domain)
                        (eql (coefficient-domain-of d) domain)))))

(defun unit-quaternion-domain-print-object (domain stream)
  (format stream "UQuat(~S)" (coefficient-domain-of domain)))


;;; Quaternion elements themselves


(define-domain-element-classes quaternion-domain
    quaternion-domain-element)

(define-domain-element-classes unit-quaternion-domain
    unit-quaternion-domain-element)

(defmethod make-element ((domain quaternion-domain) (value vector)
			 &rest values)
  (unless (and (eql (array-dimension value 0) 4)
	       (null values))
    (error "Wrong number of vector elements in ~S" domain))
  (make-instance (first (domain-element-classes domain))
		 :domain domain :values value))

(defmethod weyl::make-element ((domain quaternion-domain) (value vector)
			       &rest values)
  (unless (and (eql (array-dimension value 0) 4)
	       (null values))
    (error "Wrong number of vector elements in ~S" domain))
  (let ((coef-domain (coefficient-domain-of domain))
	(vector (make-array 4)))
    (loop for i below 4 do
      (setf (aref vector i) (coerce (aref value i) coef-domain)))
    (make-instance (first (domain-element-classes domain))
		   :domain domain :values vector)))

(defmethod make-element ((domain quaternion-domain) value &rest values)
  (unless (eql 3 (length values))
    (error "Wrong number of vector elements in ~S" domain))
  (make-instance (first (domain-element-classes domain))
		 :domain domain
		 :values (%apply #'vector value values)))

(defmethod weyl::make-element ((domain quaternion-domain) value &rest values)
  (unless (eql 3 (length values))
    (error "Wrong number of vector elements in ~S" domain))
  (let ((coef-domain (coefficient-domain-of domain))
	(vector (make-array 4)))
    (setf (aref vector 0) (coerce value coef-domain))
    (setf (aref vector 1) (coerce (first values) coef-domain))
    (setf (aref vector 2) (coerce (second values) coef-domain))
    (setf (aref vector 3) (coerce (third values) coef-domain))
    (make-instance (first (domain-element-classes domain))
		   :domain domain
		   :values vector)))

(defmethod make-element ((domain unit-quaternion-domain) (value vector)
			 &rest values)  
  (unless (and (eql (array-dimension value 0) 4)
	       (null values))
    (error "Wrong number of vector elements in ~S" domain))
  (make-instance (first (domain-element-classes domain))
		 :domain domain :values value))

;; FIXTHIS: Should check to make sure that quaternion is a unit
(defmethod weyl::make-element ((domain unit-quaternion-domain) (value vector)
			       &rest values)  
  (unless (and (eql (array-dimension value 0) 4)
	       (null values))
    (error "Wrong number of vector elements in ~S" domain))
  (let ((coef-domain (coefficient-domain-of domain))
	(vector (make-array 4)))
    (loop for i below 4 do
      (setf (aref vector i) (coerce (aref value i) coef-domain)))
    (make-instance (first (domain-element-classes domain))
		   :domain domain :values vector)))

(defmethod make-element ((domain unit-quaternion-domain) value &rest values)
  (unless (eql 3 (length values))
    (error "Wrong number of vector elements in ~S" domain))
  (make-instance (first (domain-element-classes domain))
		 :domain domain
		 :values (%apply #'vector value values)))

;; FIXTHIS: Should check to make sure that quaternion is a unit
(defmethod weyl::make-element ((domain unit-quaternion-domain) value
			       &rest values)
  (unless (eql 3 (length values))
    (error "Wrong number of vector elements in ~S" domain))
  (let ((coef-domain (coefficient-domain-of domain))
	(vector (make-array 4)))
    (setf (aref vector 0) (coerce value coef-domain))
    (setf (aref vector 1) (coerce (first values) coef-domain))
    (setf (aref vector 2) (coerce (second values) coef-domain))
    (setf (aref vector 3) (coerce (third values) coef-domain))
    (make-instance (first (domain-element-classes domain))
		   :domain domain
		   :values vector)))

(defmethod conjugate ((q quaternion-with-multiplication))
  (let ((value (tuple-value q)))
    (make-element (domain-of q)
      (aref value 0) (- (aref value 1))
      (- (aref value 2)) (- (aref value 3)))))

(defmethod-sd dot-product
    ((q1 quaternion-with-multiplication) (q2 quaternion-with-multiplication))
  (loop for i upfrom 1 below 4
	with ans = (* (ref q1 0) (ref q2 0))
	do (setq ans (+ ans (* (ref q1 i) (ref q2 i))))
	finally (return ans)))

(defmethod-sd times
  ((p quaternion-with-multiplication) (q quaternion-with-multiplication))
  (let* ((pp (tuple-value p))
	 (p0 (aref pp 0))
	 (p1 (aref pp 1))
	 (p2 (aref pp 2))
	 (p3 (aref pp 3))
	 (qq (tuple-value q))
	 (q0 (aref qq 0))
	 (q1 (aref qq 1))
	 (q2 (aref qq 2))
	 (q3 (aref qq 3))) 
    (make-element domain
		  (- (* p0 q0) (+ (+ (* p1 q1) (* p2 q2)) (* p3 q3)))
		  (- (+ (+ (* p1 q0) (* p0 q1)) (* p2 q3)) (* p3 q2))
		  (- (+ (+ (* p2 q0) (* p0 q2)) (* p3 q1)) (* p1 q3))
		  (- (+ (+ (* p3 q0) (* p0 q3)) (* p1 q2)) (* p2 q1)))))
	     
#|  For metricized fields|

(defmethod norm ((q quaternion-domain-element))
  (sqrt (dot-product q q)))

(defmethod normalize ((q quaternion-domain-element))
  (let ((l (norm q))
	(v (tuple-value q)))
    (make-quaternion-domain-element 
     (domain-of q)
     (make-array 4 :initial-contents (list (/ (aref v 0) l)
					   (/ (aref v 1) l)
					   (/ (aref v 2) l)
					   (/ (aref v 3) l))))))
||#

(defmethod create-unit-quaternion
  ((domain unit-quaternion-domain) (v vector-space-element) (angle number))
  (unless (= 3 (dimension-of (domain-of v)))
	  (error "Illegal call to create-unit-quaternion: ~S" v))
  ;; must coerce domains
  (make-element domain
		(cos (/ angle 2)) 
		(* (sin (/ angle 2)) (ref v 0))
		(* (sin (/ angle 2)) (ref v 1))
		(* (sin (/ angle 2)) (ref v 2))))

;;
;; homomorphism SO(3) --> Unit Quaternions
;;

(defmethod coerce ((Q unit-quaternion-domain-element) (domain SO-n))
  (if (eql (dimension-of domain) 3)
      (let* ((q0 (ref Q 0))
	     (q1 (ref Q 1))
	     (q2 (ref Q 2))
	     (q3 (ref Q 3))
	     (q0q0 (* q0 q0))
	     (q0q1 (* q0 q1))
	     (q0q2 (* q0 q2))
	     (q0q3 (* q0 q3))
	     (q1q1 (* q1 q1))
	     (q1q2 (* q1 q2))		;
	     (q1q3 (* q1 q3))
	     (q2q2 (* q2 q2))
	     (q2q3 (* q2 q3))
	     (q3q3 (* q3 q3))
	     (mat (make-array '(3 3))))
	(setf (aref mat 0 0) (+ (* 2 q0q0) (* 2 q1q1) -1))
	(setf (aref mat 1 1) (+ (* 2 q0q0) (* 2 q2q2) -1))
	(setf (aref mat 2 2) (+ (* 2 q0q0) (* 2 q3q3) -1))
	(setf (aref mat 0 1) (* 2 (- q1q2 q0q3)))
	(setf (aref mat 0 2) (* 2 (+ q1q3 q0q2)))
	(setf (aref mat 1 2) (* 2 (- q2q3 q0q1)))
	(setf (aref mat 1 0) (* 2 (+ q1q2 q0q3)))
	(setf (aref mat 2 0) (* 2 (- q1q3 q0q2)))
	(setf (aref mat 2 1) (* 2 (+ q0q1 q2q3)))
	(make-element domain mat))
      (error "Cannot coerce a quaternion in SO(~D)" (dimension-of domain))))

