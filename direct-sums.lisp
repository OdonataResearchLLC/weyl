;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;				  Direct Sums
;;; ===========================================================================
;;; (c) Copyright 1991,1993 Cornell University

;;; direct-sums.lisp,v 1.4 1995/05/24 17:41:59 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.4")

(defmethod dimension-of ((domain direct-sum))
  (length (tuple-value domain)))

(defmethod initialize-instance :after ((domain direct-sum) &rest plist)
  (declare (ignore plist))
  (with-slots (print-function) domain
    (setf print-function 'direct-sum-print-object)))

(defun direct-sum-print-object (domain stream)
  (%apply #'format stream "~S~@{ (+) ~S~}"
	  (loop with v = (tuple-value domain)
		for i below (array-dimension v 0)
		collect (aref v i))))

(defgeneric %make-direct-sum (domain1 domain2)
  (:documentation
   "The purpose of this method is unknown."))

(defmacro define-direct-sum (domain-name classes
			     &optional other-domain-classes other-elt-classes)
  (let ((ds-domain (intern (format nil "DIRECT-SUM-~A" domain-name)))
	(ds-domain-elt (intern (format nil "DIRECT-SUM-~A-ELT" domain-name))))
    `(progn
      (defclass ,ds-domain 
          (,@(loop for name in classes 
                   collect (intern (format nil "DIRECT-SUM-~A" name)))
             ,domain-name ,@other-domain-classes direct-sum) ())
      (defclass ,ds-domain-elt 
          (,@(loop for name in classes 
                   collect (intern (format nil "DIRECT-SUM-~A-ELT" name))) 
             ,@other-elt-classes direct-sum-element) ())
      (define-domain-element-classes ,ds-domain ,ds-domain-elt)
      (defmethod %make-direct-sum ((a ,domain-name) (b ,domain-name))
        (%make-direct-sum-internal ',ds-domain a b))
      (defmethod make-element ((domain ,ds-domain) elt1 &rest elts)
        (%apply #'make-instance ',ds-domain-elt 
                :domain domain
                :values (cons elt1 elts)))
      (defmethod weyl::make-element ((domain ,ds-domain) elt1 &rest elts)
        (let* ((domains (tuple-value domain))
               (len (array-dimension domains 0)))
          (unless (cl:= (1- len) (length elts))
            (error "Incorrect number of elements to MAKE-ELMENT ~A" domain))
          (make-instance ',ds-domain-elt
                         :domain domain
                         :values (cons (coerce elt1 (aref domains 0))
                                       (loop for i upfrom 1 below len
                                             for elt in elts
                                             collect (coerce elt (aref domains i))))))))))

(defun make-direct-sum* (domain1 &rest domains)
  (when (null domains) 
    (error "Illegal number of arguments to MAKE-DIRECT-SUM"))
  (labels ((iterate (values)
	     (cond ((null (rest values))
		    (first values))
		   (t (%make-direct-sum (first values)
					(iterate (rest values)))))))
    (let ((domain (iterate (cons domain1 domains)))
	  (Z (get-rational-integers)))
      (make-homomorphism Z #'(lambda (x)			     
			       (map-with-domain
                                (first (domain-element-classes domain))
                                domain
                                #'(lambda (d) (coerce x d)) domain))
			 domain)
      domain)))

(defun make-direct-sum (domain1 &rest domains)
  (add-domain #'false 
    (%apply #'make-direct-sum* domain1 domains)))

(defun %make-direct-sum-internal (type a b)
  (flet ((domain-list (a)
	   (loop for i below (dimension-of a)
		 collect (ref a i))))
    (cond ((typep a 'direct-sum)
	   (if (typep b 'direct-sum)
	       (make-instance type :values (nconc (domain-list a) (domain-list b)))
	       (make-instance type :values (nconc (domain-list a) (list b)))))
	  ((typep b 'direct-sum)
	   (make-instance type :values (cons a (domain-list b))))
	  (t (make-instance type :values (list a b))))))

(defun get-direct-sum (domain1 &rest domains)
  (add-domain #'(lambda (d) 
		  (and (typep d 'direct-sum)
		       (eql domain1 (ref d 0))
		       (cl:= (1- (dimension-of d)) (length domains))
		       (loop for i below (dimension-of d)
			     for dom in domains
			     when (not (eql (ref d i) dom))
                             do (return nil)
			     finally (return t))))
    (%apply #'make-direct-sum* domain1 domains)))

(defmethod print-object ((domain direct-sum-element) stream)
  (%apply #'format stream "~S~@{ (+) ~S~}"
	  (loop with v = (tuple-value domain)
		for i below (array-dimension v 0)
		collect (aref v i))))

(define-direct-sum semigroup ())

(defmethod-sd times ((a direct-sum-semigroup-elt) (b direct-sum-semigroup-elt))
  (map-with-domain 'direct-sum-semigroup-elt domain #'times a b))

(define-direct-sum monoid (semigroup))

(defmethod one ((domain direct-sum-monoid))
  (map 'direct-sum-monoid-elt #'one domain))

(defmethod 0? ((x direct-sum-monoid-elt))
  (let ((v (tuple-value x)))
    (loop for i below (array-dimension v 0)
	  when (not (0? (aref v i)))
          do (return nil)
	  finally (return t))))

(define-direct-sum group (monoid))

(defmethod-sd quotient ((a direct-sum-group-elt) (b direct-sum-group-elt))
  (map-with-domain 'direct-sum-semigroup-elt domain #'quotient a b))

(defmethod recip ((a direct-sum-group-elt))
  (map-with-domain 'direct-sum-semigroup-elt (domain-of a) #'recip a))

(define-direct-sum abelian-semigroup ())

(defmethod-sd plus ((a direct-sum-semigroup-elt) (b direct-sum-semigroup-elt))
  (map-with-domain 'direct-sum-semigroup-elt domain #'plus a b))

(define-direct-sum abelian-monoid (abelian-semigroup))

(defmethod zero ((domain direct-sum-monoid))
  (map 'direct-sum-monoid-elt #'zero domain))

(defmethod 1? ((x direct-sum-monoid-elt))
  (let ((v (tuple-value x)))
    (loop for i below (array-dimension v 0)
	  when (not (1? (aref v i)))
          do (return nil)
	  finally (return t))))

(define-direct-sum abelian-group (abelian-monoid))

(defmethod-sd difference ((a direct-sum-abelian-group-elt) (b direct-sum-abelian-group-elt))
  (map-with-domain 'direct-sum-semigroup-elt domain #'difference a b))

(defmethod minus ((a direct-sum-abelian-group-elt))
  (map-with-domain 'direct-sum-semigroup-elt (domain-of a) #'minus a))

(define-direct-sum module (abelian-group) (has-coefficient-domain))

(define-direct-sum algebra (module semigroup))

(define-direct-sum ring (algebra monoid))
