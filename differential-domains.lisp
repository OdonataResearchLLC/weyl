;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			     Differential Rings
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; differential-domains.lisp,v 1.7 1995/05/24 17:41:58 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.7")

(defmethod ring-variables ((domain differential-polynomial-ring))
  (with-slots ((vars variables)) domain
    (loop for v in vars
	  when (or (atom v) (not (eql (first v) 'derivation)))
	    collect v)))

(defsetf variable-derivation set-variable-derivation)

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator differential-ring ((coefficient-domain ring) variables) 
    (progn
      (setq variables (loop for var in variables
                            collect (coerce var *general*)))
      (let ((ring (make-instance 'differential-polynomial-ring 
                                 :variables variables
                                 :coefficient-domain coefficient-domain
                                 :print-function 'differential-ring-print-object)))
        (loop for var in variables do
              (setf (variable-derivation ring var) :generate))
        ring))
    :predicate 
    #'(lambda (d)  
        (and (eql (class-name (class-of d)) 'differential-polynomial-ring) 
             (eql (coefficient-domain-of d) coefficient-domain)
             (eql (ring-variables d) variables)
             ;; And check that the derivations are the same.
             )))) 

(defun differential-ring-print-object (d stream)
  (format stream "~A<" (coefficient-domain-of d))
  (display-list (ring-variables d))
  (princ ">" stream))

(defmethod coerce ((variable list) (domain differential-polynomial-ring))
  (cond ((member variable (ring-variables domain))
	 (make-polynomial domain
	    (cons (variable-index domain variable)
		  (make-terms 1 (one (coefficient-domain-of domain))))))
	((and (not (atom variable))
	      (eql (first variable) 'deriv))
	 (loop for i below (third variable)
	       for p = (deriv (coerce (second variable) domain)) then (deriv p)
	       finally (return p)))
	((coercible? variable (coefficient-domain-of domain)))
	(t (call-next-method))))

;; Derivations are more complex than differentation.
;; This returns the derivation of the main variable of the polynomial.
;; In general this polynomial is expected to be of degree 1 with
;; coefficient 1.
(defmacro variable-derivation (domain var)
  `(get-variable-number-property ,domain (poly-order-number ,var)
				 :derivation))

(defmacro variable-derivative-order (domain var)
  `(get-variable-number-property ,domain (poly-order-number ,var)
				 :derivative-order))

(defgeneric set-variable-derivation (domain variable derivation)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod set-variable-derivation
    ((domain differential-polynomial-ring)
     (variable symbol) derivation)
  (setq variable (coerce variable *general*))
  (with-slots (variables) domain
    (unless (member variable variables :test #'ge-equal)
      #+Genera
      (error "~'i~A~ is not a variable of ~S" variable domain)
      #-Genera      
      (error "~A is not a variable of ~S" variable domain)))
  (cond ((eql derivation :generate)
	 (setf (get-variable-number-property domain
					     (variable-index domain variable)
					     :derivation)
	       :generate))
	(t (cond ((eql (domain-of derivation) *general*)
		  (setq derivation (coerce derivation domain)))
		 ((not (eql (domain-of derivation) domain))
		  (error "The derivation ~S is not an element of ~S" 
			 derivation domain)))
	   (setf (get-variable-number-property domain 
					       (variable-index domain variable)
					       :derivation)
		 (poly-form derivation)))))

(defmethod set-variable-derivation
    ((domain differential-polynomial-ring)
     (variable general-expression) derivation)
  (setq variable (coerce variable *general*))
  (with-slots (variables) domain
    (unless (member variable variables :test #'ge-equal)
      #+Genera
      (error "~'i~A~ is not a variable of ~S" variable domain)
      #-Genera      
      (error "~A is not a variable of ~S" variable domain)))
  (cond ((eql derivation :generate)
	 (setf (get-variable-number-property domain
					     (variable-index domain variable)
					     :derivation)
	       :generate))
	(t (cond ((eql (domain-of derivation) *general*)
		  (setq derivation (coerce derivation domain)))
		 ((not (eql (domain-of derivation) domain))
		  (error "The derivation ~S is not an element of ~S" 
			 derivation domain)))
	   (setf (get-variable-number-property domain 
					       (variable-index domain variable)
					       :derivation)
		 (poly-form derivation)))))

(defmethod add-new-variable ((domain differential-ring) variable)
  (prog1
    (call-next-method)
    (setq variable (coerce variable *general*))
    (setf (variable-derivation domain variable) :generate)))

(defun standard-derivation (p)
  (let ((deriv (variable-derivation *domain* p)))
    (cond ((null deriv) (zero *coefficient-domain*))
	  ((eql deriv :generate)
	   (let* ((old-var (variable-symbol *domain* (poly-order-number p)))
		  (new-order
		   (cond ((ge-variable? old-var) 1)
			 ((eql (first old-var) 'derivation)
			  (1+ (third old-var)))
			 (t 1)))
		  (new-var `(derivation
			     ,(if (or (ge-variable? old-var)
				      (not (eql (first old-var) 'derivation)))
				  old-var
				  (second old-var))
			     ,new-order))
		  new-var-num)
	     (add-new-variable *domain* new-var)
	     (setq new-var-num (variable-index *domain* new-var))
	     (setf (variable-derivation *domain* old-var) new-var)
	     #+ignore
	     (setf (variable-derivative-order *domain* new-var) new-order)
	     (cons new-var-num (make-terms 1 (one *coefficient-domain*)))))
 	  (t deriv))))

(defun poly-derivation (p &optional (derivation #'standard-derivation))
  (let ((deriv nil) (temp nil))
    (cond ((poly-coef? p) (zero *coefficient-domain*))
	  (t (setq deriv (%funcall derivation p))
	     (poly-plus
	       (if (poly-0? deriv) deriv
		   (poly-times
		     (make-poly-form
		       p
		       (map-over-each-term (poly-terms p) (e c)
			 (unless (e0? e)
			   (unless (poly-0?
				     (setq temp
					   (poly-times
					    (coerce e *coefficient-domain*)
					    c)))
			     (collect-term (e1- e) temp)))))
		     deriv))
	       (poly-differentiate-coefs p derivation))))))

(defun poly-differentiate-coefs (p derivation)
  (let* ((dc nil)
	 (one (one *coefficient-domain*))
	 (terms (poly-terms p))
	 (sum (poly-times (make-poly-form p (make-terms (le terms) one))
			  (poly-derivation (lc terms) derivation))))
    (map-over-each-term (red terms) (e c)
      (setq dc (poly-derivation c derivation))
      (setq sum (poly-plus sum 
			   (poly-times dc
				       (make-poly-form p 
						       (make-terms e one))))))
    sum))

(defmethod derivation ((poly polynomial))
  (let ((domain (domain-of poly)))
    (unless (typep domain 'differential-ring)
      (error "No derivation operator for ~S" domain))
    (bind-domain-context domain
      (make-polynomial domain (poly-derivation (poly-form poly))))))

(defmethod derivation ((rat rational-function))
  (let ((domain (domain-of rat)))    
    (unless (typep (qf-ring domain) 'differential-ring)
      (error "No derivation operator for ~S" domain))
    (with-numerator-and-denominator (n d) rat
      (bind-domain-context (qf-ring domain)
	(ratfun-reduce domain
		       (poly-difference
			(poly-times (poly-derivation n) d)
			(poly-times (poly-derivation d) n))
		       (poly-times d d))))))
