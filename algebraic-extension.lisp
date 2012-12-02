;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;				  Finite Algebraic Extension
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;; algebraic-extension.lisp,v 1.5 1994/10/04 22:30:39 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.5")

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator factor-ring ((ring ring) (ideal ideal))
    (cond ((eql (ring-of ideal) ring)
           (make-instance 'factor-ring :numerator ring :denominator ideal))
          (t (error "Don't know how to compute ~S/~S" ring ideal)))
    :predicate
    #'(lambda (d)
        (and (typep d 'factor-ring)
             (eql (factor-numer-of d) ring)
             (= (factor-denom-of d) ideal)))))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator algebraic-extension
      ((coefficient-domain ring) variables)
    (progn
      (unless (integral-domain? coefficient-domain)
        (error "Can only create algebraic extensions of integral domains: ~S"
               coefficient-domain))
      (let ((domain
             (make-instance 'algebraic-extension-ring
                            :variables (loop for var in variables
                                             collect (coerce var *general*))
                            :coefficient-domain coefficient-domain)))
        (make-homomorphism coefficient-domain
                           #'(lambda (c) (make-polynomial domain c))
                           domain)
        domain))
    :predicate
    #'(lambda (d) ;; FIXTHIS: the predicate needs to be improved
        (and (typep d 'algebraic-extension-ring)
             (eql (coefficient-domain-of d) coefficient-domain)
             (equal (ring-variables d) variables)))))

;; Use the polynomial print-object method for now

;; This returns the term list for the minimal polynomial of the main
;; variable of the polynomial.  This polynomial is expected to be monic.
(defmacro variable-minimal-polynomial (domain var)
  `(get-variable-number-property ,domain (poly-order-number ,var)
				 :minimal-polynomial))

(defgeneric minimal-polynomial (domain variable)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod minimal-polynomial ((domain algebraic-extension-ring) variable)  
  (with-slots (variables) domain
    (unless (member variable variables :test #'ge-equal)
      (error "~'i~A~ is not a variable of ~S" variable domain)))
  (get-variable-number-property domain (variable-index domain variable)
				:minimal-polynomial))

(defmethod minimal-polynomial
    ((domain algebraic-extension-ring) (variable integer))  
  (get-variable-number-property domain variable :minimal-polynomial))

(defmethod set-minimal-polynomial
	   ((domain algebraic-extension-ring) variable minimal-polynomial)
  (setq variable (coerce variable *general*))
  (with-slots (variables) domain    
    (unless (member variable variables :test #'ge-equal)
      (error "~'i~A~ is not a variable of ~S" variable domain)))
  (unless (eql (domain-of minimal-polynomial) domain)
    (error "The algebraic relation ~S is not an element of ~S" 
	   minimal-polynomial domain))
  (let ((poly-form (poly-form minimal-polynomial))
	(var-index (variable-index domain variable)))
    (unless (= var-index (poly-order-number poly-form))
      (error "~S is not the most main variable of ~S"
	     (with-output-to-string (string)
	       (display variable string))
	       minimal-polynomial))
    (setf (get-variable-number-property domain var-index :minimal-polynomial)
	  (poly-terms poly-form))))

(defsetf minimal-polynomial set-minimal-polynomial)

(defmethod make-polynomial ((domain algebraic-extension-ring) form)
  (make-instance 'algebraic-object :domain domain :form form))

(defmethod-sd times ((x algebraic-object) (y algebraic-object))
  (bind-domain-context domain
    (make-polynomial domain (alg-poly-times (poly-form x) (poly-form y)))))

(defun alg-poly-times (x y)
  (cond ((poly-coef? x)
	 (if (poly-coef? y) (* x y)
	     (poly-simp y (terms-mon-times (poly-terms y) (e0) x))))
	((poly-coef? y)
	 (poly-simp x (terms-mon-times (poly-terms x) (e0) y)))
	((same-variable? x y)
	 (let ((min-poly (minimal-polynomial *domain* (poly-order-number x))))
	   (poly-simp x (if min-poly
			    (terms-pseudo-remainder
			     (terms-times (poly-terms x) (poly-terms y))
			     min-poly)
			    (terms-times (poly-terms x) (poly-terms y))))))
	((more-main? x y)
	 (poly-simp x (terms-mon-times (poly-terms x) (e0) y)))
	(t (poly-simp y (terms-mon-times (poly-terms y) (e0) x)))))

(defmethod expt ((base algebraic-object) (expt integer))
  (let ((domain (domain-of base)))
    (bind-domain-context domain
      (make-polynomial domain
		       (%funcall (repeated-squaring #'alg-poly-times
							(one *coefficient-domain*))
				     (poly-form base) expt)))))
