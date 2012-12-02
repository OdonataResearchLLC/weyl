;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;		       Taylor Expansions
;;; ===========================================================================
;;; (c) Copyright 1994 Cornell University

 
(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.7")

(defmacro define-taylor-expansion-fun (name (domain order) &body body)
  (let ((maker-name (intern (format nil "TAYLOR-~A" name))))
       `(progn
	  (defun ,maker-name (,domain ,order) ,@body)
	  (setf (getf (get-function nil ',name) 'taylor-expansion-fun)
		',maker-name))))

(define-taylor-expansion-fun SIN (domain order)
  (let ((coeffs))
       (loop for i from 1 to order
	     if (evenp i) do (setq coeffs (append coeffs (list 0)))
	       else do (setq coeffs
			     (append coeffs
				     (list (cl:* (cl:expt -1 (cl:/ (cl:- i 1) 2))
						 (cl:/ 1 (factorial i))))))
	     finally
	  (return (make-tpower-series domain coeffs :valence 1 :order order)))))

(define-taylor-expansion-fun COS (domain order)
  (let ((coeffs))
       (loop for i from 0 to order       
	     if (oddp i) do (setq coeffs (append coeffs (list 0)))
	       else do (setq coeffs (append coeffs
					    (list (cl:* (cl:expt -1 (cl:/ i 2))
							(cl:/ 1 (factorial i))))))
	     finally
	  (return (make-tpower-series domain coeffs :order order)))))
	    
(define-taylor-expansion-fun TAN (domain order)
  (taylor (/ (sin (ring-variables domain))
	     (cos (ring-variables domain)))
	  domain order))

(define-taylor-expansion-fun LOG (domain order)
  (let ((coeffs))
       (loop for i from 1 to order       
	     do (setq coeffs (append coeffs
				     (list (cl:* (cl:expt -1 (cl:- i 1))
						 (cl:/ 1 i)))))
	     finally
	  (return (make-tpower-series domain coeffs :valence 1 :order order)))))

(define-taylor-expansion-fun ASIN (domain order)
  (let ((coeffs))
       (loop for i from 1 to order              
	     if (evenp i) do (setq coeffs (append coeffs (list 0)))
	       else do (let ((temp-coeff 1))
			    (loop for j from 1 below i
				  if (oddp j) do
				    (setq temp-coeff (cl:* temp-coeff j))
				  else do
				    (setq temp-coeff (cl:/ temp-coeff j))
				  finally
			       (setq coeffs (append coeffs (list (cl:* temp-coeff
								    (cl:/ 1 i)))))))
	     finally
	  (return (make-tpower-series domain coeffs :valence 1 :order order)))))
		     
(define-taylor-expansion-fun SINH (domain order)
  (let ((coeffs))
       (loop for i from 1 to order
	     if (evenp i) do (setq coeffs (append coeffs (list 0)))
	       else do (setq coeffs (append coeffs (list (cl:/ 1 (factorial i)))))
	     finally
	  (return (make-tpower-series domain coeffs :valence 1 :order order)))))

(define-taylor-expansion-fun COSH (domain order)
  (let ((coeffs))
       (loop for i from 0 to order
	     if (oddp i) do (setq coeffs (append coeffs (list 0)))
	       else do (setq coeffs (append coeffs (list (cl:/ 1 (factorial i)))))
	  finally
       (return (make-tpower-series domain coeffs :order order)))))

(define-taylor-expansion-fun TANH (domain order)
  (taylor (/ (sinh (ring-variables domain))
	     (cosh (ring-variables domain)))
	  domain order))

(define-taylor-expansion-fun ASINH (domain order)
  (let ((coeffs))
       (loop for i from 1 to order              
	     if (evenp i) do
	       (setq coeffs (append coeffs (list 0)))
	     else do
	       (let ((temp-coeff 1))
		    (loop for j from 1 below i
			  if (oddp j) do
			    (setq temp-coeff (cl:* temp-coeff j))
			  else do
			    (setq temp-coeff (cl:/ temp-coeff j))
			  finally
		       (setq coeffs
			     (append coeffs
				     (list (cl:* (cl:expt -1 (cl:/ (cl:- i 1) 2))
						 (cl:* temp-coeff
						       (cl:/ 1 i))))))))
	     finally
	  (return (make-tpower-series domain coeffs :valence 1 :order order)))))

(defun TAYLOR-EXP (domain order)
  (let ((coeffs))
       (loop for i from 0 to order
	     do (setq coeffs (append coeffs (list (cl:/ 1 (factorial i)))))
	     finally
	  (return (make-tpower-series domain coeffs :order order)))))

(defmethod substitute (value (variable symbol) (tp tpower-series)
			     &rest ignore)
  (declare (ignore ignore))
  (substitute value (coerce variable *general*) tp))

(defmethod substitute ((value tpower-series) (variable ge-variable)
		       (tp tpower-series) &rest ignore)
  (declare (ignore ignore))  
  (cond ((not (ge-equal variable (ring-variables (domain-of tp))))
	 (error "Can not substitute for ~A in ~A." variable (domain-of tp)))
	((not (plusp (valence value)))
	 (error "Can not substitute ~S for ~A in ~A." value variable tp))
	(t (let ((result (zero (domain-of value)))
		 (one (one (domain-of value))))
	     (with-tpower-series ((tp tp))
	       (declare (ignore tp-bo tp-order))
	       (loop for exp from (valence tp)
		     until (>= (- exp tp-val)
				      (array-dimension tp-coeffs 0))
		     do (setq result (+ result
				       (* (coerce (aref tp-coeffs
							(- exp tp-val))
						  (domain-of value))
					  (if (= exp 0) one
					      (expt value exp)))))))
		result))))

(defgeneric taylor (exp domain order)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod taylor (exp (domain tpower-series-domain) order)
  (taylor1 (coerce exp *general*) domain order))

(defgeneric taylor1 (exp domain order)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod taylor1 ((exp ge-variable) (domain tpower-series-domain) order)
  (cond ((ge-equal exp (ring-variables domain))
	 (make-tpower-series domain 1 :valence 1 :order order))
	(t (make-tpower-series domain (coerce exp (coefficient-domain-of domain))
					    :order order))))

(defmethod taylor1 ((exp ge-plus) (domain tpower-series-domain) order)
  (let ((sum (zero domain)))
       (loop for x in (terms-of exp)
	     do (setq sum (+ sum (taylor1 x domain order))))
       sum))

(defmethod taylor1 ((exp ge-times) (domain tpower-series-domain) order)
  (let ((new-valence 0)
	(max-valence 0)
	(new-order order)
	(prod (taylor1 1 domain order))
	(temp))
       (loop for x in (terms-of exp)
	     do (setq temp (taylor1 x domain order))
		(setq prod (* prod temp))
		(if (> (valence temp) max-valence)
		    (setq max-valence (valence temp)))
		(setq new-valence (+ new-valence (valence temp))))
       (setq new-order (+ new-order (- max-valence new-valence)))
       (if (= new-order order)
	   prod
	   (let ((prod (taylor1 1 domain new-order)))
		(loop for x in (terms-of exp)
		      do (setq prod (* prod (taylor1 x domain new-order))))
		prod))))

(defmethod taylor1 ((exp ge-expt) (domain tpower-series-domain) order)
  (let ((temp (taylor1 (base-of exp) domain order))
	(exponent (coerce (exponent-of exp) *general*)))
       (if (= (valence temp) 0)
	   (expt temp exponent)
	   (let ((new-order))
		(cond
		  ((typep exponent 'rational-integer)
		   (setq new-order (+ order (* (- 1 (integer-value exponent))
						  (valence temp)))))
		  ((typep exponent 'rational-number)
		   (let* ((new-br (lcm (denominator exponent)
				       (branch-order temp)))
			  (factor-old (/ new-br (branch-order temp)))
			  (factor-new (/ new-br (denominator exponent))))
			 (setq new-order (+ (ceiling
					      (/ (- (* order new-br)
						    (* factor-new
						       (numerator exponent)
						       (valence temp)))
						 factor-old))
					      (valence temp)))))
		  (t (error "can not raise ~s to ~s" (base-of exp) exponent)))
		(expt (taylor1 (base-of exp) domain new-order) exponent)))))

(defmethod taylor1 ((exp ge-application) (domain tpower-series-domain) order)
  (if (get-function nil (name-of (funct-of exp)))
      (let ((arg (car (args-of exp))))
	   (if (string= (name-of (funct-of exp)) "log")
	       (setq arg (- arg 1)))
	   (if (ge-equal arg (ring-variables domain))
	       (apply (getf (funct-of exp) 'taylor-expansion-fun)
		      (list domain order))
	       (substitute (taylor1 arg domain order)
			   (ring-variables domain)
			   (apply (getf (funct-of exp) 'taylor-expansion-fun)
				  (list domain order)))))
      (let ((tp (get-default-taylor-expansion domain)))
	   (if (typep (funct-of exp) 'ge-function-deriv)
	       (loop for i in (derivs-of (funct-of exp))
		     do (setq tp (deriv tp (nth i (args-of exp))))))
	   tp)))

(defmethod taylor1 (exp (domain tpower-series-domain) order)
  (let ((value (coercible? exp (coefficient-domain-of domain))))
    (cond ((not (null value))
	   (make-tpower-series domain value :order order))
	  (t (error "don't know how to coerce ~s to be an element of ~s"
		    exp domain)))))

(defmethod plus ((tp tpower-series) (exp general-expression))
  (+ tp (taylor1 exp (domain-of tp) (order tp))))

(defmethod plus ((exp general-expression) (tp tpower-series))
  (+ tp exp))

(defmethod difference ((tp tpower-series) (exp general-expression))
  (- tp (taylor1 exp (domain-of tp) (order tp))))

(defmethod difference ((exp general-expression) (tp tpower-series))
  (- (taylor1 exp (domain-of tp) (order tp)) tp))

(defmethod times ((tp tpower-series) (exp general-expression))
  (let ((temp (taylor1 exp (domain-of tp) (order tp))))
       (if (= (- (valence temp) (valence tp)) 0)
	   (* tp temp)
	   (* tp (taylor1 exp (domain-of tp) (+ (- (order tp) (valence tp))
						(valence temp)))))))

(defmethod times ((exp general-expression) (tp tpower-series))
  (* tp exp))

(defmethod quotient ((tp tpower-series) (exp general-expression))
  (* tp (expt exp -1)))

(defmethod quotient ((exp general-expression) (tp tpower-series))
  (* (expt exp -1) tp))
(in-package :weyli)

(defun get-default-taylor-expansion (power-series-domain)
    (let* ((temp-ring (coefficient-domain-of power-series-domain))
	   (coef-ring (if (typep temp-ring 'rational-function-field)
			      (QF-ring temp-ring)
			      temp-ring))   
	   (ring-vars (ring-variables coef-ring)))
	  (make-tpower-series power-series-domain
			      (mapcar #'(lambda (x) (coerce x coef-ring))
				      ring-vars)
			      :order (length ring-vars))))

(defmethod deriv ((tp tpower-series) &rest vars)
  (loop for v in vars
	with tp-var = (ring-variables (domain-of tp))
	do (setq tp (if (ge-equal (coerce v *general*) tp-var)
			(tps-deriv1 tp)
			(tps-deriv2 tp v))))
  tp)


;; Computes the derivative with respect to the power series variable.
(defun tps-deriv1 (a)
  (with-tpower-series ((a a))
    (let* ((domain (domain-of a))
	   (zero (zero (coefficient-domain-of domain)))
	   (n-terms (cl:- a-order a-val -1))
	   coeffs)

      (setq coeffs (make-array n-terms :initial-element zero))

      (loop for i below n-terms
	    do (setf (svref coeffs i)
		     (* (svref a-coeffs i) (+ a-val (/ i a-bo)))))
      (multiple-value-bind (ncoeffs shift) (trim-zeroes coeffs)
	(make-tpower-series domain ncoeffs
			    :valence (cl:+ a-val shift (cl:- a-bo))
			    :order (cl:- a-order a-bo)
			    :branch-order a-bo)))))

;; The coefficients are to be differentiated
(defun tps-deriv2 (a var)
  (with-tpower-series ((a a))
    (let* ((domain (domain-of a))
	   (zero (zero (coefficient-domain-of domain)))
	   (n-terms (cl:- a-order a-val -1))
	   coeffs)

      (setq coeffs (make-array n-terms :initial-element zero))

      (loop for i below n-terms
	    do (setf (svref coeffs i) (deriv (svref a-coeffs i) var)))
      (multiple-value-bind (ncoeffs shift) (trim-zeroes coeffs)
	(make-tpower-series domain ncoeffs
			    :valence (cl:+ a-val shift -1)
			    :order a-order
			    :branch-order a-bo)))))

(defmethod reversion ((tp tpower-series))
  (if (/= 1 (valence tp))
      (error "Can't compute reversion if valence is not 1")
      (let* ((domain (get-matrix-space (coefficient-domain-of (domain-of tp))))
	    (rank (length (coeffs tp)))
	    (array (make-array (list rank rank)))
	    (zero (zero (coefficient-domain-of (domain-of tp))))
	    (temp-tp tp))
	   (loop for i from 0 below rank do
	     (loop for k from 0 below i do
	       (setf (aref array i k) zero))
	     (loop for j from i below rank do
	       (setf (aref array i j) (aref (coeffs temp-tp) (- j i))))
	     (setq temp-tp (* temp-tp tp)))
	   (setq array (make-element domain array))
	   (setq array (recip array))
	   (make-tpower-series (domain-of tp)
			       (loop for j from 0 below rank
				     collect (ref array 0 j))
			       :valence 1
			       :order (order tp)))))

(defmethod solve-for-coeffs (coef-poly (domain rational-function-field)
				       coef-list value-list)
  (let ((cur-var (nth (length value-list) coef-list)))
       (quotient-reduce
	  domain
	  (- (coefficient (numerator coef-poly) (numerator cur-var) 0))
	  (coefficient (numerator coef-poly) (numerator cur-var) 1))))

(defmethod solve-for-coeffs (coef-poly domain coef-list value-list)
  (declare (ignore domain))
  (let ((cur-var (nth (length value-list) coef-list)))
	  (/ (- (coefficient coef-poly cur-var 0))
	     (coefficient coef-poly cur-var 1))))

;; diff-eqn is a differential-equation given as a general-expression
;; The solution of diff-eqn is assumed to be of the form
;;   a0 + a1 t + a2 t^2 + ... + an t^n
;; where "ai" are in the coef-ring, 't' is the variable and 'n' is the
;; order. The ai's are obtained using the initial value list "init-list"
;; containing (a0, a1, ..., aj) where 'j' is the order of the
;; differential equation.
#+ignore
(defmethod solve-diff-eqn ((diff-eqn general-expression)
			   coef-ring variable order init-list)
  (let* ((order (+ order 1))
	 (new-coefs (loop for i in init-list
			  if (not (coercible? i coef-ring)) collect i))
	 (coef-ring (if (null new-coefs) coef-ring
			  (get-polynomial-ring coef-ring new-coefs)))
	 (domain (get-quotient-field
		   (get-polynomial-ring coef-ring
			 (loop for i from 0 below order
			       collect (intern (format nil ".a~A" i))))))
	 (power-series-domain (get-tpower-series-domain domain variable))
	 (coef-list (mapcar #'(lambda (x) (coerce x domain)) init-list))
	 (var-list  (mapcar #'(lambda (x) (coerce x domain))
			    (ring-variables (QF-ring domain)))))	
	(setq diff-eqn (taylor diff-eqn power-series-domain order))
	(loop for i from 0 below (- (length var-list) (length coef-list)) do
	  (setq coef-list
		(append coef-list
			(list (solve-for-coeffs
				(substitute coef-list
					    (subseq var-list 0 (length coef-list))
					    (get-coeff diff-eqn i))
				domain var-list coef-list)))))
	(make-tpower-series power-series-domain
			    (simp-zeros coef-list)
			    :valence (num-leading-zeros coef-list)
			    :order (- (length coef-list) 1))))
