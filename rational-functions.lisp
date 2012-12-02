;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			  Rational Function Fields
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; rational-functions.lisp,v 1.9 1995/05/24 17:42:10 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.9")

(defmethod initialize-instance :after ((qf rational-function-field) &rest plist)
  (declare (ignore plist))
  (with-slots (print-function) qf
    (setf print-function 'ratfun-field-print-object))) 

(defun ratfun-field-print-object (qf stream)
  (format stream "~A(" (coefficient-domain-of (QF-ring qf)))
  (display-list (ring-variables (QF-ring qf)) stream)
  (princ ")" stream))

(defmethod ring-variables ((qf rational-function-field))
  (ring-variables (qf-ring qf)))

;; The general GET-Q... in quotient-field.lisp is good enough
(define-domain-creator quotient-field ((ring multivariate-polynomial-ring))
  (let* ((coefs (coefficient-domain-of ring))
         (qf (make-instance 'rational-function-field
                            :ring ring)))
    (with-slots (zero one) qf
      (setq zero (make-rational-function qf (zero coefs) (one coefs)))
      (setq one (make-rational-function qf (one coefs) (one coefs))))
    (make-homomorphism ring #'(lambda (x)
                                (make-quotient-element qf x (one ring)))
                       qf)
    qf))

(defsubst make-rational-function (domain numerator denominator)
  (make-instance 'rational-function :domain domain
		 :numerator numerator
		 :denominator denominator))

(defgeneric make-rational-function* (domain num den)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod make-rational-function* (domain num den)
  (let* ((poly-domain (qf-ring domain))
	 (coef-domain (coefficient-domain-of poly-domain)))
    (bind-domain-context poly-domain
      (cond ((and (typep coef-domain 'field)
		  (poly-coef? den))
	     (setq num (poly-times (recip den) num))
	     (setq den (one coef-domain)))
	    ((poly-minus? den)
	     (setq num (poly-minus num) den (poly-minus den)))))
    (make-rational-function domain num den)))

(defmethod make-quotient-element
    ((domain rational-function-field) numerator denominator)
  (make-rational-function domain
			  (poly-form (coerce numerator (qf-ring domain)))
			  (poly-form (coerce denominator (qf-ring domain)))))

(defmethod print-object ((ratfun rational-function) stream) 
  (with-numerator-and-denominator (numerator denominator) ratfun
    (cond ((poly-1? denominator)
	   (print-mpolynomial-form (QF-ring (domain-of ratfun)) numerator stream))
	  (t (princ "(" stream)
	     (print-mpolynomial-form (QF-ring (domain-of ratfun)) numerator stream)
	     (princ ")/(" stream)
	     (print-mpolynomial-form (QF-ring (domain-of ratfun)) denominator stream)
	     (princ ")" stream))))) 

(defmethod numerator ((r rational-function))
  (let ((domain (domain-of r)))
    (make-polynomial (QF-ring domain) (qo-numerator r))))

(defmethod denominator ((r rational-function))
  (let ((domain (domain-of r)))
    (make-polynomial (QF-ring domain) (qo-denominator r))))

(defmethod 0? ((r rational-function))
  (poly-0? (qo-numerator r)))

(defmethod 1? ((r rational-function))
    (and (poly-1? (qo-numerator r))
	 (poly-1? (qo-denominator r))))

(defmethod minus ((r rational-function))
  (let ((domain (domain-of r)))
    (with-numerator-and-denominator (numerator denominator) r
      (bind-domain-context (qf-ring domain)
	(make-rational-function domain (poly-minus numerator) denominator)))))

(defmethod quotient-reduce ((qf rational-function-field) num &optional den)
  (with-slots (ring) qf
    (when (not (eql (domain-of num) ring))
      (error "The numerator's domain, ~S, is not the ring of the quotient field ~S"
	     (domain-of num) ring))
    (when (not (eql (domain-of den) ring))
      (error "The denominator's domain, ~S, is not the ring of the quotient field ~S"
	     (domain-of den) ring))
    (ratfun-reduce qf (poly-form num) (poly-form den))))

;; The arguments to ratfun-reduce are poly-forms not polynomials!!!
(defun ratfun-reduce (qf num &optional den)
  (when (null den)
    (setq den (one (QF-ring qf))))
  (if (poly-0? num) (zero qf)
      (let ((common-gcd (poly-gcd num den)))
	(unless (poly-1? common-gcd)
	  (setq num (poly-quotient num common-gcd)
		den (poly-quotient den common-gcd)))
	(make-rational-function* qf num den))))

(defmethod-sd plus ((r1 rational-function) (r2 rational-function))
  (with-numerator-and-denominator (n1 d1) r1
    (with-numerator-and-denominator (n2 d2) r2
      (bind-domain-context (qf-ring domain)
	(ratfun-reduce domain
		       (poly-plus (poly-times n1 d2) (poly-times n2 d1))
		       (poly-times d1 d2))))))

(defmethod-sd difference ((r1 rational-function) (r2 rational-function))
  (with-numerator-and-denominator (n1 d1) r1
    (with-numerator-and-denominator (n2 d2) r2
      (bind-domain-context (qf-ring domain)
	(ratfun-reduce domain
		       (poly-difference (poly-times n1 d2) (poly-times n2 d1))
		       (poly-times d1 d2))))))

(defmethod-sd times ((r1 rational-function) (r2 rational-function)) 
  (with-numerator-and-denominator (n1 d1) r1
    (with-numerator-and-denominator (n2 d2) r2
      (bind-domain-context (qf-ring domain)
	(let (common-gcd)
	  (setq common-gcd (poly-gcd n1 d2))
	  (if (not (poly-1? common-gcd))
	      (setq n1 (poly-quotient n1 common-gcd)
		    d2 (poly-quotient d2 common-gcd)))
	  (setq common-gcd (poly-gcd n2 d1))
	  (if (not (poly-1? common-gcd))
	      (setq n2 (poly-quotient n2 common-gcd)
		    d1 (poly-quotient d1 common-gcd)))
	  (setq d1 (poly-times d1 d2)
		n1 (poly-times n1 n2))
	  (make-rational-function* domain n1 d1))))))

(defmethod-sd quotient ((r1 rational-function) (r2 rational-function)) 
  (with-numerator-and-denominator (n1 d1) r1
    (with-numerator-and-denominator (n2 d2) r2	       
      (bind-domain-context (qf-ring domain)
	(let (common-gcd)
	  (setq common-gcd (poly-gcd n1 n2))
	  (if (not (poly-1? common-gcd))
	      (setq n1 (poly-quotient n1 common-gcd)
		    n2 (poly-quotient n2 common-gcd)))
	  (setq common-gcd (poly-gcd d1 d2))
	  (if (not (poly-1? common-gcd))
	      (setq d2 (poly-quotient d2 common-gcd)
		    d1 (poly-quotient d1 common-gcd)))
	  (setq n1 (poly-times n1 d2)
		d1 (poly-times d1 n2))
	  (make-rational-function* domain n1 d1))))))

(defmethod recip ((r1 rational-function))
    (with-numerator-and-denominator (num den) r1
      (make-rational-function* (domain-of r1) den num)))

(defmethod expt ((r1 rational-function) (exp integer))
  (let ((domain (domain-of r1)))
    (with-numerator-and-denominator (n1 d1) r1
      (bind-domain-context (qf-ring domain)
	(if (minusp exp)
	    (make-rational-function domain
				    (poly-expt d1 (cl:- exp))
				    (poly-expt n1 (cl:- exp)))
	    (make-rational-function domain
				    (poly-expt n1 exp) (poly-expt d1 exp)))))))

(defmethod expt ((r1 rational-function) (exp rational-integer))
  (expt r1 (integer-value exp)))

(defmethod-sd binary-gcd ((r1 rational-function) (r2 rational-function))
  (with-numerator-and-denominator (n1 d1) r1
    (with-numerator-and-denominator (n2 d2) r2
      (bind-domain-context (qf-ring domain)
	(make-rational-function domain (poly-gcd n1 n2) (poly-lcm d1 d2))))))


(defmethod list-of-variables
    ((x rational-function) &optional list-of-variables)
  (let* ((domain (domain-of x))
	 (ring-domain (qf-ring domain)))
    (with-numerator-and-denominator (num-x den-x) x
      (loop for order-number in (poly-list-of-variables
				     den-x (poly-list-of-variables num-x))
	    do (pushnew (get-variable-name order-number ring-domain)
			list-of-variables :test #'ge-equal)))
    list-of-variables))

;; This is just like poly-subst, but its result is a rational function
;; and thus the values being substituted can be rational functions. 
(defun rational-poly-subst (poly var-value)
  (let ((temp nil))
    (cond ((null var-value)
	   poly)
	  ((poly-coef? poly) (coerce poly *domain*))
	  (t (setq temp (or (second (assoc (poly-order-number poly) var-value
				     :test #'eql))))
	   (when (null temp)
	     (error "This variable can't be mapped into the domain ~S"
		    *domain*))
	   (rational-terms-horners-rule (poly-terms poly) temp var-value)))))

(defun rational-terms-horners-rule (terms value &optional var-value)
  (let ((old-e (le terms))
	(ans (rational-poly-subst (lc terms) var-value)))
    (map-over-each-term (red terms) (e c) 
      (setq ans (+ (* (expt value (e- old-e e)) ans)
		   (rational-poly-subst c var-value)))
      (setq old-e e))
    (* ans (expt value old-e))))

(defmethod substitute 
    ((value rational-function) (variable rational-function)
     (p rational-function) &rest ignore)
  (declare (ignore ignore))
  (substitute (list value) (list variable) p))

(defmethod substitute ((values list) (variables list) (p rational-function)
		       &rest ignore)
  (declare (ignore ignore))
  (let* ((domain (domain-of p))
	 (ring (qf-ring domain))
	 (new-domain (domain-of (first values)))
	 subst-list)
    (loop for var in variables
	  unless (eql (domain-of var) domain)
	    do (error "Domain of ~S was expected to be ~S" var domain))
    (loop for val in values
	  unless (eql (domain-of val) new-domain)
	    do (error "Domain of ~S was expected to be ~S" val new-domain))
    (loop for var in (ring-variables ring)
	  do (unless (find var variables 
			   :test #'(lambda (a b) 
				     (eql a (variable-symbol
					   ring (numerator b))))) 
	       (push (coerce var domain) variables)
	       (push (if (coercible? var new-domain)
			 (coerce var new-domain)
			 nil)
		     values)))
    (setq subst-list (loop for var in variables
			   for val in values
			   collect (list (variable-index ring (numerator var))
					 val)))
    (with-numerator-and-denominator (num den) p
      (bind-domain-context new-domain
	(/ (rational-poly-subst num subst-list)
	   (rational-poly-subst den subst-list))))))

(defmethod partial-deriv ((p rational-function) x)
  (error "Don't know how to compute the partial deriv with respect to ~S"
	 x))

(defmethod partial-deriv ((p rational-function) (x symbol))
  (partial-deriv p (coerce x *general*)))

(defmethod partial-deriv ((p rational-function) (x list))
  (partial-deriv p (coerce x *general*)))

(defmethod partial-deriv ((p rational-function) (x general-expression))
  (let ((domain (domain-of p)))
    (with-slots (variables) (qf-ring domain)
      (if (member x variables :test #'ge-equal)
	  (partial-deriv p (coerce x domain))
	  (call-next-method)))))

(defmethod partial-deriv ((p rational-function) (x rational-function))
  (with-numerator-and-denominator (num-x den-x) x
    (with-numerator-and-denominator (num-p den-p) p
      (let ((domain (domain-of p))
	    terms)
	(unless (and (eql domain (domain-of x))
		     (1? den-x)
		     (null (red (setq terms (poly-terms num-x))))
		     (e1? (le terms))
		     (poly-1? (lc terms)))
	  (error "~S is not a variable in ~S" x domain))
	(bind-domain-context (qf-ring domain)
	  (ratfun-reduce domain
			 (poly-difference
			  (poly-times (poly-derivative num-p num-x) den-p)
			  (poly-times (poly-derivative den-p num-x) num-p))
			 (poly-times den-p den-p)))))))

(defmethod deriv ((poly rational-function) &rest vars)
  (let* ((domain (domain-of poly))
	 deriv diff)
    (bind-domain-context domain
      (loop for var in vars do
	(setq var (coerce var *general*))
	(setq deriv (zero domain))
	(loop with variables = (list-of-variables poly)
	      for kernel in variables do
	  (when (depends-on? kernel var)
	    (setq diff (deriv kernel var))
	    (loop for new in (different-kernels diff variables) do
	      (add-new-variable (qf-ring domain) new))
	    (setq deriv
		  (+ deriv (* (partial-deriv poly kernel)
			      (coerce diff domain))))))
	(setq poly deriv)))
    poly))

(defmethod coerce ((x ge-expt) (domain rational-function-field))
  (if (ge-minus? (exponent-of x))
      (recip (coerce (expt (base-of x) (- (exponent-of x)))
		     domain))
      (call-next-method)))
