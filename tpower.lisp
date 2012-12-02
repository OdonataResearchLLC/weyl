;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;		       Truncated Power Series Domain
;;; ===========================================================================
;;; (c) Copyright 1994 Cornell University

 
(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.16")

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator tpower-series-ring
      ((coefficient-domain domain) variable)
    (let* ((field? (field? coefficient-domain))
           (domain
            (make-instance (if field? 'tpower-series-field
                               'tpower-series-ring)
                           :variables (coerce variable *general*)
                           :coefficient-domain coefficient-domain
                           :print-function (if field? 'tp-field-print-object
                                               'tp-ring-print-object))))
      (make-homomorphism coefficient-domain
                         #'(lambda (c) (make-tpower-series domain c))
                         domain)
      domain)))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-element-classes tpower-series-ring
    tpower-series))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-element-classes tpower-series-field
    tpower-series))

(defun tp-field-print-object (d stream)
  (with-slots (coefficient-domain) d
    (format stream "~A((" coefficient-domain)
    (display (ring-variables d) stream)
    (princ "))" stream)))

(defun tp-ring-print-object (d stream)
  (with-slots (coefficient-domain) d
    (format stream "~A[[" coefficient-domain)
    (display (ring-variables d) stream)
    (princ "]]" stream)))

;; GET-TPOWER-SERIES-DOMAIN
;;
;; If the coefficient domain is a ring, then construct a TPS which is
;; also a ring.  If the coefficient domain is a field, construct a TPS
;; which is also a field.
(defun get-tpower-series-domain (cdom variable)
  (let ((gvariable (coerce variable *general*)))
    (cond ((ring? cdom)
	   (add-domain #'(lambda (d)
			   (and (typep d 'tpower-series-ring)
				(eql (coefficient-domain-of d) cdom)
				(equal (ring-variables d) (list gvariable))))
	     (make-tpower-series-ring* cdom gvariable)))
	  ((field? cdom)
	   (add-domain #'(lambda (d)
			   (and (typep d 'tpower-series-ring)
				(eql (coefficient-domain-of d) cdom)
				(equal (ring-variables d) (list gvariable))))
	     (make-tpower-series-ring* cdom gvariable)))
	  (t (error "~S must be at least a ring" cdom)))))

(defgeneric make-tpower-series (domain series &key &allow-other-keys)
  (:documentation
   "The purpose of this function is unknown."))

;; This is a copy constructor.  It creates a new TPS like the one given as
;; a parameter with modifications as indicated by keyword parameters
(defmethod make-tpower-series ((domain tpower-series-domain) (tp tpower-series)
			       &key (valence (valence tp))
			            (order (order tp))
			            (branch-order (branch-order tp))
			            (coeffs (coeffs tp))
                                    &allow-other-keys)
  (make-instance 'tpower-series
		 :domain domain
		 :valence valence
		 :order (max valence order)
		 :branch-order branch-order
		 :coeffs coeffs))

;; Construct a TPS.  Assume the 2nd parameter is a constant and can
;; be coerced into the coefficient domain
(defmethod make-tpower-series ((domain tpower-series-domain) coef
			       &rest options &key &allow-other-keys)
  (apply #'make-instance 'tpower-series
		 :domain domain
		 :coeffs (vector (coerce coef (coefficient-domain-of domain)))
		 options))

;; Construct a zero valence unit branching order TPS from a list
;; of coefficients.
(defmethod make-tpower-series ((domain tpower-series-domain) (plist list)
			       &rest options &key &allow-other-keys)
  (let ((coef-domain (coefficient-domain-of domain)))
    (apply #'make-tpower-series
	   domain
	   (map 'array #'(lambda (e) (coerce e coef-domain))
		plist)
	   options)))

(defun trim-zeroes (coeffs)
  (let ((lead 0)
	(trail (length coeffs)))
    (loop for i fixnum below trail
	  do (if (0? (svref coeffs i)) (incf lead)
		 (return t)))
    #+ignore
    (loop for i fixnum downfrom trail above (1- lead)
	  do (if (0? (svref coeffs i)) (decf trail)
		 (return t))) 
    (if (cl:= lead trail) (values (vector (svref coeffs 0)) nil)
	(loop with vect = (make-array (cl:- trail lead))
	      for i upfrom lead below trail
	      do (setf (svref vect (cl:- i lead)) (svref coeffs i))
	      finally (return (values vect lead))))))

;; The argument is an array then, the we assume the arguments are
;; already coerced into the proper domain.
(defmethod make-tpower-series ((domain tpower-series-domain) (terms array) 
                                &key (valence 0) (order *positive-infinity*)
			       (branch-order 1))
  (multiple-value-bind (ncoeffs shift) (trim-zeroes terms)
    (make-instance 'tpower-series
		   :domain domain :coeffs ncoeffs
		   :valence (if shift (cl:+ valence shift) 0)
		   :order order :branch-order branch-order)))

(defmethod initialize-instance :after ((d power-series-domain)
				       &rest plist)
  (declare (ignore plist))
  (with-slots (zero one coefficient-domain) d
    (setq zero (make-tpower-series d (zero coefficient-domain)))
    (setq one (make-tpower-series d (one coefficient-domain)))))

;;
;; Printing functions

;; PRINT-GROUPED
;; Print n to the stream and parenthesize it if is contains any non-
;; alphanumeric characters.  This seem to be a good heuristic for a
;; human comprehensible output form.
(defun print-grouped (n stream)
  (let ((str (format nil "~A" n)))
    (if (or (every #'alphanumericp str)
	    (grouped? str #\( #\) )
	    (grouped? str #\[ #\] )
	    (grouped? str #\{ #\} ))
	(princ str stream)
	(format stream "(~A)" str))))

(defun grouped? (str a b)
  (let ((last (- (length str) 1)))
  (and (char= (char str 0) a )
       (char= (char str (- (length str) 1)) b)
       (loop for i from 1 to (- last 1)
	     with depth = 1
	     do (cond ((char= (char str i) a) (incf depth))
		      ((char= (char str i) b) (decf depth))
		      (t))
		never (= depth 0)))))
		     

(defun print-exponent (e br stream)
  (if (not (1? (/ e br)))
      (progn
	(princ "^" stream)
	(print-grouped (/ e br) stream))))
        
(defun print-tpower-series (var tp stream)
  (labels ((print-term (e c)
		       (cond ((0? c)
			      (print-object c stream))
			     ((0? e)
			      (print-object c stream))
			     (t
			       (if (not (1? c))
				   (print-grouped c stream))
			       #+Genera
			       (format stream "~'i~A~" var)
			       #-Genera
			       (display var stream)
			       (print-exponent e (branch-order tp) stream)))))
	  (progn
	    (print-term (valence tp) (aref (coeffs tp) 0))
	    (loop for exp from (+ (valence tp) 1)
		  with coef
		  until (>= (- exp (valence tp))
				   (array-dimension (coeffs tp) 0)) do
				     (setq coef (aref (coeffs tp)
							  (- exp (valence tp))))
					 (cond ((0? coef) nil)
					       ((minus? coef) (princ " - " stream)
						(print-term exp (minus coef)))
					       (t (princ " + " stream)
						  (print-term exp coef))))
	    (if (/= (order tp) *positive-infinity*)
		(progn
		  (princ " + o(" stream)
		  (print-term (order tp) 1)
		  (princ ")" stream ) )))))

(defmethod print-object ((p tpower-series) stream)
  (print-tpower-series
     (ring-variables (domain-of p))
     p
     stream))


;; SPREAD-COEFFS
;;
;; Construct a list of coefficients from a TPS but place (b-1) zeros
;; in between each coefficient of the given TPS.  This is used to
;; construct a new TPS with a larger branching order from an old TPS.
(defun tps-spread-coeffs (coeffs b)
  (let* ((len (length coeffs))
	 (zed (zero (domain-of (aref coeffs 0))))
	 (rval (make-array (cl:+ (cl:* b (cl:- len 1)) 1))))
    (loop for i fixnum from 0 below len
	  do (setf (aref rval (cl:* b i)) (aref coeffs i))
	     (if (cl:< (1+ i) len)
		 (loop for j fixnum from 1 to b
		       do (setf (aref rval (cl:+ (cl:* b i) j)) zed)))
	  finally (return rval))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun tps-var-val (v)
    (intern (format nil "~A-VAL" v) (symbol-package v)))
  
  (defun tps-var-bo (v)
    (intern (format nil "~A-BO" v) (symbol-package v)))

  (defun tps-var-order (v)
    (intern (format nil "~A-ORDER" v) (symbol-package v)))

  (defun tps-var-coeffs (v)
    (intern (format nil "~A-COEFFS" v) (symbol-package v))))

(defmacro with-tpower-series (vars-and-tps &body body)
  (let ((decls nil)
	(new-body body))
    (loop for (form . rest) on body
	  while (and (not (atom form))
		     (eql (first form) 'declare))
	  do (setq decls (append decls (rest form)))
	     (setq new-body rest))
  (setq body new-body)
  `(let (,@(loop for (var tps) in vars-and-tps 
		 append `((,(tps-var-val var) (valence ,tps))
			  (,(tps-var-bo var) (branch-order ,tps))
			  (,(tps-var-order var) (order ,tps))
			  (,(tps-var-coeffs var) (coeffs ,tps)))))
     ,@(when decls `((declare ,@decls)))
     (macrolet ((set-branch-order (var ord)
		  `(progn
		     (setf ,(tps-var-coeffs var)
			   (tps-spread-coeffs ,(tps-var-coeffs var) ,ord))
		     (setf ,(tps-var-order var)
			   (cl:* ,(tps-var-order var) ,ord))
		     (setf ,(tps-var-val var) (cl:* ,(tps-var-val var) ,ord))
		     (setf ,(tps-var-bo var) (cl:* ,(tps-var-bo var) ,ord))))
		(tps-rotatef (x y)
		  `(progn
		     (rotatef ,(tps-var-val x) ,(tps-var-val y))
		     (rotatef ,(tps-var-bo x) ,(tps-var-bo y))
		     (rotatef ,(tps-var-order x) ,(tps-var-order y))
		     (rotatef ,(tps-var-coeffs x) ,(tps-var-coeffs y)))))
       ,@body))))


;; MODIFY-BRANCH-ORDER

(defgeneric modify-branch-order (series factor)
  (:documentation
   "The purpose of this function is unknown."))
;;
;; Create a new TPS equivalent to the given TPS but with a branchorder
;; larger by a factor.  This is similar to TEMP-MODIFY-BR but the result
;; is a "genuine" TPOWER-SERIES whose use need not be temporary.
(defmethod modify-branch-order ((a tpower-series) (factor integer))
  (if (1? factor)
      a
      (make-tpower-series (domain-of a)
			  (tps-spread-coeffs (coeffs a) factor)
			  :valence (* factor (valence a))
			  :order (* factor (order a))
			  :branch-order (* factor (branch-order a)))))

(defmethod set-branch-order ((a tpower-series) (bo integer))  
  (cond ((cl:= (branch-order a) bo) 
	 a)
	((zerop (rem bo (branch-order a)))
	 (modify-branch-order a (cl:/ bo (branch-order a))))
	(t (error "New branch order must a multiple of old: ~S does not divide ~S"
		  (branch-order a) bo))))

(defsetf branch-order set-branch-order)

;; MAP-TPOWER-SERIES
;;
;; Create a new TPS by mapping a function onto all the coefficient of
;; an existing TPS.  This is convenient for computing minus.
(defun map-tpower-series (tp f)
  (make-tpower-series (domain-of tp) tp
		      :coeffs (map 'array f (coeffs tp))))

;; Coercions 
(defmethod coerce (elt (domain tpower-series-domain))
  (let ((value (coercible? elt (coefficient-domain-of domain))))
    (cond ((not (null value))
	   (make-tpower-series domain (vector value)))
	  (t (call-next-method)))))

(defmethod coerce ((exp symbol) (domain tpower-series-domain))
  (coerce (coerce exp *general*) domain))

(defmethod coerce ((exp list) (domain tpower-series-domain))
  (coerce (coerce exp *general*) domain))

;; INCOMPLETE!!!
#+ignore
(defmethod coerce ((p tpower-series) (d general-expressions))
  )

(defmethod coerce ((exp general-expression) (domain tpower-series-domain)) 
  (with-slots (variables) domain 
    (cond ((ge-equal exp variables)
	   (make-tpower-series domain
			       (vector (zero (coefficient-domain-of domain))
				       (one (coefficient-domain-of domain)))))
	  ((and (ge-atom? exp)
		(let ((var (coercible? exp (coefficient-domain-of domain))))
		  (and var  (make-tpower-series domain (vector var))))))
	  ((ge-plus? exp)
	   (let ((sum (zero domain)))
	     (loop for x in (terms-of exp)
		   do (setq sum (+ sum (coerce x domain))))
	     sum))
	  ((ge-times? exp)
	   (let ((prod (one domain)))
	     (loop for x in (terms-of exp)
		   do (setq prod (* prod (coerce x domain))))
	     prod))
	  ((and (ge-expt? exp)
		(integer? (exponent-of exp)))
	   (expt (coerce (base-of exp) domain) (exponent-of exp)))
	  (t (coerce exp (coefficient-domain-of domain))))))

;; Polynomial coercions
;;
;; Since the TPS can represent any polynomial, there is an obvious mapping
;; from a polynomial domain to the power series domain when the variables
;; of the domains are the same.
;;
;; The coercion is successful only for upolynomials and univariate
;; mpolyomials
(defmethod coerce ((a upolynomial) (domain tpower-series-domain))
  (if (coercible? a (coefficient-domain-of domain)) (call-next-method)  
      (if (not
	    (equal (ring-variables (domain-of a)) (list (ring-variables domain))))
	  (error "Can't coerce element of ~A into domain ~A." (domain-of a)
		 domain)
	  (make-instance 'tpower-series :domain domain
			 :coeffs (copy-seq (poly-form a))))))

(defmethod coerce ((a mpolynomial) (domain tpower-series-domain))
  (if (coercible? a (coefficient-domain-of domain)) (call-next-method)
      (if (not
	    (equal (ring-variables (domain-of a)) (list (ring-variables domain))))
	  (error "Can't coerce element of ~A into domain ~A." (domain-of a)
		 domain)
	  (let* ((ord (clist-degree (poly-form a)))
		 (val (reduce #'(lambda (a b) (min (car a) (car b)))
			      (poly-terms (poly-form a))))
		 (arr (make-array (+ (- ord val) 1) :initial-element
				  (zero (coefficient-domain-of domain)))))
		(map 'array #'(lambda (a)
				      (setf (aref arr (- (car a) val)) (cdr a)))
		     (poly-terms (poly-form a)))
		(make-instance 'tpower-series :domain domain
			       :coeffs arr
			       :valence val)))))

;; LARGEST-STORED-COEFF
;;
;; Computes the numerator of the largest exponent for which a coefficient
;; is stored in (coeffs tp).
(defmethod largest-stored-coeff ((tp tpower-series))
  (+ (valence tp)
     (- (array-dimension (coeffs tp) 0) 1)))

;; TRUNCATE-ORDER
;;
;; Truncates the order of a TPS to an integer.  All coefficients with
;; exponents greater than the new order are discarded.
(defmethod truncate-order ((tp tpower-series) (torder integer))
  (let ((mtorder (* (branch-order tp) torder)))
    (cond ((< mtorder (valence tp))
	   (make-tpower-series (domain-of tp) tp
			       :valence mtorder
			       :order mtorder
			       :coeffs (vector (zero (coefficient-domain-of
						       (domain-of tp))))))
	  ((>= mtorder (order tp)) tp)
	  ((>= (+ 1 (- mtorder (valence tp)))
	       (array-dimension (coeffs tp) 0))
	   (make-tpower-series (domain-of tp) tp
			       :order mtorder))
	  (t (make-tpower-series (domain-of tp) tp
				 :order mtorder
				 :coeffs (subseq (coeffs tp)
						 0
						 (+ 1 (- mtorder (valence tp)))))))))

#+ignore
(defsetf order truncate-order)


;; unary MINUS
;;
(defmethod minus ((tp tpower-series))
  (map-tpower-series tp #'minus))

;; PLUS
;; 
;; Computes the sum of two TPS.
;; General procedure
;;    1) make branching orders equal
;;    2) compute coefficients
;;    3) simplify out zeros
;;    4) package result
(defmethod-sd plus ((a tpower-series) (b tpower-series))
  (with-tpower-series ((a a) (b b))
    (let* ((bo (lcm a-bo b-bo))
	   (zero (zero (coefficient-domain-of domain)))
	    a-lim b-lim valence order coeffs com)

      (unless (= a-bo b-bo)
	(set-branch-order a (/ bo a-bo))
	(set-branch-order b (/ bo b-bo))) 

      (setq valence (cl:min a-val b-val))
      (setq order (min a-order b-order))

      (when (< b-val a-val)
	(tps-rotatef a b))

      (setq com (cl:- b-val a-val))
      
      (setq a-lim (length a-coeffs)
	    b-lim (cl:+ com (length b-coeffs)))

      (setq coeffs (make-array
		     (if (integerp order) (cl:- order valence -1)
			 (max (+ a-val (length a-coeffs))
			      (+ b-val (length b-coeffs))))
		     :initial-element zero))

      (loop for i fixnum below com
	    do (setf (svref coeffs i)
		     (if (cl:< i a-lim) (svref a-coeffs i) zero)))

      (loop for i upfrom com below (array-dimension coeffs 0)
	    do (setf (svref coeffs i)
		     (+ (if (cl:< i a-lim) (svref a-coeffs i) zero)
			(if (cl:< i b-lim) (svref b-coeffs (cl:- i com))
			    zero))))
      (make-tpower-series domain coeffs
			  :valence valence :order order :branch-order bo))))

;; DIFFERENCE
;;
;; Use PLUS
(defmethod-sd difference ((a tpower-series) (b tpower-series))
  (plus a (minus b)))

;; TIMES
;; 
;; Computes the product of two TPS.
;; General procedure
;;    1) make branching orders equal
;;    2) compute coefficients
;;    3) simplify out zeros
;;    4) package result
(defmethod-sd times ((a tpower-series) (b tpower-series))
  (with-tpower-series ((a a) (b b))
    (let* ((bo (lcm a-bo b-bo)) 
	   (zero (zero (coefficient-domain-of domain)))
	   valence order coeffs)

      (unless (= a-bo b-bo)
	(set-branch-order a (/ bo a-bo))
	(set-branch-order b (/ bo b-bo)))

      (setq valence (cl:+ a-val b-val))
      (setq order (cl:+ (min (cl:- a-order a-val)
                             (cl:- b-order b-val))
                        valence))
            
      (setq coeffs (make-array
		     (if (integerp order) (cl:- order valence -1)
			 (cl:+ (length a-coeffs) (length b-coeffs) -1))
		     :initial-element zero))

      (loop with n-terms = (array-dimension coeffs 0)
            for i below n-terms
	    with a-lim = (min (length a-coeffs) n-terms)
            and b-lim = (min (length b-coeffs) n-terms)
	    do (loop for j below (1+ i)
                     do (when (and (cl:< j a-lim)
                                   (cl:< (cl:- i j) b-lim))
                          (setf (svref coeffs i)
		                (+ (svref coeffs i)
                                   (* (svref a-coeffs j)
                                      (svref b-coeffs (cl:- i j))))))))
      (make-tpower-series domain coeffs
			  :valence valence :order order :branch-order bo))))
;; EXPT
;;
;; Compute tp^s for a TPS.  The exponent may be a rational-integer, a
;; rational-number, or a element of the coefficient domain.
;;
(defmethod expt ((a tpower-series) k) 
  (if (not (or (integerp k) (typep k 'rational-integer)
	       (typep k 'ratio) (typep k 'rational-number)
               (0? (valence a))))
      (call-next-method)
      (with-tpower-series ((a a))
        (let* ((domain (domain-of a))
               (zero (zero (coefficient-domain-of domain)))
	       valence order coeffs)

	  (setq k (convert-to-lisp-number k))
	      
	  (cond ((zerop a-val)
		 (setq valence a-val)
		 (setq order (cl:+ (cl:- a-order a-val) valence)))
		((integerp k)
		 (setq valence (cl:* k a-val))
		 (setq order (cl:+ (cl:- a-order a-val) valence)))
		((typep k 'ratio)
		 (set-branch-order a (denominator k))
		 (setq valence (cl:/ (cl:* (numerator k) a-val)
				     (denominator k)))
		 (setq order (cl:+ (cl:- a-order a-val) valence)))
		(t (error "Internal error")))

	  (setq coeffs (make-array
			 (cond ((integerp order) (cl:- order valence -1))
			       ((not (integerp k))
				(error "Can't compute ~S ^ ~S"
				       a k))
			       (t (1+ (cl:* k (1- (length a-coeffs))))))
			 :initial-element zero))

	  (setf (svref coeffs 0) (expt (svref a-coeffs 0) k))

	  (loop with n-terms = (array-dimension coeffs 0)
		and a-lim = (array-dimension a-coeffs 0)
		for i upfrom 1 below n-terms
		do (loop for j below (1+ i)
			 do (setf (svref coeffs i)
				  (+ (svref coeffs i)
				     (* (- (* (+ k 1) j) i)
					(if (cl:< j a-lim) (svref a-coeffs j)
					    zero)
					(svref coeffs (cl:- i j)))))
			 finally (setf (svref coeffs i) 
				       (/ (svref coeffs i)
					  (* i (svref a-coeffs 0))))))
	  (make-tpower-series domain coeffs
			      :valence valence
			      :order order
			      :branch-order a-bo)))))

;; QUOTIENT
;;
;; This is the easy way.  There may be a more efficient way.
(defmethod quotient ((a tpower-series) (b tpower-series))
  (* a (expt b -1)))
