;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;		      Sparse Polynomial Routines for GCD
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; sparsegcd.lisp,v 1.5 1995/03/13 22:36:50 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.5")

;; The linear-form macro makes sure that the linear polynomials we
;; generate are well formed.
;;; FIXME : Generates defined but never used warnings for zero in some cases.
(defmacro with-linear-support (&body body)
  `(let ((zero (zero *coefficient-domain*))
	 (one (one *coefficient-domain*)))
     (macrolet ((linear-form (pt)
		  `(if (0? ,pt) (make-terms (e1) one)
		      (make-terms (e1) one (make-terms (e0) (- ,pt))))))
       ,@body)))

;; This is a general interpolation routine.  Given the value of a
;; univariate polynomial at several points it determines the polynomial.
(defun terms-interpolate (pts vals)
  (with-linear-support
      (do ((u (make-terms (e0) (car vals))
	      (terms-plus
	       u (terms-mon-times qk (e0)
				  (/ (- (car uk)
					(terms-horners-rule u (car xk)))
				     denom))))
	   (qk (linear-form (first pts))
	       (terms-times qk (linear-form (first xk))))
	   (uk (cdr vals) (cdr uk))
	   (xk (cdr pts) (cdr xk))
	   (denom))
	  ((null xk) u)
	(when (poly-0? (setq denom (terms-horners-rule qk (first xk))))
	  (error "~S occurs twice in list of evaluation points: ~S"
		 (first xk) pts)))))

(defun poly-interpolate (var pts vals)
  (poly-simp var (terms-interpolate pts vals)))

(defgeneric interpolate (vars pts vals &key degrees)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod interpolate ((x polynomial) pts vals &rest ignore)
  (declare (ignore ignore))
  (let ((domain (domain-of x)))
    (bind-domain-context domain
      (make-polynomial domain
        (poly-interpolate (poly-form x) pts
			  (loop for v in vals
				collect (if (and (typep v 'mpolynomial)
						 (eql (domain-of v) domain))
					    (poly-form v)
					    v)))))))

(defmethod interpolate ((vars list) pts vals &key (degrees :total))
  (let ((var-cnt (length vars))
	ring array)
    (cond ((typep (first vars) 'domain-element)
	   (setq ring (domain-of (first vars))))
	  (t (setq ring *general*)))
    (setq vars
	  (loop for v in vars
		collect (cond ((symbolp v)
			       (coerce v ring))
			      ((eql ring (domain-of v))
			       v)
			      (t (error
				   "Variables are not of the same domain: ~S"
				   vars)))))
    (loop for pt in pts
	  do (unless (eql (length pt) var-cnt)
	       (error "Point ~S does not have ~D components"
		      pt var-cnt))) 
    (unless (eql (length pts) (length vals))
      (error "Points (~D) and values (~D) are of different lengths"
	     (length pts) (length vals)))

    (when (or (eql degrees :total)
	      (eql degrees :maximum))
      (setq degrees
	    (mapcar #'rest
		    (degree-partition
		      (1+ var-cnt)
		      (bound-degree-term-count var-cnt
					       (length pts)
					       :type degrees)))))
    (setq array (make-degree-matrix ring degrees pts))
    (loop for i below (length pts)
	  with poly = (zero ring)
	  for expt in degrees
	  do (setq poly
		   (+ poly
		     (* (expt-list vars expt)
			(loop for j below (length pts)
			      for v in vals
			      with c = (zero ring)
			      do (setq c (+ c (* v (aref array i j))))
			      finally (return c)))))
	  finally (return poly))))

	     
(defun expt-list (base expt)
  (loop with ans = (one (domain-of (first base)))
	for b in base
	for e in expt
	do (setq ans (* ans (expt b e)))
	finally (return ans)))

#|  The following is often useful for debugging |
(defun display-array (arr)
  (let ((dims (array-dimensions arr)))
    (loop for i below (first dims)
	  do (loop for j below (second dims)
		   do (format t "~S " (aref arr i j)))
	     (fresh-line))))
||#	

(defun bound-degree-term-count (num-vars term-count &key (type :total))
  (cond ((eql type :total)
	 (loop for i upfrom 0
	       do (when (> (combinations (+ i num-vars) i) term-count)
		    (return (1- i)))))
	((eql type :maximum)
	 (1- (truncate (integer-nth-root term-count num-vars))))
	(t (error "Degree bound must be either :total or :maximum: ~S"
		  type))))

;; The following routine returns a list of all the exponent vectors
;; for monomials in v variables of total degree equal to d.  If one
;; wants all the monomials of total degree less than or equal to d,
;; incrase n by one and drop the first component of every list.
(defun degree-partition (v d)
  (cond ((1? v) `((,d)))
	((0? d)
	 (loop for part in (degree-partition (1- v) d)
	       collect (cons 0 part)))
	(t (loop for u below (1+ d)
		 append (loop for part in (degree-partition (1- v) (- d u))
			      collect (cons u part))))))

;; This routine takes a list of exponent vectors and points (values)
;; for the variables and returns the inverse of the matrix where
;; each row consists of one of the vals raised to each of the exponent
;; vectors.
(defun make-degree-matrix (domain expt-vects pts)
  (let ((array (make-array (list (length pts) (length expt-vects)))))
    (loop for i upfrom 0
	  for pt in pts
	  do (loop for j upfrom 0
		   for expt-vect in expt-vects
		   for temp = (one domain)
		   do (setf (aref array i j)
			    (loop for e in expt-vect
				  for p in pt
				  do (setq temp (* temp (expt p e)))
				  finally (return temp)))))
    (invert-array domain array)))

;; This routine could be sped up by computing Q in place.  Theoretical
;; speedups are possible (Kaltofen&Yagati), but they are impractical.
(defun compute-vandermonde-Q (pts)
  (with-linear-support
      (let ((ans (make-terms (e1) (one *coefficient-domain*)
			     (make-terms (e0) (- (first pts))))))
	(loop for pt in (rest pts)
	      do (setq ans (terms-times ans (linear-form pt))))
	ans)))

(defun solve-vandermonde (pts vals &optional Q)
  (unless Q
    (setq Q (compute-vandermonde-Q pts)))
  (with-linear-support
    (let* ((sols (make-array (length vals) :initial-element zero))
	   qi)
      (loop for pt in pts
	    for val in vals
	    do (setq qi (terms-quotient q (linear-form pt)))
	       (map-over-each-term
		    (terms-cquotient qi (terms-horners-rule qi pt))
		    (e c)
		 (setf (aref sols e) (+ (aref sols e) (* c val)))))
      (loop for i below (array-dimension sols 0)
	    collect (aref sols i)))))

(defun solve-vandermondeT (pts vals &optional Q)
  (unless Q
    (setq Q (compute-vandermonde-Q pts)))
  (with-linear-support
    (let ((k (make-array (length vals) :initial-contents vals))
	  qi x sols)
      (loop for pt in pts do
	(setq qi (terms-quotient q (linear-form pt)))
	(setq x zero)
	(map-over-each-term (terms-cquotient qi (terms-horners-rule qi pt))
			    (e c)
          (setq x (+ x (* c (aref k e)))))
	(push x sols))
      sols)))

(defun solve-vandermondeTD (pts vals &optional Q)
  (unless Q
    (setq Q (compute-vandermonde-Q pts)))
  (with-linear-support
    (let ((k (make-array (length vals) :initial-contents vals))
	  qi x sols)
      (loop for pt in pts do
	(setq qi (terms-quotient q (linear-form pt)))
	(setq x zero)
	(map-over-each-term (terms-cquotient qi (terms-horners-rule qi pt))
			    (e c)
          (setq x (+ x (* c (aref k e)))))
	(push x sols))
      (loop for sol in (nreverse sols)
	    for pt in pts
	    collect (/ sol pt)))))

(defun poly-skeleton (poly vars)
  (cond ((poly-coef? poly)
	 (list (loop for i in vars collect 0)))
	((same-variable? poly (first vars))
	 (let ((skeleton ()))
	   (map-over-each-term (poly-terms poly) (e c)
	     (loop for skel in (poly-skeleton c (rest vars))
		   do (push (cons e skel) skeleton)))
	   (nreverse skeleton)))
	((more-main? poly (first vars))
	 (error "Involves a variable that it shouldn't"))
	(t (loop for skel in (poly-skeleton poly (rest vars))
		 collect (cons 0 skel)))))

(defgeneric pskeleton (poly vars)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod pskeleton ((p mpolynomial) vars)
  (let ((vars (mapcar
		#'(lambda (x) (poly-form (coerce x (domain-of p))))
		      vars)))
       (poly-skeleton (poly-form p) vars)))

(defgeneric sparseinterpstage (poly bp d k)
  (:documentation
   "The purpose of this function is unknown."))

;; Sparse multivariate Interpolation.
(defmethod SparseInterpStage ((Pkminus1 mpolynomial) Bp D k)
  (SparseInterpStagei (coefficient-domain-of (domain-of Pkminus1))
		      Pkminus1 Bp D k))

(defgeneric sparseinterpstagei (coef-domain pkminus1 bp d k &optional coef-bound)
  (:documentation
   "The purpose of this function is unknown."))

;; If the polynomial ring is over integers then we can compute over Zp
;; and use Chinese Remainder Theorem.
(defmethod SparseInterpStagei ((coef-domain rational-integers)
			       (Pkminus1 mpolynomial) Bp D k
			       &optional (coef-bound most-positive-fixnum))
  (let* ((domain (domain-of Pkminus1))
	 (vars (list-coerce (list-of-variables Pkminus1) domain))
	 (Skel (pskeleton Pkminus1 vars))
	 (l (length Skel))
	 (S (make-array (list l (1- k)) :initial-contents (reverse Skel)))
	 (primes (choice-primes (* 2 coef-bound)))
	 (gfp nil)
	 (gfp-domain nil)
	 (Y '())
	 (m '())
	 (cur-pt 0)
	 (X '())
	 (vals '())	 
	 (B '())
	 (EQ (make-array (list D l)))
	 (poly (make-array (list (length primes) l)))
	 (Pprime '())
	 (factor (one domain))
	 (temp-poly (zero domain))
	 (Pk (zero domain)))
	(loop for ip upfrom 0 as p in primes do
	  (setq X '())
	  (setq gfp (get-finite-field p))
	  (setq gfp-domain (get-polynomial-ring gfp (ring-variables domain)))
	  (setq *coefficient-domain* gfp)
	  (setq vals (loop for i below (- (length (ring-variables domain)) k)
			   collect (random gfp)))
	  (multiple-value-setq (Y m) (InitY gfp S k l))
	  (loop for i below D do
	    (loop while (member (setq cur-pt (random gfp))
				X))
	    (setq X (append X (list cur-pt)))
	    (setq B (loop for j below l
			  collect (%funcall Bp (append
						 (loop for pt in Y
						       collect (expt pt j))
						 (list cur-pt)
						 vals))))
	    (setq Pprime (Solve-vandermondeT m B))
	    (loop for j upfrom 0 as eqn in Pprime do
	      (setf (aref EQ i j) eqn)))
	  (loop for j below l do
	    (setf (aref poly ip j)
		  (interpolate
		    (coerce (nth (1- k) (ring-variables domain))
			    gfp-domain)
		    X
		    (loop for i below D 
			  collect (aref EQ i j))))))
	(loop for j below l do
	  (setq temp-poly (zero domain))
	  (loop for i below D do
	    (setq temp-poly (+ temp-poly
			       (* (expt (coerce
					  (nth (1- k) (ring-variables domain))
					  domain) i)
		    (compute-result
		      (use-chinese-remainder
			(loop for ip below (length primes)
			  collect (poly-form (coefficient (aref poly ip j)
			       (nth (1- k) (ring-variables domain))
					       i)))))))))
	  (setf (aref poly 0 j) temp-poly))
	(loop for j below l do
	  (setq factor (one domain))
	  (loop for k upfrom 0
		as var in (list-of-variables Pkminus1) do
		  (setq factor (* factor (expt (coerce var domain)
					       (aref S j k)))))
	  (setq Pk (+ Pk (* factor (aref poly 0 j)))))
	Pk))

(defgeneric inity (coef-domain s k l)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod InitY ((coef-domain field) S k l)
  (let ((Y '())
	(m '())
	(f 1))
       (loop while (< (length m) l) do
	 (setq m '())
	 (setq Y (loop for i below (1- k)
		       collect (random coef-domain)))
	 (loop for j below l do
	   (setq f 1)
	   (loop for k upfrom 0
		 as pt in Y do
		   (setq f (* f (expt pt (aref S j k)))))
	   (pushnew f m))
	 finally
      (return (values Y m)))))

;; Bp is the function name representing the black box, i.e, the
;; function should return the value of the polynomial at the
;; requested point.
(defmethod interpolate ((domain multivariate-polynomial-ring)
			Bp degree-bounds &rest ignore)
  (declare (ignore ignore))
;  (setq *cur-poly* (coerce (coerce *cur-poly* *general*) domain))
  (let* ((vars (list-coerce (ring-variables domain) domain))
	 (var (first vars))
	 (coef-domain (coefficient-domain-of domain))
	 (pts (loop for i from 1 below (length vars)
	            collect (coerce i coef-domain)))
	 (p (interpolate var (loop for i upto (first degree-bounds)
				   collect i)
		(loop for i upto (first degree-bounds)
		      collect (%funcall
				Bp
				(cons (coerce i coef-domain) pts))))))
	(setq *coefficient-domain* (coefficient-domain-of domain))
	(loop for k from 2 to (length vars) do
	  (setq p (SparseInterpStage
		    p
		    Bp
		    (1+ (nth (1- k) degree-bounds))
		    k)))
	p))

(defgeneric list-coerce (list domain)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod list-coerce ((l list) domain)
  (loop for x in l 
    collect (coerce x domain)))

;; Implementation of the algorithm given in the book by Zippel in
;; the chapter on Multivariate Interpolation.
(defmethod SparseInterpStagei ((coef-domain field)
			       (Pkminus1 mpolynomial) Bp D k
			       &optional ignore)
  (declare (ignore ignore))
  (let* ((domain (domain-of Pkminus1))
	 (vars (list-coerce (list-of-variables Pkminus1) domain))
	 (Skel (pskeleton Pkminus1 vars))
	 (l (length Skel))
	 (S (make-array (list l (1- k)) :initial-contents (reverse Skel)))
	 (Y '())
	 (m '())
	 (cur-pt 0)
	 (X '())
	 (vals '())	 
	 (B '())
	 (EQ (make-array (list D l)))
	 (Pprime '())
	 (factor (one domain))
	 (Pk (zero domain)))
	(setq vals (loop for i below (- (length (ring-variables domain)) k)
			 collect (random coef-domain)))
	(multiple-value-setq (Y m) (InitY coef-domain S k l))
	(loop for i below D do
	  (loop while (member (setq cur-pt (random coef-domain))
			      X))
	  (setq X (append X (list cur-pt)))
	  (setq B (loop for j below l
			collect (%funcall Bp (append
					       (loop for pt in Y
						     collect (expt pt j))
					       (list cur-pt)
					       vals))))
	  (setq Pprime (Solve-vandermondeT m B))
	  (loop for j upfrom 0 as eqn in Pprime do
	    (setf (aref EQ i j) eqn)))
	(loop for j below l do
	  (setq factor (one domain))
	  (loop for k upfrom 0
		as var in (list-of-variables Pkminus1) do
		  (setq factor (* factor (expt (coerce var domain)
					       (aref S j k)))))
	  (setq Pk (+ Pk (* factor
			    (interpolate
			      (coerce (nth (1- k) (ring-variables domain))
				      domain)
			      X
			      (loop for i below D 
				    collect (aref EQ i j)))))))
	Pk))

;;SPMOD-GCD takes two polynomials over the integers and computes their GCD
;;SPMOD-GCD1 takes two polynomials over a field and computes their GCD.

(defun spmod-gcd (p q)
  (let ((b (min (poly-factor-cbound p) (poly-factor-cbound q)))
        (primes (list (newprime))))
    (loop for prod = (first primes) then (* prod (first primes))
          while (> (+ (* 4 b) 2) prod)
          do (push (newprime (first primes)) primes))
    (print b)))

;; Vars is a list var-number and value, so the entries themselves can
;; be used as polynomials representing the variable.
(defun spmod-gcd1 (p q)
  (multiple-value-bind (pc pp) (poly-content-and-prim-part p)
    (multiple-value-bind (qc qp) (poly-content-and-prim-part q)
      (let ((vars (sort (union (poly-list-of-variables pp)
			   (poly-list-of-variables qp))
		    #'cl:<))
	    (plc (lc (poly-terms pp)))
	    (qlc (lc (poly-terms qp)))
	    start-point lc)

	(unless (and (eql (poly-order-number p)
			  (poly-order-number q))
		     (eql (poly-order-number p) (first vars)))
	  (error "Polynomials do not involve the same variables -- SPMOD-GCD"))

    ;; Assuming p and q are primitive

    (setq vars (loop for var on (rest vars)
		     collect (list (first var))))

    (setq lc (poly-gcd plc qlc))

    (setq start-point
	  (loop for var in vars
		collect (list (first var) (random *coefficient-domain*))))

    (poly-times (poly-gcd pc qc)
		(poly-prim-part
		 (spmod-gcd2 lc
			     (poly-times pp (poly-quotient (lc (poly-terms qp)) lc))
			     (poly-times qp (poly-quotient (lc (poly-terms pp)) lc))
			     start-point)))))))

(defun spmod-gcd2 (lc p q vars)
  (cond ((null vars)
	 (poly-times lc (poly-gcdu p q)))
	(t (let ((image (spmod-gcd2 (poly-subst lc (list (first vars)))
				    (poly-subst p (list (first vars)))
				    (poly-subst q (list (first vars)))
				    (rest vars)))
		 (max-terms 0)
		 skel skels rand-vals gcds subst ans) 
	     (map-over-each-term (poly-terms image) (e c)
	       (setq skel (poly-skeleton c (rest vars)))
	       (push (list e skel) skels)
	       (setq max-terms (cl:max max-terms (length skel))))

	     (setq skels (nreverse skels))

	     (push image gcds)
	     (push (second (first vars)) rand-vals)

	     (loop for i below (min (poly-degree p (first vars))
				    (poly-degree q (first vars)))
		   for rand = (random *coefficient-domain*)
		   do (setq subst (list (list (first (first vars)) rand)))
		      (push rand rand-vals)
		      (push (spmod-gcd3 skels
					(poly-subst lc subst)
					(poly-subst p subst)
					(poly-subst q subst)
					(rest vars)
				  max-terms)
			    gcds)
		   #+Testing
		    (print (list 'GCD2 (first subst) (first gcds))))
	     (setq ans 
		   (dense-interpolate-coefficients gcds rand-vals
						   (first vars)))
	     (cond ((and (poly-test-quotient p ans)
			 (poly-test-quotient q ans))
		    ans)		   
		   (t (error "Test divide failed: ~S" ans)))))))

(defun non-zero-random (domain)
  (loop for rand = (random domain)
	do (when (not (0? rand))
	     (return rand))))
		      
(defun spmod-gcd3 (skels lc p q vars max-terms)
  (flet ((check-degree (ans)
	   (when (e> (le ans) (first (first skels)))
	     (error "Degree of skeleton is too large"))
	   (when (e> (first (first skels)) (le ans))
	     (error "Leading coefficient of GCD vanished"))
	   (cons (first p) ans)))
    (if (null vars)
	(check-degree (terms-gcdu+ lc (poly-terms p) (poly-terms q)))
	(let ((rand-vars
	       (loop for (var-num) in vars
		     collect (list var-num
				   (non-zero-random *coefficient-domain*))))
	      subst init-pt gcds coef ans)
	  (setq init-pt (loop for (nil xi) in rand-vars
			      collect xi))

	  (loop for i upfrom 1 below (cl:1+ max-terms) do
	    (setq subst (loop for (var-num xi) in rand-vars
			      collect (list var-num (expt xi i))))
	    (push (terms-gcdu+ (poly-subst lc subst)
			       (poly-terms (poly-subst p subst))
			       (poly-terms (poly-subst q subst)))
		  gcds)
	    #+Testing
	    (print (list 'GCD3 subst (first gcds))))
	  (setq gcds (reverse gcds))
	  (loop for (e skel) in skels
		for vals = ()
		do (loop for g on gcds do
		  (cond ((e> e (le (first g)))
			 (push (zero *coefficient-domain*) vals))
			(t (push (lc (first g)) vals)
			   (setf (first g) (red (first g))))))
		   (setq coef (interpolate-from-skel skel init-pt
							  (reverse vals) vars))
		   (unless (poly-0? coef)
		     (push (cons e coef) ans)))
	  (check-degree (nreverse ans))))))
		
(defun eval-monomial (exps vals)
  (let ((ans (expt (first vals) (first exps))))
    (loop for e in (rest exps)
	  for val in (rest vals)
	  do (setq ans (* ans (expt val e))))
    ans))

(defun interpolate-from-skel (skel init-pt vals vars)
  (let* ((pts (loop for term in skel
		    collect (eval-monomial term init-pt)))
	 (coefs (solve-vandermondetd pts vals))
	 (ans (zero *coefficient-domain*))
	 (one (one *coefficient-domain*)))
    (labels ((make-monomial (c exp vars)
	       (cond ((null exp)
		      c)
		     ((null vars)
		      (error "Not enough variables"))
		     ((e0? (first exp))
		      (make-monomial c (rest exp) (rest vars)))
		     (t (poly-times (poly-simp (first vars)
					       (make-terms (first exp) one))
				    (make-monomial c (rest exp) (rest vars)))))))
      (loop for exps in skel
	    for c in coefs
	    do (unless (poly-0? c)
		 (setq ans (poly-plus ans (make-monomial c exps vars)))))
      ans)))


(defun dense-interpolate-coefficients (poly-list vals var)
  (with-linear-support
    (let ((ans zero) this-var terms-list degree)
      (loop for poly in poly-list do
	(cond ((poly-coef? poly))
	      ((or (null this-var)
		   (more-main? poly this-var))
	       (setq this-var poly))))
      (cond ((null this-var)    
	     ;; Everything is constant at this point, so we can do the dense
	     ;; interpolation and return
	     (poly-interpolate var vals poly-list))
	    (t 
	     ;; Find the maximum degree that appears and turn everything into a 
	     ;; list of terms
	     (loop for poly in poly-list
		   do (push (cond ((poly-coef? poly)
				   (make-terms (e0) poly))
				  ((null degree)
				   (setq degree (le (poly-terms poly)))
				   (setq this-var poly)
				   (poly-terms poly))
				  ((same-variable? this-var poly)
				   (setq degree (max degree (le (poly-terms poly))))
				   (poly-terms poly)))
			    terms-list))
	     (setq terms-list (reverse terms-list))

	     ;; Now work down the list of terms and recurse
	     (loop for deg downfrom degree
		   with sub-polys
		   while (not (eminus? deg))
		   do (setq sub-polys nil) 
		      (loop for terms on terms-list do
			(cond ((null (first terms))
			       (push zero sub-polys))
			      ((e= (le (first terms)) deg)
			       (push (lc (first terms)) sub-polys)
			       (setf (first terms) (red (first terms))))
			      (t (push zero sub-polys))))
		      (setq ans (poly-plus
				 (poly-times (dense-interpolate-coefficients
					      (reverse sub-polys) vals var)
					     (poly-simp this-var (make-terms deg one)))
				     ans)))
	     ans)))))

    
;;; GCD of univariate polynomials over a finite field.
(defun poly-gcdu (p q)
  (cond ((or (poly-coef? p) (poly-coef? q))
	 (one *coefficient-domain*))
	((same-variable? p q)
	 (poly-simp p (terms-gcdu (poly-terms p) (poly-terms q))))
	(t (error "Must be same main variable"))))

(defun terms-gcdu (pt qt)
  (do ()
      ((terms0? qt)
       (if (e0? (le pt))
	   (make-terms (e0) (one *coefficient-domain*))
	   (terms-monicize pt)))
    (psetq pt qt
	   qt (terms-pseudo-remainder pt qt))))

(defun terms-gcdu+ (lc pt qt)
  (terms-mon-times (terms-gcdu pt qt) (e0) lc))

;;;  Testing functions

#+Testing
(defun random-poly (domain degree vars)
  (labels ((random-poly1 (vars)
	     (if (null vars) (random domain)
		 (poly-simp vars
		   (loop for i downfrom degree
			 with c
			 while (not (cl:minusp i))
			 when (and (cl:zerop (cl:random 2))
				   (not (poly-0?
					 (setq c (random-poly1 (rest vars))))))
			   collect (cons i c))))))
    (random-poly1 vars)))

#+Testing
(defun random-poly (degree vars terms)
  (let ((one (one *coefficient-domain*))
	(ans (zero *coefficient-domain*)))
    (labels ((random-monomial (vars)
	       (if (null vars) (one *coefficient-domain*)
		                ;; (random *coefficient-domain*)
		   (poly-times
		    (random-monomial (rest vars))
		    (poly-simp vars
			       (make-terms (cl:random degree) one))))))
      (loop for i below terms
	    do (setq ans (poly-plus ans (random-monomial vars))))
      ans)))

#+Testing
(defun initialize-pq (n deg terms)
  (let ((vars (loop for i below n collect i)))
    (flet ((rand-poly ()
	     (random-poly deg vars terms)
	     #+Ignore
	     (poly-plus `(0 (,(+ 1 deg) . ,(one *coefficient-domain*)))
			(random-poly deg vars terms))))
      (setq g (rand-poly))
      (setq p (poly-times g (rand-poly)))
      (setq q (poly-times g (rand-poly))))))


