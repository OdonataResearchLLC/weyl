;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			      Univariate Polynomial Domain
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; upolynomial.lisp,v 1.6 1994/12/20 22:43:27 sekhar Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.6")

;;; ===========================================================================
;;;			       Univariate Polynomial Elements
;;; ===========================================================================

;;; Univariate polynomials are a vector of coefficients, with the degree
;;; of the term equal to the index in the vector.  (I.e. the leftmost
;;; coefficient is for the x^0 term, and the rightmost for the x^n term).
;;; We explicitly assume that the rightmost coefficient is non-zero,
;;; unless it is a zero-degree polynomial.
;;; Unlike multivariate polynomials, even a zero-degree polynomial will
;;; be represented as a (singleton) vector.  Right now I can't see any
;;; reason to allow a coefficient as a polynomial.
;;; The coef-list vector should never have length 0.

;;; In many places this code assumes that the coefficient domain is an
;;; integral domain.  In the remainder code it assumes the coefficient
;;; domain is a field.

(defmethod scalar? ((x upolynomial))
  (= 1 (length (poly-form x))))

(defmethod 0? ((x upolynomial))
  (coef-list0? (poly-form x)))
  
(defsubst coef-list0? (c)
  (and (= 1 (length c))
       (0? (svref c 0))))

(defmacro coef-list0 (coefficient-domain)
  `(vector (zero ,coefficient-domain)))

(defmethod 1? ((x upolynomial))
  (coef-list1? (poly-form x)))

(defsubst coef-list1? (c)
  (and (= 1 (length c))
       (1? (svref c 0))))

(defmacro coef-list1 (coefficient-domain)
  `(vector (one ,coefficient-domain)))    

;; This produces the polynomial 'x+0' where x is the variable of the ring
(defmacro clist-x (coefficient-domain)
  `(vector (zero ,coefficient-domain) (one ,coefficient-domain)))

(defmacro copy-clist (c)
  `(make-array (length ,c) :initial-contents ,c))

(defmacro clist-degree (c)
  `(cl:- (length ,c) 1))

;; Need the variable argument here because all degree methods need the
;; same number of required arguments.
(defmethod degree ((x upolynomial) variable &rest other-variables)
  (declare (ignore variable other-variables))
  (clist-degree (poly-form x)))

(defmacro clist-zero-deg? (c)
  `(cl:= (length ,c) 1))

(defmacro clist-length (c)     ; Always 1 more than clist-degree, of course.
  `(length ,c))

(defmacro make-clist (length &rest args)
  `(make-array ,length ,@args))

(defmacro clist-get (cl exp)
  `(svref ,cl ,exp))

(defmacro clist-subseq (cl beg end)
  `(subseq ,cl ,beg ,end))

(defgeneric weyl::make-upolynomial (domain coef-list)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod weyl::make-upolynomial
    ((domain multivariate-polynomial-ring) (coef-list array))
  (unless (null (rest (ring-variables domain)))
    (error "Cannot create a univariate polynomial in ~S" domain))
  (let ((coef-domain (coefficient-domain-of domain)))
    (loop for i fixnum below (array-dimension coef-list 0)
	  do (setf (svref coef-list i)
		   (coerce (svref coef-list i) coef-domain)))
    (make-instance 'upolynomial :domain domain 
		   :form (clist-simplify coef-list))))

(defgeneric make-upolynomial (domain coef-list)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod make-upolynomial
    ((domain multivariate-polynomial-ring) (coef-list array))
  (unless (null (rest (ring-variables domain)))
    (error "Cannot create a univariate polynomial in ~S" domain))
  (make-instance 'upolynomial :domain domain 
		 :form (clist-simplify coef-list)))

(defmethod weyl::make-upolynomial
    ((domain multivariate-polynomial-ring) (coef-list list))
  (unless (null (rest (ring-variables domain)))
    (error "Cannot create a univariate polynomial in ~S" domain))
  (let* ((len (length coef-list))
	 (array (make-array len))
	 (coef-domain (coefficient-domain-of domain)))
    (loop for i downfrom (1- len)
	  and #-ANSI-CL for c in coef-list
	  do (setf (svref array i) (coerce c coef-domain)))
    (make-instance 'upolynomial :domain domain 
		   :form array)))

(defmethod make-upolynomial
    ((domain multivariate-polynomial-ring) (coef-list list))
  (unless (null (rest (ring-variables domain)))
    (error "Cannot create a univariate polynomial in ~S" domain))
  (make-instance 'upolynomial :domain domain 
                 :form (clist-simplify
			(make-array (length coef-list)
				    :initial-contents (reverse coef-list)))))

(defmethod make-upolynomial ((domain multivariate-polynomial-ring) coef-list)
  (unless (null (rest (ring-variables domain)))
    (error "Cannot create a univariate polynomial in ~S" domain))
  (make-instance 'upolynomial :domain domain 
                 :form (clist-simplify coef-list)))

(defmethod make-upolynomial
    ((domain multivariate-polynomial-ring) (poly mpolynomial))
  (let* ((var (car (ring-variables (domain-of poly))))
	 (form (poly-form poly))
	 (coef-list (make-array (1+ (degree poly var))
				:initial-element
				(zero (coefficient-domain-of domain)))))
	(if (poly-coef? form)
	    (setf (svref coef-list 0) form)
	    (map-over-each-term (poly-terms form) (e c)
	      (setf (svref coef-list e) c)))
       (make-upolynomial domain coef-list)))


(defmethod print-object ((p upolynomial) stream)
  (print-upolynomial 
     (first (ring-variables (domain-of p)))
     (poly-form p)
     stream))

;; This is still rough with spacing, parenthesis, etc.
(defun print-upolynomial (var clist stream)
  (labels ((print-term (e c)
             (cond ((cl:= e 0)
                    (print-object c stream))
                   (t
		    (if (not (1? c))
			(print-object c stream))
		    #+Genera
		    (format stream "~'i~A~" var)
		    #-Genera
		    (display var stream)
		    (if (cl:> e 1)
			#+Genera
			(format stream "~S" e)
			#-Genera
			(format stream "^~S" e))))))
    (let ((exp (clist-degree clist))
          coef)
      (print-term exp (clist-get clist exp))
      (loop until (cl:zerop exp) do
	(setq exp (- exp 1))
	(setq coef (clist-get clist exp))
	(cond ((0? coef) nil)
	      ((minus? coef)
	       (princ " - " stream)
	       (print-term exp (minus coef)))
	      (t (princ " + " stream)
		 (print-term exp coef)))))))

;;; ===========================================================================
;;;				    POLYNOMIAL ARITHMETIC
;;; ===========================================================================

(defmethod coerce ((poly upolynomial) (domain general-expressions))
  (let* ((var (car (ring-variables (domain-of poly))))
	 (form (poly-form poly)))
	(loop for i below (length form) with exp = 0
	      unless (0? (svref form i))
		do (setq exp (+ exp (* (svref form i)
				       (make-ge-expt domain var i))))
	      finally
	   (return exp))))

;; Make sure the highest degree coefficient is non-zero
(defun clist-simplify (c)
  (let ((end (clist-degree c)))
    (if (or (cl:= end 0)
            (not (0? (clist-get c end))))
        c
        (loop
          (setq end (- end 1))
          (if (or (not (0? (clist-get c end)))
                  (cl:= end 0))
              (return (clist-subseq c 0 (+ end 1)))
              nil)))))

(defmethod-sd plus ((x upolynomial) (y upolynomial))
  (make-upolynomial (domain-of x)
                    (clist-plus (poly-form x) (poly-form y))))

(defun clist-plus (x y)
  (let ((x-deg (clist-degree x))
	(y-deg (clist-degree y))
	array
	exp)
    (flet ((zipper-sum ()
	     (loop for i downfrom exp
		   while (not (cl:minusp i))
		   do (setf (svref array i) (+ (svref x i) (svref y i))))))

      (cond ((cl:> x-deg y-deg)
	     (setq array (make-array (cl:1+ x-deg)))
	     (setq exp y-deg)
	     (loop for i fixnum downfrom x-deg
		   while (cl:>= i y-deg)
		   do (setf (svref array i) (svref x i)))
	     (zipper-sum))
	    ((cl:> y-deg x-deg)
	     (setq array (make-array (cl:1+ y-deg)))
	     (setq exp x-deg)
	     (loop for i downfrom y-deg
		   while (cl:>= i x-deg)
		   do (setf (svref array i) (svref y i)))
	     (zipper-sum))
	    (t (loop for i fixnum downfrom x-deg
		     do 
		      (cond ((cl:minusp i)
			     (setq array (make-array 1))
			     (setf (svref array 0)
				   (zero *coefficient-domain*)))
			    ((not (0? (+ (svref x i) (svref y i))))
			     (setq exp i)
			     (setq array (make-array (cl:1+ exp)))
			     (zipper-sum)
			     (return t))))))
      array)))

(defmethod minus ((x upolynomial))
  (make-upolynomial (domain-of x) (clist-minus (poly-form x))))

(defun clist-minus (x)
  (do ((anslist (make-clist (clist-length x)))
       (exp 0 (+ exp 1)))
      ((cl:= exp (clist-length x)) 
       anslist)
    (setf (clist-get anslist exp) (minus (clist-get x exp)))))

(defmethod minus? ((x upolynomial))
    (clist-minus? (poly-form x)))

(defun clist-minus? (x)
  (minus? (clist-get x (clist-degree x))))

(defmethod-sd difference ((x upolynomial) (y upolynomial))
  (bind-domain-context (domain-of x)
    (make-upolynomial (domain-of x) 
		      (clist-difference (poly-form x) (poly-form y)))))
                                                  
(defun clist-difference (x y)
  (let ((x-deg (clist-degree x))
	(y-deg (clist-degree y))
	array
	exp)
    (flet ((zipper-sum ()
	     (loop for i downfrom exp
		   while (not (cl:minusp i))
		   do (setf (svref array i) (- (svref x i) (svref y i))))))

      (cond ((cl:> x-deg y-deg)
	     (setq array (make-array (cl:1+ x-deg)))
	     (setq exp y-deg)
	     (loop for i fixnum downfrom x-deg
		   while (cl:>= i y-deg)
		   do (setf (svref array i) (svref x i)))
	     (zipper-sum))
	    ((cl:> y-deg x-deg)
	     (setq array (make-array (cl:1+ y-deg)))
	     (setq exp x-deg)
	     (loop for i downfrom y-deg
		   while (cl:>= i x-deg)
		   do (setf (svref array i) (- (svref y i))))
	     (zipper-sum))
	    (t (loop for i fixnum downfrom x-deg do 
	         (cond ((cl:minusp i)
			(setq array (make-array 1))
			(setf (svref array 0)
			      (zero *coefficient-domain*))
			(return t))
		       ((not (0? (- (svref x i) (svref y i))))
			(setq exp i)
			(setq array (make-array (cl:1+ exp)))
			(zipper-sum)
			(return t))))))
      array)))

(defmethod-sd times ((x upolynomial) (y upolynomial))
  (make-upolynomial (domain-of x) 
		    (clist-times (poly-form x) (poly-form y))))

(defun clist-times (x y)
  (declare (type simple-array x y)
	   (optimize (safety 0)))
  (let* ((xlen (clist-length x))
         (ylen (clist-length y))
         (anslist (make-array (cl:- (the fixnum (cl:+ xlen ylen)) 1)
			      :initial-element (zero *coefficient-domain*))))
    (declare (fixnum xlen ylen)
	     (type simple-array anslist))
    (do ((xexp 0 (cl:+ xexp 1)))
        ((cl:= xexp xlen)
         anslist)                        ; return this when done
      (declare (fixnum xexp))
      (do ((xelt (clist-get x xexp) )
           (yexp 0 (cl:+ yexp 1))
           (ansexp xexp (cl:+ ansexp 1)))
          ((cl:= yexp ylen))
	(declare (fixnum yexp ansexp))
        (setf (clist-get anslist ansexp)
              (+ (clist-get anslist ansexp)
		 (* xelt (clist-get y yexp))))))))

;;; In CLIST-EXPT the second argument is a repetition count, and thus
;;; is actually an integer.

(defmethod expt ((base upolynomial) (expt integer))
  (bind-domain-context (domain-of base)
    (make-upolynomial (domain-of base)
		      (clist-exptsq (poly-form base) expt))))

(defun clist-exptsq (c n)
  (%funcall (repeated-squaring #'clist-times (coef-list1 *coefficient-domain*))
           c n))

;; This assumes the coefficient domain is a field
(defmethod-sd quotient ((x upolynomial) (y upolynomial))
  (bind-domain-context (domain-of x)
    (make-upolynomial (domain-of x)
		      (clist-quotient (poly-form x) (poly-form y)))))

(defun clist-quotient (x y)
  (cond ((coef-list0? y)
         (error "Attempt to divide ~S by zero univariate polynomial ~S" x y))
        ((cl:> (clist-degree y) (clist-degree x))
         (error "Attempt to divide ~S by uni. poly. of higher degree ~S" x y))
        (t
         (multiple-value-bind (q r) (clist-divide x y)
           (if (coef-list0? r) 
               q
               (error "Quotient of ~S, and ~S not exact" x y))))))

(defmethod-sd remainder ((x upolynomial) (y upolynomial))
  (bind-domain-context (domain-of x)
    (make-upolynomial (domain-of x)
		      (clist-remainder (poly-form x) (poly-form y)))))

(defun clist-remainder (x y)
  (cond ((coef-list0? y)
         (error "Attempt to divide ~S by zero univariate polynomial ~S" x y))
        (t
         (multiple-value-bind (q r) (clist-divide x y)
	   (declare (ignore q))
           r))))
         
;; This could be improved.  clist-divide should be merged with
;; clist-remainder and clist-quotient.  That way we wouldn't be
;; generating a remainder whe we only want a quotient and vice versa.
;; --RZ (FIXTHIS)

(defun clist-divide (x y)
  (cond ((coef-list0? y)
         (error "Attempt to divide by zero univariate polynomial ~S" y))
        ((coef-list1? y)
         (values x (coef-list0 *coefficient-domain*)))
        ((cl:> (clist-degree y) (clist-degree x))
         (values (coef-list0 *coefficient-domain*) x))
        (t
         (let* ((xdeg (clist-degree x))
                (ydeg (clist-degree y))
                (yhigh (clist-get y ydeg))
                (qdeg (cl:- xdeg ydeg))
                (rem (copy-clist x))
                (quot (make-clist (cl:+ qdeg 1))))
           (do* ((qexp qdeg (cl:- qexp 1)))
                ((cl:= qexp -1)
                 (values quot (clist-simplify rem)))
             (let ((c (/ (clist-get rem (cl:+ ydeg qexp)) yhigh)))
               (setf (clist-get quot qexp) c)
               (do ((yexp ydeg (- yexp 1)))
                   ((cl:= yexp -1))
                 (setf (clist-get rem (+ yexp qexp))
                            (- (clist-get rem (+ yexp qexp))
			       (* c (clist-get y yexp)))))))))))

;;; This gcd uses Euclid's algorithm and the above remainder function.
(defmethod-sd binary-gcd ((x upolynomial) (y upolynomial))
  (bind-domain-context (domain-of x)
    (make-upolynomial (domain-of x)
		      (clist-gcd (poly-form x) (poly-form y)))))

(defun clist-gcd (x y)
  (do ((a x b)
       (b y (clist-remainder a b)))
      ((coef-list0? b)
       a)
    ))

(defmethod-sd binary-lcm ((x upolynomial) (y upolynomial))
  (bind-domain-context (domain-of x)
    (make-upolynomial (domain-of x)
		      (clist-lcm (poly-form x) (poly-form y)))))

(defun clist-lcm (x y)
  (clist-times (clist-quotient x (clist-gcd x y)) y))

;; Compute the gcd d of a,b and upolynomials x,y such that ax + by = d.
;; This method works for univariate polynomial ring over a field.
(defmethod extended-gcd ((a upolynomial) (b upolynomial))
  (let ((domain (domain-of a)))
       (setq *coefficient-domain* (coefficient-domain-of domain))       
       (multiple-value-bind
	   (d x y) (clist-extended-gcd (poly-form a) (poly-form b))
	 (values (make-upolynomial domain d)
		 (make-upolynomial domain x)
		 (make-upolynomial domain y)))))

(defun clist-extended-gcd (a b)
  (if (coef-list0? b)
      (values a (make-array 1 :initial-element 1)
	      (make-array 1 :initial-element 0))
      (multiple-value-bind (q r) (clist-divide a b)
	(multiple-value-bind (d x y) (clist-extended-gcd b r)
	  (values d y (clist-difference x (clist-times q y)))))))

;; I don't know why this routines was ever here (but there's a similar
;; routine in mpolynomial).  --RZ (FIXTHIS)
#+ignore
(defmethod-sd binary> ((x upolynomial) (y upolynomial))
  (let ((cl-x (poly-form x))
	(cl-y (poly-form y)))
    (cond ((and (clist-zero-deg? cl-x) (clist-zero-deg? cl-y))
           (> (clist-get cl-x 0) (clist-get cl-y 0)))
          (t nil))))

(defmethod-sd binary= ((x upolynomial) (y upolynomial))
  (bind-domain-context (domain-of x)
    (coef-list0? (clist-difference (poly-form x) (poly-form y)))))

;; this should return a list of (polynomial . power) pairs
(defmethod factor ((poly upolynomial))
  (bind-domain-context (domain-of poly)
    (if (typep *coefficient-domain* 'gfp)
	(if (cl:< 2 (characteristic *coefficient-domain*))
	    (mapcar #'(lambda (pair)
		        (cons (make-upolynomial *domain* (car pair))
			      (cdr pair)))
		    (clist-factor (poly-form poly)))
	    (error "Can't factor univariate polynomials over GF(2) yet"))
	(error "Can't factor unless coefficients domain is GF(p)"))))

(defmethod cfactor ((poly upolynomial))
  (let* ((factlist (factor poly))
         (prod (one (domain-of poly))))
    (mapcar #'(lambda (fact) 
	        (setq prod (* prod (expt (car fact) (cdr fact))))
	      t)
            factlist)
    (if (= prod poly)
        factlist
        (error "Factor didn't work.  Prod ~S not = ~S" factlist poly))))


;; This should return a list of (clist . power) pairs
(defun clist-factor (poly)
  (if (coef-list0? (clist-deriv poly))
      (if (cl:= (clist-degree poly) 0)
          (list (cons poly 1))
          (let ((p (characteristic *coefficient-domain*)))
            (mapcar #'(lambda (fact)
                        (cons (car fact) (cl:* p (cdr fact))))
                    (clist-factor (clist-pth-root poly)))))
      (loop for (pp . degree) in (clist-sqfr-decompose poly)
	    append (loop for p in (clist-factor-squarefree pp)
			 collect (cons p degree)))))

;; The exclamation point indicatest that this routine modifies its
;; argument.
(defun clist-primitive-part! (poly)
  (let* ((deg (clist-degree poly))
	 (lc (clist-get poly deg)))
    (unless (1? lc)
      (loop for i fixnum below deg
	    do (setf (clist-get poly i) (/ (clist-get poly i) lc)))
      (setf (clist-get poly deg) (one *coefficient-domain*)))
    poly))
	     
	  
;; As currently constituted this routine first monicizes the
;; polynomial and then computes the primitive part.  It should just
;; multiply it into the square free terms.  --RZ (FIXTHIS)
(defmethod square-free ((poly upolynomial))
  (bind-domain-context (domain-of poly)
    (mapcar #'(lambda (pair)
	        (cons (make-upolynomial *domain* (car pair))
		      (cdr pair)))
	    (clist-sqfr-decompose (poly-form poly)))))

(defun clist-sqfr-decompose (poly)
  (loop for f1 = poly then f2
	for n upfrom 0
	for prod1 = nil then prod2
	with factor and factors and deriv and f2 and prod2 and
	p = (characteristic *coefficient-domain*)
	do (when (coef-list0? (setq deriv (clist-deriv f1)))
	     (push (cons (clist-primitive-part! prod1) n) factors)
	     (unless (cl:= (clist-degree f1) 0)
	       (push (cons (clist-primitive-part! (clist-pth-root f1)) p)
		     factors))
	     (return (reverse factors)))
	   (setq f2 (clist-gcd deriv f1))
	   (setq prod2 (if (cl:> (clist-degree f2) 0)
			    (clist-quotient f1 f2)
			    f1))
	   (when prod1
	     (setq factor (clist-quotient prod1 prod2)))
	   (when (cl:plusp (clist-degree factor))
	     (push (cons (clist-primitive-part! factor) n) factors))))

(defun clist-deriv (f)
  (if (cl:= (clist-degree f) 0)
      (coef-list0 *coefficient-domain*)
      (let ((deriv (make-clist (cl:- (clist-length f) 1))))
        (do ((exp (clist-degree f) (- exp 1)))
            ((cl:= exp 0)
             (clist-simplify deriv))
          (setf (clist-get deriv (- exp 1))
                (* (coerce exp *coefficient-domain*)
		   (clist-get f exp)))))))

(defmethod derivation ((f upolynomial))
  (bind-domain-context (domain-of f)
    (make-upolynomial (domain-of f)
		      (clist-deriv (poly-form f)))))

;; this assumes that poly is a proper pth power -- i.e. that its 
;; degree is a multiple of p, and all non-zero terms have degree
;; a multiple of p.  in this case, the pth root is just the polynomial
;; generated by dividing all the exponents by p.
(defun clist-pth-root (poly)
  (let* ((p (characteristic *coefficient-domain*))
         (rootdegree (cl:/ (clist-degree poly) p))
         (root (make-clist (cl:+ rootdegree 1))))
    (do ((pexp (clist-degree poly) (cl:- pexp p))
         (rexp rootdegree (cl:- rexp 1)))
        ((cl:= rexp -1)
         root)
      (setf (clist-get root rexp) (clist-get poly pexp)))))

;; this assumes that poly is square-free, and returns a list of factors
;; (as clists).
(defun clist-factor-squarefree (poly)
  (if (cl:= (clist-degree poly) 0)
      (list poly)
      (let ((factlist nil)
            (x (clist-x *coefficient-domain*))    ; the poly 'x'
            (p (characteristic *coefficient-domain*))
            dpoly)
        (when (0? (clist-get poly 0))
	  (setq factlist (append (list x) factlist))
	  (setq poly (clist-quotient poly x)))
        (do ((n 1 (+ n 1))
             (power p (* power p))    ; always equals p^n
             (f poly (cond ((cl:= (clist-degree dpoly) 0) f)
                           ((cl:= (clist-degree dpoly) (clist-degree f))
                            (coef-list1 *coefficient-domain*))
                           (t (clist-quotient f dpoly)))))
            ((cl:= (clist-degree f) 0)
             factlist)
          (cond ((cl:< (clist-degree f) (cl:* 2 n))
		 (setq n (clist-degree f))
		 (setq dpoly f))
		(t (setq dpoly (clist-gcd f (clist-difference 
					     (clist-expt-mod-poly x power f)
					     x)))))
          (if (cl:> (clist-degree dpoly) 0)
              (setq factlist 
                    (append factlist
                            (clist-factor-product-nth-degrees dpoly n))))))))
          
;; this evaluates (x)^n mod poly by repeated squaring
(defun clist-expt-mod-poly (x n poly)
  (%funcall (repeated-squaring
	      #'(lambda (a b) (clist-remainder (clist-times a b) poly))
	      (coef-list1 *coefficient-domain*))
	    x n))

;; this assumes that poly is a product of nth degree irreducible factors.
;; it returns a list of factors (as clists, of course)
(defun clist-factor-product-nth-degrees (poly n)
  (if (cl:= (clist-degree poly) n)
      (list poly)
      (let ((exp (/ (- (expt (characteristic *coefficient-domain*) n) 1) 2))
            (factlist nil)
            (base (clist-x *coefficient-domain*)))  ; the poly 'x+0'
        (do ((reducibles (list poly) newreducibles)
             (newreducibles nil nil))
            ((null reducibles)
             factlist)
          (dolist (f reducibles)
            (let* ((q1 (clist-gcd f
                           (clist-plus (clist-expt-mod-poly base exp f)
                                       (coef-list1 *coefficient-domain*))))
                   (q2 (if (cl:= (clist-degree q1) 0)
                           f
                           (clist-quotient f q1))))
              (cond ((cl:> (clist-degree q1) n) (push q1 newreducibles))
                    ((cl:= (clist-degree q1) n) (push q1 factlist)))
              (cond ((cl:> (clist-degree q2) n) (push q2 newreducibles))
                    ((cl:= (clist-degree q2) n) (push q2 factlist)))))
          (setf (clist-get base 0)
                     (random *coefficient-domain*)))))) ; reset base to 'x+a'



