;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			      Expanded Polynomials
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; epolynomial.lisp,v 1.9 1994/11/15 19:55:25 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.9")

(defgeneric make-epolynomial* (domain greater-function terms)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod make-epolynomial*
    ((domain multivariate-polynomial-ring) greater-function terms)
  (make-instance 'epolynomial :domain domain
		 :greater-function greater-function
		 :form terms))

(defgeneric make-epolynomial (domain greater-function poly)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod make-epolynomial
    ((domain multivariate-polynomial-ring) greater-function (poly mpolynomial))
  (make-epolynomial domain greater-function (poly-form poly)))

(defmethod make-epolynomial
    ((domain multivariate-polynomial-ring) greater-function (poly epolynomial))
  (if (eql greater-function (greater-function-of poly)) poly
      (make-epolynomial* domain greater-function
			 (sort (poly-form poly) greater-function))))

(defmethod make-epolynomial 
    ((domain multivariate-polynomial-ring) greater-function
     (r rational-function))
  (let ((c (coercible? r (coefficient-domain-of domain)))
	term)
    (cond ((null c) (call-next-method))
	  ((0? c)
	   (make-epolynomial* domain greater-function ()))
	  (t (setq term (make-array (cl:1+ (length (ring-variables domain)))
				    :initial-element 0))
	     (setf (aref term 0) c)
	     (make-epolynomial* domain greater-function (list term))))))

(defmethod make-epolynomial
	   ((domain multivariate-polynomial-ring) greater-function (form list))
  (let* ((dimension (length (ring-variables domain)))
	 poly-form)
    (labels ((scan-poly-form (form exp next-var)
	       (cond ((poly-coef? form)
		      (unless (0? form)
			(loop for i fixnum upfrom next-var below dimension
			      do (push 0 exp))
			(push (make-array (1+ dimension) 
					  :initial-contents
					  (cons form (reverse exp)))
			      poly-form)))
		     (t (let ((var-index (poly-order-number form)))
			  (loop for i fixnum upfrom next-var below var-index
				do (push 0 exp))
			  (setq var-index (1+ var-index))
			  (map-over-each-term (poly-terms form) (e c)
			    (scan-poly-form c (cons e exp)
					    var-index)))))))
      (scan-poly-form form () 0)
      (setq greater-function 
	    (get-comparison-fun dimension greater-function))
      (make-epolynomial* domain greater-function
			(sort poly-form greater-function)))))
	
(defmethod print-object ((poly epolynomial) stream)
  (if (0? poly) (princ 0 stream)
      (with-slots (var-count) poly
        (let* ((first? t)
	       (variables (ring-variables (domain-of poly)))
	       (dim (cl:1+ (length variables))))
	  (loop for term in (poly-form poly)
		for c = (svref term 0)
		do (cond ((minus? c)
			  (setq c (minus c))
			  (if (and (1? c) (not (gterm-constant? term dim)))
			      (princ " -" stream)
			      (format stream " - ~S" c)))
			 ((null first?)
			  (if (and (1? c) (not (gterm-constant? term dim)))
			      (princ " + " stream)
			      (format stream " + ~S" c)))
			 (t (unless (and (1? c) (not (gterm-constant? term dim)))
			      (format stream "~S" c))))
	    (loop for var in variables
		  for i fixnum upfrom 1
		  for exp = (svref term i) do
              (unless (cl:zerop exp)
		(princ " " stream)
		(display var stream)
		(unless (eql 1 exp)
		  (format stream "^~D" exp))))
		(setq first? nil))))))

(defmethod 0? ((x epolynomial))
  (null (poly-form x)))

(defmethod 1? ((x epolynomial))
  (let ((form (poly-form x)))
    (and (null (rest form))
	 (0? (le form))
	 (1? (lc form)))))

(defmethod binary= ((p epolynomial) (q epolynomial))
  (0? (- p q)))

(defun get-comparison-fun (num-vars name)
  (cond ((and (not (symbolp name)) (functionp name))
	 name)
	((fboundp name) (symbol-function name))
	((eql name :lexical)
	 (make-comparison-fun num-vars 
			      (loop for i fixnum below num-vars 
				    collect (cl:1+ i))
			      :total? nil :reverse? nil))
	((eql name :revlex)
	 (make-comparison-fun num-vars 
			      (reverse
				(loop for i fixnum below num-vars
				      collect (cl:1+ i)))
			      :total? t :reverse? t))
	((eql name :total-lexical)
	 (make-comparison-fun num-vars
			      (loop for i fixnum below num-vars
				    collect (cl:1+ i))
			      :total? t :reverse? nil))
	((eql name :total-revlex)
	 (make-comparison-fun num-vars
			      (reverse 
				(loop for i fixnum below num-vars
				      collect (cl:1+ i)))
			      :total? t :reverse? t))
	(t (error "Unknown comparison function ~S" name))))

(defun make-comparison-fun (num-vars var-order &key total? reverse? new?)
 (let ((name (intern (format nil "COMPARE<~D>-~D~{.~D~}-~S-~S"
			     num-vars (first var-order) (rest var-order)
			     total? reverse?)))
       great less)
   (if (null reverse?)
       (setq great 'cl:> less 'cl:<)
       (setq great 'cl:< less 'cl:>))
   (unless (and (null new?) (fboundp name))
     (compile name 
       `(lambda (a b)
	  (let (a-temp b-temp)
	       (declare (fixnum a-temp b-temp)
			(optimize (safety 0)))
	    (cond
	      ,@(when total?
		  `(((cl:plusp
		       ,(if (> num-vars 10)
			    `(loop for i fixnum upfrom 1 below ,num-vars
				   summing
				   (the fixnum
					(- (the fixnum (svref a i))
					   (the fixnum (svref b i)))))
			    `(cl:+ ,@(loop for i upfrom 1 below num-vars
					   collect
					   `(the fixnum
						 (- (the fixnum (svref a ,i))
						    (the fixnum (svref b ,i))))))))
		     t)
		    ((cl:< a-temp b-temp) nil)))
	      ,@(loop for v in var-order
		      append `(((,great (setq a-temp (svref a ,v))
				      (setq b-temp (svref b ,v)))
				   t)
			       ((,less a-temp b-temp) nil))))))))
   (values (symbol-function name) name)))

(defmethod make-polynomial 
    ((domain multivariate-polynomial-ring) (epol epolynomial))
  (unless (eql domain (domain-of epol))
    (error "Don't understand this: ~S" epol))
  (let* ((terms (poly-form epol))
         (dim (cl:- (array-dimension (first terms) 0) 1)))
    (labels ((convert-term (term i)
	       (cond ((cl:zerop i) (aref term 0))
		     ((cl:zerop (aref term i))
		      (convert-term term (cl:- i 1)))
		     (t (cons (cl:- i 1) 
			      (make-terms (aref term i) 
					  (convert-term term (cl:- i 1))))))))
      (loop with ans = (convert-term (first terms) dim)
	    for term in (rest terms)
	    do (setq ans (poly-plus ans (convert-term term dim)))
	    finally (return (make-polynomial domain ans))))))
  
(defmethod coerce ((p epolynomial) (d general-expressions))
  (let* ((domain (domain-of p))
	 (vars (ring-variables domain))
	 (n (length vars)))
    (labels ((transform-term (term)
	       (let ((factors (list (coerce (aref term 0) d))))
		 (loop for i below n
		       for e = (aref term (cl:1+ i))
		       do (unless (cl:zerop e)
			    (push (expt (nth i vars) e) factors)))
		 (simp-times-terms d factors))))
       (simp-plus-terms d (loop for term in (poly-form p)
				collect (transform-term term))))))

;;; ===========================================================================
;;;			Polynomial Arithmetic
;;; ===========================================================================

;; Notice that we can use MAP-OVER-EACH-TERM since it doesn't depend
;; upon the exponent arithmetic, while UPDATE-TERMS does.

(defmacro same-greater-functions ((x y) &body body)
  `(with-slots (greater-function) x
     (unless (eq greater-function (slot-value ,y 'greater-function))
       (error "EPolynomials don't have same compare function: ~S and ~S"
	      ,x ,y))
    ,@body))

(defun make-eterm (exp coef)
  (declare (type simple-array exp)
	   (optimize (safety 0)))
  (let* ((dim (array-dimension exp 0))
	 (term (make-array dim)))
    (declare (fixnum dim)
	     (type simple-array term))
    (setf (svref term 0) coef)
    (do ((i 1 (cl:1+ i)))
	((cl:> i dim) term)
      (declare (fixnum i))
      (setf (svref term i) (svref exp i)))))

;; These should really check to make sure that the number of variables
;; hasn't changed.
(defmethod-sd plus ((x epolynomial) (y epolynomial))
  (same-greater-functions (x y)
    (bind-domain-context domain
      (make-epolynomial* domain greater-function
			 (gterms-plus greater-function (poly-form x) (poly-form y))))))

(defun gterms-plus (greater-function x y) ;x and y are term lists
  (let ((ans-terms (list nil))
	(terms nil)
	sum)
    (macrolet
	((collect-term (.e. .c.)
	   `(progn (setf (rest terms) (list (make-eterm , .e. , .c.)))
		   (setf terms (rest terms))))
	 (collect-old-term (term)
	   `(progn (setf (rest terms) (list ,term))
		   (setf terms (rest terms)))))
      (setq terms ans-terms)
      (loop
       (cond ((terms0? x)
	      (cond ((terms0? y) (return (rest ans-terms)))
		    (t (collect-old-term (lt y))
		       (setq y (red y)))))
	     ((or (terms0? y)
		  (%funcall greater-function (lt x) (lt y)))
	      (collect-old-term (lt x))
	      (setq x (red x)))
	     ((%funcall greater-function (lt y) (lt x))
	      (collect-old-term (lt y))
	      (setq y (red y)))
	     (t (setq sum (+ (svref (lt x) 0) (svref (lt y) 0)))
		(unless (0? sum)
		  (collect-term (lt x) sum))
		(setq x (red x) y (red y))))))))

(defmethod minus ((x epolynomial))
  (let ((domain (domain-of x)))
    (bind-domain-context domain      
      (make-epolynomial* domain (slot-value x 'greater-function)
			 (gterms-minus (poly-form x))))))

(defun gterms-minus (x)
  (loop for term in x
	collect (make-eterm term (- (svref term 0)))))

(defmethod-sd difference ((x epolynomial) (y epolynomial))
  (same-greater-functions (x y)
    (bind-domain-context domain
      (make-epolynomial* domain greater-function
			 (gterms-difference greater-function
					    (poly-form x) (poly-form y))))))

(defun gterms-difference (greater-function x y) ;x and y are term lists
  (let ((ans-terms (list nil))
	(terms nil)
	sum)
    (macrolet
	((collect-term (.e. .c.)
	   `(progn (setf (rest terms) (list (make-eterm , .e. , .c.)))
		   (setf terms (rest terms))))
	 (collect-old-term (term)
	   `(progn (setf (rest terms) (list ,term))
		   (setf terms (rest terms)))))
      (setq terms ans-terms)
      (loop
       (cond ((terms0? x)
	      (cond ((terms0? y) (return (rest ans-terms)))
		    (t (collect-term (lt y) (- (elt y 0)))
		       (setq y (red y)))))
	     ((or (terms0? y)
		  (%funcall greater-function (lt x) (lt y)))
	      (collect-old-term (lt x))
	      (setq x (red x)))
	     ((%funcall greater-function (lt y) (lt x))
	      (collect-term (lt y) (- (elt (lt y) 0)))
	      (setq y (red y)))
	     (t (setq sum (- (elt (lt x) 0) (elt (lt y) 0)))
		(unless (0? sum)
		  (collect-term (lt x) sum))
		(setq x (red x) y (red y)))))))) 

(defmethod-sd times ((x epolynomial) (y epolynomial))
  (same-greater-functions (x y)
    (bind-domain-context domain
      (make-epolynomial* domain greater-function
			 (gterms-times greater-function
				       (poly-form x) (poly-form y))))))

;; Assumes we are working over an integral domain.
(defun gterms-mon-times (poly-terms term)
  (let ((c (svref term 0))
	(dim (array-dimension term 0)))
    (if (0? c) (terms0)
	(loop for pterm in poly-terms
	      collect (let ((nterm (make-array dim)))
			(setf (svref nterm 0)
			      (* (svref pterm 0) (svref term 0)))
			(loop for i fixnum upfrom 1 below dim
			      do (setf (svref nterm i)
				       (cl:+  (svref pterm i)
						(svref term i))))
			nterm)))))

(defun gterm-times (x-term y-term dim)
  (declare (type simple-array x-term y-term)
	   (optimize (safety 0)))
  (let ((e (make-array dim)))
    (loop for i fixnum upfrom 1 below dim
	  do (setf (svref e i)
		   (the fixnum (cl:+ (the fixnum (svref x-term i))
				       (the fixnum (svref y-term i))))))
    e))

(defun gterm-quot (x-term y-term dim)
  (declare (type simple-array x-term y-term)
	   (optimize (safety 0)))
  (let ((e (make-array dim)))
    (loop for i fixnum upfrom 1 below dim
	  do (setf (svref e i)
		   (the fixnum (cl:- (the fixnum (svref x-term i))
				       (the fixnum (svref y-term i))))))
    e))

(defun gterm-lcm (x-term y-term dim)
  (declare (type simple-array x-term y-term)
	   (optimize (safety 0)))
  (let ((e (make-array dim)))
    (loop for i fixnum upfrom 1 below dim
	  do (setf (svref e i) (cl:max (the fixnum (svref x-term i))
					(the fixnum (svref y-term i)))))
    e))

(defun gterm-disjoint (x-term y-term dim)
  (declare (type simple-array x-term y-term)
	   (optimize (safety 0)))
  (loop for i fixnum upfrom 1 below dim
	when (not (or (cl:zerop (the fixnum (svref x-term i)))
		      (cl:zerop (the fixnum (svref y-term i)))))
	  do (return nil)
	finally (return t)))

(defun gterm-dominates (x-term y-term dim)
  (declare (type simple-array x-term y-term)
	   (optimize (safety 0)))
  (loop for i fixnum upfrom 1 below dim
	when (cl:< (the fixnum (svref x-term i))
		     (the fixnum (svref y-term i)))
	  do (return nil)
	finally (return t)))

(defun gterm-equal (x-term y-term dim)
  (declare (type simple-array x-term y-term)
	   (optimize (safety 0)))
  (loop for i fixnum upfrom 1 below dim
	do (when (not (cl:= (the fixnum (svref x-term i))
			      (the fixnum (svref y-term i))))
	     (return nil))
	finally (return t)))

(defun gterm-constant? (term dim)
  (declare (type simple-array term)
	   (optimize (safety 0)))
  (loop for i fixnum upfrom 1 below dim
	do (when (not (cl:zerop (the fixnum (svref term i))))
	     (return nil))
	finally (return t)))

(defun gterms-times (greater-function x y)
  (let (;; Multiply x by the first term of y.  This is the initial
	;; term list we will modify.
	(answer (gterms-mon-times x (lt y))) 
	(dim (length (first x)))
	e c)
    (setq answer (cons nil answer))
    (loop for y-term in (red y)
	  for ans = answer do
      (loop for x-term in x do
	(unless (0? (setq c (* (svref x-term 0) (svref y-term 0))))
	  (setq e (gterm-times x-term y-term dim))
	  ;; Find place to insert this term.
	  (loop	for red-ans = (red ans) do
	    ;; Sure would be nice if the complier recognized and optimized
	    ;; the usages of (red ans)
	    (cond ((or (terms0? red-ans)
		       (%funcall greater-function e (lt red-ans)))
		   (setf (svref e 0) c)
		   (setf (red ans) (list e))
		   (return t))	
		  ((gterm-equal e (lt red-ans) dim)
		   (setf (svref (lt red-ans) 0)
			 (+ (svref (lt red-ans) 0) c))
		   (return t))
		  (t (setq ans red-ans)))))))
    (loop for ans on answer
	  do (when  (and (red ans) (0? (svref (lt (red ans)) 0)))
	       (setf (red ans) (red (red ans)))))
    (red answer)))

(defmethod expt ((base epolynomial) (expt integer))
  (let ((domain (domain-of base))
	(cf (greater-function-of base)))
    (bind-domain-context domain
      (make-epolynomial* domain cf (gterms-expt cf (poly-form base) expt)))))

(defun gterms-expt (cf terms exp)
  (cond ((e0? exp)
	 (list (make-eterm (lt terms) (one *coefficient-domain*))))
	(t (let ((ans terms))
	     (loop for i below exp
		   do (setf ans (gterms-times cf ans terms)))
	     ans))))


(defmethod make-mpolynomial
    ((domain multivariate-polynomial-ring) (poly epolynomial))
  (let* ((dimension (length (ring-variables domain))))
    (labels ((convert-term (term i)
	       (cond ((> i dimension)
		      (svref term 0))
		     ((cl:zerop (aref term i))
		      (convert-term term (cl:+ i 1)))
		     (t `(,(cl:- i 1)
			   (,(aref term i) .
			      ,(convert-term term (cl:+ i 1))))))))
      (make-polynomial domain
	(loop for term in (poly-form poly)
	      for sum = (convert-term term 1)
		then (poly-plus sum (convert-term term 1))
	      finally (return sum))))))
