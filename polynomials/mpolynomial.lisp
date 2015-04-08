;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			      General Polynomial Domain
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; mpolynomial.lisp,v 1.10 1995/05/24 17:42:05 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.10")

(define-domain-element-classes multivariate-polynomial-ring
  mpolynomial epolynomial)

(defmethod initialize-instance :after ((d multivariate-polynomial-ring)
				       &rest plist)
  (declare (ignore plist))
  (with-slots (zero one coefficient-domain) d
    (setq zero (make-polynomial d (zero coefficient-domain)))
    (setq one (make-polynomial d (one coefficient-domain)))))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator polynomial-ring
      ((coefficient-domain domain) (variables list))
    (let ((domain (make-instance 'multivariate-polynomial-ring
                                 :variables (loop for var in variables
                                                  collect (coerce var *general*))
                                 :coefficient-domain coefficient-domain
                                 :print-function 'polynomial-ring-print-object)))
      (make-homomorphism coefficient-domain
                         #'(lambda (c) (make-polynomial domain c))
                         domain)
      domain)))

(defun polynomial-ring-print-object (d stream)
  (format stream "~A[" (coefficient-domain-of d))
  (display-list (ring-variables d) stream)
  (princ "]" stream))

(defgeneric get-polynomial-ring (domain variables)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod get-polynomial-ring
    ((coefficient-domain domain) (variables list))
  (let ((kernels ()))
    (loop for var in variables
	  do (loop for v in (different-kernels (coerce var *general*)
                                               kernels)
		   do (pushnew v kernels :test #'ge-equal)))
    (setq kernels (nreverse kernels))
    (add-domain #'(lambda (d)
		    (and (typep d 'polynomial-ring)
			 (eql (coefficient-domain-of d) coefficient-domain)
			 (ge-lequal (ring-variables d) kernels)))
      (make-polynomial-ring* coefficient-domain kernels))))

;;; ===========================================================================
;;;			       Polynomial Elements
;;; ===========================================================================

;;; Polynomials are a structure consisting of three parts: an order
;;; number, the variable at this level, and a list of the terms of the
;;; polynomial.  Term-list are exponent coefficient pairs.

;; Polynomials  := <coef> | (<var-number> . <term-list>)
;; term-list := nil | ((<exponent> . <coefficient>) . <term-list>)

(defmacro poly-order-number (poly)
  `(first ,poly))

(defmacro poly-terms (poly)
  `(rest ,poly))

(defmacro poly-coef? (x)
  `(not (listp ,x)))

(defgeneric scalar? (object)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod scalar? ((x mpolynomial))
  (poly-coef? (poly-form x)))

(defsubst poly-0? (x)
  (and (poly-coef? x) (0? x))) 

(defmethod 0? ((x mpolynomial))
  (poly-0? (poly-form x)))

(defsubst poly-1? (x)
  (and (poly-coef? x) (1? x)))

(defmethod 1? ((x mpolynomial))
  (poly-1? (poly-form x)))

(defun make-poly-form (poly terms)
  (cons (poly-order-number poly) terms))

(defgeneric make-polynomial (domain form)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod make-polynomial ((domain multivariate-polynomial-ring) form)
  (make-instance 'mpolynomial :domain domain :form form))

(defmethod variable-symbol ((domain variable-hash-table) (poly mpolynomial))
  (aref (variable-index-table domain) (poly-order-number (poly-form poly)) 0))

(defsubst more-main? (x y)
  (cl:< (poly-order-number x) (poly-order-number y)))

(defun more-main-order-number? (x y) (cl:< x y))

(defsubst same-variable? (x y)
  (cl:= (poly-order-number x) (poly-order-number y)))

;; The following macros are for dealing with term-lists.

(defmacro le (terms) `(first  (first ,terms)))

(defmacro lc (terms) `(rest (first ,terms)))

(defmacro lt (terms) `(first ,terms))

(defmacro red (terms) `(rest ,terms))

(defmacro make-terms (e c &optional (terms ()))
  (if (null terms)
      `(list (cons ,e ,c))	 
      `(cons (cons ,e ,c) ,terms)))

(defmacro make-many-terms (&rest e-c-pairs)
  (if (null e-c-pairs) `(terms0)
      `(make-terms ,(first e-c-pairs) ,(second e-c-pairs)
        (make-many-terms ,@(rest (rest e-c-pairs))))))

(defmacro terms0? (x) `(null ,x))

(defmacro terms0 () '())

(defgeneric make-poly (domain variable &optional terms)
  (:documentation
   "This is not part of the interface. It is mostly used for
testing."))

(defmethod make-poly
    ((domain multivariate-polynomial-ring) variable &optional terms)  
  (unless terms
    (setq terms (make-terms 1 (coerce 1 (coefficient-domain-of domain)))))
  (make-polynomial domain (cons (variable-index domain variable) terms)))

;; Some useful control abstractions for term lists

;; This is the version we should use when the walker is re-installed properly.
#+ignore 
(defmacro map-over-each-term (terms (e c) &body body)
  (let ((collects? nil) (updates? nil))
    (walker::walk-form `(progn ,@body) nil
		       #'(lambda (form context env)
			   (declare (ignore env))
			   (cond ((and (eql context :eval) (not (atom form)))
				  (cond ((eql (first form) 'collect-term)
					 (setq collects? t))
					((eql (first form) 'update-term)
					 (setq updates? t)))))
                           form))
    `(let ((.ans-terms. (list nil))
	   ,@(when collects? 
                   `((.terms. nil))))
      (macrolet (,@(when collects?
                         '((collect-term (.e. .c.)
                            `(progn (setf (rest .terms.) (make-terms , .e. , .c.))
                              (setf .terms. (rest .terms.))))))
                 ,@(when updates?
                         '((update-term (.e. .c.)
                            `(progn (setf (le .t.) , .e.)
                              (setf (lc .t.) , .c.))))))
        ,@(when collects?
                '((setq .terms. .ans-terms.)))
        (loop for .t. on ,terms
              for ((,(and (not (eql e 'ignore)) e)
                     . ,(and (not (eql c 'ignore)) c))) = .t.
              do ,@body))
      (rest .ans-terms.))))

(defmacro map-over-each-term (terms (e c) &body body)
  `(let ((.ans-terms. (list nil))
	 (.terms. nil))
    (macrolet ((collect-term (.e. .c.)
                 `(progn (setf (rest .terms.) (make-terms , .e. , .c.))
                   (setf .terms. (rest .terms.))))
               (update-term (.e. .c.)
                 `(progn (setf (le .t.) , .e.)
                   (setf (lc .t.) , .c.))))
      (setq .terms. .ans-terms.)
      (loop for .t. on ,terms
            for ((,(and (not (eql e 'ignore)) e)
                   . ,(and (not (eql c 'ignore)) c))) = .t.
            do ,@body))
    (rest .ans-terms.)))

(defmacro pair-up-terms (terms1 (e1 c1) terms2 (e2 c2) order-predicate &body body)
  (unless (atom order-predicate)
    (error "Invalid order predicate for PAIR-UP-TERMS: ~S" order-predicate))
  `(let ((.ans-terms. (list nil))
	 (.terms. nil)
	 (.t1. ,terms1)
	 (.t2. ,terms2)
	 ,e1 ,c1 ,e2 ,c2)
    (macrolet ((collect-term (.e. .c.)
                 `(progn (setf (rest .terms.) (make-terms , .e. , .c.))
                   (setf .terms. (rest .terms.))))		
               (update-term1 (.e. .c.)
                 `(progn (setf (le .t1.) .e.)
                   (setf (lc .t1.) .c.)))
               (update-term2 (.e. .c.)
                 `(progn (setf (le .t2.) .e.)
                   (setf (lc .t2.) .c.))))
      (setq .terms. .ans-terms.)
      (loop
       (cond ((terms0? .t1.)
              (cond ((terms0? .t2.)
                     (return (rest .ans-terms.)))
                    (t (setq ,e2 (le .t2.) ,c2 (lc .t2.) .t2. (red .t2.))
                       (setq ,e1 nil))))
             ((or (terms0? .t2.) (,order-predicate (le .t1.) (le .t2.)))
              (setq ,e1 (le .t1.) ,c1 (lc .t1.) .t1. (red .t1.))
              (setq ,e2 nil))
             ((,order-predicate (le .t2.) (le .t1.))
              (setq ,e2 (le .t2.) ,c2 (lc .t2.) .t2. (red .t2.))
              (setq ,e1 nil))
             (t
              (setq ,e1 (le .t1.) ,c1 (lc .t1.) .t1. (red .t1.))
              (setq ,e2 (le .t2.) ,c2 (lc .t2.) .t2. (red .t2.))))
       ,@body))))

;; Simple version for book
#+ignore
(defmacro accummulate-terms (terms accumulator element-fun &optional (identity (terms0)))
  `(let ((answer ,identity))
    (map-over-each-term terms (e c)
      (setq answer (,accumulator answer (,element-fun e c))))))

(defvar *empty-accumulation-slot* (list nil))

(defmacro accumulate-terms (terms (accumulator &optional (identity (terms0)))
			    (e c)
			    &body element-forms)
  `(let ((.accum-list. (list *empty-accumulation-slot*))
	 (.accum-fun. ,accumulator))
    (map-over-each-term ,terms (,e ,c)
      (insert-into-accumulation-list .accum-list. (progn ,@element-forms) .accum-fun.))
    (accumulate-accumulation-list .accum-list. .accum-fun. ,identity)))

(defun insert-into-accumulation-list (l element accumulator)
  (cond ((eq (car l) *empty-accumulation-slot*)
	 (setf (car l) element))
	(t (setq element (%funcall accumulator (car l) element))
	   (setf (car l) *empty-accumulation-slot*)
	   (when (null (cdr l))
	     (setf (cdr l) (list *empty-accumulation-slot*)))
	   (insert-into-accumulation-list (cdr l) element accumulator)))
  l)

(defun accumulate-accumulation-list (accum-list accumulator identity)
  (cond ((null accum-list) identity)
	((eq (car accum-list) *empty-accumulation-slot*)
	 (accumulate-accumulation-list (cdr accum-list) accumulator identity))
	(t (do ((sum (car accum-list))
		(l (cdr accum-list) (cdr l)))
	       ((null l) sum)
	     (unless (eq (car l) *empty-accumulation-slot*)
	       (setq sum (%funcall accumulator sum (car l))))))))

;;; ===========================================================================
;;;				     EXPONENT ARITHMETIC
;;; ===========================================================================

(defmacro e= (x y) `(cl:= ,x ,y))

(defmacro e> (x y) `(cl:> ,x ,y))

(defmacro e< (x y) `(cl:< ,x ,y))

(defmacro e0 () 0)

(defmacro e0? (x)  `(cl:= (e0) ,x))

(defmacro e1 () 1)

(defmacro e1? (x)  `(cl:= (e1) ,x))

(defmacro e+ (x y) `(cl:+ ,x ,y))

(defmacro e1+ (x) `(cl:1+ ,x))

(defmacro e1- (x) `(cl:1- ,x))

(defmacro e- (x y) `(cl:- ,x ,y))

(defmacro e* (x y) `(cl:* ,x ,y))

(defmacro e/ (x y) `(cl:/ ,x ,y))

(defmacro eminus? (x) `(cl:minusp ,x))

(defmacro eoddp (x) `(cl:oddp ,x))

(defmacro eminus (x) `(cl:- ,x))

(defmacro emax (x y) `(cl:max ,x ,y))

(defgeneric make-polynomial-morphism (domain range &rest pairs)
  (:documentation
   "The purpose of this method is unknown."))

;; The pairs are variables in the domain and their values in the range
(defmethod make-polynomial-morphism
    ((domain polynomial-ring) (range polynomial-ring) &rest pairs)
  (let ((array (make-array (length (ring-variables domain))))
	(range-coefficient-domain (coefficient-domain-of range)))
    (loop for (v value) in pairs
	  do (setf (aref array (variable-index domain v))
		   (poly-form value)))
    (%funcall (if (eql domain range) #'make-automorphism #'make-homomorphism)
              domain
              #'(lambda (poly)
                  (bind-domain-context range ;; Because all computations are in the range
                    (labels ((transform (form)
                               (cond ((poly-coef? form)
                                      (coerce form range-coefficient-domain))
                                     (t (let* ((terms (poly-terms form))
                                               (old-e (le terms))
                                               (ans (transform (lc terms)))
                                               (value (aref array (poly-order-number form))))
                                          (map-over-each-term (red terms) (e c)
                                            (setq ans (poly-plus
                                                       (poly-times (poly-expt value (e- old-e e))
                                                                   ans)
                                                       (transform c)))
                                            (setq old-e e))
                                          (poly-times ans (poly-expt value old-e)))))))
                      (make-polynomial range (transform (poly-form poly))))))
              range)))

(defmethod make-polynomial-morphism
    ((domain free-module) (range free-module) &rest pairs)
  (unless (and (typep (coefficient-domain-of domain) 'polynomial-ring)
	       (typep (coefficient-domain-of range) 'polynomial-ring))
    (error "Don't know how to create a polynomial map from ~S to ~S"
	   domain range))
  (let ((morphism (%apply #'make-polynomial-morphism
                          (coefficient-domain-of domain)
                          (coefficient-domain-of range)
                          pairs)))
    (%funcall (if (eql domain range) #'make-automorphism #'make-homomorphism)
              domain
              #'(lambda (vector)
                  (%apply #'make-element
                          range
                          (loop with vect = (tuple-value vector)
                                for i below (array-dimension vect 0)
                                collect (apply-morphism morphism (aref vect i)))))
              range)))

(defun poly-monomial? (poly)
  (cond ((poly-coef? poly) t)
	((terms0? (red (poly-terms poly)))
	 (poly-monomial? (lc (poly-terms poly))))
	(t nil)))

(defgeneric print-mpolynomial-form (domain p stream)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod print-mpolynomial-form ((domain multivariate-polynomial-ring)
                                   p stream)
  (let* ((coef-domain (coefficient-domain-of domain))
	 (simple-coefs?
	  (and (typep coef-domain 'numeric-domain)
	       (not (typep coef-domain 'complex-numbers))))
	 (complex-num-coefs? (typep coef-domain 'complex-numbers)))
    (labels
      ((print-form (form)
         (if (poly-coef? form)
           (princ form stream)
           (let ((terms (poly-terms form))
                 (variable (variable-symbol
                            domain (poly-order-number form))))
             (let ((c (lc terms)))
               (when (and (poly-coef? c) 
                          (= c (- (one domain))))
                 (princ "- " stream)
                 (setq c (- c)))
               (print-term variable (le terms) c))
             (map-over-each-term (red terms) (e c)
               (if (and (poly-coef? c)
                        (number? c) (real? c) (minus? c))
                 (progn (princ " - " stream) (setq c (- c)))
                 (princ " + " stream))
               (print-term variable e c)))))
       (print-term (v e c)
         (unless (and (poly-1? c) (not (e0? e)))
           (cond ((or (and simple-coefs? (poly-monomial? c))
		      (and complex-num-coefs? (poly-coef? c)
			   (or (0? (realpart c)) (0? (imagpart c)))))
                  (print-form c))
                 (t (princ "(" stream)
                    (print-form c)
                    (princ ")" stream))))
         (unless (e0? e)
           (and (not (poly-1? c))
                (princ " " stream))
           (display v stream)
           (if (not (e1? e))
             #+Genera
             (format stream "~S" e)
             #-Genera
             (format stream "^~S" e)))))
      (print-form p))))

(defmethod print-object ((p mpolynomial) stream)
  (print-mpolynomial-form (domain-of p) (poly-form p) stream))

;;; ===========================================================================
;;;				    POLYNOMIAL ARITHMETIC
;;; ===========================================================================

;; Coercions 

(defmethod coerce (elt (domain multivariate-polynomial-ring))
  (let ((value (coercible? elt (coefficient-domain-of domain))))
    (cond ((not (null value))
	   (make-polynomial domain value))
	  (t (call-next-method)))))

(defmethod coerce ((exp symbol) (domain multivariate-polynomial-ring))
  (coerce (coerce exp *general*) domain))

(defmethod coerce ((exp list) (domain multivariate-polynomial-ring))
  (coerce (coerce exp *general*) domain))

(defmethod coerce ((p mpolynomial) (d general-expressions))
  (let ((domain (domain-of p)))
    (labels ((transform (form)
               (if (poly-coef? form)
                   (coerce form d)
                   (let ((terms (poly-terms form))
                         (variable (variable-symbol
                                    domain (poly-order-number form)))
                         (sum ()))
                     (map-over-each-term terms (e c)
                       (push (cond ((e0? e) (transform c))
                                   ((poly-1? c)
                                    (if (e1? e) variable
                                        (make-ge-expt d variable e)))
                                   ((e1? e)
                                    (make-ge-times d
                                                   (list (transform c) variable)))
                                   (t (make-ge-times d
                                                     (list (transform c)
                                                           (make-ge-expt d variable e)))))
                             sum))
                     (if (null (rest sum))
                         (first sum)
                         (make-ge-plus d (nreverse sum)))))))
      (transform (poly-form p)))))

(defmethod coerce ((exp general-expression)
		   (domain multivariate-polynomial-ring)) 
  (with-slots (variables) domain 
    (cond ((member exp variables :test #'ge-equal)
	   (make-polynomial domain
                            (cons (variable-index domain exp)
                                  (make-terms 1 (one (coefficient-domain-of domain))))))
	  ((and (ge-atom? exp)
		(let ((var (coercible? exp (coefficient-domain-of domain))))
		  (and var (make-polynomial domain var)))))
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
		(integer? (exponent-of exp))
		(plus? (exponent-of exp)))
	   (expt (coerce (base-of exp) domain) (exponent-of exp)))
	  (t (coerce exp (coefficient-domain-of domain))))))

(defun poly-simp (variable x)
  (cond ((terms0? x)
	 (zero *coefficient-domain*))
	((atom x)
	 (error "An atom, ~A, was passed to poly-simp, this is a bug" X))
	((and (e0? (le x))              ;just a coefficient
	      (terms0? (red x)))        ;and no additional terms
	 (lc x))
	(t (make-poly-form variable x))))

(defun terms-term (terms n)
  (do ((terms terms (red terms)))
      ((terms0? terms) (zero *coefficient-domain*))
    (cond ((e= (le terms) n) (return (lc terms)))
          ((e< (le terms) n)
           (return (zero *coefficient-domain*))))))

;;; The term lists are assumed to come from legitimate polynomials, so
;;; none of their coefficients can be zero.  Consequently, make-terms
;;; can be used in some places.

(defun terms-plus (x y)			;x and y are term lists
  (pair-up-terms x (e1 c1) y (e2 c2) e>
    (if e1 (if e2 (let ((c-sum (poly-plus c1 c2)))
		    (if (not (poly-0? c-sum))
			(collect-term e1 c-sum)))
	       (collect-term e1 c1))
	(collect-term e2 c2))))

(defun poly-plus (x y)
  (cond ((poly-coef? x)
	 (if (poly-coef? y) (+ x y)
	     (if (poly-0? x) y
		 (poly-simp y (terms-plus (make-terms (e0) x)
					  (poly-terms y))))))
	((poly-coef? y)
	 (if (poly-0? y) x
	     (poly-simp x (terms-plus (make-terms (e0) y) (poly-terms x)))))
	((same-variable? x y)
	 (poly-simp x (terms-plus (poly-terms y) (poly-terms x))))
	((more-main? x y)
	 (poly-simp x (terms-plus (make-terms (e0) y) (poly-terms x))))
	(t (poly-simp y (terms-plus (make-terms (e0) x) (poly-terms y))))))

(defmethod-sd plus ((x mpolynomial) (y mpolynomial))
  (bind-domain-context domain
    (make-polynomial domain (poly-plus (poly-form x) (poly-form y)))))

(defun poly-minus (p)
  (cond ((poly-coef? p)
	 (minus p))
	(t (make-poly-form p (terms-minus (poly-terms p))))))

(defun terms-minus (x)
  (map-over-each-term x (e c)
    (collect-term e (poly-minus c))))

(defmethod minus ((x mpolynomial))
  (let ((domain (domain-of x)))
    (bind-domain-context (domain-of x)
      (make-polynomial domain (poly-minus (poly-form x))))))

(defun poly-minus? (p)
  (if (poly-coef? p) (minus? p)
      (terms-minus? (poly-terms p))))

(defun terms-minus? (terms)
  (poly-minus? (lc terms)))

(defmethod minus? ((x mpolynomial))
  (bind-domain-context (domain-of x)
    (poly-minus? (poly-form x))))

(defun terms-difference (x y)			;x and y are term lists
  (pair-up-terms x (e1 c1) y (e2 c2) e>
    (if e1 (if e2 (let ((c-sum (poly-difference c1 c2)))
		    (if (not (poly-0? c-sum))
			(collect-term e1 c-sum)))
	       (collect-term e1 c1))
	(collect-term e2 (poly-minus c2)))))

(defun poly-difference (x y)
  (cond ((poly-coef? x)
	 (if (poly-coef? y) (- x y)
	     (if (poly-0? x) (poly-minus y)
		 (poly-simp y (terms-difference (make-terms (e0) x)
						(poly-terms y))))))
	((poly-coef? y)
	 (if (poly-0? y) x
	     (poly-simp x (terms-difference (poly-terms x)
					    (make-terms (e0) y)))))
	((same-variable? x y)
	 (poly-simp x (terms-difference (poly-terms x) (poly-terms y))))
	((more-main? x y)
	 (poly-simp x (terms-difference (poly-terms x) (make-terms (e0) y))))
	(t (poly-simp y (terms-difference (make-terms (e0) x)
					  (poly-terms y))))))

(defmethod-sd difference ((x mpolynomial) (y mpolynomial))
  (bind-domain-context domain
    (make-polynomial domain (poly-difference (poly-form x) (poly-form y)))))

(defun terms-mon-times (poly-terms e c)
  (if (poly-0? c) (terms0)
      (map-over-each-term poly-terms (te tc)
	(collect-term (e+ e te) (poly-times tc c)))))

#+AMBook
(defun terms-times (x y)
  (accumulate-terms y (#'terms-plus) (e c) (terms-mon-times x e c)))

(defun terms-times (x y)
  (let ( ;; Multiply x by the first term of y.  This is the initial
	;; term list we will modify.
	(answer (terms-mon-times x (le y) (lc y))) 
	e c)
    (setq answer (cons nil answer))
    (loop for (e-y . c-y) in (red y)
	  for ans = answer do
          (loop for (e-x . c-x) in x do
                (unless (poly-0? (setq c (poly-times c-x c-y)))
                  (setq e (e+ e-x e-y))
                  ;; Find place to insert this term.
                  (loop			
                   ;; Sure would be nice if the complier recognized and optimized
                   ;; the usages of (red ans)
                   (cond ((or (terms0? (red ans)) (e> e (le (red ans))))
                          (setf (red ans) (make-terms e c (red ans)))
                          (return t))	
                         ((e= e (le (red ans)))
                          (setf (lc (red ans)) (poly-plus (lc (red ans)) c))
                          (return t))
                         (t  (setq ans (red ans))))))))
    (loop for ans on answer
	  do (when (poly-0? (lc (red ans)))
	       (setf (red ans) (red (red ans)))))
    (red answer)))			     

(defun poly-times (x y)
  (cond ((poly-coef? x)
	 (if (poly-coef? y) (* x y)
	     (poly-simp y (terms-mon-times (poly-terms y) (e0) x))))
	((poly-coef? y)
	 (poly-simp x (terms-mon-times (poly-terms x) (e0) y)))
	((same-variable? x y)
	 (poly-simp x (terms-times (poly-terms x) (poly-terms y))))
	((more-main? x y)
	 (poly-simp x (terms-mon-times (poly-terms x) (e0) y)))
	(t (poly-simp y (terms-mon-times (poly-terms y) (e0) x)))))

(defmethod-sd times ((x mpolynomial) (y mpolynomial))
  (bind-domain-context domain
    (make-polynomial domain (poly-times (poly-form x) (poly-form y))))) 

;;; In both POLY-EXPTSQ and POLY-EXPT the second argument is a
;;; repetition count, and is thus is actually an integer.

(defun poly-exptsq (p n)
  (%funcall (repeated-squaring #'poly-times (one *coefficient-domain*))
            p n))

(defun poly-expt (p n)
  (cond ((e0? n) (one *coefficient-domain*))
	((e1? n) p)
	((poly-coef? p) (expt p n))
	((poly-monomial? p)	 
	 (let ((c (poly-expt (lc (poly-terms p)) n)))
	   (if (poly-0? c) c
	       (poly-simp p (make-terms (e* n (le (poly-terms p))) c)))))
	(t (let ((bl (do ((b (poly-simp p (red (poly-terms p))))
			  (bl (list (poly-simp p (red (poly-terms p))))
			      (cons (poly-times b (car bl)) bl))
			  (m 1 (1+ m)))
			 ((cl:= m n) bl)))
		 (monomial
                  (make-poly-form p (make-terms (le (poly-terms p))
                                                (lc (poly-terms p))))))
	     (do ((x^m monomial (poly-times x^m monomial))
		  (u (cdr bl) (cdr u))
		  (m 1 (1+ m))
		  (nom n (cl:/ (cl:* nom (cl:- n m)) (1+ m)))
		  (answer (car bl)
			  (poly-plus
			   (poly-times
			    (poly-times (coerce nom *coefficient-domain*) x^m)
			    (car u))
			   answer)))
		 ((null u)
		  (poly-plus x^m answer)))))))

(defmethod expt ((base mpolynomial) (expt integer))
  (let ((domain (domain-of base)))
    (bind-domain-context domain
      (make-polynomial domain (poly-expt (poly-form base) expt)))))

(defmethod expt ((base mpolynomial) (expt rational-integer))
  ;; Ignoring the domain of expt!!  FIXTHIS
  (let ((domain (domain-of base)))
    (bind-domain-context domain
      (make-polynomial domain (poly-expt (poly-form base)
					 (integer-value expt))))))

(defun terms-quotient (u v)
  (do ((coef)
       (exp)
       (quotient (terms0)))
      ((e< (le u) (le v))       
       (throw 'quotient-error
	 "~S is not exactly divisible by ~S"))
    (setq coef (poly-quotient* (lc u) (lc v)))
    (setq exp (e- (le u) (le v)))
    (setq u (terms-difference u (terms-mon-times v exp coef)))
    (setq quotient (terms-plus (make-terms exp coef) quotient))
    (if (terms0? u)
	(return quotient))))

(defun poly-quotient* (x y)
  (cond ((poly-0? y)
	 (throw 'quotient-error  "~S was divided by zero"))
	((poly-0? x) x)
	((poly-coef? x)
	 (if (poly-coef? y)
	     (cond ((typep *coefficient-domain* 'field)
		    (/ x y))
		   (t (multiple-value-bind (q r) (truncate x y)
			(if (0? r) q
			    (throw 'quotient-error
			      "Inexact division of ~S by ~S")))))
	     (throw 'quotient-error
	       "~S was divided by a polynomial of higher degree ~S")))
	((or (poly-coef? y)
	     (more-main? x y))
	 (make-poly-form x (terms-cquotient (poly-terms x) y)))
	((more-main? y x)
	 (throw 'quotient-error
	   "~S was divided by a polynomial of more main variable: ~S"))
	(t (poly-simp x (terms-quotient (poly-terms x) (poly-terms y))))))

(defun poly-quotient (x y)
  (let ((ans (catch 'quotient-error
	       (poly-quotient* x y))))
    (cond ((stringp ans)
	   (error ans x y))
	  (t ans))))

(defun terms-cquotient (terms c)
  (map-over-each-term terms (te tc)
    (collect-term te (poly-quotient tc c))))

(defmethod-sd quotient ((x mpolynomial) (y mpolynomial))
  (bind-domain-context domain
    (make-polynomial domain (poly-quotient (poly-form x) (poly-form y)))))

(defun poly-test-quotient (x y)
  (let ((ans (catch 'quotient-error
	       (poly-quotient* x y))))
    (if (stringp ans) nil
	ans)))

(defun terms-pseudo-remainder (u v)
  (if (e< (le u) (le v)) u
      (do ((k (e- (le u) (le v)) m)
	   (m))
	  (nil)
	(setq u (terms-difference (terms-mon-times u (e0) (lc v))
				  (terms-mon-times v k (lc u))))
	(cond ((terms0? u) (return u))
	      ((eminus? (setq m (e- (le u) (le v))))
	       (return (if (e0? k) u
			   (terms-mon-times u (e0) (poly-expt (lc v) k))))))
	(if (e> (e- k 1) m)
	    (setq u (terms-mon-times u
				     (e0)
				     (poly-expt (lc v) (e- (e- k 1) m))))))))

(defun poly-pseudo-remainder (p q)
  (cond ((poly-coef? p)
	 (cond ((poly-coef? q) (remainder p q))
	       (t p)))
	((poly-coef? q)
	 (with-slots (coefficient-domain) p
	   (zero coefficient-domain)))
	((same-variable? p q)
	 (poly-simp p (terms-pseudo-remainder (poly-terms p)
					      (poly-terms q)))) 
	((more-main? p q)
	 (poly-simp p (terms-coef-remainder (poly-terms p) q)))
	(t p)))

(defun terms-coef-remainder (u q)
  (map-over-each-term u (e c) 
    (collect-term e (poly-pseudo-remainder c q))))

(defmethod-sd remainder ((x mpolynomial) (y mpolynomial))
  (bind-domain-context domain
    (make-polynomial domain
      (poly-pseudo-remainder (poly-form x) (poly-form y)))))

(defun poly-truncate2 (u v)
  (cond ((poly-coef? v)
	 (cond ((typep *coefficient-domain* 'field)
		(values (poly-times u (/ v))
			(zero *coefficient-domain*)))
	       (t (error "Not implemented yet"))))
	(t (error "not implemented yet"))))

(defmethod-sd truncate2 ((x mpolynomial) (y mpolynomial))
  (bind-domain-context domain
    (multiple-value-bind (quo rem) (poly-truncate2 (poly-form x) (poly-form y))
      (values (make-polynomial domain quo)
	      (make-polynomial domain rem)))))

(defun poly-height (x)
  (if (poly-coef? x) (height x)
      (let (h)
        (map-over-each-term (poly-terms x) (e  c)
          (setq h (if h (max h (poly-height c))
                      (poly-height c))))
        h)))

(defmethod height ((x mpolynomial))
  (poly-height (poly-form x)))

;; The following routine gives a bound on the absolute value of largest 
;; coefficient that can occur in a factor involving the main variable (using
;; Gelfond's bound).
(defun poly-factor-cbound (p)
  (if (poly-coef? p) (height p)
      (* (expt 2 (le (poly-terms p))) (sqrt (le (poly-terms p)))
         (poly-height p))))

(defvar poly-gcd-algorithm 'poly-subresultant-gcd
  "Algorithm to be used to compute the GCD of two polynomials with the
   same main variable")

(defun terms-content (p)
  (do ((gcd (lc p) (poly-gcd gcd (lc pp)))
       (pp (red p) (red pp)))
      ((terms0? pp)
       (if (poly-minus? (lc p))
	   (poly-minus gcd)
	   gcd))))

(defun poly-content (p)
  (cond ((poly-coef? p) p)
	(t (terms-content (poly-terms p)))))

(defun poly-gcd (p q)
  (cond ((poly-1? p) p)
	((poly-1? q) q)
	((poly-coef? p)
	 (cond ((poly-coef? q) (gcd p q))
	       (t (poly-gcd (poly-content q) p))))
	((poly-coef? q)
	 (poly-gcd (poly-content p) q))
	((more-main? p q)
	 (poly-gcd (poly-content p) q))
	((more-main? q p)
	 (poly-gcd (poly-content q) p))
	(t (if (e< (le (poly-terms p)) (le (poly-terms q)))
	       (rotatef p q))
	   (let ((pc (poly-content p))
		 (qc (poly-content q)))
	     (poly-times (poly-gcd pc qc)
			 (%funcall poly-gcd-algorithm
				       (poly-quotient p pc)
				       (poly-quotient q qc)))))))

(defmethod-sd binary-gcd ((x mpolynomial) (y mpolynomial))
  (bind-domain-context domain
    (make-polynomial domain (poly-gcd (poly-form x) (poly-form y))))) 

(defun poly-lcm (p q)
  (poly-times (poly-quotient p (poly-gcd p q)) q))

(defun poly-split-on-vars 
    (poly vars &optional (pvars (reverse (poly-list-of-variables poly))))
  (let ((polys (list poly)))
    (unless (or (poly-coef? poly) (subsetp pvars vars)) 
      (loop for bad-v in (loop for v in pvars
                               when (not (member v vars))
                               collect v)
            for var = (list bad-v)
            for new-polys = nil
            do (loop for p in polys do 
                     (loop for deg below (1+ (poly-degree p var))
                           for coef = (poly-coefficient p var deg)
                           do (unless (0? coef)
                                (push coef new-polys))))
            (setq polys new-polys)))
    polys))

;; The following routine takes a list of polynomials and returns a list of
;; polynomials that all involve the same variables.  When computing the GCD
;; of the original polynomials, you only need to compute the GCD of the 
;; polynomials in returned list.
(defun poly-find-common-vars (polys)
  (let (pvars vars all-polys)
    (loop for same-vars = t 
          do 
          (setq pvars (loop for p in polys
                            collect (reverse (poly-list-of-variables p))))
          (setq vars (loop for vs in (rest pvars)
                           with ans = (first pvars)
                           do (unless (equal ans vs)
                                (setq same-vars nil)
                                (setq ans (intersection ans vs)))
                           finally (return ans)))
          (when same-vars 
            (return t))
          (setq all-polys nil)
          (loop for p in polys
                for vs in pvars
                do (setq all-polys (nconc all-polys 
                                          (poly-split-on-vars p vars vs))))
          (setq polys all-polys))
    polys))

(defun poly-mgcd (polys)
  (let ((d *coefficient-domain*)
        gcd odd-poly even-poly new-polys)
    (loop 
     (setq odd-poly (first polys))
     (setq even-poly (second polys))
     (loop for (op ep) on (poly-find-common-vars polys) by #'cddr
           do (setq odd-poly (poly-plus odd-poly (poly-times (random-constant d)
                                                             op)))
           (when ep
             (setq even-poly (poly-plus even-poly (poly-times (random-constant d)
                                                              ep)))))
     (print "Begin GCD")
     (setq gcd (spmod-gcd1 odd-poly even-poly))
     (print "Begin Test divides")
     (setq new-polys nil)
     (loop for p in polys
           do (unless (poly-test-quotient p gcd)
                (push p new-polys)))
     (print new-polys)
     (if (null new-polys)
         (return gcd)
         (push gcd new-polys)))))

;;; The following should ultimately be optimized to use SpGCD.
(defun poly-content-and-prim-part (p)
  (let ((content (poly-content p)))
    (values content (poly-quotient p content))))

(defun terms-prim-part (p)
  (if (terms0? p) p
      (terms-cquotient p (terms-content p))))

(defun poly-prim-part (p)
  (make-poly-form p (terms-prim-part (poly-terms p))))

(defun terms-monicize (terms)
  (let ((inv (recip (lc terms))))
    (map-over-each-term terms (e c)
      (collect-term e (* c inv)))))

(defun poly-monicize (p)
  (make-poly-form p (terms-monicize (poly-terms p))))

(defun terms-euclidean-gcd (u v)
  (do ((u u v)
       (v v (terms-pseudo-remainder u v)))
      ((terms0? v)
       (terms-prim-part u))
    ;; (print v)
    (if (e0? (le v)) (return (make-terms (e0) (one *coefficient-domain*))))))

(defun poly-euclidean-gcd (p q)
  (poly-simp p (terms-euclidean-gcd (poly-terms p) (poly-terms q))))

(defun terms-primitive-gcd (u v)
  (do ((u u v)
       (v v (terms-prim-part (terms-pseudo-remainder u v))))
      ((terms0? v)
       u)
    ;; (print v)
    (if (e0? (le v)) (return (make-terms (e0) (one *coefficient-domain*))))))

(defun poly-primitive-gcd (p q)
  (poly-simp p (terms-primitive-gcd (poly-terms p) (poly-terms q))))

(defun poly-reduced-gcd (p q)
  (poly-simp p (terms-reduced-gcd (poly-terms p) (poly-terms q))))

(defun terms-reduced-gcd (u v) 
  (let ((delta (e- (le u) (le v))))
    (do ((u u v)
	 (v v (terms-cquotient (terms-pseudo-remainder u v) beta))
	 (beta 1 (poly-expt (lc v) (e1+ delta))))
      ((terms0? v) (terms-prim-part u))
      ;;(print v)
      (if (e0? (le v)) (return (make-terms (e0) (one *coefficient-domain*))))
      (setq delta (e- (le u) (le v))))))

(defun poly-subresultant-gcd (p q)
  (let ((result (terms-subresultant-gcd (poly-terms p) (poly-terms q))))
    (if (e0? (le result))
	(one *coefficient-domain*)
	(poly-simp p result))))

(defgeneric resultant (polynomial polynomial~ variable)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod resultant ((x mpolynomial) (y mpolynomial) var)
  (let ((domain (domain-of x)) 
        pvar one into-varlist out-varlist)
    (unless (eql (domain-of y) domain)
      (error "Incompatible domains for resultant"))
    (setq one (one (coefficient-domain-of domain)))
    (setq pvar (poly-form (coerce var domain)))
    (setq into-varlist (list (list (poly-order-number pvar)
                                   (make-poly-form (list '-1) 
                                                   (make-terms (e1) one))))
          out-varlist (list (list -1 (make-poly-form pvar
                                                     (make-terms (e1) one)))))
    (bind-domain-context domain
      (make-polynomial domain 
                       (poly-subst 
                        (poly-resultant (poly-subst (poly-form x) into-varlist)
                                        (poly-subst (poly-form y) into-varlist))
                        out-varlist)))))

(defun poly-resultant (p q)
  (let ((result (terms-subresultant-gcd (poly-terms p) (poly-terms q))))
    (if (e0? (le result))
	(lc result)
	(zero *coefficient-domain*))))

(defun terms-subresultant-gcd (u v) 
  (let ((delta (e- (le u) (le v)))
	beta)
    (do ((u u v)
	 (v v (terms-cquotient (terms-pseudo-remainder u v) beta))
	 (first-time t nil)
	 (h ;; (poly-expt (lc v) (e- (le u) (le v)))
          ;; (if (e1? delta) (lc v)
          ;; 	(poly-quotient (poly-times h (poly-expt (lc v) delta))
          ;;		       (poly-expt h delta)))
          ))
	((terms0? v)
	 (terms-prim-part u))
      (if (e0? (le v))
	  (return v))
      (setq delta (e- (le u) (le v)))
      (setq beta (if first-time
		     (if (eoddp delta) 1 -1)
		     (if (eoddp delta) 
			 (poly-times (lc u) (poly-expt h delta))
			 (poly-times (poly-minus (lc u))
				     (poly-expt h delta)))))
      (if first-time
	  (setq h (poly-expt (lc v) (e- (le u) (le v))))
	  (setq h 
                (if (e1? delta) (lc v)
                    (poly-quotient (poly-times h (poly-expt (lc v) delta))
                                   (poly-expt h delta)))))
      ;;      (format t "~& = ~S,  = ~S, V = ~S, h = ~S" beta delta v h)
      )))

(defun poly-coerce (poly domain)
  (cond ((poly-coef? poly)
	 (coerce poly domain))
	(t (poly-simp poly (terms-coerce (poly-terms poly) domain)))))

(defun terms-coerce (terms domain)
  (map-over-each-term terms (e c)
    (collect-term e (poly-coerce c domain))))

(defun poly-subst (poly var-value)
  (let ((temp nil))
    (cond ((null var-value)
	   poly)
	  ((poly-coef? poly) (coerce poly *coefficient-domain*))
	  ((setq temp (second (assoc (poly-order-number poly) var-value
                                     :test #'eql)))	   
           (terms-horners-rule (poly-terms poly) temp var-value))
          (t (terms-horners-rule 
              (poly-terms poly) 
              (poly-simp poly (make-terms (e1) (one *coefficient-domain*)))
              var-value)
             ;; This assumed that more main variables were not substituted for
             #+ignore
             (poly-simp poly (terms-subst (poly-terms poly) var-value))
             ))))

(defun terms-subst (terms var-val-pairs)
  (let (temp)
    (map-over-each-term terms (e c)
      (unless (poly-0? (setq temp (poly-subst c var-val-pairs)))
	(collect-term e temp)))))

(defun terms-horners-rule (terms value &optional var-value)
  (let ((old-e (le terms))
	(ans (poly-subst (lc terms) var-value)))
    (map-over-each-term (red terms) (e c) 
      (setq ans (poly-plus (poly-times (poly-expt value (e- old-e e)) ans)
			   (poly-subst c var-value)))
      (setq old-e e))
    (poly-times ans (poly-expt value old-e))))

(defmethod substitute (value variable (p mpolynomial) &rest ignore)
  (declare (ignore ignore))
  (substitute (list (coerce value (domain-of p)))
	      (list (coerce variable (domain-of p)))
	      p))

(defmethod substitute 
    ((value mpolynomial) (variable mpolynomial) (p mpolynomial) &rest
     ignore)
  (declare (ignore ignore))
  (substitute (list value) (list variable) p))

(defmethod substitute ((values list) (variables list) (p mpolynomial)
		       &rest ignore)
  (declare (ignore ignore))
  (let ((domain (domain-of p))
	(new-domain (domain-of (first values))))
    (loop for var in variables
	  unless (eql (domain-of var) domain)
          do (error "Domain of ~S was expected to be ~S" var domain))
    (loop for val in values
	  unless (eql (domain-of val) new-domain)
          do (error "Domain of ~S was expected to be ~S" val new-domain))
    (loop for var in (ring-variables domain)
	  do (unless (find var variables 
			   :test #'(lambda (a b) 
				     (ge-equal a (variable-symbol domain b)))) 
	       (push (coerce var domain) variables)
	       (push (if (coercible? var new-domain)
			 (coerce var new-domain)
			 nil)
		     values)))
    (bind-domain-context new-domain
      (make-polynomial new-domain
                       (poly-subst (poly-form p)
                                   (loop for var in variables
                                         for val in values
                                         collect (list (variable-index domain var)
                                                       (and val (poly-form val)))))))))

(defun poly-variable-list (p &optional (varlist ()))
    (cond ((poly-coef? p) varlist)
	  (t (when (not (member (poly-order-number p) varlist))
	       (push (poly-order-number p) varlist))
	     (do ((terms (poly-terms p) (red terms)))
		 ((terms0? terms) varlist)
	       (setq varlist (poly-variable-list (lc terms) varlist))))))

(defgeneric partial-deriv (polynomial variable)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod partial-deriv ((p mpolynomial) x)
  (error "Don't know how to compute the partial deriv with respect to ~S"
	 x))

(defmethod partial-deriv ((p mpolynomial) (x symbol))
  (partial-deriv p (coerce x *general*)))

(defmethod partial-deriv ((p mpolynomial) (x general-expression))
  (let ((domain (domain-of p)))
    (with-slots (variables) domain
      (if (member x variables :test #'ge-equal)
	  (partial-deriv p (coerce x domain))
	  (call-next-method)))))

(defmethod partial-deriv ((p mpolynomial) (x mpolynomial))
  (let ((domain (domain-of p))
	terms)
    (cond ((and (eql domain (domain-of x))
		(null (red (setq terms (poly-terms (poly-form x)))))
		(e1? (le terms))
		(poly-1? (lc terms)))
	   (bind-domain-context domain
	     (make-polynomial domain
			      (poly-derivative (poly-form p) (poly-form x)))))
	  (t (error "~S is not a variable in ~S" x domain)))))

;;; var in the following is expected to be a polynomial of degree one
;;; with coefficient 1.

(defun poly-derivative (p var)
  (cond ((poly-coef? p) (zero *coefficient-domain*))
	((same-variable? var p)
	 (poly-simp p (terms-derivative (poly-terms p))))
	((more-main? var p)
	 (zero *coefficient-domain*))
	(t (poly-simp p (let (dc)
			  (map-over-each-term (poly-terms p) (e c)
			    (if (not (poly-0?
				      (setq dc (poly-derivative c var))))
				(collect-term e dc))))))))

(defun terms-derivative (x)
  (map-over-each-term x (e c)
    (if (not (e0? e))
	(collect-term (e1- e)
		      (poly-times (coerce e *coefficient-domain*) c)))))

(defmethod deriv ((poly mpolynomial) &rest vars)
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
                          (add-new-variable domain new))
                    (setq deriv
                          (+ deriv (* (partial-deriv poly kernel)
                                      (coerce diff domain))))))
            (setq poly deriv)))
    poly))		   

(defun poly-max-coefficient (p)
  (unless (poly-coef? p)
    (terms-max-coefficient (poly-terms p))))

(defun terms-max-coefficient (terms &optional (max 0))
  (map-over-each-term terms (ignore c)
    (setq max (if (poly-coef? c) (max max (abs c))
		  (terms-max-coefficient (poly-terms c) max))))
  max)

(defgeneric degree (polynomial variable &rest rest)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod degree ((p mpolynomial) (var symbol) &rest ignore)
  (declare (ignore ignore))
  (degree p (coerce var (domain-of p))))

(defmethod degree ((p mpolynomial) (var ge-variable) &rest ignore)
  (declare (ignore ignore))
  (degree p (coerce var (domain-of p))))

(defmethod degree ((p mpolynomial) (x mpolynomial) &rest ignore)
  (declare (ignore ignore))
  (let ((domain (domain-of p))
	terms)
    (cond ((and (eql domain (domain-of x))
		(null (red (setq terms (poly-terms (poly-form x)))))
		(e1? (le terms))
		(poly-1? (lc terms)))
	   (bind-domain-context domain
	     (poly-degree (poly-form p) (poly-form x))))
	  (t (error "~S is not a variable in ~S" x domain)))))

(defun poly-degree (p var)
  (cond ((poly-coef? p) (e0))
	((same-variable? var p)
	 (le (poly-terms p)))
	((more-main? var p) (e0))
	(t (do ((l (poly-terms p) (red l))
		(e (e0)))
	       ((terms0? l) e)
	     (setq e (emax e (poly-degree (lc l) var)))))))

;;; The following routine returns a list of pairs (var . degree).
;;; There should probably be a structure defined called variable
;;; information which is actually returned.
(defun poly-degree-vector (p)
  (unless (poly-coef? p) 
    (let ((pdv (list (cons (poly-order-number p) (le (poly-terms p))))))
      (terms-degree-vector (poly-terms p) pdv)
      (sort pdv #'(lambda (x y)
		    (more-main-order-number? (first x) (first y)))))))

(defun add-variable-degree (pdv var deg)
  (loop for var-info in pdv
	when (eql (car var-info) var)
        return (if (cl:> deg (cdr var-info))
                   (setf (cdr var-info) deg))
	when (more-main-order-number? var (car var-info))
        return nil
	finally (setq pdv (nconc pdv (list (cons var deg)))))
  pdv)

(defun terms-degree-vector (terms pdv)
  (map-over-each-term terms (ignore c)
    (unless (poly-coef? c)
      (setq pdv (add-variable-degree pdv
				     (poly-order-number c)
				     (le (poly-terms c))))
      (terms-degree-vector (poly-terms c) pdv)))
  pdv)

(defgeneric list-of-variables (polynomial &optional variables)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod list-of-variables ((x mpolynomial) &optional list-of-variables)
  (let ((domain (domain-of x)))
    (loop for order-number in (poly-list-of-variables (poly-form x))
          do (pushnew (get-variable-name order-number domain)
                      list-of-variables :test #'ge-equal))
    list-of-variables))

(defun poly-list-of-variables (p &optional list-of-vars)
  (labels ((terms-list-of-vars (terms)
	     (map-over-each-term terms (ignore c)
	       (p-list-of-vars c)))
	   (p-list-of-vars (p)
	     (cond ((poly-coef? p))
		   (t (pushnew (poly-order-number p) list-of-vars)
		      (terms-list-of-vars (poly-terms p))))))
    (p-list-of-vars p)
    list-of-vars))

(defgeneric coefficient (polynomial variables &optional exponent)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod coefficient ((p mpolynomial) (var symbol) &optional (exponent 1))
  (coefficient p (coerce var (domain-of p)) exponent))  

(defmethod coefficient ((p mpolynomial) (var ge-variable) &optional (exponent 1))
  (coefficient p (coerce var (domain-of p)) exponent))  

(defmethod coefficient
    ((p mpolynomial) (var mpolynomial) &optional (exponent 1))
  (let ((domain (domain-of p)))
    (bind-domain-context domain
      (make-polynomial domain
                       (cond ((eql domain (domain-of var))
                              (if (not (poly-coef? (poly-form var)))
                                  (poly-coefficient (poly-form p)
                                                    (poly-form var)
                                                    exponent)
                                  (poly-c-coefficient (poly-form p)
                                                      (poly-form var)
                                                      exponent)))
                             (t (poly-c-coefficient (poly-form p) var
                                                    exponent)))))))

(defun poly-coefficient (poly var exp)
  (cond ((poly-coef? poly)
	 (if (e0? exp) poly (zero *coefficient-domain*)))
	((same-variable? poly var)
	 (do ((l (poly-terms poly) (red l)))
	     ((terms0? l) (zero *coefficient-domain*))
	   (if (e= (le l) exp)
	       (return (lc l)))))
	((more-main? var poly)
	 (if (e0? exp) poly (zero *coefficient-domain*)))
	(t (do ((l (poly-terms poly))
		(coef (zero *coefficient-domain*)))
	       ((terms0? l) coef)
	     (setq coef (poly-plus
                         (poly-times
                          (poly-simp poly
                                     (make-terms (le l)
                                                 (one *coefficient-domain*)))
                          (poly-coefficient (lc l) var exp))
                         coef))
	     (setq l (red l))))))

(defun poly-c-coefficient (poly var exp)
  (cond ((poly-coef? poly) (coefficient poly var exp))
	(t (do ((l (poly-terms poly))
		(coef (zero *coefficient-domain*)))
	       ((terms0? l) coef)
	     (setq coef (poly-plus
                         (poly-times
                          (poly-simp poly
                                     (make-terms (le l)
                                                 (one *coefficient-domain*)))
                          (poly-c-coefficient (lc l) var exp))
                         coef))
	     (setq l (red l))))))

(defun poly-leading-coefficient (poly)
  (if (poly-coef? poly) poly
      (poly-leading-coefficient (lc (poly-terms poly)))))

(defmethod-sd binary= ((x mpolynomial) (y mpolynomial))
  (bind-domain-context domain
    (poly-0? (poly-difference (poly-form x) (poly-form y)))))

(defmethod-sd binary> ((x mpolynomial) (y mpolynomial))
  (let ((pf-x (poly-form x))
	(pf-y (poly-form y)))
    (cond ((and (poly-coef? pf-x) (poly-coef? pf-y))
	   (> pf-x pf-y))
	  (t (call-next-method)))))

(defgeneric get-variable-name (order-number domain)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod get-variable-name (order-number (domain variable-hash-table))
  (aref (variable-index-table domain) order-number 0))

(defmethod variable-index ((domain domain) (p mpolynomial))
  (poly-order-number (poly-form p)))

(defmethod parse-linear-equation ((p mpolynomial) &rest variables)
  (let ((domain (domain-of p)))
    (bind-domain-context domain
      (loop for var in variables
	    with poly = (poly-form p)
	    and coefs
	    unless (eql (domain-of var) domain)
            do (error "~S is not a variable of ~S" var domain)
	    do (setq var (poly-form var))
            (push (poly-coefficient poly var 1) coefs)
            (setq poly (poly-difference poly
                                        (poly-times (first coefs) var)))
	    finally (return (values
                             (values
                              (mapcar #'(lambda (c)
                                          (make-polynomial domain c))
                                      (reverse coefs)))
                             (make-polynomial domain poly)))))))
