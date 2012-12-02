;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			      Fourier Transforms
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; fourier.lisp,v 1.3 1994/11/15 19:55:25 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.3")

(defgeneric make-ge-fourier (domain argument svar fvar)
  (:documentation
   "The purpose of this method is not known."))

(defmethod make-ge-fourier ((domain general-expressions) argument svar fvar)
  (make-instance 'ge-fourier :domain domain :argument argument
		 :space-var svar :freq-var fvar))

(defmethod print-object ((expr ge-fourier) stream)
  (format stream "Four{~S, ~S->~S}"
	  (argument-of expr) (space-var-of expr) (freq-var-of expr)))

(defmethod ge-equal ((x ge-fourier) (y ge-fourier))
  (and (ge-equal (argument-of x) (argument-of y))
       (ge-equal (space-var-of x) (space-var-of y))
       (ge-equal (freq-var-of x) (freq-var-of y))))

(defmethod ge-great ((x ge-fourier) (y ge-fourier))
  (cond ((ge-great (argument-of x) (argument-of y)) t)
	((ge-great (argument-of y) (argument-of x)) nil)
	((ge-great (space-var-of x) (space-var-of y)) t)	
	((ge-great (space-var-of y) (space-var-of x)) nil)
	((ge-great (freq-var-of x) (freq-var-of y)) t)))

(defgeneric ge-fourier (exp svar fvar)
  (:documentation
   "The purpose of this method is unknown.")
  (:method (exp svar fvar)
    (declare (ignore fvar))
    (error "Don't know how to take the Fourier transform of ~S wrt ~S"
           exp svar)))

(defmethod ge-fourier ((exp general-expression) (svar symbol) (fvar symbol))  
  (ge-fourier exp (coerce svar (domain-of exp)) (coerce fvar (domain-of exp))))

(defmethod ge-fourier (exp (svar ge-variable) (fvar ge-variable))
  (make-ge-fourier (domain-of svar) (coerce exp (domain-of svar)) svar fvar))

(defmethod ge-fourier ((exp numeric) (svar ge-variable) (fvar ge-variable))
  exp)

(defmethod ge-fourier ((exp ge-variable) (svar ge-variable) (fvar ge-variable))
  (let ((domain (domain-of exp)))
    (unless (and (eql domain (domain-of svar))
		 (eql domain (domain-of fvar)))
      (error "Taking Fourier transform from different domains"))
    (cond ((ge-equal exp svar) fvar)
	  ((depends-on? exp svar)
	   (make-ge-fourier domain exp svar fvar))
	  (t exp))))

(defmethod ge-fourier ((exp ge-plus) (svar ge-variable) (fvar ge-variable))
  (let ((domain (domain-of exp)))
    (cond ((and (eql domain (domain-of svar))
		(eql domain (domain-of fvar)))
	   (call-next-method))
	  (t (simplify
	      (make-ge-plus domain
                            (loop for x in (terms-of exp)
                                  collect (ge-fourier x svar fvar))))))))

(defmethod ge-fourier ((exp ge-times) (svar ge-variable) (fvar ge-variable))
  (let ((domain (domain-of exp))
	terms depend-term free-terms)
    (unless (and (eql domain (domain-of svar))
		 (eql domain (domain-of fvar)))
      (error "Taking Fourier transform from different domains"))
    (setq terms (terms-of exp))
    (loop for term in terms
          do (when (depends-on? term svar)
               (cond ((null depend-term)
                      (setq depend-term term))
                     (t (return (setq free-terms :non-linear)))))
          finally (setq free-terms
                        (remove depend-term terms)))
    (cond ((eql free-terms :non-linear)
           (make-ge-fourier domain exp svar fvar))
          ((null depend-term)
           exp)
          (t (simplify
              (make-ge-times domain
                             (cons (ge-fourier depend-term svar fvar)
                                   free-terms)))))))

#+ignore
(defmethod ge-fourier ((exp ge-deriv) (svar ge-variable) (fvar ge-variable))
  (let ((domain (domain-of exp)))
    (unless (and (eql domain (domain-of svar))
		 (eql domain (domain-of fvar)))
      (error "Taking Fourier transform from different domains"))
    (loop for entry in (varlist-of exp)
          with varlist
          do (when (ge-equal svar (first entry))
               (setq varlist (remove entry (varlist-of exp)))
               (return
                 (simplify
                  (* (expt fvar (second entry))
                     (if (null varlist)
                         (ge-fourier (argument-of exp) svar fvar)
                         (make-ge-deriv domain
                                        (ge-fourier (argument-of exp) svar fvar)
                                        varlist))))))
          finally
          (return 
            (simplify
             (make-ge-deriv domain
                            (ge-fourier exp svar fvar)
                            (varlist-of exp)))))))

(defgeneric fourier (expression &rest variables)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod fourier ((exp number) &rest vars)
  (declare (ignore vars))
  (make-element *general* exp))

(defmethod fourier ((exp numeric) &rest vars)
  (declare (ignore vars))
  exp)

(defmethod fourier ((exp symbol) &rest vars)
  (setq exp (coerce exp *general*))
  (loop for (sv fv) on vars by #'cddr
	do (setq exp (ge-fourier exp (coerce sv *general*)
                                 (coerce fv *general*))))
  exp)

(defmethod fourier ((exp general-expression) &rest vars)
  (setq exp (coerce exp *general*))
  (loop for (sv fv) on vars by #'cddr
	do (setq exp (ge-fourier exp (coerce sv *general*)
                                 (coerce fv *general*))))
  exp)

;; Inverse Fourier Transforms

(defgeneric make-ge-ifourier (domain argument svar fvar)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod make-ge-ifourier ((domain general-expressions) argument svar fvar)
  (make-instance 'ge-ifourier :domain domain :argument argument
		 :space-var svar :freq-var fvar))

(defmethod print-object ((expr ge-ifourier) stream)
  (format stream "IFour{~S, ~S->~S}"
	  (argument-of expr) (space-var-of expr) (freq-var-of expr)))
