;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			      Expanded Polynomials
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; grobner.lisp,v 1.8 1995/05/24 17:42:02 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.8")

(defmethod initialize-instance :after ((id ideal) &rest ignore)
  (declare (ignore ignore))
  (let ((ring (ring-of id)))
    (with-slots (coefficient-domain print-function) id
      (unless coefficient-domain
	(setf coefficient-domain (ring-of id)))

      (setf print-function 'ideal-print-object))
    (unless (super-domains-of id)
      (setf (super-domains-of id) (cons ring (super-domains-of ring))))))

(defun ideal-print-object (id stream)
  (let ((gens (generators-of id)))
    (format stream "#Id(~S~{, ~S~})" (first gens) (rest gens))))

(defgeneric make-ideal (ring &rest generators)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod make-ideal ((ring ring) &rest generators)
  (make-instance 'ideal
		 :ring ring
		 :generators (loop for g in generators
				   collect (coerce g ring))))

(defmethod make-ideal ((ring field) &rest generators)
  (declare (ignore generators))
  (make-instance 'ideal :ring ring :generators (list (one ring))))

;;FIXTHIS:  The following assumes that PID are GCD domains, which isn't true. 
(defmethod make-ideal ((ring rational-integers) &rest generators)
  (make-instance 'PID-ideal :ring ring
		 :generators (list
			       (loop with g = (coerce (first generators) ring)
				     for e in (rest generators)
				     do (setq g (gcd g (coerce e ring)))
					finally (return g)))))

(defmethod reduce-basis ((id PID-ideal))
  id)

(defmethod plus ((id1 ideal) (id2 ideal))
  (cond ((and (eql (ring-of id1) (ring-of id2))
	      (eql (coefficient-domain-of id1) (coefficient-domain-of id2)))
	 (apply #'make-ideal (ring-of id1)
		(append (generators-of id1) (generators-of id2))))
	(t (call-next-method))))

(defmethod times ((id1 ideal) (id2 ideal))
  (cond ((and (eql (ring-of id1) (ring-of id2))
	      (eql (coefficient-domain-of id1) (coefficient-domain-of id2)))
	 (apply #'make-ideal (ring-of id1)
		(loop for e1 in (generators-of id1)
		      append (loop for e2 in (generators-of id2)
				   collect (* e1 e2)))))
	(t (call-next-method))))

(defmethod binary= ((id1 ideal) (id2 ideal))
  (or (eql id1 id2)
      (loop with id2-gen = (generators-of id2)
	    for p in (generators-of id1)
	    do (unless (member p id2-gen :test #'binary=)
		 (return nil))
	    finally (return t))))
		     

(defmacro with-grobner-operations (grobner-basis &body body)
  `(with-slots (greater-function ring generators undones reducibles possibles)
	       ,grobner-basis
     (let ((dim (cl:1+ (length (ring-variables ring)))))
       (macrolet ((e> (a b) `(%funcall greater-function ,a ,b))
		  (e< (a b) `(%funcall greater-function ,b ,a)))
	 ,@body))))

;; Grobner calculations are done within the context of an instance of
;; the Grobner-Basis flavor.  Each instance has its own variable list
;; and flags sets.  At any time the user can add polynomials or
;; extract information from the structure.

#|  ;; The following is actually in algebraic-domains.lisp |
(defclass grobner-basis (ideal has-comparison)
  (;; The exponent comparison function is managed by HAS-COMPARISON

   (undones :initform ())
   ;; A list of triples pairs (lt f g), lt(f)<=lt(g), of elements of
   ;; GENERATORS such that if any pair is not in the list, its s-poly
   ;; is guaranteed to be writable as a linear combination of
   ;; GENERATORS, with smaller s-degs
   
   (reducibles :initform nil :accessor reducibles-of)
   (possibles :initform nil)	
   ))
||#

(defmethod initialize-instance :after ((gb grobner-basis) &rest ignore)
  (declare (ignore ignore))
  (with-slots (greater-function ring) gb
    (setq greater-function 
	  (get-comparison-fun (length (ring-variables ring))
			      greater-function))))

(defun check-same-domain (exprs)
  (let ((domain (domain-of (first exprs))))
    (loop for exp in (rest exprs)
	  do (unless (eql domain (domain-of exp))
	       (return nil))
	  finally (return domain))))


(defmethod make-ideal ((ring polynomial-ring) &rest polys)
  (let (ideal)
    (cond ((field? (coefficient-domain-of ring))
	   (setq ideal (make-instance 'grobner-basis :ring ring
				      :greater-function :lexical)))
	  (t (error "Can't deal with polynomials not over fields: ~S"
		    ring))) 
    (loop for p in polys
	  do (add-relation ideal (coerce p ring)))
    ideal))

(defmethod (setf greater-function) (new-function (grob grobner-basis))
  (with-slots (ring greater-function generators reducibles possibles) grob
    (unless (eql greater-function new-function) 
      (flet ((convert-list (list) 
	       (loop for poly in list 
		     collect (sort poly new-function))))
	(unless (functionp new-function)
	  (setq new-function
		(get-comparison-fun (length (ring-variables ring))
				    new-function)))
	(setq generators (convert-list generators))
	(setq reducibles (convert-list reducibles))
	(setq possibles (convert-list possibles))
	(setq greater-function new-function)))
    grob))

(defgeneric add-relation (basis poly)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod add-relation ((grob-struct grobner-basis) (relation mpolynomial))
  (let ((ring (ring-of grob-struct)))
    (if (not (eql ring (domain-of relation)))
	(add-relation grob-struct (coerce relation ring)))
    (let ((poly (make-epolynomial ring (greater-function-of grob-struct)
				  relation)))
      (push (poly-form poly) (reducibles-of grob-struct))
      poly)))

(defmethod add-relation ((grob-struct grobner-basis) (relation epolynomial))
  (let ((ring (ring-of grob-struct)))
    (if (not (eql ring (domain-of relation)))
	(add-relation grob-struct (coerce relation ring)))
    (let ((poly (make-epolynomial ring (greater-function-of grob-struct)
				  relation)))
      (push (poly-form poly) (reducibles-of grob-struct))
      poly)))

(defmethod generators-of ((grob-struct grobner-basis))
  (with-slots (generators reducibles greater-function ring) grob-struct
    (append
      (loop for g in generators
	    collect (make-instance 'epolynomial
				   :domain ring
				   :greater-function greater-function
				   :form g))
      (loop for g in reducibles
	    collect (make-instance 'epolynomial
				   :domain ring
				   :greater-function greater-function
				   :form g)))))

(defmethod reset-grobner-basis ((grob-struct grobner-basis))
  (with-slots (generators undones possibles reducibles) grob-struct
    (setq generators nil undones nil
	  possibles nil reducibles nil)))

#+Ignore
(defun terms-s-poly (greater-function terms1 terms2)
  (let ((m (max (le terms1) (le terms2))))
    (gterms-difference greater-function
     (gterms-mon-times terms1 (- m (le terms1)) (lc terms2))
     (gterms-mon-times terms2 (- m (le terms2)) (lc terms1)))))

;; The following saves a bunch of consing, but not as much as I would expect
(defun terms-s-poly (greater-function terms1 terms2)
  #+Lucid
  (declare (optimize (safety 0)))
  (let* ((dim (length (first terms1))) 
	 (m (gterm-lcm (lt terms1) (lt terms2) dim))
	 (ans-terms (list nil))
	 (terms ans-terms)
	 (x (red terms1))
	 (y (red terms2))
	 (xe (gterm-quot m (lt terms1) dim))
	 (xc (svref (lt terms2) 0))
	 (ye (gterm-quot m (lt terms2) dim))
	 (yc (- (svref (lt terms1) 0)))
	 temp sum new-xe new-ye)
    (loop
      (cond ((terms0? x)
	     (cond ((terms0? y) (return (rest ans-terms)))
		   (t (setq temp (gterm-times ye (lt y) dim))
		      (setf (svref temp 0) (* yc (svref (lt y) 0)))
		      (setf (rest terms) (list temp))
		      (setf terms (rest terms))
		      (setq y (red y)))))
	    ((or (terms0? y)
		 (%funcall greater-function
			   (setq new-xe (gterm-times xe (lt x) dim))
			   (setq new-ye (gterm-times ye (lt y) dim))))
	     (setq temp (gterm-times xe (lt x) dim))
	     (setf (svref temp 0) (* xc (svref (lt x) 0)))
	     (setf (rest terms) (list temp))
	     (setf terms (rest terms))
	     (setq x (red x)))
	    ((%funcall greater-function new-ye new-xe)
	     (setf (svref new-ye 0) (* yc (svref (lt y) 0)))
	     (setf (rest terms) (list new-ye))
	     (setf terms (rest terms))
	     (setq y (red y)))
	    (t (setq sum (+ (* xc (svref (lt x) 0))
			    (* yc (svref (lt y) 0))))
	       (unless (0? sum)
		 (setf (svref new-xe 0) sum)
		 (setf (rest terms) (list new-xe))
		 (setf terms (rest terms)))
	       (setq x (red x) y (red y)))))))

(defmethod reduce-basis ((grob-struct grobner-basis))
  (with-grobner-operations grob-struct
    (flet ((criterion1 (degree f1 f2)
	     (loop for p in generators do
	       (when (and (not (eql p f1))
			  (not (eql p f2))
			  (gterm-dominates degree (lt p) dim))
		 (unless (member nil undones
				 :test
				 #'(lambda (x prod)
				     (declare (ignore x))
				     (let ((b1 (second prod))
					   (b2 (third prod)))
					  (or (and (eql f1 b1) (eql p b2))
					      (and (eql f1 b2) (eql p b1))
					      (and (eql p b1) (eql f2 b2))
					      (and (eql p b2) (eql f2 b1))))))
		   (return-from criterion1 t))))))
      (let (temp f1 f2 h)
	(reduce-all grob-struct)
	(new-basis grob-struct)
      	(loop while undones do
          (setq temp (pop undones))
	  (setq f1 (second temp) f2 (third temp))
	  (when (and (null (criterion1 (first temp) f1 f2))
		     (not (gterm-disjoint (lt f1) (lt f2) dim)))
	    (setq h (terms-reduce greater-function
				  (gterms-prim*
				   (terms-s-poly greater-function f1 f2))
				  generators))
	    (when (not (terms0? h))
	      (setq reducibles nil)
	      (setq possibles (list h))
	      (setq generators
		    (loop for g in generators
			  when (gterm-dominates (lt g) (lt h) dim)
			    do (push g reducibles)
			  else collect g))
	      (setq undones
		    (loop for undone in undones
			  unless (or (member (second undone) reducibles)
				     (member (third undone) reducibles))
			    collect undone)) 
	      (reduce-all grob-struct)
	      (new-basis grob-struct)))))))
  grob-struct)

(defgeneric reduce-all (basis)
  (:documentation
   "The purpose of this function is unknown."))

;; This makes sure that all of the polynomials in generators and
;; possibles are AUTOREDUCED.
(defmethod reduce-all ((grob-struct grobner-basis))
  (with-grobner-operations grob-struct
    (let (h g0)
      (loop while (not (null reducibles)) do
        (setq h (terms-reduce greater-function
			      (pop reducibles)
			      (append generators possibles)))
	(unless (terms0? h)
	  (setq generators (loop for elt in generators 
				 when (gterm-dominates (lt elt) (lt h) dim)
				   do (push elt reducibles)
				      (push elt g0)
				 else collect elt))
	  (setq possibles (loop for elt in possibles
				when (gterm-dominates (lt elt) (lt h) dim)
				  do (push elt reducibles)
				else collect elt))
	  (setq undones (loop for (nil f1 f2) in undones
			      when (and (not (member f1 g0))
					(not (member f2 g0)))
				collect (list (gterm-lcm (lt f1) (lt f2) dim)
					      f1 f2)))
	  (push h possibles))))))

(defgeneric new-basis (basis)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod new-basis ((grob-struct grobner-basis))
  (with-grobner-operations grob-struct
    (flet ((add-undone (f g)
	     (when (e> (lt f) (lt g))
	       (rotatef f g))
	     (loop for (nil ff gg) in undones
		   do (when (and (eql ff f) (eq gg g))
			(return t))
		   finally (push (list (gterm-lcm (lt f) (lt g) dim) f g)
				 undones))))
      (setq generators (append generators possibles))
      (loop for g in generators do
	(loop for elt in possibles do
	  (when (not (eql elt g))
	    (add-undone elt g))))
      (setq possibles nil)
      (setq undones (sort undones #'(lambda (a b) (e< (first a) (first b)))))
      #+ignore
      (setq generators
	    (loop for g in generators
		  for h = (terms-reduce greater-function g (remove g generators))
		  when (not (terms0? h))
		    collect h)))))

;; Reduce terms modulo the current basis
(defun terms-reduce (greater-function terms basis)
  (let ((dim (length (first terms))))
    #+ignore
    (format t "~&~%Poly = ~S~%Basis: "
	    (le terms))
    #+ignore
    (princ (mapcar #'(lambda (f) (le f)) basis))
    (let ((again t))
      (loop while again do
	(when (terms0? terms)
	  (return nil))
	#+ignore
	(format t "~&Terms = ~S"
		(make-instance 'epolynomial
			       :domain (slot-value grob-struct 'ring)
			       :greater-function greater-function
			       :form terms))
	(loop for g in basis
	      do (when (gterm-dominates (lt terms) (lt g) dim)
		   (setq terms (gterms-prim*
				 (terms-s-poly greater-function terms g)))
		   (return t))
	      finally (setq again nil))))
    #+ignore
    (format t "~&Result = ~S~%" (le terms))
    terms))

;; Make poly primitive.  
;; This isn't really well defined since coefs are in a field.  Idea is
;; to make the coefficients smaller.  Its really worth avoiding
;; dividing out a content of 1!!!
#+ignore  ;; Use for integral domains
(defun gterms-prim* (poly) 
  (unless (terms0? poly)
    (let ((coef-domain (domain-of (lc poly)))
	  (num-gcd (numerator (lc poly)))
	  (den-gcd (denominator (lc poly)))
	  1/content)
      ;; Should really use a probabilistic algorithm content algorithm
      ;; here
      (map-over-each-term (red poly) (nil c)
	(if (1? num-gcd)
	    (if (1? den-gcd) (return t)
		(setq den-gcd (gcd den-gcd (denominator c))))
	    (if (1? den-gcd)
		(setq num-gcd (gcd num-gcd (numerator c)))
		(setq num-gcd (gcd num-gcd (numerator c))
		      den-gcd (gcd den-gcd (denominator c))))))
      (unless (and (1? num-gcd) (1? den-gcd))
	(setq 1/content (make-quotient-element coef-domain den-gcd num-gcd))
	(map-over-each-term poly (e c)
	  (update-term e (* c 1/content))))))
  poly)

;; Use for fields
(defun gterms-prim* (poly)
  (unless (terms0? poly)
    (let ((1/lc (/ (svref (lt poly) 0))))
      (unless (1? 1/lc)
	(loop for term in poly
	      do (setf (svref term 0) (* (svref term 0) 1/lc))))))
  poly)
