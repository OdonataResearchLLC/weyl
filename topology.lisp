;;; -*- Base: 10; Mode: Lisp; Syntax: Common-lisp; Lowercase: T -*-

;;; ===========================================================================
;;;			 Topology and the Basics of Geometry
;;; ===========================================================================
;;; (c) Copyright 1994 Cornell University

;;; topology.lisp,v 1.37 1995/06/05 20:24:49 rick Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.37")

;; The following declaration causes all elements of euclidean spaces
;; to be points.
(define-domain-element-classes euclidean-space point)

;; This is needed to avoid a precedence problem. 
(defmethod make-element ((domain euclidean-space) (value vector) &rest values)
  (declare (ignore values))
  (make-element-free-module-vector domain value))

#+ignore
(defmethod print-object ((elt euclidean-space-element) stream)
  (print-free-module-element elt stream))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator euclidean-space (dimension &optional (domain *general*))
    (make-instance 'euclidean-space 
                   :coefficient-domain domain
                   :dimension dimension
                   :print-function 'euclidean-space-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'euclidean-space)
                        (eql (coefficient-domain-of d) domain)
                        (eql (dimension-of d) dimension)))))

(defun euclidean-space-print-object (domain stream)
  (format stream #+Genera "E~D" #-Genera "E^~D"
	  (dimension-of domain)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Points.
;;
(defmethod-sd binary= ((p1 abstract-point) (p2 abstract-point))
  (cl:= (id-number-of p1) (id-number-of p2)))

(defmethod-sd binary= ((p1 point) (p2 point))
  (let ((p1-tuple (tuple-value p1))
	(p2-tuple (tuple-value p2)))
    (loop for i fixnum below (array-dimension p1-tuple 0)
	  do (unless (= (svref p1-tuple i) (svref p2-tuple i))
	       (return nil))
	     finally (return t))))

(defgeneric make-point (domain value &rest values)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod make-point ((domain vector-space) (value vector) &rest values)
  (let ((coef-domain (coefficient-domain-of domain)))
    (unless (and (eql (array-dimension value 0) (dimension-of domain))
    	     (null values))
      (error "Wrong number of vector elements in ~S" domain))
    (make-instance 'point :domain domain
    	       :values (%apply #'vector
    			       (loop for i fixnum below (length value)
    				     collect (coerce (aref value i)
    						     coef-domain))))))

(defmethod make-point ((domain vector-space) value &rest values)
  (let ((coef-domain (coefficient-domain-of domain)))
    (unless (eql (1- (dimension-of domain)) (length values))
      (error "Wrong number of vector elements in ~S" domain))
    (make-instance 'point :domain domain
    	       :values (%apply #'vector
    			       (coerce value coef-domain)
    			       (loop for v in values
    				     collect (coerce v coef-domain))))))

(defmethod make-point ((domain vector-space) (value vector-space-element)
    		   &rest values)
  (apply #'make-point domain (tuple-value value) values))

(defmethod make-point ((domain abstract-space) value &rest values)
  (declare (ignore values))
  (if (null value)
      (make-instance 'abstract-point :domain domain)
      (make-instance 'named-point :domain domain :name value)))

(defmethod print-object ((point named-point) stream)
  (format stream "<~A>" (name-of point)))

(defmethod print-object ((point abstract-point) stream)
  (format stream "<~S>" (id-number-of point)))

;;  #P appears before coordinates of points.
(defmethod print-object ((point point) stream)
  (format stream "#P") (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Cells.

;;  Had to use defmethod instead of defmethod-sd.  The -sd version
;;  checks domains and cells don't have domains.
(defmethod binary= ((cell1 cell) (cell2 cell))
  (equal (cell-id cell1) (cell-id cell2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Simplices.

(defgeneric make-simplex (point &rest rest)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod make-simplex ((point abstract-point) &rest rest)
  (loop with domain = (domain-of point)
	for other in rest
	if (not (eql domain (domain-of other)))
	  do
       (error "Cannot make simplex with points from differing domains. ~s"
	      (cons point rest)))
  (make-instance 'simplex :vertices (cons point (copy-list rest))))

(defmethod print-object ((simplex simplex) stream)
  (format stream "[~S~{, ~S~}]"
	  (first (vertices-of simplex)) (rest (vertices-of simplex))))

(defgeneric cell-id (simplex)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod cell-id ((simplex simplex))
  (mapcar #'id-number-of (vertices-of simplex)))

(defmethod facets ((simplex simplex) (complex (eql nil)))
  (with-slots (vertices) simplex
    (if (rest vertices)
	(let ((f-list nil))
	  (choose (vertices-of simplex) (facet (dimension-of simplex))
		  (push (apply #'make-simplex facet) f-list))
	  f-list))))

(defmethod dimension-of ((s simplex))
  (- (length (vertices-of s)) 1))

;;  Return the list of vertices opposite the given face of the
;;  simplex.  The input face can be a simplex, a list of vertices, or
;;  a single point.
(defmethod opposite ((face simplex) (simplex simplex))
  (set-difference (vertices-of simplex) (vertices-of face)))

(defmethod opposite ((face list) (simplex simplex))
  (set-difference (vertices-of simplex) face))

(defmethod opposite ((face point) (simplex simplex))
  (remove face (vertices-of simplex)))

(defgeneric face? (points simplex)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod face? ((points list) (simplex simplex))
  (subsetp points (vertices-of simplex)))

(defmethod face? ((simplex1 simplex) (simplex2 simplex))
  (subsetp (vertices-of simplex1) (vertices-of simplex2)))

(defun segment? (thing)
  (and (typep thing 'simplex) (= (length (vertices-of thing)) 2)))

(defun triangle? (thing)
  (and (or (typep thing 'simplex) (typep thing 'polygon))
       (= (length (vertices-of thing)) 3)))

(defun tetrahedron? (thing)
  (and (typep thing 'simplex) (= (length (vertices-of thing)) 4)))

(defun sign-of-permutation (lista listb)
  (cond ((and (null lista) (null listb)) 1)
	((eql (first lista) (first listb))
	 (sign-of-permutation (rest lista) (rest listb)))
	((member (first lista) (rest listb))
	 (* -1 (sign-of-permutation
		(rest lista)
		(substitute (first listb) (first lista) (rest listb)))))
	(t 0)))

(defmacro map-over-oriented-facets ((facet orientation simplex complex)
				    &rest body)
  "Map over the faces of SIMPLEX, taking ORIENTATION into account"
  `(loop  
    for ,facet in (facets ,simplex ,complex)
    for ,orientation in (if (orient-of ,simplex)
			    '(t nil t nil)
			  '(nil t nil t))
    do
    ,@body))

(defmacro map-over-oriented-cofacets ((cofacet orientation simplex complex)
				      &rest body)
  "Map over the faces of SIMPLEX, taking ORIENTATION into account"
  ;;; this is pretty ugly.  Could be fixed either by keeping
  ;;; orientation info with cofacets, or by iterating over the cells
  ;;; of 1 higher dimension higher up in the calling seequence
  (let ((facet (gensym)))
    `(loop  
      for ,cofacet in (cofacets ,simplex ,complex)
      for ,orientation = (map-over-oriented-facets
			  (,facet ,orientation ,cofacet ,complex)
			  (if (eql ,facet ,simplex)
			      (return ,orientation)))
      do
      ,@body)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Polygons.

(defmethod cell-id ((polygon polygon))
  (let ((id-list (mapcar #'id-number-of (vertices-of polygon))))
    (if (= 3 (length (vertices-of polygon)))
	id-list
	(cons :p id-list))))	       ; Uses :p to distinguish from simplices.

(defmethod facets ((polygon polygon) (complex (eql nil)))
  (loop with vertices = (vertices-of polygon)
	for a in (cons (first (last vertices)) vertices) and b in vertices
	collect (make-simplex a b)))

(defmethod dimension-of ((polygon polygon))
  2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Cell-Complexes.

(defgeneric get-cell (cell complex)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod get-cell ((cell cell) (complex cell-complex))
  (if (member cell complex) cell
      (gethash (cell-id cell) (cell-table-of complex))))

;;  Allow get-cell to take a list of vertices (representing a simplex).
(defmethod get-cell ((vertex-list list) (complex cell-complex))
  (gethash (sort (mapcar #'id-number-of vertex-list) #'cl:<)
	   (cell-table-of complex)))

(defmethod member ((cell cell) (complex cell-complex) &rest ignore)
  (declare (ignore ignore))
  (second (multiple-value-list (gethash (id-number-of cell)
					(facet-table-of complex)))))

(defgeneric facets (cell complex)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod facets ((cell cell) (complex cell-complex))
  (gethash (id-number-of cell) (facet-table-of complex)))

(defmethod facets ((cells list) (complex cell-complex))
  (loop for cell in cells
	append (facets cell complex) into facet-list
	finally (return (remove-duplicates facet-list))))

(defgeneric cofacets (cell complex)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod cofacets ((cell cell) (complex cell-complex))
  (gethash (id-number-of cell) (cofacet-table-of complex)))

(defmethod cofacets ((cells list) (complex cell-complex))
  (loop for cell in cells
	append (cofacets cell complex) into cofacet-list
	finally (return (remove-duplicates cofacet-list))))

(defgeneric maximal-cell? (cell complex)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod maximal-cell? ((cell cell) (complex cell-complex))
  (and (member cell complex) (null (cofacets cell complex))))

;;  Destructive modification of cell-complex.
(defmethod insert ((cell cell) (complex cell-complex) &rest ignore)
  (declare (ignore ignore))
  (with-slots (cell-table facet-table cofacet-table) complex
    ;;  Internal insert.  Checking only needs to be done on insert;
    ;;  %insert does no checking.  Implements distinction between
    ;;  user-level insert and internal insert; nice for triangulations
    ;;  where user-level inserts only triangles, while internal
    ;;  inserts can do 1- and 0-simplices.  If there is already an
    ;;  equivalent cell in the complex then we return the equivalent
    ;;  cell (and do no insertion); otherwise we return the newly
    ;;  inserted cell.  [May want to make %insert a method on its own
    ;;  at some point.]
    (labels ((%insert (cell complex)
	       (or (get-cell cell complex)
		   (loop with facets-list
			 for facet in (facets cell nil)
			 do (setf facet (%insert facet complex))
			    (push facet facets-list)
			    (push cell (gethash (id-number-of facet)
						cofacet-table))
			 finally
		      (setf (gethash (cell-id cell) cell-table) cell)
		      (setf (gethash (id-number-of cell) facet-table)
			    facets-list)
		      (return cell)))))
      (%insert cell complex))))

;;  Destructive modification.  Can only delete a maximal cell.
(defmethod delete-maximal-cell ((cell cell) (complex cell-complex))
  (unless (member cell complex)
    (error "Cannot delete ~s from ~s.  It is not a member." cell complex))
  (when (cofacets cell complex)
    (error "Cannot delete a cell that is not maximal. ~s" cell))
  (with-slots (cell-table facet-table cofacet-table) complex
    (labels ((%delete (cell complex)
	       (loop with cofacets
		     for facet in (facets cell complex) do
		       (setf cofacets (remove cell (cofacets facet complex)))
		       (cond
			 (cofacets (setf (gethash (id-number-of facet)
						  cofacet-table)
					 cofacets))
			 (t (remhash (id-number-of facet) cofacet-table)
			    (%delete facet complex))))
	       (remhash (cell-id cell) cell-table)
	       (remhash (id-number-of cell) facet-table)))
      (%delete cell complex))))

(defgeneric %map-over-cells (function complex dimension)
  (:documentation
   "The purpose of this function is unknown."))

;;  Use function on all cells of complex that have the given
;;  dimension.  If dimension is null then use function on all cells.
(defmethod %map-over-cells ((function function) (complex cell-complex)
			    dimension)
  (maphash #'(lambda (cell-id cell)
	       (declare (ignore cell-id))
	       (when (or (null dimension) (= dimension (dimension-of cell)))
		 (funcall function cell)))
	   (cell-table-of complex)))

;;  Syntactic sugar.
(defmacro map-over-cells ((cell &optional (dimension nil)) structure
			  &body body)
  `(%map-over-cells #'(lambda (,cell) ,@body) ,structure ,dimension))

;;  More syntactic sugar.
(eval-when (:compile-toplevel :load-toplevel)
  (defmacro map-over-maximal-cells ((cell) complex &body body)
    `(map-over-cells (,cell) ,complex
      (when (maximal-cell? ,cell ,complex)
        ,@body))))

(defmethod union ((complex1 cell-complex) (complex2 cell-complex) &rest rest)
  (when rest
    (error "Too many arguments to Union."))
  (unless (eql (class-of complex1) (class-of complex2))
    (error "~s and ~s are not of the same class." complex1 complex2))
  (let ((new (make-instance (class-of complex1))))
    (map-over-maximal-cells (cell) complex1 (insert cell new))
    (map-over-maximal-cells (cell) complex2 (insert cell new))
    new))

(defmethod intersection ((complex1 cell-complex) (complex2 cell-complex)
			 &rest rest)
  (when rest
    (error "Too many arguments to Intersection."))
  (unless (eql (class-of complex1) (class-of complex2))
    (error "~s and ~s are not of the same class." complex1 complex2))
  (let ((new (make-instance (class-of complex1))))
    (map-over-cells (cell) complex1
      (when (get-cell cell complex2) (insert cell new)))
    new))
		    
(defmethod vertex-set ((cell-complex cell-complex))
  (let ((vert-list nil))
    (map-over-cells (v-cell 0) cell-complex
		    (push (first (vertices-of v-cell)) vert-list))
    vert-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Simplicial-Complexes.

;;  Catch bad insertions.  This avoids a nasty precedence problem:
;;  without this, the command (insert cell simplicial-complex) can end
;;  up at command (insert cell cell-complex) and work even when cell
;;  is a nonsimplex.
(defmethod insert :before (thing (complex simplicial-complex) &rest ignore)
  (declare (ignore ignore))
  (unless (typep thing 'simplex)
    (error "Illegal attempt to Insert a nonsimplex ~s into ~s" thing complex)))

(defun make-simplicial-complex (cells)
  (loop with complex = (make-instance 'simplicial-complex)
	for cell in cells do
	(insert cell complex) 
	finally (return complex)))

(defgeneric max-dimension-of (cell)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod max-dimension-of ((cm cell-complex))
  (let ((max 0))
    (map-over-cells(cell) cm
		   (setf max (max max (dimension-of cell))))
    max))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Coerce.
;;
;;  Coerce via the coercion-cache.  A single object can have multiple
;;  representations; this allows fast access without recomputing the
;;  map.  Maps are specified via Make-Homomorphism.  It's also
;;  possible to force a coercion to have a particular value via (setf
;;  coerce).
;;
;;  This has three advantages: (1) You can force a coercion to have a
;;  particular value via (setf coerce).  (2) You can save time (when
;;  the map between spaces is slow).  (3) You can save consing
;;  (nothing new is created after the first time that the cache is
;;  used; while a map is likely to create new objects during every
;;  use).
;;
;;  FIXTHIS: This method should probably be in a different file.
;;
(defmethod coerce ((thing has-coercion-cache) (domain domain))

  ;;  Return the thing itself if it is already in the correct domain.
  (or (if (eql (domain-of thing) domain) thing)

      ;;  Use the stored value if it exists.
      (rest (assoc domain (%coercion-cache-of thing)))

      ;;  Get the value, cache it, and return it.
      (let ((value (call-next-method)))
        (if value (push (cons domain value) (%coercion-cache-of thing)))
        value)))

(defmethod %set-coerce ((thing has-coercion-cache) (domain domain) value)
  (with-slots (coercion-cache) thing
    (when (assoc domain coercion-cache)
      (error "Multiple representations for a single item. ~s" thing))
    (unless (eql (domain-of value) domain)
      (error "Mismatch when defining coercion. ~s ~s" domain value))
    (push (cons domain value) coercion-cache)
    value))

(defsetf coerce %set-coerce)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chains
;;
;; CHAINs are elements of CHAIN-MODULE domains
;;
;;   Use GET-CHAIN-MODULE to create CHAIN-MODULE domains.
;;   CHAIN representations are based upon the canonical cells returned by
;;   GET-Canonical-Cell from the CELL-COMPLEX in the CHAIN-MODULE.

(defun chain-module-print-object (cm stream)
  (format stream "C_~D(~A;~A)" (dimension-of cm) (complex-of cm)
	  (coefficient-domain-of cm)))

(defun cochain-module-print-object (cm stream)
  (format stream "C^~D(~A;~A)" (dimension-of cm) (complex-of cm)
	  (coefficient-domain-of cm)))

(defmethod complex-of ((chain chain))
  (complex-of(domain-of chain)))

(defgeneric get-chain-module (cell integer &optional ring)
  (:documentation
   "The purpose of this function is unkown."))

(defmethod get-chain-module ((c cell-complex) (n integer)
			     &optional (ring (get-rational-integers)))
  (make-instance 'chain-module
		 :complex c
                 :dimension n
                 :coefficient-domain ring
		 :print-function 'chain-module-print-object))

(defgeneric get-cochain-module (cell integer &optional ring)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod get-cochain-module ((c cell-complex) (n integer)
			       &optional (ring (get-rational-integers)))
  (make-instance 'cochain-module
		 :complex c
                 :dimension n
                 :coefficient-domain ring
		 :print-function 'cochain-module-print-object
		 ))

(defgeneric boundary-domain (chain)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod boundary-domain ((c chain-module))
  (if (= (dimension-of c) 0)
     (error "Can not create a chain module with a less than zero dimension.")
     (get-chain-module (complex-of c)
                       (- (dimension-of c) 1)
                       (coefficient-domain-of c))))

(defmethod dimension-of ((chain chain))
  (dimension-of (domain-of chain)))

(defmethod boundary-domain ((c chain))
  (if (= (dimension-of (domain-of c)) 0)
      (error "Can not create boundary domain for a zero dimension chain.")
    (get-chain-module (complex-of c)
		      (- (dimension-of c) 1)
		      (coefficient-domain-of (domain-of c)))))

(defmethod boundary-domain ((s simplex))
  (get-chain-module (make-simplicial-complex (list s))
		    (- (dimension-of s) 1)
		    (get-rational-integers)))

(defmethod boundary-domain ((cc cell-complex))
  (get-chain-module cc (1- (max-dimension-of cc))
		    (get-rational-integers)))

(defgeneric coboundary-domain (cochain)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod coboundary-domain ((c cochain))
  (if (= (dimension-of (domain-of c)) (max-dimension-of (complex-of c)))
      (error "Can not create coboundary domain for a maximal dimensional cochain.")
    (get-cochain-module (complex-of c)
			(+ (dimension-of c) 1)
			(coefficient-domain-of (domain-of c)))))

(defmethod coboundary-domain ((s simplex))
  (get-cochain-module (make-simplicial-complex (list s))
		      (+ (dimension-of s) 1)
		      (get-rational-integers)))


(defmethod print-object ((c chain) stream)
  (flet ((print-term (s coef)
		     (cond ((minus? coef)
		  (princ " - " stream)
		  (setq coef (- coef)))
		 (t (princ " + " stream)))
	   (unless (1? coef)
	     (print-grouped coef stream))
	   (princ s stream)))
    (let* ((terms (chain-terms-of c))
	   (s (first (first terms)))
	   (coef (rest (first terms))))
      (cond ((null terms) (princ 0 stream))
	    (t (cond ((minus? coef)
		      (princ " - " stream)
		      (setq coef (- coef))))
	       (unless (1? coef)
		 (print-grouped coef stream))
               (princ s stream)
	       (loop for (cell . coef) in (rest terms)
		     do (print-term cell coef)))))))

(defun canonical (pair domain)
  (multiple-value-bind (cached sign)
      (get-cell (complex-of domain) (first pair))
    (cons cached (if (> sign 0) (cdr pair) (minus (cdr pair))))))

(defmethod coerce ((simplex simplex) (cm chain-module))
  (make-chain cm (list(cons simplex
 			    (coerce 1 (coefficient-domain-of
 				       cm))))))

(defmethod coerce ((cc simplicial-complex) (cm chain-module))
  (let ((one (coerce 1 (coefficient-domain-of cm)))
 	(dim (dimension-of cm))
 	(list nil))
    (map-over-cells (face dim) cm
		    (push (cons face one) list))
    (make-chain cm list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CELL-ORDER
;;   defines a canonical ordering for cells
;;   - used to create canonical stored and printed representations for chains
(defun points-order (a b)
  (loop for av in a
	for bv in b
	do (cond ((cl:< (id-number-of av) (id-number-of bv))
		  (return t))
		 ((eql av bv) nil)
		 (t (return nil)))))

(defgeneric cell-order (cell1 cell2)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod cell-order ((a cell) (b cell))
  (let ((aorder (sort (copy-list (vertices-of a)) #'cl:< :key #'id-number-of))
	(border (sort (copy-list (vertices-of b)) #'cl:< :key #'id-number-of)))
    (or (points-order aorder border)
	(points-order (vertices-of a) (vertices-of b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAKE-CHAIN
;;   Used to create CHAIN elements.  
;;   -  Currently does not verify that the simplexes given are of the proper
;;      dimension indicated in the CHAIN-MODULE


;; Internally, the chains are stored as dotted pairs of simplices and
;; coefficients.  The argument list to make-chain is also a list of
;; dotted pairs.

(defun make-chain-or-cochain (d simplices chain-or-cochain)
  (let ((simps nil))
    (loop for simp on 
	  (sort simplices
		#'(lambda (x y)
		    (cell-order (car y) (car x))))
	  do
	  (cond
	   ((eq (first simp) (second simp))
	    (setf (cdr (second simp))
		  (+ (cdr (second simp))
		     (cdr (first simp)))))
	   ((0? (cdr (first simp)))nil)
	   (t (push (first simp) simps))))
    (make-instance chain-or-cochain
		   :domain d
		   :terms simps)))

(defun make-chain (d simplices)
  (make-chain-or-cochain d simplices 'chain))

(defun make-cochain (d simplices)
  (make-chain-or-cochain d simplices 'cochain))

(defmethod zero ((d chain-module))
  (make-chain d ()))

(defmethod apply ((c chain) &rest args)
  (setq args (accum-apply-args args))
  (cond ((typep (first args) 'simplex)
	 (loop with s = (first args)
	       for (simplex . coef) in (chain-terms-of c)
	       do (when (face? s simplex)
		    (return coef))))))
  

(defun free-group-plus (xt yt)
  (pair-up-terms xt (simp1 c1) yt (simp2 c2) cell-order
		 (if simp1 (if simp2 (let ((c-sum (+ c1 c2)))
				       (if (not (0? c-sum))
					   (collect-term simp1 c-sum)))
			     (collect-term simp1 c1))
		   (collect-term simp2 c2))))

(defun free-group-difference (xt yt)
  (pair-up-terms xt (simp1 c1) yt (simp2 c2) cell-order
		 (if simp1 (if simp2 (let ((c-sum (- c1 c2)))
				       (if (not (0? c-sum))
					   (collect-term simp1 c-sum)))
			     (collect-term simp1 c1))
		   (collect-term simp2 c2))))

(defun free-group-minus (xt)
   (free-group-difference nil xt))

(defun free-group-scalar-times (c terms)
  (cond ((0? c) nil)
	(t (loop for (simp . coef) in terms
		 for c1 = (* c coef)	; coefficient ring need not be an 
		 unless (0? c1)		; integral domain!
		   collect (cons simp c1)))))

(defmethod-sd plus ((x chain) (y chain))
  (make-chain (domain-of x)
	      (free-group-plus (chain-terms-of x) (chain-terms-of y))))

(defmethod-sd difference ((x chain) (y chain))
  (make-chain (domain-of x)
	      (free-group-difference (chain-terms-of x) (chain-terms-of y))))

(defmethod times ((x number) (y chain))
  (make-chain  (domain-of y)
	       (free-group-scalar-times x (chain-terms-of y))))

(defmethod times ((x domain-element) (y chain))
  (make-chain  (domain-of y)
	       (free-group-scalar-times x (chain-terms-of y))))

(defmethod times ((x chain) (y number))
  (make-chain (domain-of x)
	      (free-group-scalar-times y (chain-terms-of x))))

(defmethod times ((x chain) (y number))
  (make-chain (domain-of x)
	      (free-group-scalar-times y (chain-terms-of x))))

(defmethod minus ((x chain))
  (make-chain (domain-of x)
	      (free-group-minus (chain-terms-of x))))

(defun chain-terms-times (xt yt)
  (pair-up-terms xt (simp1 c1) yt (simp2 c2) cell-order
    (when (and simp1 simp2)
      (let ((c-prod (* c1 c2)))
	   (if (not (0? c-prod))
	       (collect-term simp1 c-prod))))))

(defmethod-sd times ((x chain) (y chain))
  (make-chain domain
	      (chain-terms-times (chain-terms-of x) (chain-terms-of y))))

(defgeneric boundary (simplex &optional domain)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod boundary ((s simplex) &optional (domain (boundary-domain s)))
  (let* ((list nil)
         (one (one (coefficient-domain-of domain)))
	 (simplicial-complex (complex-of domain))
         )
    (map-over-oriented-facets
     (f o s simplicial-complex)
     (push (cons f (if o  one (minus one))) list))
    (make-chain domain list)))

(defmethod boundary ((cc cell-complex) &optional (domain (boundary-domain cc)))
  (let ((bound (zero domain)))
    (map-over-cells
     (cell (1+(dimension-of domain))) cc
     (setf bound (+ bound (boundary cell domain))))
    bound))

(defmethod boundary ((c chain) &optional d)
  (let((pairs nil)
       (bd  (if d d (boundary-domain c)))
       (zero nil)
       (ht (make-hash-table))
       (complex (complex-of c)))
    
    (setf zero (zero(coefficient-domain-of bd)))
    (loop for term in (chain-terms-of c) do
	  (map-over-oriented-facets
	   (facet orientation (car term) complex)
	   (setf (gethash facet ht)
		 (if orientation
		     (+ (gethash facet ht zero) (cdr term))
		   (- (gethash facet ht zero) (cdr term))))))
    (maphash #'(lambda(x y) (push (cons x y) pairs)) ht)
    (make-chain bd pairs)))


;; Takes a boundary (which is chain and returns the list of simplices
;; that make up the boundary.
(defmethod boundary-set ((c chain))
  (loop for (simp . coef) in (chain-terms-of c)
	with simps = nil
	do (unless (even? coef)
	     (push simp simps))
	   finally (return simps)))

(defmethod deriv ((ch chain) &rest vars)
  (make-chain (domain-of ch)
	      (loop for (simp . coef) in (chain-terms-of ch)
		    for c1 = (apply #'deriv (cons coef vars))
		    unless (0? c1)
		      collect (cons simp c1))))



;;;COBOUNDARY
(defmethod coboundary ((c cochain) &optional d)
  (let((pairs nil)
       (bd  (if d d (coboundary-domain c)))
       (zero nil)
       (ht (make-hash-table))
       (complex (complex-of c)))
    
    (setf zero (zero(coefficient-domain-of bd)))
	  ;;; use chain-terms-of to take advantage of structural
	  ;;; isomorphism between finite chains and cochains
    (loop for (simp . coef)  in (chain-terms-of c) do
	  (map-over-oriented-cofacets
	   (cofacet orientation simp complex)
	   (setf (gethash cofacet ht)
		 (if orientation
		     (+ (gethash cofacet ht zero) coef)
		   (- (gethash cofacet ht zero) coef)))))
    (maphash #'(lambda(x y) (push (cons x y) pairs)) ht)
    (make-cochain bd pairs)))


;;;Eventually the duality relationship between chains and cochains
;;;should be taken into account.  For now -- these coersions

(defmethod coerce ((chain chain)  (cocm cochain-module))
  (make-cochain cocm (copy-tree (chain-terms-of chain))))

(defmethod coerce ((cochain cochain)  (cm chain-module))
  (make-chain cm (copy-tree (chain-terms-of cochain))))
