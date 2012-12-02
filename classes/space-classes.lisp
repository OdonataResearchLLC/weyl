;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			  Space Classes
;;; ===========================================================================
;;; (c) Copyright 1994 Cornell University

;;; space-classes.lisp,v 1.11 1995/05/30 18:10:14 rick Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.11")

;; Topological Domains

;; Abstract spaces don't necessarily have a well defined dimension
(defclass abstract-space (domain) ())

(defclass dimensional-space (abstract-space dimensional-domain) ())

(defclass euclidean-space (vector-space dimensional-space) ())

;;  FIXTHIS: This class should probably be in a different file.
;;  Saves any results of coercions in a coercion cache accessible via
;;  Coerce.
(defclass has-coercion-cache ()
  ((coercion-cache
    :initform nil
    :accessor %coercion-cache-of)))

;;  Associates a unique id-number with each instance.
(defclass has-id-number ()
  ((global-counter
    :initform 0
    :allocation :class
    :accessor %global-id-counter-of)
   (id-number
    :reader id-number-of)))

(defclass has-name ()
  ((name :initarg :name :accessor name-of)))

(defmethod initialize-instance :after ((obj has-id-number) &rest ignore)
  (declare (ignore ignore))
  (with-slots (id-number) obj
    (setf id-number (incf (%global-id-counter-of obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Points.

;;  Base class of different types of points. 
(defclass abstract-point (has-id-number domain-element) ())

;; An abstract point with a name.  Points with the same name in the
;; same space are identical.
(defclass named-point (has-name abstract-point) ())

;;  A point may have different coordinates in different spaces.  The
;;  appropriate coordinates of a point are found using Coerce.
;;  General-Point is for point in possibly non-euclidean coordinate
;;  systems (polar, spherical, etc.)
(defclass general-point (tuple has-coercion-cache abstract-point) ())

(defclass point (vector-space-element general-point) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Cells and Cell-Complex.

;;  To fit within a complex, each cell class must have the following
;;  functions: cell-id, facets, and dimension-of.  Function cell-id
;;  returns something (it doesn't really matter what) so that two
;;  cell-ids are #'equal iff the two cells are equivalent.  [It also
;;  has to hash efficiently, so for Lucid, we have to use id-numbers
;;  instead of points because all points hash to the same location.]
;;  Function facets returns all the subcells that are one dimension
;;  lower than the cell.  Function dimension-of does what you'd
;;  expect.
(defclass cell (has-id-number)
  ((orient :initform t :initarg :orient :accessor orient-of)))

(defclass simplex (cell)
  (;;  Maintained in order of id-number.
   (vertices :initform nil :initarg :vertices :reader vertices-of)))


;;; there must be a better place for ORIENTED-SORT
(defun oriented-sort (list)
  "Sort keeping track of the number of swaps"
  ;; bubble sort
  (loop with orient = t
	for l1 on list do
	(loop for l2 on (rest l1) do
	      (when (cl:< (id-number-of (first l2)) (id-number-of (first l1)))
		(setf orient (null orient))
		(psetf (first l1) (first l2)
		       (first l2) (first l1))))
	finally (return (values list orient))))

(defmethod initialize-instance :after ((simplex simplex)
				       &rest ignore &key home)
  (declare (ignore ignore home))
  (with-slots (vertices orient) simplex
    (multiple-value-bind (v o)
        (oriented-sort (copy-list vertices))
      (setf vertices v
            orient o))))

(defclass polygon (cell)
  ( ;;  Maintained with smallest id-number first, then adjacent
   ;;  vertex with smaller id-number, followed by other vertices in
   ;;  order around the polygon.
   (vertices :initform nil :initarg :vertices :reader vertices-of)))

(defmethod initialize-instance :after ((polygon polygon) &rest ignore)
  (declare (ignore ignore))
  (warn "Polygons are not completely implemented."))

(defclass cell-complex ()
  ( ;;  Used to recognize cells that are equivalent.
   (cell-table :initform (make-hash-table :test #'equal)
               :reader cell-table-of)
   (facet-table :initform (make-hash-table) :reader facet-table-of)
   (cofacet-table :initform (make-hash-table) :reader cofacet-table-of)))

(defclass simplicial-complex (cell-complex) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Chains.

(defclass chain-module (module)
  ((complex :initarg :complex :reader complex-of)
   (dim :initform 0 :initarg :dimension :reader dimension-of)))

(defclass cochain-module (module)
  ((complex :initarg :complex :reader complex-of)
   (dim :initform 0 :initarg :dimension :reader dimension-of)))

;; Chains are elements of chain-modules
(defclass chain (domain-element)
  ((terms :initarg :terms :accessor chain-terms-of)))

;; Mathematically, cochains would probably not inherit from chains.
;; This is done to simplify implemention -- since chains and cochains
;; are structurally identical, we'll inherit all functionality from
;; chains.  Fix this later???
(defclass cochain (chain)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Spaces

(defclass function-space (vector-space dimensional-space ring)
  ((funct-domain :initarg :domain :reader funct-domain-of)
   (funct-range :initarg :range :reader funct-range-of)))

(defclass function-space-element (domain-element)
  ())

(defmethod initialize-instance :after ((h function-space) &rest plist)
  (declare (ignore plist))
  (with-slots (print-function) h
    (setf print-function 'function-space-print-function)))

(defun function-space-print-object (h  stream)
  (format stream "C(~S->~S)" (funct-domain-of h) (funct-range-of h)))

;; The domain and range for a Banach space should both be geometric domains
(defclass Banach-space (function-space)
  ())

;;Both Banach and Hilbert spaces are supposed to be complete under the
;;norm.  I wonder how that is defined.

(define-operations Banach-space
  (norm (element self)) -> REAL-NUMBERS)

(defclass Hilbert-space (Banach-space)
  ())

(define-operations Hilbert-space
  (inner-product (element self) (element self)) -> REAL-NUMBERS)

(defclass hilbert-space-element (function-space-element)
  ())

(defmethod initialize-instance :after ((h hilbert-space) &rest plist)
  (declare (ignore plist))
  (with-slots (print-function) h
    (setf print-function 'hilbert-space-print-function)))

(defun hilbert-space-print-object (h stream)
  (format stream "Hilb(~S, ~S)" (funct-domain-of h) (funct-range-of h)))
