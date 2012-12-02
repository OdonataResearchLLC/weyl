;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;				    Matrices
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; matrix.lisp,v 1.14 1995/05/24 17:49:23 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.14")

;; This is a very general matrix implementation.   At some point it will
;; be worth while implementing some more specialized matrix spaces.

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator matrix-space ((ring ring))
    (make-instance 'matrix-space
                   :coefficient-domain ring
                   :print-function 'make-space-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'matrix-space)
                        (eql (coefficient-domain-of d) ring)))))

(defun matrix-space-print-object (domain stream)
  (format stream "Mat(~S)" (coefficient-domain-of domain)))

(defmethod make-element ((domain matrix-space) (value array) &rest ignore)
  (declare (ignore ignore))
  (make-instance 'matrix-space-element
		 :domain domain
		 :dimension1 (array-dimension value 0)
		 :dimension2 (array-dimension value 1)
		 :value value))

(defmethod weyl::make-element ((domain matrix-space) (value array)
			       &rest ignore)
  (declare (ignore ignore))
  (let* ((array-dims (array-dimensions value))
	 (x-dim (first array-dims))
	 (y-dim (second array-dims))
	 (coef-domain (coefficient-domain-of domain))
	 (array (make-array (list x-dim y-dim))))
    (loop for i fixnum below x-dim do
	  (loop for j fixnum below y-dim do
		(setf (aref array i j) (coerce (aref value i j) coef-domain))))
    (make-instance 'matrix-space-element
		   :domain domain
		   :dimension1 x-dim 
		   :dimension2 y-dim 
		   :value array)))

(defmethod make-element ((domain matrix-space) (value list) &rest values)
  (setq values (if (null values) value
		   (cons value values)))
  (unless (loop for row in (rest values)
		with n = (length (first values))
		do (unless (eql (length row) n)
		     (return nil))
		finally (return t))
    (error "All rows not the same length: ~S" values))
  (make-element domain
		(make-array (list (length values) (length (first values)))
			    :initial-contents values)))

(defmethod weyl::make-element ((domain matrix-space) (value list) &rest values)
  (setq values (if (null values) value
		   (cons value values)))
  (unless (loop for row in (rest values)
		with n = (length (first values))
		do (unless (eql (length row) n)
		     (return nil))
		finally (return t))
    (error "All rows not the same length: ~S" values))
  (let* ((x-dim (length values))
	 (y-dim (length (first values)))
	 (array (make-array (list x-dim y-dim))))
    (loop for i fixnum  below x-dim
	  for row in values do
	    (loop for j fixnum below y-dim
		  for val in row do
		    (setf (aref array i j) val)))
    (weyl::make-element domain array)))

(defgeneric matrix-dimensions (matrix)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod matrix-dimensions ((m matrix-space-element))
  (with-slots (dimension1 dimension2) m
    (values dimension1 dimension2)))

(defmethod dimensions ((m matrix-space-element))
  (with-slots (dimension1 dimension2) m
    (list dimension1 dimension2)))

#+Genera
(defmacro with-matrix-dimensions ((dim1 dim2 &optional array) matrix &body body
				  &environment env)
  (scl:once-only (matrix &environment env)
    `(multiple-value-bind (,dim1 ,dim2) (matrix-dimensions ,matrix)
       ,(if array `(let ((,array (matrix-value ,matrix)))
		     ,@body)
	    `(progn ,@body)))))

#-Genera
(eval-when (:compile-toplevel :load-toplevel)
  (defmacro with-matrix-dimensions ((dim1 dim2 &optional array) matrix &body body)
    (let ((decls (list `(fixnum ,dim1 ,dim2))))
      (loop while (and (not (atom (first body)))
                       (eql (first (first body)) 'declare))
            do (setq decls (append (rest (pop body)) decls)))
      `(multiple-value-bind (,dim1 ,dim2) (matrix-dimensions ,matrix)
        (declare ,@decls)
        ,(if array `(let ((,array (matrix-value ,matrix)))
                     ,@body)
             `(progn ,@body))))))

#-Genera
(defmethod print-object ((matrix matrix-space-element) stream)
  (with-matrix-dimensions (dim1 dim2 array) matrix
    (princ "Mat<" stream)
    (loop for i fixnum below dim1
	  do (princ "<" stream)
	     (loop for j fixnum below dim2
		   do (print-object (aref array i j) stream)
		      (if (< (1+ j) dim2)
			  (princ ",  " stream)
			  (princ ">" stream)))	     
	     (if (< (1+ i) dim1)
		 (princ ",  " stream)
		 (princ ">" stream)))))

#+Genera
(defmethod print-object ((matrix matrix-space-element) stream)
  (with-matrix-dimensions (dim1 dim2 array) matrix
    (dw:formatting-table (stream)
      (loop for i below dim1 do
	(dw:formatting-row (stream)
	  (loop for j below dim2 do
	    (dw:formatting-cell (stream :align-x :center)
	      (princ (aref array i j) stream))))))))

(defmethod ref ((matrix matrix-element) &rest args)
  (let ((x (first args))
	(y (second args)))
    (cond ((numberp x)
	   (cond ((numberp y)
		  (aref (matrix-value matrix) x y))
		 ((eql y :*)
		  (with-matrix-dimensions (rows cols array) matrix
		    (declare (ignore rows))
		    (let ((new-array (make-array (list 1 cols))))
		      (loop for j fixnum below cols
			    do (setf (aref new-array 0 j) (aref array x j)))
		      (make-element (domain-of matrix) new-array))))
		 (t (error "Unknown argument to REF(~S ~S)"
			   x y))))
	  ((eql x :*)
	   (cond ((numberp y)		  
		  (with-matrix-dimensions (rows cols array) matrix
		    (declare (ignore cols))
		    (let ((new-array (make-array (list rows 1))))
		      (loop for i fixnum below rows
			    do (setf (aref new-array i 0) (aref array i y)))
		      (make-element (domain-of matrix) new-array))))
		 (t (error "Unknown argument to REF(~S ~S)"
			   x y))))
	  (t (error "Unknown argument to REF(~S ~S)"
		    x y)))))

(defmethod set-ref ((matrix matrix-element) new-value &rest args)
  (setf (aref (matrix-value matrix) (first args) (second args)) new-value))

(defmethod zero-matrix ((domain matrix-space) &optional rank)
  (unless (numberp rank)
    (error "Must specify rank to ZERO-MATRIX (~D)" domain))
  (make-element domain
	       (make-array (list rank rank)
			   :initial-element (zero (coefficient-domain-of domain)))))

(defgeneric one-matrix (domain &optional rank)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod one-matrix ((domain matrix-space) &optional rank)
  (unless (numberp rank)
    (error "Must specify rank to ONE-MATRIX (~D)" domain))
  (let* ((zero (zero (coefficient-domain-of domain)))
	 (one (one (coefficient-domain-of domain)))
	 (array (make-array (list rank rank) :initial-element zero)))
    (loop for i fixnum below rank
	  do (setf (aref array i i) one))
    (make-element domain array)))

(defmethod plus ((m1 matrix-space-element) (m2 matrix-space-element))
  (let ((domain (domain-of m1)))
    (cond ((eql domain (domain-of m2))
	   (with-matrix-dimensions (1dim1 1dim2 1array) m1
	     (with-matrix-dimensions (2dim1 2dim2 2array) m2
	       (unless (and (eql 1dim1 2dim1) (eql 1dim2 2dim2))
		 (error "Trying to add matrices of different dimensions: (~D ~D) and (~D ~D)"
			1dim1 1dim2 2dim1 2dim2))
	       (let ((array (make-array (list 1dim1 1dim2))))
		 (loop for i fixnum below 1dim1 do
		   (loop for j fixnum below 1dim2 do
		     (setf (aref array i j)
			   (+ (aref 1array i j) (aref 2array i j)))))
		 (make-element domain array)))))
	  (t (error "Can't add these matrices")))))

(defmethod difference ((m1 matrix-space-element) (m2 matrix-space-element))
  (let ((domain (domain-of m1)))
    (cond ((eql domain (domain-of m2))
	   (with-matrix-dimensions (1dim1 1dim2 1array) m1
	     (with-matrix-dimensions (2dim1 2dim2 2array) m2
	       (unless (and (eql 1dim1 2dim1) (eql 1dim2 2dim2))
		 (error "Trying to subtract matrices of different dimensions: (~D ~D) and (~D ~D)"
			1dim1 1dim2 2dim1 2dim2))
	       (let ((array (make-array (list 1dim1 1dim2))))
		 (loop for i fixnum below 1dim1 do
		   (loop for j fixnum below 1dim2 do
		     (setf (aref array i j)
			   (- (aref 1array i j) (aref 2array i j)))))
		 (make-element domain array)))))
	  (t (error "Can't subtract these matrices")))))


(defmethod-sd times ((m1 matrix-element) (m2 matrix-element))
  (with-matrix-dimensions (1dim1 1dim2 1array) m1
    (with-matrix-dimensions (2dim1 2dim2 2array) m2
      (unless (eql 1dim2 2dim1)
	(error "Trying to multiply matrices of incompatible dimensions: (~D ~D) and (~D ~D)"
	       1dim1 1dim2 2dim1 2dim2))
      (make-element domain (times-array 1array 1dim1 1dim2 2array 2dim2)))))

(defun array-times (array1 array2)
  (let ((dims1 (array-dimensions array1))
	(dims2 (array-dimensions array2)))
    (unless (and (eql (length dims1) 2)
		 (eql (length dims2) 2)
		 (eql (second dims1) (first dims2)))
      (error "Incompatible array dimensions"))
    (times-array array1 (first dims1) (second dims1) array2 (second dims2))))


(defun times-array (1array 1dim1 1dim2 2array 2dim2)
  (let ((array (make-array (list 1dim1 2dim2))))
    (loop for i fixnum below 1dim1 do
      (loop for j fixnum below 2dim2 do
	(loop for k fixnum below 1dim2
	      for c = (* (aref 1array i k) (aref 2array k j))
		then (+ c (* (aref 1array i k) (aref 2array k j)))
	      finally (setf (aref array i j) c))))
    array))

(defmethod times ((m matrix-space-element) (v free-module-element))
  (matrix-fme-times m v))

(defun matrix-fme-times (m v)  
  (let ((elt-domain (coefficient-domain-of (domain-of m)))
	(vector-space (domain-of v)))
    (cond ((eql elt-domain (coefficient-domain-of vector-space))
	   (with-matrix-dimensions (dim1 dim2 array) m
	     (unless (eql dim2 (dimension-of vector-space))
	       (error "Trying to multiply a matrix and vector of incompatible dimensions: (~D ~D) and ~D"
		      dim1 dim2 (dimension-of vector-space)))
	     (%apply #'make-element
		     (if (cl:= dim1 dim2) vector-space
			 (get-free-module elt-domain dim1))
		     (loop for i fixnum below dim1
			   collect
			   (loop for k fixnum below dim2 
				 for c = (* (aref array i k) (ref v k))
				   then (+ c (* (aref array i k) (ref v k)))
				 finally (return c))))))
	  (t (error "Incompatible arguments: ~S and ~S" m v)))))

(defmethod times ((v free-module-element) (m matrix-space-element))
  (fme-matrix-times v m))

(defun fme-matrix-times (v m)  
  (let ((elt-domain (coefficient-domain-of (domain-of m)))
	(vector-space (domain-of v)))
    (cond ((eql elt-domain (coefficient-domain-of vector-space))
	   (with-matrix-dimensions (dim1 dim2 array) m
	     (unless (eql (dimension-of vector-space) dim1)
	       (error "Trying to multiply a vector and matrix of incompatible dimensions:  ~D and (~D ~D)"
		      (dimension-of vector-space) dim1 dim2))
	     (%apply #'make-element
		     (if (cl:= dim1 dim2) vector-space
			 (get-free-module elt-domain dim2))
		     (loop for i fixnum below dim2
			   collect
			   (loop for k fixnum below dim1
				 for c = (* (ref v k) (aref array k i))
				   then (+ c (* (ref v k) (aref array k i)))
				 finally (return c))))))
	  (t (error "Incompatible arguments: ~S and ~S" v m)))))

(defgeneric transpose (matrix)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod transpose ((m matrix-element))
  (let ((domain (domain-of m)))
    (with-matrix-dimensions (dim1 dim2 array) m
      (let ((transpose (make-array (list dim2 dim1))))
	(loop for i fixnum below dim1 do
	  (loop for j fixnum below dim2 do
	    (setf (aref transpose j i) (aref array i j))))
	(make-element domain transpose)))))

(defmethod-sd direct-sum ((x matrix-element) (y matrix-element))
  (with-matrix-dimensions (x-rows x-cols x-array) x
    (with-matrix-dimensions (y-rows y-cols y-array) y
      (cond ((eql x-rows y-rows)
	     (let ((array (make-array (list x-rows (cl:+ x-cols y-cols)))))
	       (loop for i fixnum below x-rows
		     for j fixnum = 0
		     do (loop for k fixnum below x-cols
			      do (setf (aref array i j) (aref x-array i k ))
				 (incf j))
			(loop for k fixnum below y-cols
			      do (setf (aref array i j) (aref y-array i k))
				 (incf j)))
	       (make-element domain array)))
	    (t (error "Incompatable dimensions (~D, ~D) and (~D, ~D)"
		      x-rows x-cols y-rows y-cols))))))

(defmethod recip ((m matrix-element))
  (let ((domain (domain-of m)))
    (with-matrix-dimensions (dim1 dim2 array) m
      (unless (eql dim1 dim2)
	(error "Can't invert a non-square matrix"))
      (let* ((dims (array-dimensions array))
	     (new-array (make-array dims)))
	(loop for i below (first dims) do
	  (loop for j below (second dims) do
	    (setf (aref new-array i j) (aref array i j))))
	(make-element domain
	  (invert-array (coefficient-domain-of domain) new-array))))))

(defgeneric invert-array (domain array &optional into-array)
  (:documentation
   "The purpose of this function is unknown."))

;; Invert an array of elements of domain in an ordered or un-ordered
;; ring. This operation destroys the first array.
(defmethod invert-array (domain array &optional into-array)
  (let ((dimension (array-dimensions array)))
    (unless (and (null (rest (rest dimension)))
		 (eql (first dimension) (second dimension)))
      (error "Wrong dimensions for recip: ~S" array))
    (cond (into-array
	   (unless (eql dimension (array-dimensions into-array))
	     (error "Wrong dimensions for ~S, expected ~S"
		    into-array dimension)))
	  (t (setq into-array (make-array dimension))
	     (loop for i fixnum below (first dimension) 
		   with zero = (zero domain) and one = (one domain) do
	       (loop for j fixnum below (second dimension) do
		 (setf (aref into-array i j) (if (eql i j) one zero))))))
    (setq dimension (first dimension))
    (flet ((exchange-rows (j k)
	     (loop for i fixnum below dimension do
	       (rotatef (aref array j i) (aref array k i))
	       (rotatef (aref into-array j i) (aref into-array k i))))
	   (find-pivot-ordered (i) 
	     (loop for j fixnum upfrom (1+ i) below dimension
		   for elt = (aref array j i)
		   with max = (aref array i i) and row = i do
	       (when (> (abs elt) (abs max))
		 (setq max elt
		       row j))
		   finally  (return (values row max))))
	   (find-pivot-unordered (i)
	     (loop for j fixnum upfrom (1+ i) below dimension
		   for elt = (aref array j i)
		   with max = (aref array i i) and row = i do
	       (when (and (0? max) (not (0? elt)))
		 (setq max elt
		       row j))
		   finally (if (0? max)
			       (error "Matrix is singular")
			       (return (values row max)))))
	   (subtract-rows (row1 row2)
	     (unless (0? (aref array row2 row1))
	       (let ((mult (aref array row2 row1)))
		 (loop for j fixnum upfrom row1 below dimension do
		   (setf (aref array row2 j)
			 (- (aref array row2 j) (* mult (aref array row1 j)))))
		 (loop for j fixnum below dimension do
		   (setf (aref into-array row2 j)
			 (- (aref into-array row2 j) (* mult (aref into-array row1 j)))))))))
      ;; Triangulate
      (loop for i fixnum below dimension do 
	(multiple-value-bind (row pivot)
			     (if (ordered-domain? domain)
				 (find-pivot-ordered i)
			       (find-pivot-unordered i))
	  (unless (eql i row)
	    (exchange-rows i row))
	  ;; Make the pivot 1
	  (unless (1? pivot) 
	    (loop for j fixnum upfrom i below dimension do 
	      (setf (aref array i j) (/ (aref array i j) pivot)))
	    (loop for j below dimension do
	      (setf (aref into-array i j) (/ (aref into-array i j) pivot))))
	  (loop for j fixnum upfrom (1+ i) below dimension do
	    (subtract-rows i j))))

      ;; Backsolve
      (loop for i fixnum downfrom (1- dimension) above -1 do
	(loop for j fixnum downfrom (1- i) above -1 do
	  (subtract-rows i j))))
    into-array))

(defmethod substitute ((values list) (variables list) (m matrix-space-element)
		       &rest ignore)
  (declare (ignore ignore))
  (with-matrix-dimensions (dim1 dim2 array) m
     (let ((new-array (make-array (list dim1 dim2))))
       (loop for i fixnum below dim1 do
	 (loop for j fixnum below dim2 do
	   (setf (aref new-array i j)
		 (substitute values variables (aref array i j)))))
       (make-element (get-matrix-space (domain-of (aref new-array 0 0)))
		     new-array))))

(defmethod jacobian ((function-list list) (var-list list))
  (let* ((ring (domain-of (first function-list)))
	 (dim-col (length var-list))
	 (dim-row (length function-list))
	 (array (make-array (list dim-row dim-col))))
    (loop for poly in function-list
	  for i fixnum below dim-row
	  do (loop for var in var-list
		   for j fixnum  below dim-col
		   do (setf (aref array i j) (partial-deriv poly var))))
    (make-element (get-matrix-space ring) array)))

;; Matrix Groups

;;; ==========================================================================
;;; The Groups GL(n), SL(n), PSL(n), O(n), SO(n) with the following
;;; hierarchy:
;;;  
;;;  det<>0  GL(n)
;;;            |
;;;            |
;;;  det=+-1 PSL(n) -------------> O(n) M*M^t = In
;;;            |                    |
;;;            V                    |
;;;  det=1   SL(n)                  |
;;;            \                   /
;;;              \               /
;;;                \           /
;;;                  \       /
;;;                    \   /
;;;                    SO(n)
;;;
;;;
;;; ==========================================================================


;; The coefficient domain of GL-n must be a field otherwise, it will
;; not be a group.  This is not necessary for the other matrix groups
;; because the determinants are required to be units.

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator GL-n ((domain field) dimension)
    (make-instance 'GL-n
                   :coefficient-domain domain
                   :dimension dimension
                   :print-function 'GL-n-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'GL-n)
                        (eql (coefficient-domain-of d) domain)
                        (eql (dimension-of d) dimension)))))

(defun GL-n-print-object (domain stream)
  (let ((n (dimension-of domain)))
    (format stream "GL^~D(~S)" n (coefficient-domain-of domain))))

(defmethod print-object ((matrix GL-n-element) stream)
  (with-matrix-dimensions (dim1 dim2 array) matrix
    (format stream "~A<" (class-name (class-of (domain-of matrix))))
    (loop for i fixnum below dim1
	  do (princ "<" stream)
	     (loop for j fixnum below dim2
		   do (princ (aref array i j) stream)
		      (if (< (1+ j) dim2)
			  (princ ",  " stream)
			  (princ ">" stream)))	     
	     (if (< (1+ i) dim1)
		 (princ ",  " stream)
		 (princ ">" stream)))))

(define-domain-element-classes GL-n GL-n-element)

(defmethod matrix-dimensions ((m GL-n-element))
  (let ((dim (dimension-of (domain-of m))))
    (values dim dim)))

(defmethod make-element ((domain GL-n) (value array) &rest ignore)
  (declare (ignore ignore))
  (make-instance (first (domain-element-classes domain))
		 :domain domain :value value))

(defmethod weyl::make-element ((domain GL-n) (value array)
			       &rest ignore)
  (declare (ignore ignore))
  (destructuring-bind (x-dim y-dim) (array-dimensions value)
    (let ((coef-domain (coefficient-domain-of domain))
	  (array (make-array (list x-dim y-dim))))
      (loop for i below x-dim do
	(loop for j below y-dim do
	  (setf (aref array i j) (coerce (aref value i j) coef-domain))))
      (make-element domain value))))

(defmethod make-element ((domain GL-n) (value list) &rest values)
  (setq values (if (null values) value
		   (cons value values)))
  (unless (loop for row in (rest values)
		with n = (length (first values))
		do (unless (eql (length row) n)
		     (return nil))
		finally (return t))
    (error "All rows not the same length: ~S" values))
  (make-element domain
		(make-array (list (length values) (length (first values)))
			    :initial-contents values)))

(defmethod weyl::make-element ((domain GL-n) (value list) &rest values)
  (setq values (if (null values) value
		   (cons value values)))
  (unless (loop for row in (rest values)
		with n = (length (first values))
		do (unless (eql (length row) n)
		     (return nil))
		finally (return t))
    (error "All rows not the same length: ~S" values))
  (let* ((x-dim (length values))
	 (y-dim (length (first values)))
	 (array (make-array (list x-dim y-dim))))
    (loop for i fixnum  below x-dim
	  for row in values do
	    (loop for j fixnum below y-dim
		  for val in row do
		    (setf (aref array i j) val)))
    (make-element domain array)))

(defmethod one-matrix ((domain GL-n) &optional rank)
  (let ((computed-rank (dimension-of domain)))
    (if rank
	(if (not (eq rank computed-rank))
	    (error "rank argument conflicts with domain dimension")))
    (let* ((zero (zero (coefficient-domain-of domain)))
	   (one (one (coefficient-domain-of domain)))
	   (array (make-array (list computed-rank computed-rank)
			      :initial-element zero)))
      (loop for i fixnum below computed-rank do
	(setf (aref array i i) one))
      (make-element domain array))))

(defmethod one ((domain GL-n))
  (one-matrix domain))

(defmethod times ((m GL-n-element) (v free-module-element))
  (matrix-fme-times m v))

(defmethod times ((v free-module-element) (m GL-n-element))
  (fme-matrix-times v m))

;;
;; PSL(n) : group of matrices with determinant +1 or -1
;;

(defun PSL-n-print-object (domain stream)
  (let ((n (dimension-of domain)))
    (format stream "PSL^~D(~S)" n (coefficient-domain-of domain))))

(define-domain-element-classes PSL-n PSL-n-element)

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator PSL-n ((domain field) dimension)
    (make-instance 'PSL-n
                   :coefficient-domain domain
                   :dimension dimension
                   :print-function 'PSL-n-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'PSL-n)
                        (eql (coefficient-domain-of d) domain)
                        (eql (dimension-of d) dimension)))))
;;
;; SL(n) : group of matrices with determinant +1 
;;


(defun SL-n-print-object (domain stream)
  (let ((n (dimension-of domain)))
    (format stream "SL^~D(~S)" n (coefficient-domain-of domain))))

(define-domain-element-classes SL-n SL-n-element)

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator SL-n ((domain field) dimension)
    (make-instance 'SL-n
                   :coefficient-domain domain
                   :dimension dimension
                   :print-function 'SL-n-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'SL-n)
                        (eql (coefficient-domain-of d) domain)
                        (eql (dimension-of d) dimension)))))

(defmethod determinant ((m SL-n-element))
  (one (coefficient-domain-of (domain-of m))))

;;
;; O(n) : group of orthogonal matrices
;;


(defun O-n-print-object (domain stream)
  (let ((n (dimension-of domain)))
    (format stream "O^~D(~S)" n (coefficient-domain-of domain))))

(define-domain-element-classes O-n O-n-element)

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator O-n ((domain field) dimension)
    (make-instance 'O-n
                   :coefficient-domain domain
                   :dimension dimension
                   :print-function 'O-n-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'O-n)
                        (eql (coefficient-domain-of d) domain)
                        (eql (dimension-of d) dimension)))))
;;
;; SO(n) : orthogonal matrices with unit determinant
;;

(defun SO-n-print-object (domain stream)
  (let ((n (dimension-of domain)))
    (format stream "SO^~D(~S)" n (coefficient-domain-of domain))))

(define-domain-element-classes SO-n SO-n-element)

(defmethod recip ((m SO-n-element))
  (transpose m))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator SO-n ((domain field) dimension)
    (make-instance 'SO-n
                   :coefficient-domain domain
                   :dimension dimension
                   :print-function 'SO-n-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'SO-n)
                        (eql (coefficient-domain-of d) domain)
                        (eql (dimension-of d) dimension)))))



;;; ====================================================================
;;; Routines for obtaining determinants and subdeterminants of matrices
;;; ====================================================================

(defvar *work-array*)

(defvar *work-matrix*)

(defmacro with-open-modular-arith (p &body body)
  `(let ((.prime. ,p)
	 temp)
	(macrolet ((c+ (x y)
		     `(progn (setq temp (cl:+ (the fixnum ,x) (the fixnum ,y)))
			     (if (cl:> temp .prime.) (cl:- temp .prime.)
				 temp)))
		   (c- (x y)
		     `(cl:mod (cl:- (the fixnum ,x) (the fixnum ,y))
			      .prime.))
		   (c* (x y)
		     `(the fixnum (cl:mod (cl:* (the fixnum ,x)
						(the fixnum ,y))
					  .prime.))))
	  temp
	  ,@body)))

(defgeneric determinant (matrix)
  (:documentation
   "The purpose of this function is unknown."))

(defgeneric determinant* (domain matrix)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod determinant ((m matrix-element))
  (with-matrix-dimensions (dim1 dim2) m
    (if (/= dim1 dim2)
	(error "Matrix is not square: Can't compute the determinant"))
    (setq *work-array* (make-array (list dim1 dim2)))
    (determinant* (coefficient-domain-of (domain-of m)) m)))

(defmethod determinant* ((domain polynomial-ring) (m matrix-element))
  (setq *work-matrix* m)
  (interpolate domain 'Evaluate-matrix (degree-bounds m)))

(defgeneric degree-bounds (matrix)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod degree-bounds ((m matrix-element))
  (with-matrix-dimensions (dim1 dim2 array) m  
    (let ((vars (ring-variables (coefficient-domain-of (domain-of m))))
	  ring-var d)
	 (loop for var in vars do
	   (setq ring-var (coerce var (coefficient-domain-of (domain-of m))))
	   (setq d (degree (aref array 0 0) ring-var))
	   (loop for i below dim1 do
	     (loop for j below dim2 do
	       (if (> (degree (aref array i j) ring-var) d)
		   (setq d (degree (aref array i j)ring-var)))))
	   collect (* (min dim1 dim2) d)))))

(defmethod substitute ((values list) (variables list) (m matrix-element)
		       &rest ignore)
  (declare (ignore ignore))
  (if (not (typep (coefficient-domain-of (domain-of m))
		  'multivariate-polynomial-ring))
      (error "Expected ~S to be over a multivariate-polynomial-ring" m))
  (with-matrix-dimensions (dim1 dim2 array) m
    (let ((new-array (make-array (list dim1 dim2))))
	 (loop for i below dim1 do
	   (loop for j below dim2 do
	     (setf (aref new-array i j)
		   (substitute values variables (aref array i j)))))
	 (make-element (domain-of m) new-array))))

(defmethod coerce ((m matrix-element) (domain matrix-space))
  (with-matrix-dimensions (dim1 dim2 array) m
    (let ((new-array (make-array (list dim1 dim2))))
	 (loop for i below dim1 do
	   (loop for j below dim2 do
	     (setf (aref new-array i j)
		   (coerce (coerce (aref array i j) *general*)
			   (coefficient-domain-of domain)))))
	 (make-element domain new-array))))

;; Used as a black box for sparse multivariate interpolation.
;; Note the use of *work-matrix*.
(defmethod evaluate-matrix ((vals list))
  (let ((poly-ring (coefficient-domain-of (domain-of *work-matrix*)))
	(coef-domain (domain-of (first vals)))
	(domain (get-matrix-space (domain-of (first vals)))))
       (cond ((eql (coefficient-domain-of poly-ring) coef-domain)
	      (determinant
		(coerce
		  (substitute
		    (list-coerce vals poly-ring)
		    (list-coerce (ring-variables poly-ring) poly-ring)
		    *work-matrix*)
		  domain)))
	     (t (setq poly-ring (get-polynomial-ring
				  coef-domain
				  (ring-variables poly-ring)))
		(determinant
		  (coerce
		    (substitute
		      (list-coerce vals poly-ring)
		      (list-coerce (ring-variables poly-ring) poly-ring)
		      (coerce *work-matrix* (get-matrix-space poly-ring)))
		    domain))))))

(defmethod determinant* ((domain rational-integers) (m matrix-element))
  (loop for p in (choice-primes (hadamard-bound m))
	collect
	(make-element (get-finite-field p) 
	  (determinant (weyl:make-element
			   (get-matrix-space
			     (get-finite-field p)) (matrix-value m))))
	  into remainders
	finally (return (compute-result (use-chinese-remainder remainders)))))

(defgeneric hadamard-bound (matrix)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod hadamard-bound ((m matrix-element))
  (with-matrix-dimensions (dim1 dim2 array) m  
    (let ((d (aref array 0 0))
	  (m (min dim1 dim2)))
	 (loop for i below dim1 do
	   (loop for j below dim2 do
	     (if (> (aref array i j) d)
		 (setq d (aref array i j)))))
	 (* (expt m m) (expt d m)))))

(defmethod  determinant* ((domain GFp) (m matrix-element))
  (with-matrix-dimensions (dimension dim2 old-array) m
    (declare (ignore dim2))
    (let* ((determinant 1)
	   (sign 1)
	   (modulus (characteristic domain)))
    (flet ((exchange-rows (i j)
	     (loop for l from j below dimension do
	       (rotatef (aref *work-array* i l) (aref *work-array* j l)))
	     (setq sign (* sign -1)))
	   (find-pivot (j)
	     (loop for i fixnum upfrom j below dimension do
	       (if (not (= (aref *work-array* i j) 0))
		   (return i))
	       finally (return -1))))
    (loop for i fixnum below  dimension do
      (loop for j fixnum below dimension do
	(setf (aref *work-array* i j) (gfp-value (aref old-array i j)))))
    (with-open-modular-arith modulus
      (loop for j below (- dimension 1)
	    with row of-type fixnum and pivot fixnum and d do
	      (setq row (find-pivot j))
	      (if (/= row -1)
		  (setf determinant (c* determinant (aref *work-array* row j)))
		  (return (* sign determinant)))
	      (setq pivot (aref *work-array* row j))
	      (setq d (reduce-modulo-integer
		       (compute-inverse pivot modulus)
			     modulus))
;	    (format t "pivot = ~D d = ~D~%" pivot d)
	      (if (> row j)
		  (exchange-rows row j))
	      (loop for k upfrom (cl:+ j 1) below dimension
		    with ck do
		      (setq ck (c* d (aref *work-array* k j)))
;		    (format t "ck = ~D~%" ck)
		      (loop for l of-type fixnum upfrom j below dimension do
			(setf (aref *work-array* k l)
			      (c- (aref *work-array* k l)
				  (c* ck (aref *work-array* j l))))))
;	    (print *work-array*)
	    finally
	 (setq determinant (c* determinant (aref *work-array* j j))))))
    (* sign determinant))))

(defmethod determinant* ((domain field) (m matrix-element))
  (with-matrix-dimensions (dimension dim2 old-array) m
    (declare (ignore dim2))
    (let* ((determinant (one domain))
	   (sign (one domain)))
	  (flet ((exchange-rows (i j)
		   (loop for l from j below dimension do
		     (rotatef (aref *work-array* i l) (aref *work-array* j l)))
		   (setq sign (* sign (minus (one domain)))))
		 (find-pivot (j)
	           (loop for i fixnum upfrom j below dimension do
		     (if (not (0? (aref *work-array* i j)))
			 (return i))
			 finally
		      (return -1))))
          (loop for i fixnum below  dimension do
	    (loop for j fixnum below dimension do
	      (setf (aref *work-array* i j) (aref old-array i j))))
		;; Triangulate
	   (loop for j fixnum below (- dimension 1)
		 with row and pivot and d do
		   (setq row (find-pivot j))
		   (if (/= row -1)
		       (setf determinant (* determinant
					    (aref *work-array* row j)))
		       (return (zero domain)))
		   (setq pivot (aref *work-array* row j))
		   (setq d (recip pivot))
		   ;; (print (format nil "pivot = ~D d = ~D" pivot d))
		   (if (> row j)
		       (exchange-rows row j))
		   (loop for k fixnum upfrom (+ j 1) below dimension
			 with ck do
			   (setq ck (* d (aref *work-array* k j)))
			   ;; (print (format nil "ck = ~D" ck))
			   (loop for l fixnum upfrom j below dimension do
			     (setf (aref *work-array* k l)
				   (- (aref *work-array* k l)
				      (* ck (aref *work-array* j l))))))
		   ;; (print *work-array*)
		 finally
	      (return (* sign determinant (aref *work-array* j j))))))))

;; Use this method to find determinant only if the matrix is very sparse.
;; It computes determinant by expansion of the minors, so it is very slow
;; unless the matrix is very sparse.
(defmethod sparse-determinant ((m matrix-element))
  (with-matrix-dimensions (dim1 dim2 array) m
    (if (/= dim1 dim2)
	(error "Matrix is not square: Can't compute the determinant"))

    (let* ((domain (domain-of m))
	   (coefficient-domain (coefficient-domain-of domain))
	   (one (one coefficient-domain))	   
	   (zero (zero coefficient-domain)))
      (labels ((sparse-det (row cols)
		 #+ignore
	         (format t "in minor: row = ~D, cols = ~S~%" row cols)
		 (memoize `(sparse-det ,m ,row ,cols)
		   (if (null cols) one
		       (loop for col in cols
			     for positive-sign? = t then (not positive-sign?)
			     with minor and det = zero
			     do (unless (0? (aref array row col))
				  (setq minor 
					(* (aref array row col)
					   (sparse-det (1+ row) (remove col cols))))
				  (setq det
					(if positive-sign?
					    (+ det minor)
					    (- det minor))))
			     finally (return det))))))
    (sparse-det 0 (loop for i below dim1 collect i))))))

(defgeneric independent-rows (array)
  (:documentation
   "The purpose of this function is unknown."))

;; Independent rows of a LISP array are obtained by transforming it
;; into row echelon form.
(defmethod independent-rows (array)
  (let ((dim1 (first (array-dimensions array)))
	(dim2 (second (array-dimensions array))))
       (flet ((find-pivot (i)
		     (loop for j fixnum below dim2
		       do (if (not (0? (aref array i j)))
			      (return j))
			   finally
			(return -1))))
	     (loop for i fixnum below dim1 with col and pivot
		   do (setq col (find-pivot i))
		      (cond ((/= col -1)
			     (setq pivot (recip (aref array i col)))
			     (loop for k fixnum upfrom (+ i 1) below dim1
				   with ck
				   do (setq ck (* pivot (aref array k col)))
				      (loop for l fixnum upfrom col below dim2
					    do (setf (aref array k l)
						     (- (aref array k l)
							(* ck (aref array i l)))))))))
	     (loop for i below dim1
		   if (/= (find-pivot i) -1)
		     collect i))))

(defgeneric independent-cols (array)
  (:documentation
   "The purpose of this function is unknown."))

;; independent columns of the array are abtained by transforming it into
;; column echelon form.
(defmethod independent-cols (array)
  (let ((dim1 (first (array-dimensions array)))
	(dim2 (second (array-dimensions array))))
       (flet ((find-pivot (j)
			  (loop for i fixnum below dim1
			    do (if (not (0? (aref array i j)))
				   (return i))
				finally
			     (return -1))))
	     (loop for j fixnum below dim2
		   with row and pivot do
		     (setq row (find-pivot j))
		     (cond ((/= row -1)
			    (setq pivot (recip (aref array row j)))
			    (loop for l fixnum upfrom (+ j 1) below dim2
				  with ck
				  do (setq ck (* pivot (aref array row l)))
				     (loop for k fixnum upfrom row below dim1
					   do (setf (aref array k l)
						    (- (aref array k l)
						       (* ck (aref array k j)))))))))
	     (loop for j below dim2
		   if (/= (find-pivot j) -1)
		     collect j))))

(defgeneric subdeterminant (matrix)
  (:documentation
   "The purpose of this function is unknown."))

(defgeneric subdeterminant* (domain matrix)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod subdeterminant ((m matrix-element))
  (subdeterminant* (coefficient-domain-of (domain-of m)) m))

(defmethod subdeterminant* ((domain field) (m matrix-element))
  (with-matrix-dimensions (dim1 dim2 array) m
    (setq *work-array* (make-array (list dim1 dim2)))
    (let ((rows '())
	  (cols '())
	  (new-array nil))
	 (loop for i below dim1
	       do (loop for j fixnum below dim2
			do (setf (aref *work-array* i j) (aref array i j))))
	 (setq rows (independent-rows *work-array*))
	 (loop for i below dim1
	       do (loop for j of-type fixnum below dim2
			do (setf (aref *work-array* i j) (aref array i j))))
	 (setq cols (independent-cols *work-array*))
	 (if (/= (length rows) (length cols))
	     (error "Internal error: row rank is not equal to column rank"))
	 (setq new-array (make-array (list (length rows) (length cols))))
	 (loop for i upfrom 0 as row in rows
	       do (loop for j upfrom 0 as col in cols
			do (setf (aref new-array i j)
				 (aref array row col))))
	 (values (determinant* domain
			       (weyl:make-element (domain-of m)
				 new-array))
		 new-array
		 rows))))

;; First, find a prime for which the rank of the matrix m is maximum.
;; Then obtain independent rows and columns for that prime to obtain
;; the largest nonsingular submatrix.
(defmethod subdeterminant* ((domain rational-integers) (m matrix-element))
  (with-matrix-dimensions (dim1 dim2 array) m
    (setq *work-array* (make-array (list dim1 dim2)))
    (let ((primes (choice-primes (hadamard-bound m)))
	  (gfp nil)
	  (rows '())
	  (temp-rows '())
	  (cols '())
	  (prime-of-maxrank nil)
	  (new-array nil))
	 (loop for p in primes
	       do (setq gfp (get-finite-field p))
		  (loop for i below dim1
			do (loop for j fixnum below dim2
				 do (setf (aref *work-array* i j)
					  (coerce (aref array i j) gfp))))
		  (setq temp-rows (independent-rows *work-array*))
		  (cond ((> (length temp-rows) (length rows))
			 (setq rows temp-rows)
			 (setq prime-of-maxrank p))))
	 (setq gfp (get-finite-field prime-of-maxrank))
	 (loop for i below dim1
	       do (loop for j below dim2
			do (setf (aref *work-array* i j)
				 (coerce (aref array i j) gfp))))
	 (setq cols (independent-cols *work-array*))
	 (if (/= (length rows) (length cols))
	     (error "Internal error: row rank is not equal to column rank"))
	 (setq new-array (make-array (list (length rows) (length cols))))
	 (loop for i upfrom 0 as row in rows
	       do (loop for j upfrom 0 as col in cols
			do (setf (aref new-array i j)
				 (aref array row col))))
	 (values (determinant* domain
			       (weyl:make-element (domain-of m)
				 new-array))
		 new-array
		 rows))))

;;; ====================================================================
;;; Routines for obtaining Hermite and Smith normal forms of matrices
;;; over the ring of integers.
;;; ====================================================================

(defgeneric hermite* (domain matrix)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod hermite ((m matrix-element))
  (with-matrix-dimensions (dim1 dim2) m
    (if (> dim1 dim2)
	(setq m (transpose m)))
    (hermite* (coefficient-domain-of (domain-of m)) m)))

(defmethod hermite* ((domain rational-integers) (m matrix-element))
  (with-matrix-dimensions (dim1 dim2 array) m
    (declare (ignore dim1))
    (multiple-value-bind (R new-array rows) (subdeterminant m)
      (declare (ignore new-array))
      (let ((A (make-array (list (length rows) dim2))))
	   (loop for i upfrom 0 as row in rows
		 do (loop for j below dim2
			  do (setf (aref A i j)
				   (integer-value (aref array row j)))))
	   (weyl:make-element (domain-of m) (hermite-array A (abs R)))))))

;; Algorithm 2.4.8 on page 71 of the book by Cohen.
(defun hermite-array (A R)
  (let* ((m (first (array-dimensions A)))
	 (n (second (array-dimensions A)))
	 (B (make-array m))
	 (W (make-array (list m m)))
	 (j (- n 1))
	 (k j)
	 u v d q)
	(loop for i downfrom (- m 1) downto 0 by 1 do
	  (loop while (> j 0) do
	    (setf j (- j 1))
	    (cond
	      ((/= (aref A i j) 0)
	       (if (= (aref A i k) 0)
		   (setf (aref A i k) R))
	       (multiple-value-setq
		   (u v d) (extended-gcd (aref A i k) (aref A i j)))
	       (loop for l below m do
		 (setf (aref B l)
		       (cl:+ (cl:* u (aref A l k)) (cl:* v (aref A l j))))
		 (setf (aref A l j)
		       (sym-mod (cl:- (cl:* (cl:/ (aref A i k) d) (aref A l j))
				      (cl:* (cl:/ (aref A i j) d) (aref A l k)))
			   R))
		 (setf (aref A l k) (sym-mod (aref B l) R))))))
	  (multiple-value-setq (u v d) (extended-gcd (aref A i k) R))
	  (loop for l below m do
	    (setf (aref W l i) (mod (cl:* u (aref A l k)) R)))
	  (if (= (aref W i i) 0)
	      (setf (aref W i i) R))
	  (loop for j from (1+ i) below m do
	    (setf q (floor (cl:/ (aref W i j) (aref W i i))))
	    (loop for l below m do
	      (setf (aref W l j) (cl:mod (cl:- (aref W l j)
					       (cl:* q (aref W l i)))
				      R))))
	  (setf R (cl:/ R d))
	  (decf k)
	  (setf j k))
	W))

(defgeneric smith (matrix)
  (:documentation
   "The purpose of this function is unknown."))

(defgeneric smith* (domain matrix)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod smith ((m matrix-element))
  (with-matrix-dimensions (dim1 dim2) m
    (if (> dim1 dim2)
	(setq m (transpose m)))
    (smith* (coefficient-domain-of (domain-of m)) m)))

(defmethod smith* ((domain rational-integers) (m matrix-element))
  (multiple-value-bind (R array) (subdeterminant m)
    (let ((A (make-array (array-dimensions array))))
	 (loop for i below (first (array-dimensions array))
	       do (loop for j below (second (array-dimensions array))
			do (setf (aref A i j)
				 (integer-value (aref array i j)))))
	 (smith-array A (abs R)))))

;; Algorithm 2.4.14 on page 77 of the book by Cohen.
(defun smith-array (A R)
  (let* ((n (first (array-dimensions A)))
	 (B (make-array n))
	 (Diagonal (make-array n))
	 (foundb '())
	 (failed '())
	 u v d b1 k l)
    (flet
      ((processi (i)
	 (let ((j i)
	       (c 1))
          (loop while (> c 0) do
	    (setf c 0)
	    (loop while (> j 0) do
	      (setf j (- j 1))
	      (cond
		((/= (aref A i j) 0)
		 (multiple-value-setq
		     (u v d) (extended-gcd (aref A i i) (aref A i j)))
		 (loop for l below n do
		   (setf (aref B l)
			 (cl:+ (cl:* u (aref A l i)) (cl:* v (aref A l j))))
		   (setf (aref A l j)
			 (sym-mod
			  (cl:- (cl:* (cl:/ (aref A i i) d) (aref A l j))
				(cl:* (cl:/ (aref A i j) d) (aref A l i)))
			     R))
		   (setf (aref A l i) (sym-mod (aref B l) R))))))
	    (setf j i)
	    (loop while (> j 0) do
	      (setf j (- j 1))
	      (cond
		((/= (aref A j i) 0)
		 (multiple-value-setq
		     (u v d) (extended-gcd (aref A i i) (aref A j i)))
		 (loop for l below n do
		   (setf (aref B l)
			 (cl:+ (cl:* u (aref A i l)) (cl:* v (aref A j l))))
		   (setf (aref A j l)
			 (sym-mod (cl:- (cl:* (cl:/ (aref A i i) d)
					      (aref A j l))
				     (cl:* (cl:/ (aref A j i) d) (aref A i l)))
			     R))
		   (setf (aref A i l) (sym-mod (aref B l) R)))
		 (incf c))))))))
    (if (= n 1)
	(setf (aref Diagonal 0) R))
    (loop for i downfrom (- n 1) above 0 by 1 do
      (setf foundb '())
      (loop while (not foundb) do
	(processi i)
	(setf b1 (if (= (aref A i i) 0) R (aref A i i)))
	(setf k (- i 1))
	(setf l (- i 1))
	(setf failed '())
	(loop while (and (>= k 0) (not failed)) do
	  (loop while (and (>= l 0) (not failed)) do
	    (cond ((/= (rem (aref A k l) b1) 0)
		   (setf failed 1)
		   (loop for m below n do
		     (setf (aref A i m) (cl:+ (aref A i m) (aref A k m))))))
	    (decf l))
	  (setf l (- i 1))
	  (decf k))
	(if (not failed)
	    (setf foundb 1)))
      (setf (aref Diagonal i) (gcd (aref A i i) R))
      (setf R (cl:/ R (aref Diagonal i))))
    (setf (aref Diagonal 0) (gcd (aref A 0 0) R))
    Diagonal)))
