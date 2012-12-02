;;; -*- Mode:Lisp; Package:WEYLI; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			   Function Spaces
;;; ===========================================================================
;;; (c) Copyright 1993 Cornell University

;;; funct-spaces.lisp,v 1.7 1994/10/21 18:16:36 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.7")

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator function-space ((domain domain) (range field))
    (make-instance 'function-space :domain domain :range range
                   :coefficient-domain range)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'function-space)
                        (eql (funct-domain-of d) domain)
                        (eql (funct-range-of d) range)))))

;; The points of a function space are the functions themselves.  (i.e.
;; function-space-element's)

(def-binary-coercion inner-product
  "No way to compute the inner-product of ~S and ~S"
  "Ambiguous coercion for inner-product (~S, ~S)")

;; FIXTHIS:: I'm leaving this here for now, but being a Hilbert space
;; should bea property, not a class.  That way, function spaces AND
;; vector space could both be Hilbert Spaces.

;; Default norm for a Hilbert space
(defmethod norm ((x hilbert-space-element))
  (inner-product x x))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator hilbert-space ((domain dimensional-space)
                                        (range dimensional-space))
    (make-instance 'hilbert-space :domain domain :range range)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'free-module)
                        (eql (funct-domain-of d) domain)
                        (eql (funct-range-of d) range)))))


;; The following routines maintain a cache of abscissa vectors.

(defvar *maximum-number-of-cached-vectors* 12)
(defvar *cached-vectors* ())

(defun check-cached-vector (vector)
  (let ((len (length vector)))
    (flet ((equal-vector (vect)
	     (when (and vect (eql (length vect) len))
	       (loop for i below len
		     do (unless (= (aref vector i) (aref vect i))
			  (return nil))
		     finally (return t)))))
      (cond ((null *cached-vectors*)
	     (setq *cached-vectors*
		   (make-array *maximum-number-of-cached-vectors*))
	     (setf (svref *cached-vectors* 0) vector)
	     vector)
	    (t (loop for i below *maximum-number-of-cached-vectors*
		     for vect = (svref *cached-vectors* i)
		     do (cond ((null vect)
			       (setf (svref *cached-vectors* i) vector)
			       (return vector))
			      ((equal-vector vect)
			       (unless (zerop i)
				 (rotatef (svref *cached-vectors* (1- i))
					  (svref *cached-vectors* i)))
			       (return vect)))
		     finally
		  (setf (svref *cached-vectors*
			       (1- *maximum-number-of-cached-vectors*))
			vector)
		  (return vector)))))))

(defclass sampled-function (function-space-element)
  ((x :initarg :x
      :reader function-x)
   (y :initarg :y
      :reader function-y)))

(defmethod print-object ((obj sampled-function) stream)
  (let* ((x-coords (function-x obj))
	 (npts (array-dimension x-coords 0)))
    (format stream "#<SFun: [~S, ~S] (~D pts)>"
	    (aref x-coords 0) (aref x-coords (- npts 1))
	    npts)))

(defgeneric make-sampled-function (domain x y)
  (:documentation
   "The purpose of this function is unknown."))

;; Include domain here since eventually we'll need it.  For now we
;; use the global *FUNCTION-SPACE-RING*
(defmethod make-sampled-function ((domain function-space) x y)
  (make-instance 'sampled-function :domain domain
		 :x (check-cached-vector x) :y y))

(defmethod print-table
    ((func sampled-function) &optional (stream *standard-output*))
  (let ((x-array (function-x func))
	(y-array (function-y func)))
    (loop for i below (length x-array)
	  do (format stream "~D: ~D~%" (aref x-array i) (aref y-array i)))))

(defgeneric make-sampled-function-1 (domain x-list function)
  (:documentation
   "The purpose of this function is unknown."))

;; Creates a sampled function, by sampling the argument function at
;; the points given in x-list.
(defmethod make-sampled-function-1 ((domain function-space) x-list function)
  (let* ((num-pts (length x-list))
	 (y-array (make-array (list num-pts)))
	 (x-array
	  (if (listp x-list)
	      (make-array (list num-pts)
			  :initial-contents (mapcar #'float x-list))
	      x-list)))
    (loop for i below num-pts
	  do (setf (aref y-array i) (funcall function (aref x-array i))))
    (make-sampled-function domain x-array y-array)))

(defmethod make-sampled-function-1 ((domain (eql nil)) x-list function)
  (let* ((num-pts (length x-list))
	 (y-array (make-array (list num-pts)))
	 (x-array
	  (if (listp x-list)
	      (make-array (list num-pts)
			  :initial-contents (mapcar #'float x-list))
	      x-list))
	 domain range)
    (loop for i below num-pts
	  do (setf (aref y-array i) (funcall function (aref x-array i))))
    (setq domain (if (typep (aref x-array 0) 'domain-element)
		     (domain-of (aref x-array 0))
		   (domain-of (canonicalize-number (aref x-array 0)))))
    (setq range (if (typep (aref y-array 0) 'domain-element)
		    (domain-of (aref y-array 0))
		  (domain-of (canonicalize-number (aref y-array 0)))))
    (make-sampled-function (get-function-space domain range)
			   x-array y-array)))

;; Takes a sampled-function and re-samples it at n evenly spaced
;; points.

(defmethod resample ((func sampled-function) n)
  (let* ((x (make-array n))
	 (f-x (function-x func))
	 (x0 (aref f-x 0))
	 (step (/ (- (aref f-x (1- (length f-x))) x0) (1- n))))

    ;; Divide the domain of func into n-1 evenly spaced intervals.
    (loop for i below n
	  do (setf (aref x i) x0
		   x0 (+ x0 step)))
    (smooth2 func x)))

(defgeneric smooth2 (function x)
  (:documentation
   "The purpose of this function is unknown."))

;; Given sampled function, resample it at each point of new-x,
;; interpolating from func.
(defmethod smooth2 ((func sampled-function) new-x)
  (let ((new-y (make-array (length new-x)))
	(x (function-x func))
	(y (function-y func)))	  
    (loop for i below (length new-x)
	  for xval = (aref new-x i)
	  do (setf (aref new-y i) (polynomial-interpolate x y xval 4)))
    (make-sampled-function (domain-of func) new-x new-y)))
	  
(defmethod evaluate-at ((func sampled-function) pt)
  (polynomial-interpolate (function-x func) (function-y func) pt 4))

;; Intepolate the function that passes through x-vector/y-vector at
;; point x using a polynomial interpolate of degree n.
(defun polynomial-interpolate (x-vector y-vector x &optional (n 4))
  (let* ((dif (abs (- x (aref x-vector 0))))
	 (ns 0)
	 (vector-length (array-dimension x-vector 0))
	 p offset)
    ;; If the number of points is smaller than the desired order,
    ;; reduce the order.
    (setq n (min n vector-length))
    (setq p (make-array (list n)))
    (loop for i below vector-length
	  for dift = (abs (- x (aref x-vector i)))
	  do (when (< dift dif)
	       (setq ns i)
	       (setq dif dift)))  
    (setq offset (max 0 (floor (+ ns (- (/ n 2))
				  (if (minusp (- x (aref x-vector ns)))
				      0 1)))))
    (when (> (+ offset n) (1- vector-length))
      (setq offset (max 0 (- vector-length n)))) 
    (loop for i below n do
      (setf (aref p i) (aref y-vector (+ i offset))))
    (loop for m upfrom 1 below n do 
      (loop for i below (- n m) do
	(setf (aref p i)
	      (/ (+ (* (- x (aref x-vector (+ offset i m))) (aref p i))
		    (* (- (aref x-vector (+ offset i)) x) (aref p (1+ i))))
		 (- (aref x-vector (+ offset i))
		    (aref x-vector (+ offset i m)))))))
    (aref p 0)))
	    
(defmethod-sd plus ((func1 sampled-function) (func2 sampled-function))
  (let ((x-array (function-x func1)))
    (unless (eql x-array (function-x func2))
      (error "Different supports PLUS, ~A, ~A" func1 func2))
  (let* ((num-pts (length x-array))
	 (y1 (function-y func1))
	 (y2 (function-y func2))
	 (sum (make-array (list num-pts))))
      (dotimes (i num-pts)
	(setf (aref sum i) (cl:+ (aref y1 i) (aref y2 i))))
      (make-sampled-function domain x-array sum))))

(defmethod-sd difference ((func1 sampled-function) (func2 sampled-function))
  (let ((x-array (function-x func1)))
    (unless (eql x-array (function-x func2))
      (error "Different supports DIFFERENCE, ~A, ~A" func1 func2))
  (let* ((num-pts (length x-array))
	 (y1 (function-y func1))
	 (y2 (function-y func2))
	 (sum (make-array (list num-pts))))
      (dotimes (i num-pts)
	(setf (aref sum i) (cl:- (aref y1 i) (aref y2 i))))
      (make-sampled-function domain x-array sum))))

(defmethod-sd times ((func1 sampled-function) (func2 sampled-function))
  (let ((x-array (function-x func1)))
    (unless (eql x-array (function-x func2))
      (error "Different supports PLUS, ~A, ~A" func1 func2))
  (let* ((num-pts (length x-array))
	 (y1 (function-y func1))
	 (y2 (function-y func2))
	 (prod (make-array (list num-pts))))
      (dotimes (i num-pts)
	(setf (aref prod i) (cl:* (aref y1 i) (aref y2 i))))
      (make-sampled-function domain x-array prod))))

(defmethod map
    ((result-type (eql 'sampled-function)) oper (func sampled-function)
     &rest ignore)
  (declare (ignore ignore))
  (let* ((y (function-y func))
	 (new-y (make-array (list (length y)))))
    (dotimes (i (length y))
      (setf (aref new-y i) (funcall oper (aref y i))))
    (make-sampled-function (domain-of func) (function-x func) new-y)))

(defmethod minus ((func sampled-function))
  (map 'sampled-function #'cl:- func))

(defmethod times ((num number) (func sampled-function))
  (map 'sampled-function #'(lambda (x) (* x num)) func))

(defmethod times ((num numeric) (func sampled-function))
  (map 'sampled-function #'(lambda (x) (* x num)) func))

(defmethod times ((func sampled-function) (num number))
  (map 'sampled-function #'(lambda (x) (* x num)) func))

(defmethod times ((func sampled-function) (num numeric))
  (map 'sampled-function #'(lambda (x) (* x num)) func))

(defmethod quotient ((func sampled-function) (num number))
  (map 'sampled-function #'(lambda (x) (/ x num)) func))

(defmethod quotient ((func sampled-function) (num numeric))
  (map 'sampled-function #'(lambda (x) (/ x num)) func))

(defmethod conjugate ((func sampled-function))
  (map 'sampled-function #'cl:conjugate func))

(defmethod realpart ((func sampled-function))
  (map 'sampled-function #'cl:realpart func))

(defmethod imagpart ((func sampled-function))
  (map 'sampled-function #'cl:imagpart func))

(defmacro deriv-2point (x y index1 index2)
  `(/ (- (aref ,y ,index2) (aref ,y ,index1))
      (- (aref ,x ,index2) (aref ,x ,index1))))

(defmethod deriv ((func sampled-function) &rest vars)
  (declare (ignore vars))
  (let* ((x-array (function-x func))
	 (y-array (function-y func))
	 (num-pts (length x-array))
	 (y-prime (make-array (list num-pts))))
    (setf (aref y-prime 0) (deriv-2point x-array y-array 1 0))
    (do ((i 1 (+ i 1)))
	((= i (1- num-pts))
	 (setf (aref y-prime i) (deriv-2point x-array y-array (- i 1) i)))
      (setf (aref y-prime i)
	    (/ (+ (deriv-2point x-array y-array (- i 1) i)
		  (deriv-2point x-array y-array  i (+ i 1))) 2)))
    (make-sampled-function (domain-of func) x-array y-prime)))

(defmacro trapezoidal (x y i1 i2)
  `(/ (* (- (aref ,x ,i2) (aref ,x ,i1))
	  (+ (aref ,y ,i1) (aref ,y ,i2))) 2))

(defgeneric integral (function &key lower upper &allow-other-keys)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod integral ((func sampled-function)
		     &key lower upper &allow-other-keys)
  (cond ((and (null lower) (null upper))
	 (indef-integral func))
	(t (let* ((x (function-x func))
		  (y (function-y func))
		  (num-pts (length x))
		  (sum 0.0))
	     ;; FIXTHIS: The trapezoidal rule is used here because it
	     ;; seems a bit more accurate.  But its really stupid.
	     ;; Since we know how we are interpolating the interval
	     ;; between any pair of points we should use an exact
	     ;; integration scheme.
	     (do ((i1 0 i2)
		  (i2 1 (cl:1+ i2)))
		 ((or (cl:>= i2 num-pts) (cl:<= upper (aref x i1)))
		  sum)
		 (setq sum (+ sum (trapezoidal x y i1 i2))))
	     #+ignore 
	     (do ((i1 0 (+ 2 i1))
		  (i2 1 (+ 2 i2))
		  (i3 2 (+ 2 i3)))
		 ((or (cl:>= i3 num-pts) (cl:<= upper (aref x i1)))
		  sum)
	       (multiple-value-bind (w1 w2) (simpson x y i1 i2 i3)
		 (setq sum (+ sum w1 w2))))))))

;; FIXTHIS:  I don't trust this routine. 
(defun simpson (x y i1 i2 i3)
  (let* ((x1 (svref x i1))
	 (x2 (svref x i2))
	 (x3 (svref x i3))
	 (h (- x2 x1))
	 (k (- x3 x2))
	 (range (- x2 x1))
	 (denom (* h k range))
	 (ksq (* k k))
	 (hsq (* h h))

	 (y1 (svref y i1))
	 (y2 (svref y i2))
	 (y3 (svref y i3))

	 (a (/ (- (+ (* k y1) (* h y3)) (* range y2)) (* 3 denom)))
	 (b (/ (+ (* hsq (- y3 y2)) (* ksq (- y2 y1))) (* 2 denom)))
	 (c y2))
    (values (+ (- (* a hsq h) (* b hsq)) (* c h))
	    (+ (* a ksq k) (* b ksq) (* c k)))))

(defun indef-integral (func)
  (let* ((x (function-x func))
	 (y (function-y func))
	 (num-pts (length x))
	 (int-y (make-array (list num-pts)))
	 (sum 0.0))
    (setf (aref int-y 0) 0.0)
    (do ((i1 0 (+ 2 i1))
	 (i2 1 (+ 2 i2))
	 (i3 2 (+ 2 i3)))
	((cl:>= i3 num-pts)
	 (if (= i2 (1- num-pts))
	     (setf (aref int-y i2) (+ sum (trapezoidal x y i1 i2))))
	 (make-sampled-function (domain-of func) x int-y))
      (multiple-value-bind (w1 w2) (simpson x y i1 i2 i3)
	(setq sum (+ sum w1))
	(setf (aref int-y i2) sum)
	(setq sum (+ sum w2))
	(setf (aref int-y i3) sum)))))

(defmethod-sd inner-product ((x sampled-function) (y sampled-function))
  (let* ((fun (* x (conjugate y)))
	 (abscissa (function-x x))
	 (dim (array-dimension abscissa 0)))
    (integral fun :lower (aref abscissa 0)
	      :upper (aref abscissa (1- dim)))))
	    

#| Test programs |

(defun sine-waves (n-pts n)
  (let ((abscissa (loop for x below 1 by (cl:/ n-pts)
			collect x)))
    (loop for i below n
	  collect (make-sampled-function-1
		       abscissa
		       #'(lambda (x) (sin (* 2 pi (1+ i) x)))))))

;; The following should generate Legendre polynomials!
(defun polys (n-pts n)
  (let ((abscissa (loop for x upfrom -1 below 1 by (cl:/ 2.0 n-pts)
			collect x)))
    (loop for i below n
	  collect (make-sampled-function-1
		       abscissa
		       #'(lambda (x) (cl:expt x i))))))

;; This uses randomly selected points.
(defun rpolys (n-pts n)
  (let ((abscissa (sort
		   (append (list 1.0 -1.0)
			   (loop for i below (- n-pts 2)
				 collect (/ (float (- (cl:random 20000) 10000))
					    10000)))
		   #'cl:<)))
    (loop for i below n
	  collect (make-sampled-function-1 abscissa
					   #'(lambda (x) (cl:expt x i))))))

;; Each argument is supposed to be a vector.  The vectors are are
;; normalized and an orthorgonal basis is returned.
(defun gram-schmidt (&rest vects)
  (let* ((dim (length vects))
	 (space (apply #'vector vects)))
    (loop for k below dim
	  for vect = (aref space k)
	  do ;; Normalize vector
	     (setq vect (/ vect (sqrt (abs (inner-product vect vect)))))
	     (setf (aref space k) vect)
	     (loop for j upfrom (+ k 1) below dim
		   for vect2 = (aref space j)
		   do (setf (aref space j)
			    (- vect2 (* (inner-product vect vect2) vect)))))
    space))

(defmethod print-Mathematica
    ((func sampled-function) &key (smooth 4) (stream *standard-output*)) 
  (let* ((x-array (function-x func))
	 (y-array (function-y func)))

    (when (and (not (null smooth))
	       (not (cl:numberp smooth)))
      (setq smooth 4))

    (format stream "Graphics[{GrayLevel[0.0]")
    (loop for i upfrom 1 below (length x-array)
	  for x0 = (aref x-array 0) then x1
	  and y0 = (aref y-array 0) then y1
	  and x1 = (aref x-array i)
	  and y1 = (aref y-array i)
	  do  (if (null smooth)
		 (format stream ",~%Line[{{~10F, ~10F}, {~10F, ~10F}}]"
			 x0 y0 x1 y1)
	       (loop for j below smooth
		     with delta = (cl:/ (cl:- x1 x0) smooth)
		     and xp = x0 and yp = y0 and xn and yn
		     do (setq xn (cl:+ xp delta)
			      yn (evaluate-at func xn))
		     (format stream ",~%Line[{{~10F, ~10F}, {~10F, ~10F}}]"
			     xp yp xn yn)
		     (setq xp xn yp yn))))
    (format stream "}]")))

||#
