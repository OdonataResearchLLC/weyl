;;; -*- Mode:Lisp; Package:User; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;				  Lisp Support
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; lisp-support.lisp,v 1.8 1994/10/21 18:16:43 rz Exp

(in-package :common-lisp-user)

;;Infinities...
(defvar weyli::*positive-infinity*
  #+Genera si:infinite-positive-double-float
  #+Lucid system:float-positive-infinity
  #-(or Lucid Genera) most-positive-double-float)

(defvar weyli::*negative-infinity*
  #+Genera si:infinite-negative-double-float
  #+Lucid system:float-negative-infinity
  #-(or Genera Lucid) most-negative-double-float)

#|

 Extend defmethod slightly

 1. Allows the use of OR when defining the method specializer.

 DELETE : It is not clear how DEFMETHOD is being extended. Regardless,
 the code should be written to use the standard DEFMETHOD. When that is
 complete and verified, these macros should be deleted.

 This version is SBCL specific.
#+SB-PCL
(defmacro weyli::defmethod (&rest args &environment env)
  (labels ((duplicate-arglist (arglist)
	     (cond ((null arglist) (list nil))
		   ((or (atom (first arglist))
			(null (rest (first arglist)))
			(atom (second (first arglist)))
			(not (eql 'or (first (second (first arglist))))))
		    (mapcar (lambda (q) (cons (first arglist) q))
			    (duplicate-arglist (rest arglist))))
		   (t (loop for type in (rest (second (first arglist)))
			    with rest = (duplicate-arglist (rest arglist))
			    nconc (mapcar #'(lambda (q)
					      (cons (list (first (first arglist)) type)
                                                    q))
					  rest))))))
    (multiple-value-bind (name qualifiers lambda-list body)
	(sb-pcl::parse-defmethod args)
      (multiple-value-bind (proto-gf proto-method)
          (sb-pcl::prototypes-for-make-method-lambda name)
        `(progn
          ,@(loop for ll in (duplicate-arglist lambda-list)
                  collect
                  (sb-pcl::expand-defmethod name proto-gf proto-method qualifiers ll body env)))))))

#+(or MCL (and Genera CLOS))
(defun clos-parse-defmethod (form)
  (let ((name (pop form))
	qualifiers)
    (loop while (and (atom (first form))
		     (not (null (first form))))
	  do (push (pop form) qualifiers))
    (values name (reverse qualifiers) (first form) (rest form))))

#+CLOS
(defmacro weyli::defmethod (&rest args)
  #+Genera
  (declare (arglist name {method-qualifier}* specialized-lambda-list
		    &body body)
	   (zwei:indentation . wei:indent-for-clos-defmethod))
  (labels ((duplicate-arglist (arglist)
	     (cond ((null arglist) (list nil))
		   ((or (atom (first arglist))
			(null (rest (first arglist)))
			(atom (second (first arglist)))
			(not (eql 'or (first (second (first arglist))))))
		    (mapcar #'(lambda (q) (cons (first arglist) q))
			    (duplicate-arglist (rest arglist))))
		   (t (loop for type in (rest (second (first arglist)))
			    with rest = (duplicate-arglist (rest arglist))
			    nconc (mapcar #'(lambda (q)
					      (cons (list (first (first arglist)) type)
						    q))
					  rest))))))
    #-LispWorks
    (multiple-value-bind (name qualifiers lambda-list body)
        #+(or excl Lucid) (clos::parse-defmethod args)
        #+(or MCL Genera) (clos-parse-defmethod args)
        `(progn
          ,@(loop for ll in (duplicate-arglist lambda-list)
                  collect
                  #-MCL
                  `(clos::defmethod ,name ,@qualifiers ,ll ,@body)
                  #+MCL
                  `(,(if (or qualifiers ll) 'cl:defmethod 'defun) ,name ,@qualifiers
                    ,ll ,@body))))
    #+LispWorks
    (let ((name (first args)))
      (multiple-value-bind (qualifiers lambda-list body)
          (clos::parse-defmethod nil name (rest args))
        `(progn
	  ,@(loop for ll in (duplicate-arglist lambda-list)
		  collect
                  `(clos:defmethod ,name ,@qualifiers ,ll ,@body)))))))
|#

;;; The following predicate determines if class is a subclass of super-class.
;;; FIXME : Relies on the MOP and is SBCL specific.
(defun weyli::subclass-of? (class super-class)
  (when (symbolp class)
    (setq class (find-class class)))
  (when (symbolp super-class)
    (setq super-class (find-class super-class)))
  (labels ((search-list (list)
	     (or (member class list)
		 (loop for c in list
		       when (search-list (closer-mop:class-direct-superclasses c))
                       return t))))
    (or (eql class super-class)
	(search-list (closer-mop:class-direct-superclasses super-class)))))

(defmacro weyli::%apply (function &rest args)
  `(common-lisp:apply ,function ,@args))

(defun weyli::accum-apply-args (args)
  (if (null (rest args))
      (first args)
      (cons (first args) (weyli::accum-apply-args (rest args)))))

(defgeneric weyli::apply (function &rest args)
  (:documentation
   "Extend the common lisp standard APPLY."))

(defmethod weyli::apply (function &rest args)
  "Use the common lisp APPLY by default."
  (if (null args)
      (error "The function APPLY was called with too few arguments")
      (common-lisp:apply function (weyli::accum-apply-args args))))

(defmacro weyli::%funcall (function &rest args)
  `(common-lisp:funcall ,function ,@args))

(defgeneric weyli::funcall (function &rest args)
  (:documentation
   "Extend the common lisp standard FUNCALL."))

(defmethod weyli::funcall (function &rest args)
  "Rely on apply for performing the FUNCALL."
  (weyli::apply function args))

(defmacro weyli::%getf (place indicator &optional (default nil))
  (if default
      `(common-lisp:getf ,place ,indicator ,default)
      `(common-lisp:getf ,place ,indicator)))

(defgeneric weyli::getf (place indicator &optional default)
  (:documentation
   "Extend the common lisp standard GETF."))

(defmethod weyli::getf (place indicator &optional (default nil))
  "Use the common lisp GETF by default."
  (common-lisp:getf place indicator default))

(defgeneric weyli::putf (place indicator value)
  (:documentation
   "Extend the common lisp standard PUTF."))

(defmethod weyli::putf (place indicator value)
  "Use SETF and the common list standard GETF by default."
  (setf (common-lisp:getf place indicator) value))

(defsetf weyli::getf weyli::putf
    "Define weyli::getf settable.")

(defgeneric weyli::delete (item set &key &allow-other-keys)
  (:documentation
   "Extend the common lisp standard DELETE."))

(defmethod weyli::delete (item (sequence sequence) &rest args)
  "Use the common lisp DELETE function by default."
  (apply #'common-lisp:delete item sequence args))

(defgeneric weyli::member (item list &key &allow-other-keys)
  (:documentation
   "Extend the common lisp standard member."))

(defmethod weyli::member (item (list list) &rest args)
  "Use the common lisp MEMBER function by default."
  (apply #'common-lisp:member item list args))

(defgeneric weyli::replace (item list &key &allow-other-keys)
  (:documentation
   "Extend the common lisp standard replace."))

(defmethod weyli::replace ((item sequence) (list sequence) &rest args)
  "Use the common lisp REPLACE function by default."
  (apply #'common-lisp:replace item list args))

(defgeneric weyli::substitute (newitem olditem sequence &key &allow-other-keys)
  (:documentation
   "Extend the common lisp SUBSTITUTE function."))

(defmethod weyli::substitute (newitem olditem (seq sequence) &rest args)
  "Use the common lisp SUBSTITUTE function by default."
  (apply #'common-lisp:substitute newitem olditem seq args))

(defgeneric weyli::map (result-type function sequence &rest sequences)
  (:documentation
   "Extend the common lisp MAP function."))

(defmethod weyli::map (result-type function sequence &rest sequences)
  "Use the common lisp MAP function by default."
  (apply #'common-lisp:map result-type function sequence sequences))

(defgeneric weyli::reduce (function sequence &rest options)
  (:documentation
   "Extend the common lisp REDUCE function."))

(defmethod weyli::reduce (function (sequence sequence) &rest options)
  "Use the common lisp REDUCE function for sequences."
  (apply #'common-lisp:reduce function sequence options))

(defgeneric weyli::union (arg1 arg2 &rest rest)
  (:documentation
   "Extend the common lisp UNION function."))

(defmethod weyli::union ((arg1 list) (arg2 list) &rest rest)
  "Use the common lisp UNION function for lists."
  (apply #'common-lisp:union arg1 arg2 rest))

(defgeneric weyli::intersection (arg1 arg2 &rest rest)
  (:documentation
   "Extend the common lisp INTERSECTION function."))

(defmethod weyli::intersection ((arg1 list) (arg2 list) &rest rest)
  "Use the common lisp INTERSECTION function for lists."
  (apply #'common-lisp:intersection arg1 arg2 rest))

#+Genera
(eval-when (compile load eval)
  ;; Link the value cells of algebra:* and zl:*, etc.
  (unless (eq (locf (symbol-value 'weyli::*))
	      (locf (symbol-value 'zl:*)))
    (setq weyli::* zl:*)
    (si:link-symbol-value-cells 'weyli::* 'zl:*))
  (unless (eq (locf (symbol-value 'weyli::+))
	      (locf (symbol-value 'zl:+)))
    (setq weyli::+ zl:+)
    (si:link-symbol-value-cells 'weyli::+ 'zl:+)))

#+Lucid
(setf (symbol-function 'lucid-old-top-level-eval) #'lucid::top-level-eval)

#+Lucid
(defun  lucid::top-level-eval (&rest arguments)
  (declare (special weyli::* weyli::+ cl:* cl:+))
  (multiple-value-prog1 (apply #'lucid-old-top-level-eval arguments)
    (setq weyli::* cl:*)
    (setq weyli::+ cl:+)))

;;; DELETE : defsubst is an archaic technique. It should be replaced
;;; with function definitions and, if justified, INLINE declarations.
(defmacro weyli::defsubst (function lambda-list &body body)
  `(#+Genera scl:defsubst
    #+Lucid  lcl:defsubst
    #-(or Genera Lucid) defun
    ,function ,lambda-list ,@body))

(defun weyli::%copy-array-contents* (from-array to-array)
  "Perform a shallow copy of the array contents."
  (if (equal (array-dimensions from-array) (array-dimensions to-array))
      (let ((from-flat (make-array
                        (array-total-size from-array)
                        :element-type (array-element-type from-array)
                        :displaced-to from-array))
            (to-flat (make-array
                      (array-total-size to-array)
                      :element-type (array-element-type to-array)
                      :displaced-to to-array)))
        (dotimes (index (array-total-size from-array) to-array)
          (setf (aref to-flat index) (aref from-flat index))))
      (error "Array dimensions are not equal.")))

(defmacro weyli::copy-array-contents (from-array to-array)
  #+Genera
  `(scl:copy-array-contents ,from-array ,to-array)
  #-Genera
  `(weyli::%copy-array-contents* ,from-array ,to-array))

(defun weyli::circular-list (&rest arguments)
  #+Genera (apply #'scl:circular-list arguments)
  #-Genera (nconc arguments arguments))

(weyli::defsubst structure-of (x)
  (common-lisp:type-of x))

;;; The following macros deal with certain functions that should take an
;;; arbitrary number of arguments.

;;; FIXME : This symbol should not be in CL-USER.
(defun associate-predicate (predicate values)
  (let ((forms 
	 (loop for (x y) on values
	       when y
               collect `(,predicate ,x ,y))))
    (if (null (rest forms)) (first forms)
	(cons 'and forms))))

(defmacro weyli::< (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to <"))
	((null (rest values)) t)
	(t (associate-predicate 'weyli::binary< values))))

(defmacro weyli::= (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to ="))
	((null (rest values)) t)
	(t (associate-predicate 'weyli::binary= values))))

(defmacro weyli::> (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to >"))
	((null (rest values)) t)
	(t (associate-predicate 'weyli::binary> values))))

(defmacro weyli::<= (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to <="))
	((null (rest values)) t)
	(t (associate-predicate 'weyli::binary<= values))))

(defmacro weyli::>= (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to >="))
	((null (rest values)) t)
	(t (associate-predicate 'weyli::binary>= values))))

;;; FIXME : This symbol should not be in CL-USER.
(defun associate-operation (operation values)
  (labels ((iterate (values result)
	     (cond ((null values)
		    result)
		   (t (iterate (rest values)
			       `(,operation ,result ,(first values)))))))
    (iterate (rest values) (first values))))

(defmacro weyli::max (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to max"))
	((null (rest values))
	 (first values))
	(t (associate-operation 'weyli::max-pair values))))

(defun weyli::%max (&rest values)
  (if (null values)
      (error "Illegal number of arguments to max")
      (labels ((next-loop (vals)
	         (if (null (rest vals))
		     (first vals)
                     ;; FIXME : max-pair needs to be defined prior to this.
		     (weyli::max-pair (first vals) (next-loop (rest vals))))))
        (next-loop values))))  

(defmacro weyli::min (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to min"))
	((null (rest values))
	 (first values))
	(t (associate-operation 'weyli::min-pair values))))

(defun weyli::%min (&rest values)
  (if (null values)
      (error "Illegal number of arguments to min")
      (labels ((next-loop (vals)
	         (if (null (rest vals))
		     (first vals)
                     ;; FIXME : min-pair needs to be defined prior to this.
		     (weyli::min-pair (first vals) (next-loop (rest vals))))))
        (next-loop values))))

(defmacro weyli::+ (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to +"))
	((null (rest values))
	 (first values))
	(t (associate-operation 'weyli::plus values))))

(defun weyli::%plus (&rest values)
  (if (null values)
      (error "Illegal number of arguments to +")
      (labels ((next-loop (vals)
	         (if (null (rest vals))
		     (first vals)
                     ;; FIXME : plus needs to be defined prior to this.
		     (weyli::plus (first vals) (next-loop (rest vals))))))
        (next-loop values))))

(defmacro weyli::- (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to -"))
	((null (rest values))
	 `(weyli::minus ,(first values)))
	(t (associate-operation 'weyli::difference values))))

(defun weyli::%difference (&rest values)
  (if (null values)
      (error "Illegal number of arguments to -")
      (labels ((next-loop (vals)
	         (if (null (rest vals))
		     (first vals)
                     ;; FIXME : plus needs to be defined prior to this.
		     (weyli::plus (first vals) (next-loop (rest vals))))))
        (if (null (rest values))
            ;; FIXME : minus needs to be defined prior to this.
            (weyli::minus (first values))
            (next-loop values)))))

(defmacro weyli::* (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to *"))
	((null (rest values))
	 (first values))
	(t (associate-operation 'weyli::times values))))

(defun weyli::%times (&rest values)
  (if (null values)
      (error "Illegal number of arguments to *")
      (labels ((next-loop (vals)
	         (if (null (rest vals))
		     (first vals)
                     ;; FIXME : times needs to be defined prior to this.
		     (weyli::times (first vals) (next-loop (rest vals))))))
        (next-loop values))))

(defmacro weyli::/ (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to /"))
	((null (rest values))
	 `(weyli::recip ,(first values)))
	(t (associate-operation 'weyli::quotient values))))

(defun weyli::%quotient (&rest values)
  (if (null values)
      (error "Illegal number of arguments to -")
      (labels ((next-loop (vals)
	         (if (null (rest vals))
		     (first vals)
                     ;; FIXME : quotient needs to be defined prior to this.
		     (weyli::quotient (first vals) (next-loop (rest vals))))))
        (if (null (rest values))
            ;; FIXME : recip needs to be defined prior to this.
            (weyli::recip (first values))
            (next-loop values)))))

(defmacro weyli::gcd (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to GCD"))
	((null (rest values))
	 (first values))
	(t (associate-operation 'weyli::binary-gcd values))))

(defun weyli::%gcd (&rest values)
  (if (null values)
      (error "Illegal number of arguments to GCD")
      (labels ((next-loop (vals)
	         (if (null (rest vals))
		     (first vals)
                     ;; FIXME : binary-gcd needs to be defined prior to this.
		     (weyli::binary-gcd (first vals)
					(next-loop (rest vals))))))
        (next-loop values))))

(defmacro weyli::lcm (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to LCM"))
	((null (rest values))
	 (first values))
	(t (associate-operation 'weyli::binary-lcm values))))

(defun weyli::%lcm (&rest values)
  (if (null values)
      (error "Illegal number of arguments to LCM")
      (labels ((next-loop (vals)
	         (if (null (rest vals))
		     (first vals)
                     ;; FIXME : binary-lcm needs to be defined prior to this.
		     (weyli::binary-lcm (first vals)
					(next-loop (rest vals))))))
        (next-loop values))))

(defmacro weyli::floor (a &optional b)
  (if b `(weyli::floor2 ,a ,b) `(weyli::floor1 ,a)))

(defmacro weyli::ceiling (a &optional b)
  (if b `(weyli::ceiling2 ,a ,b) `(weyli::ceiling1 ,a)))

(defmacro weyli::round (a &optional b)
  (if b `(weyli::round2 ,a ,b) `(weyli::round1 ,a)))

(defmacro weyli::truncate (a &optional b)
  (if b `(weyli::truncate2 ,a ,b) `(weyli::truncate1 ,a)))

#|

DELETE : All of the following forms can probably be deleted.

#+PCL
(defvar pcl:*compile-class-hash* (make-hash-table :test #'eq))

#+PCL
(defun pcl:COMPILE-CLASS-METHODS-1 (classes)
  (clrhash pcl:*compile-class-hash*)
  (dolist (class-spec classes)
    (let ((class (cond ((symbolp class-spec) (pcl:find-class class-spec nil))
		       ((pcl:classp class-spec) class-spec))))
      (cond (class
	     (dolist (gf (pcl:class-direct-generic-functions class))
	       (unless (gethash gf pcl:*compile-class-hash*)
		 (setf (gethash gf pcl:*compile-class-hash*) T)
		 (pcl:notice-methods-change-1 gf))))
	    (t (warn "~A is neither a class nor the name of a class" class-spec))))))

#+PCL
(defmacro weyli::compile-class-methods (&rest classes)
  `(pcl:compile-class-methods-1 ',classes))

#-PCL
(defmacro compile-class-methods (&rest classes)
  (declare (ignore classes))
  "Ignored")

#+PCL
(defun weyli::class-uncompiled-methods (class-spec &optional (function #'print))
  (let ((class (cond ((symbolp class-spec) (pcl:find-class class-spec nil))
		     ((pcl:classp class-spec) class-spec))))
    (cond (class
	   (dolist (gf (pcl:class-direct-generic-functions class))
	     (dolist (method (pcl:generic-function-methods gf))
	       (unless (or (compiled-function-p (pcl:method-function method))
			   #+Genera
			   (typep (pcl:method-function method) 'sys:lexical-closure))
		 (funcall function method)))))
	  (t (warn "~A is neither a class nor the name of a class" class-spec)))))

#+PCL
(defun weyli::all-weyl-classes (&optional (function #'print))
  (let (list)
    (labels ((find-sub-classes (class)
	       (loop for class in (pcl:class-direct-subclasses class)
		     do (unless (member class list)
			  (push class list)
			  (funcall function class)
			  (find-sub-classes class)))))
      (find-sub-classes (pcl:find-class 'weyli::domain))
      (find-sub-classes (pcl:find-class 'weyli::domain-element))
      (find-sub-classes (pcl:find-class 'weyli::morphism)))))

#+PCL
(defun weyli::all-uncompiled-weyl-methods (&optional (function #'print))
  (let (list generic)
    (weyli::all-weyl-classes
      #'(lambda (class)
	  (weyli::class-uncompiled-methods class
	    #'(lambda (method)
	        (setq generic (pcl:method-generic-function method))
		(unless (member generic list)
		  (push generic list)
		  (funcall function generic))))))))

|#
