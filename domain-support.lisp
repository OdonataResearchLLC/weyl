;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;				  Domains
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; domain-support.lisp,v 1.7 1995/05/24 17:41:59 rz Exp

(in-package "WEYLI")

;;; DELETE (make::adjust-version-numbers Weyl "1.7")

(defclass has-property-list ()
  ((property-list :initform nil
                  :accessor property-list-of)))

(defmethod getf ((obj has-property-list) key &optional (default nil))
  (common-lisp:getf (property-list-of obj) key default))

(defmethod putf ((obj has-property-list) key value)
  (setf (common-lisp:getf (property-list-of obj) key) value))

(defun domain-print-object (d stream)
  (format stream "#<Domain: ~A>" (class-name (class-of d))))

(defclass domain (has-property-list)
  ((operation-table :initform (make-hash-table))
   (super-domains :initform nil
		  :initarg :super-domains
		  :accessor super-domains-of)
   (morphisms-from :initform nil
                   :accessor domain-morphisms-from)
   (morphisms-to :initform nil
                 :accessor domain-morphisms-to)
   (print-function :initform #'domain-print-object
                   :initarg :print-function)))

;;; FIXME : Merge with domain-print-object.
(defmethod print-object ((d domain) stream)
  (with-slots (print-function) d
    ;; This is so that you can pretty print objects in lucid.  It
    ;; appears, that you are not supposed to use PRINC inside these
    ;; methods.
    #+Lucid
    (let ((*print-pretty* nil))
      (funcall print-function d stream))
    #-Lucid
    (funcall print-function d stream)))

(defmacro define-operations (domain &body operations)
  `(defmethod parse-operations :after ((d ,domain))
    (parse-operation-list d ',operations)))

(defgeneric parse-operation-list (domain operation-list)
  (:documentation
   "The purpose of this method is not known."))

(defmethod parse-operation-list ((d domain) operation-list)
  (with-slots (operation-table) d
    (loop for ((operation . arguments) nil values) on operation-list by #'cdddr
	  do (setf (gethash operation operation-table)
		   (list operation arguments values)))))

;;; Need a dummy primary method to hang all the :after methods on.
;;; FIXME : Organize so that the primary method is not useless.
(defgeneric parse-operations (domain)
  (:method ((domain domain))
    nil)
  (:documentation
   "The purpose of this method is not known."))

;;; FIXME : Audit for merging with parse-operations.
(defmethod initialize-instance :after ((d domain) &rest plist)
  (declare (ignore plist))
  (parse-operations d))

(defgeneric list-operations (domain)
  (:documentation
   "Return a list of operations for the domain."))

;;; FIXME : Convert the maphash to a LOOP.
(defmethod list-operations ((d domain))
  (with-slots (operation-table) d
    (let (ops)
      (maphash #'(lambda (key value)
		   (declare (ignore value))
		   (push key ops))
	       operation-table)
      ops)))

(defgeneric operation-arguments (domain operation)
  (:documentation
   "The purpose of this method is not known."))

(defmethod operation-arguments ((d domain) operation)
  (with-slots (operation-table) d
    (subst (class-name (class-of d)) 'self
	   (second (gethash operation operation-table)))))

(defgeneric operation-values (domain operation)
  (:documentation
   "The purpose of this method is not known."))

(defmethod operation-values ((d domain) operation)
  (with-slots (operation-table) d
    (subst (class-name (class-of d)) 'self
	   (third (gethash operation operation-table)))))

(defgeneric describe-operations (domain &optional no-complaints)
  (:documentation
   "The purpose of this method is not known."))

#+Genera
(defmethod describe-operations ((d domain) &optional no-complaints)
  (declare (ignore no-complaints))
  (let* ((class-name (class-name (class-of d)))
	 (domain-element (cond ((null (rest (get class-name 'domain-elements)))
				(first (get class-name 'domain-elements)))
			       (t (format nil "~A element" class-name)))))
    (labels ((canonicalize-class (name)
	       (cond ((eql name 'self) class-name)
		     ((atom name) name)
		     ((equal name '(element self))
		      domain-element)
		     (t (mapcar #'canonicalize-class name)))))
      (format t "~&~S is a ~A~%" d class-name)
      (fresh-line)
      (with-slots (operation-table) d
	(scl:formatting-table ()
	  (scl:with-character-style ('(nil :italic nil))
	    (scl:formatting-row ()
	      (scl:formatting-cell ()
		(princ "Operation"))
	      (scl:formatting-cell ()
		(princ "Arguments"))
	      (scl:formatting-cell ()
		(princ "Values"))))
	  (maphash #'(lambda (key value)
		       (declare (ignore key))
		       (scl:formatting-row ()
			 (scl:formatting-cell ()
			   (princ (first value)))
			 (scl:formatting-cell ()
			   (format t "~A~{, ~A~}"
				   (canonicalize-class (first (second value)))
				   (mapcar #'canonicalize-class
					   (rest (second value)))))
			 (scl:formatting-cell ()
			   (princ (canonicalize-class (third value))))))
		   operation-table))))))

#-Genera
(defmethod describe-operations ((d domain) &optional no-complaints)
  (declare (ignore no-complaints))
  (let* ((class-name (class-name (class-of d)))
	 (element-classes (get class-name 'element-classes))
	 (domain-element (cond ((and element-classes
				     (null (rest element-classes)))
				(first element-classes))
			       (t (format nil "~A element" class-name)))))
    (labels ((canonicalize-class (name)
	       (cond ((eql name 'self) class-name)
		     ((atom name) name)
		     ((equal name '(element self))
		      domain-element)
		     (t (mapcar #'canonicalize-class name)))))
      (format t "~&~S is a ~A~%" d class-name)
      (fresh-line)
      (with-slots (operation-table) d
	(format t "Operation Arguments Values")
	(maphash #'(lambda (key value)
		     (declare (ignore key))
		     (format t "~&(~A ~A~{, ~A~}) -> ~A~%"
			     (first value)
			     (canonicalize-class (first (second value)))
			     (mapcar #'canonicalize-class
				     (rest (second value)))
			     (canonicalize-class (third value))))
		 operation-table)))))

(defgeneric required-operations (domain &optional fun)
  (:documentation
   "The purpose of this method is not known."))

(defmethod required-operations ((d domain) &optional fun)
  (let* ((class-name (class-name (class-of d)))
	 (element-classes (get class-name 'element-classes))
	 (domain-element (cond ((and element-classes
				     (null (rest element-classes)))
				(first element-classes))
			       (t (cons 'or element-classes))))
         list)
    (labels ((canonicalize-class (name)
	       (cond ((eql name 'self) class-name)
		     ((atom name) name)
		     ((equal name '(element self))
		      domain-element)
		     (t (mapcar #'canonicalize-class name)))))
      
      (unless fun
	(setq fun #'(lambda (form)
		      (push (cons (first form)
				  (mapcar #'canonicalize-class (second form)))
			    list))))
      (with-slots (operation-table) d
        (maphash #'(lambda (key value)
                     (declare (ignore key))
                     (%funcall fun value))
                 operation-table))
      list)))

(defun map-over-arglist-combinations (self arglist fun)
  (labels ((recur (arglist types) 
	     (cond ((null arglist)
		    (%funcall fun (reverse types)))
		   ((atom (first arglist))
		    (recur (rest arglist) (cons (first arglist) types)))
		   ((eql (first (first arglist)) 'or)
		    (loop for type in (rest (first arglist))
			  do (recur (cons type (rest arglist)) types)))
		   ((eql (first (first arglist)) 'element)
		    (loop for type in (get self 'element-classes)
			  do (recur (cons type (rest arglist)) types)))
		   (t (error "Don't understand arglist entry: ~S"
			     (first arglist))))))
    (recur (first arglist) ())))  

;;; DELETE : The method does not appear to be used anywhere.
(defgeneric check-domain (domain)
  (:documentation
   "The purspose of this method is not known."))

;;; FIXME : SBCL specific. Need to abstract for other implementations.
#+SB-MOP
(defmethod check-domain ((d domain))
  (required-operations
   d
   (lambda (form)
     (let ((operation (first form))
           (args (rest form))) 
       (map-over-arglist-combinations
        (class-name (class-of d)) args
        #'(lambda (arg-names) 
            (let ((args (loop for type in arg-names
                              collect (find-class type nil))))
              (loop for method in (sb-mop:generic-function-methods
                                   (symbol-function operation))
                    do (when (equal args
                                    (sb-mop::method-specializers method))
                         (return t))
                    finally (format t "No method for ~S~%"
                                    (cons operation arg-names))))))))))

;; Domain creators

;;; FIXME : Need to make creating domains atomic so that domains are
;;; not added to the list unless they are actually created.
(defvar *domains* ()
  "List of domains currently in use")

(defvar *general* ()  
  "The general representation domain")

(defun reset-domains ()
  (setq *domains* nil)
  (setf (domain-morphisms-from *general*) nil)  
  (setf (domain-morphisms-to *general*) nil))

(defmacro add-domain (predicate &body body)
  `(add-domain-internal ,predicate #'(lambda () ,@body)))

(defun add-domain-internal (predicate body)
  (let ((domain (find nil *domains*
                      :test #'(lambda (a b) 
                                (declare (ignore a))
                                (%funcall predicate b)))))
    (when (null domain)
      (setq domain (%funcall body))
      (push domain *domains*))
    domain))

(defun false (&rest args)
  (declare (ignore args))
  nil)

(defun true (&rest args)
  (declare (ignore args))
  t)

;;; FIXME : Need to ensure that the generic function is defined prior
;;; to the methods. The exact semantics depend on how this is used. It
;;; either needs to test for the existing of a generic function and
;;; create one if it doesn't exist or just create one if there should
;;; not already be one.
(defmacro define-domain-creator (name args creator &key predicate body)
  (labels ((parse-args (args)
	     (cond ((null args)
		    args)
		   ((member (first args) '(&optional &key))
		    (parse-args (rest args)))
		   ((eql (first args) '&rest)
		    (error "Can't handle &rest args here"))
		   ((atom (first args))
		    (cons (first args) (parse-args (rest args))))
		   (t (cons (first (first args))
			    (parse-args (rest args)))))))
    (let ((internal-fun (intern (format nil "MAKE-~A*" name)))
	  (true-args (parse-args args)))
      `(progn
        (defmethod ,internal-fun ,args ,creator)
        (defmethod ,(intern (format nil "MAKE-~A" name)) ,args
          (add-domain #'false (,internal-fun ,@true-args)))
        ,@(when predicate
                `((defmethod ,(intern (format nil "GET-~A" name)) ,args
                    (add-domain ,predicate (,internal-fun ,@true-args)))))
        ,@(when body
                `((defmethod ,(intern (format nil "GET-~A" name)) ,args
                    ,body)))))))

(defmacro with-new-weyl-context ((plist) &body body)
  `(let ((*domains* nil)
	 (*allow-coercions*
	  ,(or (%getf plist :allow-coercions) '*allow-coercions*)))
    ,@body))  

;; All elements of a domain should include this class

(defclass domain-element ()
  ((domain :initarg :domain
	   :reader domain-of)))

(defmacro define-domain-element-classes (domain &body element-classes)
  `(progn
    ;; At one time we thought there would be a one to one
    ;; correspondence between classes of domains and the classes of
    ;; their elements.  This isn't the case.  In addition, no uses
    ;; the element-class to domain-class correspondence, as you would
    ;; expect, so I'm not bothering to keep track of it.   --RZ 7/12/94
    #+ignore
    ,@(loop for element-class in element-classes
            collect
            `(cond ((eql (get ',element-class 'domain-class) ',domain))
              (t
               (when (get ',element-class 'domain-class)
                 (format t "WARNING: Reset domain-class of ~S~%"
                         ',element-class))
               (setf (get ',element-class 'domain-class) ',domain))))
    (setf (get ',domain 'element-classes) ',element-classes)))

(defgeneric domain-element-classes (domain)
  (:method ((domain domain))
    (get (class-name (class-of domain)) 'element-classes))
  (:documentation
   "The purpose of this method is not known."))

;; This is so that you can pretty print objects in lucid.  It appears,
;; that you are not supposed to use PRINC inside these methods.
#+Lucid
;; This must be an :around method since it must come before all the
;; primary methods.
(defmethod print-object :around ((object domain-element) stream)
  (let ((*print-pretty* nil))
    (call-next-method)))

(defgeneric coerce (elt domain)
  (:documentation
   "Coerce the element into the domain."))

(defgeneric coercible? (elt domain)
  (:documentation
   "Return true if the element is coercible into the domain."))

(defmacro defmethod-sd (op (x-spec y-spec) &body body)
  #+Genera
  (declare (zwei:indentation . wei:indent-for-clos-defmethod))
  (let ((x (if (atom x-spec) x-spec (first x-spec)))
	(y (if (atom y-spec) y-spec (first y-spec))))
    `(defmethod ,op (,x-spec ,y-spec)
      (let ((domain (domain-of ,x)))
        (cond ((eql domain (domain-of ,y))
               ,@body)
              (t (call-next-method)))))))

;; These are often of use when defining generic operations for domains.

(defvar *domain* ()
  "Within the context of an operation, the current domain")

(defgeneric %bind-dynamic-domain-context (domain function)
  (:documentation
   "The purpose of this method is not known.")
  (:method ((domain domain) function)
    (let ((*domain* domain))
      (%funcall function))))

(defmacro bind-domain-context (domain &body body)
  `(%bind-dynamic-domain-context ,domain 
    (lambda ()
      #+Genera (declare (sys:downward-function))
      ,@body)))
