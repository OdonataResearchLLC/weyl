;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			  Algebraic Domains
;;; ===========================================================================
;;; (c) Copyright 1994 Cornell University
;;; new-domains.lisp,v 1.2 1995/05/24 17:42:06 rz Exp

;;; NB: initialization code moved to seperate file new-domains-init.lisp.
;;; This allows seperate compilation of files.

(in-package "USER")

(defconstant *math-operator-properties*
    '(identity unary-inverse binary-inverse))

;;; Operators
(defclass math-operator ()
     ((name :initarg :name :accessor name-of)
      (nargs :initarg :nargs :accessor nargs-of)
      (identity :initarg :identity :accessor %identity-of)
      (unary-inverse :initarg :unary-inverse :accessor %unary-inverse-of)
      (binary-inverse :initarg :binary-inverse :accessor %binary-inverse-of)))

(defmethod print-object ((op math-operator) stream)
  (format stream "#<Op: ~A (~D)>" (name-of op) (nargs-of op)))

(defvar *math-operators* (make-hash-table)
  "Table of all know mathematical operators")

(defmacro math-operator (name)
  `(gethash ,name *math-operators*))

(defmacro define-math-operator-accessors ()
  (flet ((internal-name (op)
	   (intern (format nil "%~A-OF" op)))
	 (external-name (op)
	   (intern (format nil "~A-OF" op))))
    `(progn
       ,@(loop for op in *math-operator-properties*
	       append
	       `((defmacro ,(external-name op) (obj)
		   `(name-of (,',(internal-name op) ,obj)))
		 (defmethod ,(internal-name op) ((obj symbol))
		   (when (setq obj (math-operator obj))
		     (,(internal-name op) obj))))))))

(define-math-operator-accessors)
	

(defmacro define-math-operator (name (nargs) &rest properties)
  `(let ((operator (make-instance 'math-operator :name ',name :nargs ,nargs))
	 temp1 temp2)

     ,@(loop for op in *math-operator-properties*
	     collect
	     `(when (setq temp1 ',(getf properties
					(intern (symbol-name op) "KEYWORD")))
		 (unless (setq temp2 (math-operator temp1))
		   (error "The ~S operator must be defined before it is used."
			  temp1))
		 (setf (,(intern (format nil "%~A-OF" op)) operator) temp2)))

     (setf (math-operator ',name) operator)
     ',name))


;; The PRETTY-NAME slot of a domain will be used by the print-object
;; method.  The CREATOR and CREATOR-ARGS slots of a domain are used
;; when the domain is creator from other domains.  There is no
;; overwhelming reason why these should have been split into two
;; slots, but I suspect that we may want to do some optimizations on
;; the CREATOR slot at some point in the future.

; For testing purposes, remove dependence on anything in WEYL.
;(defclass domain (weyli::has-property-list)

(defclass domain ()
     ((pretty-name :initform nil
		   :initarg :pretty-name
		   :accessor pretty-name-of)
      (creator :initform :primitive
	       :initarg :creator
	       :accessor creator-of)
      (creator-args :initform nil
		    :initarg :creator-args
		    :accessor creator-args-of)))

(defmethod print-object ((obj domain) stream)
  (if (pretty-name-of obj)
      (princ (pretty-name-of obj) stream)
      (format stream "#<Domain ~A>" (creator-of obj))))

;;; FIXME : This is an alternate definition from that in
;;; domain-support.lisp.
(defmacro define-domain-creator (name (domain . args) &body body)
  (let ((create-function (intern (format nil "MAKE-~A" name)))
	(predicate (intern (format nil "~A?" name)))
	arguments)
    (loop for arg in args
	  do (cond ((member arg '(&optional &key)))
		   ((atom arg) (push arg arguments))
		   (t (push (first arg) arguments))))
    (setq arguments (nreverse arguments))
     
    `(progn
       (defun ,create-function ,args
         (let ((,domain
		  (make-instance 'domain
		     :creator ',name
		     :creator-args (list ,@arguments))))
	   ,@body
	   ,domain))
       (defun ,predicate (domain)
	 (eql (creator-of domain) ',name))
       ',name)))


(define-domain-creator rational-integers (domain)
  (setf (pretty-name-of domain) "Z"))





;; This is the basic table of all properties of domains.  It is
;; indexed by property name.  (It could be made into a simple array,
;; but a hash table gives us a bit more flexibility now.)
(defvar *domain-property-table* (make-hash-table))

(defun compare-pterm-lists (term1 term2)
  (loop for a in term1
	for b in term2
	do (unless (eql a b)  ;; FIXTHIS: This predicate may need to be improved!
	     (return nil))
	finally (return t)))

(defun assert-property (property terms)
  (let ((table (gethash property *domain-property-table*)))
    (loop for prop in table
	  do (when (compare-pterm-lists prop terms)
	       (return t))
	     finally (setf (gethash property *domain-property-table*)
			   (cons terms table)))))

(defun test-property (property terms)
  (let ((table (gethash property *domain-property-table*)))
    (loop for prop in table
	  do (when (compare-pterm-lists prop terms)
	       (return prop)))))

;; This version of delete expects an exact match before deleteing a
;; property.  This is probably OK, buts its worth thinking about.
(defun delete-property (property terms)
  (setf (gethash property *domain-property-table*)
	(delete terms (gethash property *domain-property-table*))))

;; The function will be passed two arguments, the name of the property
;; and the list terms of the property.
(defmethod %map-over-properties ((domain domain) function)
  (maphash #'(lambda (key value)
	       (loop for term in value
		     do (when (eql (car term) domain)
			  (funcall function key term)
			  (return t))))
	   *domain-property-table*))

(defmacro map-over-properties (domain (property terms) &body body)
  `(%map-over-properties ,domain #'(lambda (,property ,terms) ,@body)))

(defmethod show-properties
    ((domain domain) &optional (stream *standard-output*))
  (map-over-properties domain (prop val)
      (print (list prop '= val) stream)))

;; NEEDS FIXING:
;; Bad idea to destructively modify a data structure while 
;; mapping over it.
(defmethod delete-domain ((domain domain))
  (map-over-properties domain (property terms)
    (delete-property property terms)))

(defvar *math-properties* nil)
(defvar *primitive-properties* nil)

(defmacro define-primitive-property (property-name args)
  (when (member property-name *math-properties*)
    (error "~A has already been used as a non-primitive property!"
	   property-name))
  (let ((assert-function-name (intern (format nil "~A!" property-name)))
	(predicate-function-name (intern (format nil "~A?" property-name)))
	req-args opt-args)
    (labels ((make-predicate (args body)
	       (if (null args) body
		   `(if (null ,(first args)) ,body
			,(make-predicate (rest args)
					 (append body (list (first args))))))))
      (loop for prop in args
	    with req = t
	    do (cond ((eql prop '&optional)
		      (setq req nil))
		     ((null req)
		      (if (atom prop)
			  (push (list prop :true) opt-args)
			  (push prop opt-args)))
		     (t (push prop req-args))))
      (setq req-args (reverse req-args))
      (setq opt-args (reverse opt-args))
     
      `(progn
	 (defun ,assert-function-name (,@req-args
					 ,@(when opt-args
					     '(&optional))
					 ,@opt-args)
	   (assert-property ',property-name
			    (list ,@req-args
				  ,@(loop for arg in opt-args
					  collect (first arg)))))
	 (defun ,predicate-function-name (,@req-args
					    ,@(when opt-args
						'(&optional))
					    ,@(loop for arg in opt-args
						    collect (first arg)))
	   (test-property ',property-name
			  ,(make-predicate (mapcar #'first opt-args)
					   `(list . ,req-args))))
	 (pushnew ',property-name *primitive-properties*)

	 ',property-name))))

(defun assert-function-name (name)
  (unless (or (member name *primitive-properties*)
	      (member name *math-properties*))
    (error "The property ~S is not yet defined"
	   name))
  (intern (format nil "~A!" name)))

(defun predicate-function-name (name)
  (unless (or (member name *primitive-properties*)
	      (member name *math-properties*))
    (error "The property ~S is not yet defined"
	   name))
  (intern (format nil "~A?" name)))

;; The predicate is a form that must evaluate to true to assert this
;; property.
(defmacro define-math-property (property-name args &body body)
  (when (member property-name *primitive-properties*)
    (error "~A has already been used as a primitive property!"
	   property-name))
  (when (null body)
    (error "~S should probably be a primitive property" property-name))
  (let ((assert-name (intern (format nil "~A!" property-name)))
	(predicate-name (intern (format nil "~A?" property-name)))
	opt-args req-args)
    (loop for prop in args
	    with req = t
	    do (cond ((eql prop '&optional)
		      (setq req nil))
		     ((null req)
		      (if (atom prop)
			  (push (list prop :true) opt-args)
			  (push prop opt-args)))
		     (t (push prop req-args))))
    (setq req-args (reverse req-args))
    (setq opt-args (reverse opt-args))
    
      `(progn
	 (defun ,assert-name (,@req-args
					 ,@(when opt-args
					     '(&optional))
					 ,@opt-args)
	   ,@(loop for property in body
		   collect `(,(assert-function-name (first property))
				     ,@(rest property))))

	 (defun ,predicate-name (,@req-args
					    ,@(when opt-args
						'(&optional))
					    ,@(loop for arg in opt-args
						    collect (first arg)))
	   (and
	     ,@(loop for property in body
		     collect `(,(predicate-function-name (first property))
			       ,@(rest property)))))

	 (pushnew ',property-name *math-properties*)
	 ',property-name)))
