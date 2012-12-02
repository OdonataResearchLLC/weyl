;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			           Sets 
;;; ===========================================================================
;;; (c) Copyright 1989, 1991 Cornell University

;;; sets.lisp,v 1.6 1995/05/24 17:42:11 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.6")

;; Tuples are just indexed lists.
(defclass tuple ()
  ((value
    :initarg :values
    :initform ()
    :reader value-of)))

;;; DELETE : This is a major update, so we're not concerned with
;;; backward compatibility.
(defgeneric tuple-value (tuple)
  (:documentation
   "A wrapper function for value-of for backward compatibility.")
  (:method ((tuple tuple)) (value-of tuple)))

(defmethod initialize-instance :after ((object tuple) &rest plist)
  (declare (ignore plist))
  (with-slots (value) object
    (cond ((null value)
	   (error "Values need to be provided for ~A"
		  (class-name (class-of object))))
	  ((listp value)
	   (setq value (%apply #'vector value))))))
	      
(defmethod print-object ((tuple tuple) stream)
  (%apply #'format stream "<~S~@{, ~S~}>"
          (loop with v = (value-of tuple)
                for i below (array-dimension v 0)
                collect (aref v i))))

(defgeneric ref (tuple &rest args)
  (:documentation
   "Refernce the item of tuple specified by the first arg.")
  (:method ((tuple tuple) &rest args)
    (aref (value-of tuple) (first args))))

;;; FIXME : Merge set-ref and (defsetf ref).
(defgeneric set-ref (tuple new-value &rest args)
  (:documentation
   "Set the element specified by args to new-value."))

(defmethod set-ref ((tuple tuple) new-value &rest args)
  (setf (aref (value-of tuple) (first args)) new-value))

(defsetf ref (vect &rest indices) (new-value)
  `(set-ref ,vect ,new-value ,@indices))

(defgeneric list-of-elements (tuple)
  (:documentation
   "Return a list of the tuple elements."))

(defmethod list-of-elements ((tuple tuple))
  (let ((array (value-of tuple)))
    (loop for i fixnum below (array-dimension array 0)
	  collect (aref array i))))

(defmethod map (type function (seq tuple) &rest sequences)
  (setq type (cond ((null type) (class-of seq))
		   ((typep type 'class) type)
		   ((typep type 'symbol) (find-class type))
		   (t (error "Bad type specifier: ~S" type))))
  (let ((values (loop with v = (value-of seq)
		      for i below (array-dimension v 0)
		      collect (%apply function (aref v i)
				      (loop for seq in sequences
					    collect (ref seq i))))))
    (if (subclass-of? 'domain-element type)
        (if (typep seq 'domain-element)
            (make-instance type :domain (domain-of seq)
                           :values values)
            (error "Can't determine domain of resulting sequence"))
        (make-instance type :values values))))

(defgeneric map-with-domain (type domain function sequence &rest sequences)
  (:documentation
   "Map the values of the sequences into the domain."))

(defmethod map-with-domain (type domain function (seq tuple) &rest sequences)
  (make-instance
   type :domain domain
   :values
   (loop with v = (value-of seq)
         for i below (array-dimension v 0)
         collect (%apply function (aref v i)
                         (loop for seq in sequences
                               collect (ref seq i))))))

;; (empty? set)
;; (insert key set &rest args)
;; (delete item set &rest args)
;; (member item set &rest args) 
;; (map-over-elements set function)
;; (make-generator set) -> function

(define-operations set
  (= (element self) (element self)) -> Boolean
  (coerce default self) -> (element self)
  (member (element self) self) -> Boolean
  (make-generator self) -> (-> (element self))
  (print-object (element self) stream) -> Null
  (number-of-elements self) -> integer)

;; Default version of this...
#+ignore
(defmethod binary= (x y) (equal x y))

(define-operations mutable-set
  (insert (element self) self) -> Null
  (delete (element self) self) -> Null)

#+IGNORE
(define-operations ordered-set
  (< (element self) (element self)) -> Boolean
  (> (element self) (element self)) -> Boolean
  (max (element self) (element self)) -> (element self)
  (min (element self) (element self)) -> (element self))

#+IGNORE
(defmethod initialize-method :after ((set ordered-set) &rest plist)
  (unless (%getf plist :compare-function)
    (error "Must provide a comparison function for ordered sets")))

(define-operations finite-set
  (size self) -> Integer
  (random self) -> (element self))

;; Set elements are also objects in Weyl.  They behave like
;; domain-elements (they are domain-elements).  The function
;; (element-key ..) gets their key.  They are two basic types of
;; set-elements.  SET-ELEMENT1 is a class where the elements are the
;; keys themselves.  There are many applications where we want to have
;; sets of pairs (key, value).  The class SET-ELEMENT2 is used for this
;; purpose.  Set-elements can be compared with =, and > in which case
;; the comparison will use the comparision function of the set.

;;When building more complex structures, (AVL trees etc.) the nodes of
;;the datastructures should be built out of these classes.

(defmethod print-object ((element set-element) stream)
  (format stream "~S" (element-key element)))

;; This is used for sets whose elements are associated with a value.

(defmethod print-object ((element set-element2) stream)
  (format stream "(~S, ~S)" (element-key element) (element-value element)))

(defmethod-sd binary= ((e1 set-element) (e2 set-element))
  (%funcall (equal-function-of domain) (element-key e1) (element-key e2)))

(defmethod binary= ((e1 set-element) e2)
  (%funcall (equal-function-of (domain-of e1)) (element-key e1) e2))

;; The following needs to be an around method so that it doesn't come
;; at the end of the precidence list (after (number domain-element)
;; defined in morphisms).
(defmethod binary= :around (e1 (e2 set-element))
  (%funcall (equal-function-of (domain-of e2)) e1 (element-key e2)))

(defmethod-sd binary> ((e1 set-element) (e2 set-element))
  (%funcall (greater-function-of domain) (element-key e1) (element-key e2)))

(defmethod binary> ((e1 set-element) e2)
  (%funcall (greater-function-of (domain-of e1)) (element-key e1) e2))

;; The following needs to be an around method so that it doesn't come
;; at the end of the precidence list (after (number domain-element)
;; defined in morphisms).
(defmethod binary> :around (e1 (e2 set-element))
  (%funcall (greater-function-of (domain-of e2)) e1 (element-key e2)))

;; In building real sets one should include one of these classes to
;; indicate how elements of the set will be represented.

(defmethod make-element ((set set-elements-as-singletons) key &rest rest)
  (declare (ignore rest))
  (make-instance 'set-element1 :domain set :key key))

(defmethod make-element ((set set-elements-as-pairs) key &rest rest)
  (make-instance 'set-element2 :domain set
		 :key key :value (first rest)))

;; Here are some simple sets that we might use in a program.

(defgeneric set-elements (set)
  (:documentation
   "The purpose of this method is unknown.")
  (:method ((set set-with-element-list))
    (rest (set-element-list set))))

(defun set-with-element-list-print-object (set stream)
  (let ((elts (set-elements set)))
    (if (null elts) (princ "{}" stream)
	(format stream "{~S~{, ~S~}}" (first elts) (rest elts)))))

(defmethod initialize-instance :after ((set set-with-element-list) &rest plist)
  (let ((initial-elements (loop for (item . args) in (%getf plist :initial-elements)
				collect (%apply #'make-element set item args))))
    #+IGNORE
    (if (typep set 'ordered-set)
	(setf initial-elements (sort initial-elements #'binary>)))
    (setf (rest (set-element-list set)) initial-elements)
    (with-slots (print-function) set
      (setf print-function 'set-with-element-list-print-object))))

(defmethod insert (key (set mutable-set-with-element-list) &rest rest)
  (let ((list (set-element-list set)))
    (loop for elt in (rest list)
	  when (= key elt)
          do (return set)
	  finally (push (%apply #'make-element set key rest) (rest list))
          (return set))))

(defmethod delete (item (set mutable-set-with-element-list) &rest args)
  (declare (ignore args))
  (flet ((not-an-element ()
	   (error "~S is not an element of ~S" item set)))
    (loop for elts on (set-element-list set)
          when (null (rest elts))
          do (not-an-element)
          when (= item (second elts))
          do (setf (rest elts) (rest (rest elts)))
          (return set)
          finally (not-an-element))))

(defmethod member (key (set set-with-element-list) &rest args)
  (declare (ignore args))
  (loop for elt in (set-elements set)
	when (= key elt)
        do (return elt)
	finally (return nil)))

(defgeneric map-over-elements (set function)
  (:documentation
   "Map over the elements of the set applying the function."))

(defmethod map-over-elements ((set set-with-element-list) function)
  (loop for elt in (set-elements set) do
        (%funcall function elt)))

(defmethod make-generator ((set set-with-element-list))
  (let ((list (set-elements set)))
    (lambda () (pop list))))

;; This is just a variant on the previous class.  The inclusion of the
;; ordered-set class causes the initialize-instance method to put the
;; elements in the set ordered.

(defmethod insert (key (set mutable-set-with-sorted-element-list) &rest rest)
  (loop for elts on (set-element-list set) do
        (cond ((or (null (rest elts)) (> key (second elts)))
               (setf (rest elts) (cons (%apply #'make-element set key rest) (rest elts)))
               (return set))
              ((= key (second elts))
               (return set)))))

(defmethod delete (item (set mutable-set-with-sorted-element-list) &rest args)
  (declare (ignore args))
  (flet ((not-an-element ()
	   (error "~S is not an element of ~S" item set)))
    (loop for elts on (set-element-list set)
          when (null (rest elts))
          do (not-an-element)
          when (= item (second elts))
          do (setf (rest elts) (rest (rest elts)))
          (return set)
          when (> item (second elts))
          do (not-an-element)
          finally (not-an-element))))

(defmethod member (key (set set-with-sorted-element-list) &rest args)
  (declare (ignore args))
  (loop for elt in (set-elements set)
	when (= key elt)
        do (return elt)
	when (> key elt)
        do (return nil)
	finally (return nil)))

;;; FIXME : It would be better to define this as length.
(defgeneric size (set)
  (:documentation
   "Return the length of the set.")
  (:method ((set set-with-element-list))
    (common-lisp:length (set-elements set))))

(defgeneric random (set &optional height)
  (:documentation
   "Return a random element of the list."))

(defmethod random ((set set-with-element-list) &optional height)
  (declare (ignore height))
  (let ((l (set-elements set)))
    (nth (cl:random (length l)) l)))
