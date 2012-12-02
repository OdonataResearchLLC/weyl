;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;				  Morphisms
;;; ===========================================================================
;;; (c) Copyright 1989, 1991 Cornell University

;;; morphisms.lisp,v 1.7 1994/10/21 18:16:43 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.7")

(defmethod print-object ((homo morphism) stream)
  (format stream
	  #+Genera "~S~S"
	  #-Genera "~S->~S"
	  (morphism-domain homo) (morphism-range homo)))

(defvar *morphism-composition-table*
    (make-hash-table))

(defgeneric compose (morphism1 morphism2)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod compose ((f homomorphism) (g homomorphism))
  (let ((cache (gethash f *morphism-composition-table*))
	comp)
    (cond ((setq comp (assoc g cache))
	   (second comp))
	  ((eql (morphism-range f) (morphism-domain g))
	   (setq comp
		 (make-instance
                  'homomorphism
                  :domain (morphism-domain f)
                  :map (lambda (x)
                         (%funcall (morphism-map g)
                                   (%funcall (morphism-map f) x)))
                  :range (morphism-range g)))
	   (push (list g comp) (gethash f *morphism-composition-table*))
	   comp)
	  (t (error "Incompatible homomorphisms: ~S o ~S"
		    f g)))))

(defun get-morphisms (&key type domain range direct?)
  (let (morphisms)
    (labels ((get-morphisms-from (d predecessor)
	       (loop for morph in (domain-morphisms-from d) do
                     (when (or (null type) (typep morph type))
                       (cond ((eql range (morphism-range morph))
                              (push (if predecessor
                                        (compose predecessor morph)
                                        morph)
                                    morphisms))
                             (t
                              (setq morph (if predecessor
                                              (compose predecessor morph)
                                              morph))
                              (when (null range)
                                (push morph morphisms))
                              (when (null direct?)
                                (get-morphisms-from (morphism-range morph)
                                                    morph)))))))
	     (get-morphisms-to (d successor)
	       ;; We know that the range is null here, otherwise we
	       ;; would have called GET-MORPHISMS-FROM
	       (loop for morph in (domain-morphisms-to d) do
                     (when (or (null type) (typep morph type))
                       (setq morph (if successor
                                       (compose morph successor)
                                       morph))
                       (push morph morphisms)
                       (when (null direct?)
                         (get-morphisms-to (morphism-domain morph) morph))))))
      (cond (domain
	     (get-morphisms-from domain nil))
	    (range
	     (get-morphisms-to range nil))
	    (t (loop for d in *domains*
		     do (get-morphisms-to d nil))))
      morphisms)))

(defun make-morphism (domain map range &key (replace? t))
  (let ((old-h (get-morphisms :domain domain :range range))
	(h (make-instance 'morphism :domain domain :map map :range range)))
    (when replace?
      (loop for morph in old-h
	    do (delete-morphism morph :error? t)))
    (push h (domain-morphisms-from domain))    
    (push h (domain-morphisms-to range))
    h))

(defgeneric delete-morphism (morphism)
  (:documentation
   "Delete the morphism."))

(defmethod delete-morphism ((morph morphism))
  (let ((domain (morphism-domain morph))
	(range (morphism-range morph)))
    (setf (domain-morphisms-from domain)
	  (delete morph (domain-morphisms-from domain)))    
    (setf (domain-morphisms-to range)
	  (delete morph (domain-morphisms-to range)))))

;;; Homomorphisms

(defun make-homomorphism (domain map range)
  (let ((h (make-instance 'homomorphism :domain domain :map map :range range)))
    (push h (domain-morphisms-from domain))
    (push h (domain-morphisms-to range))
    h))

(defun get-homomorphisms (&key domain range)
  (get-morphisms :type 'homomorphism :domain domain :range range))

(defun get-embeddable-domains (domain)
  (let ((domains ()))
    (loop for homo in (domain-morphisms-from domain)
	  do (when (typep homo 'homomorphism)
	       (push (morphism-range homo) domains)))
    domains))

(defun make-automorphism (domain map &optional range)
  (declare (ignore range))
  (make-homomorphism domain map domain))

(defun get-automorphisms (&key domain)
  (get-morphisms :type 'automorphism :domain domain :range domain))

;;; Operations with morphisms
(defgeneric apply-morphism (morphism argument)
  (:documentation
   "Apply the morphism to the argument."))

(defmethod apply-morphism ((h morphism) argument)
  (when (eql (domain-of argument) (morphism-domain h))
    (%funcall (morphism-map h) argument)))

(defgeneric canonicalize-number (elt)
  (:documentation
   "Converts a LISP number into a a Weyl number in the appropriate
canonical domain. These guys CANNOT use coerce!!!!"))

(defmethod canonicalize-number ((num integer))
  (make-element (get-rational-integers) num))

(defmethod canonicalize-number ((num rational-integer))
  (cond ((typep (domain-of num) 'general-expressions)
	 (make-element (get-rational-integers) (integer-value num)))
	(t num)))

(defmethod canonicalize-number ((num ratio))
  (make-instance 'rational-number :domain (get-rational-numbers)
		 :numerator (cl:numerator num)
		 :denominator (cl:denominator num)))

(defmethod canonicalize-number ((num rational-number))
  (cond ((typep (domain-of num) 'general-expressions)
	 (make-instance 'rational-number :domain (get-rational-numbers)
			:numerator (qo-numerator num)
			:denominator (qo-denominator num)))
	(t num)))

(defmethod canonicalize-number ((num float))
  (make-instance 'floating-point-number
		 :domain (get-real-numbers)
		 :value num))

(defmethod canonicalize-number ((num floating-point-number))
  (cond ((typep (domain-of num) 'general-expressions)
	 (make-instance 'floating-point-number
			:domain (get-real-numbers)
			:value (fp-value num)))
	(t num)))

(defmethod canonicalize-number ((num bigfloat))
  (cond ((typep (domain-of num) 'general-expressions)
	 (make-bigfloat (get-real-numbers)
			(bigfloat-mantissa num)
			(bigfloat-exponent num)))
	(t num)))

(defmethod canonicalize-number ((num cl:complex))
  (make-element (get-complex-numbers)
		(cl:realpart num)
		(cl:imagpart num)))

(defmethod canonicalize-number ((num complex-number))
  (cond ((typep (domain-of num) 'general-expressions)
	 (make-element (get-complex-numbers)
		       (cn-realpart num)
		       (cn-imagpart num)))
	(t num)))

(defmethod canonicalize-number ((num GFp-element)) num)

(defmethod canonicalize-number ((num GFm-element)) num)

;;; This is what allows lisp numbers to be used relatively freely.
;;; Unfortunately, this introduces more consing than would be idea.
(defmethod apply-morphism ((map morphism) (elt number))
  (apply-morphism map (canonicalize-number elt)))

;;; Support for COERCIONS

(defun reachable-domains (domain)
  (flet ((next-domains (x)
	   (loop for m in (domain-morphisms-from x)
		 collect (morphism-range m))))
    (let (domains)
      (map-over-tree d (domain :depth-first? t
                               :collection-fun next-domains)
        (push d domains))
      domains)))

(defun find-common-domains (d1 d2)
  (flet ((next-domains (x)
	   (loop for m in (domain-morphisms-from x)
		 collect (morphism-range m))))
    (let ((d1-domains (reachable-domains d1))
	  domains)
      (map-over-tree d (d2 :breadth-first? t
			   :collection-fun next-domains)
	(when (member d d1-domains)
	  (pushnew d domains)
	  (terminate-branch)))
      domains)))

;;; FIXME : Common Lisp does have an error system, now.
;;;
;;; Since we don't have an error system in Common Lisp yet, we use the
;;; following flag to control whether an error is generated or NIL is
;;; returned from COERCE.

(defvar *coercibility-checking* nil)

;;; This method provides the the default homorphism coercions
(defmethod coerce (elt (domain domain))
  (let (homos)
    (cond ((null (typep elt 'domain-element))
	   (unless *coercibility-checking*
	     (error "Don't know how to coerce ~S to be an element of ~S"
		    elt domain)))
	  ((eql (domain-of elt) domain)
	   elt)
	  ((null (setq homos (get-homomorphisms :domain (domain-of elt)
						:range domain)))
	   (unless *coercibility-checking*
	     (error "Don't know how to coerce ~S to be an element of ~S"
		    elt domain)))
	  ((null (rest homos))
	   (apply-morphism (first homos) elt))
	  (t (error "More than one homomorphism from ~S to ~S.~%~
                     Can't do automatic coercion"
		    (domain-of elt) domain)))))

;;; This method must be primary because there are more specific
;;; versions of it.
(defmethod coerce ((elt number) (domain domain))
  (coerce (canonicalize-number elt) domain))

;; FIXTHIS: Why is this here?  Both branches are the same.
(defmethod coerce ((elt numeric) (domain domain))
  (cond ((and (typep (domain-of elt) 'general-expressions)
	      (not (typep domain 'general-expressions)))
	 (call-next-method (canonicalize-number elt) domain))
	(t (call-next-method (canonicalize-number elt) domain))))

(defmethod coercible? (elt (d domain))
  (let ((*coercibility-checking* t))
    (coerce elt d)))

;;; This code provides the default methods for binary operators.

(defvar *coerce-where-possible* t)

(defmacro def-binary-coercion (op illegal-mess ambig-mess
                               &key (numeric-numeric? t)
                               (domain-element-domain-element? t))
  `(progn
    ,(when domain-element-domain-element?
           `(defmethod ,op ((x domain-element) (y domain-element))
             (when (null *coerce-where-possible*)
               (error ,illegal-mess x y))
             (let ((domain-x (domain-of x))
                   (domain-y (domain-of y))
                   common-domains)
               (when (eql domain-x domain-y)
                 (error ,illegal-mess x y))
               (setq common-domains (find-common-domains domain-x domain-y))
               (cond ((null common-domains)
                      (error ,illegal-mess x y))
                     ((null (rest common-domains))
                      (,op (coerce x (first common-domains))
                           (coerce y (first common-domains))))
                     (t (error ,ambig-mess  x y))))))
    ;; If the domain of y is non-strict then always try to coerce x
    ;; into (domain-of y).  If the domain of y is strict, then
    ;; canoncalize x, ie, coerce x into its natural algebraic domain
    ;; R, Z, C, etc, and try again.
    (defmethod ,op ((x number) (y domain-element))
      (cond ((typep (domain-of y) 'non-strict-domain)
             (,op (coerce x (domain-of y)) y))
            (*coerce-where-possible*	       
             (,op (canonicalize-number x) y))
            (t (error ,illegal-mess x y))))
    (defmethod ,op ((x domain-element) (y number))
      (cond ((typep (domain-of x) 'non-strict-domain)
             (,op x (coerce y (domain-of x))))
            (*coerce-where-possible*	       
             (,op x (canonicalize-number y)))
            (t (error ,illegal-mess x y))))
    ;; If the domain of y is non-strict then proceed as with numbers
    ;; (always coerce).  If the domain of y is strict, but the domain
    ;; of x is not, then coerce x into its natural algebraic domain
    ;; and try again.
    (defmethod ,op ((x numeric) (y domain-element))
      (cond ((eql (domain-of x) (domain-of y))
             (call-next-method))
            ((typep (domain-of y) 'non-strict-domain)
             (,op (coerce x (domain-of y)) y))
            ((and *coerce-where-possible*
                  (typep (domain-of x) 'non-strict-domain))
             (,op (canonicalize-number x) y))
            (t (call-next-method))))
    (defmethod ,op ((x domain-element) (y numeric))
      (cond ((eql (domain-of x) (domain-of y))
             (call-next-method))
            ((typep (domain-of x) 'non-strict-domain)
             (,op x (coerce y (domain-of x))))
            ((and *coerce-where-possible*
                  (typep (domain-of y) 'non-strict-domain))
             (,op x (canonicalize-number y)))
            (t (call-next-method))))
    ,(when numeric-numeric?
           `(defmethod ,op ((x numeric) (y numeric))
             (let ((x-domain (domain-of x))
                   (y-domain (domain-of y)))
               (cond ((eql x-domain y-domain)
                      (call-next-method))
                     ((typep (domain-of x) 'non-strict-domain)
                      (,op x (coerce y (domain-of x))))
                     ((typep (domain-of y) 'non-strict-domain)
                      (,op (coerce x (domain-of y)) y))
                     (t (call-next-method))))))))

(def-binary-coercion binary=
    "No way to compare ~S and ~S"
  "Ambiguous coercion for = (~S, ~S)"
  :numeric-numeric? nil)

(def-binary-coercion binary>
    "No way to compare ~S and ~S"
  "Ambiguous coercion for > (~S, ~S)"
  :numeric-numeric? nil)

(def-binary-coercion binary>=
    "No way to compare ~S and ~S"
  "Ambiguous coercion for >= (~S, ~S)"
  :numeric-numeric? nil)

(def-binary-coercion plus
    "No way to add ~S and ~S"
  "Ambiguous coercion for addition (~S, ~S)"
  :numeric-numeric? nil)

(def-binary-coercion difference
    "No way to subtract ~S and ~S"
  "Ambiguous coercion for subtraction (~S, ~S)"
  :numeric-numeric? nil)

(def-binary-coercion times
    "No way to multiply ~S and ~S"
  "Ambiguous coercion for multiplication (~S, ~S)"
  :numeric-numeric? nil)

;;; FIXME : Explicitly define a generic function for quotient.
(def-binary-coercion quotient
    "No way to compute the quotient of ~S and ~S"
  "Ambiguous coercion for division (~S, ~S)"
  :numeric-numeric? nil)

(def-binary-coercion expt
  "No way to raise ~S to the ~S power"
  "Ambiguous coercion for exponentiation (~S, ~S)")

;;; FIXME : Explicitly define a generic function for remainder.
(def-binary-coercion remainder
  "No way to compute the remainder of ~S and ~S"
  "Ambiguous coercion for remainder (~S, ~S)"
  :numeric-numeric? nil)

;; The numeric-numeric method for GCD and LCM are in numbers.lisp
(def-binary-coercion binary-gcd
    "No way to compute the GCD of ~S and ~S"
  "Ambiguous coercion for gcd (~S, ~S)"
  :numeric-numeric? nil)

(def-binary-coercion binary-lcm
    "No way to compute the LCM of ~S and ~S"
  "Ambiguous coercion for lcm (~S, ~S)"
  :numeric-numeric? nil)

;;; FIXME : Explicitly define a generic function for max-pair.
(def-binary-coercion max-pair
    "No way to compute the maximum of ~S and ~S"
  "Ambiguous coercion for max (~S, ~S)"
  :domain-element-domain-element? nil
  :numeric-numeric? nil)

;;; FIXME : Explicitly define a generic function for min-pair.
(def-binary-coercion min-pair
    "No way to compute the minimum of ~S and ~S"
  "Ambiguous coercion for min (~S, ~S)"
  :domain-element-domain-element? nil
  :numeric-numeric? nil)

;;; FIXME : Explicitly define a generic function for floor2.
(def-binary-coercion floor2
    "No way to compute the floor of ~S modulo ~S"
  "Ambiguous coercion for floor (~S, ~S)"
  :numeric-numeric? nil)

;;; FIXME : Explicitly define a generic function for ceiling2.
(def-binary-coercion ceiling2
    "No way to compute the ceiling of ~S modulo ~S"
  "Ambiguous coercion for ceiling (~S, ~S)"
  :numeric-numeric? nil)

;;; FIXME : Explicitly define a generic function for round2.
(def-binary-coercion round2
    "No way to round ~S modulo ~S"
  "Ambiguous coercion for round (~S, ~S)"
  :numeric-numeric? nil)

;;; FIXME : Explicitly define a generic function for truncate2.
(def-binary-coercion truncate2
    "No way to truncate ~S modulo ~S"
  "Ambiguous coercion for truncate (~S, ~S)"
  :numeric-numeric? nil)

;;; FIXME : Explicitly define a generic function for dot-product.
(def-binary-coercion dot-product
    "No way to compute the dot-product of ~S and ~S"
  "Ambiguous coercion for dot-product (~S, ~S)")
