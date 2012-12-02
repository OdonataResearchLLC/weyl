;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			  Algebraic Domains
;;; ===========================================================================
;;; (c) Copyright 1989, 1994 Cornell University

;;; algebraic-domains.lisp,v 1.18 1995/05/24 17:41:57 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.18")

(defclass set (domain)
  ()
  (:documentation
   "A class for finite, unordered sets"))

;;; Some objects and/or domains have names. If so the following class
;;; should always be included.

(defclass has-name ()
  ((name :initarg :name :accessor name-of))
  (:documentation
   "Include this class in objects and/or domains that have names."))

(defgeneric binary= (x y)
  (:documentation
   "Elements of a set are assumed to be comparable using binary=. For
more complex set structures, the binary= operation looks up the
comparison function in EQUAL-FUNCTION slot."))

;; Ordered set's presume the existence of a > predicate.  We provide a
;; default for >= but that is likely to also be provided primitively.
;; < and <= are handled via macros since they just involve changing
;; the order of the arguments.

(defgeneric binary> (x y)
  (:documentation
   "Elements of a set are assumed to be comparable using binary>. For
more complex set structures, the binary> operation looks up the
comparison function in greater-funcion slot."))

(defgeneric binary>= (x y)
  (:method (x y)
    (or (binary> x y) (binary= x y))))

(defsubst binary< (x y) (binary> y x))

(defsubst binary<= (x y) (binary>= y x))

(defgeneric make-element (domain obj &rest rest)
  (:documentation
   "The purpose of this method is unknown."))

;;; Some data structure need to have ready access to equality and
;;; inequality comparison functions.  For instance, AVL trees, the
;;; exponent comparison function of expanded polynomials, etc.  The
;;; following classes provides a canonical place to cache this
;;; information

(defclass has-equality ()
  ((equal-function
    :initform #'binary=
    :initarg :equal-function
    :accessor equal-function-of))
  (:documentation
   "A canonical place to cache equality function information."))

(defclass has-comparison ()
  ((greater-function
    :initform #'binary>
    :initarg :greater-function
    :accessor greater-function-of))
  (:documentation
   "A cononical place to cache inequality function information."))

;;; Whether or not a set is ordered is a property, but elements of an
;;; ordered set can be compared using binary>.

;;; Properties indicate additional axioms that can be true about a
;;; domain. All the properties of an algebraic domain (e.g. field-ness)
;;; could treated as axioms, and in many ways this would be preferable,
;;; but I'm not quite willing to do this at this point.  --RZ

;;; When looking up a property, two values are returned: whether the
;;; property is true and whether we are certain of the result.

;;; Non strict domains!!  Non-Strict domains have all possible
;;; properties.  I.e. if it is syntactically possible to do an
;;; operation on the operands, then Weyl will go ahead and do it
;;; without checking any properties of the domain.

(defclass non-strict-domain ()
  ())

;;; FIXME : Move these to the top of the file.
(defvar *empty-property* (list nil))

(defvar *math-properties* ())

;;; FIXME : Add support for declarations and documentation.
;;; AUDIT : Needs a detailed audit.
(defmacro define-math-property (property-name subsumed-properties)
  (let ((predicate-name (intern (format nil "~A?" property-name)))
	(keyword (intern (symbol-name property-name)
			 (find-package 'keyword)))
	(assert-name (intern (format nil "ASSERT-~A" property-name))))
    `(progn
      (defgeneric ,predicate-name (domain))
      (defmethod ,predicate-name ((domain domain))
        (let ((val (getf domain ,keyword *empty-property*)))
          (if (eql val *empty-property*)
              (values nil nil)
              (values val t))))
      (defmethod ,predicate-name ((domain non-strict-domain))
        t)
      (defgeneric ,assert-name (domain))
      (defmethod ,assert-name ((domain domain))
        ,@(loop for prop in subsumed-properties
                collect `(,(intern (format nil "ASSERT-~A" prop)) domain))
        (setf (getf domain ,keyword) t))
      (pushnew ',property-name *math-properties*))))

(define-math-property ordered-domain ())

;; The following property is used to determine if floating point
;; numbers can be elements of the domain.

(define-math-property complete-set ())

;; Define-operations for sets is in sets.lisp

(defclass semigroup (set)
  ())

(defsubst semigroup? (domain) (typep domain 'semigroup))

(define-operations semigroup
  (times (element self) (element self)) -> (element self)
  (expt (element self) positive-integer) -> (element self))

(defgeneric times (x y))

(defgeneric expt (x y))

(defclass monoid (semigroup)
  ())

(defsubst monoid? (domain) (typep domain 'monoid))

(define-operations monoid
  (one self) -> (element monoid)
  (1? (element self)) -> Boolean
  (expt (element self) integer) -> (element self))

(defgeneric one (x))

(defmethod one ((domain domain))
  (coerce 1 domain))

(defgeneric 1? (x))

(defclass group (monoid)
  ())

(defsubst group? (domain) (typep domain 'group))

(define-operations group
  (recip (element self)) -> (element self)
  (expt (element self) Integer) -> (element self))

(defgeneric recip (x))

(defclass abelian-semigroup (set)
  ())

(defsubst abelian-semigroup? (domain) (typep domain 'abelian-semigroup))

(define-operations abelian-semigroup
  (plus (element self) (element self)) -> (element self)
  (times Integer (element self)) -> (element self))

(defgeneric plus (x y))

(defclass abelian-monoid (abelian-semigroup)
  ())

(defsubst abelian-monoid? (domain) (typep domain 'abelian-monoid))

(define-operations abelian-monoid
  (zero self) -> (element self)
  (0? (element self)) -> Boolean
  (times integer (element self)) -> (element self))

(defgeneric zero (domain))

(defmethod zero ((domain domain))
  (coerce 0 domain))

(defgeneric 0? (x))

(defclass abelian-group (abelian-monoid)
  ())

(defsubst abelian-group? (domain) (typep domain 'abelian-group))

(define-operations abelian-group
  (minus (element self)) -> (element self)
  (difference (element self) (element self)) -> (element self)
  (times integer (element self)) -> (element self))

(defgeneric minus (x))

(defgeneric difference (x y))

;; This is the mathematical definition of a RING.  It has just the
;; operations plus and times, and times distributes over plus.  In
;; most cases however we usually mean somewhat more.
(defclass rng (semigroup abelian-group)
  ())

#+IGNORE
(defaxiom rng ()
  (leftdistributive times plus)
  (rightDistributive times plus))

(defclass simple-ring (rng monoid)
  ())

(defsubst simple-ring? (domain) (typep domain 'simple-ring))

(define-operations simple-ring
  (characteristic self) -> Integer
  (one self) -> (element self)
  (1? (element self)) -> (element self)
  (recip (element self)) -> (element self))

(defclass has-coefficient-domain ()
  ((coefficient-domain
    :initform nil
    :initarg :coefficient-domain
    :reader coefficient-domain-of)))

(defvar *coefficient-domain* ()
  "Within the context of a polynomial operation, the coefficient
domain")

(defmethod %bind-dynamic-domain-context
    ((domain has-coefficient-domain) function)
  (let ((*coefficient-domain* (coefficient-domain-of domain)))
    (call-next-method)))

(defclass module (abelian-group has-coefficient-domain)
  ())

(defsubst module? (domain) (typep domain 'module))

(defgeneric characteristic (domain)
  (:documentation
   "The purpose of this method is not known."))

(defmethod characteristic ((domain module))
  (characteristic (coefficient-domain-of domain)))

;; The coefficient domain of an algebra should be a SIMPLE-RING 
(defclass algebra (module semigroup)
  ())

(defsubst algebra? (domain) (typep domain 'algebra))

(defclass ring (algebra simple-ring)
  ()
  ;; Also has the distributive law
  )

(defsubst ring? (domain) (typep domain 'ring))

(defmethod-sd max-pair ((x domain-element) (y domain-element))
  (if (> x y) x y))

(defmethod-sd min-pair ((x domain-element) (y domain-element))
  (if (> x y) y x))


(define-math-property integral-domain ())

;; A GCD can be defined
(define-math-property euclidean-domain (integral-domain))

;; A GCD algorithm exists
(define-math-property gcd-domain (euclidean-domain))

(defgeneric binary-gcd (x y))

(defgeneric binary-lcm (x y))

(define-math-property unique-factorization-domain (euclidean-domain))

;; Every field is an integral domain.
(defclass field (ring)
  ())

(defsubst field? (domain) (typep domain 'field))

(defmethod initialize-instance :after ((domain field) &rest plist)
  (declare (ignore plist))  
  (assert-integral-domain domain))

(define-operations field
  (quotient (element self) (element self)) -> (element self)
  (recip (element self)) -> (element self))

(defclass finite-field (field finite-set)
  ())

(defclass factor-domain (domain)
  ((factor-numer
    :initarg :numerator
    :accessor factor-numer-of)
   (factor-denom
    :initarg :denominator
    :accessor factor-denom-of)))

(defgeneric factor-domain-print-object (domain stream))

(defmethod factor-domain-print-object (domain stream)
  (format stream "~S/~S" (factor-numer-of domain) (factor-denom-of domain)))

(defmethod initialize-instance :after ((domain factor-domain) &rest plist)
  (declare (ignore plist))
  (with-slots (print-function) domain
    (setf print-function 'factor-domain-print-object)))

(defclass factor-group (factor-domain group)
  ())

(defclass factor-module (factor-domain module)
  ())

(defclass factor-ring (factor-domain ring)
  ())

(defclass algebraic-extension (ring)
  ())

(defclass simple-field-extension (algebraic-extension field)
  ())

;; A domain that has a dimension
(defclass dimensional-domain (domain)
  ((dimension
    :initform nil
    :initarg :dimension
    :reader dimension-of)))

(defgeneric dimensions (domain))

;;;added following to conform to manual -- rsp
(defmethod dimensions ((d dimensional-domain))
  (list (dimension-of d)))

(defclass free-module (module dimensional-domain)
  ())

(defsubst free-module? (domain) (typep domain 'free-module))

(defclass vector-space (free-module)
  ()
  ;; Coefficient domain must be a field
  )

(defsubst vector-space? (domain) (typep domain 'vector-space))

(defclass projective-space (free-module)
  ())

(defsubst projective-space? (domain) (typep domain 'projective-space))

(defclass differential-ring (ring)
  ())

(defsubst differential-ring? (domain) (typep domain 'differential-ring))

(define-operations differential-ring
  (deriv (element self)) -> (element self))

(defclass quotient-ring (domain)
  ())

;; Quotient Fields

(defclass quotient-field (field)  
  ((ring :initform nil :initarg :ring
	 :reader QF-ring)
   (zero :initform nil)
   (one :initform nil)))

(defmethod characteristic ((domain quotient-field))
  (characteristic (QF-ring domain)))

;; The accessors here must not be numerator and denominator because
;; sometimes the internal structure is not a domain element and we
;; actually want to get our hands on the internal structure.
;; NUMERATOR and DENOMINATOR always return domain elements.

(defclass quotient-element (domain-element)
  ((numerator :accessor qo-numerator
	      :initarg :numerator)
   (denominator :accessor qo-denominator
		:initarg :denominator)))

#+Genera
(defmacro with-numerator-and-denominator
    ((num denom) quotient-element &body body &environment env)
  (scl:once-only (quotient-element &environment env)
    `(let ((,num (qo-numerator ,quotient-element))
	   (,denom (qo-denominator ,quotient-element)))
      ,@body)))

#-Genera
(defmacro with-numerator-and-denominator
    ((num denom) quotient-element &body body)
  `(let ((,num (qo-numerator ,quotient-element))
	 (,denom (qo-denominator ,quotient-element)))
    ,@body))

;;; Concrete classes

;; Sets

(defclass mutable-set (set)
  ()
  (:documentation "Sets built from this class can be modified"))


(defclass finite-set (set)
  ())

(defclass set-element (domain-element)
  ((key :reader element-key
        :initarg :key)))

(defclass set-element1 (set-element)
  ())

(defclass set-element2 (set-element)
  ((value :accessor element-value
          :initarg :value)))

(defclass set-elements-as-singletons (set)
  ())

(defclass set-elements-as-pairs (set)
  ())

(defclass set-with-element-list (finite-set has-equality)
  ((elements
    :accessor set-element-list
    :initform (list nil)
    :initarg :elements)))

(defclass mutable-set-with-element-list (set-with-element-list mutable-set)
  ())

(defclass set-with-sorted-element-list (set-with-element-list has-comparison)
  ())

(defclass mutable-set-with-sorted-element-list
    (mutable-set-with-element-list has-comparison)
  ())

;; The intiable sets classes

(defclass simple-set (mutable-set-with-element-list set-elements-as-singletons)
  ())

(defclass set-of-pairs (mutable-set-with-element-list set-elements-as-pairs)
  ())

(defclass ordered-simple-set
    (mutable-set-with-sorted-element-list set-elements-as-singletons)
  ())

(defclass ordered-set-of-pairs
    (mutable-set-with-sorted-element-list set-elements-as-pairs) 
  ())


;; Numbers of all sorts

;; The following class is included in the numeric and
;; general-expression classes to minimize the number of different
;; methods that have to be created.  In the general expression code,
;; we usually want to treat numeric quantities the same as
;; general-expressions.  So, instead of generating two different methods we dispatch on ge-or-numeric.   --RZ 8/3/94

(defclass ge-or-numeric (domain-element) ())

;; All numeric quantities are built from this class (including
;; transcendental elements like e and pi when they exist).
(defclass numeric (ge-or-numeric)
  ())

;; All domains that consist solely of numeric elements contain this class
(defclass numeric-domain (domain)
  ())

(defsubst number? (x)
  (or (typep x 'cl:number)
      (typep x 'numeric)))

;; Rational integers

(defclass rational-integers (ring caching-zero-and-one numeric-domain)
  ())

(defmethod characteristic ((domain rational-integers))
  0)

(defclass rational-integer (numeric)
  ((value :initarg :value
          :reader integer-value)))

;; Real numbers 

(defclass real-numbers (field numeric-domain)
  ())

(defmethod initialize-instance :after ((domain real-numbers) &rest plist)
  (declare (ignore plist))
  (assert-ordered-domain domain)
  (assert-complete-set domain))

(defmethod characteristic ((domain real-numbers))
  0)

(defclass real-number (numeric)
  ())

(defclass floating-point-number (real-number)
  ((value :initarg :value
          :reader fp-value)))

(defclass bigfloat (real-number)
  ((mantissa
    :reader bigfloat-mantissa
    :initarg :mantissa)
   (exponent
    :reader bigfloat-exponent
    :initarg :exponent)))


(defclass complex-numbers (algebraic-extension field numeric-domain)
  ())

(defmethod initialize-instance :after ((domain complex-numbers) &rest plist)
  (declare (ignore plist))
  (assert-complete-set domain))

(defmethod characteristic ((domain complex-numbers))
  0)

(defclass complex-number (numeric)
  ((real
    :initarg :realpart
    :reader cn-realpart)
   (imag
    :initarg :imagpart
    :reader cn-imagpart)))

;;  Rational Numbers

(defclass rational-numbers (field numeric-domain)
  ())

(defmethod initialize-instance :after ((domain rational-numbers) &rest plist)
  (declare (ignore plist))
  (assert-ordered-domain domain))

(defmethod characteristic ((domain rational-numbers))
  0)

(defclass rational-number (quotient-element numeric)
  ())

;; Finite fields 

(defclass GFp (field numeric-domain)
  ((characteristic
    :initform 0
    :initarg :characteristic
    :reader characteristic)))

(defclass GFq (GFp)
  ((degree
    :initarg :degree
    :reader field-degree)))

(defclass GFp-element (numeric)
  ((value
    :reader gfp-value
    :initarg :value)))

(defclass GF2^n (GFq)
  ((reduction-table
    :initarg :reduction-table
    :reader GFp-reduction-table)))

(defclass GFm (rng numeric-domain)
  ())

(defclass GFm-element (numeric)
  ((value :initarg :value :reader value)
   (modulus :initarg :modulus :reader modulus)))

;; Polynomials

;; These are the pieces that are common to all polynomial domains and
;; polynomial representations.
(defclass has-ring-variables ()
  ((variables
    :initform nil
    :initarg :variables
    :reader ring-variables)))

(defclass polynomial-ring (ring module has-ring-variables)
  ())

;;FIXTHIS I don't think this is quite right.  I.e. Its not a GCD
;;domain for any coefficient domain.
(defmethod initialize-instance :after ((domain polynomial-ring) &rest plist)
  (declare (ignore plist))
  (assert-integral-domain domain))

;; Multivariate Polynomial rings need some structure to manage the their
;; variables.  This class provides hash tables and accessor methods of
;; this purpose.  This class is implementational.
(defclass variable-hash-table (has-ring-variables)  
  ((variable-hash-table
    :initform nil
    :accessor variable-hash-table)
   (variable-table
    :initform nil
    :accessor variable-index-table)))

;; It is often useful to cache the values of zero and one since they are
;; often needed.  Need to include the class domain here to make
;; caching... more specific than just domain.
(defclass caching-zero-and-one (domain)
  ((zero)
   (one)))

;; Multivariate polynomials

(defclass multivariate-polynomial-ring
    (polynomial-ring variable-hash-table caching-zero-and-one)
  ())

;; This is just the root of the polynomial structural type hierarchy.
(defclass polynomial (domain-element)
  ())

;; The following are the two different representation that are used.
;; An mpolynomial uses a recursive structure in the variables, while a
;; epolynomial is an expanded representation that uses exponent vectors.

(defclass mpolynomial (polynomial)
  ((form
    :reader poly-form
    :initarg :form)))

(defclass epolynomial (polynomial has-comparison)  
  ((form
    :reader poly-form
    :initarg :form)))

;; Univariate polynomials

(defclass upolynomial (polynomial)
  ((coef-list
    :reader poly-form
    :initarg :form)))

;; Power Series

(defclass power-series-domain (has-coefficient-domain caching-zero-and-one)
  ())

(defclass tpower-series-domain (power-series-domain)
  ())

(defclass tpower-series-ring (ring has-ring-variables tpower-series-domain)
  ())

(defclass tpower-series-field (field has-ring-variables tpower-series-domain)
  ())

(defclass power-series (domain-element)
  ())

;; Elements of truncated power series domains.  valence, branch-order,
;; and order should always be LISP integers (for now), and coeffs
;; should be a LISP simple vector whose elements are elements of the
;; coefficient domain
;;
;; Here is an example illustrating the use of branch-order, valence and
;; order.
;; Consider the power series 1/x + 1 + x^1/2 + x + x^2 + ... .
;; We first eliminate the fractional exponent "1/2" by putting u = x^1/2.
;; Now the series is u^-2 + 1 + u + u^2 + u^4 + ... with branch order 1/2.
;; Now, we normalize the series by multiplying it with u^2.
;; Now, we have 1 + u^2 + u^3 + u^4 + u^6 + ... with valence -2.
;; Finally, we truncate the series using order.
;;                                                     -- Sekhar 8/2/94.

(defclass tpower-series (power-series)
  ((valence
    :initform 0
    :initarg :valence
    :reader valence)
   (branch-order
    :initform 1
    :initarg :branch-order
    :reader branch-order)
   (order
    :initform *positive-infinity*
    :initarg :order
    :reader order)
   (coeffs
    :initarg :coeffs
    :reader coeffs)))

;; Rational functions

(defclass rational-function-field (quotient-field)  
  ())

(defclass rational-function (quotient-element)
  ())

;; Morphisms

(defclass morphism ()
  ((domain
    :reader morphism-domain
    :initarg :domain)
   (map
    :reader morphism-map
    :initarg :map)
   (range
    :reader morphism-range
    :initarg :range)))

(defclass homomorphism (morphism)
  ())

(defclass automorphism (homomorphism)
  ())

;; Differential domains

(defclass differential-polynomial-ring
    (multivariate-polynomial-ring differential-ring)
  ())

;; This is the base class of all ideals.  Recall that an ideal of a
;; ring R is an R module.  The ring slot is included to indicate the
;; domain that the generators lie in.  The generators themselves are only 
(defclass ideal (module)
  ((ring
    :initarg :ring
    :reader ring-of)
   ;; The current list of generators
   (generators
    :initform ()
    :initarg :generators
    :reader generators-of)))

;; Ideals over principal ideal domains are instances of this class
(defclass PID-ideal (ideal) ())

(defclass grobner-basis (ideal has-comparison)
  ;; The exponent comparison function is managed by HAS-COMPARISON
  ((undones :initform ())
   ;; A list of triples pairs (lt f g), lt(f)<=lt(g), of elements of
   ;; GENERATORS such that if any pair is not in the list, its s-poly
   ;; is guaranteed to be writable as a linear combination of
   ;; GENERATORS, with smaller s-degs
   (reducibles :initform nil :accessor reducibles-of)
   (possibles :initform nil)))

;; Algebraic Extensions

(defclass algebraic-extension-ring 
    (algebraic-extension multivariate-polynomial-ring)
  ())

(defclass algebraic-object (mpolynomial)
  ())


;; Direct Sums

;; These are the root classes.  Classes like DIRECT-SUM-SEMIGROUP are
;; created in the direct-sum.lisp file along with several support
;; methods.

(defclass direct-sum (tuple domain) ())

(defclass direct-sum-element (tuple domain-element) ())

;; Vector Spaces

(defclass free-module-element (tuple domain-element)
  ())

(defclass vector-space-element (free-module-element)
  ())

;; This optimization is included because lisp vectors are used as
;; exponents in the expanded polynomial representation.
(defclass lisp-vector-space (vector-space)
  ())

(defclass lisp-vector (vector-space-element)
  ())

;; Projective spaces

(defclass projective-space-element (vector-space-element)
  ())

;; Matrices

;; This is is the domain of all matrices over a given ring.
(defclass matrix-space (module) ())

(defclass real-matrix-space (matrix-space) ())

(defclass complex-matrix-space (matrix-space) ())

(defclass GL-n (group has-coefficient-domain dimensional-domain) 
  ()
  (:documentation "General linear group"))

(defclass PSL-n (GL-n)
  ())

(defclass SL-n (PSL-n)
  ())

(defclass O-n (GL-n)
  ())

(defclass SO-n (O-n)
  ())

(defclass matrix-element (domain-element)
  ((value
    :initarg :value
    :reader matrix-value)))

(defclass matrix-space-element (matrix-element)
  ((dimension1 :initarg :dimension1)
   (dimension2 :initarg :dimension2)))

;; These two classes are for efficiency
(defclass real-matrix-space-element (matrix-space-element) ())

(defclass complex-matrix-space-element (matrix-space-element) ())

(defclass GL-n-element (matrix-element)
  ())

(defclass PSL-n-element (GL-n-element)
  ())

(defclass SL-n-element (PSL-n-element)
  ())

(defclass O-n-element (GL-n-element)
  ())

(defclass SO-n-element (O-n-element)
  ())

;; Quaternions

(defclass quaternion-domain (vector-space algebra)
  ()
  (:documentation "algebra of quaternions"))

(define-operations quaternion-domain
  (conjugate (element self)) ->(element self))

(defclass unit-quaternion-domain
    (group dimensional-domain has-coefficient-domain)
  ()
  (:documentation "group of unit quaternions"))

(defclass quaternion-with-multiplication ()
  ())

(defclass quaternion-domain-element
    (quaternion-with-multiplication vector-space-element)
  ())

(defclass unit-quaternion-domain-element
    (quaternion-with-multiplication tuple domain-element)
  ())
