
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;		    General Representation Classes
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; general-classes.lisp,v 1.5 1995/05/24 17:42:00 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.5")

(defclass has-memoization ()
  ((memos :initform (make-hash-table :test #'equal))))

;;; Classes for General expressions

(defvar *global-functions* ()
  "These are the functions known by everyone")

(defclass general-expressions (has-memoization non-strict-domain domain)
  ((variables
    :initform ()
    :accessor ge-variables)
   (functions
    :initform nil
    :accessor ge-functions)
   (context
    :initform ()
    :accessor ge-context)))

(defclass general-expression (ge-or-numeric)
  ((simplified?
    :initform nil
    :accessor simplified?)))

;; This class is used to define those general expressions that are
;; indivisible.
(defclass ge-atom () ())

(defsubst ge-atom? (x) (typep x 'ge-atom))

(defclass ge-variable (general-expression has-property-list ge-atom)
  ((symbol
    :initarg :symbol
    :accessor symbol-of)
   (string
    :initarg :string
    :accessor string-of)))

;; N-ary operators are built from this class 
(defclass ge-nary (general-expression)
  ((terms
    :initform nil
    :initarg :terms
    :accessor terms-of)))

(defsubst ge-nary? (x) (typep x 'ge-nary))

(defclass ge-plus (ge-nary)
  ())

(defsubst ge-plus? (x) (typep x 'ge-plus))

(defclass ge-times (ge-nary)
  ())

(defsubst ge-times? (x) (typep x 'ge-times))

(defclass ge-expt (general-expression)
  ((base
    :initarg :base
    :accessor base-of)
   (exp
    :initarg :exp
    :accessor exponent-of)))

(defsubst ge-expt? (x) (typep x 'ge-expt))

;; FUNCTIONS

;; Functions themselves are first class objects.  So we need a representation 
;; for a function, and a second representation for a functional application. 
;; These functions should be cached just the way variables are cached. 

;; ABSTRACT-FUNCTION is the base class for all functions.  At a minimum 
;; all functions have a specified number of arguments, and a property-list 
;; in which additional information can be stored. 
(defclass abstract-function (domain-element has-property-list)
  ((nargs
    :initarg :nargs                     ; The number of arguments
    :accessor nargs-of)))

;; This separated out so that we can implemented existential as well
;; universal quantifiers.
(defclass has-bound-variables ()
  ((bound-vars
    :initarg :bound-vars
    :accessor bound-vars-of)))

;; APPLICABLE-FUNCTION indicates that this object is a symbolic 
;; lambda expression
(defclass applicable-function (abstract-function has-bound-variables)
  ((body
    :initarg :body
    :reader body-of)))

(defsubst applicable-function? (x) (typep x 'applicable-function))

;; GE-FUNCTION is the class of named functions, 
(defclass ge-function (abstract-function has-name)
  ())

(defsubst ge-function? (x) (typep x 'ge-function))

;; GE-FUNCTION-DERIV is used to represent the derivative of a function. 
;; The DERIVS slot is used to hold an order list of the derivatives.  Each 
;; element of the list is a number from 0 to nars - 1 indicating a derivative 
;; in that position.  Numbers can appear more than once and are sorted.

(defclass ge-function-deriv (ge-function)
  ((derivs
    :initform ()
    :initarg :derivs
    :accessor derivs-of)))

;; Notice that a GE-FUNCTION-DERIV is also a GE-FUNCTION
(defsubst ge-function-deriv? (x) (typep x 'ge-function-deriv))

(defclass ge-application (general-expression)
  ((funct
    :initarg :funct                     ; The function being applied
    :accessor funct-of)
   (args
    :initarg :args                      ; Arguments to the function
    :accessor args-of)))

(defsubst ge-application? (x) (typep x 'ge-application))

(defclass ge-equation (general-expression)
  ((lhs
    :initarg :lhs
    :accessor lhs-of)
   (rhs
    :initarg :rhs
    :accessor rhs-of)))

(defclass ge-eqn= (ge-equation)
  ())

(defsubst ge-eqn=? (exp)
  (typep exp 'ge-eqn=))

(defclass ge-eqn> (ge-equation)
  ())

(defsubst ge-eqn>? (exp)
  (typep exp 'ge-eqn>))

(defclass ge-eqn>= (ge-equation)
  ())

(defsubst ge-eqn>=? (exp)
  (typep exp 'ge-eqn>=))

;; The expression must be general-expression, so I am also make a univerally 
;; quantified set a domain-element
(defclass universal-quantified-set (has-bound-variables set general-expression)
  ((exprs
    :initarg :expressions  
    :accessor exprs-of)))

;; Fourier transforms 

(defclass ge-fourier (general-expression)
  ((argument
    :initarg :argument
    :accessor argument-of)
   (space-var
    :initarg :space-var
    :accessor space-var-of)
   (freq-var
    :initarg :freq-var
    :accessor freq-var-of)))

(defsubst ge-fourier? (x) (typep x 'ge-fourier))

(defclass ge-ifourier (general-expression)
  ((argument
    :initarg :argument
    :accessor argument-of)
   (space-var
    :initarg :space-var
    :accessor space-var-of)
   (freq-var
    :initarg :freq-var
    :accessor freq-var-of)))

(defsubst ge-ifourier? (x) (typep x 'ge-ifourier))
