;;; -*- Mode:Lisp; Package:CL-USER; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			    Package Definitions
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; packages.lisp,v 1.19 1995/04/05 21:39:44 chew Exp

(in-package :common-lisp-user)

(defvar *weyli-exported-symbols*
  '("*" "+" "-" "/" "=" ">" "<" ">=" "<=" "0?" "1?"

    "%MAX"
    "%MIN"
    "%PLUS"
    "%DIFFERENCE"
    "%TIMES"
    "%QUOTIENT"

    "*COERCE-WHERE-POSSIBLE*"
    "*DEFAULT-RANDOM-HEIGHT*"
    "*DOMAINS*"
    "*GENERAL*"
    "*MORPHISMS*"
    "*NEGATIVE-INFINITY*"
    "*POSITIVE-INFINITY*"
    "*PRINT-MODULUS*"

    "ABELIAN-GROUP"
    "ABELIAN-MONOID"
    "ABELIAN-SEMIGROUP"
    "ABSTRACT-POINT"
    "ABSTRACT-SPACE"
    "ADD-NEW-VARIABLE"
    "ADD-RELATION"
    "ADD-SUBSCRIPTS"
    "ADJACENCIES"
    "ALGEBRA"
    "ALGEBRAIC-EXTENSION"
    "ALL-NAMES"
    "ANGLES"
    "ARGS-OF"
    "ARGUMENT-OF"
    "BANACH-SPACE"
    "BASE-OF"
    "BIGFLOAT"
    "BOUNDARY"
    "BOUNDARY-COMPLEX-OF"
    "BOUNDARY-DOMAIN"
    "BOUND-VARS-OF"
    "CHARACTERISTIC"
    "CHOOSE"
    "COEFFICIENT"
    "COEFFICIENT-DOMAIN-OF"
    "COERCE"
    "COERCIBLE?"
    "COMBINATIONS"
    "COMPLEX-NUMBER"
    "COMPLEX-NUMBERS"
    "COMPOSE"
    "CONVERT-TO-LISP-NUMBER"
    "CREATE-MESH"
    "CROSS-PRODUCT"
    "DECLARE-DEPENDENCIES"
    "DEFINE-OPERATIONS"
    "DEGREE"
    "DELETE"
    "DEPENDS-ON?"
    "DERIV"
    "DERIVS-OF"
    "DESCRIBE-OPERATIONS"
    "DIFFERENT-KERNELS"
    "DIFFERENTIAL-RING"
    "DIMENSION-OF"
    "DIMENSIONAL-SPACE"
    "DIMENSIONS"
    "DIRECT-SUM"
    "DISPLAY"
    "DOMAIN-OF"
    "DOT-PRODUCT"
    "DRAW"
    "EQN="
    "EQN>"
    "EQN>="
    "EVEN?"
    "EUCLIDEAN-DOMAIN"
    "EVALUATE-AT"
    "EXPONENT-OF"
    "EXPR-OF"
    "EXPRS-OF"
    "EXPAND"
    "EXPT"
    "FACTOR"
    "FACTORIAL"
    "FIELD"
    "FINITE-FIELD"
    "FINITE-SET"
    "FOURIER"
    "FUNCT"
    "FUNCT-DOMAIN-OF"
    "FUNCT-OF"
    "FUNCT-RANGE-OF"
    "FUNCTION-SPACE"
    "GCD"
    "GCD-DOMAIN"
    "GE-ABS?"
    "GE-APPLICATION?"
    "GE-COS?"
    "GE-DERIV?"
    "GE-EQN=?"       
    "GE-EQN>?"
    "GE-EQN>=?"
    "GE-EQUAL"
    "GE-EXPT?"
    "GE-FOURIER?"
    "GE-FUNCTION?"
    "GE-FUNCTION-DERIV?"
    "GE-IFOURIER?"
    "GE-LOG?"
    "GE-NARY?"
    "GE-PLUS?"
    "GE-SIN?"
    "GE-TAN?"
    "GE-TIMES?"
    "GE-VARIABLE?"
    "GENERATORS-OF"
    "GET-ABSTRACT-SPACE"
    "GET-ALGEBRAIC-EXTENSION"
    "GET-AUTOMORPHISMS"
    "GET-CHAIN-MODULE"
    "GET-COMPLEX-NUMBERS"
    "GET-DIFFERENTIAL-RING"
    "GET-DIRECT-SUM"
    "GET-EUCLIDEAN-SPACE"
    "GET-FACTOR-GROUP"
    "GET-FACTOR-MODULE"
    "GET-FACTOR-RING"
    "GET-FINITE-FIELD"
    "GET-FREE-MODULE"
    "GET-FUNCTION"
    "GET-GL-N"
    "GET-HILBERT-SPACE"
    "GET-HOMOMORPHISMS"
    "GET-LISP-NUMBERS"
    "GET-MATRIX-SPACE"
    "GET-MORPHISMS"
    "GET-O-N"
    "GET-POLYNOMIAL-RING"
    "GET-PSL-N"
    "GET-QUATERNION-DOMAIN"
    "GET-QUOTIENT-FIELD"
    "GET-RATIONAL-INTEGERS"
    "GET-RATIONAL-NUMBERS"
    "GET-REAL-NUMBERS"
    "GET-SL-N"
    "GET-SO-N"
    "GET-TPOWER-SERIES-DOMAIN"
    "GET-UNIT-QUATERNION-DOMAIN"
    "GET-VARIABLE-PROPERTY"
    "GET-VARIABLE-NAME"
    "GET-VECTOR-SPACE"
    "GREATER-FUNCTION"
    "GFM"
    "GFP"
    "GROUP"
    "HEIGHT"
    "HILBERT-SPACE"
    "HOME-OF"
    "HOMOMORPHISM"
    "IFOURIER"
    "INNER-PRODUCT"
    "INSERT"
    "INSERT-BOUNDARY"
    "INTEGRAL"
    "INTEGRAL-DOMAIN"
    "INTERPOLATE"
    "JACOBIAN"
    "LEXICAL-<"
    "LEXICAL->"
    "LHS-OF"
    "LIST-OF-ELEMENTS"
    "LIST-OF-VARIABLES"
    "LIST-OPERATIONS"
    "LOCATE"
    "MAKE-APP-FUNCTION"
    "MAKE-CURVED-SEGMENT"
    "MAKE-GE-FUNCTION"
    "MAKE-GE-DERIV"
    "MAKE-GE-EXPT"
    "MAKE-GE-FUNCTION"
    "MAKE-GE-PLUS"
    "MAKE-GE-TIMES"
    "MAKE-GENERATOR"
    "MAKE-IDEAL"
    "MAKE-MESH"
    "MAKE-MESH-FROM-FILE"
    "MAKE-POINT"
    "MAKE-SAMPLED-FUNCTION"
    "MAKE-SIMPLEX"
    "MAKE-UNION"
    "MAKE-UNIVERSAL-QUANTIFIED-SET"
    "MAKE-GE-VARIABLE"
    "MAP"
    "MAP-OVER-CELLS"
    "MAP-OVER-MAXIMAL-CELLS"
    "MAP-OVER-ELEMENTS"
    "MAP-OVER-EXPRESSIONS"
    "MAP-OVER-FACES"
    "MAP-WITH-DOMAIN"
    "MATRIX-DIMENSIONS"
    "MAX"
    "MEMBER"
    "MEMOIZE"
    "MESH"
    "MIN"
    "MINIMAL-POLYNOMIAL"
    "MINUS?"
    "MONOID"
    "MORPHISM"
    "MULTIPLICATIVE-ORDER"
    "MUTABLE-SET"
    "NAME"
    "NAME-OF"
    "NAME-REGION"
    "NARGS-OF"
    "NORM"
    "NUMBER-OF-ELEMENTS"
    "NUMBER?"
    "ODD?"
    "ONE"
    "ONE-MATRIX"
    "OPERATION-ARGUMENTS"
    "OPERATION-VALUES"
    "OPPOSITE"
    "ORDERED-ABELIAN-GROUP"
    "ORDERED-RING"
    "ORDERED-SET"
    "ORDERED-SET-OF-PAIRS"
    "ORDERED-SIMPLE-SET"
    "PARTIAL-DERIV"
    "PARTITION"
    "PERMUTE"
    "PLUS?"
    "POCHHAMMER"
    "POINT"
    "POLYNOMIAL"
    "POWER-OF?"
    "PRIME?"
    "PROJECTIVE-SPACE"
    "QUOTIENT-FIELD"
    "QUOTIENT-RING"
    "RATIONAL-INTEGER"
    "RATIONAL-INTEGERS"
    "RATIONAL-NUMBER"
    "RATIONAL-NUMBERS"
    "READ-MESH"
    "REAL-NUMBER"
    "REAL-NUMBERS"
    "RECIP"
    "REDUCE-BASIS"
    "REF"
    "REFINE-MESH"
    "RELATIONS"
    "REMAINDER"
    "REPLACE"
    "REQUIRED-OPERATIONS"
    "RESET-DOMAINS"
    "RESULTANT"
    "REVERSION"
    "REVLEX->"
    "RHS-OF"
    "RING"
    "RING-VARIABLES"
    "RNG"
    "SCALAR?"
    "SEGMENT?"
    "SEMIGROUP"
    "SET"
    "SET-ELEMENTS"
    "SET-OF-PAIRS"
    "SIMPLE-FIELD-EXTENSION"
    "SIMPLE-RING"
    "SIMPLE-SET"
    "SIMPLEX"
    "SIMPLEX-SIZE"
    "SIMPLICIAL-COMPLEX"
    "SIMPLIFY"
    "SKEW-FIELD"
    "SPLIT"
    "SQUARE-FREE"
    "STRING-OF"
    "SUBFACE?"
    "SUBSTITUTE"
    "TAYLOR"
    "TERMS-OF"
    "TETRAHEDRON?"
    "TILDE"
    "TOTAL->"
    "TOTIENT"
    "TRANSPOSE"
    "TRIANGLE?"
    "TRUNCATE-ORDER"
    "TUPLE"
    "UNIQUE-FACTORIZATION-DOMAIN"
    "UNIVERSAL-QUANTIFIED-SET"
    "VAR-OF"
    "VAR-DOMAIN-OF"
    "VARIABLE-DERIVATION"
    "VARIABLE-INDEX"
    "VARLIST-OF"
    "VECTOR-SPACE"
    "VERTICES-OF"
    "WITH-MATRIX-DIMENSIONS"
    "WITH-NUMERATOR-AND-DENOMINATOR"
    "WRITE-MESH"
    "ZERO"
    "ZERO-MATRIX")
  "Symbols exported from the internal WEYL package.")

(defvar *weyli-shadowed-symbols*
  '(coerce set + - * / = > < >= <= minus expt abs random
    gcd lcm floor ceiling truncate round max min
    complex conjugate realpart imagpart
    sqrt exp log phase signum minusp zerop plusp
    sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh
    numerator denominator reduce 
    map delete member replace substitute getf union intersection
    apply funcall variable
    type-of)
  "Common lisp symbols shadowed in the internal WEYL package.")

(defvar *weyl-exported-symbols*
  '("MAKE-ELEMENT"
    "MAKE-UPOLYNOMIAL")
  "The WEYL package export these symbols in addition to those exported
form WEYLI.")

;; This should probably only be used to create the WEYL package
(defun use-weyli-package (package)
  "Shadow import the shadowed symbols in the WEYLI package and then
use it."
  (declare (special *weyli-shadowed-symbols*))
  (shadowing-import (loop for sym in *weyli-shadowed-symbols*
			  collect (intern (symbol-name sym) 'weyli))
		    package)
  (use-package (find-package 'weyli) package))

(defun use-weyl-package (package)
  "Shadow import the shadowed symbols in the WEYL package and then use
it."
  (declare (special *weyli-shadowed-symbols*))
  (shadowing-import (loop for sym in *weyli-shadowed-symbols*
			  collect (intern (symbol-name sym) 'weyl))
		    package)
  (use-package (find-package 'weyl) package))

(defun intern-in-package (package-name symbols)
  (loop for sym in symbols
	with  package = (find-package package-name)
	collect (intern sym package)))

(defpackage "WEYLI"
  (:use :common-lisp))

(shadow *weyli-shadowed-symbols* 'weyli)
(export (intern-in-package "WEYLI" *weyli-exported-symbols*) 'weyli)

(defpackage "WEYL"
  (:use :common-lisp))

(use-weyli-package 'weyl)

(export (intern-in-package "WEYL" *weyli-exported-symbols*) 'weyl)
(export (intern-in-package "WEYL" *weyl-exported-symbols*) 'weyl)

;;  Create the basic-graphics package.  This is package is needed if
;;  the system Mesh-Draw is loaded.
#-(and)
(unless (find-package "BASIC-GRAPHICS")
  #-Genera
  (make-package "BASIC-GRAPHICS" :nicknames '(BG)
		:use '(#-MCL LISP #+MCL CL #+PCL PCL
		       #+(and CLOS (not MCL)) CLOS))
  #+Genera
  (make-package "BASIC-GRAPHICS" :nicknames '(BG)
		:use '(#-Rel8 LISP #+Rel8 FUTURE-COMMON-LISP
		       #+PCL PCL #+CLOS CLOS)
		:colon-mode :external))
