;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
#|

 ===========================================================================
			    Weyl ASDF Definition
 ===========================================================================
 (c) Copyright 1989, 1993 Cornell University

|#

(in-package :asdf)

(defsystem :weyl
  :description "WEYL: A computer algebra substrate."
  :version "0.1.0"
  :depends-on ("closer-mop")
  :license "Custom"
  :components
  ((:file "packages")
   (:file "lisp-support" :depends-on ("packages"))
   (:file "domain-support"
    :depends-on ("packages" "lisp-support"))
   (:module "classes"
    :pathname "classes"
    :depends-on ("domain-support")
    :components
    ((:file "algebraic-domains")
     (:file "space-classes")
     (:file "general-classes")))
   (:file "avl" :depends-on ("classes"))
   (:file "lisp-numbers" :depends-on ("classes"))
   (:file "sets" :depends-on ("classes"))
   (:file "morphisms":depends-on ("classes" "avl"))
   (:file "quotient-fields" :depends-on ("classes"))
   (:file "general" :depends-on ("classes"))
   (:file "fourier" :depends-on ("classes"))
   (:file "functions" :depends-on ("classes" "general" "fourier"))
   (:file "direct-sums" :depends-on ("classes"))
   (:module "numbers"
    :pathname "numbers"
    :depends-on ("classes")
    :components
    ((:file "bigfloat")
     (:file "numbers" :depends-on ("bigfloat"))
     (:file "gfp")))
   (:module "polynomials"
    :pathname "polynomials"
    :depends-on ("classes")
    :components
    ((:file "poly-tools")
     (:file "mpolynomial" :depends-on ("poly-tools"))
     (:file "upolynomial" :depends-on ("poly-tools"))
     (:file "epolynomial" :depends-on ("poly-tools"))
     (:file "sparsegcd" :depends-on ("mpolynomial"))
     (:file "grobner" :depends-on ("mpolynomial" "epolynomial"))))
   (:file "tpower" :depends-on ("polynomials"))
   (:file "taylor" :depends-on ("tpower"))
   (:file "rational-functions" :depends-on ("polynomials" "quotient-fields"))
   (:file "differential-domains" :depends-on ("polynomials"))
   (:file "algebraic-extension" :depends-on ("polynomials"))
   (:module "vector-spaces"
    :pathname "vector-spaces"
    :depends-on ("sets")
    :components
    ((:file "vector")
     (:file "projective-space" :depends-on ("vector"))
     (:file "quaternions" :depends-on ("vector"))))
   (:file "matrix" :depends-on ("morphisms"))
   (:file "topology" :depends-on ("avl" "polynomials" "vector-spaces"))
   (:file "funct-spaces" :depends-on ("classes" "vector-spaces"))
   (:file "mesh" :depends-on ("topology"))))

(defmethod perform :after ((op load-op) (comp (eql (find-system "weyl"))))
  "Initialize and reset the contexts."
  (pushnew :weyl *features*)
  (funcall (intern "INITIALIZE-CONTEXTS" :weyli))
  (funcall (intern "RESET-DOMAINS" :weyli)))
