;;;; Weyl Unit Testing

(in-package :weyl-test)

(lisp-unit:define-test permute
  (let ((seq '()))
    (permute '(a b c) (var) (push var seq))
    (lisp-unit:assert-equal '(a b c) (first  seq))
    (lisp-unit:assert-equal '(b a c) (second seq))
    (lisp-unit:assert-equal '(a c b) (third  seq))
    (lisp-unit:assert-equal '(c a b) (fourth seq))
    (lisp-unit:assert-equal '(b c a) (fifth  seq))
    (lisp-unit:assert-equal '(c b a) (sixth  seq))))

(lisp-unit:define-test choose
  ;; 1 element subset
  (let ((seq '()))
    (choose '(a b c d) (var 1) (push var seq))
    (lisp-unit:assert-equal '(d) (first  seq))
    (lisp-unit:assert-equal '(c) (second seq))
    (lisp-unit:assert-equal '(b) (third  seq))
    (lisp-unit:assert-equal '(a) (fourth seq)))
  ;; 2 element subset
  (let ((seq '()))
    (choose '(a b c d) (var 2) (push var seq))
    (lisp-unit:assert-equal '(d c) (first  seq))
    (lisp-unit:assert-equal '(d b) (second seq))
    (lisp-unit:assert-equal '(c b) (third  seq))
    (lisp-unit:assert-equal '(d a) (fourth seq))
    (lisp-unit:assert-equal '(c a) (fifth  seq))
    (lisp-unit:assert-equal '(b a) (sixth  seq)))
  ;; 3 element subset
  (let ((seq '()))
    (choose '(a b c d) (var 3) (push var seq))
    (lisp-unit:assert-equal '(D C B) (first  seq))
    (lisp-unit:assert-equal '(D C A) (second seq))
    (lisp-unit:assert-equal '(D B A) (third  seq))
    (lisp-unit:assert-equal '(C B A) (fourth seq)))
  ;; 4 element
  (lisp-unit:assert-equal
   '(a b c d)
   (choose '(a b c d) (var 4) var))
  ;; Error
  (lisp-unit:assert-error
   'error
   (choose '(a b c d) (var 5) var)))
