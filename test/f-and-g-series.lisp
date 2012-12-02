;;;;  Weyl Unit Testing

(in-package :weyl-test)

(lisp-unit:define-test f-and-g-series
  (let* ((Z (get-rational-integers))
         (R (get-polynomial-ring Z '(mu eps sigma)))
         (sigma (coerce 'sigma R))
         (mu    (coerce 'mu    R))
         (eps   (coerce 'eps   R))
         (x1 (* (- sigma) (+ mu (* 2 eps))))
         (x2 (+ eps (* -2 (expt sigma 2))))
         (x3 (* -3 mu sigma))
         (x4 (- mu))
         (f-array (make-array 6 :initial-element nil))
         (g-array (make-array 6 :initial-element nil)))
    (labels ((compute-f (n)
               (or (aref f-array n)
                   (setf (aref f-array n)
                         (+ (* x4 (compute-g (1- n)))
                            (* x1 (partial-deriv (compute-f (1- n)) eps))
                            (* x2 (partial-deriv (compute-f (1- n)) sigma))
                            (* x3 (partial-deriv (compute-f (1- n)) mu))))))
             (compute-g (n)
               (or (aref g-array n)
                   (setf (aref g-array n)
                         (+ (compute-f (1- n))
                            (* x1 (partial-deriv (compute-g (1- n)) eps))
                            (* x2 (partial-deriv (compute-g (1- n)) sigma))
                            (* x3 (partial-deriv (compute-g (1- n)) mu)))))))
      (setf (aref f-array 0) (coerce 0 R))
      (setf (aref g-array 0) (coerce 1 R))
      ;; Initial values
      (lisp-unit:assert-true (= (compute-f 0) (coerce 0 R)))
      (lisp-unit:assert-true (= (compute-g 0) (coerce 1 R)))
      ;; n = 1
      (lisp-unit:assert-true (= (compute-f 1) (- mu)))
      (lisp-unit:assert-true (= (compute-g 1) (coerce 0 R)))
      ;; n = 2
      (lisp-unit:assert-true (= (compute-f 2) (* 3 sigma mu)))
      (lisp-unit:assert-true (= (compute-g 2) (- mu)))
      ;; n = 3
      (lisp-unit:assert-true
       (= (compute-f 3)
          (+ (* mu mu) (* (+ (* 3 eps) (* -15 sigma sigma)) mu))))
      (lisp-unit:assert-true
       (= (compute-g 3) (* 6 sigma mu)))
      ;; n = 4
      (lisp-unit:assert-true
       (= (compute-f 4)
          (+ (* -15 sigma mu mu)
             (* (+ (* -45 sigma eps) (* 105 (expt sigma 3))) mu))))
      (lisp-unit:assert-true
       (= (compute-g 4)
          (+ (* mu mu) (* (+ (* 9 eps) (* -45 sigma sigma)) mu))))
      ;; n = 5
      (lisp-unit:assert-true
       (= (compute-f 5)
          (+ (* -1 mu mu mu)
             (* (+ (* -24 eps) (* 210 sigma sigma)) mu mu)
             (* (+ (* -45 eps eps)
                   (* 630 sigma sigma eps)
                   (* -945 (expt sigma 4)))
                mu))))
      (lisp-unit:assert-true
       (= (compute-g 5)
          (+ (* -30 sigma mu mu)
             (* (+ (* -180 sigma eps) (* 420 (expt sigma 3))) mu)))))))
