; This has not been tested well enough.

(in-package "WEYLI")

;; We define a class for representing multipoles here.
;; We make it a subclass of domain-element,so that we may associate
;; something in the domain inheritence hierarchy with it later.
;; Maybe powerseries-domain?

;;Also when we want to generalize the transformations
;;to other interactions , it may be necessarry to inherit mutipole
;;from xyz that inherits from domain-element

;; Center will be a pair of real numbers (a complex number)&&
;; range will be a real number &&
;; form will be a list of real numbers.

;We will assume that each multipole is 10 terms long

(defvar *number-of-terms* 10)

;; The domain of multipole expansions.  Multipole expansions are best
;; thought of as functions from one domain into another, but which
;; have a very specific represnetation.  They are a representation of
;; a Hilbert space.
(defclass multipole-ring (function-space ring has-coefficient-domain)
    ())

(defmethod print-object ((domain multipole-ring) stream)
  (format stream "MultiPole(~S->~S)"
	  (funct-domain-of domain) (funct-range-of domain)))

;; The form of a multipole expansion is just a list of coefficients.
(defclass multipole-expansion (domain-element) 
     ((center :initarg :center :reader center-of)
      (range :initarg :range :accessor range-of)
      (form :initarg :form :reader form-of)))

(define-domain-creator multipole-ring ((domain domain) (range domain))
  (let ((domain (make-instance 'multipole-ring :domain domain :range range)))
    (make-homomorphism coefficient-domain
		       #'(lambda (c) (make-multipole-expansion domain 0 c))
		       domain)
    domain))

;; This method creates a multipole for a given charge at a given
;; position. Is it better not to check for being in-bound?
(defmethod make-multipole-expansion ((domain multipole-ring)
				     (position number) &rest coefs)
  (when (not (in-bound? position))
    (error "Point out of range: ~S" position))
  (let* ((coef-domain (coefficient-domain-of domain))
	 (array (make-array *number-of-terms*
			    :initial-element (zero coef-domain))))
    (loop for i fixnum below *number-of-terms*
	  for c in coefs
	  do (setf (svref array i) (coerce c coef-domain)))
    (make-instance 'multipole-expansion :domain domain
		  :center position
		  :range (zero (range-domain-of domain))
		  :form array)))
;; Shifting multipoles

(defmethod shift-multipole ((m multipole-expansion) (xy number))
  (let ((domain (domain-of m))))
  (make-instance 'multipole :domain 'power-series?
		 :center xy
		 :range (+ (range-of m) (abs xy))
		 :form (shift-m-form (form-of m) (center-of m)
				     (coerce xy (coefficient-domain-of domain)))))



;; The work involved will be in
;; 1) computing the new range.
;; 2) extracting the old form and producing the new one


;;adding two mutipoles
(defmethod-sd plus ((m multipole-expansion) (n multipole-expansion))
  (when (not (eq (center-of m) (center-of n)))
    (error "Can't add multipole-expansions with different centers"))
  (make-instance 'multipole :domain 'power-series?
		 :center (center-of m)
		 :range (max (range-of m) (range-of n))
		 :form (mpef-pairwise-m-sum (form-of m) (form-of n))))
;;The work involved will be in
;; 1)error check to ensure that the centers coincide
;; 2)computing the new form
;; 3)choosing the greater of the two ranges
;; 4)fixing the center




;;We now define a class called local-field.
;;This is a representation of the local field.
;;Again what would the domain be? We may want it to be
;;powerseries or something else (We might want mutipole expansions
;;and local expansions to be in different domains)

(defclass local-field (domain-element)
     ((center :initarg :center :reader center-of)
      (range :initarg :range :reader range-of)
      (form :initarg :form :reader form-of)))

;shifting local fields
(defmethod shift-local-field ((l local-field) (posn number))
  (when (> (abs (- posn (center-of l)))
	   (range-of l))
    (error "Can't make this shift"))
  (make-instance 'local-field :domain 'power-series?
		 :center posn
		 :range (- (range-of l) (abs (- posn (center-of l))))
		 :form (shift-l-form (form-of l) (center-of l) posn)))

;the work involved will be in
;;1)making sure that the position is within the local field (error check)
;;2)computing the new center (easy) and the new range
;;3)computing the new form (hard)

;adding local fields
(defmethod plus ((l1 local-field)(l2 local-field))
  (cond ((neq (center-of l1) (center-of l2)) nil)
	(t (make-instance 'local-field :domain 'power-series?
			  :center (center-of l1)
			  :range (min (range-of l1) (range-of l2))
			  :form (mpef-pairwise-m-sum (form-of l1) (form-of l2))))))

;;the work involved will be in
;;1)making sure that the centers coincide(error check)
;;2)computing the new center (easy) and the new range (easy)
;;3)computing the new form

;this is the key method that localizes a multipole

(defmethod localize ((m multipole) (posn number))
  (when (< (dist posn (center-of m)) (* (coerce 2 r) (range-of m)))
    (error "Some problem in localize"))
  (make-instance 'local-field :domain 'power-series?
		 :center posn
		 :range (range-of m)
		 :form (localize-form (form-of m) (center-of m) posn)))

;;the work involved will be in
;;1)Checking that the posn is sufficiently away from the multipole
;;2)Computing the range
;;3)Computing the new form(very hard)

;; IN-BOUND? checks that the number lies within the vertical strip
;; bounded by Re = 0 and Re = 1
(defmethod in-bound? ((position complex-number))
  (and (< 0 (cn-realpart position) 1)
       (< 0 (cn-imagpart position) 1)))

(defmethod in-bound? ((position floating-point-number))
  (< 0 (fp-value position) 1))

(defmethod in-bound? ((position rational-integer))
  (< 0 (integer-value position) 1))


;The param order may be confusing.The kth term is ak.(a1,a2...)

(defmethod the-kthterm (form k)
  (or (nth (- k 1) form) (zero *coefficient-domain*)))

;(a0,a1,a2...)
(defmethod kthterm (form  k)
  (let ((result (nth k form)))
       (if (eq result nil) (coerce 0 c)
	   result)))

;shift from center z1 to z2

(defun shift-m-form (form z1 z2)
  (shift-m-form* form (difference z1 z2)))


(defun shift-m-form* (form z0)
  (do ((result nil) (l 1))
      ((eq l *number-of-terms*) result)
    (setf result (append result (list (what-is-the-lthterm form z0 l))))
    (setf l (+ l 1))))

(defun what-is-the-lthterm (form z0 l)
  (do ((result (coerce 0 r))
       (k 1))
      ((= k (+ l 1))  result)
    (setf result (+ result (* (the-kthterm form k)
			      (expt z0 (- l k))
			      (combinations (- l 1) (- k 1)))))
    (setf k (+ k 1))))

(defun dist (cn1  cn2)
  (abs (- cn1 cn2)))

;; I use mpef- as the prefix before functions that work with multipole
;; expansion forms.


(defun mpef-pairwise-m-sum (f1 f2)
  (cond ((null f1) f2)
	((null f2) nil)
	(t (cons (+ (first f1) (first f2))
		 (mpef-pairwise-m-sum (rest f1) (rest f2))))))

;shift from center z1 to center z2

(defun shift-l-form (form z1  z2 )
  (shift-l-form* form (- z1 z2)))

(defun shift-l-form* (form z0)
  (let ((result nil))
    (dotimes (l *number-of-terms* result)
      (setf result (append result (list (what-is-the-lthterm2 form z0 l)))))))

(defun what-is-the-lthterm2 (form z0)
  (do ((result (coerce 0 c)) (k l))
      ((= k *number-of-terms*) result)
    (setf result (+ result (* (the-kthterm form (+ k 1))
			      (expt (minus z0) (- k l))
			      (combinations k l))))
    (setf k (+ k 1)))) 

(defun localize-form (form z1  z2 )
  (localize-form* form (- z1 z2)))

(defun localize-form* (form z0 ) 
  (cons (do ((result (coerce 0 c)) (k 1))
	    ((= k *number-of-terms*) result)
	  (setf result (+ result (* (the-kthterm form k)
				    (expt (/ (- z0)) k))))
          (setf k (+ k 1)))              
	(rest-of-local form z0)))

(defun rest-of-local (form z0 )
  (do ((l 1)(result nil))
      ((= l (- *number-of-terms* 1)) result) ;;;ha ha ha ha
    (setf result (append result (list (lth-local-term form z0 l))))
    (setf l (+ l 1))))

(defun lth-local-term (form z0  l)
  (do ((result (coerce 0 c)) (k 1))
      ((= k *number-of-terms*) result)
    (setf result (+ result (* (the-kthterm form k )
			      (expt (recip (- z0)) (+ k l))
			      (combinations (+ l (- k 1)) (- k 1)))))   
    (setf k (+ k 1))))














