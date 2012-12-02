;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			         Flat Meshing
;;; ===========================================================================
;;; (c) Copyright 1995 Cornell University

;;; mesh.lisp,v 1.11 1995/06/09 14:21:32 chew Exp

;;  Everything needed for flat meshing.

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.11")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Control Variables.

(defvar *delaunay* T)			; Each new triangle is checked.
(defvar *cross-edges* nil)              ; Can't cross constraint edges.

(defvar *mesh* nil)			; Holds the current mesh.
(defvar *space* nil)			; Holds the current mesh space.
(defvar *too-close-factor* 0.75)        ; Affects what gets deleted when edges
					; are split.  1.0 deletes too many.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Classes.

(defclass has-home-space ()
     ((home :initarg :home :reader home-of)))

;;  The home-space acts as the parameter space.
(defclass curved-simplex (has-home-space simplex) ())

(defgeneric home-of (simplex)
  (:documentation
   "The home of a noncurved simplex is determined by (the first of)
its vertices.")
  (:method ((simplex simplex)) (domain-of (first (vertices-of simplex)))))

(defclass triangulation (simplicial-complex)
     (;;  Most recent triangle inserted; used for beginning searches.
      (most-recent :initform nil :accessor %most-recent)))

(defclass c-triangulation (triangulation)
     (;;  Holds the constraint edges. (The main tool to query this
      ;;  structure is Constraint.)
      (constraints :initform (make-instance 'simplicial-complex)
		   :reader %constraints-of)))

(defclass CDT (c-triangulation) ())

(defclass named-simplicial-complex (simplicial-complex)
  (;;  Holds names.
   (name-table :initform (make-hash-table) :reader %name-table-of)
   (default-name :initform nil :accessor %default-name-of)))

(defclass mesh (cdt named-simplicial-complex has-home-space)
     (;;  Triangles waiting to be improved during Refine-Mesh.
      (pending-list :initform nil :accessor %pending-list-of)))

(defmethod initialize-instance :after ((mesh mesh) &rest ignore)
  (declare (ignore ignore))
  (with-slots (constraints) mesh
	      (setf constraints (make-instance 'named-simplicial-complex))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Maintenance of Names.
(defgeneric name (simplex mesh)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod name ((simplex simplex) (nsc named-simplicial-complex))
  (gethash (id-number-of simplex) (%name-table-of nsc)))

(defmethod name ((simplex simplex) (mesh mesh))
  (case (dimension-of simplex)
    ((0 1) (name simplex (%constraints-of mesh)))
    (2 (call-next-method))
    (otherwise (error "Illegal use of NAME. ~s" simplex))))    

(defgeneric %set-name (simplex mesh name)
  (:documentation
   "The purpose of this function is unkown."))

(defmethod %set-name ((simplex simplex) (nsc named-simplicial-complex) name)
  (if name
      (setf (gethash (id-number-of simplex) (%name-table-of nsc)) name)
      (remhash (id-number-of simplex) (%name-table-of nsc)))
  name)

(defmethod %set-name ((simplex simplex) (mesh mesh) name)
  (case (dimension-of simplex)
    ((0 1) (%set-name simplex (%constraints-of mesh) name))
    (2 (call-next-method))
    (otherwise (error "Illegal use of (SETF NAME). ~s" simplex))))

(defsetf name %set-name)

(defgeneric insert (simplex nsc &key name &allow-other-keys)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod insert ((simplex simplex) (nsc named-simplicial-complex)
		   &key (name (%default-name-of nsc))
		   &allow-other-keys)
  (setf (name simplex nsc) name)
  (call-next-method))

(defgeneric delete-maximal-cell (simplex nsc)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod delete-maximal-cell ((simplex simplex)
				(nsc named-simplicial-complex))
  (setf (name simplex nsc) nil)
  (call-next-method))

(defgeneric all-names (nsc)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod all-names ((nsc named-simplicial-complex))
  (let ((names nil))
    (maphash #'(lambda (ignore name) (declare (ignore ignore))
		       (pushnew name names))
	     (%name-table-of nsc))
    names))

(defmethod all-names ((sc simplicial-complex))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Utilities.

;;  Rotate the given list to bring the specified position to the front
;;  (items are numbered 0, 1, 2,...).  If no position is specified
;;  then one item is rotated (the second item is then on the front).
(defun rotate-list (list &optional (position 1))
  (append (nthcdr position list) 
	  (butlast list (- (length list) position))))

;;  Rotate a list to bring the leftmost occurrence of the specified
;;  member to the front.  The test is eql unless altered using the
;;  keyword :test.  Results are undefined if the given item is not a
;;  member of the given list.
(defun member-rotate (item list &key (test #'eql) (key #'identity))
  (rotate-list list (position item list :test test :key key)))

;;  Convert a vector into a lisp complex number.
(defgeneric complexer (vector)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod complexer ((vector vector-space-element))
  (unless (= 2 (dimension-of (domain-of vector)))
    (error "Wrong length vector for conversion to complex number. ~s" vector))
  (cl:complex (convert-to-lisp-number (ref vector 0))
	      (convert-to-lisp-number (ref vector 1))))

(defgeneric coordinate-list (vector)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod coordinate-list ((vector vector-space-element))
  (loop for n below (dimension-of (domain-of vector))
	collect (convert-to-lisp-number (ref vector n))))

(defun sqr (item)
  (* item item))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Queue (a list implementation of a standard queue).

;;  FIXTHIS: All this queue stuff should be somewhere else.  There are
;;  also implementations (with consistent interface) for stack,
;;  priority-queue, and random-queue.

(defclass queue ()
     ((front :initarg :front :accessor front :reader %contents)
      ;;  The last item in the queue.
      (back :initarg :back :accessor back)))

(defun make-queue (&key (initial-contents nil))
  (setf initial-contents (copy-list initial-contents))
  (make-instance 'queue :front initial-contents
		 :back (last initial-contents)))

(defgeneric clearq (queue)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod clearq ((queue queue))
  (setf (front queue) nil)
  (setf (back queue) nil))

(defgeneric insertq (item queue)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod insertq (item (queue queue))
  (with-slots (front back) queue
     (cond (front			; Nonempty queue.
	    (setf (rest back) (list item))
	    (setf back (rest back)))
	   (t				; Empty queue.
	    (setf front (list item))	
	    (setf back front)))))

;;  If :delete is null then the item is not removed from the queue.
(defgeneric getq (queue &key delete)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod getq ((queue queue) &key (delete t))
  (with-slots (front) queue
	      (let ((item (first front)))
		(if delete (setf front (rest front)))
		item)))

(defgeneric emptyq? (queue)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod emptyq? ((queue queue))
  (not (front queue)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Geometry.

;;  Given 3 complex numbers forming a triangle, return the complex
;;  number that is the circumcenter.  Works with lisp complex numbers,
;;  since the weyl versions have some problems.
(defun complex-circumcenter (a b c)
  ;;  Map a and b to 0 and 1.
  (let* ((bb (cl:- b a))
	 (cc (cl:/ (cl:- c a) bb))
	 (cx (cl:realpart cc))
	 (cy (cl:imagpart cc))
	 w)    
    (when (cl:= 0 cy) (error "Flat triangle. ~s ~s ~s" a b c))
    (setf w (cl:complex 0.5 (cl:/ (cl:+ (cl:* cx cx) (cl:* cy cy) (cl:- cx))
				  (cl:* 2.0 cy))))
    ;;  Map back.
    (cl:+ (cl:* w bb) a)))

;;  Determine the center of a circle described by two points on the
;;  circle and a radius.  The two points and the returned center are
;;  all complex numbers.  There can be zero, one, or two solutions to
;;  this problem.  If there is no solution then a warning is printed
;;  and the midpoint of the two vertices is returned.  A radius of 0
;;  is taken as a special case and no warning is issued.  If there are
;;  two solutions then normally the solution to the left of the line
;;  from a to b is returned.  The other solution is returned if the
;;  given radius is negative.  Uses Lisp complex numbers.
(defun circle-center (a b radius)
  ;;  Based on using complex arithmetic to map 0,2 to a,b.  c1 and c2
  ;;  represent the center in various coordinate frames.
  (let* ((bb (cl:* 0.5 (cl:- b a)))
	 (rad (cl:/ radius (cl:abs bb)))
	 (cy (cl:sqrt (cl:- 1 (cl:* rad rad))))
	 (c1 (cl:+ 1 cy))
	 ;;  Check for bad radius and alternate solution.
	 (c2 (cond 
	       ((cl:/= 0 (cl:realpart cy))
		(unless (cl:= 0 radius)
		  (warn "Radius too small; half circle assumed. ~s ~s" a b))
		1.0)
	       ((cl:plusp radius) c1)
	       (t (cl:conjugate c1)))))
    ;;  Switch back to original reference frame.
    (cl:+ a (cl:* bb c2))))

(defgeneric make-mean-point (points &key mean-space point-space)
  (:documentation
   "The purpose of this function is unknown."))

;;  Make a point that is the mean of the given points.  The mean-space
;;  is the domain in which the mean is calculated; the point-space is
;;  the domain in which the resulting point resides.  The two spaces
;;  are presumably related via coerce.
(defmethod make-mean-point ((points list) &key
			    (mean-space (domain-of (first points)))
			    (point-space (domain-of (first points))))
  (let* ((vectors (mapcar #'(lambda (p) (coerce p mean-space)) points))
	 (mean (/ (apply #'%plus vectors) (float (length points))))
	 (point (make-point point-space (coerce mean point-space))))
    (unless (eql point-space mean-space)
      (setf (coerce point mean-space) mean))
    point))

(defconstant %deg-over-rad (cl:/ 180.0 cl:pi))

;;  Compute the angle between two vectors.
(defgeneric angle (vertex triangle &rest args &key space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod angle ((a vector-space-element) (b vector-space-element)
		  &key (radians nil) (degrees (not radians)) &allow-other-keys)
  ;;  The realpart is needed because we sometimes get a small
  ;;  imaginary component due to numerical error.
  (let ((angle (cl:realpart (cl:acos (convert-to-lisp-number
				      (/ (dot-product a b)
					 (sqrt (* (dot-product a a)
						  (dot-product b b)))))))))
    (if degrees (cl:* %deg-over-rad angle)
	angle)))

;;  Create a parameter-space.  This is used mainly to create simplices
;;  that are curved in the target-space.  The map should take a
;;  vector-space-element and produce an element of the target-space.
;;
;;  If an inverse-map is given then that is used build the
;;  correspondence between the parameter-space and the target-space
;;  (but inverse-maps are usually unavailable).  The inverse-map
;;  should take an element of the target-space and produce a lisp
;;  vector.  The dimension of the parameter-space must be specified
;;  when an inverse-map is used.
;;
;;  If no inverse-map is available then the spaces are tied together
;;  via the correspondence between the parameter-vectors and the
;;  target-points (i.e., the result will be that the target-points can
;;  be coerced into the parameter-space where they have the
;;  coordinates given by the parameter vectors).  The
;;  parameter-vectors must be lisp vectors.  [At some point, we may
;;  want to check that the map actually takes the parameter-vectors
;;  into the target-points.]
;;
(defgeneric make-parameter-space (map-function target-space &key parameter-vectors
                                               target-points dimension inverse-map)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod make-parameter-space ((map function) (target-space vector-space)
				 &key parameter-vectors target-points
				 dimension inverse-map)
  (unless dimension
    (if parameter-vectors (setf dimension (length (first parameter-vectors)))
	(error "Cannot determine dimension in Make-Parameter-Space.")))
  (let ((parameter-space (make-euclidean-space dimension))) ; A new space.
    (make-homomorphism parameter-space map target-space)

    ;;  Establish the inverse correspondence between the spaces.
    (if inverse-map
	(make-homomorphism target-space
			   #'(lambda (e)
			       (make-element parameter-space
					     (funcall inverse-map e)))
			   parameter-space)
	(mapcar #'(lambda (vector point)
		    (setf (coerce point parameter-space)
			  (make-element parameter-space vector)))
		parameter-vectors target-points))
    parameter-space))

;;  Split the given simplex along the given face using the given
;;  splitting-point.  The default splitting-point is the mean-point of
;;  the face.  Results are undefined if the given face does not
;;  correspond to part of the simplex.  Note that the calculations for
;;  the splitting-point and the creation of new simplices takes place
;;  in the simplex's home coordinate system.
(defgeneric split (simplex where &rest args)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod split ((simplex simplex) (where (eql nil)) &key
		  (face (vertices-of simplex))
		  (splitting-point
		   (make-mean-point face :mean-space (home-of simplex))))
  (loop with points = (vertices-of simplex)
	with home = (home-of simplex)
	for v in face
	for new-set = (subst splitting-point v points)
	collect (make-instance (class-of simplex)
		     :vertices new-set :home home)))

;;  Split a simplex within a simplicial-complex.
(defmethod split ((simplex simplex) (where simplicial-complex) &rest args)
  (loop with simplices = (apply #'split simplex nil args)
	initially (delete-maximal-cell simplex where)
	for s in simplices do
	  (insert s where)
	finally (return simplices)))

;;  Splitting a list splits each thing in the list.
(defmethod split ((things list) where &rest ignore)
  (declare (ignore ignore))
  (loop for thing in things
	append (split thing where)))

;;  Size of the given simplex.  We use the length of the longest side.
(defgeneric simplex-size (simplex &optional space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod simplex-size ((simplex simplex) &optional (space (home-of simplex)))
  (loop with points = (vertices-of simplex)
        repeat (1- (length points))
        maximize (loop with p = (first points)
                       for other in (rest points)
                       maximize (convert-to-lisp-number
                                 (distance p other :space space)))
        do (setf points (rest points))))

;;  Returns :left, :right, or :on depending on position of third
;;  vertex in relation to ray from first to second vertex.  Some
;;  effort has been taken to ensure that this operation is safe in the
;;  sense that the answer is the same even when the three vertices are
;;  given in a different order.  Without this caution, you can run
;;  into problems (due to numerical error); for instance, a vertex
;;  that looks as if it's on the line between two triangles can be
;;  claimed to be :outside of each of them.
(defgeneric bend (space &rest points)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod bend ((space vector-space) &rest three-points)
  (unless (= 3 (length three-points))
    (error "Improper number of arguments to Bend; need space then 3 points."))
  (let* ((ordered (sort (copy-list three-points) #'cl:< :key #'id-number-of))
	 (a (coerce (first ordered) space))
	 (b (coerce (second ordered) space))
	 (c (coerce (third ordered) space))
	 (det (* (sign-of-permutation ordered three-points)
		 (+ (* (- (ref b 1) (ref a 1)) (- (ref c 0) (ref b 0)))
		    (* (- (ref a 0) (ref b 0)) (- (ref c 1) (ref b 1)))))))
    (cond ((> 0 det) :left)
	  ((< 0 det) :right)
	  (t :on))))

(defgeneric distance (vector1 vector2 &rest ignore)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod distance ((vectora vector-space-element)
		     (vectorb vector-space-element) &rest ignore)
  (declare (ignore ignore))
  (let ((diff (- vectorb vectora)))
    (sqrt (dot-product diff diff))))

(defmethod distance ((pointa point) (pointb point) &key (space nil))
  (unless space
    (error "Must specify space for Distance between points."))
  (call-next-method (coerce pointa space) (coerce pointb space)))

(defmethod distance ((lista list) (listb list) &rest ignore)
  (declare (ignore ignore))
  (unless (= (length lista) (length listb))
    (error "Cannot comput distance between lists of different lengths. ~s ~s"
	   lista listb))
  (loop for a in lista
        for b in listb
	sum (sqr (- b a)) into sum-of-squares
	finally (return (sqrt sum-of-squares))))	

;;  Returns T iff the given edges cross each other.  Returns true even
;;  when one vertex of one edge is :on the other edge.
(defgeneric edges-cross? (space edge1 edge2)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod edges-cross? ((space vector-space) (edge-a list) (edge-b list))
  (and (not (eql (bend space (first edge-a) (second edge-a) (first edge-b))
		 (bend space (first edge-a) (second edge-a) (second edge-b))))
       (not (eql (bend space (first edge-b) (second edge-b) (first edge-a))
		 (bend space (first edge-b) (second edge-b) (second edge-a))))
       ))

;;  A bounding-box is a pair (low high) of coordinate lists.  For
;;  instance, 2D points would produce: ((low-x low-y) (high-x
;;  high-y)).  Useful mostly for graphics.  Also used for mesh
;;  initialization.
(defgeneric bounding-box (point space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod bounding-box ((point point) (space vector-space))
  (let ((c (coordinate-list (coerce point space))))
    (list c c)))

(defmethod bounding-box ((list list) (space vector-space))
  (if (= 1 (length list))
      (bounding-box (first list) space)
      (let ((a (bounding-box (first list) space))
	    (b (bounding-box (rest list) space)))
	(list (mapcar #'cl:min (first a) (first b))
	      (mapcar #'cl:max (second a) (second b))))))

(defmethod bounding-box ((simplex simplex) (space vector-space))
  (bounding-box (vertices-of simplex) space))

(defmethod bounding-box ((sc simplicial-complex) (space vector-space))
  (let (old new)
    (map-over-cells (cell) sc
      (setf new (bounding-box cell space))
      (unless old (setf old new))
      (setf old (list (mapcar #'cl:min (first new) (first old))
		      (mapcar #'cl:max (second new) (second old)))))
    old))

;;  Report the measure of the given simplex in the given space.
;;  FIXTHIS: This should be a general measure function based on
;;  determinants so that it works in any dimension.
(defgeneric measure (simplex space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod measure ((simplex simplex) (space vector-space))
  (let ((vertices (vertices-of simplex)))
    (case (dimension-of simplex)
      (0 0)
      (1 (distance (first vertices) (second vertices) :space space))
      (2 (let* ((a (complexer (coerce (first vertices) space)))
		(b (complexer (coerce (second vertices) space)))
		(c (complexer (coerce (third vertices) space)))
		(bb (cl:- b a))
		(cc (cl:/ (cl:- c a) bb)))
	   (cl:* 0.5 (cl:imagpart cc) bb (cl:conjugate bb))))
      (otherwise (error "Measure not yet implemented for higher dimensions."))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Segments.

;;  Given endpoints of an arc together with other information about
;;  the arc's shape, return a segment corresponding to the arc.  The
;;  arc can go :thru a given vertex, can have a given :radius, or can
;;  have a given :center vertex.  A radius that is too small (e.g.,
;;  zero) creates a half circle.  Weird things can happen if the given
;;  center is impossible for the given endpoints or if more than one
;;  of these options is given.  Direction can be specified, either :cw
;;  or :ccw.  If direction is not specified then :ccw is the default.
;;  Uses Lisp complex numbers.
(defgeneric arc (point1 point2 space &key thru radius center clockwise cw
                        counterclockwise ccw direction)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod arc ((apoint point) (bpoint point) (space vector-space) &key
		thru radius center
		clockwise (cw clockwise)
		(counterclockwise (not cw)) (ccw counterclockwise)
		(direction (if ccw :ccw :cw)))
  ;;  Convert to complex.  A and b are endpoints of the arc; c is the center. 
  (let* ((a (complexer (coerce apoint space)))
	 (b (complexer (coerce bpoint space)))
	 (c (cond
	      ;;  If center was given, we do a simple conversion.
	      (center (complexer (coerce center space)))
	      ;;  If thru was given, we use the circumcenter.
	      (thru (complex-circumcenter a b (complexer (coerce thru space))))
	      ;;  If radius was given, we compute the center.
	      (radius (circle-center a b (convert-to-lisp-number radius)))))
	 theta-a theta-b generator)
    ;;  Swap the endpoints if not doing :ccw.
    (when (not (eql :ccw direction)) 
      (rotatef a b) (rotatef apoint bpoint))
    ;;  Determine angles and radius (the existing radius value could
    ;;  be nil, too small, or negative).
    (setf theta-a (cl:phase (cl:- a c)))
    (setf theta-b (cl:phase (cl:- b c)))
    (setf radius (cl:abs (cl:- a c)))
    ;;  Make sure direction is right.
    (unless (cl:< theta-a theta-b)
      (setf theta-b (cl:+ theta-b (cl:* 2 cl:pi))))
    ;;  Create the parametric function.
    (setf generator #'(lambda (theta)
			(let* ((ltheta (convert-to-lisp-number theta))
			       (d (cl:complex (cl:cos ltheta) (cl:sin ltheta)))
			       (transformed (cl:+ c (cl:* radius d))))
			  (make-element space (cl:realpart transformed)
					(cl:imagpart transformed)))))
    ;;  Create the segment.
    (make-instance 'curved-simplex :vertices (list apoint bpoint)
		   :home (make-parameter-space
			  #'(lambda (v) (funcall generator (ref v 0)))
			  space
			  :parameter-vectors (list (vector theta-a)
						   (vector theta-b))
			  :target-points (list apoint bpoint)))))

;;  Note that the generator here take a number (the parameter) and
;;  returns an element of the space.
(defgeneric make-curved-segment (space param1 endpoint1 param2 endpoint2 generator)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod make-curved-segment ((space vector-space)
				(a-param-value number) (a-endpoint point)
				(b-param-value number) (b-endpoint point)
				(generator function))
  (make-instance 'curved-simplex :vertices (list a-endpoint b-endpoint)
		 :home (make-parameter-space
			#'(lambda (v) (funcall generator (ref v 0)))
			space
			:parameter-vectors (list (vector a-param-value)
						 (vector b-param-value))
			:target-points (list a-endpoint b-endpoint))))

;;  Return the endpoint common to two segments.
(defun common-endpoint (segment-a segment-b)
  (first (intersection (vertices-of segment-a) (vertices-of segment-b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Triangles.

;;  Report whether the given point is :inside, :outside, or :on the
;;  given triangle.  Works regardless of whether triangle is clockwise
;;  or counterclockwise.  Returns multiple values -- if the vertex is
;;  :on the triangle then an appropriate side or vertex is also
;;  returned.
(defgeneric point-vs-triangle (vertex triangle &key space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod point-vs-triangle ((vertex point) (triangle simplex)
			      &key (space (home-of triangle)))
  (unless (triangle? triangle)
    (error "Point-vs-Triangle only works on triangles. ~s" triangle))
  (loop with on-side = nil
	with vertices = (vertices-of triangle)
	for va in vertices
	for vb in (rotate-list vertices)
	for bend = (bend space va vb vertex)
	collect bend into bends
	do (if (eql bend :on) (setf on-side (list va vb)))
	finally
     (cond
       ((and (member :left bends) (member :right bends)) (return :outside))
       (on-side (return (values :on (cond
				      ((0? (distance (first on-side) vertex
						     :space space))
				       (first on-side))
				      ((0? (distance (second on-side) vertex
						     :space space))
				       (second on-side))
				      (t on-side)))))
       (T (return :inside)))))
		    
;;  Return the (counterclockwise) oriented side opposite the given
;;  vertex.
(defgeneric ccw-side (vertex triangle)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod ccw-side ((vertex point) (triangle simplex))
  (let ((side (opposite vertex triangle)))
    (if (eql :left (apply #'bend (home-of triangle) vertex side))
	side (rotate-list side))))

;;  Given a set of adjacency triples of the form (left-ptr vertex
;;  right-ptr), return a list of triangles created by these triples.
;;  When triangles are created, the points are in counterclockwise
;;  order.  If a poison-vertex is given then no triangle that contains
;;  that vertex or pair of triangles that cover that vertex will be
;;  allowed until the very last triangle created.  This function takes
;;  linear time: Whenever a triangle is created, two new vertices must
;;  be inspected (they are pushed onto triples).  The sum
;;  3*(triangles-to-do) + |triples| always decreases.
(defun triangulate-triples (triples poison-vertex space triangle-class
				    &rest args)
  (loop	with left and right and triangle and relation
	;; T when we need to watch for pair of covering triangles.
	with on-flag = nil		
	with triangle-list         ;;  What we return.
	with triangles-to-do = (- (length triples) 2)
	;;  Choose the next vertex.
	while (and (> triangles-to-do 0) triples)
	for triple = (pop triples)
	for v = (second triple)
	;;   Use it if...  it hasn't been done (once underway, a
	;;   triple can appear more than once), it has a left
	;;   neighbor, it has a right neighbor, and the triangle bends
	;;   the right way.
	do (when (and v	
		      (first triple) (setq left (second (first triple)))
		      (third triple) (setq right (second (third triple)))
		      (eql :left (bend space left right v)))
	     (setf triangle (apply #'make-instance triangle-class
				   :vertices (list left right v)
				   :home space args))
	     ;;  Check for the poison-vertex.  Automatically ok if
	     ;;  there's no poison vertex or we're on the last
	     ;;  triangle.  Also, ok if this is first triangle that
	     ;;  poison-vertex is :on.
	     (setf relation (if poison-vertex
				(point-vs-triangle
				 poison-vertex triangle :space space)))
	     (when (or (null poison-vertex) (= triangles-to-do 1)
		       (eql :outside relation)
		       (and (eql :on relation) (not on-flag) (setq on-flag T)))
	       ;;  Save the triangle, cancel v, and update the
	       ;;  neighboring vertices.  The neighboring vertices
	       ;;  should be rechecked (put back on the list).
	       (decf triangles-to-do) (push triangle triangle-list)
	       (setf (second triple) nil)
	       (setf (third (first triple)) (third triple))
	       (setf (first (third triple)) (first triple))
	       (push (first triple) triples)
	       (push (third triple) triples)))
	finally (return triangle-list))) ; Return the list of triangles.

;;  Given a list of vertices that describe a star-shaped polygon in
;;  counterclockwise order and a star-source vertex, return a set of
;;  triangles that fills the star-shaped polygon.  Note that the star
;;  source is not used as a vertex in the triangulation; it's needed
;;  to make the triangulation algorithm efficient.  Planned use:
;;  retriangulate a hole in a triangulation after a single vertex has
;;  been eliminated.
(defgeneric star-triangulate (star-shape star-source space triangle-class &rest args)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod star-triangulate ((star-shape list) (star-source point)
			     (space vector-space) triangle-class 
			     &rest args)
  ;;  We convert the vertex list into a list of adjacency triples of
  ;;  the form (left v right).
  (let ((triples (mapcar #'(lambda (v) (list nil v nil)) star-shape)))
    (mapc #'(lambda (pred current succ) 
	      (setf (third current) pred) (setf (first current) succ))
	  triples (rotate-list triples) (rotate-list triples 2))
    ;;  Pass the triples to our triangulator.
    (apply #'triangulate-triples
	   triples star-source space triangle-class args)))

;;  Given a list of vertices that describe a flat polygon in
;;  counterclockwise order, return a set of triangles that fills the
;;  flat polygon.  The polygon must be illuminated by the base and all
;;  vertices must be to one side of the base.  The base is determined
;;  by the first and last vertices given.  Planned use: Triangulate
;;  the hole that appears when a constraint side is forced through a
;;  triangulation.  There is one hole on each side of such a
;;  constraint side.
(defgeneric flat-triangulate (flat-polygon space triangle-class &rest args)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod flat-triangulate ((flat-polygon list) (space vector-space)
			     triangle-class &rest args)
  ;;  We convert the vertex list into a list of adjacency triples of
  ;;  the form (left v right)
  (let ((triples (mapcar #'(lambda (v) (list nil v nil)) flat-polygon)))
    (mapc #'(lambda (pred current succ)
	      (setf (third current) pred) (setf (first current) succ))
	  triples (rest triples) (rest (rest triples)))
    ;;  Pass the triples to our triangulator.
    (apply #'triangulate-triples triples nil space triangle-class args)))

;;  Return the circumcenter of the given triangle.
(defgeneric circumcenter (triangle &key space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod circumcenter ((triangle simplex) &key (space (home-of triangle)))
  (unless (triangle? triangle)
    (error "Circumcenter is implemented only for triangles. ~s" triangle))
  (let ((center (apply #'complex-circumcenter
		       (mapcar #'(lambda (v) (complexer (coerce v space)))
			       (vertices-of triangle)))))
    (make-point space (cl:realpart center) (cl:imagpart center))))

(defgeneric circumradius (triangle &key space)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod circumradius ((triangle simplex) &key (space (home-of triangle)))
  (distance (first (vertices-of triangle))
	    (circumcenter triangle :space space) :space space))

;;  Return the triangle's angle at the given vertex.
(defmethod angle ((vertex point) (triangle simplex)
		  &rest args &key (space (home-of triangle)))
  (unless (triangle? triangle)
    (error "Function Angle is implemented only for triangles. ~s" triangle))
  (let ((others (remove vertex (vertices-of triangle)))
	(v (coerce vertex space)))
    (apply #'angle (- (coerce (first others) space) v)
	   (- (coerce (second others) space) v) args)))

(defgeneric angles (triangle &key space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod angles ((triangle simplex) &key (space (home-of triangle)))
  (loop for v in (vertices-of triangle)
	collect (angle v triangle :space space)))

(defgeneric vertices-sorted-by-angle (triangle &key space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod vertices-sorted-by-angle ((triangle simplex) &key
				     (space (home-of triangle)))
  (loop with (a b c) = (mapcar #'(lambda (v) (coerce v space))
				 (vertices-of triangle))
	repeat 3
	for diff = (- c b)
	for size = (convert-to-lisp-number (dot-product diff diff))
	collect (cons size a) into pairs			   
	do (rotatef a b c)
	finally (return (mapcar #'rest (sort pairs #'cl:< :key #'first)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Triangulations.

(defmethod insert ((triangle simplex) (triangulation triangulation)
		   &rest ignore)
  (declare (ignore ignore)) 
  (unless (= 2 (dimension-of triangle))
    (error "Only triangles can be INSERTed into a triangulation. ~s" triangle))
  (setf (%most-recent triangulation) triangle)
  (call-next-method))

;;  Return a list of all triangles adjacent to the given vertices.
(defgeneric neighbors (vertices triangulation)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod neighbors ((vertices list) (triangulation triangulation))
  (case (length vertices)
    (1 (cofacets (cofacets (get-cell vertices triangulation) triangulation)
		 triangulation))
    (2 (cofacets (get-cell vertices triangulation) triangulation))))

;;  Report the neighbor of the given triangle across the given side.
(defgeneric neighbor (triangle side triangulation)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod neighbor ((triangle simplex) (side list)
		     (triangulation triangulation))
  (find triangle (cofacets (get-cell side triangulation) triangulation)
	:test-not #'eql))

;;  Return a list of all the triangles within a given region.  The
;;  region's boundaries are defined by the function Neighbor.  This
;;  method is not terribly efficient, but it probably won't be done
;;  all that often.
(defgeneric neighborhood (start triangulation)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod neighborhood ((start simplex) (triangulation triangulation))
  (unless (triangle? start)
    (error "Neighborhood must start at a triangle. ~s" start))
  (loop with stack = (list start)
        with mark-list = (list start)
        while stack			; Process triangles until we run out.
        for triangle = (pop stack)
        collect triangle
        ;;  Check each neighbor; if it's not on the stack then put it
        ;;  there and mark it.
        do (loop for side in (facets triangle triangulation)
		 for neighbor = (neighbor triangle (vertices-of side)
					  triangulation)
                 if (and neighbor (not (member neighbor mark-list)))
                   do (push neighbor stack)
                      (push neighbor mark-list))))

;;  Return a triangle base (a list of two points) that is between the
;;  given vertex and the corresponding triangle apex.  Triangle
;;  orientation does not matter (aside from not :flat).
(defgeneric near-base (triangle vertex &key space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod near-base ((triangle simplex) (vertex point)
		      &key (space (home-of triangle)))
  (unless (triangle? triangle)
    (error "Near-Base works only on triangles. ~s" triangle))
  (loop with (a b c) = (vertices-of triangle)
	with direction = (bend space a b c)
	repeat 3
	if (eql direction (bend space vertex c b))
	  return (list b c)
	do (rotatef a b c)))

;;  Given a triangle and a destination (a vertex), travel from the
;;  triangle to the destination.  We return the triangle that contains
;;  the destination if one exists.  Otherwise, we return multiple
;;  values: nil and the side where we fell off the triangulation.
;;  Travel is not necessarily along a straight line, although each
;;  triangle we visit is closer than the previous one.
(defgeneric directed-locate (triangle destination triangulation &key space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod directed-locate ((triangle simplex) (destination point)
			    (triangulation triangulation)
			    &key (space (home-of triangle)))
  (unless (triangle? triangle)
    (error "Directed-Locate must start at a triangle. ~s" triangle))
  ;;  Find a side of the triangle that points toward the destination.
  (loop for side = (near-base triangle destination :space space)
	;;  If no side found then we're done.
	do (unless side (return triangle))
	   ;;  Update triangle.  If no triangle, we've fallen off.
	   (setf triangle (neighbor triangle side triangulation))
	   (unless triangle (return (values nil side)))))

;;  Find the triangle that contains the given vertex.
(defgeneric locate (vertex triangulation)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod locate ((vertex point) (triangulation triangulation))
  ;;  Check first to see if it's already present in the triangulation...
  (or (first (neighbors (list vertex) triangulation))
      ;;  Then try a directed search...
      (let ((start (%most-recent triangulation)))
	(if (maximal-cell? start triangulation)
	    (directed-locate start vertex triangulation)))
      ;;  Finally, try looking at everything.
      (catch 'found
	(map-over-maximal-cells (triangle) triangulation
	   (if (not (eql :outside (point-vs-triangle vertex triangle)))
	       (throw 'found triangle)))
	nil)))

;;  An inefficient way to look at all triangles in a triangulation.
;;  Not used elsewhere, but handy for debugging.
(defgeneric triangles (triangulation)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod triangles ((triangulation triangulation))
  (let ((triangles nil))
    (map-over-maximal-cells (triangle) triangulation
			    (push triangle triangles))
    triangles))

;;  Report all vertices adjacent to given vertex (not necessarily in order).
(defgeneric adj-vertices (vertex triangulation)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod adj-vertices ((vertex point) (triangulation triangulation))
  (loop for edge in (cofacets (get-cell (list vertex) triangulation)
			      triangulation)
	for vertices = (vertices-of edge)
	collect (if (eql vertex (first vertices))
		    (second vertices)
		    (first vertices))))

;;  Returns T iff the given edge can be flipped without producing an
;;  inverted triangle.
(defgeneric flip-ok? (edge triangulation)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod flip-ok? ((edge list) (triangulation triangulation))
  (let* ((triangles (neighbors edge triangulation))
	 (apexes (mapcar #'(lambda (tri) (first (opposite edge tri)))
			 triangles))
	 (space (home-of (first triangles))))
    (and (= 2 (length triangles))
	 (eql (apply #'bend space (first edge) apexes)
	      (apply #'bend space (second edge) (reverse apexes))))))

;;  Check if Delaunay property holds for the given edge.  The Delaunay
;;  property here is that the opposite angles sum to less than or
;;  equal to 180 degrees.  There are lots of choices on how to
;;  implement the Delaunay property (e.g., look at the circles, choose
;;  the smallest angle sum, etc.).  This one has the advantage that it
;;  also works for curved surface Delaunay triangulations.  At some
;;  point, it may be desirable to switch to a faster method.
(defgeneric delaunay? (edge triangulation &key space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod delaunay? ((edge list) (triangulation triangulation) &key space)
  (let* ((triangles (neighbors edge triangulation))
	 (t1 (first triangles))
	 (t2 (second triangles)))
    (unless space (setf space (home-of t1)))
    (or  (< (length triangles) 2)
	 (<= (+ (angle (first (opposite edge t1)) t1 :space space)
		(angle (first (opposite edge t2)) t2 :space space))
	     180.0))))

;;  Flip the given side of the triangulation.  In other words, the
;;  side implicitly defines a quadrilateral; replace the side with the
;;  other diagonal of the quadrilateral.  This function will go ahead
;;  and Flip even when inverted triangles are produced.  To avoid
;;  this, check beforehand using Flip-OK?.
(defgeneric flip (side triangulation)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod flip ((side list) (triangulation triangulation))
  (let* ((triangles (neighbors side triangulation))
	 (vert (first (opposite side (second triangles)))))
    (delete-maximal-cell (second triangles) triangulation)
    (split (first triangles) triangulation
	   :face side :splitting-point vert)))

;;  Split the given edge of the triangulation.  Expect strange results
;;  if the splitting-vertex is not on or near the edge.
(defgeneric split-edge (edge triangulation splitting-vertex)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod split-edge ((edge list) (triangulation triangulation)
		       (splitting-vertex point))
  (loop with triangles = (neighbors edge triangulation)
	for triangle in triangles
	do (split triangle triangulation :face edge
		  :splitting-point splitting-vertex)))

;;  Force an edge between two existing vertices of the triangulation.
;;  In a sense, this is an extended version of Flip.
(defgeneric force-edge (edge triangulation)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod force-edge ((edge list) (triangulation triangulation))
  ;;  Don't do anything if the edge already exists.
  (unless (get-cell edge triangulation)
    ;;  We want to travel in a line across the triangulation, deleting
    ;;  the crossed triangles and accumulating the left- and
    ;;  right-chains of vertices that bound them.
    (loop with (start dest) = edge
	  with triangles = (neighbors (list start) triangulation)
	  with space and opposite
	  ;;  Determine the starting triangle; it must be one that
	  ;;  "points at" the dest.
	  with triangle
	    = (loop for tri in triangles
		    do (setf space (home-of tri))
		       (if (edges-cross? space (opposite start tri) edge)
			   (return tri)))
	  ;;  Vertices vl and vr are :left and :right (or :on) of the
	  ;;  edge, respectively.  (We are assuming that start is at
	  ;;  the bottom and dest is at the top.)
	  with (vr vl) = (ccw-side start triangle)
	  with left-chain = (list vl start)
	  with right-chain = (list vr start)
	  until (eql vr dest)		; Quit if dest is reached.
	  for side = (list vl vr)
	  for next-triangle = (neighbor triangle side triangulation)
	  ;;  Delete the old triangle and update (vl vr).
	  do (unless next-triangle
	       (error "Boundary (constraint) edges cross. ~s ~s" edge side))
	     (setf opposite (first (opposite side next-triangle)))
	     (delete-maximal-cell triangle triangulation)
	     (setf triangle next-triangle)
	     (case (bend space start dest opposite)
	       (:left (setf vl opposite) (push vl left-chain))
	       ((:right :on) (setf vr opposite) (push vr right-chain)))
	  finally       
       ;;  Get rid of the final triangle and create new triangles.
       (delete-maximal-cell triangle triangulation)
       (loop for tri in (append
			  (flat-triangulate (cons dest left-chain) space
					    (class-of triangle))
			  (flat-triangulate (reverse right-chain) space
					    (class-of triangle)))
	     do (insert tri triangulation)))))

;;  Remove a vertex and retriangulate.  The vertex must be completely
;;  surrounded by triangles for this to make sense.  Nil is returned
;;  if the operation fails.  Failure implies that no vertex is
;;  eliminated.
(defgeneric remove-vertex (vertex triangulation)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod remove-vertex ((vertex point) (triangulation triangulation))
  ;;  Determine surrounding vertices, in counterclockwise order.
  (loop with neighbors = (neighbors (list vertex) triangulation)
	with triangle = (first neighbors)
	with class = (class-of triangle)
	with space = (home-of triangle)
	with v = (first (ccw-side vertex triangle))
	repeat (length neighbors)
	for edge = (opposite v triangle)
	collect v into surrounding
	do (setf triangle (neighbor triangle edge triangulation))
	   (setf v (find vertex edge :test-not #'eql))
	   (if (null triangle) (return nil))
	finally
     ;;  Remove the old triangles and retriangulate.
     (loop for tri in neighbors do (delete-maximal-cell tri triangulation))
     (loop for tri in (star-triangulate surrounding vertex space class)
	   do (insert tri triangulation))
     (return T)))

;;  Place the given vertex into the triangulation and retriangulate.
;;  This is the main method for building a triangulation.
(defgeneric place (vertex triangulation &key triangle)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod place ((vertex point) (triangulation triangulation) &key
		  (triangle (locate vertex triangulation)))
  ;;  Do nothing if the vertex already appears in the triangulation.
  (unless (get-cell (list vertex) triangulation)
    (unless triangle
      (error "Unable to PLACE; no triangle found. ~s" vertex))
    (unless (member triangle triangulation)
      (error "Unable to PLACE; triangle has been deleted. ~s" triangle))
    (multiple-value-bind
	(relation hit) (point-vs-triangle vertex triangle)
      (case relation
	;;  Inside of triangle; this is the normal case.
	(:inside (split triangle triangulation :splitting-point vertex))
	;;  On triangle.  Here we have to be careful; we've landed on
	;;  the triangle's boundary.  This case should be rare.
	(:on (cond ((listp hit)
		    (split-edge hit triangulation vertex))
		   (t
		    (warn "Placed ~s atop existing vertex ~s." vertex hit)
		    (unless (remove-vertex hit triangulation)
		      (error "Cannot Remove ~s, so cannot Place ~s"
			     hit vertex))
		    (place vertex triangulation))))
	;;  Outside the triangle.  This case should never happen.
	(:outside
	 (error "Cannot PLACE; ~s is :outside ~s." vertex triangle))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Constrained-Triangulations (C-Triangulations).
;;
;;  A triangulation in which some edges (the constraint edges) and
;;  vertices (the constraint vertices) are special.  Constraint edges
;;  cannot be flipped (although they can be Split).  Edges adjacent to
;;  exactly one triangle also cannot be flipped and ideally these
;;  should be constraint edges although this is not enforced.
;;  Constraint segments act as barriers through the use of function
;;  Neighbor; this function normally returns a neighbor, but it cannot
;;  see past a constraint edge when *cross-edges* is nil.  Most
;;  triangulation operations are inherited unchanged, but some have
;;  new behaviors.

;;  Given a list of vertices, return the corresponding constraint.
;;  Return nil if there is no such constraint.  To make sense, the
;;  list should be of length 1 or 2.  This is the main tool for
;;  querying the constraints table of the c-triangulation.
(defgeneric constraint (vertices triangulation)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod constraint ((vertices list) (c-triangulation c-triangulation))
  (get-cell vertices (%constraints-of c-triangulation)))

;;  Modified Neighbor (uses *cross-edges*).
(defmethod neighbor ((triangle simplex) (side list)
		     (c-triangulation c-triangulation))
  (unless (and (not *cross-edges*) (constraint side c-triangulation))
    (call-next-method)))

(defmethod flip-ok? ((edge list) (c-triangulation c-triangulation))
  (unless (constraint edge c-triangulation)
    (call-next-method)))

(defmethod flip ((edge list) (c-triangulation c-triangulation))
  (if (constraint edge c-triangulation)
      (error "Cannot flip a constraint edge. ~s" edge)
      (call-next-method)))

(defmethod split-edge ((edge list) (c-triangulation c-triangulation)
		       (splitting-vertex point))
  (if (constraint edge c-triangulation)
      (error "Use Split to split a constraint edge. ~s" edge)
      (call-next-method)))

;;  Force an edge between existing vertices.  *Cross-edges* is set so
;;  that the operation fails if an attempt is made to force an edge
;;  through a constraint edge.
(defmethod force-edge ((edge list) (c-triangulation c-triangulation))
  (let ((*cross-edges* nil))
    (call-next-method)))

(defmethod remove-vertex ((vertex point) (c-triangulation c-triangulation))
  (unless (constraint (list vertex) c-triangulation)
    (call-next-method)))

;;  Place segment or single vertex into a c-triangulation as a
;;  constraint.  Note that Place acts differently when placing a
;;  dimension-zero simplex than when placing a point.
(defmethod place ((simplex simplex) (c-triangulation c-triangulation)
		  &rest ignore)
  (declare (ignore ignore))
  (when (> (dimension-of simplex) 1)
    (error "Only segments and vertices can be PLACEd. ~s" simplex))
  (insert simplex (%constraints-of c-triangulation)) ; Must do this first.
  (loop for v in (vertices-of simplex)
	do (place v c-triangulation))
  (when (segment? simplex)
    (force-edge (vertices-of simplex) c-triangulation)))

;;  Split triangle on one side of a constraint that is being split.
(defgeneric %split-constraint-one-side (edge triangle new-vertex concave triangulation)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod %split-constraint-one-side ((edge list) (triangle simplex)
				       (new-vertex point) concave
				       (c-triangulation c-triangulation))
  (cond       
    ;;  Difficult case: we are on the same side as the triangle's apex
    ;;  and the new-vertex is :outside the triangle.
    ((and concave (eql :outside (point-vs-triangle new-vertex triangle)))
     (let ((old-neighbors (neighbors edge c-triangulation))
	   (hit-tri (directed-locate triangle new-vertex c-triangulation)))
       (place new-vertex c-triangulation :triangle hit-tri)
       (force-edge (list new-vertex (first edge)) c-triangulation)
       (force-edge (list new-vertex (second edge)) c-triangulation)
       (loop with new = (first
			  (set-difference (neighbors edge c-triangulation)
			      old-neighbors))
	     for tri in (neighborhood (or new triangle) c-triangulation)
	     do (delete-maximal-cell tri c-triangulation))))
    ;;  Easy case: replace a single triangle with 2 new ones.
    (T (split triangle c-triangulation :face edge :splitting-point new-vertex))
    ))

;;  Allow the splitting of a constraint segment.  We return the two
;;  new segments.  Splitting a constraint is a pain; there doesn't
;;  seem to be any way to do it that is esthetically pleasing.
(defmethod split ((simplex simplex) (c-triangulation c-triangulation)
		  &rest args)
  (cond
    ((triangle? simplex) (call-next-method))
    ((segment? simplex)
     (loop with points = (vertices-of simplex)
	   with new-segments = (apply #'split simplex
				      (%constraints-of c-triangulation) args)
	   with new-vert = (apply #'common-endpoint new-segments)
	   ;;  We temporarily re-insert the segment.
	   initially (insert simplex (%constraints-of c-triangulation))

	   ;;  Do each side of the old segment.
	   for triangle in (neighbors points c-triangulation)
	   for space = (home-of triangle)
	   for apex = (first (opposite points triangle))
	   for verts = (vertices-of triangle)
	   for concave = (eql (apply #'bend space (subst new-vert apex verts))
			      (apply #'bend space verts))
	   do (%split-constraint-one-side points triangle new-vert
		     concave c-triangulation)
	   ;;  Finally, we remove the old segment as a constraint.
	   finally (delete-maximal-cell simplex
					(%constraints-of c-triangulation))
		   (return new-segments)))
    (T (error "Can only SPLIT segments and triangles. ~s" simplex))))
	   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  CDTs (Constrained Delaunay Triangulations).
;;
;;  A constrained-triangulation with the empty circle property.
;;  Delaunay properties are maintained via the function Insert; each
;;  newly inserted triangle is checked.  Delaunay checking can be
;;  turned off by setting *delaunay* to nil.  *Delaunay* is also used
;;  internally to decrease the amount of checking done when a new
;;  vertex is inserted via Place (this also prevents an infinite loop
;;  that can occur when 4 points are nearly cocircular).

;;  Constraint edge always satisifies Delaunay property.
(defmethod delaunay? ((edge list) (cdt cdt) &rest ignore)
  (declare (ignore ignore))
  (if (constraint edge cdt) T (call-next-method)))

;;  Check Delaunay properties on insertion.
(defmethod insert ((triangle simplex) (cdt cdt) &rest ignore)
  (declare (ignore ignore))
  (call-next-method)
  (cond
    ((typep *delaunay* 'point)
     (let ((side (opposite *delaunay* triangle)))
       (unless (delaunay? side cdt) (flip side cdt))))
    (*delaunay* (loop for edge in (facets triangle cdt)
		      for side = (vertices-of edge)
		      if (not (delaunay? side cdt))
			do (flip side cdt) (return)))))

;;  Limit delaunay checking when placing a new vertex.
(defmethod place ((vertex point) (cdt cdt) &rest ignore)
  (declare (ignore ignore))
  (let ((*delaunay* vertex))
    (call-next-method)))

;;  Limit delaunay checking.
(defmethod split-edge ((edge list) (cdt cdt) (splitting-vertex point))
  (let ((*delaunay* splitting-vertex))
    (call-next-method)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Meshes.
;;
;;  A mesh is basically a CDT with names and a home-space.
;;  Refine-Mesh can be used to improve a mesh.

(defmethod insert ((triangle simplex) (mesh mesh) &rest ignore)
  (declare (ignore ignore))
  (call-next-method)
  ;;  Save new triangles for later processing.
  (when (member triangle mesh) (push triangle (%pending-list-of mesh))))

(defgeneric dimension-of (mesh)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod dimension-of ((mesh mesh))
  (dimension-of (home-of mesh)))

;;  When a new vertex is placed into a mesh, we want to avoid
;;  introducing new edges that are especially small, so we remove any
;;  vertices that are too close to the new vertex.  A proof shows that
;;  only adjacent vertices (those adjacent to the new vertex) need to
;;  be considered.  [There can be vertices closer that are not
;;  adjacent, but if such a vertex exists then there are already short
;;  edges in the vicinity anyway.]  The idea is to use this only for
;;  vertices that are forced upon us by being part of a boundary.
;;  Thus, we use this when we split boundary edges.  This also needs
;;  to be used if we insert boundary edges in among existing mesh
;;  points (the current version doesn't allow this).
(defgeneric %delete-too-close (vertex mesh too-close)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod %delete-too-close ((vertex point) (mesh mesh) too-close)
  (loop with space = (home-of mesh)
	for v in (adj-vertices vertex mesh)
	if (< (distance vertex v :space space) too-close)
	  do (remove-vertex v mesh)))

;;  The following 3 methods (remove-vertex,
;;  %split-constraint-one-side, and split) cover all the ways that new
;;  triangles or boundaries can be introduced into a mesh during
;;  refine-mesh.  These methods handle the necessary updating of
;;  names.
(defmethod remove-vertex ((vertex point) (mesh mesh))
  (setf (%default-name-of mesh)
	(name (first (neighbors (list vertex) mesh)) mesh))
  (call-next-method))

;;  Name updating.
(defmethod %split-constraint-one-side ((edge list) (triangle simplex)
				       (new-vertex point) concave
				       (mesh mesh))
  (setf (%default-name-of mesh) (name triangle mesh))
  (call-next-method))

;;  Name updating along with code to delete any vertices too-close to
;;  a new boundary vertex.
(defmethod split ((simplex simplex) (mesh mesh) &rest ignore)
  (declare (ignore ignore))
  (cond
    ((triangle? simplex)
     (setf (%default-name-of mesh) (name simplex mesh))
     (call-next-method))
    ((segment? simplex)
     (setf (%default-name-of (%constraints-of mesh)) (name simplex mesh))
     (let* ((new-segments (call-next-method))
	    (new-vert (apply #'common-endpoint new-segments))
	    (points (vertices-of simplex))
	    (space (home-of mesh))
	    (too-close (min (distance new-vert (first points) :space space)
			    (distance new-vert (second points) :space space))))
           ;;  We throw out vertices that are clearly too close, but
           ;;  the factor *too-close-factor* (constant less than 1)
           ;;  prevents us from throwing away too many -- at the cost
           ;;  of generating some edges that are possibly shorter than
           ;;  necessary.
       (%delete-too-close new-vert mesh (* *too-close-factor* too-close))
       new-segments))
    (T (call-next-method))))

;;  Place a boundary and set its name.
(defmethod place ((simplex simplex) (mesh mesh) &key name &allow-other-keys)
  (call-next-method)
  (when (and name (segment? simplex)) (setf (name simplex mesh) name)))

;;  Return the center of the given triangle.  What we mean by the
;;  center depends on the type of mesh that we are doing or on what
;;  approximation we are using for the circumcenter.  Here, we do a
;;  straightforward standard circumcenter.
(defgeneric triangle-center (triangle mesh)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod triangle-center ((triangle simplex) (mesh mesh))
  (unless (eql (home-of triangle) (home-of mesh))
    (warn "Possible improper use of Triangle-Center."))
  (circumcenter triangle))

;;  Checks if given simplex is larger than the-bound.  The-bound can
;;  be nil or a number or a function of two numbers that returns a
;;  number.  The function is evaluated at the mean of the vertices of
;;  the simplex (calculated in space).  Nil effectively acts as
;;  infinity.
(defgeneric too-big? (simplex the-bound space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod too-big? ((simplex simplex) (the-bound T) (space vector-space))
  (let ((bound (cond
		 ((null the-bound) nil)
		 ((numberp the-bound) the-bound)
		 ((functionp the-bound)
		  (apply the-bound (coordinate-list
				    (make-mean-point (vertices-of simplex)
						     :mean-space space
						     :point-space space))))
		 (T (warn "Ignoring unknown bound ~s in Too-Big?" the-bound)
		    nil))))
    (if bound (< bound (simplex-size simplex space)))))

;;  Returns a grade for the given triangle showing whether the
;;  triangle needs to be improved.
(defgeneric grade (triangle mesh angle-bounds size-list)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod grade ((triangle simplex) (mesh mesh)
		  (angle-bounds list) (size-list list))
  (let* ((space (home-of mesh))
	 (vertices (vertices-sorted-by-angle triangle :space space)))
    (cond
      ;;  Largest angle too large.
      ((and (second angle-bounds)
	    (> (angle (third vertices) triangle :space space)
	       (second angle-bounds)))
       :bad-shape)
      ;;  Smallest angle too small (and not a boundary angle).
      ((and (first angle-bounds)
	    (< (angle (first vertices) triangle :space space)
	       (first angle-bounds))
	    (or (not (constraint (opposite (second vertices) triangle) mesh))
		(not (constraint (opposite (third vertices) triangle) mesh))))
       :bad-shape)
      ;;  Size too big.
      ((too-big? triangle
		 (second (member (name triangle mesh) size-list)) space)
       :bad-size)
      ;;  Bounday size too big.
      ((loop for edge in (facets triangle mesh)
	     for segment = (constraint (vertices-of edge) mesh)
	     if (and segment
		     (too-big? segment (second (member (name segment mesh)
						       size-list)) space))
	       return T)
       :bad-segment)
      (T :good))))

;;  Improve the triangle.  Different techniques are used depending on
;;  whether the triangle is obtuse, near-right, or acute.  A proof
;;  shows this works and has the advantage of avoiding the calculation
;;  of circumcenters for obtuse triangles.
(defgeneric improve (triangle mesh)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod improve ((triangle simplex) (mesh mesh))
  (let* ((space (home-of mesh))
	 (vertex (third (vertices-sorted-by-angle triangle :space space)))
	 (max-angle (angle vertex triangle :space space))
	 (side (opposite vertex triangle))
	 (constraint (constraint side mesh)))
    (cond
      ;;  Acute triangle.  Place the circumcenter.
      ((< max-angle 89.9)
       (place (triangle-center triangle mesh) mesh :triangle triangle))
      
      ;;  The long side is a boundary.  Split the boundary.
      (constraint (split constraint mesh))

      ;;  Obtuse triangle.  Recursively split the neighboring triangle.
      ((> max-angle 90.1) (improve (neighbor triangle side mesh) mesh))

      ;;  Near-right triangle; hypotenuse is not a boundary.  Break
      ;;  the hypotenuse.
      (t (split-edge side mesh (make-mean-point side
						:mean-space (home-of triangle)
						:point-space space))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Initializations used by the Mesher Interface.

;;  Create a background-box for the mesh.  This is a box that contains
;;  all the boundaries of the mesh.  Used to initialize the mesh's
;;  CDT.
(defgeneric do-background-box (box border mesh)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod do-background-box ((box list) (border number) (mesh mesh))
  (let* ((space (home-of mesh))
	 (low (mapcar #'(lambda (c) (cl:- c border)) (first box)))
	 (high (mapcar #'(lambda (c) (cl:+ c border)) (second box)))
	 (ll (apply #'make-point space low))
	 (hh (apply #'make-point space high))
	 (lh (make-point space (first low) (second high)))
	 (hl (make-point space (first high) (second low))))
    (insert (make-simplex ll hl hh) mesh)
    (insert (make-simplex ll hh lh) mesh)))

;;  Initialization needed in Name-Region.
(defgeneric build-cdt-from-boundaries (mesh)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod build-cdt-from-boundaries ((mesh mesh))
  ;;  Determine bounding-box and build containing rectangle.
  (loop with box =  (bounding-box (mapcar #'first (%pending-list-of mesh))
				  (home-of mesh))
	with boundaries = (reverse (%pending-list-of mesh))
	initially (setf (%pending-list-of mesh) nil)
		  (do-background-box box (* 0.1 (apply #'distance box)) mesh)
	for (boundary name) in boundaries
	do (place boundary mesh :name name)))

;;  Initialization needed in Refine-Mesh.
(defgeneric refine-mesh-prep (mesh)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod refine-mesh-prep ((mesh mesh))
  ;;  Make sure control variables are set properly.
  (setf *delaunay* T)
  (setf *cross-edges* nil)
  (setf *mesh* mesh)
  (setf *space* (home-of mesh))
  ;;  Place exsting triangles into pending-list in standard order
  ;;  (this ensures we get the same results on different runs).  Also,
  ;;  we get rid of unnamed triangles here.
  (let ((triangles nil))
    (map-over-maximal-cells (tri) mesh
      (if (name tri mesh)
	  (push tri triangles)
	  (delete-maximal-cell tri mesh)))
  (setf (%pending-list-of mesh) (sort triangles #'cl:> :key #'id-number-of))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Mesher Interface.
;;
;;  These are the functions that form the major part of the mesh
;;  interface.  All of them appear in the manual.
(defgeneric boundary-complex-of (mesh)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod boundary-complex-of ((mesh mesh))
  (%constraints-of mesh))

;;  Create an empty mesh.
(defgeneric create-mesh (space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod create-mesh ((space euclidean-space))
  (setf *mesh* (make-instance 'mesh :home space)))

;;  Insert boundaries into the mesh during mesh-initialization.  Can
;;  insert a 0-dimensional simplex, a 1-dimensional simplex (a
;;  segment), or a simplicial-complex containing such simplices.
(defgeneric insert-boundary (simplex mesh &key name)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod insert-boundary ((simplex simplex) (mesh mesh) &key name)
  (when (%most-recent mesh)
    (error "Misuse of Insert-Boundary.  Use only during mesh initialization."))
  (when (< 1 (dimension-of simplex))
    (error "Illegal boundary simplex.  Only dimension 0 or 1 allowed. ~s"
	   simplex))
  (push (list simplex name) (%pending-list-of mesh)))

;;  Name a subregion of the mesh.  The first time this is called it
;;  builds the CDT of the boundaries.
(defgeneric name-region (name point mesh)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod name-region ((name symbol) (point point) (mesh mesh))
  ;;  Build the CDT on the first call.
  (unless (%most-recent mesh)
    (build-cdt-from-boundaries mesh))
  ;;  Region naming.
  (mapcar #'(lambda (tri) (setf (name tri mesh) name))
	  (neighborhood (locate point mesh) mesh))
  mesh)

;;  Check and improve each triangle of the mesh, including new ones.
(defgeneric refine-mesh (mesh &key angle-bounds size-list)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod refine-mesh ((mesh mesh) &key (angle-bounds '(30)) (size-list nil))
  (refine-mesh-prep mesh)
  ;;  Pull triangles from the improvement queue until the queue is empty.
  (loop with triangle
	with improvement-queue = (make-queue)
     	do
     ;;  Load any pending triangles into the improvement queue.
     (loop for tri in (%pending-list-of mesh)
	   do (when (and (maximal-cell? tri mesh)
			 (not (eql (grade tri mesh angle-bounds size-list)
				   :good)))
		(insertq tri improvement-queue)))
     (setf (%pending-list-of mesh) nil)
     (when (emptyq? improvement-queue) (return))
     (setf triangle (getq improvement-queue :delete nil))
     (if (maximal-cell? triangle mesh)
	 (improve triangle mesh)
	 (getq improvement-queue :delete T))) ; Delete only when really gone.
  mesh)

(defmacro make-mesh ((space &rest keyargs &key size-list angle-bounds)
		     &body body)
  (declare (ignore size-list angle-bounds))
  (loop for clause in body
	for (type name . rest) = (if (listp clause) clause
				     (error "Improper clause in Make-Mesh: ~s"
					    clause))
	collect 
	(cond				; string= allows use across packages.
	  ((string= type "POINT")
	   `(setf (gethash ',name %pt-table) (make-point *space* ,@rest)))
	  ((string= type "REGION")
	   `(setf (gethash ',name %rg-table) (make-point *space* ,@rest)))
	  ((string= type "BOUNDARY")
	   `(%mm-boundary ,@(rest clause)))
	  (t (error "Unknown descriptor in Make-Mesh: ~s" clause)))
	into new-body
	finally
     (return `(let ((%pt-table (make-hash-table))
		    (%rg-table (make-hash-table)))
		(setf *space* ,space)
		(setf *mesh* (create-mesh *space*))
		(gethash nil %pt-table) ; Prevents a warning message.
		,@new-body
		(maphash #'(lambda (name point)
			     (name-region name point *mesh*)) %rg-table)
		(refine-mesh *mesh* ,@keyargs)
		*mesh*))))

(defmacro %mm-get-point (desc)
  (cond
    ((atom desc) `(or (gethash ',desc %pt-table)
		      (error "Unbound point name: ~s" ',desc)))
    ((string= (first desc) "PT") `(make-point *space* ,@(rest desc)))
    (t `(error "Unknown point descriptor: ~s" ',desc))))
		
(defmacro %mm-boundary (name options type . desc)
  (cond
    ((string= type "LINE")
     `(%boundary-line (list ,@(mapcar #'(lambda (x) `(%mm-get-point ,x)) desc))
		      ',name ,@options))
    ((string= type "ARC")
     (loop while desc
	   for (point arc-args) = desc
	   collect `(%mm-get-point ,point) into new-desc
	   collect
	   (if (listp arc-args)
	       (let ((part (or (member :thru arc-args)
			       (member :center arc-args))))
		 (when (second part)
		   (setf (second part) `(%mm-get-point ,(second part))))
		 `(list ,@arc-args))
	       `(error "Improperly formed arc options: ~s" ',arc-args))
	   into new-desc
	   do (setf desc (rest (rest desc)))
	   finally
	(return `(%boundary-arc (list ,@new-desc) ',name ,@options))))
    (t `(error "Unknown type in boundary descriptor: ~s" ',type))))

;;  Macro to handle the boundary construction.  Handles the boundary
;;  options :closed? and :split.  Also inserts the new boundaries into
;;  the mesh.  To use this, the body is expected to return a list of
;;  the boundary simplices.
(defmacro defun-boundary (function-name (point-list name) &body body)
  `(defun ,function-name (,point-list ,name &key (closed? nil) (split 0))
     (if closed? (setf ,point-list
		       (append ,point-list (list (first ,point-list)))))
     (loop with boundaries = (progn ,@body)
	   repeat split
	   do (setf boundaries (split boundaries nil))
	   finally
	(loop for b in boundaries
	      do (insert-boundary b *mesh* :name ,name)))))

(defun-boundary %boundary-line (points name)
  (loop for a in points
	for b in (rest points)
	collect (make-simplex a b)))

(defun-boundary %boundary-arc (point-arc-list name)
  (loop for (apoint arc-args bpoint) = point-arc-list
	while bpoint
	collect (if arc-args
		    (apply #'arc apoint bpoint *space* arc-args)
		    (make-simplex apoint bpoint))
	do (setf point-arc-list (rest (rest point-arc-list)))))

;;  Macro for building 2D meshes.  Creates a function that can create
;;  a mesh.  The function can take keyword arguments to control the
;;  mesh requirements.  Defaults for the keyword arguments can be
;;  specified within defmesh.
(defmacro defmesh (name (&key size-list angle-bounds) &body body)
  `(defun ,name (&key (size-list ,size-list)
		      (angle-bounds (or ,angle-bounds '(30))))
     (make-mesh ((get-euclidean-space 2)
		 :size-list size-list :angle-bounds angle-bounds)
		,@body)))

(defmesh circle (:size-list '(circle-boundary .1))
  (region inside 0 0)
  (point origin 0 0)
  (boundary circle-boundary (:closed? t) arc
	    (pt 1 0) (:center origin) (pt 0 1) (:center origin)
	    (pt -1 0) (:center origin) (pt 0 -1) (:center origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  File I/O.
(defgeneric make-mesh-from-file (stream &key angle-bounds size-list)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod make-mesh-from-file ((stream stream) &key angle-bounds size-list)
  (unless (string= (read stream) "MESHREQUEST")
    (error "File should start with token `MeshRequest'."))
  (loop with space and boundary-complex and region-points
	and file-angle-bounds and file-size-list
	with vertex-table = (make-hash-table)
	for token = (read stream nil '%EOF%)
	until (string= token "%EOF%")
	do
     (cond
       ((string= token "VERTEXSET")
	(when space
	  (error "Multiple VertexSets in file."))
	(setf space (get-euclidean-space (read stream)))
	(setf *space* space)
	(read-vertex-set stream vertex-table space))
       ((string= token "SIMPLICIALCOMPLEX")
	(unless space
	  (error "VertexSet must be defined before SimplicialComplex."))
	(setf boundary-complex
	      (read-boundary-simplicial-complex stream vertex-table)))
       ((string= token "REGIONS")
	(setf region-points (read-region-points stream space)))
       ((string= token "ANGLEBOUNDS")
	(setf file-angle-bounds (list (read stream) (read stream))))
       ((string= token "SIZETABLE")
	(setf file-size-list (read-size-table stream)))
       (t
	(error "Confused in file; cannot understand this token: ~s" token)))
	finally
     (let ((mesh (create-mesh space)))
       (map-over-maximal-cells (boundary) boundary-complex
	 (insert-boundary boundary mesh
			  :name (name boundary boundary-complex)))
       (loop for (name point) in region-points
	     do (name-region name point mesh))
       (refine-mesh mesh :angle-bounds (or angle-bounds file-angle-bounds)
		    :size-list (or size-list file-size-list))
       (return mesh))))

(defgeneric read-point (stream space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod read-point ((stream stream) (space domain))
  (apply #'make-point space (loop repeat (dimension-of space)
				  collect (read stream))))

(defgeneric read-vertex-set (stream vertex-table space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod read-vertex-set ((stream stream) (vertex-table hash-table)
			    (space domain))
  (loop with numvertices = (read stream)
	for i below numvertices
	do (setf (gethash i vertex-table) (read-point stream space))))
    
;;  If a complex is passed in via keyword then new simplices are added
;;  to that complex.
(defgeneric read-simplicial-complex (stream vertex-table &key complex)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod read-simplicial-complex
    ((stream stream) (vertex-table hash-table)
     &key (complex (make-instance 'named-simplicial-complex)))
  (loop with simp-dimension = (read stream)
	with numsimps = (read stream)
	with current-name = nil and count = 0
	until (>= count numsimps)
	for next = (read stream)
	if (typep next 'symbol) do
	  (setf current-name next)
	else do
	  (insert (apply #'make-simplex (gethash next vertex-table)
			 (loop repeat simp-dimension
			       collect (gethash (read stream) vertex-table)))
		  complex :name current-name)
	  (incf count))
  complex)

;;  Special code for boundaries so that arcs can be used.  This is
;;  basically a hack that is here until we get a chance to put in
;;  general code for curved objects.
(defgeneric read-boundary-simplicial-complex (stream vertex-table &key complex)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod read-boundary-simplicial-complex
    ((stream stream) (vertex-table hash-table)
     &key (complex (make-instance 'named-simplicial-complex)))
  (loop with simp-dimension = (read stream)
	with numsimps = (read stream)
	with current-name = nil and count = 0
	until (>= count numsimps)
	initially (unless (and (numberp simp-dimension) (= simp-dimension 1))
		    (error "Use 1 for boundary SimplicialComplex, not ~s."
			   simp-dimension))
	for next = (read stream)
	if (typep next 'symbol) do
	  (setf current-name next)
	else do
	  (let* ((arc-args (read stream))
		 (other arc-args))
	    (if (listp arc-args)
		(setf other (read stream))
		(setf arc-args nil))
	    (when arc-args
	      (let ((code (string (first arc-args))))
		(unless (= 2 (length arc-args))
		  (error "Badly formed arc descriptor: ~s" arc-args))
		(setf (first arc-args)
		      (case (aref code 0)
			(#\C :center)
			(#\T :thru)
			(#\R :radius)
			(otherwise (error "Unrecognized arc descriptor: ~s"
					  arc-args))))
		(unless (numberp (second arc-args))
		  (error "Second element should be a number: ~s" arc-args))
		(unless (eql (first arc-args) :radius)
		  (setf (second arc-args)
			(gethash (second arc-args) vertex-table)))
		;;  Any more than single character implies clockwise.
		(if (> (length code) 1)
		    (setf arc-args (append arc-args '(:cw t))))))
	    (setf next (gethash next vertex-table))
	    (setf other (gethash other vertex-table))
	    (insert (if arc-args
			(apply #'arc next other *space* arc-args)
			(make-simplex next other))
		    complex :name current-name)
	    (incf count)))
  complex)

(defgeneric read-region-points (stream space)
  (:documentation
   "The purspose of this function is unknown."))

(defmethod read-region-points ((stream stream) (space domain))
  (loop with numpoints = (read stream)
	repeat numpoints
	for name = (read stream)
	for point = (read-point stream space)
	collect (list name point)
	do (unless (typep name 'symbol)
	     (error "Bad name for Region: ~s ~s" name point))))

(defgeneric read-size-table (stream)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod read-size-table ((stream stream))
  (loop with numentries = (read stream)
	repeat numentries
	for name = (read stream)
	for size = (read stream)
	collect name
	collect size
	do (unless (and (typep name 'symbol) (typep size 'number))
	     (error "Improper entry for SizeTable: ~s ~s" name size))))

(defgeneric fwrite (tuple stream &rest ignore)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod fwrite ((tuple tuple) stream &rest ignore)
  (declare (ignore ignore))
  (apply #'format stream "~s~@{ ~s~}~%"
	 (loop for i below (dimension-of (domain-of tuple))
	       collect (ref tuple i))))

;;  Write the vertex set of anything for which we can do
;;  map-over-cells.  The vertex-table is returned.
(defgeneric write-vertex-set (thing stream)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod write-vertex-set ((thing t) stream)
  (let ((count 0)
	(space nil)
	(vertex-table (make-hash-table)))
    (map-over-cells (vertex 0) thing
	 (setf vertex (first (vertices-of vertex)))
	 (incf count)
	 (if space
	     (unless (eql space (domain-of vertex))
	       (error "Mismatched vertex domains in ~s." thing))
	     (setf space (domain-of vertex))))
    (format stream "VertexSet ~s ~s~%" (dimension-of space) count)
    (setf count 0)
    (map-over-cells (vertex 0) thing
	 (setf vertex (first (vertices-of vertex)))
	 (fwrite vertex stream)
	 (setf (gethash (id-number-of vertex) vertex-table) count)
	 (incf count))
    vertex-table))

(defmethod fwrite ((simplex simplex) stream &key
		   (vertex-table (write-vertex-set simplex stream)))
  (apply #'format stream "~s~@{ ~s~}~%"
	 (loop for v in (vertices-of simplex)
	       collect (gethash (id-number-of v) vertex-table))))

(defmethod fwrite ((sc simplicial-complex) stream &key
		   (vertex-table (write-vertex-set sc stream)))
  (let ((count 0)
	(max-dimension nil)
	(names (all-names sc)))
    (map-over-maximal-cells (cell) sc
      (incf count)
      (if max-dimension
	  (unless (= max-dimension (dimension-of cell))
	    (error "Inconsistent maximal-cell dimensions in ~s." sc))
	  (setf max-dimension (dimension-of cell))))
    (format stream "SimplicialComplex ~s ~s~%" max-dimension count)
    (if names
	(loop for name in names do
	  (format stream "~s~%" name)
	  (map-over-cells (cell max-dimension) sc
	      (when (eql name (name cell sc))
		(fwrite cell stream :vertex-table vertex-table))))
	(map-over-cells (cell max-dimension) sc
	    (fwrite cell stream :vertex-table vertex-table)))))

(defmethod fwrite ((mesh mesh) stream &rest ignore)
  (declare (ignore ignore))
  (format stream "Mesh~%")
  (let ((vertex-table (write-vertex-set mesh stream)))
    (fwrite (%constraints-of mesh) stream :vertex-table vertex-table)
    (call-next-method mesh stream :vertex-table vertex-table)))

;;  User interface.
(defgeneric write-mesh (mesh stream)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod write-mesh ((mesh mesh) stream)
  (unless stream (setf stream *standard-output*))
  (fwrite mesh stream))

(defgeneric read-mesh (stream)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod read-mesh ((stream stream))
  (let ((token (read stream))
	(vertex-table (make-hash-table))
	(space nil)
	(mesh nil))
    (when (string= token "MESH") (setf token (read stream)))
    ;;  Vertex set.
    (unless (string= token "VERTEXSET")
      (error "Expected VertexSet instead of ~s." token))
    (setf space (get-euclidean-space (read stream)))
    (setf *space* space)
    (setf mesh (create-mesh space))
    (read-vertex-set stream vertex-table space)
    ;;  Boundary complex.
    (setf token (read stream))
    (unless (string= token "SIMPLICIALCOMPLEX")
      (error "Expected boundary SimplicialComplex instead of ~s." token))
    (read-simplicial-complex stream vertex-table
			     :complex (%constraints-of mesh))
    ;;  Mesh complex.
    (setf token (read stream))
    (unless (string= token "SIMPLICIALCOMPLEX")
      (error "Expected SimplicialComplex instead of ~s." token))
    (read-simplicial-complex stream vertex-table :complex mesh)
    ;;  End of file.
    (setf token (read stream nil '%eof%))
    (unless (eql token '%eof%)
      (warn "Unexpected data instead of end-of-file. ~s" token))
    mesh))  

