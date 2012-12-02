;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;				    AVL trees
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; avl.lisp,v 1.6 1994/10/24 14:23:30 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.6")

;; FIXTHIS:  This is a stupid place for this code

(defmacro choose (seq (var n . options) &body body)
  (cond ((%getf options :allow-repeats)
	 `(%choose-repeats ,seq ,n #'(lambda (,var) ,@body)))
	(t `(%choose ,seq ,n #'(lambda (,var) ,@body)))))

(defgeneric %choose (variables number function)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod %choose ((vars list) n fn)
  (unless (or (null n) (cl:integerp n))
    (error "Invalid count argument to CHOOSE: ~D" n))
  (labels ((pick (vars n so-far)
	     (declare (fixnum n))
	     (cond ((cl:zerop n)
		    (%funcall fn so-far))
		   (t (pick (rest vars) (cl:1- n) (cons (first vars) so-far))
		      (if (> (length vars) n)
			  (pick (rest vars) n so-far)))))
	   (pick-null (vars so-far)
	     (when vars
	       (let ((new-so-far (cons (first vars) so-far)))
 	         (%funcall fn new-so-far)
		 (pick-null (rest vars) new-so-far)
		 (when (rest vars)
		   (pick-null (rest vars) so-far))))))
    (let ((len (length vars)))
      (declare (fixnum len))
      (cond ((null n)
	     (%funcall fn nil)
	     (pick-null vars ()))
	    ((cl:> n len)
	     (error "Not that many elements in vars"))
	    ((or (cl:= n len) (cl:zerop len))
	     (%funcall fn vars))
	    ((cl:minusp n)
	     (pick vars (cl:+ len n) ()))	   
	    (t (pick vars n ()))))))

(defgeneric %choose-repeats (variables number function)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod %choose-repeats ((vars list) n fn)
  (unless (or (null n) (cl:integerp n))
    (error "Invalid count argument to CHOOSE: ~D" n))
  (labels ((pick (vars n so-far)
	     (declare (fixnum n))
	     (cond ((cl:zerop n)
		    (%funcall fn so-far))
		   (t (loop while vars do
                            (pick vars (cl:1- n) (cons (first vars) so-far))
                            (setq vars (rest vars)))))))
    (let ((len (length vars)))
      (declare (fixnum len))
      (cond ((cl:> n len)
	     (error "Not that many elements in vars"))
	    ((cl:minusp n)
	     (pick vars (cl:+ len n) ()))	   
	    (t (pick vars n ()))))))

;; FIXTHIS: At some point put in code for even and odd permutations.
(defmacro permute (seq (var . options) &body body)
  (declare (ignore options))
  `(permute% ,seq #'(lambda (,var) ,@body)))

(defgeneric permute% (sequence function)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod permute% ((seq list) fun)
  (labels ((pick (vars so-far)
	     (cond ((null vars) (%funcall fun so-far))
		   (t (loop for v in vars
			    do (pick (remove v vars) (cons v so-far)))))))
    (pick seq nil)))

(defun %partition1 (n fun)
  (labels ((part-int (n min list) 
             (declare (type fixnum n))
             (cond ((cl:zerop n) (funcall fun list))
                   (t (loop for i fixnum upfrom min below (cl:+ 1 n)
                            do (part-int (cl:- n i) i (cons i list)))))))
    (part-int n 1 nil)
    (values)))

;; N is the number to be partitioned, FUN is the function to apply to
;; the partition.

;; NUM-PARTS is the number of parts to be used in the partitioning.
;; If negative, then any number of parts can be used.
;; MAXIMUM-PART is the maximum size of any component in the partition. 

(defun %partition2 (n fun num-parts minimum-part maximum-part distinct?)
  (labels ((part-int (n min parts list)
	     (declare (fixnum n parts))
             (cond ((cl:zerop n)
		    (if (not (cl:plusp parts)) (funcall fun list)))
		   ((or (cl:minusp n) (cl:zerop parts)))
                   (t (loop for i fixnum upfrom min below maximum-part
                            do (part-int (cl:- n i)
					 (if distinct? (cl:1+ i) i)
					 (cl:-  parts 1)
                                         (cons i list)))))))
    (part-int n minimum-part num-parts nil)
    (values)))

(defmacro partition ((var n . options) &body body)
  (let ((num-parts (or (getf options :number-of-parts) -1))
	(minimum-part (or (getf options :minimum-part) 1))
	(maximum-part `(cl:1+ ,(or (getf options :maximum-part) '.number.))))
    (loop for (keyword nil) on options by #'cddr
	  with bad-keywords
	  do (unless (member keyword
			     '( :number-of-parts :minimum-part :maximum-part
			       :distinct?))
	       (push keyword bad-keywords))
	  finally (when bad-keywords
		    (error "Invalid options to partition: ~S"
			   bad-keywords)))
    (cond ((and (null num-parts)
		(null maximum-part))
           `(%partition1 ,n #'(lambda (,var) ,@body)))
          (t `(let ((.number. ,n))
               (%partition2 .number. #'(lambda (,var) ,@body)
                ,num-parts ,minimum-part ,maximum-part
                ,(getf options :distinct?)))))))

#| Partition Demonstation routines |
(defun part-count (n)
  (let ((cnt 0))
    (partition (l n)
      (declare (ignore l))
      (incf cnt))
    cnt))
||#

(defmacro map-over-tree (node (root . options) &body body)
  (let ((collection-fun (or (%getf options :collection-fun) 'identity))
	(breadth-first? (%getf options :breadth-first?))
	(depth-first? (%getf options :depth-first?)))
    (when (and breadth-first? depth-first?)
      (error "Can't specify both breadth and depth first in MAP-OVER-TREE"))
    (cond (breadth-first?
	   `(let ((.collections. (list ,root))
		  .temp. .new-collections.)
             (loop while .collections. do
              (loop for ,node in .collections. do
               (macrolet ((terminate-branch ()
                            `(return-from .mapper.block. nil)))
                 (block .mapper.block.
                   ,@body
                   (when (cl:listp (setq .temp. (,collection-fun ,node)))
                     (setq .new-collections.
                           (nconc .new-collections. (copy-list .temp.)))))))
              (setq .collections. .new-collections.)
              (setq .new-collections. nil))))
	  (depth-first?
	   `(labels ((mapper.fn (,node)
                      (macrolet ((terminate-branch ()
                                   `(return-from .mapper.block. nil)))
                        (block .mapper.block.
                          ,@body
                          (let ((.collection. (,collection-fun ,node)))
                            (when (cl:listp .collection.) 
                              (loop for .node. in .collection.
                                    do (mapper.fn .node.))))))))
             (mapper.fn ,root)))
	  (t (error "Must specify either breadth or depth first in MAP-OVER-TREE")))))	     

;; Need to do the non-mutating version also.  I think this can be done
;; by just changin update-node...

;; This code comes is derived from code that was originally written by
;; Bruce Donald.

;; AVL trees

(defclass avl-tree (has-comparison)
  ((root
    :initform nil
    :accessor avl-root)))

(defclass avl-node (set-element)
  ((left
    :initform nil
    :initarg :left
    :accessor avl-left)
   (right
    :initform nil
    :initarg :right
    :accessor avl-right)
   (balance
    :initform 0
    :initarg balance
    :accessor avl-balance)))

(defclass avl-tree-elements-as-singletons (set-elements-as-singletons)
  ())

(defclass avl-node-as-pairs (set-element2 avl-node)
  ())

(defclass avl-tree-elements-as-pairs (set-elements-as-pairs)
  ())

(defclass simple-avl-tree (avl-tree avl-tree-elements-as-singletons)
  ())

(defclass avl-tree-of-pairs (avl-tree avl-tree-elements-as-pairs)
  ())

(defgeneric avl-size (tree)
  (:documentation
   "Return the size of the avl-tree."))

(defmethod avl-size ((tree avl-tree))
  (let ((root (avl-root tree)))
    (if root (avl-size root)
	0)))

(defmethod avl-size ((node avl-node))
  (let ((left (avl-left node))
	(right (avl-right node)))
    (1+ (cl:+ (if left (avl-size left) 0)
              (if right (avl-size right) 0)))))

(defmethod print-object ((tree avl-tree) stream)
  (format stream "#<AVL tree: ~D elts>" (avl-size tree)))

(defmethod print-object ((node avl-node) stream)
  (format stream "<AVL~[-~;=~;+~]: ~S>"
	  (1+ (avl-balance node)) (element-key node)))

;;; This is for debugging
#+ignore
(defmethod pretty-print-object ((node avl-node) &optional (stream *standard-output*))
  (labels ((indent (n)
	     (loop for i below n do (princ " " stream)))
	   (pp (node indent)
	     (when (avl-left node)
	       (indent indent)
	       (format stream  "L: ~S~%" (avl-left node))
	       (pp (avl-left node) (cl:+ 2 indent)))
	     (when (avl-right node)
	       (indent indent)
	       (format stream  "R: ~S~%" (avl-right node))
	       (pp (avl-right node) (cl:+ 2 indent)))))
    (format stream "~&Root: ~S~%" node)
    (pp node 2)))

(defgeneric update-node (node balance left right &rest args)
  (:documentation
   "This is used to pudate a node with new information since we don't
know all the information that could be stored in a node we've assumed
they can all be lumped into args."))

(defmethod update-node ((node avl-node) balance left right &rest args)
  (declare (ignore args))
  (setf (avl-balance node) balance)
  (setf (avl-left node) left)
  (setf (avl-right node) right)
  node)

(defgeneric empty? (tree)
  (:documentation
   "Return true if the tree is empty.")
  (:method ((tree avl-tree))
    (null (avl-root tree))))

(defgeneric avl-height (tree)
  (:documentation
   "This determines the height of an AVL tree and also checks if your
tree is out of balance or 'Koyaanisquatsi' in Hopi Indian. Actual
height difference should be the same as the balance value, and should
be in the range {-1,0,1}."))

(defmethod avl-height ((tree avl-tree))
  (let ((root (avl-root tree)))
    (if root (avl-height root) 0)))

(defmethod avl-height ((node avl-node))
  (let ((hl (if (avl-left node) (avl-height (avl-left node))
		0))
	(hr (if (avl-right node) (avl-height (avl-right node))
		0)))
    (cond ((not (eql (cl:- hr hl) (avl-balance node)))
	   (format t "~
              The actual height difference ~S does not agree with the ~%~
              balance entry ~S for node ~S"
		   (cl:- hr hl) (avl-balance node) node))
	  ((cl:> (cl:abs (avl-balance node)) 1)
	   (format t "Node ~S is Koyaanisquatsi, its balance value is ~S"
		   node (avl-balance node))))
    (cl:1+ (cl:max hl hr))))

(defgeneric left-most (node)
  (:documentation
   "Return true if the node is the left most node of the tree."))

(defmethod left-most ((node avl-node))
  (labels ((find-left-most (node)
	     (cond ((null (avl-left node)) node)
		   (t (find-left-most (avl-left node))))))
    (find-left-most node)))

(defgeneric avl-maximum (tree)
  (:documentation
   "The purpose of this method is unknown.")
  (:method ((tree avl-tree))
    (left-most (avl-root tree))))

(defgeneric map-over-each-node (tree function)
  (:documentation
   "Map over each node of the tree applying the function."))

(defmethod map-over-each-node ((tree avl-tree) function)
  (labels ((map-over (node)
	     (unless (null (avl-left node))
	       (map-over (avl-left node)))
	     (%funcall function node)
	     (unless (null (avl-right node))
	       (map-over (avl-right node)))))
    (let ((root (avl-root tree)))
      (when root 
	(map-over root)))))

(defgeneric make-generator (tree)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod make-generator ((tree avl-tree))
  (let (stack)
    (macrolet ((current-state () `(first (first stack)))
	       (set-current-state (state) `(setf (first (first stack)) ,state))
	       (current-node () `(rest (first stack)))
	       (new-node (node) `(push (cons :left ,node) stack)))
      (labels ((scan ()
		 (cond ((null stack) nil)
		       ((eql (current-state) :left) 
			(cond ((null (avl-left (current-node))) 
			       (set-current-state :right)
			       (current-node))
			      (t (set-current-state :here)
				 (new-node (avl-left (current-node)))
				 (scan))))
		       ((eql (current-state) :here)
			(set-current-state :right)
			(current-node))
		       (t ;; (eql (current-state) :right)
			(cond ((null (avl-right (current-node)))
			       (pop stack)
			       (scan))
			      (t (new-node
				  (prog1 (avl-right (current-node))
				    (pop stack)))
				 (scan))))
		       )))
	(new-node (avl-root tree))
	#'scan))))    

(defgeneric right-most (node)
  (:documentation
   "Return true if the node is the right most of the tree."))

(defmethod right-most ((node avl-node))
  (labels ((find-right-most (node)
	     (cond ((null (avl-right node)) node)
		   (t (find-right-most (avl-right node))))))
    (find-right-most node)))

(defgeneric avl-minimum (tree)
  (:documentation
   "The purpose of this method is unknown.")
  (:method ((tree avl-tree))
    (right-most (avl-root tree))))

(defgeneric avl-next (key tree-or-node)
  (:documentation
   "Return the next item of the tree or node."))

(defmethod avl-next (key (tree avl-tree))
  (avl-next key (avl-root tree)))

(defmethod avl-next (key (node avl-node))
  (labels ((next-loop (node)
	     (and node
		  (if (not (> node key))
		      (next-loop (avl-right node))
		      (or (next-loop (avl-left node))
			  node)))))
    (next-loop node)))

(defgeneric avl-previous (key tree-or-node)
  (:documentation
   "Return the next item of the tree or node."))

(defmethod avl-previous (key (tree avl-tree))
  (avl-previous key (avl-root tree)))

(defmethod avl-previous (key (node avl-node))
  (labels ((next-loop (node)
	     (and node
		  (if (not (> key node))
		      (next-loop (avl-left node))
		      (or (next-loop (avl-right node))
			  node)))))
    (next-loop node)))

(defgeneric balance-right (node height-change)
  (:documentation
   "Balance a TREE that is right-Koyaanisquatsi, i.e. the right
subtree is 2 levels higher than the left subtree. HEIGHT-CHANGE is the
height of TREE relative to its value before the delete/insert
operation. Balance-right returns a node and the height of that node
relative to the original height of TREE."))

(defmethod balance-right ((node avl-node) height-change)
  (let ((r (avl-right node)))
    (cond ((cl:plusp (avl-balance r))
	   (setq node (update-node node 0 (avl-left node) (avl-left r)))
	   (setq r (update-node r 0 node (avl-right r)))
	   (values r (1- height-change)))
	  ((cl:zerop (avl-balance r))
	   (setq node (update-node node 1 (avl-left node) (avl-left r)))
	   (setq r (update-node r -1 node (avl-right r)))
	   (values r height-change))
	  (t (let ((lr (avl-left r)))
	       (setq r (update-node r (if (cl:minusp (avl-balance lr)) 1 0)
				    (avl-right lr) (avl-right r)))
	       (setq node (update-node node
				       (if (cl:plusp (avl-balance lr)) -1 0)
				       (avl-left node) (avl-left lr)))
	       (setq lr (update-node lr 0 node r))
	       (values lr (1- height-change)))))))

(defgeneric balance-left (node height-change)
  (:documentation
   "Balance a TREE that is left-Koyaanisquatsi, i.e. the left subtree
is 2 levels higher than the right subtree. HEIGHT-CHANGE is the height
of TREE relative to its value before the delete/insert operation.
Balance-left returns a node and the height of that node relative to
the original height of TREE."))

(defmethod balance-left ((node avl-node) height-change)
  (let ((l (avl-left node)))
    (cond ((cl:minusp (avl-balance l))
	   (setq node (update-node node 0 (avl-right l) (avl-right node)))
	   (setq l (update-node l 0 (avl-left l) node))
	   (values l (1- height-change)))
	  ((cl:zerop (avl-balance l))
	   (setq node (update-node node -1 (avl-right l) (avl-right node)))
	   (setq l (update-node l 1 (avl-left l) node))
	   (values l height-change))
	  (t (let ((rl (avl-right l)))
	       (setq l (update-node l (if (cl:plusp (avl-balance rl)) -1 0)
				    (avl-left l) (avl-left rl)))
	       (setq node (update-node node (if (cl:minusp (avl-balance rl))
						1 0)
				       (avl-right rl) (avl-right node)))
	       (setq rl (update-node rl 0 l node))
	       (values rl (1- height-change)))))))

(defgeneric insert (item tree &rest args)
  (:documentation
   "The first interesting operation on AVL trees. This inserts THING
into the tree and returns a new tree and an integer which is the
change in height of the tree."))

(defmethod insert (item (tree avl-tree) &rest args)
  (labels
      ((avl-insert (node)
	 (if (null node)
	     (values (%apply #'make-element tree item args) 1)
	     (cond ((= item node)
		    (values ;; Just update the value field if necessary
                     (%apply #'update-node node (avl-balance node)
			     (avl-left node) (avl-right node)
			     args)
                     0))
		   ((> item node) 
		    (multiple-value-bind (subtree height-change)
			(avl-insert (avl-right node)) 
		      (setq node 
			    (update-node node
					 (cl:+ (avl-balance node)
                                               height-change)
					 (avl-left node) subtree))
		      (if (cl:> (avl-balance node) 1)
			  (balance-right node 1)
			  (values node (if (cl:plusp (avl-balance node))
					   height-change
					   0)))))
		   (t (multiple-value-bind (subtree height-change)
			  (avl-insert (avl-left node))
			(setq node
			      (update-node node
					   (cl:- (avl-balance node)
                                                 height-change)
					   subtree
					   (avl-right node)))
			(if (cl:< (avl-balance node) -1)
			    (balance-left node 1)
			    (values node
				    (if (cl:minusp (avl-balance node))
					height-change
					0)))))))))
    (setf (avl-root tree) (avl-insert (avl-root tree)))
    tree))

(defgeneric delete-head (tree)
  (:documentation
   "This returns the head (leftmost element) in the tree, and removes
it from the tree. Useful for implementing priority queues as AVL
trees. Values returned are the value of the leftmost element, the
modified tree, and the change in height of the tree."))

(defmethod delete-head ((tree avl-tree))  
  (multiple-value-bind (tail new-root height-change)
      (delete-head (avl-root tree))
    (setf (avl-root tree) new-root)
    (values tail height-change)))

(defmethod delete-head ((node avl-node))
  (cond ((null node) nil)
	((null (avl-left node))
	 (values node (avl-right node) -1))
	(t (multiple-value-bind (head-value subnode height-change)
	       (delete-head (avl-left node))
	     (setq node (update-node node (cl:- (avl-balance node)
                                                height-change)
				     subnode (avl-right node)))
	     (if (> (avl-balance node) 1)
		 (multiple-value-setq (node height-change)
		   (balance-right node 0))
		 (if (not (cl:zerop (avl-balance node)))
		     (setq height-change 0)))
	     (values head-value node height-change))))) 

(defgeneric delete-tail (tree)
  (:documentation
   "This returns the tail (rightmost element) in the tree, and removes
it from the tree.  Values returned are the value of the rightmost
element, the modified tree, and the change in height of the tree."))

(defmethod delete-tail ((tree avl-tree))
  (multiple-value-bind (tail new-root height-change)
      (delete-tail (avl-root tree))
    (setf (avl-root tree) new-root)
    (values tail height-change)))

(defmethod delete-tail ((node avl-node))
  (cond ((null node) nil)
	((null (avl-right node))
	 (values node (avl-left node) -1))
	(t (multiple-value-bind (tail-value subnode height-change)
	       (delete-tail (avl-right node))
	     (setq node (update-node node (cl:+ (avl-balance node)
                                                height-change)
				     (avl-left node) subnode))
	     (if (cl:< (avl-balance node) -1)
		 (multiple-value-setq (node height-change)
		   (balance-left node 0))
		 (if (not (cl:zerop (avl-balance node)))
		     (setq height-change 0)))
	     (values tail-value node height-change)))))

(defgeneric erase-node (node)
  (:documentation
   "This gets rid of a value that has been found in the tree. NODE is
the node containing the value. If the right subtree of NODE is higher
than its left, replace the value of NODE with the value of the
left-most leaf of the right subtree, and remove this leaf from the
right subtree. Otherwise replace NODE's value with the value of the
right-most leaf of the left subtree of NODE, and remove this leaf from
the left subtree."))

(defmethod erase-node ((node avl-node))
  (cond ((and (null (avl-left node)) (null (avl-right node)))
	 (values nil -1))
	((cl:plusp (avl-balance node))
	 (multiple-value-bind (head-node subtree height-change)
	     (delete-head (avl-right node))
	   (setq node (update-node head-node
				   (cl:+ (avl-balance node)
                                         height-change)
				   (avl-left node) subtree))
	   (values node height-change)))
	(t (multiple-value-bind (tail-node subtree height-change)
	       (delete-tail (avl-left node))
	     (setq node (update-node tail-node
				     (cl:- (avl-balance node) height-change)
                                     subtree (avl-right node)))
	     (values node (if (cl:zerop (avl-balance node))
			      height-change 0))))))

(defmethod delete (item (tree avl-tree) &rest rest)
  "This deletes an entry from an AVL tree."
  (declare (ignore rest))
  (let ((root (avl-root tree)))
    (labels
        ((delete-left (node parent)
           (cond ((null node)
                  (values nil 0))
                 ((= item node)
                  (multiple-value-bind (new-left height-change) (erase-node node)
                    (setf (avl-left parent) new-left)
                    (values new-left height-change)))
                 (t (avl-delete node))))
         (delete-right (node parent)
           (cond ((null node)
                  (values nil 0))
                 ((= item node)
                  (multiple-value-bind (new-right height-change)
                      (erase-node node)
                    (setf (avl-right parent) new-right)
                    (values new-right height-change)))
                 (t (avl-delete node))))
         (avl-delete (node)
           (cond ((> item node)
                  (multiple-value-bind (subtree height-change)
                      (delete-right (avl-right node) node)
                    (setq node (update-node node
                                            (cl:+ (avl-balance node)
						  height-change)
                                            (avl-left node) subtree))
                    (if (cl:< (avl-balance node) -1)
                        (balance-left node 0)
                        (values node (if (cl:zerop (avl-balance node))
                                         height-change 0)))))
                 (t (multiple-value-bind (subtree height-change)
                        (delete-left (avl-left node) node)
                      (setq node (update-node node
                                              (cl:- (avl-balance node)
						    height-change)
                                              subtree (avl-right node)))
                      (if (cl:> (avl-balance node) 1)
                          (balance-right node 0)
                          (values node (if (cl:zerop (avl-balance node))
                                           height-change 0))))))))
      (cond ((null root)
	     (values nil 0))
	    ((= item root)
	     (setf (avl-root tree) (erase-node root)))
	    ((> item root)
	     (delete-right (avl-right root) root))
	    (t (delete-left (avl-left root) root)))
      tree)))

(defmethod member (item (tree avl-tree) &rest rest)
  (declare (ignore rest))
  (labels ((search-node (node)
	     (cond ((null node) nil)
		   ((= item node) node)
		   ((> item node)
		    (search-node (avl-right node)))
		   (t (search-node (avl-left node))))))
    (search-node (avl-root tree))))

(defmethod make-element ((tree avl-tree-elements-as-singletons) key &rest rest)
  (declare (ignore rest))
  (make-instance 'avl-node :domain tree :key key))

(defmethod make-element ((tree avl-tree-elements-as-pairs) key &rest rest)
  (make-instance 'avl-node-as-pairs :domain tree :key key :value (first rest)))
