;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-INTERNALS; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Graph Formatting
;;;   Created: 2002-08-13
;;;   License: LGPL (See file COPYING for details).
;;;       $Id: graph-formatting.lisp,v 1.18 2006/04/10 09:48:40 crhodes Exp $
;;; ---------------------------------------------------------------------------

;;;  (c) copyright 2002 by Gilbert Baumann
;;;  (c) copyright 2005 by Robert P. Goldman

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.
 
(in-package :clim-internals)

;;;; Notes

;; - Now what exactly are layout-graph-nodes and layout-graph-edges
;;   supposed to do? If LAYOUT-GRAPH-NODES is only responsible for
;;   laying out the node output records, why does it get the
;;   arc-drawer? If it should also draw the edges why then is there
;;   the other function? --GB 2002-08-13

;; - There is this hash table initarg to graph-output-records? Should
;;   FORMAT-GRAPH-FROM-ROOTS pass a suitable hash table for the given
;;   'duplicate-test', if so why it is passed down and why is it not
;;   restricted to the set of hash test functions? --GB 2002-08-13

;; - What is the purpose of (SETF GRAPH-NODE-CHILDREN) and
;;   (SETF GRAPH-NODE-PARENTS)? --GB 2002-08-14

;; - FORMAT-GRAPH-FROM-ROOTS passes the various options on to the
;;   instantiation of the graph-output-record class, so that the
;;   individual classes can choose appropriate defaults. --GB 2002-08-14

;; - In the same spirit, a non given ARC-DRAWER option is passed as it
;;   is, that is being NIL, to LAYOUT-GRAPH-EDGES so that the concrete
;;   graph-output-record can choose a default. --GB 2002-08-14

;;;; Declarations

;; format-graph-from-roots

(defgeneric graph-root-nodes (graph-output-record))
(defgeneric (setf graph-root-nodes) (new-value graph-output-record))
(defgeneric generate-graph-nodes (graph-output-record stream root-objects
                                  object-printer inferior-producer
                                  &key duplicate-key duplicate-test))
(defgeneric layout-graph-nodes (graph-output-record stream arc-drawer arc-drawing-options))
(defgeneric layout-graph-edges (graph-output-record stream arc-drawer arc-drawing-options))
;;; NOTE: Which calls which? --GB 2002-08-13

(defgeneric graph-node-parents (graph-node-record))
(defgeneric (setf graph-node-parents) (new-value graph-node-record))
(defgeneric graph-node-children (graph-node-record))
(defgeneric (setf graph-node-children) (new-value graph-node-record))
(defgeneric graph-node-object (graph-node-record))

;;;; Machinery for graph types

(defconstant +built-in-graph-types+
  '(:tree :directed-graph :digraph :directed-acyclic-graph :dag)
  "List of graph types builtin by CLIM.")

(defvar *graph-types-hash*
  (make-hash-table :test #'eq)
  "A hash table which maps from symbols that name graph-types to class names; Filled by CLIM:DEFINE-GRAPH-TYPE")

(defun register-graph-type (graph-type class)
  "Registers a new graph-type."
  (setf (gethash graph-type *graph-types-hash*) class))

(defun find-graph-type (graph-type)
  "Find the a graph type; when it does not exist barks at the user."
  (or (gethash graph-type *graph-types-hash*)
      (progn
        (cerror "Specify another graph type to use instead."
                "There is no such graph type defined: ~S.~%The defined ones are: ~{~S~^, ~@_~}."
                graph-type
                (loop for key being each hash-key of *graph-types-hash*
                      collect key))
        ;; accept anyone?
        (princ "Graph Type? ")
        (find-graph-type (read)))))

(defmacro define-graph-type (graph-type class)
  (check-type graph-type symbol)
  (check-type class symbol)
  (unless (eq *package* (find-package :climi))
    (when (member graph-type +built-in-graph-types+)
      (cerror "Do it anyway" "You are about to redefine the builtin graph type ~S."
              graph-type)))
  ;; Note: I would really like this to obey to package locks and stuff.
  `(progn
    (register-graph-type ',graph-type ',class)
    ',graph-type))

(define-graph-type :tree tree-graph-output-record)
(define-graph-type :directed-acyclic-graph dag-graph-output-record)
(define-graph-type :dag dag-graph-output-record)
(define-graph-type :directed-graph digraph-graph-output-record)
(define-graph-type :digraph digraph-graph-output-record)

;;;; Entry

(defun format-graph-from-roots (root-objects object-printer inferior-producer
                                &rest graph-options
                                &key stream orientation cutoff-depth
                                     merge-duplicates duplicate-key duplicate-test
                                     generation-separation
                                     within-generation-separation
                                     center-nodes
                                     (arc-drawer #'clim-internals::standard-arc-drawer)
                                     arc-drawing-options
                                     graph-type (move-cursor t)
                                &allow-other-keys)
  (declare (ignore orientation generation-separation within-generation-separation center-nodes))
  ;; Mungle some arguments
  (check-type cutoff-depth (or null integer))
  (check-type root-objects sequence)
  (setf stream (or stream *standard-output*)
        graph-type (or graph-type (if merge-duplicates :digraph :tree))
        duplicate-key (or duplicate-key #'identity)
        duplicate-test (or duplicate-test #'eql) )

  ;; I'm not sure what to do here.  Saying you want a tree, but want
  ;; duplicates merged seems wrong.  OTOH, if you go out of your way
  ;; to do it, at your own risk, is it our place to say "no"?
  ;; [2005/08/11:rpg]
;;;  (when (and (eq graph-type :tree) merge-duplicates)
;;;    (cerror "Substitute NIL for merge-duplicates"
;;;	    "Merge duplicates specified to be true when using :tree layout.")
;;;    (setf merge-duplicates nil))
  
  ;; clean the options
  (remf graph-options :stream)
  (remf graph-options :duplicate-key)
  (remf graph-options :duplicate-test)
  (remf graph-options :arc-drawer)
  (remf graph-options :arc-drawing-options)
  (remf graph-options :graph-type)
  (remf graph-options :move-cursor)
  
  (multiple-value-bind (cursor-old-x cursor-old-y)
      (stream-cursor-position stream)
    (let ((graph-output-record
           (labels ((cont (stream graph-output-record)
                      (with-output-recording-options (stream :draw nil :record t)
                        (generate-graph-nodes graph-output-record stream root-objects
                                              object-printer inferior-producer
                                              :duplicate-key duplicate-key
                                              :duplicate-test duplicate-test)
                        (layout-graph-nodes graph-output-record stream arc-drawer arc-drawing-options)
                        (layout-graph-edges graph-output-record stream arc-drawer arc-drawing-options)) ))
             (apply #'invoke-with-new-output-record stream
                    #'cont
                    (find-graph-type graph-type)
		    nil
		    ;; moved to local variable... [2005/07/25:rpg]
                    ;; :hash-table (make-hash-table :test duplicate-test)
		    graph-options 
		    ))))
      (setf (output-record-position graph-output-record)
            (values cursor-old-x cursor-old-y))
      (with-output-recording-options (stream :draw t :record nil)
        (replay graph-output-record stream))               
      (when move-cursor
        (setf (stream-cursor-position stream)
              (values (bounding-rectangle-max-x graph-output-record)
                      (bounding-rectangle-max-y graph-output-record))))
      graph-output-record)))

(defun format-graph-from-root (root &rest rest)
  (apply #'format-graph-from-roots (list root) rest))

;;;; Graph Output Records

(defclass standard-graph-output-record (graph-output-record
                                        standard-sequence-output-record)
     ((orientation
       :initarg :orientation
       :initform :horizontal)
      (center-nodes
       :initarg :center-nodes
       :initform nil)
      (cutoff-depth
       :initarg :cutoff-depth
       :initform nil)
      (merge-duplicates
       :initarg :merge-duplicates
       :initform nil)
      (generation-separation
       :initarg :generation-separation
       :initform '(4 :character))
      (within-generation-separation
       :initarg :within-generation-separation
       :initform '(1/2 :line))
      ;; removed HASH-TABLE slot and stuffed it into
      ;; GENERATE-GRAPH-NODES method definition [2005/07/25:rpg]
      (root-nodes
       :accessor graph-root-nodes)
      ))

(defclass tree-graph-output-record (standard-graph-output-record)
     ())

;;;(defmethod initialize-instance :after ((obj tree-graph-output-record) &key merge-duplicates)
;;;  (when merge-duplicates
;;;    (warn "Cannot use a TREE layout for graphs while merging duplicates.")))

(defclass dag-graph-output-record (standard-graph-output-record)
  (
   ))

(defclass digraph-graph-output-record (standard-graph-output-record)
  ())

;;;; Nodes

(defclass standard-graph-node-output-record (graph-node-output-record
                                             standard-sequence-output-record)
  ((graph-parents
    :initarg :graph-parents
    :initform nil
    :accessor graph-node-parents)
   (graph-children
    :initarg :graph-children
    :initform nil
    :accessor graph-node-children)
   (edges-from :initform (make-hash-table))
   (edges-to   :initform (make-hash-table))
   (object
    :initarg :object
    :reader graph-node-object)
   ;; internal slots for the graph layout algorithmn
   (minor-size
    :initform nil
    :accessor graph-node-minor-size
    :documentation "Space requirement for this node and its children. Also used as a mark.") ))

;;;;

;;; Modified to make this obey the spec better by using a hash-table
;;; for detecting previous nodes only when the duplicate-test argument
;;; permits it.  [2005/08/10:rpg]
(defmethod generate-graph-nodes ((graph-output-record standard-graph-output-record)
                                 stream root-objects
                                 object-printer inferior-producer
                                 &key duplicate-key duplicate-test)
  (with-slots (cutoff-depth merge-duplicates) graph-output-record
    (let* ((hash-table (when (and merge-duplicates (member duplicate-test (list #'eq #'eql #'equal #'equalp)))
			 (make-hash-table :test duplicate-test)))
	   node-list 
	   (hashed hash-table))
      (labels
	  ((previous-node (obj)
	     ;; is there a previous node for obj?  if so, return it.
	     (when merge-duplicates
	       (if hashed
		   (locally (declare (type hash-table hash-table))
		     (gethash obj hash-table))
		 (cdr (assoc obj node-list :test duplicate-test)))))
	   ((setf previous-node) (val obj)
	     (if hashed
		 (locally (declare (type hash-table hash-table))
		   (setf (gethash obj hash-table) val))
		 (setf node-list (push (cons obj val) node-list))))
	   (traverse-objects (node objects depth)
	     (unless (and cutoff-depth (>= depth cutoff-depth))
	       (remove nil
		       (map 'list
			 (lambda (child)
			   (let* ((key (funcall duplicate-key child))
				  (child-node (previous-node key)))
			     (cond (child-node
				    (when node
				      (push node (graph-node-parents child-node)))
				    child-node)
				   (t
				    (let ((child-node
					   (with-output-to-output-record
					       (stream 'standard-graph-node-output-record new-node
						       :object child)
					     (funcall object-printer child stream))))
				      (when merge-duplicates
					(setf (previous-node key) child-node)
					;; (setf (gethash key hash-table) child-node)
					)
				      (when node
					(push node (graph-node-parents child-node)))
				      (setf (graph-node-children child-node)
					(traverse-objects child-node
							  (funcall inferior-producer child)
							  (+ depth 1)))
				      child-node)))))
			 objects)))))
	;;
	(setf (graph-root-nodes graph-output-record)
	  (traverse-objects nil root-objects 0))
	(values)))))

(defun traverse-graph-nodes (graph continuation)
  ;; continuation: node x children x cont -> some value
  (let ((hash (make-hash-table :test #'eq)))
    (labels ((walk (node)
               (unless (gethash node hash)
                 (setf (gethash node hash) t)
                 (funcall continuation node (graph-node-children node) #'walk))))
      (funcall continuation graph (graph-root-nodes graph) #'walk))))

(defmethod layout-graph-nodes ((graph-output-record tree-graph-output-record)
                               stream arc-drawer arc-drawing-options)
  ;; work in progress! --GB 2002-08-14
  (declare (ignore arc-drawer arc-drawing-options))
  (with-slots (orientation center-nodes generation-separation within-generation-separation root-nodes) graph-output-record
    (check-type orientation (member :horizontal :vertical)) ;xxx move to init.-inst.
    ;; here major dimension is the dimension in which we grow the
    ;; tree.
    (let ((within-generation-separation (parse-space stream within-generation-separation
                                                     (case orientation
                                                       (:horizontal :vertical)
                                                       (:vertical :horizontal))))
          (generation-separation (parse-space stream generation-separation orientation)))
      ;; generation sizes is an adjustable array that tracks the major
      ;; dimension of each of the generations [2005/07/18:rpg]
      (let ((generation-sizes (make-array 10 :adjustable t :initial-element 0)))
        (labels ((node-major-dimension (node)
                   (if (eq orientation :vertical)
                       (bounding-rectangle-height node)
                       (bounding-rectangle-width node)))
                 (node-minor-dimension (node)
                   (if (eq orientation :vertical)
                       (bounding-rectangle-width node)
                       (bounding-rectangle-height node)))
		 ;; WALK returns a node minor dimension for the node,
		 ;; AFAICT, allowing space for that node's children
		 ;; along the minor dimension. [2005/07/18:rpg]
                 (walk (node depth)
                   (unless (graph-node-minor-size node)
                     (when (>= depth (length generation-sizes))
                       (setf generation-sizes (adjust-array generation-sizes (ceiling (* depth 1.2)) :initial-element 0)))
                     (setf (aref generation-sizes depth)
                           (max (aref generation-sizes depth) (node-major-dimension node)))
                     (setf (graph-node-minor-size node) 0)
                     (max (node-minor-dimension node)
                          (setf (graph-node-minor-size node)
                                (let ((sum 0) (n 0))
                                  (map nil (lambda (child)
                                             (let ((x (walk child (+ depth 1))))
                                               (when x
                                                 (incf sum x)
                                                 (incf n))))
                                       (graph-node-children node))
                                  (+ sum
                                     (* (max 0 (- n 1)) within-generation-separation))))))))
          (map nil #'(lambda (x) (walk x 0)) root-nodes)
          (let ((hash (make-hash-table :test #'eq)))
            (labels ((foo (node majors u0 v0)
                       (cond ((gethash node hash)
                              v0)
                             (t
                              (setf (gethash node hash) t)
                              (let ((d (- (node-minor-dimension node)
                                          (graph-node-minor-size node))))
                                (let ((v (+ v0 (/ (min 0 d) -2))))
                                  (setf (output-record-position node)
                                        (if (eq orientation :vertical)
                                            (transform-position (medium-transformation stream) v u0)
                                            (transform-position (medium-transformation stream) u0 v)))
                                  (add-output-record node graph-output-record))
                                ;;
                                (let ((u (+ u0 (car majors)))
                                      (v (+ v0 (max 0 (/ d 2))))
                                      (firstp t))
                                  (map nil (lambda (q)
                                             (unless (gethash q hash)
                                               (if firstp
                                                   (setf firstp nil)
                                                   (incf v within-generation-separation))
                                               (setf v (foo q (cdr majors)
                                                            u v))))
                                           (graph-node-children node)))
                                ;;
                                (+ v0 (max (node-minor-dimension node)
                                           (graph-node-minor-size node))))))))
              ;;
              (let ((majors (mapcar (lambda (x) (+ x generation-separation))
                                    (coerce generation-sizes 'list))))
                (let ((u (+ 0 (car majors)))
                      (v 0))
                  (maplist (lambda (rest)
                             (setf v (foo (car rest) majors u v))
                             (unless (null rest)
                               (incf v within-generation-separation)))
                           (graph-root-nodes graph-output-record)))))))))))

;;;; Edges

(defclass standard-edge-output-record (standard-sequence-output-record)
  ((stream)
   (arc-drawer)
   (arc-drawing-options)
   (from-node :initarg :from-node)
   (to-node :initarg :to-node)))


(defmethod layout-graph-nodes ((graph-output-record dag-graph-output-record)
                               stream arc-drawer arc-drawing-options)
  "This is a first shot at a DAG layout.  First does a TOPO sort that associates 
each node with a depth, then lays out by depth.  Tries to reuse a maximum of the
tree graph layout code.
PRECONDITION:  This code assumes that we have generated only nodes up to the 
cutoff-depth.  GENERATE-GRAPH-NODES seems to obey this precondition."
  (declare (ignore arc-drawer arc-drawing-options))
  (with-slots (orientation center-nodes generation-separation within-generation-separation root-nodes
			   merge-duplicates) graph-output-record
    ;; this code is snarly enough, handling merge-duplicates.  If
    ;; you're not merging duplicates, you're out of luck, at least for
    ;; now... [2005/07/18:rpg]
    (unless merge-duplicates
      (cerror "Set to T and continue?" "DAG graph-layout type only supports merge-duplicates to be T")
      (setf merge-duplicates t))
	      
    (check-type orientation (member :horizontal :vertical)) ;xxx move to init.-inst.

    ;; here major dimension is the dimension in which we grow the
    ;; tree.
    (let ((within-generation-separation (parse-space stream within-generation-separation
                                                     (case orientation
                                                       (:horizontal :vertical)
                                                       (:vertical :horizontal))))
          (generation-separation (parse-space stream generation-separation orientation)))
      ;; generation sizes is an adjustable array that tracks the major
      ;; dimension of each of the generations [2005/07/18:rpg]
      (let ((generation-sizes (make-array 10 :adjustable t :initial-element 0))
	    (visited (make-hash-table :test #'eq))
	    (parent-hash (make-hash-table :test #'eq)))
        (labels ((node-major-dimension (node)
                   (if (eq orientation :vertical)
                       (bounding-rectangle-height node)
                       (bounding-rectangle-width node)))
                 (node-minor-dimension (node)
                   (if (eq orientation :vertical)
                       (bounding-rectangle-width node)
                       (bounding-rectangle-height node)))
		 ;; WALK returns a node minor dimension for the node,
		 ;; AFAICT, allowing space for that node's children
		 ;; along the minor dimension. [2005/07/18:rpg]
                 (walk (node depth &optional parent)
		   (unless (gethash node visited)
		     (setf (gethash node visited) depth)
		     (when parent
		       (setf (gethash node parent-hash) parent))
		     (unless (graph-node-minor-size node)
		       (when (>= depth (length generation-sizes))
			 (setf generation-sizes (adjust-array generation-sizes (ceiling (* depth 1.2))
							      :initial-element 0)))
		       (setf (aref generation-sizes depth)
			     (max (aref generation-sizes depth) (node-major-dimension node)))
		       (setf (graph-node-minor-size node) 0)
		       (max (node-minor-dimension node)
			    (setf (graph-node-minor-size node)
				  (let ((sum 0) (n 0))
				    (map nil (lambda (child)
					       (let ((x (walk child (+ depth 1) node)))
						 (when x
						   (incf sum x)
						   (incf n))))
					 (graph-node-children node))
				    (+ sum
				       (* (max 0 (- n 1)) within-generation-separation)))))))))
          (map nil #'(lambda (x) (walk x 0)) root-nodes)
          (let ((hash (make-hash-table :test #'eq)))
            (labels ((foo (node majors u0 v0)
                       (cond ((gethash node hash)
                              v0)
                             (t
                              (setf (gethash node hash) t)
                              (let ((d (- (node-minor-dimension node)
                                          (graph-node-minor-size node))))
                                (let ((v (+ v0 (/ (min 0 d) -2))))
                                  (setf (output-record-position node)
                                        (if (eq orientation :vertical)
                                            (transform-position (medium-transformation stream) v u0)
                                            (transform-position (medium-transformation stream) u0 v)))
                                  (add-output-record node graph-output-record))
                                ;;
                                (let ((u (+ u0 (car majors)))
                                      (v (+ v0 (max 0 (/ d 2))))
                                      (firstp t))
                                  (map nil (lambda (q)
                                             (unless (gethash q hash)
                                               (if firstp
                                                   (setf firstp nil)
                                                   (incf v within-generation-separation))
                                               (setf v (foo q (cdr majors)
                                                            u v))))
				       ;; when computing the sizes, to
				       ;; make the tree-style layout
				       ;; work, we have to have each
				       ;; node have a unique
				       ;; parent. [2005/07/18:rpg]
				       (remove-if-not #'(lambda (x) (eq (gethash x parent-hash) node))
						      (graph-node-children node))))
                                ;;
                                (+ v0 (max (node-minor-dimension node)
                                           (graph-node-minor-size node))))))))
              ;;
              (let ((majors (mapcar (lambda (x) (+ x generation-separation))
                                    (coerce generation-sizes 'list))))
                (let ((u (+ 0 (car majors)))
                      (v 0))
                  (maplist (lambda (rest)
                             (setf v (foo (car rest) majors u v))
                             (unless (null rest)
                               (incf v within-generation-separation)))
                           (graph-root-nodes graph-output-record)))))))))))



#+ignore
(defmethod layout-graph-edges ((graph-output-record standard-graph-output-record)
                               stream arc-drawer arc-drawing-options)
  (with-slots (root-nodes orientation) graph-output-record
    (let ((hash (make-hash-table)))
      (labels ((walk (node)
		 (unless (gethash node hash)
                   (setf (gethash node hash) t)
                   (dolist (k (graph-node-children node))
                     (with-bounding-rectangle* (x1 y1 x2 y2) node
                       (with-bounding-rectangle* (u1 v1 u2 v2) k
                         (ecase orientation
                           ((:horizontal)
                            (multiple-value-bind (from to) (if (< x1 u1)
                                                               (values x2 u1)
                                                               (values x1 u2))
                              (apply arc-drawer stream node k
                                     from (/ (+ y1 y2) 2)
                                     to   (/ (+ v1 v2) 2)
                                     arc-drawing-options)))
                           ((:vertical)
                            (multiple-value-bind (from to) (if (< y1 v1)
                                                               (values y2 v1)
                                                               (values y1 v2))
                              (apply arc-drawer stream node k
                                     (/ (+ x1 x2) 2) from
                                     (/ (+ u1 u2) 2) to
                                     arc-drawing-options)) ))))
                     (walk k)))))
        (map nil #'walk root-nodes)))))

(defun layout-edges (graph node stream arc-drawer arc-drawing-options)
  (dolist (k (graph-node-children node))
    (layout-edge graph node k stream arc-drawer arc-drawing-options)))

(defun ensure-edge-record (graph major-node minor-node)
  (let ((edges-from (slot-value major-node 'edges-from))
	(edges-to   (slot-value minor-node 'edges-to)))
    (assert (eq (gethash minor-node edges-from)
		(gethash major-node edges-to)))
    (or (gethash minor-node edges-from)
	(let ((record (make-instance 'standard-edge-output-record
				     :from-node major-node :to-node minor-node)))
	  (setf (gethash minor-node edges-from) record
		(gethash major-node edges-to) record)
	  (add-output-record record graph)
	  record))))

(defun layout-edge-1 (graph major-node minor-node)
  (let ((edge-record (ensure-edge-record graph major-node minor-node)))    
    (with-slots (stream arc-drawer arc-drawing-options) edge-record
      (with-bounding-rectangle* (x1 y1 x2 y2) major-node
        (with-bounding-rectangle* (u1 v1 u2 v2) minor-node
          (clear-output-record edge-record)  ;;; FIXME: repaint?
           (letf (((stream-current-output-record stream) edge-record))
            (ecase (slot-value graph 'orientation)
	      ((:horizontal)
	       (multiple-value-bind (from to) (if (< x1 u1)
						  (values x2 u1)
					          (values x1 u2))
		 (apply arc-drawer stream major-node minor-node
			from (/ (+ y1 y2) 2)
			to   (/ (+ v1 v2) 2)
			arc-drawing-options)))
	      ((:vertical)
	       (multiple-value-bind (from to) (if (< y1 v1)
						  (values y2 v1)
				     	          (values y1 v2))
		 (apply arc-drawer stream major-node minor-node
			(/ (+ x1 x2) 2) from
			(/ (+ u1 u2) 2) to
			arc-drawing-options))))))))))

(defun layout-edge (graph major-node minor-node stream arc-drawer arc-drawing-options)
  (let ((edge-record (ensure-edge-record graph major-node minor-node)))
    (setf (slot-value edge-record 'stream) stream
	  (slot-value edge-record 'arc-drawer) arc-drawer
	  (slot-value edge-record 'arc-drawing-options) arc-drawing-options)
    (layout-edge-1 graph major-node minor-node)))

(defmethod layout-graph-edges ((graph standard-graph-output-record)
                               stream arc-drawer arc-drawing-options)
  (with-slots (orientation) graph
   ;; We tranformed the position of the nodes when we inserted them into
   ;; output history, so the bounding rectangles queried below will be
   ;; transformed. Therefore, disable the transformation now, otherwise
   ;; the transformation is effectively applied twice to the edges.
   (with-identity-transformation (stream)
    (traverse-graph-nodes graph
                          (lambda (node children continuation)
                            (unless (eq node graph)
			      (layout-edges graph node stream arc-drawer arc-drawing-options))
                            (map nil continuation children))))))

(defmethod layout-graph-edges :around ((graph-output-record tree-graph-output-record)
                                       stream arc-drawer arc-drawing-options)
  (setf arc-drawer (or arc-drawer #'standard-arc-drawer))
  (call-next-method graph-output-record stream arc-drawer arc-drawing-options))

(defmethod layout-graph-edges :around ((graph-output-record digraph-graph-output-record)
                                       stream arc-drawer arc-drawing-options)
  (setf arc-drawer (or arc-drawer #'arrow-arc-drawer))
  (call-next-method graph-output-record stream arc-drawer arc-drawing-options))

(defmethod layout-graph-edges :around ((graph-output-record dag-graph-output-record)
                                       stream arc-drawer arc-drawing-options)
  (setf arc-drawer (or arc-drawer #'standard-arc-drawer))
  (call-next-method graph-output-record stream arc-drawer arc-drawing-options))

(defun standard-arc-drawer (stream from-node to-node x1 y1 x2 y2
                            &rest drawing-options
                            &key &allow-other-keys)
  (declare (ignore from-node to-node))
  (apply #'draw-line* stream x1 y1 x2 y2 drawing-options))

(defun arrow-arc-drawer (stream from-node to-node x1 y1 x2 y2
                            &rest drawing-options
                            &key &allow-other-keys)
  (declare (ignore from-node to-node))
  (apply #'draw-arrow* stream x1 y1 x2 y2 drawing-options))

#||

;; Experimental version for rectangular graphs

(defmethod layout-graph-edges ((graph-output-record tree-graph-output-record)
                               stream arc-drawer arc-drawing-options)
  (with-slots (root-nodes orientation) graph-output-record
    (let ((hash (make-hash-table)))
      (labels ((walk (node &aux (vlast nil) uu)
                 (unless (gethash node hash)
                   (setf (gethash node hash) t)
                   (with-bounding-rectangle* (x1 y1 x2 y2) node
                     (dolist (k (graph-node-children node))
                       (with-bounding-rectangle* (u1 v1 u2 v2) k
                         (case orientation
                           (:horizontal
                            (draw-line* stream (/ (+ x2 u1) 2) (/ (+ v1 v2) 2)
                             (- u1 2) (/ (+ v1 v2) 2))
                            (setf uu u1)
                            (setf vlast (max (or vlast 0) (/ (+ v1 v2) 2))))
                           (:vertical
                            (draw-line* stream (/ (+ x1 x2) 2) y2
                             (/ (+ u1 u2) 2) v1))))
                       (walk k))
                     (when vlast
                       (draw-line* stream (+ x2 2) (/ (+ y1 y2) 2) (/ (+ x2 uu) 2) (/ (+ y1 y2) 2))
                       (draw-line* stream (/ (+ x2 uu) 2) (/ (+ y1 y2) 2)
                                   (/ (+ x2 uu) 2) vlast))))))
        (map nil #'walk root-nodes)))))
||#

#||

;;; Testing --GB 2002-08-14

(define-application-frame graph-test ()
  ()
  (:panes
   (interactor :interactor :width 800 :height 400 :max-width +fill+ :max-height +fill+))
  (:layouts
   (default
       interactor)))

(define-graph-test-command foo ()
  (with-text-style (*query-io* (make-text-style :sans-serif nil 12))
    (let ((*print-case* :downcase))
      (format-graph-from-roots
       (list `(define-graph-test-command test ()
               (let ((stream *query-io*)
                     (orientation :horizontal))
                 (fresh-line stream)
                 (macrolet ((make-node (&key name children)
                              `(list* ,name ,children)))
                   (flet ((node-name (node)
                            (car node))
                          (node-children (node)
                            (cdr node)))
                     (let* ((2a (make-node :name "2A"))
                            (2b (make-node :name "2B"))
                            (2c (make-node :name "2C"))
                            (1a (make-node :name "1A" :children (list 2a 2b)))
                            (1b (make-node :name "1B" :children (list 2b 2c)))
                            (root (make-node :name "0" :children (list 1a 1b))))
                       (format-graph-from-roots
                        (list root)
                        #'(lambda (node s)
                            (write-string (node-name node) s))
                        #'node-children
                        :orientation orientation
                        :stream stream)))))))
       #'(lambda (x s) (with-output-as-presentation (s x 'command)
                         (let ((*print-level* 1))
                           (princ (if (consp x) (car x) x) s))))
       #'(lambda (x) (and (consp x) (cdr x)))
       :stream *query-io*
       :orientation :horizontal))))

(defun external-symbol-p (sym)
  ;; *cough* *cough*
  (< (count #\: (let ((*package* (find-package :keyword)))
                  (prin1-to-string sym)))
     2))

(define-graph-test-command bar ()
  (with-text-style (*query-io* (make-text-style :sans-serif nil 10))
    (let ((*print-case* :downcase))
      (format-graph-from-roots
       (list (clim-mop:find-class 'climi::basic-output-record))
       #'(lambda (x s)
           (progn ;;surrounding-output-with-border (s :shape :oval)
             (with-text-style (s (make-text-style nil
                                                  (if (external-symbol-p (class-name x))
                                                      :bold
                                                      nil)
                                                  nil))
               (prin1 (class-name x) s))))
       #'(lambda (x)
           (clim-mop:class-direct-subclasses x))
       :generation-separation '(4 :line)
       :within-generation-separation '(2 :character)
       :stream *query-io*
       :orientation :vertical))))

(define-graph-test-command bar ()
  (with-text-style (*query-io* (make-text-style :sans-serif nil 10))
    (format-graph-from-roots
     (list '(:foo
             (:bar)
             (:baaaaaaaaaaaaaaz
              (:a)
              (:b))
             (:q
              (:x) (:y)))
           )
     #'(lambda (x s)
         (prin1 (first x) s))
     #'(lambda (x)
         (cdr x))
     :generation-separation '(4 :line)
     :within-generation-separation '(2 :character)
     :stream *query-io*
     :orientation :vertical)))

(define-graph-test-command baz ()
  (with-text-style (*query-io* (make-text-style :sans-serif nil 10))
    (let ((*print-case* :downcase))
      (format-graph-from-roots
       (list (clim-mop:find-class 'standard-graph-output-record)
             ;;(clim-mop:find-class 'climi::basic-output-record)
             ;;(clim-mop:find-class 'climi::graph-output-record)
             
             )
       #'(lambda (x s)
           (with-text-style (s (make-text-style nil
                                                (if (external-symbol-p (class-name x))
                                                    :bold
                                                    nil)
                                                nil))
             (prin1 (class-name x) s)))
       #'(lambda (x)
           (reverse(clim-mop:class-direct-superclasses x)))
       ;; :duplicate-key #'(lambda (x) 't)
       :merge-duplicates t
       :graph-type :tree
       :arc-drawer #'arrow-arc-drawer
       :stream *query-io*
       :orientation :vertical))))

(define-graph-test-command test ()
  (let ((stream *query-io*)
        (orientation :vertical))
    (fresh-line stream)
    (macrolet ((make-node (&key name children)
                 `(list* ,name ,children)))
      (flet ((node-name (node)
               (car node))
             (node-children (node)
               (cdr node)))
        (let* ((2a (make-node :name "2A"))
               (2b (make-node :name "2B"))
               (2c (make-node :name "2C"))
               (1a (make-node :name "1A" :children (list 2a 2b)))
               (1b (make-node :name "1B" :children (list 2b 2c)))
               (root (make-node :name "0" :children (list 1a 1b))))
          (format-graph-from-roots
           (list root)
           #'(lambda (node s)
               (write-string (node-name node) s))
           #'node-children
           :arc-drawer #'arrow-arc-drawer
           :arc-drawing-options (list :ink +red+ :line-thickness 1)
           :orientation orientation
           :stream stream))))))

(defun make-circ-list (list)
  (nconc list list))

(define-graph-test-command test2 ()
  (let ((stream *query-io*)
        (orientation :vertical))
    (fresh-line stream)
    (format-graph-from-roots
     (list '(defun dcons (x) (cons x x))
           (make-circ-list (list 1 '(2 . 4) 3)))
     #'(lambda (node s)
         (if (consp node)
             (progn
               (draw-circle* s 5 5 5 :filled nil))
             (princ node s)))
     #'(lambda (x) (if (consp x) (list (car x) (cdr x))))
     :cutoff-depth nil
     :graph-type :tree
     :merge-duplicates t
     :arc-drawer #'arrow-arc-drawer
     :arc-drawing-options (list :ink +red+ :line-thickness 1)
     :orientation orientation
     :stream stream)))
||#