;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2002 by Gilbert Baumann
;;;  (c) copyright 2005 by Robert P. Goldman
;;;  (c) copyright 2017 by John A. Carroll
;;;  (c) copyright 2020 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Graph Formatting.
;;;

(in-package :clim-internals)

;;;; Notes

;;; - Now what exactly are layout-graph-nodes and layout-graph-edges
;;;   supposed to do? If LAYOUT-GRAPH-NODES is only responsible for
;;;   laying out the node output records, why does it get the
;;;   arc-drawer? If it should also draw the edges why then is there
;;;   the other function? --GB 2002-08-13

;;; - There is this hash table initarg to graph-output-records? Should
;;;   FORMAT-GRAPH-FROM-ROOTS pass a suitable hash table for the given
;;;   'duplicate-test', if so why it is passed down and why is it not
;;;   restricted to the set of hash test functions? --GB 2002-08-13

;;; - What is the purpose of (SETF GRAPH-NODE-CHILDREN) and
;;;   (SETF GRAPH-NODE-PARENTS)? --GB 2002-08-14

;;; - FORMAT-GRAPH-FROM-ROOTS passes the various options on to the
;;;   instantiation of the graph-output-record class, so that the
;;;   individual classes can choose appropriate defaults. --GB 2002-08-14

;;; - In the same spirit, a non given ARC-DRAWER option is passed as it
;;;   is, that is being NIL, to LAYOUT-GRAPH-EDGES so that the concrete
;;;   graph-output-record can choose a default. --GB 2002-08-14

;;;; Declarations

;;; format-graph-from-roots

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

(defun format-graph-from-root (root-object &rest other-args)
  (apply #'format-graph-from-roots (list root-object) other-args))

(defun format-graph-from-roots (root-objects object-printer inferior-producer
                                &rest rest-args
                                &key stream (orientation :horizontal) cutoff-depth
                                  merge-duplicates duplicate-key duplicate-test
                                  generation-separation
                                  within-generation-separation
                                  center-nodes
                                  arc-drawer arc-drawing-options
                                  graph-type maximize-generations (move-cursor t)
                                &allow-other-keys)
  (declare (ignore generation-separation within-generation-separation center-nodes
                   maximize-generations))
  ;; don't destructively modify the &rest arg
  (let ((graph-options (alexandria:remove-from-plist
                        rest-args :stream :duplicate-key :duplicate-test
                        :arc-drawer :arc-drawing-options
                        :graph-type :move-cursor)))

    ;; munge some of the arguments
    (check-type cutoff-depth (or null integer))
    (check-type root-objects sequence)
    (check-type orientation (member :horizontal :vertical))
    (setf stream (or stream *standard-output*)
          graph-type (or graph-type (if merge-duplicates :digraph :tree))
          duplicate-key (or duplicate-key #'identity)
          duplicate-test (or duplicate-test #'eql) )

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
                           (layout-graph-edges graph-output-record stream arc-drawer arc-drawing-options))))
                (apply #'invoke-with-new-output-record stream
                       #'cont
                       (find-graph-type graph-type)
                       graph-options))))
        (setf (output-record-position graph-output-record)
              (values cursor-old-x cursor-old-y))
        (when (and (stream-drawing-p stream)
                   (output-record-ancestor-p (stream-output-history stream)
                                             graph-output-record))
          (with-output-recording-options (stream :draw t :record nil)
            (replay graph-output-record stream)))
        (when move-cursor
          (setf (stream-cursor-position stream)
                (values (bounding-rectangle-max-x graph-output-record)
                        (bounding-rectangle-max-y graph-output-record))))
        graph-output-record))))

;;;; Graph Output Records

(defclass standard-graph-output-record
    (graph-output-record standard-sequence-output-record)
  ((orientation
    :initarg :orientation)
   (center-nodes
    :initarg :center-nodes)
   (cutoff-depth
    :initarg :cutoff-depth)
   (merge-duplicates
    :initarg :merge-duplicates)
   (generation-separation
    :initarg :generation-separation)
   (within-generation-separation
    :initarg :within-generation-separation)
   (maximize-generations
    :initarg :maximize-generations)
   (root-nodes
    :accessor graph-root-nodes))
  (:default-initargs :orientation :horizontal
                     :center-nodes nil
                     :cutoff-depth nil
                     :merge-duplicates nil
                     :maximize-generations nil))

(defmethod initialize-instance :after ((record standard-graph-output-record)
                                       &key
                                         orientation
                                         generation-separation
                                         within-generation-separation)
  (unless generation-separation
    (setf (slot-value record 'generation-separation)
          (ecase orientation
            (:horizontal '(3 :character))
            (:vertical '(2 :line)))))
  (unless within-generation-separation
    (setf (slot-value record 'within-generation-separation)
          (ecase orientation
            (:horizontal '(1 :line))
            (:vertical '(2 :character))))))

(defclass tree-graph-output-record (standard-graph-output-record)
  ())

(defclass dag-graph-output-record (standard-graph-output-record)
  ())

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
   ;; internal slots for the graph layout algorithm
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
  (let* ((cutoff-depth (slot-value graph-output-record 'cutoff-depth))
         (merge-duplicates (slot-value graph-output-record 'merge-duplicates))
         (node-list '())
         (hashed (and merge-duplicates
                      (member duplicate-test '(#'eq #'eql #'equal #'equalp))))
         (hash-table (and hashed (make-hash-table :test duplicate-test))))
    (labels
        ((lookup (key)
           (when merge-duplicates
             (if hashed
                 (locally (declare (type hash-table hash-table))
                   (gethash key hash-table))
                 (cdr (assoc key node-list :test duplicate-test)))))
         ((setf lookup) (node key)
           (if hashed
               (locally (declare (type hash-table hash-table))
                 (setf (gethash key hash-table) node))
               (push (cons key node) node-list))
           node)
         (make-node (object)
           (with-output-to-output-record
               (stream 'standard-graph-node-output-record new-node
                       :object object)
             (with-end-of-line-action (stream :allow)
               (funcall object-printer object stream))))
         (merge-node (parent object)
           (multiple-value-bind (child oldp)
               (if merge-duplicates
                   (let* ((key (funcall duplicate-key object))
                          (existing (lookup key)))
                     (if existing
                         (values existing t)
                         (setf (lookup key) (make-node object))))
                   (make-node object))
             (when parent
               (push parent (graph-node-parents child)))
             (values child oldp)))
         (traverse-objects (parent objects depth)
           (unless (and cutoff-depth (>= depth cutoff-depth))
             (collect (children)
               (do-sequence (object objects)
                 (multiple-value-bind (node oldp) (merge-node parent object)
                   (unless oldp
                     (setf (graph-node-children node)
                           (traverse-objects
                            node
                            (funcall inferior-producer object)
                            (+ depth 1))))
                   (children node)))
               (children)))))
      (setf (graph-root-nodes graph-output-record)
            (traverse-objects nil root-objects 0))
      (values))))

(defun traverse-graph-nodes (graph continuation)
  ;; continuation: node x children x cont -> some value
  (let ((hash (make-hash-table :test #'eq)))
    (labels ((walk (node)
               (unless (gethash node hash)
                 (setf (gethash node hash) t)
                 (funcall continuation node (graph-node-children node) #'walk))))
      (funcall continuation graph (graph-root-nodes graph) #'walk))))

(defmethod generate-graph-nodes ((graph-output-record tree-graph-output-record)
                                 stream root-objects
                                 object-printer inferior-producer
                                 &key &allow-other-keys)
  #+ (or)
  (with-slots (merge-duplicates) graph-output-record
    (when merge-duplicates
      ;; I'm not sure what to do here. Saying you want a tree, but want
      ;; duplicates merged seems wrong. OTOH, if you go out of your way to do
      ;; it, at your own risk, is it our place to say "no"?  -- rpg 2005-08-11
      ;;
      ;; Let's not signal an error, but proceed such that that the output will
      ;; be equivalent to the DAG graph type. --JAC 2017/09/04
      (cerror "Set to NIL and continue?"
              "Merge duplicates specified to be true when using :tree layout")
      (setf merge-duplicates nil)))
  (call-next-method))

(defmethod generate-graph-nodes ((graph-output-record dag-graph-output-record)
                                 stream root-objects
                                 object-printer inferior-producer
                                 &key &allow-other-keys)
  (with-slots (merge-duplicates) graph-output-record
    ;; This case is more serious than the converse above. If we're not allowed
    ;; to merge duplicate nodes then we can't output a DAG... get the user to
    ;; acknowledge this. --JAC 2017/09/04
    (unless merge-duplicates
      (cerror "Set to T and continue?"
              "DAG graph-layout type only supports merge-duplicates to be T")
      (setf merge-duplicates t)))
  (call-next-method))

(defmethod generate-graph-nodes ((graph-output-record digraph-graph-output-record)
                                 stream root-objects
                                 object-printer inferior-producer
                                 &key &allow-other-keys)
  (with-slots (merge-duplicates) graph-output-record
    (unless merge-duplicates
      (cerror "Set to T and continue?"
              "Digraph graph-layout type only supports merge-duplicates to be T")
      (setf merge-duplicates t)))
  (call-next-method))


;;; The CLIM II specification gives a broad hint that what we are doing here
;;; is fine: "Typically, different graph types will use different output
;;; record classes and layout engines to lay out the graph. However, it is
;;; permissible for all of the required graph types to use exactly the same
;;; graph layout engine." We have specialized versions of layout-graph-nodes
;;; since we have a single engine that works tolerably well for all.

(defmethod layout-graph-nodes ((graph-output-record standard-graph-output-record)
                               stream arc-drawer arc-drawing-options)
  "Layout for DAGs, digraphs and trees. There are three phases that traverse the
graph topologically. The first assigns a depth to each node, the second
computes minor and major dimensions, and the final one lays nodes out by depth.
Assumes that GENERATE-GRAPH-NODES has generated only nodes up to the cutoff-depth."
  (declare (ignore arc-drawer arc-drawing-options))
  (with-slots (orientation center-nodes generation-separation
               within-generation-separation root-nodes maximize-generations)
      graph-output-record
    ;; major dimension is the dimension in which we grow the graph,
    ;; i.e. horizontal for horizontal orientation
    (let ((within-generation-separation
            (parse-space stream within-generation-separation
                         (case orientation
                           (:horizontal :vertical)
                           (:vertical :horizontal))))
          (generation-separation
            (parse-space stream generation-separation orientation)))
      ;; generation sizes is an adjustable array that tracks the major
      ;; dimension of each of the generations -- rpg 2005-07-18
      (let ((generation-sizes (make-array 10 :adjustable t :initial-element 0))
            (depth-hash (make-hash-table :test #'eq))
            (parent-hash (make-hash-table :test #'eq)))
        (flet ((node-major-dimension (node)
                 (if (eq orientation :vertical)
                     (bounding-rectangle-height node)
                     (bounding-rectangle-width node)))
               (node-minor-dimension (node)
                 (if (eq orientation :vertical)
                     (bounding-rectangle-width node)
                     (bounding-rectangle-height node))))
          ;;
          (labels ((assign-depth (node depth trail)
                     ;; In a DAG or digraph, there may be multiple paths to a
                     ;; node ending up at different depths, so in the major
                     ;; dimension - depending on maximize-generations - put
                     ;; the node one generation beyond all of its parents or
                     ;; one generation beyond the parent nearest the
                     ;; roots. Use a trail to escape from circularity, while
                     ;; still processing reentrancies -- JAC 2017-09-04
                     (unless (member node trail :test #'eq)
                       (let ((node-depth (gethash node depth-hash)))
                         (when (or (null node-depth)
                                   (if maximize-generations
                                       (> depth node-depth)
                                       (< depth node-depth)))
                           (setf (gethash node depth-hash) depth)
                           (do-sequence (child (graph-node-children node))
                             (assign-depth child
                                           (1+ depth)
                                           (cons node trail))))))))
            (do-sequence (x root-nodes)
              (assign-depth x 0 nil)))
          ;;
          (labels ((compute-dimensions (node depth &optional parent)
                     ;; compute the major dimension of all the nodes at this
                     ;; generation depth, and the minor dimension of this
                     ;; individual node - which takes into account the space
                     ;; for the node's children
                     (when (eql (gethash node depth-hash) depth)
                       (setf (gethash node depth-hash) nil) ; mark as done
                       (when parent
                         (setf (gethash node parent-hash) parent))
                       (when (>= depth (length generation-sizes))
                         (setf generation-sizes (adjust-array
                                                 generation-sizes
                                                 (ceiling (* depth 1.2))
                                                 :initial-element 0)))
                       (setf (aref generation-sizes depth)
                             (max (aref generation-sizes depth)
                                  (node-major-dimension node)))
                       (setf (graph-node-minor-size node) 0)
                       (max (node-minor-dimension node)
                            (setf (graph-node-minor-size node)
                                  (let ((sum 0)
                                        (n 0)
                                        (depth-1 (+ depth 1)))
                                    (do-sequence (child (graph-node-children node))
                                      (when-let ((x (compute-dimensions child depth-1 node)))
                                        (incf sum x)
                                        (incf n)))
                                    (+ sum
                                       (* (max 0 (- n 1))
                                          within-generation-separation))))))))
            (do-sequence (x root-nodes)
              (compute-dimensions x 0)))
          ;;
          (let ((visited (make-hash-table :test #'eq)))
            (labels ((compute-position (node majors u0 v0)
                       (when (gethash node visited)
                         (return-from compute-position v0))
                       (setf (gethash node visited) t)
                       (let ((d (- (node-minor-dimension node)
                                   (graph-node-minor-size node))))
                         ;; center-nodes is meant to be "with respect to the
                         ;; widest node in the same generation". Interpret
                         ;; "wide" as being the size in the major dimension
                         ;; --JAC 2017-09-04
                         (let ((u (if center-nodes
                                      (+ u0 (/ (- (car majors) (node-major-dimension node)
                                                  generation-separation)
                                               2))
                                      u0))
                               (v (+ v0 (/ (min 0 d) -2)))
                               (tr (medium-transformation stream)))
                           (setf (output-record-position node)
                                 (if (eq orientation :vertical)
                                     (transform-position tr v u)
                                     (transform-position tr u v)))
                           (add-output-record node graph-output-record))
                         ;;
                         (let ((u (+ u0 (car majors)))
                               (v (+ v0 (max 0 (/ d 2))))
                               (firstp t)
                               ;; to make the tree style layout work for DAGs
                               ;; and digraphs, we must only recurse to those
                               ;; children whose dimensions were computed
                               ;; previously with this node as their principal
                               ;; parent -- JAC 2017-09-04
                               (nodes (remove-if-not
                                       (lambda (x)
                                         (eq (gethash x parent-hash) node))
                                       (graph-node-children node))))
                           (do-sequence (q nodes)
                             (unless (gethash q visited)
                               (if firstp
                                   (setf firstp nil)
                                   (incf v within-generation-separation))
                               (setf v (compute-position q (cdr majors) u v)))))
                         ;;
                         (+ v0 (max (node-minor-dimension node)
                                    (graph-node-minor-size node))))))
              ;;
              (let ((majors (loop for x across generation-sizes
                                  collect (+ x generation-separation)))
                    (u 0)
                    (v 0))
                (do-sequence (elt (graph-root-nodes graph-output-record))
                  (setf v (compute-position elt majors u v))
                  (incf v within-generation-separation))))))))))

;;;; Edges

(defclass standard-edge-output-record (standard-sequence-output-record)
  ((stream)
   (arc-drawer)
   (arc-drawing-options)
   (from-node :initarg :from-node :reader from-node)
   (to-node :initarg :to-node :reader to-node)))

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
  ;; If arc-drawer is unsupplied, the default behavior is to draw a
  ;; thin line...
  (setf arc-drawer (or arc-drawer #'standard-arc-drawer))
  (with-slots (orientation) graph
    ;; We transformed the position of the nodes when we inserted them
    ;; into output history, so the bounding rectangles queried below
    ;; will be transformed. Therefore, disable the transformation now,
    ;; otherwise the transformation is effectively applied twice to
    ;; the edges.
    (with-identity-transformation (stream)
      (traverse-graph-nodes graph
                            (lambda (node children continuation)
                              (unless (eq node graph)
                                (layout-edges graph node stream arc-drawer arc-drawing-options))
                              (map nil continuation children))))))

(defmethod layout-graph-edges :around ((graph-output-record digraph-graph-output-record)
                                       stream arc-drawer arc-drawing-options)
  ;; The default behaviour of a mere line is not appropriate for digraphs, so default to a
  ;; more helpful alternative
  (setf arc-drawer (or arc-drawer #'arrow-arc-drawer))
  (call-next-method graph-output-record stream arc-drawer arc-drawing-options))

(defun standard-arc-drawer (stream from-node to-node x1 y1 x2 y2
                            &rest drawing-options
                            &key &allow-other-keys)
  (declare (ignore from-node to-node))
  (apply #'draw-line* stream x1 y1 x2 y2 drawing-options))

(defun arrow-arc-drawer (stream from-node to-node x1 y1 x2 y2
                         &rest drawing-options
                         &key &allow-other-keys)
  (if (eq from-node to-node)
      (let ((rad 8))
        (apply #'draw-circle*
               stream (- x1 rad -2) (- y1 rad) rad
               :filled nil :end-angle (* pi 1.5) drawing-options)
        (apply #'draw-arrow*
               stream (- x1 rad -2) y1 x1 y1 drawing-options))
      (apply #'draw-arrow* stream x1 y1 x2 y2 drawing-options)))

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
