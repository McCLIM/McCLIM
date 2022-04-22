;;;; Core functionality of mcclim-dot
;;;;
;;;; This file is part of the mcclim-dot extension. See
;;;; Extensions/dot/README.md and Extensions/dot/LICENSE for more information.

(cl:in-package #:mcclim-dot)

;;; Graph clases

(defclass dot-graph-output-record (clim:standard-graph-output-record)
  ((dot-layout
    :documentation
    "Holds an instance of DOT-LAYOUT with the results of the layout.")
   (dot-bounding-box
    :documentation
    "A RECTANGLE specifying the bounding box of the DOT-LAYOUT.")
   (dot-id-to-record
    :initform (make-hash-table :test 'equal)
    :documentation
    "Maps a dot id to a node record.")
   (layout-override
    :initform nil
    :initarg :layout-override
    :documentation
    "If non-NIL, is an instance of DOT-LAYOUT specifying where nodes
and (optionally) edges are positioned. Must be an object returned by
MAKE-LAYOUT-OVERRIDE.")
   (dot-processor
    :initarg :dot-processor
    :documentation
    "A function that takes two positional arguments and any number of keyword
arguments. The first argument is this graph output record. The second is an
instance of CL-DOT::GRAPH, representing the layout problem. This function must
return an instance of CL-DOT::GRAPH with the nodes and edges layed out.")
   (dot-processor-options
    :initarg :dot-processor-options
    :initform nil
    :documentation
    "A plist that is passed to the dot processor function."))
  (:documentation
   "Base class for all graphs that use DOT based tools to layout the graph."))

(defmethod initialize-instance :after ((graph-record dot-graph-output-record)
                                       &key dot-processor)
  (with-slots ((%dot-processor dot-processor)) graph-record
    (setf %dot-processor
          (or dot-processor
              (if (fboundp 'external-graphviz-dot-processor)
                  'external-graphviz-dot-processor)
              (error "DOT-PROCESSOR not provided and no suitable default could be found.")))))

(defclass dot-digraph-output-record (dot-graph-output-record
                                     climi::digraph-graph-output-record)
  ()
  (:documentation
   "A directed dot graph."))

(clim:define-graph-type :dot-digraph dot-digraph-output-record)


;;; Arc drawing

(defparameter *dot-edge-label-fontsize* 14)
(defparameter *dot-edge-label-fontname* "Courier New")
(defparameter *dot-edge-label-width-ratio* 6/10)

(defclass dot-arc-drawer (c2mop:funcallable-standard-object)
  ((label-records
    :initform (make-hash-table :test 'equal))
   (edge-label-printer
    :initform (constantly nil)
    :initarg :edge-label-printer
    :documentation
    "A convenience helper. This function is called by the default
DRAW-DOT-LABEL with this DOT-ARC-DRAWER, the stream, the objects the arc is
FROM and TO (not the GRAPH-NODE-RECORDs), and the ARC-DRAWING-OPTIONS."))
  (:documentation
   "A FUNCALLABLE-STANDARD-OBJECT, instances of which can be used as the
:ARC-DRAWER for FORMAT-GRAPH-FROM-ROOTS. It adds an extensible procedure for
drawing arcs, complete with labels.

DRAW-DOT-ARC is the primary arc drawing function. It delegates much of the work
to DRAW-DOT-EDGE and DRAW-DOT-LABEL.")
  (:metaclass c2mop:funcallable-standard-class))

(defmethod initialize-instance :after ((drawer dot-arc-drawer) &key)
  (c2mop:set-funcallable-instance-function drawer (a:curry #'draw-dot-arc drawer)))

(defgeneric draw-dot-arc (drawer stream from to from-x from-y to-x to-y
                          &key splines label-center)
  (:documentation
   "The function that is called to draw arcs when passing an instance of
DOT-ARC-DRAWER to FORMAT-GRAPH-FROM-ROOTS."))

(defgeneric draw-dot-edge (drawer stream from to from-x from-y to-x to-y splines &key)
  (:documentation
   "Draw the line connecting FROM and TO nodes."))

(defgeneric draw-dot-label (drawer stream from to &key)
  (:documentation
   "Draw the label for the arc between FROM and TO."))

(defgeneric dot-label-record (drawer stream from to &key)
  (:documentation
   "Returns the output record for the label between FROM and TO."))

(defmethod draw-dot-label ((drawer dot-arc-drawer) stream from to &rest args)
  "Default method. Calls the :EDGE-LABEL-PRINTER provided to DOT-ARC-DRAWER."
  (with-slots (edge-label-printer) drawer
    (apply edge-label-printer drawer stream (clim:graph-node-object from) (clim:graph-node-object to) args)))

(defmethod dot-label-record ((drawer dot-arc-drawer) stream from to &rest args)
  "Default method, calls DRAW-DOT-LABEL, wraps it in an output record, and
caches it."
  (with-slots (label-records) drawer
    (values
     (a:ensure-gethash
      (list from to) label-records
      (multiple-value-bind (cursor-x cursor-y)
          (clim:stream-cursor-position stream)
        (multiple-value-prog1
            (clim:with-new-output-record (stream)
              (clim:with-end-of-line-action (stream :allow)
                (apply #'draw-dot-label drawer stream from to args)))
          (clim:stream-set-cursor-position stream cursor-x cursor-y)))))))

(defmethod draw-dot-arc ((drawer dot-arc-drawer) stream from to from-x from-y to-x to-y
                         &rest args
                         &key splines label-center)
  "Default method. Calls DRAW-DOT-EDGE and DRAW-DOT-LABEL."
  (a:remove-from-plistf args :splines :label-center)
  (apply #'draw-dot-edge drawer stream from to from-x from-y to-x to-y splines args)
  (unless (null label-center)
    (let* ((record (apply #'dot-label-record drawer stream from to args))
           (bb-width (clim:bounding-rectangle-width record))
           (bb-height (clim:bounding-rectangle-height record)))
      ;; shift the record so that the center is at the point chosen by the layout
      ;; engine.
      (setf (clim:output-record-position record)
            (clim:transform-position (clim:medium-transformation stream)
                                     (- (clim:point-x label-center) (/ bb-width 2))
                                     (+ (clim:point-y label-center) (/ bb-height 2)))))))

(defmethod draw-dot-edge ((drawer dot-arc-drawer) stream from to from-x from-y to-x to-y splines
                          &rest args)
  "Default method. Draws the spline from SPLINES."
  (declare (ignore from to from-x from-y to-x to-y))
  (dolist (spline splines)
    (let ((start (dot-spline-start spline))
          (end (dot-spline-end spline))
          (points (dot-spline-points spline)))
      (apply #'mcclim-bezier:draw-bezier-design*
             stream (mcclim-bezier:make-bezier-curve points) args)
      (unless (null start)
        (apply #'clim:draw-arrow stream (first points) start args))
      (unless (null end)
        (apply #'clim:draw-arrow stream (a:last-elt points) end args)))))

(defgeneric dot-edge-label-to-dot-attributes (drawer stream from to &key)
  (:documentation
   "Return a list of dot attributes that represent the label between FROM and
TO."))

(defmethod dot-edge-label-to-dot-attributes ((drawer dot-arc-drawer) stream from to &rest args)
  "Default method. The dot language unfortunately does not have any way to
express a label in terms of its bounding box, only text. This method generates
a string that has the same bounding box as the label."
  (let* ((record (apply #'dot-label-record drawer stream from to args))
         (bb-width (clim:bounding-rectangle-width record))
         (bb-height (clim:bounding-rectangle-height record))
         (chars-wide (ceiling (/ bb-width (* *dot-edge-label-width-ratio* *dot-edge-label-fontsize*))))
         (chars-high (ceiling (/ bb-height *dot-edge-label-fontsize*)))
         (line (make-string chars-wide :initial-element #\m)))
    (list :label (with-output-to-string (s)
                   (dotimes (j chars-high)
                     (write-string line s)
                     (terpri s))
                   (write-string line s))
          :labelfontsize *dot-edge-label-fontsize*
          :labelfontname *dot-edge-label-fontname*)))


;;; Utilities

(defvar *id* nil
  "Used to generate unique ids for every node in a graph.")

(defvar *dot-stream*)
(defvar *arc-drawer*)

(defconstant +pts-per-inch+ 72)

(defclass dot-spline ()
  ((start
    :initarg :start
    :initform nil
    :reader dot-spline-start)
   (end
    :initarg :end
    :initform nil
    :reader dot-spline-end)
   (points
    :initarg :points
    :initform nil
    :reader dot-spline-points))
  (:documentation
   "A DOT b-spline. POINTS is a list of POINTs defining the points of a
b-spline. START is either a POINT or NIL. If it is non-NIL, an arrow should be
drawn from the first POINTS to START. END is either a POINT or NIL. If it is
non-NIL, an arrow should be drawn from the last POINTS to END."))

(defun pts-to-inches (points)
  (/ points +pts-per-inch+))

(defun inches-to-pts (inches)
  (* inches +pts-per-inch+))

(defun coordinate-string-to-point (string)
  "Convert a string of two numbers, separated by a comma into a POINT."
  (apply #'clim:make-point
         (mapcar #'pn:parse-number (ss:split-sequence #\, string))))

(defun point-to-coordinate-string (point)
  "Convert a POINT to a tring of two numbers, separated by a comma."
  (format nil "~F,~F" (clim:point-x point) (clim:point-y point)))

(defun coordinate-string-to-rectangle (string)
  "Convert a string of four numbers, separated by a comma into a RECTANGLE."
  (apply #'clim:make-rectangle*
         (mapcar #'pn:parse-number (ss:split-sequence #\, string))))

(defun spline-string-to-spline (string)
  (let* ((elements (ss:split-sequence #\Space string :remove-empty-subseqs t))
         (start (find #\s elements :key (lambda (s) (aref s 0))))
         (end (find #\e elements :key (lambda (s) (aref s 0))))
         (points (mapcar #'coordinate-string-to-point
                         (remove start (remove end elements)))))
    (when start
      (setf start (coordinate-string-to-point (subseq start 2))))
    (when end
      (setf end (coordinate-string-to-point (subseq end 2))))
    (make-instance 'dot-spline
                   :start (if (not (null start)) start)
                   :end (if (not (null end)) end)
                   :points points)))

(defun splines-string-to-splines (string)
  (mapcar #'spline-string-to-spline (ss:split-sequence #\; string)))

(defun spline-to-spline-string (spline)
  "Convert a DOT-SPLINE to a spline string in the Dot language."
  (let ((start (dot-spline-start spline))
        (end (dot-spline-end spline))
        (points (dot-spline-points spline)))
    (format nil "~@[e,~A ~]~@[s,~A ~]~{~A~^ ~}"
            (if (not (null end)) (point-to-coordinate-string end))
            (if (not (null start)) (point-to-coordinate-string start))
            (mapcar #'point-to-coordinate-string points))))

(defun splines-to-splines-string (splines)
  "Convert a list of DOT-SPLINEs to a splines string in the Dot language."
  (format nil "~{~A~^;~}" (mapcar #'spline-to-spline-string splines)))


;; Layout

(defclass dot-layout ()
  ((%nodes
    :initarg :nodes
    :reader node-layout)
   (%edges
    :initarg :edges
    :reader edge-layout)
   (%key
    :initarg :key
    :initform #'identity
    :reader layout-key))
  (:documentation
   "Contains the results of performing layout. Can also be provided as a
:LAYOUT-OVERRIDE."))

(defun make-dot-layout (dot-graph dot-id-to-record)
  "Helper function to take a CL-DOT::GRAPH instance, extract the layout
relevant information, and return a DOT-LAYOUT instance."
  (let ((node-pos-ht (make-hash-table))
        (edge-spline-ht (make-hash-table :test 'equal)))
    (dolist (dot-node (dot::nodes-of dot-graph))
      (let* ((node (gethash (dot::id-of dot-node) dot-id-to-record))
             (pos-attribute (getf (dot::attributes-of dot-node) :pos))
             (pos (coordinate-string-to-point pos-attribute)))
        (setf (gethash node node-pos-ht) pos)))
    (dolist (dot-edge (dot::edges-of dot-graph))
      (let* ((attributes (dot::attributes-of dot-edge))
             (pos (getf attributes :pos))
             (lp (getf attributes :lp))
             (splines (splines-string-to-splines pos))
             (from-dot-node (dot::source-of dot-edge))
             (from (gethash (dot::id-of from-dot-node) dot-id-to-record))
             (to-dot-node (dot::target-of dot-edge))
             (to (gethash (dot::id-of to-dot-node) dot-id-to-record)))
        (setf (gethash (list from to) edge-spline-ht)
              (list splines
                    (unless (null lp)
                      (coordinate-string-to-point lp))))))
    (make-instance 'dot-layout :nodes node-pos-ht :edges edge-spline-ht)))

(defun make-layout-override (record &key (key #'identity) include-edges-p)
  "Given a DOT-GRAPH-OUTPUT-RECORD that has already been layed out, extract an
instance suitable for passing to :LAYOUT-OVERRIDE in another call to
FORMAT-GRAPH-FROM-ROOTS.

If KEY is provided, it is applied to every GRAPH-NODE-OBJECT to produce a key
that is EQL comparable in future layouts. If INCLUDE-EDGES-P, then edge layouts
will be included in the returned instance as well."
  (with-slots (dot-layout) record
    (let ((node-pos-ht (make-hash-table))
          (edge-spline-ht (make-hash-table :test 'equal)))
      (with-hash-table-iterator (next (node-layout dot-layout))
        (loop
          (multiple-value-bind (more node point) (next)
            (unless more (return))
            (setf (gethash (funcall key (clim:graph-node-object node)) node-pos-ht)
                  point))))
      (when include-edges-p
        (with-hash-table-iterator (next (edge-layout dot-layout))
          (loop
            (multiple-value-bind (more edge dot-layout) (next)
              (unless more (return))
              (destructuring-bind (from to) edge
                (setf (gethash (list (funcall key (clim:graph-node-object from))
                                     (funcall key (clim:graph-node-object to)))
                               edge-spline-ht)
                      dot-layout))))))
      (make-instance 'dot-layout
                     :nodes node-pos-ht
                     :edges (when include-edges-p edge-spline-ht)))))


;; CLIM graph to CL-DOT::GRAPH

(defmethod dot:graph-object-node ((record dot-graph-output-record) object)
  "Create a CL-DOT::NODE for the OBJECT. Create a unique ID, set the attributes
to match the OBJECT's bounding box, and save the ID in the RECORD's
DOT-ID-TO-RECORD map."
  (with-slots (layout-override) record
    (let ((id (princ-to-string (incf *id*)))
          (attributes (list :shape :rectangle
                            :fixedsize "true"
                            :width (float (pts-to-inches (clim:bounding-rectangle-width object)))
                            :height (float (pts-to-inches (clim:bounding-rectangle-height object)))))
          node)
      (when layout-override
        (a:when-let ((pos-override (gethash (funcall (layout-key layout-override)
                                                     (clim:graph-node-object object))
                                            (node-layout layout-override))))
          (push (point-to-coordinate-string pos-override) attributes)
          (push :pos attributes)))
      (setf node (make-instance 'dot:node :id id :attributes attributes))
      (with-slots (dot-id-to-record) record
        (setf (gethash id dot-id-to-record) object)
        node))))

(defmethod dot:graph-object-points-to ((record dot-graph-output-record) object)
  "Return a children that OBJECT points to."
  (with-slots (layout-override) record
    (let ((edges-ht (if (not (null layout-override)) (edge-layout layout-override)))
          (out ()))
      (dolist (child (clim:graph-node-children object) out)
        (let ((attributes ()))
          (when (typep (car *arc-drawer*) 'dot-arc-drawer)
            (a:appendf attributes
                       (apply #'dot-edge-label-to-dot-attributes
                              (car *arc-drawer*) *dot-stream* object child (cdr *arc-drawer*))))
          (when edges-ht
            (let ((object-key (funcall (layout-key layout-override)
                                       (clim:graph-node-object object)))
                  (child-key (funcall (layout-key layout-override)
                                      (clim:graph-node-object child))))
              (destructuring-bind (splines lp)
                  (gethash (list object-key child-key) edges-ht '(nil nil))
                (when splines
                  (push (splines-to-splines-string splines) attributes)
                  (push :pos attributes))
                (when lp
                  (push (point-to-coordinate-string lp) attributes)
                  (push :lp attributes)))))
          (push (make-instance 'dot:attributed
                               :object child
                               :attributes attributes)
                out))))))

(defun compute-dot-graph (record stream arc-drawer arc-drawing-options)
  "Compute the CL-DOT:GRAPH representation of the RECORD."
  (let ((*id* -1)
        (*dot-stream* stream)
        (*arc-drawer* (cons arc-drawer arc-drawing-options)))
    (with-slots (climi::orientation) record
      (dot:generate-graph-from-roots record (clim:graph-root-nodes record)
                                     (list :rankdir
                                           (ecase climi::orientation
                                             (:horizontal "LR")
                                             (:vertical "TB")))))))

(defun perform-layout (graph-record stream arc-drawer arc-drawing-options)
  "Perform the layout for GRAPH-RECORD by creating the CL-DOT::GRAPH
description, calling the DOT-PROCESSOR to perform the layout, and saving the
results in the DOT-LAYOUT and DOT-BOUNDING-BOX slots."
  (let ((input-dot-graph (compute-dot-graph graph-record stream arc-drawer arc-drawing-options)))
    (with-slots (dot-processor dot-processor-options dot-layout dot-bounding-box dot-id-to-record layout-override)
        graph-record
      (let ((dot-graph (apply dot-processor graph-record input-dot-graph
                              :directed (typep graph-record 'dot-digraph-output-record)
                              (append (if (not (null layout-override))
                                          (list :noop 2))
                                      dot-processor-options))))
        (setf dot-layout
              (make-dot-layout dot-graph dot-id-to-record))
        (setf dot-bounding-box
              (coordinate-string-to-rectangle (getf (dot::attributes-of dot-graph) :bb)))))))

(defun call-with-dot-transformation (thunk medium graph-record)
  "Call THUNK with the transformation on MEDIUM set to translate GRAPH-RECORD's
DOT coordinate system ((0,0) is lower left) to CLIM's coordinate system ((0, 0)
is upper left)."
  (with-slots (dot-bounding-box) graph-record
    (clim:with-translation (medium (clim:rectangle-min-x dot-bounding-box)
                                   (- (clim:rectangle-max-y dot-bounding-box)
                                      (clim:rectangle-min-y dot-bounding-box)))
      (clim:with-scaling (medium 1 -1)
        (funcall thunk)))))

(defmacro with-dot-transformation ((medium graph-record) &body body)
  `(call-with-dot-transformation (lambda () ,@body) ,medium ,graph-record))

(defmethod clim:layout-graph-nodes :before ((graph-record dot-graph-output-record)
                                            stream arc-drawer arc-drawing-options)
  (perform-layout graph-record stream arc-drawer arc-drawing-options))

(defmethod clim:layout-graph-nodes ((graph-record dot-graph-output-record)
                                    stream arc-drawer arc-drawing-options)
  (with-slots (dot-layout dot-id-to-record) graph-record
    (with-dot-transformation (stream graph-record)
      (with-hash-table-iterator (next (node-layout dot-layout))
        (loop
          (multiple-value-bind (more node point) (next)
            (unless more (return))
            (let ((bb-width (clim:bounding-rectangle-width node))
                  (bb-height (clim:bounding-rectangle-height node)))
              (setf (clim:output-record-position node)
                    (clim:transform-position (clim:medium-transformation stream)
                                             (- (clim:point-x point) (/ bb-width 2))
                                             (+ (clim:point-y point) (/ bb-height 2))))
              (clim:add-output-record node graph-record))))))))

(defmethod clim:layout-graph-edges :around ((graph-record dot-graph-output-record)
                                            stream arc-drawer arc-drawing-options)
  (setf arc-drawer (or arc-drawer (make-instance 'dot-arc-drawer)))
  (call-next-method graph-record stream arc-drawer arc-drawing-options))

(defmethod clim:layout-graph-edges ((graph-record dot-graph-output-record)
                                    stream arc-drawer arc-drawing-options)
  (with-slots (dot-layout dot-id-to-record) graph-record
    (with-dot-transformation (stream graph-record)
      (with-hash-table-iterator (next (edge-layout dot-layout))
        (loop
          (multiple-value-bind (more edge layout) (next)
            (unless more (return))
            (destructuring-bind (from to) edge
              (destructuring-bind (splines lp) layout
                (let ((start-point (or (dot-spline-start (first splines))
                                       (first (dot-spline-points (first splines)))))
                      (end-point (or (dot-spline-end (a:last-elt splines))
                                     (a:last-elt (dot-spline-points (a:last-elt splines))))))
                  (apply arc-drawer stream from to
                         (clim:point-x start-point) (clim:point-y start-point)
                         (clim:point-x end-point) (clim:point-y end-point)
                         :splines splines
                         :label-center (unless (null lp) lp)
                         arc-drawing-options))))))))))
