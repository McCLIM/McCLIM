;;;; Core functionality of mcclim-dot
;;;;
;;;; This file is part of the mcclim-dot extension. See
;;;; Extensions/dot/README.md and Extensions/dot/LICENSE for more information.

(cl:in-package #:mcclim-dot)

;;; Graph clases

(defclass dot-graph-output-record (standard-graph-output-record)
  ((dot-layout
    :documentation
    "Holds an instance of CL-DOT::GRAPH with the results of the layout.")
   (dot-bounding-box
    :documentation
    "A RECTANGLE specifying the bounding box of the DOT-LAYOUT.")
   (dot-id-to-record
    :initform (make-hash-table :test 'equal)
    :documentation
    "Maps a dot id to a node record.")
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

(defclass dot-digraph-output-record (dot-graph-output-record)
  ()
  (:documentation
   "A directed dot graph."))

(define-graph-type :dot-digraph dot-digraph-output-record)


;;; Utilities

(defvar *id* nil
  "Used to generate unique ids for every node in a graph.")

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
  (apply #'make-point
         (mapcar #'pn:parse-number (ss:split-sequence #\, string))))

(defun coordinate-string-to-rectangle (string)
  "Convert a string of four numbers, separated by a comma into a RECTANGLE."
  (apply #'make-rectangle*
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


;; CLIM graph to CL-DOT::GRAPH

(defmethod dot:graph-object-node ((record dot-graph-output-record) object)
  "Create a CL-DOT::NODE for the OBJECT. Create a unique ID, set the attributes
to match the OBJECT's bounding box, and save the ID in the RECORD's
DOT-ID-TO-RECORD map."
  (let* ((id (princ-to-string (incf *id*)))
         (node (make-instance 'dot:node
                              :id id
                              :attributes
                              `(:shape :rectangle
                                :fixedsize "true"
                                :width ,(float (pts-to-inches (bounding-rectangle-width object)))
                                :height ,(float (pts-to-inches (bounding-rectangle-height object)))))))
    (with-slots (dot-id-to-record) record
      (setf (gethash id dot-id-to-record) object)
      node)))

(defmethod dot:graph-object-points-to ((record dot-graph-output-record) object)
  "Return a children that OBJECT points to."
  (graph-node-children object))

(defun compute-dot-graph (record)
  "Compute the CL-DOT:GRAPH representation of the RECORD."
  (let ((*id* -1))
    (with-slots (climi::orientation) record
      (dot:generate-graph-from-roots record (graph-root-nodes record)
                                     (list :rankdir
                                           (ecase climi::orientation
                                             (:horizontal "LR")
                                             (:vertical "TB")))))))

(defun perform-layout (graph-record)
  "Perform the layout for GRAPH-RECORD by creating the CL-DOT::GRAPH
description, calling the DOT-PROCESSOR to perform the layout, and saving the
results in the DOT-LAYOUT and DOT-BOUNDING-BOX slots."
  (let ((input-dot-graph (compute-dot-graph graph-record)))
    (with-slots (dot-processor dot-processor-options dot-layout dot-bounding-box)
        graph-record
      (setf dot-layout
            (apply dot-processor graph-record input-dot-graph dot-processor-options))
      (setf dot-bounding-box
            (coordinate-string-to-rectangle (getf (dot::attributes-of dot-layout) :bb))))))

(defun call-with-dot-transformation (thunk medium graph-record)
  "Call THUNK with the transformation on MEDIUM set to translate GRAPH-RECORD's
DOT coordinate system ((0,0) is lower left) to CLIM's coordinate system ((0, 0)
is upper left)."
  (with-slots (dot-bounding-box) graph-record
    (with-translation (medium (rectangle-min-x dot-bounding-box)
                              (- (rectangle-max-y dot-bounding-box)
                                 (rectangle-min-y dot-bounding-box)))
      (with-scaling (medium 1 -1)
        (funcall thunk)))))

(defmacro with-dot-transformation ((medium graph-record) &body body)
  `(call-with-dot-transformation (lambda () ,@body) ,medium ,graph-record))

(defmethod layout-graph-nodes :before ((graph-record dot-graph-output-record)
                                       stream arc-drawer arc-drawing-options)
  (perform-layout graph-record))

(defmethod layout-graph-nodes ((graph-record dot-graph-output-record)
                               stream arc-drawer arc-drawing-options)
  (with-slots (dot-layout dot-id-to-record) graph-record
    (with-dot-transformation (stream graph-record)
      (dolist (dot-node (dot::nodes-of dot-layout))
        (let* ((node (gethash (dot::id-of dot-node) dot-id-to-record))
               (bb-width (bounding-rectangle-width node))
               (bb-height (bounding-rectangle-height node))
               (pos-attribute (getf (dot::attributes-of dot-node) :pos))
               (pos (coordinate-string-to-point pos-attribute)))
          (setf (output-record-position node)
                (transform-position (medium-transformation stream)
                                    (- (point-x pos) (/ bb-width 2))
                                    (+ (point-y pos) (/ bb-height 2))))
          (add-output-record node graph-record))))))

(defun dot-arc-drawer (stream from to from-x from-y to-x to-y &rest args &key splines
                       &allow-other-keys)
  (declare (ignore from-x from-y to-x to-y from to))
  (setf args (a:remove-from-plist :splines))
  (dolist (spline splines)
    (let ((start (dot-spline-start spline))
          (end (dot-spline-end spline))
          (points (dot-spline-points spline)))
      (apply #'mcclim-bezier:draw-bezier-design*
             stream (mcclim-bezier:make-bezier-curve points) args)
      (when start
        (apply #'clim:draw-arrow stream (first points) start args))
      (when end
        (apply #'clim:draw-arrow stream (a:last-elt points) end args)))))

(defmethod layout-graph-edges :around ((graph-record dot-graph-output-record)
                                       stream arc-drawer arc-drawing-options)
  (setf arc-drawer (or arc-drawer #'dot-arc-drawer))
  (call-next-method graph-record stream arc-drawer arc-drawing-options))

(defmethod layout-graph-edges ((graph-record dot-graph-output-record)
                               stream arc-drawer arc-drawing-options)
  (with-slots (dot-layout dot-id-to-record) graph-record
    (dolist (dot-edge (dot::edges-of dot-layout))
      (let* ((pos (getf (dot::attributes-of dot-edge) :pos))
             (splines (splines-string-to-splines pos))
             (from-dot-node (dot::source-of dot-edge))
             (from (gethash (dot::id-of from-dot-node) dot-id-to-record))
             (to-dot-node (dot::target-of dot-edge))
             (to (gethash (dot::id-of to-dot-node) dot-id-to-record))
             (start-point (or (dot-spline-start (first splines))
                              (first (dot-spline-points (first splines)))))
             (end-point (or (dot-spline-end (a:last-elt splines))
                            (a:last-elt (dot-spline-points (a:last-elt splines))))))
        (with-dot-transformation (stream graph-record)
          (apply arc-drawer stream from to
                 (point-x start-point) (point-y start-point)
                 (point-x end-point) (point-y end-point)
                 :splines splines
                 arc-drawing-options))))))
