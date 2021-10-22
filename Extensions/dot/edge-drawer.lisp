;;;; mcclim-dot edge drawer
;;;;
;;;; This file is part of the mcclim-dot extension. See
;;;; Extensions/dot/README.md and Extensions/dot/LICENSE for more information.

(cl:in-package #:mcclim-dot)

(defclass dot-arc-drawer (c2mop:funcallable-standard-object)
  ((label-records
    :initform (make-hash-table :test 'equal))
   (edge-label-printer
    :initform (constantly nil)
    :initarg :edge-label-printer))
  (:documentation
   "A FUNCALLABLE-STANDARD-OBJECT, instances of which can be used as the
:ARC-DRAWER for FORMAT-GRAPH-FROM-ROOTS. It adds an extensible procedure for
drawing arcs, complete with labels.

DRAW-DOT-ARC is the primary arc drawing function. It delegates much of the work
to DRAW-DOT-EDGE and DRAW-DOT-LABEL.")
  (:metaclass c2mop:funcallable-standard-class))

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

(defmethod draw-dot-label ((drawer dot-arc-drawer) stream from to &rest args)
  (with-slots (edge-label-printer) drawer
    (apply edge-label-printer stream (graph-node-object from) (graph-node-object to) args)))

(defgeneric dot-label-record (drawer stream from to &key))

(defmethod dot-label-record ((drawer dot-arc-drawer) stream from to &rest args)
  (with-slots (label-records) drawer
    (values
     (a:ensure-gethash
      (list from to) label-records
      (multiple-value-bind (cursor-x cursor-y)
          (stream-cursor-position stream)
        (multiple-value-prog1
            (with-new-output-record (stream)
              (with-end-of-line-action (stream :allow)
                (apply #'draw-dot-label drawer stream from to args)))
          (stream-set-cursor-position stream cursor-x cursor-y)))))))

(defmethod draw-dot-arc ((drawer dot-arc-drawer) stream from to from-x from-y to-x to-y
                         &rest args
                         &key splines label-center)
  "Default method. Calls DRAW-DOT-EDGE and DRAW-DOT-LABEL."
  (a:remove-from-plistf args :splines :label-center)
  (apply #'draw-dot-edge drawer stream from to from-x from-y to-x to-y splines args)
  (unless (null label-center)
    (let* ((record (apply #'dot-label-record drawer stream from to args))
           (bb-width (bounding-rectangle-width record))
           (bb-height (bounding-rectangle-height record)))
      ;; shift the record so that the center is at the point chosen by the layout
      ;; engine.
      (setf (output-record-position record)
            (transform-position (medium-transformation stream)
                                (- (point-x label-center) (/ bb-width 2))
                                (+ (point-y label-center) (/ bb-height 2)))))))

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

(defmethod initialize-instance :after ((drawer dot-arc-drawer) &key)
  (c2mop:set-funcallable-instance-function drawer (a:curry #'draw-dot-arc drawer)))

(defparameter *dot-edge-label-fontsize* 14)
(defparameter *dot-edge-label-fontname* "Courier New")
(defparameter *dot-edge-label-width-ratio* 6/10)

(defgeneric dot-edge-label-to-graphviz-attributes (drawer stream from to &key))

(defmethod dot-edge-label-to-graphviz-attributes ((drawer dot-arc-drawer) stream from to &rest args)
  (let* ((record (apply #'dot-label-record drawer stream from to args))
         (bb-width (bounding-rectangle-width record))
         (bb-height (bounding-rectangle-height record))
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
