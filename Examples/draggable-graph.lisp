;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2005 by Andy Hefner <ahefner@gmail.com>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Demo of draggable graph nodes
;;;

(in-package #:clim-demo)

(define-application-frame draggable-graph-demo () ()
  (:menu-bar nil)
  (:pane (make-pane 'application-pane
                    :width :compute
                    :height :compute
                    :display-function 'generate-graph
                    :display-time t)))

(defun generate-graph (frame pane)
  (declare (ignore frame))
  (format-graph-from-roots
   (list (find-class 'number))
   (lambda (object stream)
     (present (class-name object) (presentation-type-of object) :stream stream))
   #'c2mop:class-direct-subclasses
   :stream pane))

(defun find-graph-node (record)
  "Searches upward until a graph node parent of the supplied output record is found."
  (loop for current = record then (output-record-parent current)
        while current
        when (graph-node-output-record-p current)
          do (return current)))

(defun node-edges (node)
  (append (alexandria:hash-table-values (slot-value node 'climi::edges-from))
          (alexandria:hash-table-values (slot-value node 'climi::edges-to))))

(defun node-and-edges-region (node edges)
  (reduce #'region-union edges :key #'copy-rectangle
                               :initial-value (copy-rectangle node)))

(defun redisplay-edges (graph edges)
  (dolist (edge edges)
    (climi::layout-edge-1 graph (climi::from-node edge) (climi::to-node edge))))

;;; (AH) McCLIM bug of the day:
;;;
;;; (I haven't looked in detail at the spec or McCLIM to confirm my
;;; assumptions here, but as I understand things..)  CLIM regions are
;;; immutable. Output records ARE mutable. A McCLIM output record can
;;; be used as a rectangular region corresponding to its bounding
;;; rectangle.  But this bounding rectangle is not immutable! So,
;;; region operations such as region-union may build a rectangle-set
;;; capturing the mutable output-record object, violating the
;;; immutability of regions and causing widespread panic and
;;; confusion.
(defun copy-rectangle (region)
  (with-bounding-rectangle* (x0 y0 x1 y1) region
    ;; We use this rectangle to clear an area on the sheet which only
    ;; makes sense for integer coordinates.
    (make-rectangle* (floor x0) (floor y0) (ceiling x1) (ceiling y1))))

(define-draggable-graph-demo-command (com-drag-node)
    ((record t)
     (offset-x 'real :default 0)
     (offset-y 'real :default 0))
  (let* ((stream *standard-output*)
         (node-record (find-graph-node record))
         (edge-records (node-edges node-record))
         (graph-record (output-record-parent node-record))
         (erase-region))
    (assert (typep graph-record 'graph-output-record))
    (drag-output-record
     stream node-record
     :feedback (lambda (record stream old-x old-y x y mode)
                 (declare (ignore old-x old-y))
                 (ecase mode
                   (:erase
                    ;; Capture current regions before modifying the
                    ;; output records.
                    (setf erase-region
                          (node-and-edges-region record edge-records))
                    ;; Remove contents (i.e. lines) of edge output
                    ;; records. This does not repaint anything. To
                    ;; account for that, we include ERASE-REGION in
                    ;; the :DRAW clause.
                    (map nil #'clear-output-record edge-records))
                   (:draw
                    ;; Reposition the node record (this does not
                    ;; automatically replay the record).
                    (setf (output-record-position record)
                          (values (- x offset-x) (- y offset-y)))
                    ;; Regenerate child records of the edge records
                    ;; for the changed node position (without drawing
                    ;; since we will draw everything at once as a
                    ;; final step).
                    (with-output-recording-options (stream :record t :draw nil)
                      (redisplay-edges graph-record edge-records))
                    ;; Repaint all affected areas. This also replays
                    ;; the modified node and edge output records.
                    (repaint-sheet
                     stream (region-union (or erase-region +nowhere+)
                                          (node-and-edges-region
                                           record edge-records))))))
     :finish-on-release t :multiple-window nil)))

(define-presentation-to-command-translator record-dragging-translator
    (t com-drag-node draggable-graph-demo
     :tester ((object presentation)
              (find-graph-node presentation)))
    (object presentation x y)
  (multiple-value-bind (old-x old-y) (output-record-position presentation)
    (list presentation (- x old-x) (- y old-y))))
