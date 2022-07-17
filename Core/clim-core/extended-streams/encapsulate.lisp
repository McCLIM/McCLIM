;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2001 by Tim Moore <moore@bricoworks.com>
;;;  (c) Copyright 2014 by Robert Strandh <robert.strandh@gmail.com>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-internals)

;;; The sheet protocols, as specified in Chapters Properties of Sheets
;;; and Sheet Protocols.

(def-stream-method sheetp ((stream standard-encapsulating-stream)))

(def-stream-method sheet-parent ((stream standard-encapsulating-stream)))

(def-stream-method sheet-children ((stream standard-encapsulating-stream)))

(def-stream-method sheet-adopt-child
    ((stream standard-encapsulating-stream)
     child))

(def-stream-method sheet-disown-child ((stream standard-encapsulating-stream)
                                       child
                                       &key errorp))

(def-stream-method sheet-siblings ((stream standard-encapsulating-stream)))

(def-stream-method sheet-enabled-children ((stream
                                            standard-encapsulating-stream)))

(def-stream-method sheet-ancestor-p ((stream standard-encapsulating-stream)
                                     putative-ancestor))

(def-stream-method raise-sheet ((stream standard-encapsulating-stream)))

(def-stream-method bury-sheet ((stream standard-encapsulating-stream)))

(def-stream-method reorder-sheets ((stream standard-encapsulating-stream)
                                   new-ordering))

(def-stream-method sheet-enabled-p ((stream standard-encapsulating-stream)))

(def-stream-method (setf sheet-enabled-p)
    (enabled-p (stream standard-encapsulating-stream)))

(def-stream-method sheet-viewable-p ((stream standard-encapsulating-stream)))

(def-stream-method sheet-occluding-sheets
    ((stream standard-encapsulating-stream)
     child))

(def-stream-method map-over-sheets (function
                                    (stream standard-encapsulating-stream)))

(def-stream-method sheet-transformation
    ((stream standard-encapsulating-stream)))

(def-stream-method (setf sheet-transformation)
    (transformation
     (stream standard-encapsulating-stream)))

(def-stream-method sheet-region ((stream standard-encapsulating-stream)))

(def-stream-method (setf sheet-region)
    (region
     (stream standard-encapsulating-stream)))

(def-stream-method move-sheet ((stream standard-encapsulating-stream)
                               x y))

(def-stream-method resize-sheet ((stream standard-encapsulating-stream)
                                 width height))

(def-stream-method move-and-resize-sheet
    ((stream standard-encapsulating-stream) x y width height))

(def-stream-method pane-viewport-region
    ((stream standard-encapsulating-stream)))

(def-stream-method map-sheet-position-to-parent
    ((stream standard-encapsulating-stream) x y))

(def-stream-method map-sheet-position-to-child
    ((stream standard-encapsulating-stream) x y))

(def-stream-method map-sheet-rectangle*-to-parent
    ((stream standard-encapsulating-stream) x1 y1 x2 y2))

(def-stream-method map-sheet-rectangle*-to-child
    ((stream standard-encapsulating-stream) x1 y1 x2 y2))

(def-stream-method map-over-sheets-containing-position
    (function (stream standard-encapsulating-stream) x y))

(def-stream-method map-over-sheets-overlapping-region
    (function (stream standard-encapsulating-stream) region))

(def-stream-method child-containing-position
    ((stream standard-encapsulating-stream) x y))

(def-stream-method children-overlapping-region
    ((stream standard-encapsulating-stream) region))

(def-stream-method children-overlapping-rectangle*
    ((stream standard-encapsulating-stream) x1 y1 x2 y2))

(def-stream-method sheet-delta-transformation
    ((stream standard-encapsulating-stream) ancestor))

(def-stream-method sheet-allocated-region
    ((stream standard-encapsulating-stream) child))

(def-stream-method sheet-event-queue ((stream standard-encapsulating-stream)))

(def-stream-method dispatch-event ((stream standard-encapsulating-stream)
                                   event))

(def-stream-method queue-event ((stream standard-encapsulating-stream)
                                event))

(def-stream-method schedule-event ((stream standard-encapsulating-stream)
                                   event delay))

(def-stream-method handle-event ((stream standard-encapsulating-stream)
                                 event))

(def-stream-method event-read ((stream standard-encapsulating-stream)))

(def-stream-method event-read-no-hang ((stream standard-encapsulating-stream)))

(def-stream-method event-peek ((stream standard-encapsulating-stream)
                               &optional event-type))

(def-stream-method event-unread ((stream standard-encapsulating-stream)
                                 event))

(def-stream-method event-listen ((stream standard-encapsulating-stream)))

(def-stream-method graft ((stream standard-encapsulating-stream)))

(def-stream-method port ((stream standard-encapsulating-stream)))

;;; trampoline methods

(def-stream-method medium-foreground ((stream standard-encapsulating-stream)))

(def-stream-method (setf medium-foreground)
    (design (stream standard-encapsulating-stream)))

(def-stream-method medium-background ((stream standard-encapsulating-stream)))

(def-stream-method (setf medium-background)
    (design (stream standard-encapsulating-stream)))

(def-stream-method medium-ink ((stream standard-encapsulating-stream)))

(def-stream-method (setf medium-ink)
    (design (stream standard-encapsulating-stream)))

(def-stream-method medium-transformation
    ((stream standard-encapsulating-stream)))

(def-stream-method (setf medium-transformation)
    (transformation (stream standard-encapsulating-stream)))

(def-stream-method medium-clipping-region
    ((stream standard-encapsulating-stream)))

(def-stream-method (setf medium-clipping-region)
    (region (stream standard-encapsulating-stream)))

(def-stream-method medium-line-style ((stream standard-encapsulating-stream)))

(def-stream-method (setf medium-line-style)
    (line-style (stream standard-encapsulating-stream)))

(def-stream-method medium-text-style ((stream standard-encapsulating-stream)))

(def-stream-method (setf medium-text-style)
    (text-style (stream standard-encapsulating-stream)))

(def-stream-method medium-default-text-style
    ((stream standard-encapsulating-stream)))

(def-stream-method (setf medium-default-text-style)
    (text-style (stream standard-encapsulating-stream)))

(def-stream-method text-size ((stream standard-encapsulating-stream) string
                              &key text-style start end))

(def-stream-method text-style-ascent (text-style (stream standard-encapsulating-stream)))

(def-stream-method text-style-descent (text-style (stream standard-encapsulating-stream)))

(def-stream-method text-style-height (text-style (stream standard-encapsulating-stream)))

(def-stream-method text-style-width (text-style (stream standard-encapsulating-stream)))

(def-stream-method text-style-fixed-width-p (text-style (stream standard-encapsulating-stream)))

(def-stream-method sheet-medium ((stream standard-encapsulating-stream)))

(def-stream-method invoke-with-sheet-medium-bound
    (continuation medium (stream standard-encapsulating-stream)))

(def-stream-method queue-repaint ((stream standard-encapsulating-stream)
                                  repaint-event))

(def-stream-method handle-repaint ((stream standard-encapsulating-stream)
                                   region))

(def-stream-method repaint-sheet ((stream standard-encapsulating-stream)
                                  region))

(def-stream-method note-sheet-grafted ((stream standard-encapsulating-stream)))

(def-stream-method note-sheet-degrafted
    ((stream standard-encapsulating-stream)))

(def-stream-method note-sheet-adopted ((stream standard-encapsulating-stream)))

(def-stream-method note-sheet-disowned
    ((stream standard-encapsulating-stream)))

(def-stream-method note-sheet-enabled ((stream standard-encapsulating-stream)))

(def-stream-method note-sheet-disabled
    ((stream standard-encapsulating-stream)))

;;; Text Style binding forms

(defmethod invoke-with-text-style ((stream standard-encapsulating-stream)
                                   continuation text-style)
  (invoke-with-text-style (slot-value stream 'stream)
                          #'(lambda (medium)
                              (declare (ignore medium))
                              (funcall continuation stream))
                          text-style))

;;; Drawing functions

(def-stream-method medium-draw-point* ((stream standard-encapsulating-stream)
                                       x y))

(def-stream-method medium-draw-points* ((stream standard-encapsulating-stream)
                                        coord-seq))

(def-stream-method medium-draw-line* ((stream standard-encapsulating-stream)
                                      x1 y1 x2 y2))

(def-stream-method medium-draw-lines* ((stream standard-encapsulating-stream)
                                       position-seq))

(def-stream-method medium-draw-polygon* ((stream standard-encapsulating-stream)
                                         coord-seq closed filled))

(def-stream-method medium-draw-bezigon* ((stream standard-encapsulating-stream)
                                         coord-seq filled))

(def-stream-method medium-draw-rectangle*
    ((stream standard-encapsulating-stream) x1 y1 x2 y2 filled))

(def-stream-method medium-draw-rectangles*
    ((stream standard-encapsulating-stream) coord-seq filled))

(def-stream-method medium-draw-ellipse*
    ((stream standard-encapsulating-stream)
     center-x center-y radius-1-dx radius-1-dy radius-2-dx radius-2-dy
     start-angle end-angle filled))

(def-stream-method medium-draw-text*
    ((stream standard-encapsulating-stream)
      string x y start end align-x align-y
     toward-x toward-y transform-glyphs))

(def-stream-method medium-finish-output
    ((stream standard-encapsulating-stream)))

(def-stream-method medium-force-output
    ((stream standard-encapsulating-stream)))

(def-stream-method medium-clear-area ((stream standard-encapsulating-stream)
                                      left top right bottom))

(def-stream-method medium-beep ((stream standard-encapsulating-stream)))

;;; Extended Output Streams

(def-stream-method extended-output-stream-p
    ((stream standard-encapsulating-stream)))

(def-stream-method stream-text-cursor ((stream standard-encapsulating-stream)))

(def-stream-method (setf stream-text-cursor)
    (cursor (stream standard-encapsulating-stream)))

(def-stream-method stream-cursor-position
    ((stream standard-encapsulating-stream)))

;;; A setf* method, but this should still work...
;;; (It didn't. --Hefner)
;;;(def-stream-method (setf stream-cursor-position)
;;;    (x y (stream standard-encapsulating-stream)))

(defmethod* (setf stream-cursor-position)
    (x y (stream standard-encapsulating-stream))
  (let ((*original-stream* stream)
        (stream (slot-value stream 'stream)))
    (setf (stream-cursor-position stream) (values x y))))

(def-stream-method stream-increment-cursor-position
    ((stream standard-encapsulating-stream) dx dy))

(def-stream-method stream-character-width
    ((stream standard-encapsulating-stream)
     character
     &key text-style))

(def-stream-method stream-string-width
    ((stream standard-encapsulating-stream)
     string
     &key start end text-style))

(def-stream-method stream-text-margin ((stream standard-encapsulating-stream)))

(def-stream-method (setf stream-text-margin)
    (margin (stream standard-encapsulating-stream)))

(def-stream-method stream-line-height ((stream standard-encapsulating-stream)
                                       &key text-style))

(def-stream-method stream-vertical-spacing
    ((stream standard-encapsulating-stream)))

(def-stream-method stream-baseline ((stream standard-encapsulating-stream)))

(def-stream-method stream-end-of-line-action
    ((stream standard-encapsulating-stream)))

(def-stream-method (setf stream-end-of-line-action)
    (action (stream standard-encapsulating-stream)))

(def-stream-method stream-end-of-page-action
    ((stream standard-encapsulating-stream)))

(def-stream-method (setf stream-end-of-page-action)
    (action (stream standard-encapsulating-stream)))

(defmethod invoke-with-temporary-page ((stream standard-encapsulating-stream)
                                       continuation
                                       &rest args &key margins move-cursor)
  (declare (ignore margins move-cursor))
  (flet ((trampoline (old-stream)
           (declare (ignore old-stream))
           (funcall continuation stream)))
    (declare (dynamic-extent #'trampoline))
    (apply #'invoke-with-temporary-page
           (slot-value stream 'stream) #'trampoline args)))

(def-stream-method medium-buffering-output-p
    ((stream standard-encapsulating-stream)))

(def-stream-method (setf medium-buffering-output-p)
    (buffered-p (stream standard-encapsulating-stream)))

(defmethod invoke-with-drawing-options ((medium standard-encapsulating-stream)
                                        continuation &rest drawing-options)
  (flet ((trampoline (old-medium)
           (declare (ignore old-medium))
           (funcall continuation medium)))
    (declare (dynamic-extent #'trampoline))
    (apply #'invoke-with-drawing-options
           (slot-value medium 'stream) #'trampoline drawing-options)))

(def-stream-method invoke-with-room-for-graphics
    (cont (stream standard-encapsulating-stream) &rest options))

;;; Extended Input Streams

(def-stream-method extended-input-stream-p
    ((stream standard-encapsulating-stream)))

(def-stream-method stream-input-buffer
    ((stream standard-encapsulating-stream)))

(def-stream-method (setf stream-input-buffer)
    (buffer (stream standard-encapsulating-stream)))

(def-stream-method stream-pointer-position
    ((stream standard-encapsulating-stream) &key pointer))

(defmethod* (setf stream-pointer-position)
    (x y (stream standard-encapsulating-stream))
  (let ((*original-stream* stream)
        (stream (slot-value stream 'stream)))
    (setf (stream-pointer-position stream) (values x y))))

(def-stream-method stream-set-input-focus
    ((stream standard-encapsulating-stream)))

(def-stream-method stream-read-gesture
    ((stream standard-encapsulating-stream)
     &key timeout peek-p input-wait-test input-wait-handler
     pointer-button-press-handler))

(def-stream-method stream-input-wait ((stream standard-encapsulating-stream)
                                      &key timeout input-wait-test))

(def-stream-method stream-unread-gesture
    ((stream standard-encapsulating-stream) gesture))

(def-stream-method stream-accept ((stream standard-encapsulating-stream) type
                           &key view default default-type provide-default
                           insert-default replace-input history active-p
                           prompt prompt-mode display-default query-identifier
                           activation-gestures additional-activation-gestures
                           delimiter-gestures additional-delimiter-gestures))
;;; Output recording

(def-stream-method output-recording-stream-p
    ((stream standard-encapsulating-stream)))

(def-stream-method stream-recording-p
    ((stream standard-encapsulating-stream)))

(def-stream-method (setf stream-recording-p)
    (recording-p (stream standard-encapsulating-stream)))

(def-stream-method stream-drawing-p
    ((stream standard-encapsulating-stream)))

(def-stream-method (setf stream-drawing-p)
    (drawing-p (stream standard-encapsulating-stream)))

(def-stream-method stream-output-history
    ((stream standard-encapsulating-stream)))

(def-stream-method stream-current-output-record
    ((stream standard-encapsulating-stream)))

(def-stream-method (setf stream-current-output-record)
    (record (stream standard-encapsulating-stream)))

(def-stream-method stream-add-output-record
    ((stream standard-encapsulating-stream) record))

(def-stream-method stream-replay
    ((stream standard-encapsulating-stream) &optional region))

(def-stream-method erase-output-record
    (record (stream standard-encapsulating-stream) &optional errorp))

(def-stream-method stream-text-output-record
    ((stream standard-encapsulating-stream) text-style))

(def-stream-method stream-close-text-output-record
    ((stream standard-encapsulating-stream)))

(def-stream-method stream-add-character-output
  ((stream standard-encapsulating-stream)
   character text-style width height baseline))

(def-stream-method stream-add-string-output
  ((stream standard-encapsulating-stream)
   string start end text-style width height baseline))

(defmethod invoke-with-output-recording-options
    ((stream standard-encapsulating-stream) continuation record draw)
  (invoke-with-output-recording-options
   (slot-value stream 'stream)
   #'(lambda (old-stream)
       (declare (ignore old-stream))
       (funcall continuation stream))
   record
   draw))

(defmethod invoke-with-new-output-record
    ((stream standard-encapsulating-stream) continuation record-type
     &rest initargs)
  (apply #'invoke-with-new-output-record
         (slot-value stream 'stream)
         #'(lambda (inner-stream output-record)
             (declare (ignore inner-stream))
             (funcall continuation stream output-record))
         record-type
         initargs))

(defmethod invoke-with-output-to-output-record
    ((stream standard-encapsulating-stream) continuation record-type
     &rest initargs)
  (apply #'invoke-with-output-to-output-record
         (slot-value stream 'stream)
         #'(lambda (inner-stream record)
             (declare (ignore inner-stream))
             (funcall continuation stream record))
         record-type
         initargs))

;;; Presentation type generics

(def-stream-method stream-default-view
    ((stream standard-encapsulating-stream)))

(def-stream-method (setf stream-default-view)
    (view (stream standard-encapsulating-stream)))

;;; Input editing
(def-stream-method invoke-with-input-editor-typeout
    ((stream standard-encapsulating-stream) continuation &key erase))
