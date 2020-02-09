;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307  USA.

(cl:in-package #:clouseau)

;;; Utilities

(defun sheet-medium* (sheet)
  ;; TODO needs some check so we don't attempt to draw onto e.g. grafts. but not this check
  (unless (climi::graftp sheet)
    (sheet-medium sheet)))

(defun sheet-visible-region (sheet)
  (or (pane-viewport-region sheet)
      (sheet-region sheet)))

(defun highlight-sheet (sheet &key (info t))
  (flet ((draw (stream)
           (let ((region (sheet-visible-region sheet)))
             (draw-design stream region :ink *checkers-design*)
             (when info
               (with-bounding-rectangle* (x1 y1 x2 y2) region
                 (let* ((text (format nil "~A~%~A" (type-of sheet) (pane-name sheet)))
                        (cx   (/ (+ x1 x2) 2))
                        (cy   (/ (+ y1 y2) 2)))
                   (multiple-value-bind (width height) (text-size sheet text)
                     (with-translation (sheet (- cx (/ width 2)) (- cy (/ height 2)))
                       (draw-rectangle* sheet 0 0 width height :ink +white+)))
                   (draw-text* sheet text cx cy
                               :align-x :center :align-y :center :ink +black+)))))))
    (if (output-recording-stream-p sheet)
        (with-output-recording-options (sheet :record nil :draw t)
          (draw sheet))
        (draw sheet))))

(defun unhighlight-sheet (sheet)
  (when-let ((medium (sheet-medium* sheet)))
    (draw-design medium (sheet-region sheet) :ink +background-ink+)
    (repaint-sheet sheet +everywhere+)))

;;; Object states

(defmethod object-state-class ((object sheet) (place t))
  'inspected-sheet)

(defclass inspected-sheet (inspected-instance)
  ())

(defmethod object-state-class ((object event) (place t))
  'inspected-event)

(defclass inspected-event (inspected-instance)
  ())

;;; Object inspection methods

(defun inspect-sheet-geometry (sheet stream)
  (when-let* ((region            (sheet-region sheet))
              (space-requirement (climi::pane-space-requirement sheet)))
    (let* ((preferred (make-rectangle* 0
                                       0
                                       (space-requirement-width space-requirement)
                                       (space-requirement-height space-requirement)))
           (min       (make-rectangle* 0
                                       0
                                       (space-requirement-min-width space-requirement)
                                       (space-requirement-min-height space-requirement)))
           (max       (make-rectangle* 0
                                       0
                                       (space-requirement-max-width space-requirement)
                                       (space-requirement-max-height space-requirement)))
           (width     (bounding-rectangle-width region))
           (height    (bounding-rectangle-height region))
           (factor    (/ 300 (max width height))))
      (flet ((legend (x y ink dashes label)
               (let ((x (+ x 150))
                     (y (- (* factor (- 150 (/ height 2))) 20)))
                (draw-line* stream x y (+ x 16) y :ink ink :line-dashes dashes)
                (draw-text* stream label (+ x 20) y :text-size :small :align-y :center))))
        (legend -150 0 +foreground-ink+ nil    "Sheet region")
        (legend  -50 0 +green+          '(4 4) "Minimum")
        (legend   50 0 +orange+         '(6 6) "Preferred")
        (legend  150 0 +red+            '(6 6) "Maximum"))

      (with-drawing-options (stream :text-size :tiny)
        (draw-text* stream (format nil "~,2F px" width) (* factor 150) (* factor (- 150 (/ height 2)))
                    :align-x :center)
        (with-translation (stream (* factor (- 150 (/ width 2))) (* factor 150))
          (with-rotation (stream (- (/ pi 2)))
            (draw-text* stream (format nil "~,2F px" height) 0 0
                        :align-x :center :transform-glyphs t))))

      (flet ((draw-maybe-infinite-rectangle (rectangle &rest args)
               (let ((width  (bounding-rectangle-width rectangle))
                     (height (bounding-rectangle-height rectangle)))
                 (unless (or (eql width +fill+) (eql height +fill+))
                   (with-translation (stream (/ (- 300 width) 2) (/ (- 300 height) 2))
                     (apply #'draw-design stream rectangle args))))))
        (with-scaling (stream factor factor)
          (draw-maybe-infinite-rectangle region                  :filled nil)
          (draw-maybe-infinite-rectangle min       :ink +green+  :filled nil :line-dashes '(4 4))
          (draw-maybe-infinite-rectangle preferred :ink +orange+ :filled nil :line-dashes '(6 6))
          (draw-maybe-infinite-rectangle max       :ink +red+    :filled nil :line-dashes '(8 8)))))))

(defmethod inspect-object-using-state ((object t)
                                       (state  inspected-sheet)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (with-preserved-cursor-x (stream)
    (formatting-table (stream)
      (formatting-row (stream)
        (formatting-cell (stream)
          (with-section (stream) "Geometry"
            (with-room-for-graphics (stream :first-quadrant nil)
              (inspect-sheet-geometry object stream))))
        (formatting-cell (stream)
          (with-section (stream) "Display options"
            (formatting-table (stream)
              (format-place-row stream object 'reader-place 'pane-foreground ; TODO as color
                                :label "Foreground")
              (format-place-row stream object 'reader-place 'pane-background
                                :label "Background")
              (format-place-row stream object 'reader-place 'pane-text-style
                                :label "Text style")))))))

  (call-next-method))

;;; Presentation methods

(define-presentation-method highlight-presentation ((type   inspected-sheet)
                                                    (record t)
                                                    (stream extended-output-stream)
                                                    (state  (eql :highlight)))
  (call-next-method)
  (let* ((object (presentation-object record))
         (sheet  (object object)))
    (when-let ((medium (sheet-medium* sheet)))
      (highlight-sheet sheet)
      (medium-finish-output medium))))

(define-presentation-method highlight-presentation ((type   inspected-sheet)
                                                    (record t)
                                                    (stream extended-output-stream)
                                                    (state  (eql :unhighlight)))
  (call-next-method)
  (let* ((object (presentation-object record))
         (sheet  (object object)))
    (when-let ((medium (sheet-medium* sheet)))
      (unhighlight-sheet sheet)
      (medium-finish-output medium))))

;;; Events

(define-presentation-method highlight-presentation ((type   inspected-event)
                                                    (record t)
                                                    (stream extended-output-stream)
                                                    (state  (eql :highlight)))
  (call-next-method)
  (let* ((object (presentation-object record))
         (event  (object object))
         (sheet  (event-sheet event)))
    (when-let ((medium (sheet-medium* sheet)))
      (highlight-sheet sheet :info nil)
      (when (typep event 'pointer-event)
        (draw-circle* sheet (pointer-event-native-x event) (pointer-event-native-y event) 5
                      :ink +dark-violet+))
      (medium-finish-output medium))))

(define-presentation-method highlight-presentation ((type   inspected-event)
                                                    (record t)
                                                    (stream extended-output-stream)
                                                    (state  (eql :unhighlight)))
  (call-next-method)
  (let* ((object (presentation-object record))
         (event  (object object))
         (sheet  (event-sheet event)))
    (when-let ((medium (sheet-medium* sheet)))
      (unhighlight-sheet sheet)
      (medium-finish-output medium))))

;;; Commands

(define-command (com-inspect-sheet :command-table inspector-command-table
                                   :name          t)
    ()
  (let* ((frame   *application-frame*)
         (port    (port frame))
         (stream  (frame-standard-input frame))
         (sheet   nil))
    (block nil
      (climb:with-pointer-grabbed (port stream)
        (tracking-pointer (stream :multiple-window t)
          (:pointer-motion (window)
            (when (and sheet (not (eq sheet window)))
              (unhighlight-sheet sheet))
            (when (and window (not (eq window sheet)))
              (highlight-sheet window))
            (setf sheet window))
          (:pointer-button-press (window)
            (when sheet
              (unhighlight-sheet sheet))
            (setf sheet window)
            (return)))))
    (setf (root-object (inspector-state) :run-hook-p t) sheet)))

(define-command (com-inspect-frame :command-table inspector-command-table
                                   :name          t)
    ()
  (let* ((application-frame *application-frame*)
         (port              (port application-frame))
         (stream            (frame-standard-input application-frame))
         (frame             nil))
    (block nil
      (climb:with-pointer-grabbed (port stream)
        (tracking-pointer (stream :multiple-window t)
          (:pointer-motion (window)
            (format *trace-output* "~A ~A~%" window (pane-frame window))
            (let ((new-frame (pane-frame window)))
              (when (and frame (not (eq frame new-frame)))
                (unhighlight-sheet (frame-top-level-sheet frame)))
              (when (and new-frame (not (eq new-frame frame)))
                (highlight-sheet (frame-top-level-sheet new-frame)))
              (setf frame new-frame)))
          (:pointer-button-press (window)
            (format *trace-output* "~A ~A~%" window (pane-frame window))
            (when frame
              (unhighlight-sheet (frame-top-level-sheet frame)))
            (setf frame (pane-frame window))
            (return)))))
    (setf (root-object (inspector-state) :run-hook-p t) frame)))
