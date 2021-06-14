;;; ---------------------------------------------------------------------------
;;;   License: BSD-2-Clause.
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2020 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; A testing application for the function medium-copy-area that uses extended
;;; blank-area presentation type.
;;;

(defpackage #:clim-demo.pixmaps
  (:use #:clim-lisp #:clim)
  (:export #:pixmaps))
(in-package #:clim-demo.pixmaps)

;;; This demo tests copying data between different mediums. Select a region on
;;; a pane by dragging a pressed mouse pointer and paste it by clicking on a
;;; target pane. That will lead to the invocation of MEDIUM-COPY-AREA. Copied
;;; data is not persistent - it is a low level protocol that copies pixels
;;; between drawables, so when the sheet is repainted (manually or by the WM),
;;; that data may be erased. The third pane is an exception, because it is
;;; repainted from a pixmap.
;;;
;;; The "Source" pane has displayed example data, the "Target" pane is another
;;; ordinary pane while the "Pixmap" pane represents the allocated pixmap -
;;; the pixmap itself is not visible because it is an off-screen window.
;;; Copying and pasting on the "Pixmap" pane in fact copies from a pixmap and
;;; pastes to a pixmap, and that pixmap data is copied to the pane with
;;; MEDIUM-COPY-AREA.
;;;
;;; As a bonus, this demo illustrates how to use blank-area for drag-and-drop
;;; transformations with a McCLIM-specific extension allowing to specify the
;;; sheet and to obtain both sheet and coordinates of the blank-area object.

(defun draw-grid (frame medium)
  (let ((inks (make-contrasting-inks 4)))
    (when (null frame)
      (setf inks (reverse inks)))
    (draw-rectangle* medium 10 10 90 90 :ink (elt inks 0))
    (draw-rectangle* medium 20 20 80 80 :ink (elt inks 1))
    (draw-rectangle* medium 30 30 70 70 :ink (elt inks 2))
    (draw-rectangle* medium 40 40 60 60 :ink (elt inks 3))))

(defclass pixmap-sheet (clim-stream-pane)
  ((pixmap :accessor sheet-pixmap :initform nil)))

(defun reset-pixmap (sheet)
  (alexandria:when-let ((pixmap (sheet-pixmap sheet)))
    (deallocate-pixmap pixmap))
  (setf (sheet-pixmap sheet)
        (with-output-to-pixmap (medium sheet :width 100 :height  100)
          (medium-clear-area medium 0 0 100 100)
          (draw-grid nil medium)))
  (repaint-sheet sheet +everywhere+))

(defmethod note-sheet-grafted :after ((sheet pixmap-sheet))
  (reset-pixmap sheet))

(defmethod note-sheet-degrafted :before ((sheet pixmap-sheet))
  (alexandria:when-let ((pixmap (sheet-pixmap sheet)))
    (deallocate-pixmap pixmap))
  (setf (sheet-pixmap sheet) nil))

(defmethod repaint-sheet ((sheet pixmap-sheet) region)
  (with-sheet-medium (medium sheet)
    (medium-copy-area (sheet-pixmap sheet) 0 0 100 100 medium 0 0)))

(defparameter *explanation*
  (format nil "Select a region by dragging a pointer over a sheet to ~
               copy a region and when selected press the left pointer ~
               button at the place where the region should be copied. ~
               Press the right pointer button to \"reset\" the sheet.~@

               Repainting the \"Source\" and the \"Target\" sheet (i.e ~
               because of the exposure event send by the window manager) ~
               will reset them. \"Pixmap\" sheets present the in-memory ~
               pixmap, so repainting sheets won't reset them.~@

               The \"Half Dead\" sheet should only allow pasting in ~
               the bottom half of its sheet area. The \"Dead\" sheet ~
               should not react to any of the actions except the ~
               unspecialized \"repaint\" action."))

(define-application-frame pixmaps ()
  ()
  (:panes (app1 :application :display-time t :display-function 'draw-grid
                             :scroll-bars nil :borders nil
                             :min-width 100 :max-width 100
                             :min-height 100 :max-height 100)
          (app2 :clim-stream-pane
                :min-width 100 :max-width 100
                :min-height 100 :max-height 100)
          (pix1 pixmap-sheet :min-width 100 :max-width 100
                             :min-height 100 :max-height 100)
          (pix2 pixmap-sheet :min-width 100 :max-width 100
                             :min-height 100 :max-height 100)
          (half-dead :clim-stream-pane
                     :display-function
                     (lambda (frame pane)
                       (declare (ignore frame))
                       (draw-rectangle* pane 0 0 100 50 :ink +gray30+))
                     :min-width 100 :max-width 100
                     :min-height 100 :max-height 100)
          (dead :clim-stream-pane
                :background +gray32+
                :min-width 100 :max-width 100
                :min-height 100 :max-height 100)
          (desc :application :end-of-line-action :wrap*
                             :background +beige+
                             :display-function (lambda (frame pane)
                                                 (declare (ignore frame))
                                                 (format pane *explanation*))
                             :scroll-bars nil))
  (:layouts (default (vertically ()
                       (:fill desc)
                       (horizontally ()
                         (labelling (:label "Source") app1)
                         (labelling (:label "Target") app2)
                         (labelling (:label "Pixmap 1") pix1)
                         (labelling (:label "Pixmap 2") pix2))
                       (horizontally ()
                         (labelling (:label "Half Dead") half-dead)
                         (labelling (:label "Dead") dead)))))
  (:menu-bar nil)
  (:pointer-documentation t))

(defun region-feedback (frame presentation stream x0 y0 x y state)
  (declare (ignore frame presentation))
  (with-identity-transformation (stream)
    (ecase state
      (:highlight
       (with-output-recording-options (stream :record nil)
         (draw-rectangle* stream x0 y0 x y :filled nil
                                           :line-thickness 3
                                           :line-dashes t)))
      (:unhighlight
       (let ((region (make-rectangle* (- (min x0 x) 2) (- (min y0 y) 2)
                                      (+ (max x0 x) 2) (+ (max y0 y) 2))))
         (with-output-recording-options (stream :record nil)
           (with-bounding-rectangle* (x0 y0 x y) region
             (draw-rectangle* stream x0 y0 x y :ink (pane-background stream))))
         (repaint-sheet stream region))))))

(defun paste-tester (object &key &allow-other-keys)
  (member (pane-name (event-sheet object)) '(app1 app2 pix1 pix2)))

(define-drag-and-drop-translator tr-select-region
    (blank-area command blank-area pixmaps :feedback region-feedback
                                           :tester paste-tester
                                           :menu nil)
    (object destination-object)
  (let ((sheet1 (event-sheet object))
        (x1 (pointer-event-x object))
        (y1 (pointer-event-y object))
        (x2 (pointer-event-x destination-object))
        (y2 (pointer-event-y destination-object)))
    (when (> x1 x2) (rotatef x1 x2))
    (when (> y1 y2) (rotatef y1 y2))
    (region-feedback nil nil sheet1 x1 y1 x2 y2 :highlight)
    (let* ((frame *application-frame*)
           (app1 (find-pane-named frame 'app1))
           (app2 (find-pane-named frame 'app2))
           (pix1 (find-pane-named frame 'pix1))
           (pix2 (find-pane-named frame 'pix2))
           (half-dead (find-pane-named frame 'half-dead))
           (dest (with-input-context (`(or (blank-area :sheet ,app1)
                                           (blank-area :sheet ,app2)
                                           (blank-area :sheet ,pix1)
                                           (blank-area :sheet ,pix2)
                                           (blank-area :sheet ,half-dead
                                                       :region ,(make-rectangle* 0 50 100 100)))
                                       :override t)
                     (object)
                     (loop (read-gesture))
                   (blank-area object)))
           (dest-sheet (event-sheet dest))
           (dest-x (pointer-event-x dest))
           (dest-y (pointer-event-y dest)))
      (region-feedback nil nil sheet1 x1 y1 x2 y2 :unhighlight)
      `(com-copy ,(pane-name sheet1)
                 ,(pane-name dest-sheet)
                 ,x1 ,y1 ,(- x2 x1) ,(- y2 y1)
                 ,dest-x ,dest-y))))

(define-presentation-action action-repaint-sheet
    (blank-area command pixmaps :gesture :describe :documentation "Repaint sheet")
    (object)
  (let ((sheet (event-sheet object)))
    (when (typep sheet 'pixmap-sheet)
      (reset-pixmap sheet))
    (repaint-sheet sheet +everywhere+)))

(define-pixmaps-command (com-copy :name "Copy")
    ((arg1 '(member app1 app2 pix1 pix2))
     (arg2 '(member app1 app2 pix1 pix2))
     (from-x 'integer)
     (from-y 'integer)
     (width 'integer)
     (height 'integer)
     (to-x 'integer)
     (to-y 'integer))
  (let* ((frame *application-frame*)
         (pane1 (find-pane-named frame arg1))
         (pane2 (find-pane-named frame arg2))
         (medium1 (if (typep pane1 'pixmap-sheet)
                      (sheet-pixmap pane1)
                      (sheet-medium pane1)))
         (medium2 (if (typep pane2 'pixmap-sheet)
                      (sheet-pixmap pane2)
                      (sheet-medium pane2))))
    (medium-copy-area medium1 from-x from-y width height medium2 to-x to-y)
    (mapc (lambda (p)
            (when (typep p 'pixmap-sheet)
              (repaint-sheet p +everywhere+)))
          (remove-duplicates (list pane1 pane2)))))

(defun pixmaps (&key (new-process t))
  (let* ((frame (make-application-frame 'pixmaps))
         (start (alexandria:curry #'run-frame-top-level frame)))
    (if new-process
        (clim-sys:make-process start)
        (funcall start))
    frame))
