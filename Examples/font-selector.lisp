;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2006 David Lichteblau (david@lichteblau.com)
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; A font selection dialog.
;;;
;;; Usage:
;;;   (clim-demo::select-font)
;;;   (clim-demo::select-font
;;;    :port (clim:find-port :server-path (list :ps :stream *standard-output*)))
;;;

(in-package #:clim-demo)

(defun select-font (&key (port (find-port)) calling-frame)
  (let ((frame (apply #'make-application-frame 'font-selector
                      :font-selector-port port
                      (when calling-frame
                        (list :calling-frame calling-frame)))))
    (run-frame-top-level frame)
    (font-selector-text-style frame)))

(define-application-frame font-selector ()
  ((font-selector-port :initarg :font-selector-port
                       :initform (find-port)
                       :accessor font-selector-port)
   (font-selector-text-style :accessor font-selector-text-style))
  (:menu-bar nil)
  (:panes
   (canvas :application
           :height 150
           :scroll-bars nil
           :display-time t
           :display-function 'display-font-preview)
   (family :list-pane :items nil
                      :name-key #'font-family-name
                      :value-changed-callback 'family-changed)
   (face :list-pane :items nil
                    :name-key #'font-face-name
                    :value-changed-callback 'face-changed)
   (size :list-pane :items nil
                    :value-changed-callback 'size-changed)
   (ok :push-button :label "OK"
                    :activate-callback (lambda (gadget)
                                         (frame-exit (gadget-client gadget))))
   (cancel :push-button :label "Cancel"
                        :activate-callback (lambda (gadget)
                                             (let ((frame (gadget-client gadget)))
                                               (setf (font-selector-text-style frame) nil)
                                               (frame-exit frame)))))
  (:layouts
   (default
    (vertically (:height 400 :width 600)
      (horizontally ()
        (labelling (:label "Family") (scrolling () family))
        (labelling (:label "Face") (scrolling () face))
        (labelling (:label "Size") (scrolling () size)))
      canvas
      (horizontally ()
        +fill+
        ok
        cancel)))))

(defmethod generate-panes :after (fm (frame font-selector))
  (reset-list-pane (find-pane-named frame 'family)
                   (port-all-font-families (font-selector-port frame))))

(defun family-changed (pane value)
  (declare (ignore pane))
  (let* ((face-list (find-pane-named *application-frame* 'face))
         (old-face (and (slot-boundp face-list 'climi::value)
                        (gadget-value face-list)))
         (new-faces (font-family-all-faces value)))
    (reset-list-pane face-list new-faces)
    (when old-face
      (setf (gadget-value face-list :invoke-callback t)
            (or (find (font-face-name old-face)
                      new-faces
                      :key #'font-face-name
                      :test #'equal)
                (first new-faces))))))

(defun face-changed (pane value)
  (declare (ignore pane))
  (let ((sizes (if value (font-face-all-sizes value) nil)))
    (reset-list-pane (find-pane-named *application-frame* 'size)
                     sizes
                     (or (position-if (lambda (x) (>= x 20)) sizes) 0))))

(defun size-changed (pane value)
  (declare (ignore pane))
  (setf (font-selector-text-style *application-frame*)
        (let ((face
               (gadget-value (find-pane-named *application-frame* 'face))))
          (if (and face value)
              (font-face-text-style face value)
              nil)))
  (display-font-preview *application-frame*
                  (frame-standard-output *application-frame*)))

(defun reset-list-pane (pane items &optional (index 0))
  ;; umm
  (setf (clime:list-pane-items pane :invoke-callback nil) items)
  (setf (gadget-value pane :invoke-callback t)
        (or (and (slot-boundp pane 'climi::value) (gadget-value pane))
            (let ((values (climi::generic-list-pane-item-values pane)))
              (if (plusp (length values))
                  (elt values index)
                  nil)))))

(defmethod display-font-preview (frame stream)
  (window-clear stream)
  (let* ((pane-width (rectangle-width (sheet-region stream)))
         (pane-height (rectangle-height (sheet-region stream)))
         (str "The quick brown fox jumps over the lazy dog")
         (style (font-selector-text-style frame))
         (ok nil))
    (cond ((not (eq (port frame) (font-selector-port frame)))
           (setf str (format nil
                             "Cannot preview font for ~A"
                             (font-selector-port frame)))
           (setf style (make-text-style :sans-serif :italic :normal)))
          ((null style)
           (setf str "Error: Text style is null")
           (setf style (make-text-style :sans-serif :italic :normal)))
          (t
           (setf ok t)))
    (multiple-value-bind (width height final-x final-y baseline)
        (text-size stream str :text-style style)
      (declare (ignore final-x final-y))
      (let* ((x1 (/ (- pane-width width) 2))
             (y1 (/ (- pane-height height) 2))
             (y2 (+ y1 height))
             (ybase (+ y1 baseline)))
        (when ok
          (draw-line* stream 0 ybase (1- pane-width) ybase :ink +green+)
          (draw-line* stream 0 y1 (1- pane-width) y1 :ink +blue+)
          (draw-line* stream 0 y2 (1- pane-width) y2 :ink +blue+))
        (handler-case
            (draw-text* stream str x1 ybase :text-style style)
          (error (c)
            (princ c)))))))
