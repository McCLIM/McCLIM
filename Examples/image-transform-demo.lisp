;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2018 Elias Mårtenson <lokedhs@gmail.com>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Demonstrates the application of transforms to image patterns and
;;; text.

(defpackage #:clim-demo.image-transform-demo
  (:use #:clim-lisp)
  (:export #:image-transform-demo))

(in-package #:clim-demo.image-transform-demo)

(defun make-slider (initial-value min-value max-value)
  (clim:make-pane :slider
                  :value initial-value
                  :show-value-p nil
                  :orientation :horizontal
                  :min-value min-value
                  :max-value max-value
                  :value-changed-callback 'slider-updated-callback
                  :drag-callback 'slider-updated-callback))

(clim:define-application-frame image-transform-demo ()
  ((image :initarg :image
          :accessor image))
  (:menu-bar nil)
  (:panes (image-demo :application
                      :display-function 'display-image-demo
                      :scroll-bars :both
                      :incremental-redisplay t)
          (rot-slider (make-slider 0 0 (* pi 2)))
          (x-slider (make-slider 0 0 200))
          (y-slider (make-slider 0 0 200))
          (scale-slider (make-slider 1 0.1 5))
          (x-skew-slider (make-slider 0 -1 1))
          (y-skew-slider (make-slider 0 -1 1)))
  (:layouts (default (clim:vertically ()
                       (7/10 (clim:labelling (:label "Test Image"
                                              :align-x :center
                                              :label-alignment :top)
                               image-demo))
                       (clim:horizontally ()
                         (clim:vertically ()
                           (1/2 (clim:labelling (:label "Rotate")
                                  rot-slider))
                           (1/2 (clim:labelling (:label "Translate X")
                                  x-slider)))
                         (clim:vertically ()
                           (1/2 (clim:labelling (:label "Translate Y")
                                  y-slider))
                           (1/2 (clim:labelling (:label "Scale")
                                  scale-slider)))
                         (clim:vertically ()
                           (1/2 (clim:labelling (:label "Skew X")
                                  x-skew-slider))
                           (1/2 (clim:labelling (:label "Skew Y")
                                  y-skew-slider))))))))

(defmethod initialize-instance :after ((instance image-transform-demo) &key)
  (setf (image instance)
        (clim:make-pattern-from-bitmap-file
         (merge-pathnames #p"images/kitten.jpg"
                          (asdf:system-source-directory :clim-examples)))))

(defun slider-updated-callback (gadget value)
  (declare (ignore value))
  (let* ((frame (clim:gadget-client gadget))
         (image-pane (clim:find-pane-named frame 'image-demo)))
    (clim:redisplay-frame-pane frame image-pane)))

(defun make-skew-transformation (x-skew y-skew)
  (clim:make-transformation 1 (tan x-skew)
                            (tan y-skew) 1
                            0 0))

(defun display-image-demo (frame stream)
  (clim:updating-output (stream)
    (let* ((image (image frame))
           (width (clim:pattern-width image))
           (height (clim:pattern-height image))
           (rotation (clim:gadget-value (clim:find-pane-named frame 'rot-slider)))
           (x-translation (clim:gadget-value (clim:find-pane-named frame 'x-slider)))
           (y-translation (clim:gadget-value (clim:find-pane-named frame 'y-slider)))
           (scale (clim:gadget-value (clim:find-pane-named frame 'scale-slider)))
           (x-skew (clim:gadget-value (clim:find-pane-named frame 'x-skew-slider)))
           (y-skew (clim:gadget-value (clim:find-pane-named frame 'y-skew-slider)))
           (tr (clim:compose-transformations
                (clim:make-translation-transformation x-translation y-translation)
                (clim:compose-transformations
                 (clim:make-scaling-transformation scale scale)
                 (clim:compose-transformations
                  (clim:make-rotation-transformation* rotation (/ width 2) (/ height 2))
                  (make-skew-transformation x-skew y-skew))))))
      (clim:with-drawing-options (stream :transformation tr)
        (clim:draw-design stream image)
        (clim:draw-rectangle* stream 0 0 width height :filled nil :ink clim:+blue+)
        (clim:with-text-size (stream 60)
          (clim:draw-text* stream "Can transform text, too" 100 100
                           :transform-glyphs t))))))

(defun image-transform-demo ()
  (let ((frame (clim:make-application-frame 'image-transform-demo)))
    (clim:run-frame-top-level frame)))
