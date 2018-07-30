(defpackage :image-transform-demo
  (:use :cl)
  (:export :image-transform-demo))

(in-package :image-transform-demo)

(clim:define-application-frame image-transform-demo ()
  ((image :initarg :image
          :accessor image-transform-demo/image))
  (:panes (image-demo :application
                      :display-function 'display-image-demo)
          (rot-slider :slider
                      :value 0
                      :show-value t
                      :orientation :horizontal
                      :min-value 0
                      :max-value (* pi 2)
                      :value-changed-callback #'slider-updated-callback)
          (x-slider :slider
                    :value 0
                    :show-value t
                    :orientation :horizontal
                    :min-value 0
                    :max-value 200
                    :value-changed-callback #'slider-updated-callback)
          (y-slider :slider
                    :value 0
                    :show-value t
                    :orientation :horizontal
                    :min-value 0
                    :max-value 200
                    :value-changed-callback #'slider-updated-callback)
          (scale-slider :slider
                        :value 1
                        :show-value t
                        :orientation :horizontal
                        :min-value 0.1
                        :max-value 5
                        :value-changed-callback #'slider-updated-callback)
          (x-skew-slider :slider
                         :value 0
                         :show-value t
                         :orientation :horizontal
                         :min-value -1
                         :max-value 1
                         :value-changed-callback #'slider-updated-callback)
          (y-skew-slider :slider
                         :value 0
                         :show-value t
                         :orientation :horizontal
                         :min-value -1
                         :max-value 1
                         :value-changed-callback #'slider-updated-callback))
  (:layouts (default (clim:vertically (:width 1000 :height 900)
                       (14/20 image-demo)
                       (1/20 rot-slider)
                       (1/20 x-slider)
                       (1/20 y-slider)
                       (1/20 scale-slider)
                       (1/20 x-skew-slider)
                       (1/20 y-skew-slider)))))

(defmethod initialize-instance :after ((obj image-transform-demo) &key)
  (setf (image-transform-demo/image obj)
        (clim:make-pattern-from-bitmap-file (merge-pathnames #p"images/kitten.jpg"
                                                             (asdf:system-source-directory :clim-examples)))))

(defun slider-updated-callback (gadget value)
  (declare (ignore gadget value))
  (let ((frame clim:*application-frame*))
    (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'image-demo))))

(defun make-skew-transformation (x-skew y-skew)
  (clim:make-transformation (1+ (* (tan x-skew) (tan y-skew))) (tan x-skew)
                            (tan y-skew) 1
                            0 0))

(defun display-image-demo (frame stream)
  (let* ((image (image-transform-demo/image frame))
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
                (clim:make-rotation-transformation* rotation
                                                    (/ (clim:pattern-width image) 2)
                                                    (/ (clim:pattern-height image) 2))
                (make-skew-transformation x-skew y-skew))))))
    (clim:with-drawing-options (stream :transformation tr)
      (clim:draw-pattern* stream image 0 0)
      (clim:draw-rectangle* stream 0 0 (clim:pattern-width image) (clim:pattern-height image) :filled nil :ink clim:+blue+))))

(defun image-transform-demo ()
  (let ((frame (clim:make-application-frame 'image-transform-demo)))
    (clim:run-frame-top-level frame)))
