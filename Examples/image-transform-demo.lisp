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
                    :value-changed-callback #'slider-updated-callback))
  (:layouts (default (clim:vertically ()
                       (7/10 image-demo)
                       (1/10 rot-slider)
                       (1/10 x-slider)
                       (1/10 y-slider)))))

(defmethod initialize-instance :after ((obj image-transform-demo) &key)
  (setf (image-transform-demo/image obj)
        (clim:make-pattern-from-bitmap-file (merge-pathnames #p"images/kitten.jpg"
                                                             (asdf:system-source-directory :clim-examples)))))

(defun slider-updated-callback (gadget value)
  (declare (ignore gadget value))
  (let ((frame clim:*application-frame*))
    (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'image-demo))))

(defun display-image-demo (frame stream)
  (format stream "Rotation: ~s" (clim:gadget-value (clim:find-pane-named frame 'rot-slider)))
  (let ((image (image-transform-demo/image frame))
        (rotation (clim:gadget-value (clim:find-pane-named frame 'rot-slider)))
        (x-translation (clim:gadget-value (clim:find-pane-named frame 'x-slider)))
        (y-translation (clim:gadget-value (clim:find-pane-named frame 'y-slider))))
    (clim:with-drawing-options (stream :transformation (clim:compose-transformations
                                                        (clim:make-translation-transformation x-translation y-translation)
                                                        (clim:make-rotation-transformation rotation)))
      (clim:draw-pattern* stream image 100 100))))

(defun image-transform-demo ()
  (let ((frame (clim:make-application-frame 'image-transform-demo)))
    (clim:run-frame-top-level frame)))
