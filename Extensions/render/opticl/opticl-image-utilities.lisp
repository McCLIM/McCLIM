(in-package :mcclim-render-internals)


;;;
;;;scale
;;;

(defmacro define-utilities-image (class)
  `(progn
     (defmethod scale-image ((image ,class) new-width new-height)
       (make-instance ',class
                      :width new-width
                      :height new-height
                      :pixels (opticl:resize-image (slot-value image 'pixels) new-height new-width)))

     (defmethod rotate-image* ((image ,class) angle center-x center-y)
       (let ((pre-shift (opticl:make-affine-transformation :y-shift (- center-y) :x-shift (- center-x)))
             (rotate (opticl:make-affine-transformation :theta angle))
             (post-shift (opticl:make-affine-transformation :y-shift center-y :x-shift center-x))
             (pixels (slot-value image 'pixels)))
         (make-instance ',class
                        :width (image-width image)
                        :height (image-height image)
                        :pixels (opticl:transform-image
                                 (opticl:transform-image
                                  (opticl:transform-image pixels pre-shift)
                                  rotate)
                                 post-shift))))
     (defmethod flip-image ((image ,class) direction)
       (make-instance ',class
                      :width (image-width image)
                      :height (image-height image)
                      :pixels (case direction
                                (:vertical
                                 (opticl:vertical-flip-image (slot-value image 'pixels)))
                                (:horizontal
                                 (opticl:horizontal-flip-image (slot-value image 'pixels)))
                                (:both
                                 (opticl:horizontal-flip-image
                                  (opticl:vertical-flip-image (slot-value image 'pixels)))))))))


(define-utilities-image opticl-rgba-image)
(define-utilities-image opticl-rgb-image)
(define-utilities-image opticl-gray-image)
