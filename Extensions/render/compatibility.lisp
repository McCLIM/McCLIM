(in-package :mcclim-render-internals)

(deftype clim-rgb-image-data () '(simple-array (unsigned-byte 32) (* *)))

(defmethod %make-pixeled-design ((ink mcclim-image::rgb-pattern))
  (let* ((img (slot-value ink 'mcclim-image::image)))
    (make-pixeled-image-design :image
                               (make-instance 'rgb-image
                                              :width (mcclim-image::image-width img)
                                              :height (mcclim-image::image-height img)
                                              :pixels (mcclim-image::image-data img)))))
(defmethod coerce-image ((image basic-image)
                         (image-class (eql 'mcclim-image::rgb-image)) &optional image-family)
  (if (typep image 'mcclim-image::rgb-image)
      image
      (let ((img (coerce-image image 'rgb-image)))
        (make-instance 'mcclim-image::rgb-image
                       :width (image-width img)
                       :height (image-height img)
                       :data (image-pixels img)))))

(defmethod coerce-image ((image mcclim-image::rgb-image)
                         (image-class (eql 'rgb-image)) &optional image-family)
  (make-instance 'rgb-image
                 :width (mcclim-image::image-width image)
                 :height (mcclim-image::image-height image)
                 :pixels (mcclim-image::image-data image)))

(defmethod climi::medium-draw-pattern* (medium (pattern mcclim-image::rgb-pattern) x y)
  (medium-draw-image* medium
                      (coerce-image (slot-value pattern 'mcclim-image::image) 'rgb-image) x y))
