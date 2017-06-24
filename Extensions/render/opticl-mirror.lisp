
(in-package :mcclim-render)

(defclass opticl-rgb-image-mirror-mixin (image-mirror-mixin)
  ())

(defmethod %create-mirror-image ((mirror opticl-rgb-image-mirror-mixin) width height)
  (with-slots (image) mirror
    (setf image (make-opticl-rgb-image width height)))
  (with-slots (dirty-region) mirror
    (setf dirty-region nil)))
