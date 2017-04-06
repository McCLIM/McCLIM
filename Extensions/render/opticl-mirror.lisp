
(in-package :mcclim-render)

(defclass opticl-image-mirror-mixin (image-mirror-mixin)
  ())

(defmethod %create-mirror-image ((mirror opticl-image-mirror-mixin) width height)
  (with-slots (image) mirror
    (setf image (make-opticl-image width height)))
  (with-slots (dirty-region) mirror
    (setf dirty-region nil)))
