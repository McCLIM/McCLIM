
(in-package :mcclim-render)

(defclass rgba-image-mirror-mixin (image-mirror-mixin)
  ())

(defmethod %create-mirror-image ((mirror rgba-image-mirror-mixin) width height)
  (with-slots (image) mirror
    (setf image (make-rgba-image width height)))
  (with-slots (dirty-region) mirror
    (setf dirty-region nil)))
