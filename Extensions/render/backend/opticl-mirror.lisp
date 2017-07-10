(in-package :mcclim-render-internals)

(defclass opticl-rgb-image-mirror-mixin (image-mirror-mixin)
  ())

(defmethod %create-mirror-image ((mirror opticl-rgb-image-mirror-mixin) width height)
  (with-slots (image) mirror
    (setf image (make-image :rgb width height :opticl)))
  (with-slots (dirty-region) mirror
    (setf dirty-region nil)))
