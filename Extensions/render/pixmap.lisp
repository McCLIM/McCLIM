(in-package :mcclim-render)

(defclass image-pixmap-mixin (image-sheet-mixin mirrored-pixmap basic-sheet design)
  ())

(defmethod sheet-medium ((pixmap image-pixmap-mixin))
  (pixmap-medium pixmap))
