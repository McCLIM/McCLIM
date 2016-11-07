(in-package :mcclim-render)

(defclass image-pixmap-mixin (image-sheet-mixin mirrored-pixmap design)
  ())

(defmethod image-sheet-image ((sheet image-pixmap-mixin))
  (pixmap-mirror sheet))
