(in-package :mcclim-render)

(defclass image-sheet-mixin (mirrored-sheet-mixin design)
  ())

(defmethod allocate-space :before ((sheet image-sheet-mixin) width height)
  (when (sheet-mirror sheet)
    (let ((region (make-rectangle* 0 0 width height)))
      (%set-image-region (sheet-mirror sheet) region))))
