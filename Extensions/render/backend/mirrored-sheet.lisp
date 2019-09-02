(cl:in-package #:mcclim-render-internals)

(defclass image-sheet-mixin (mirrored-sheet-mixin design)
  ())

(defmethod allocate-space :before ((sheet image-sheet-mixin) width height)
  (when-let ((mirror (sheet-mirror sheet)))
    (let ((region (make-rectangle* 0 0 width height)))
      (%set-image-region mirror region))))
