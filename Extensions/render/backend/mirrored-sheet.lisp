(in-package #:mcclim-render-internals)

(defclass image-sheet-mixin (mirrored-sheet-mixin design)
  ())

(defmethod allocate-space :before ((sheet image-sheet-mixin) width height)
  (when-let ((image (mirror->%image (port sheet) (sheet-mirror sheet))))
    (%set-image-region image (make-rectangle* 0 0 width height))))
