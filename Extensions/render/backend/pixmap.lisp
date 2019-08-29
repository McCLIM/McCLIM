(cl:in-package #:mcclim-render-internals)

(defclass image-pixmap-mixin (image-sheet-mixin
                              mirrored-sheet-mixin
                              mirrored-pixmap
                              basic-sheet
                              design)
  ;; For REGION slot, MIRRORED-PIXMAP has :initform nil, BASIC-SHEET
  ;; has :type region :initform (make-bounding-rectangle ...). Specify
  ;; an initform that satisfies the type.
  ((climi::region :initform (make-bounding-rectangle 0 0 100 100))))

(defmethod sheet-medium ((pixmap image-pixmap-mixin))
  (pixmap-medium pixmap))

(defmethod sheet-direct-mirror ((pixmap image-pixmap-mixin))
  (climi::port-lookup-mirror (port pixmap) pixmap))
