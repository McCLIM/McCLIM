(in-package #:mcclim-raster-image)

;;;
;;; port
;;;

(defclass rgb-image-port (raster-image-port)
  ())

(defmethod find-port-type ((type (eql :rgb-image)))
  (values 'rgb-image-port 'identity))

(defmethod realize-mirror ((port rgb-image-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (make-instance 'image-mirror-mixin)))
    (setf (mirror->%image port mirror) mirror)
    (multiple-value-bind (width height)
        (bounding-rectangle-size sheet)
      (%make-image mirror width height))
    mirror))

(defmethod destroy-mirror ((port rgb-image-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (sheet-direct-mirror sheet)))
    (setf (mirror->%image port mirror) nil)))

;;;
;;; Pixmap
;;;

(defclass rgb-image-pixmap (image-pixmap-mixin)
  ())

(defmethod allocate-pixmap ((medium raster-image-medium) width height)
  (let ((pixmap (make-instance 'rgb-image-pixmap :width width :height height)))
    (setf (mirror->%image (port medium) pixmap) pixmap)
    (%make-image pixmap width height)
    pixmap))
