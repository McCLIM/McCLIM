(in-package :mcclim-raster-image)

;;;
;;; port
;;;

(defclass rgb-image-port (raster-image-port)
  ())

(defmethod find-port-type ((type (eql :rgb-image)))
  (values 'rgb-image-port 'identity))

(defmethod realize-mirror ((port rgb-image-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (make-instance 'image-mirror-mixin)))
    (port-register-mirror port sheet mirror)
    (%make-image mirror sheet)
    mirror))

(defmethod destroy-mirror ((port rgb-image-port) (sheet mirrored-sheet-mixin))
  (port-unregister-mirror port sheet (sheet-direct-mirror sheet)))

;;;
;;; Pixmap
;;;

(defclass rgb-image-pixmap (image-pixmap-mixin basic-pane)
  ((region :initform +nowhere+)))

(defmethod port-allocate-pixmap ((port rgb-image-port) sheet width height)
  (let ((pixmap (make-instance 'rgb-image-pixmap
                               :sheet sheet
                               :width width
                               :height height
                               :port port)))
    (when (sheet-grafted-p sheet)
      (realize-mirror port pixmap))
    pixmap))

(defmethod port-deallocate-pixmap ((port rgb-image-port) pixmap)
  (when (pixmap-mirror pixmap)
    (destroy-mirror port pixmap)))
