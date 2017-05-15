(in-package :mcclim-raster-image)

;;;
;;; port
;;;

(defclass opticl-image-port (raster-image-port)
  ())

(setf (get :opticl-image :port-type) 'opticl-image-port)
(setf (get :opticl-image :server-path-parser) 'parse-raster-image-server-path)

(defmethod realize-mirror ((port opticl-image-port) sheet)
  (setf (sheet-parent sheet) (graft port))
  (let ((mirror (make-instance 'mcclim-render::opticl-mirror-mixin)))
    (port-register-mirror port sheet mirror)
    (mcclim-render::%make-image mirror sheet)))

;;;
;;; Pixmap
;;;

(defclass opticl-image-pixmap (image-pixmap-mixin basic-pane)
  ())


(defmethod port-allocate-pixmap ((port opticl-image-port) sheet width height)
  (let ((pixmap (make-instance 'opticl-image-pixmap
			       :sheet sheet
			       :width width
			       :height height
			       :port port)))
    (when (sheet-grafted-p sheet)
      (realize-mirror port pixmap))
    pixmap))

(defmethod port-deallocate-pixmap ((port opticl-image-port) pixmap)
  (when (climi::port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))


