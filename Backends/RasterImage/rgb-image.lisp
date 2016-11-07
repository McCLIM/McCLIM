(in-package :mcclim-raster-image)

;;;
;;; Top level pane
;;;
(defclass rgb-image-top-level-pane (raster-image-top-level-pane
				    mcclim-render::rgb-image-sheet-mixin)
  ())

(defun make-rgb-image-top-level-pane (port format)
  (declare (ignore format))
  (let ((tlp (make-instance 'rgb-image-top-level-pane
			    :enabled-p nil :port port)))
    tlp))

(defmethod make-raster-top-level-sheet ((port raster-image-port) (format (eql :rgb-image)))
  (make-rgb-image-top-level-pane port format))

;;;
;;; Pixmap
;;;

(defclass rgb-image-pixmap (rgb-image-pixmap-mixin basic-pane)
  ())


(defmethod port-allocate-pixmap ((port raster-image-port) sheet width height)
  (let ((pixmap (make-instance 'rgb-image-pixmap
			       :sheet sheet
			       :width width
			       :height height
			       :port port)))
    (when (sheet-grafted-p sheet)
      (realize-mirror port pixmap))
    pixmap))

(defmethod port-deallocate-pixmap ((port raster-image-port) pixmap)
  (when (port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))
