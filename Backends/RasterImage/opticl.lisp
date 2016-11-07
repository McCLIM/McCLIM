(in-package :mcclim-raster-image)

;;;
;;; Top level pane
;;;
(defclass opticl-top-level-pane (raster-image-top-level-pane
				 mcclim-render::opticl-sheet-mixin)
  ())

(defun make-opticl-top-level-pane (port format)
  (declare (ignore format))
  (let ((tlp (make-instance 'opticl-top-level-pane
			    :enabled-p nil :port port)))
    tlp))

(defmethod make-raster-top-level-sheet ((port raster-image-port) (format (eql :png)))
  (make-opticl-top-level-pane port format))
(defmethod make-raster-top-level-sheet ((port raster-image-port) (format (eql :jpeg)))
  (make-opticl-top-level-pane port format))
(defmethod make-raster-top-level-sheet ((port raster-image-port) (format (eql :jpg)))
  (make-opticl-top-level-pane port format))
(defmethod make-raster-top-level-sheet ((port raster-image-port) (format (eql :tiff)))
  (make-opticl-top-level-pane port format))
(defmethod make-raster-top-level-sheet ((port raster-image-port) (format (eql :tif)))
  (make-opticl-top-level-pane port format))
(defmethod make-raster-top-level-sheet ((port raster-image-port) (format (eql :pbm)))
  (make-opticl-top-level-pane port format))
(defmethod make-raster-top-level-sheet ((port raster-image-port) (format (eql :pgm)))
  (make-opticl-top-level-pane port format))
(defmethod make-raster-top-level-sheet ((port raster-image-port) (format (eql :ppm)))
  (make-opticl-top-level-pane port format))
(defmethod make-raster-top-level-sheet ((port raster-image-port) (format (eql :gif)))
  (make-opticl-top-level-pane port format))
