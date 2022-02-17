(in-package #:mcclim-raster-image)

;;;
;;; Stream
;;;

(defclass raster-image-stream (basic-pane
                               sheet-leaf-mixin
                               sheet-mute-input-mixin
                               sheet-mute-repainting-mixin
                               updating-output-stream-mixin
                               standard-extended-output-stream
                               standard-output-recording-stream
                               permanent-medium-sheet-output-mixin)
  ())

(defun make-raster-image-stream (port)
  (make-instance 'raster-image-stream :port port))

;;; ?

(defmethod pane-viewport ((stream raster-image-stream))
  nil)

(defmethod scroll-extent ((stream raster-image-stream) x y)
  (declare (ignore x y))
  (values))
