(in-package :mcclim-render)

(defclass image-sheet-mixin (mirrored-sheet-mixin render-mixin)
  ((resize-image-p :initform t :reader image-sheet-resize-image-p)))

(defgeneric image-sheet-image (sheet))

(defmethod image-sheet-image ((sheet image-sheet-mixin))
  (sheet-mirror sheet))

(defmethod (setf image-sheet-image) (img (sheet image-sheet-mixin))
  (when img
    (with-slots (resize-image-p) sheet
      (setf resize-image-p nil)
      (port-register-mirror (port sheet) sheet img))))

;;;
;;; protocols
;;;

;;; saving
(defgeneric save-sheet-image-to-file (sheet file format))
(defgeneric save-sheet-image-to-stream (sheet stream format))

;;; used by the medium
(defgeneric %make-image-sheet-get-function (sheet))
(defgeneric %make-image-sheet-set-function (sheet))

;;; used by the port
(defgeneric %make-image (sheet))
(defgeneric %set-image-region (sheet region))

;;; used by subclasess
(defgeneric %create-sheet-image (sheet width height))

;;;
;;; implementation
;;;

(defmethod %make-image ((sheet image-sheet-mixin))
  (with-slots (resize-image-p) sheet
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	(sheet-region sheet)
      (let ((width (- max-x min-x))
	    (height (- max-y min-y)))
	(%create-sheet-image sheet width height)))))

(defmethod %set-image-region ((sheet image-sheet-mixin) region)
  (with-slots (resize-image-p) sheet
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	region
      (let ((width (- max-x min-x))
	    (height (- max-y min-y)))
	(if resize-image-p
	    (%create-sheet-image sheet width height)
	    nil)))))

;;; utility function

(declaim (inline float-octet)
	 (ftype (function (single-float) fixnum) float-octet))
(defun float-octet (float)
  "Convert a float in the range 0.0 - 1.0 to an octet."
  (round (* float 255.0)))
