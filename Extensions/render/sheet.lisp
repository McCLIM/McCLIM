(in-package :mcclim-render)

(defclass image-sheet-mixin (mirrored-sheet-mixin render-mixin design)
  ((image :initform nil :reader image-sheet-image)
   (resize-image-p :initform t :reader image-sheet-resize-image-p)))

(defmethod (setf image-sheet-image) (img (sheet image-sheet-mixin))
  (when img
    (with-slots (image resize-image-p) sheet
      (setf resize-image-p nil)
      (setf image img))
    (port-register-mirror (port sheet) sheet sheet)))

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
  (with-slots (image resize-image-p) sheet
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	(sheet-region sheet)
      (let ((width (round (- max-x min-x)))
	    (height (round (- max-y min-y))))
	(setf image (%create-sheet-image sheet width height))))
    image))

(defmethod %set-image-region ((sheet image-sheet-mixin) region)
  (with-slots (image resize-image-p) sheet
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
      region
      (let ((width (round (- max-x min-x)))
	    (height (round (- max-y min-y))))
	(if resize-image-p
	    (setf image (%create-sheet-image sheet width height))
	    nil)))))

(defmethod allocate-space :before ((sheet image-sheet-mixin) width height)
  (let ((region (make-rectangle* 0 0 width height)))
    (%set-image-region sheet region)))
    

;;; utility function

(declaim (inline float-octet)
	 (ftype (function (single-float) fixnum) float-octet))
(defun float-octet (float)
  "Convert a float in the range 0.0 - 1.0 to an octet."
  (round (* float 255.0)))

