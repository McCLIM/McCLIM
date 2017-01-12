(in-package :mcclim-render)

(defclass image-mirror-mixin (render-mixin design)
  ((image :initform nil :reader image-mirror-image)
   (resize-image-p :initform t :reader image-mirror-resize-image-p)
   (dirty-region :initform nil)))

(defmethod (setf image-mirror-image) (img (mirror image-mirror-mixin))
  (when img
    (with-slots (image resize-image-p) mirror
      (setf resize-image-p nil)
      (setf image img))))

;;;
;;; protocols
;;;

;;; saving
(defgeneric save-mirror-image-to-file (mirror file format))
(defgeneric save-mirror-image-to-stream (mirror stream format))

;;; used by the medium
(defgeneric %make-image-mirror-get-function (mirror))
(defgeneric %make-image-mirror-set-function (mirror))

;;; used by the port
(defgeneric %make-image (mirror sheet))
(defgeneric %set-image-region (mirror region))

;;; used by subclasess
(defgeneric %create-mirror-image (mirror width height))

;;;
;;; implementation
;;;

(defmethod %make-image ((mirror image-mirror-mixin) sheet)
  (with-slots (image resize-image-p) mirror
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	(sheet-region sheet)
      (let ((width (ceiling (- max-x min-x)))
	    (height (ceiling (- max-y min-y))))
	(%create-mirror-image mirror (1+ width) (1+ height))))))

(defmethod %set-image-region ((mirror image-mirror-mixin) region)
  (with-slots (image resize-image-p) mirror
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
      region
      (let ((width (ceiling (- max-x min-x)))
	    (height (ceiling (- max-y min-y))))
	(if resize-image-p
	    (%create-mirror-image mirror (1+ width) (1+ height))
	    nil)))))

;;;
;;;
;;;

(defmethod notify-image-updated ((mirror image-mirror-mixin) region)
  (with-slots (dirty-region) mirror
    (if dirty-region
	(setf dirty-region (region-union dirty-region region))
	(setf dirty-region region))))

;;; utility function

(declaim (inline float-octet)
	 (ftype (function (single-float) fixnum) float-octet))
(defun float-octet (float)
  "Convert a float in the range 0.0 - 1.0 to an octet."
  (round (* float 255.0)))

