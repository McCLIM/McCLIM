(in-package :mcclim-render-internals)

;;;
;;; Image
;;;
(defclass image ()
  ((width :initform 0 :initarg :width :accessor image-width :type fixnum)
   (height :initform 0 :initarg :height :accessor image-height :type fixnum)))

;;;
;;; Image mixins
;;;
(defclass image-mixin ()
  ())

(defclass rgba-image-mixin (image-mixin)
  ())

(defclass rgb-image-mixin (image-mixin)
  ())

(defclass gray-image-mixin (image-mixin)
  ())

(defgeneric image-type (image)
  (:method ((image rgba-image-mixin))
    :rgba)
  (:method ((image rgb-image-mixin))
    :rgb)
  (:method ((image gray-image-mixin))
    :gray))

(defvar *default-image-family* :two-dim-array)
(defgeneric image-family (image))
(defgeneric find-image-class (family type))

;;;
;;; Drawable Image
;;;
(defclass drawable-image (image)
  ())

(defgeneric map-rgb-color (drawable-image fn &key x y width height))

(defun draw-image* (medium image x y
                    &rest args
                    &key clipping-region transformation)
  (declare (ignorable clipping-region transformation args))
  (climi::with-medium-options (medium args)
    (medium-draw-image* medium image x y)))

(clim-internals::def-graphic-op draw-image* (image x y))

;;;
;;; Image Design
;;;
(defclass image-design (design)
  ((image :reader image
          :initarg :image)))

(defun make-image-design (image)
  (make-instance 'image-design :image image))

(defmethod clim:draw-design
    (medium (design image-design) &rest options
     &key (x 0) (y 0) &allow-other-keys)
  (climi::with-medium-options (medium options)
    (medium-draw-image* medium (slot-value design 'image) x y)))

;;;
;;; Image Pattern
;;;
(defclass image-pattern (pattern image-design)
  ())

(defmethod pattern-width ((pattern image-pattern))
  (image-width (image pattern)))

(defmethod pattern-height ((pattern image-pattern))
  (image-height (image pattern)))

(defmethod climi::medium-draw-pattern* (medium (pattern image-pattern) x y transformation)
  (multiple-value-bind (x y)
      (transform-position transformation x y)
    (medium-draw-image* medium (image pattern) x y)))

;;;
;;; Basic Image
;;;
(defclass basic-image (image)
  ((pixels :initarg :pixels
           :accessor image-pixels)))

;;;
;;; image I/O
;;;
(defgeneric read-image (source &key format image-class image-family))
(defgeneric write-image (image destination &key format quality))

(defvar *image-file-readers* (make-hash-table :test 'equalp)
  "A hash table mapping keyword symbols naming image
formats to a function that can read an image of that format. The
functions will be called with one argument, the pathname of the
file to be read.")

(defvar *image-file-writer* (make-hash-table :test 'equalp)
  "A hash table mapping keyword symbols naming image
formats to a function that can write an image of that format. The
functions will be called with two arguments, the image and the pathname of the
file to be read.")

(defmacro define-image-file-reader (format (&rest args) &body body)
  `(setf (gethash ,format *image-file-readers*)
         #'(lambda (,@args)
             ,@body)))

(defun image-format-read-supported-p (format)
  "Return true if FORMAT is supported by `read-image'."
  (not (null (gethash format *image-file-readers*))))

(defmacro define-image-file-writer (format (&rest args) &body body)
  `(setf (gethash ,format *image-file-writer*)
         #'(lambda (,@args)
             ,@body)))

(defun image-format-write-supported-p (format)
  "Return true if FORMAT is supported by `write-image'."
  (not (null (gethash format *image-file-writer*))))

;;;
;;; Image operations
;;;

(defgeneric make-image (image-class-or-type width height &optional image-family))
(defgeneric coerce-image (image image-class-or-type &optional image-family))
(defgeneric clone-image (image image-class-or-type  &optional image-family))
(defgeneric copy-image (src-image sx sy width height dst-image x y))
(defgeneric blend-image (src-image sx sy width height dst-image x y &key alpha))
(defgeneric crop-image (image sx sy width height &optional image-class-or-type image-family))
(defgeneric coerce-alpha-channel (image &optional image-class-or-type image-family))
(defgeneric clone-alpha-channel (image &optional image-class-or-type image-family))
(defgeneric copy-alpha-channel (src-image sx sy width height dst-image x y))
(defgeneric fill-image (image design stencil &key x y width height stencil-dx stencil-dy))
