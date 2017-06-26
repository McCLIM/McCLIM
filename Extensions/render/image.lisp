(in-package :mcclim-render)

(declaim (optimize speed))

;;;
;;; Image
;;;
(defclass image ()
  ((width :initform 0 :initarg :width :accessor image-width :type fixnum)
   (height :initform 0 :initarg :height :accessor image-height :type fixnum)
   (alpha-p :initform nil :initarg :alpha-p :accessor image-alpha-p)))

;;;
;;; Drawable Image
;;;
(defclass drawable-image (image)
  ())

(defgeneric map-rgb-color (drawable-image fn))
(defgeneric draw-image (sheet image &rest args
                        &key clipping-region transformation))
(defgeneric medium-draw-image* (medium image x y))

;;;
;;; Basic Image
;;;
(defclass basic-image (image)
  ((pixels :initarg :pixels
           :accessor image-pixels)))

(defgeneric image-pixels-type (image-class))
(defgeneric make-get-rgba-octets-code (image-class pixels-var x-var y-var))
(defgeneric make-set-rgba-octets-code (image-class pixels-var x-var y-var red-var grren-var blue-var alpha-var))
(defgeneric make-get-alpha-octets-code (image-class pixels-var x-var y-var))
(defgeneric make-set-alpha-octets-code (image-class pixels-var x-var y-var red-var grren-var blue-var alpha-var))

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
    (medium-draw-image* medium design x y)))

;;;
;;; Image Pattern
;;;
(defclass image-pattern (climi::pattern image-design)
  ())

(defmethod pattern-width ((pattern image-pattern))
  (image-width (image pattern)))

(defmethod pattern-height ((pattern image-pattern))
  (image-height (image pattern)))

(defmethod climi::medium-draw-pattern* (medium (pattern image-pattern) x y)
  (medium-draw-image* medium (image pattern) x y))
