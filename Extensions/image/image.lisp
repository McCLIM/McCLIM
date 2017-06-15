(in-package :mcclim-image)

;;; RGB image designs, efficient support for truecolor images. ARGB
;;; image data represented as an (unsigned-byte 32) array

(defclass rgb-image ()
    ((width :initarg :width :accessor image-width)
     (height :initarg :height :accessor image-height)
     (data :initarg :data
	   :accessor image-data
	   :type (or null (simple-array (unsigned-byte 32) (* *))))
     (alphap :initarg :alphap
	     :initform nil
	     :accessor image-alpha-p)))

;;; Applications (closure in particular) might want to cache any
;;; backend-specific data required to draw an RGB-IMAGE.
;;;
;;; To implement this caching, designs must be created separately for
;;; each medium, so that mediums can put their own data into them.

(defclass rgb-image-design (design)
    ((medium :initform nil :initarg :medium)
     (image :reader image
            :initarg :image)
     (medium-data :initform nil)))

(defun make-rgb-image-design (image)
  (make-instance 'rgb-image-design :image image))

;;; Protocol to free cached data

(defgeneric medium-free-image-design (medium design))

(defun free-image-design (design)
  (medium-free-image-design (slot-value design 'medium) design))


;;; Drawing protocol

(defgeneric medium-draw-image-design* (medium design x y))

;;; Fetching protocol

(defun sheet-rgb-image (sheet &key x y width height)
  (multiple-value-bind (data alphap)
      (sheet-rgb-data (port sheet)
		      sheet
		      :x x
		      :y y
		      :width width
		      :height height)
    (destructuring-bind (height width)
	(array-dimensions data)
      (make-instance 'rgb-image
	:width width
	:height height
	:data data
	:alphap alphap))))

(defgeneric sheet-rgb-data (port sheet &key x y width height))

;;; Most usages of WITH-MEDIUM-OPTIONS are in the file graphics.lisp,
;;; but it is also used in the method on DRAW-DESIGN specialized for
;;; RGB-IMAGE-DESIGN below.  For that reason, we need to define it
;;; here, so that it will not be considered to be an undefined
;;; function by default.

(defmethod draw-design
    (medium (design rgb-image-design) &rest options
     &key (x 0) (y 0) &allow-other-keys)
  (with-medium-options (medium options)
    (medium-draw-image-design* medium design x y)))

(defclass rgb-pattern (pattern rgb-image-design)
  ())

(defmethod pattern-width ((pattern rgb-pattern))
  (image-width (image pattern)))

(defmethod pattern-height ((pattern rgb-pattern))
  (image-height (image pattern)))




