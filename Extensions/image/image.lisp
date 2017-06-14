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

;;; RGB-PATTERNs must be treated specially...
(defmethod medium-draw-pattern* (medium (pattern rgb-pattern) x y)
  (medium-draw-image-design* medium pattern x y))

;;; Some image junk...

(defmethod medium-free-image-design ((sheet sheet-with-medium-mixin) design)
  (medium-free-image-design (sheet-medium sheet) design))

(defmethod medium-draw-image-design* :before (current-medium design x y)
  (with-slots (medium medium-data) design
    (unless (eq medium current-medium)
      (when medium
	(medium-free-image-design medium design))
      (setf medium current-medium)
      (setf medium-data nil))))

(defmethod medium-draw-image-design*
    ((medium sheet-with-medium-mixin) design x y)
  (medium-draw-image-design* (sheet-medium medium) design x y))

;;;; RGB images

(in-package :clim-internals)

(def-grecording draw-image-design (() image-design x y) ()
  (let ((width (image-width (image image-design)))
        (height (image-height (image image-design)))
	(transform (medium-transformation medium)))
    (setf (values x y) (transform-position transform x y))
    (values x y (+ x width) (+ y height))))

(defmethod* (setf output-record-position) :around
            (nx ny (record draw-image-design-output-record))
  (with-standard-rectangle* (:x1 x1 :y1 y1) record
    (with-slots (x y) record
      (let ((dx (- nx x1))
            (dy (- ny y1)))
        (multiple-value-prog1 (call-next-method)
          (incf x dx)
          (incf y dy))))))

(defrecord-predicate draw-image-design-output-record (x y image-design)
  (and (if-supplied (x coordinate)
	 (coordinate= (slot-value record 'x) x))
       (if-supplied (y coordinate)
	 (coordinate= (slot-value record 'y) y))
       (if-supplied (image-design rgb-image-design)
         (eq (slot-value record 'image-design) image-design))))
