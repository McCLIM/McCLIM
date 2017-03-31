(in-package :mcclim-render)

;;;
;;; image base class
;;;

(defclass image ()
  ((width :initform 0 :initarg :width :accessor image-width :type fixnum)
   (height :initform 0 :initarg :height :accessor image-height :type fixnum)
   (data :initarg :data
	 :accessor image-data)))

(defgeneric save-image-to-file (image file))
(defgeneric save-image-to-stream (image stream format))

;;;
;;; rbga image
;;;

(deftype rgba-image-data () 'opticl-core:8-bit-rgba-image)

(defclass rgba-image (image)
  ((data :type (or null rgba-image-data))))

(defun make-rgba-image (width height)
  (let ((data (opticl:make-8-bit-rgba-image height width :initial-element 255)))
    (opticl:do-pixels (y x) data
      (setf (opticl:pixel data y x) (values 255 255 255 0)))
    (make-instance 'rgba-image
		   :width width
		   :height height
		   :data data)))

;;;
;;; alpha channel 
;;;

(deftype mask-image-data () 'opticl-core:8-bit-gray-image)

(defclass mask-image (image)
  ((data :type (or null mask-image-data))))

(defun make-mask-image (width height)
  (let ((data (opticl:make-8-bit-gray-image height width :initial-element 0)))
    (make-instance 'mask-image
		   :width width
		   :height height
		   :data data)))

;;;
;;; get/set
;;;

(declaim (inline float-octet)
	 (ftype (function (single-float) fixnum) float-octet))
(defun float-octet (float)
  "Convert a float in the range 0.0 - 1.0 to an octet."
  (round (* float 255.0)))

(declaim (inline rgba-image-data-get-pixel)
	 (ftype (function (rgba-image-data fixnum fixnum) t) rgba-image-data-get-pixel))
(defun rgba-image-data-get-pixel (data x y)
  (multiple-value-bind (r.bg g.bg b.bg a.bg)
      (opticl:pixel data y x)
    (values 
     (float (* r.bg (/ 1.0 255.0)))
     (float (* g.bg (/ 1.0 255.0)))
     (float (* b.bg (/ 1.0 255.0)))
     (float (* a.bg (/ 1.0 255.0))))))

(declaim (inline rgba-image-data-set-pixel)
	 (ftype (function (rgba-image-data fixnum fixnum float float float float) t) rgba-image-data-set-pixel))
(defun rgba-image-data-set-pixel (data x y red green blue alpha)
  (setf (opticl:pixel data y x)
	(values
	 (float-octet red)
	 (float-octet green)
	 (float-octet blue)
	 (float-octet alpha))))

(declaim (inline mask-image-data-get-alpha)
	 (ftype (function (mask-image-data fixnum fixnum) t) mask-image-data-get-alpha))
(defun mask-image-data-get-alpha (data x y)
  (multiple-value-bind (alpha)
      (opticl:pixel data y x)
    (float (* alpha (/ 1.0 255.0)))))
     
(declaim (inline rgb-image-data-set-alpha)
	 (ftype (function (mask-image-data fixnum fixnum float) t) mask-image-data-set-alpha))
(defun mask-image-data-set-alpha (data x y alpha)
  (setf (opticl:pixel data y x)
	(float-octet alpha)))

;;;
;;; conversion
;;;

(defgeneric coerce-to-clim-rgb-image (image)
  (:method ((image rgba-image))
    (let ((width (image-width image))
	  (height (image-height image))
	  (pixels (image-data image)))
      (let ((data (make-array (list height width)
			      :element-type '(unsigned-byte 32)
			      :initial-element #x00FFFFFF)))
	(let ((rgb-image (make-instance 'climi::rgb-image
					:width width
					:height height
					:alphap t
					:data data)))
	  (opticl:do-pixels (y x) pixels
	    (multiple-value-bind (red green blue alpha)
		(opticl:pixel pixels y x)
	      (setf (aref data y x)
		    (dpb red (byte 8 0)
			 (dpb green (byte 8 8)
			      (dpb blue (byte 8 16)
				   (dpb (- 255 alpha) (byte 8 24) 0)))))))
	  rgb-image))))
  (:method ((image climi::rgb-image))
    image))

(defgeneric coerce-to-opticl-image (image)
  (:method ((image rgba-image))
    (image-data image))
  (:method ((image climi::rgb-image))
    (let ((width (image-width image))
	  (height (image-height image)))
      (let ((optimg (opticl:make-8-bit-rgba-image height width :initial-element 255))
	    (data (image-data image)))
	(loop for y from 0 to (1- height)
	   do
	     (loop for x from 0 to (1- width)
		do
		  (setf (opticl:pixel optimg y x)
			(let ((p (aref data y  x)))
			  (let ((r (ldb (byte 8 0) p))
				(g (ldb (byte 8 8) p))
				(b (ldb (byte 8 16) p))
				(a (ldb (byte 8 24) p)))
			    (values r g b a))))))
	optimg)))
  (:method ((image mask-image))
    (let ((width (image-width image))
	  (height (image-height image)))
      (let ((optimg (opticl:make-8-bit-rgba-image height width :initial-element 255))
	    (data (image-data image)))
	(loop for y from 0 to (1- height)
	   do
	     (loop for x from 0 to (1- width)
		do
		  (setf (opticl:pixel optimg y x)
			(let ((a (round (* 255 (mask-image-data-get-alpha data x y)))))
			  (values a a a 255)))))
	optimg))))

;;;
;;; I/O
;;;

(defparameter *image-stream-writer-hash-table* (make-hash-table))
(map nil (lambda (z)
           (destructuring-bind (x y) z
             (setf (gethash x *image-stream-writer-hash-table*) y)))
     '((:tiff opticl:write-tiff-stream)
       (:tif opticl:write-tiff-stream)
       (:jpeg opticl:write-jpeg-stream)
       (:jpg opticl:write-jpeg-stream)
       (:png opticl:write-png-stream)
       (:pbm opticl:write-pbm-stream)
       (:pgm opticl:write-pgm-stream)
       (:ppm opticl:write-ppm-stream)
       (:gif opticl:write-gif-stream)))

(defmethod save-image-to-file (image file)
  (let ((optimg (coerce-to-opticl-image image)))
    (opticl:write-image-file file optimg)))
    
(defmethod save-image-to-stream (image stream format)
  (let ((fn (gethash format *image-stream-writer-hash-table*)))
    (if fn
	(funcall fn stream (coerce-to-opticl-image image))
	(error "Cannot write image stream: ~S" stream))))

;;;
;;;
;;;

(declaim (inline round-coordinate))
(defun round-coordinate (x)
  (floor (+ x .5)))

(declaim (inline float-blend))
(defun float-blend (r.bg g.bg b.bg a.bg r.fg g.fg b.fg a.fg alpha)
  (when (= alpha 0.0)
    (setf alpha 0.0000001))
  (multiple-value-bind (red green blue alpha)
      (color-blend-function  r.fg g.fg b.fg (float (* alpha a.fg)) r.bg g.bg b.bg a.bg)
    (values (float red) (float green) (float blue) (float alpha))))

(declaim (inline float-xor-pixel))
(defun float-xor-pixel (d1 d2)
  (float (/ (logxor (floor (* 255 d1)) (floor (* 255 d2))) 255)))

;;;
;;; 
;;;
