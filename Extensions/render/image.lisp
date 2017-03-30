(in-package :mcclim-render)

(defclass image ()
  ((width :initarg :width :accessor image-width)
   (height :initarg :height :accessor image-height)
   (data :initarg :data
	 :accessor image-data)))

(deftype rgba-image-data () 'opticl-core:8-bit-rgba-image)

(defclass rgba-image (image)
  ((data :type (or null rgba-image-data))))

(defun make-rgba-image (width height)
  (let ((data (opticl:make-8-bit-rgba-image height width :initial-element 255)))
    (opticl:do-pixels (y x) data
      (setf (opticl:pixel data y x) (values 255 255 255 255)))
    (make-instance 'rgba-image
		   :width width
		   :height height
		   :data data)))

(deftype opacity-image-data () 'opticl-core:8-bit-gray-image)

(defclass opacity-image (image)
  ((data :type (or null opacity-image-data))))

(defun make-opacity-image (width height)
  (let ((data (opticl:make-8-bit-gray-image height width :initial-element 255)))
    (opticl:do-pixels (y x) data
      (setf (opticl:pixel data y x) (values 255 255 255 0)))
    (make-instance 'opacity-image
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

(declaim (notinline rgb-image-data-get-pixel)
	 (ftype (function (rgba-image-data fixnum fixnum) t) rgba-image-data-get-pixel))
(defun rgba-image-data-get-pixel (data x y)
  (multiple-value-bind (r.bg g.bg b.bg a.bg)
      (opticl:pixel data y x)
    (values 
     (float (/ r.bg 255)) (float (/ g.bg 255)) (float (/ b.bg 255)) (float (/ (- 255 a.bg) 255)))))

(declaim (notinline rgb-image-data-set-pixel)
	 (ftype (function (rgba-image-data fixnum fixnum float float float float) t) rgba-image-data-set-pixel))
(defun rgba-image-data-set-pixel (data x y red green blue alpha)
  (setf (opticl:pixel data y x)
	(values
	 (float-octet red)
	 (float-octet green)
	 (float-octet blue)
	 (- 255 (float-octet alpha)))))

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
				   (dpb alpha (byte 8 24) 0)))))))
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
				(a (- 255 (ldb (byte 8 24) p))))
			    (values r g b a))))))
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

(defun save-image-to-file (image file format)
  (declare (ignore format))
  (let ((optimg (coerce-to-opticl-image image)))
    (opticl:write-image-file file optimg)))
    
(defun save-image-to-stream (image stream format)
  (let ((fn (gethash format *image-stream-writer-hash-table*)))
    (if fn
	(let ((optimg (coerce-to-opticl-image image)))
	  (funcall fn stream optimg))
	(error "Cannot write image stream: ~S" stream))))
