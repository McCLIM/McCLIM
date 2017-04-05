(in-package :mcclim-render)

(declaim (optimize speed))

(deftype mask-image-data () 'opticl-core:8-bit-gray-image)

(defclass mask-image (image)
  ((data :type (or null mask-image-data))))

(defun make-mask-image (width height)
  (let ((data (opticl:make-8-bit-gray-image height width :initial-element 0)))
    (make-instance 'mask-image
		   :width width
		   :height height
		   :data data)))

(declaim (inline mask-image-data-get-alpha)
	 (ftype (function (mask-image-data fixnum fixnum) octet) mask-image-data-get-alpha))
(defun mask-image-data-get-alpha (data x y)
  (multiple-value-bind (alpha)
      (opticl:pixel data y x)
    alpha))
     
(declaim (inline rgb-image-data-set-alpha)
	 (ftype (function (mask-image-data fixnum fixnum octet) t) mask-image-data-set-alpha))
(defun mask-image-data-set-alpha (data x y alpha)
  (setf (opticl:pixel data y x)
	alpha))

;;;
;;;
;;;



(defgeneric %make-mask-image-draw-fn (image clip-region))
(defgeneric %make-mask-image-draw-span-fn (image clip-region)) 

;;; default implementation

(defmacro %make-mask-image-draw-function-macro (data)
  `(let ((data ,data))
     (declare (type mask-image-data data))
     (lambda (x y alpha)
       (declare (type fixnum x y)
		(type fixnum alpha))
       (when (or (null clip-region) (clim:region-contains-position-p clip-region x y))
	 (setf alpha (min (abs alpha) 255))
	 (when (plusp alpha)
	   (mask-image-data-set-alpha data x y alpha))))))

(defmacro %make-mask-image-draw-span-function-macro (data)
  `(let ((data ,data))
     (declare (type mask-image-data data))
     (lambda (x1 x2 y alpha)
       (declare (type fixnum x1 x2 y)
		(type fixnum alpha))
       (setf alpha (min (abs alpha) 255))
       (when (plusp alpha)
	 (loop for x from x1 below x2 do
	      (when (or (null clip-region) (clim:region-contains-position-p clip-region x y))
		(mask-image-data-set-alpha data x y alpha)))))))

(defmethod %make-mask-image-draw-fn ((image mask-image) clip-region)
  (%make-mask-image-draw-function-macro
   (image-data image)))

(defmethod %make-mask-image-draw-span-fn ((image mask-image) clip-region)
  (%make-mask-image-draw-span-function-macro
   (image-data image)))
