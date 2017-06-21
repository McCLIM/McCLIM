(in-package :mcclim-render)

(declaim (optimize speed))

;;;
;;; opticl image
;;;

(deftype opticl-image-data () 'opticl-core:8-bit-rgba-image)

(defclass opticl-image (image)
  ((data :type (or null opticl-image-data))))


(defgeneric %image-pixels (image))

(defmethod %image-pixels (image)
  (image-data image))

(defun make-opticl-image (width height)
  (let ((data (opticl:make-8-bit-rgba-image height width :initial-element 255)))
    (make-instance 'opticl-image
		   :width width
		   :height height
		   :data data)))

;;;
;;; get/set pixels
;;;

(declaim (inline opticl-image-data-get-pixel)
	 (ftype (function (opticl-image-data fixnum fixnum)
			  (values octet octet octet octet))
		opticl-image-data-get-pixel))
(defun opticl-image-data-get-pixel (data x y)
  (multiple-value-bind (r.bg g.bg b.bg a.bg)
      (opticl:pixel data y x)
    (values r.bg g.bg b.bg a.bg)))

(declaim (inline opticl-image-data-set-pixel)
	 (ftype (function (opticl-image-data fixnum fixnum octet octet octet octet)
			  t)
		opticl-image-data-set-pixel))
(defun opticl-image-data-set-pixel (data x y red green blue alpha)
  (setf (opticl:pixel data y x)
	(values red green blue alpha)))

;;;
;;; coerce
;;;

(defmethod coerce-to-clim-rgb-image ((image opticl-image))
  (let ((width (image-width image))
	(height (image-height image))
	(pixels (image-data image)))
    (declare (type opticl-image-data pixels))
    (let ((data (make-array (list height width)
			    :element-type '(unsigned-byte 32)
			    :initial-element #x00FFFFFF)))
      (let ((rgb-image (make-instance 'rgb-image
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

(defmethod coerce-to-opticl-image ((image rgb-image))
  (let ((width (image-width image))
	(height (image-height image)))
    (let ((optimg (opticl:make-8-bit-rgba-image height width :initial-element 255))
	  (data (image-data image)))
      (declare (type clim-rgb-image-data data))
      (declare (type opticl-image-data optimg))
      (loop for y from 0 to (1- height) do
	   (loop for x from 0 to (1- width) do
		(setf (opticl:pixel optimg y x)
		      (let ((p (aref data y  x)))
			(let ((r (ldb (byte 8 0) p))
			      (g (ldb (byte 8 8) p))
			      (b (ldb (byte 8 16) p))
			      (a (- 255 (ldb (byte 8 24) p))))
			  (values r g b a))))))
      optimg)))

(defmethod coerce-to-opticl-image ((image opticl-image))
  (image-data image))

(defmethod coerce-to-opticl-image ((image mask-image))
  (let ((width (image-width image))
	(height (image-height image)))
    (let ((optimg (opticl:make-8-bit-rgba-image height width :initial-element 255))
	  (data (image-data image)))
      (declare (type opticl-image-data optimg))
      (loop for y from 0 to (1- height)
	 do
	   (loop for x from 0 to (1- width)
	      do
		(setf (opticl:pixel optimg y x)
		      (let ((a (mask-image-data-get-alpha data x y)))
			(values a a a 255)))))
      optimg)))

;;;
;;; copy image
;;;

(defmethod copy-image ((image opticl-image)
		       (src-image opticl-image)
		       &key (x 0) (y 0)
			 (width (image-width image))
			 (height (image-height image))
			 (src-dx 0)
			 (src-dy 0))
  (declare (type fixnum x y width height src-dx src-dy))
  (def-copy-image
      opticl-image-data opticl-image-data-set-pixel
    opticl-image-data opticl-image-data-get-pixel))

;;;
;;; fill image
;;;

(defmethod fill-image ((image opticl-image) (rgba-design uniform-rgba-design) (mask (eql nil))
		       &key
			 (x 0) (y 0)
			 (width (image-width image)) (height (image-height image))
			 (mask-dx 0) (mask-dy 0))
  (declare (type fixnum x y width height mask-dx mask-dy)
	   (ignore mask-dx mask-dy))
  (let ((data-image (image-data image)))
    (declare (type opticl-image-data data-image))
    (make-fill-image-function
     (opticl-image-data-get-pixel data-image i j)
     (opticl-image-data-set-pixel data-image i j red green blue alpha)
     (values 
      (uniform-rgba-design-red rgba-design)
      (uniform-rgba-design-green rgba-design)
      (uniform-rgba-design-blue rgba-design)
      (uniform-rgba-design-alpha rgba-design))
     255))
  (make-rectangle* x y (+ x width) (+ y height)))

(defmethod fill-image ((image opticl-image) rgba-design (mask (eql nil))
		       &key
			 (x 0) (y 0)
			 (width (image-width image)) (height (image-height image))
			 (mask-dx 0) (mask-dy 0))
  (declare (type fixnum x y width height mask-dx mask-dy)
	   (ignore mask-dx mask-dy))
  (let ((data-image (image-data image))
	(source-fn (make-rgba-design-fn rgba-design)))
    (declare (type opticl-image-data data-image)
	     (type design-fn source-fn))
    (make-fill-image-function
     (opticl-image-data-get-pixel data-image i j)
     (opticl-image-data-set-pixel data-image i j red green blue alpha)
     (funcall source-fn i j)
     255))
  (make-rectangle* x y (+ x width) (+ y height)))

(defmethod fill-image ((image opticl-image) (rgba-design uniform-rgba-design) (mask mask-image)
		       &key
			 (x 0) (y 0)
			 (width (image-width image)) (height (image-height image))
			 (mask-dx 0) (mask-dy 0))
  (declare (type fixnum x y width height mask-dx mask-dy))
  (let ((data-image (image-data image))
	(data-mask (image-data mask)))
    (declare (type opticl-image-data data-image)
	     (type mask-image-data data-mask))
    (make-fill-image-function
     (opticl-image-data-get-pixel data-image i j)
     (opticl-image-data-set-pixel data-image i j red green blue alpha)
     (values 
      (uniform-rgba-design-red rgba-design)
      (uniform-rgba-design-green rgba-design)
      (uniform-rgba-design-blue rgba-design)
      (uniform-rgba-design-alpha rgba-design))
     (mask-image-data-get-alpha data-mask (+ mask-dx i) (+ mask-dy j))))
  (make-rectangle* x y (+ x width) (+ y height)))
  
(defmethod fill-image ((image opticl-image) rgba-design (mask mask-image)
		       &key
			 (x 0) (y 0)
			 (width (image-width image)) (height (image-height image))
			 (mask-dx 0) (mask-dy 0))
  (declare (type fixnum x y width height mask-dx mask-dy))
  (let ((data-image (image-data image))
	(data-mask (image-data mask))
	(source-fn (make-rgba-design-fn rgba-design)))
    (declare (type opticl-image-data data-image)
	     (type mask-image-data data-mask)
	     (type design-fn source-fn))
    (make-fill-image-function
     (opticl-image-data-get-pixel data-image i j)
     (opticl-image-data-set-pixel data-image i j red green blue alpha)
     (funcall source-fn i j)
     (mask-image-data-get-alpha data-mask (+ mask-dx i) (+ mask-dy j))))
  (make-rectangle* x y (+ x width) (+ y height)))

;;; private protocol
(defmethod %make-blend-draw-fn ((image opticl-image) clip-region design)
  (let ((source-fn (make-rgba-design-fn design)))
    (declare (type design-fn source-fn))
    (%make-blend-draw-function-macro
     opticl-image-data 
     (opticl-image-data-get-pixel data x y)
     (opticl-image-data-set-pixel data x y red green blue alpha)
     (funcall source-fn x y))))

(defmethod %make-blend-draw-span-fn ((image opticl-image) clip-region design)
  (let ((source-fn (make-rgba-design-fn design)))
    (declare (type design-fn source-fn))
    (%make-blend-draw-span-function-macro
     opticl-image-data 
     (opticl-image-data-get-pixel data x y)
     (opticl-image-data-set-pixel data x y red green blue alpha)
     (funcall source-fn x y))))

(defmethod %make-xor-draw-fn ((image opticl-image) clip-region design)
  (let ((source-fn (make-rgba-design-fn design)))
    (declare (type design-fn source-fn))
    (%make-xor-draw-function-macro
     opticl-image-data 
     (opticl-image-data-get-pixel data x y)
     (opticl-image-data-set-pixel data x y red green blue alpha)
     (funcall source-fn x y))))

(defmethod %make-xor-draw-span-fn ((image opticl-image) clip-region design)
  (let ((source-fn (make-rgba-design-fn design)))
    (declare (type design-fn source-fn))
    (%make-xor-draw-span-function-macro
     opticl-image-data 
     (opticl-image-data-get-pixel data x y)
     (opticl-image-data-set-pixel data x y red green blue alpha)
     (funcall source-fn x y))))

;;;
;;; Optimization
;;;

(defmethod %make-blend-draw-fn ((image opticl-image) clip-region (design uniform-rgba-design))
  (let ((s-red (uniform-rgba-design-red design))
	(s-green (uniform-rgba-design-green design))
	(s-blue (uniform-rgba-design-blue design))
	(s-alpha (uniform-rgba-design-alpha design))
	(mask (uniform-rgba-design-mask design)))
    (if mask
	(%make-blend-draw-function-macro
	 opticl-image-data 
	 (opticl-image-data-get-pixel data x y)
	 (opticl-image-data-set-pixel data x y red green blue alpha)
	 (if (region-contains-position-p mask x y)
	     (values s-red s-green s-blue s-alpha)
	     (values 0 0 0 0)))
	(%make-blend-draw-function-macro
	 opticl-image-data 
	 (opticl-image-data-get-pixel data x y)
	 (opticl-image-data-set-pixel data x y red green blue alpha)
	 (values s-red s-green s-blue s-alpha)))))

(defmethod %make-blend-draw-span-fn ((image opticl-image) clip-region (design uniform-rgba-design))
  (let ((s-red (uniform-rgba-design-red design))
	(s-green (uniform-rgba-design-green design))
	(s-blue (uniform-rgba-design-blue design))
	(s-alpha (uniform-rgba-design-alpha design))
	(mask (uniform-rgba-design-mask design)))
    (if mask
	(%make-blend-draw-span-function-macro
	 opticl-image-data 
	 (opticl-image-data-get-pixel data x y)
	 (opticl-image-data-set-pixel data x y red green blue alpha)
	 (if (region-contains-position-p mask x y)
	     (values s-red s-green s-blue s-alpha)
	     (values 0 0 0 0)))
	(%make-blend-draw-span-function-macro
	 opticl-image-data 
	 (opticl-image-data-get-pixel data x y)
	 (opticl-image-data-set-pixel data x y red green blue alpha)
	 (values s-red s-green s-blue s-alpha)))))
