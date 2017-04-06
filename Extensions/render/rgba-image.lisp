(in-package :mcclim-render)

(declaim (optimize speed))

;;;
;;; rgba image
;;;

(deftype rgba-image-data () '(simple-array (unsigned-byte 32) (* *)))

(defclass rgba-image (image)
  ((data :type (or null rgba-image-data))))

(defun make-rgba-image (width height)
  (let ((data (make-array (list height width)
			  :element-type '(unsigned-byte 32)
			  :initial-element #xFFFFFFFF)))
    (make-instance 'rgba-image
		   :width width
		   :height height
		   :data data)))

;;;
;;; get/set pixels
;;;

(declaim (inline rgba-image-data-get-pixel)
	 (ftype (function (rgba-image-data fixnum fixnum)
			  (values octet octet octet octet))
		rgba-image-data-get-pixel))
(defun rgba-image-data-get-pixel (data x y)
  (let ((p (aref data y  x)))
    (let ((r (ldb (byte 8 0) p))
	  (g (ldb (byte 8 8) p))
	  (b (ldb (byte 8 16) p))
	  (a (- 255 (ldb (byte 8 24) p))))
      (values r g b a))))

(declaim (inline rgba-image-data-set-pixel)
	 (ftype (function (rgba-image-data fixnum fixnum octet octet octet octet)
			  t)
		rgba-image-data-set-pixel))
(defun rgba-image-data-set-pixel (data x y red green blue alpha)
  (setf (aref data y x)
	(dpb red (byte 8 0)
	     (dpb green (byte 8 8)
		  (dpb blue (byte 8 16)
		       (dpb (- 255 alpha) (byte 8 24) 0))))))
		  

;;;
;;; coerce
;;;

(defmethod coerce-to-clim-rgb-image ((image rgba-image))
  (let ((width (image-width image))
	(height (image-height image))
	(pixels (image-data image)))
    (declare (type rgba-image-data pixels))
    (let ((rgbimage (make-instance 'climi::rgb-image
				    :width width
				    :height height
				    :alphap t
				    :data pixels)))
      rgbimage)))

(defmethod coerce-to-opticl-image ((image rgba-image))
  (let ((width (image-width image))
	(height (image-height image)))
    (let ((optimg (opticl:make-8-bit-rgba-image height width :initial-element 255))
	  (data (image-data image)))
      (declare (type rgba-image-data data))
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


;;;
;;; copy image
;;;

(defmethod copy-image ((image rgba-image)
		       (src-image rgba-image)
		       &key (x 0) (y 0)
			 (width (climi::image-width image))
			 (height (climi::image-height image))
			 (src-dx 0)
			 (src-dy 0))
  (declare (type fixnum x y width height src-dx src-dy))
  (def-copy-image
      rgba-image-data rgba-image-data-set-pixel
    rgba-image-data rgba-image-data-get-pixel))


;;;
;;; fill image
;;;

(defmethod fill-image ((image rgba-image) (rgba-design uniform-rgba-design) (mask (eql nil))
		       &key
			 (x 0) (y 0)
			 (width (climi::image-width image)) (height (climi::image-height image))
			 (mask-dx 0) (mask-dy 0))
  (declare (type fixnum x y width height mask-dx mask-dy)
	   (ignore mask-dx mask-dy))
  (let ((data-image (image-data image)))
    (declare (type rgba-image-data data-image))
    (make-fill-image-function
     (rgba-image-data-get-pixel data-image i j)
     (rgba-image-data-set-pixel data-image i j red green blue alpha)
     (values 
      (uniform-rgba-design-red rgba-design)
      (uniform-rgba-design-green rgba-design)
      (uniform-rgba-design-blue rgba-design)
      (uniform-rgba-design-alpha rgba-design))
     255))
  (make-rectangle* x y (+ x width) (+ y height)))

(defmethod fill-image ((image rgba-image) rgba-design (mask (eql nil))
		       &key
			 (x 0) (y 0)
			 (width (climi::image-width image)) (height (climi::image-height image))
			 (mask-dx 0) (mask-dy 0))
  (declare (type fixnum x y width height mask-dx mask-dy)
	   (ignore mask-dx mask-dy))
  (let ((data-image (image-data image))
	(source-fn (make-rgba-design-fn rgba-design)))
    (declare (type rgba-image-data data-image)
	     (type design-fn source-fn))
    (make-fill-image-function
     (rgba-image-data-get-pixel data-image i j)
     (rgba-image-data-set-pixel data-image i j red green blue alpha)
     (funcall source-fn i j)
     255))
  (make-rectangle* x y (+ x width) (+ y height)))


(defmethod fill-image ((image rgba-image) (rgba-design uniform-rgba-design) (mask mask-image)
		       &key
			 (x 0) (y 0)
			 (width (climi::image-width image)) (height (climi::image-height image))
			 (mask-dx 0) (mask-dy 0))
  (declare (type fixnum x y width height mask-dx mask-dy))
  (let ((data-image (image-data image))
	(data-mask (image-data mask)))
    (declare (type rgba-image-data data-image)
	     (type mask-image-data data-mask))
    (make-fill-image-function
     (rgba-image-data-get-pixel data-image i j)
     (rgba-image-data-set-pixel data-image i j red green blue alpha)
     (values 
      (uniform-rgba-design-red rgba-design)
      (uniform-rgba-design-green rgba-design)
      (uniform-rgba-design-blue rgba-design)
      (uniform-rgba-design-alpha rgba-design))
     (mask-image-data-get-alpha data-mask (+ mask-dx i) (+ mask-dy j))))
  (make-rectangle* x y (+ x width) (+ y height)))
  
(defmethod fill-image ((image rgba-image) rgba-design (mask mask-image)
		       &key
			 (x 0) (y 0)
			 (width (climi::image-width image)) (height (climi::image-height image))
			 (mask-dx 0) (mask-dy 0))
  (declare (type fixnum x y width height mask-dx mask-dy))
  (let ((data-image (image-data image))
	(data-mask (image-data mask))
	(source-fn (make-rgba-design-fn rgba-design)))
    (declare (type rgba-image-data data-image)
	     (type mask-image-data data-mask)
	     (type design-fn source-fn))
    (make-fill-image-function
     (rgba-image-data-get-pixel data-image i j)
     (rgba-image-data-set-pixel data-image i j red green blue alpha)
     (funcall source-fn i j)
     (mask-image-data-get-alpha data-mask (+ mask-dx i) (+ mask-dy j))))
  (make-rectangle* x y (+ x width) (+ y height)))

;;;
;;;
;;;

;;;
;;; macro to build drawing function
;;;





;;; private protocol
(defmethod %make-blend-draw-fn ((image rgba-image) clip-region design)
  (let ((source-fn (make-rgba-design-fn design)))
    (declare (type design-fn source-fn))
    (%make-blend-draw-function-macro
     rgba-image-data 
     (rgba-image-data-get-pixel data x y)
     (rgba-image-data-set-pixel data x y red green blue alpha)
     (funcall source-fn x y))))

(defmethod %make-blend-draw-span-fn ((image rgba-image) clip-region design)
  (let ((source-fn (make-rgba-design-fn design)))
    (declare (type design-fn source-fn))
    (%make-blend-draw-span-function-macro
     rgba-image-data 
     (rgba-image-data-get-pixel data x y)
     (rgba-image-data-set-pixel data x y red green blue alpha)
     (funcall source-fn x y))))

(defmethod %make-xor-draw-fn ((image rgba-image) clip-region design)
  (let ((source-fn (make-rgba-design-fn design)))
    (declare (type design-fn source-fn))
    (%make-xor-draw-function-macro
     rgba-image-data 
     (rgba-image-data-get-pixel data x y)
     (rgba-image-data-set-pixel data x y red green blue alpha)
     (funcall source-fn x y))))

(defmethod %make-xor-draw-span-fn ((image rgba-image) clip-region design)
  (let ((source-fn (make-rgba-design-fn design)))
    (declare (type design-fn source-fn))
    (%make-xor-draw-span-function-macro
     rgba-image-data 
     (rgba-image-data-get-pixel data x y)
     (rgba-image-data-set-pixel data x y red green blue alpha)
     (funcall source-fn x y))))

;;;
;;; Optimization
;;;

(defmethod %make-blend-draw-fn ((image rgba-image) clip-region (design uniform-rgba-design))
  (let ((s-red (uniform-rgba-design-red design))
	(s-green (uniform-rgba-design-green design))
	(s-blue (uniform-rgba-design-blue design))
	(s-alpha (uniform-rgba-design-alpha design))
	(mask (uniform-rgba-design-mask design)))
    (if mask
	(%make-blend-draw-function-macro
	 rgba-image-data 
	 (rgba-image-data-get-pixel data x y)
	 (rgba-image-data-set-pixel data x y red green blue alpha)
	 (if (region-contains-position-p mask x y)
	     (values s-red s-green s-blue s-alpha)
	     (values 0 0 0 0)))
	(%make-blend-draw-function-macro
	 rgba-image-data 
	 (rgba-image-data-get-pixel data x y)
	 (rgba-image-data-set-pixel data x y red green blue alpha)
	 (values s-red s-green s-blue s-alpha)))))

(defmethod %make-blend-draw-span-fn ((image rgba-image) clip-region (design uniform-rgba-design))
  (let ((s-red (uniform-rgba-design-red design))
	(s-green (uniform-rgba-design-green design))
	(s-blue (uniform-rgba-design-blue design))
	(s-alpha (uniform-rgba-design-alpha design))
	(mask (uniform-rgba-design-mask design)))
    (if mask
	(%make-blend-draw-span-function-macro
	 rgba-image-data 
	 (rgba-image-data-get-pixel data x y)
	 (rgba-image-data-set-pixel data x y red green blue alpha)
	 (if (region-contains-position-p mask x y)
	     (values s-red s-green s-blue s-alpha)
	     (values 0 0 0 0)))
	(%make-blend-draw-span-function-macro
	 rgba-image-data 
	 (rgba-image-data-get-pixel data x y)
	 (rgba-image-data-set-pixel data x y red green blue alpha)
	 (values s-red s-green s-blue s-alpha)))))

