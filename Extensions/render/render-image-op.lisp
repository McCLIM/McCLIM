(in-package :mcclim-render)

(declaim (optimize speed))

;;;
;;; Image Operations
;;;

(defgeneric fill-image (image design mask &key x y width height 
					    mask-dx mask-dy))

(defgeneric copy-image (image src-image &key x y width height 
					  src-dx src-dy))

(defgeneric save-image-to-file (image file))
(defgeneric save-image-to-stream (image stream format))

;;;
;;; conversion
;;;

(defgeneric coerce-to-clim-rgb-image (image)
  (:method ((image rgb-image))
    image))

(defgeneric coerce-to-opticl-rgb-image (image))
 
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
  (let ((optimg (coerce-to-opticl-rgb-image image)))
    (opticl:write-image-file file optimg)))
    
(defmethod save-image-to-stream (image stream format)
  (let ((fn (gethash format *image-stream-writer-hash-table*)))
    (if fn
	(funcall fn stream (coerce-to-opticl-rgb-image image))
	(error "Cannot write image stream: ~S" stream))))

(declaim (inline round-coordinate))
(defun round-coordinate (x)
  (floor (+ x .5)))

;;;
;;; fill image
;;;

(defmacro make-fill-image-function (image-get-code image-set-code design-get-code aa-alpha-code)
  `(when (and (> width 0)
	      (> height 0))
     (let ((max-y (+ y height -1))
	   (max-x (+ x width -1)))
       (loop for j from y to max-y do
	    (loop for i from x to max-x do
		 (multiple-value-bind (red green blue alpha)
		     ,design-get-code
		   (let ((aa-alpha ,aa-alpha-code))
		     (if (> (octet-mult aa-alpha alpha) 250)
			 ,image-set-code
			 (multiple-value-bind (r.bg g.bg b.bg a.bg)
			     ,image-get-code
			   (multiple-value-bind (red green blue alpha)	  
			       (octet-blend-function r.bg g.bg b.bg a.bg red green blue (octet-mult alpha aa-alpha))
			     ,image-set-code))))))))))


;;;
;;; Opticl Image Operations
;;;

;;;
;;; get/set pixels
;;;

(declaim (inline opticl-rgb-image-data-get-pixel)
	 (ftype (function (opticl-rgb-image-data fixnum fixnum)
			  (values octet octet octet octet))
		opticl-rgb-image-data-get-pixel))
(defun opticl-rgb-image-data-get-pixel (data x y)
  (multiple-value-bind (r.bg g.bg b.bg a.bg)
      (opticl:pixel data y x)
    (values r.bg g.bg b.bg a.bg)))

(declaim (inline opticl-rgb-image-data-set-pixel)
	 (ftype (function (opticl-rgb-image-data fixnum fixnum octet octet octet octet)
			  t)
		opticl-rgb-image-data-set-pixel))
(defun opticl-rgb-image-data-set-pixel (data x y red green blue alpha)
  (setf (opticl:pixel data y x)
	(values red green blue alpha)))

;;;
;;; coerce
;;;

(defmethod coerce-to-clim-rgb-image ((image opticl-rgb-image))
  (let ((width (image-width image))
	(height (image-height image))
	(pixels (%image-pixels image)))
    (declare (type opticl-rgb-image-data pixels))
    (let ((data (make-array (list height width)
			    :element-type '(unsigned-byte 32)
			    :initial-element #xFFFFFFFF)))
      (let ((rgb-image (make-instance 'rgb-image
				      :width width
				      :height height
				      :alphap nil
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

(defmethod coerce-to-opticl-rgb-image ((image rgb-image))
  (let ((width (image-width image))
	(height (image-height image)))
    (let ((optimg (opticl:make-8-bit-rgba-image height width :initial-element 255))
	  (data (image-data image)))
      (declare (type clim-rgb-image-data data))
      (declare (type opticl-rgb-image-data optimg))
      (if (mcclim-image::image-alpha-p image)
          (loop for y from 0 to (1- height) do
               (loop for x from 0 to (1- width) do
                    (setf (opticl:pixel optimg y x)
                          (let ((p (aref data y  x)))
                            (let ((r (ldb (byte 8 0) p))
                                  (g (ldb (byte 8 8) p))
                                  (b (ldb (byte 8 16) p))
                                  (a (ldb (byte 8 24) p)))
                              (values r g b a))))))
          (loop for y from 0 to (1- height) do
               (loop for x from 0 to (1- width) do
                    (setf (opticl:pixel optimg y x)
                          (let ((p (aref data y  x)))
                            (let ((r (ldb (byte 8 0) p))
                                  (g (ldb (byte 8 8) p))
                                  (b (ldb (byte 8 16) p)))
                              (values r g b 255)))))))
      optimg)))

(defmethod coerce-to-opticl-rgb-image ((image opticl-rgb-image))
  (%image-pixels image))

(defmethod coerce-to-opticl-rgb-image ((image opticl-stancil-image))
  (let ((width (image-width image))
	(height (image-height image)))
    (let ((optimg (opticl:make-8-bit-rgba-image height width :initial-element 255))
	  (data (%image-pixels image)))
      (declare (type opticl-rgb-image-data optimg))
      (loop for y from 0 to (1- height)
	 do
	   (loop for x from 0 to (1- width)
	      do
		(setf (opticl:pixel optimg y x)
		      (let ((a (opticl-stancil-image-data-get-alpha data x y)))
			(values a a a 255)))))
      optimg)))

;;;
;;; copy image
;;;

(defun copy-opticl-rgb-image (src-img sx sy width height dst-img x y)
  (declare (type opticl-rgb-image-data src-img)
           (type opticl-rgb-image-data dst-img))
  (let ((max-y (+ y height -1)) ; why -1
        (max-x (+ x width -1)) ; why -1
        (dy (- sy y))
        (dx (- sx x)))
    (flet ((copy-ff ()
             (loop for j from y to max-y do
                  (loop for i from x to max-x do
                       (multiple-value-bind (r g b a)
                           (opticl:pixel src-img (+ j dy) (+ i dx))
                         (setf (opticl:pixel dst-img j i) (values r g b a))))))
           (copy-bf ()
             (loop for j from y to max-y do
                  (loop for i from max-x downto x do
                       (multiple-value-bind (r g b a)
                           (opticl:pixel src-img (+ j dy) (+ i dx))
                         (setf (opticl:pixel dst-img j i) (values r g b a))))))
           (copy-fb ()
             (loop for j from max-y downto y do
                  (loop for i from x to max-x do
                       (multiple-value-bind (r g b a)
                           (opticl:pixel src-img (+ j dy) (+ i dx))
                         (setf (opticl:pixel dst-img j i) (values r g b a))))))
           (copy-bb ()
             (loop for j from max-y downto y do
                  (loop for i from max-x downto x do
                       (multiple-value-bind (r g b a)
                           (opticl:pixel src-img (+ j dy) (+ i dx))
                         (setf (opticl:pixel dst-img j i) (values r g b a)))))))
      (when (and (> width 0) (> height 0))
        (if (eq src-img dst-img)
            (cond
              ((and (<= sx x) (<= sy y))
               (copy-bb))
              ((and (<= sx x) (> sy y))
               (copy-bf))
              ((and (> sx x) (<= sy y))
               (copy-fb))
              ((and (> sx x) (> sy y))
               (copy-ff)))
            (copy-ff))))))

(defmethod copy-image ((image opticl-rgb-image)
		       (src-image opticl-rgb-image)
		       &key (x 0) (y 0)
			 (width (image-width image))
			 (height (image-height image))
			 (src-dx 0)
			 (src-dy 0))
  (declare (type fixnum x y width height src-dx src-dy))
  (copy-opticl-rgb-image (%image-pixels src-image) (+ x src-dx) (+ y src-dy) width height
                     (%image-pixels image) x y)
  (make-rectangle* x y (+ x width) (+ y height)))

;;;
;;; fill image
;;;

(defgeneric opticl-fill-image (image rgba-design stencil &key x y width height stencil-dx stencil-dy))

(defmethod opticl-fill-image (image (rgba-design pixeled-uniform-design) (stencil (eql nil))
                              &key (x 0) (y 0) (width 0) (height 0) (stencil-dx 0) (stencil-dy 0))
  (declare (type fixnum x y width height)
	   (ignore stencil stencil-dx stencil-dy))
  (declare (type opticl-rgb-image-data image))
  (when (and (> width 0) (> height 0))
    (let ((max-y (+ y height -1))
          (max-x (+ x width -1)))
      (multiple-value-bind (red green blue alpha)
          (values
           (pixeled-uniform-design-red rgba-design)
           (pixeled-uniform-design-green rgba-design)
           (pixeled-uniform-design-blue rgba-design)
           (pixeled-uniform-design-alpha rgba-design))
        (if (> alpha 250)
            (loop for j from y to max-y do
                 (loop for i from x to max-x do
                      (setf (opticl:pixel image j i) (values red green blue alpha))))
            (loop for j from y to max-y do
                 (loop for i from x to max-x do
                      (multiple-value-bind (r.bg g.bg b.bg a.bg)
                          (opticl:pixel image j i)
                        (multiple-value-bind (red green blue alpha)
                            (octet-blend-function r.bg g.bg b.bg a.bg red green blue alpha)
                          (setf (opticl:pixel image j i) (values red green blue alpha))))))))))
  (make-rectangle* x y (+ x width) (+ y height)))

(defmethod opticl-fill-image (image rgba-design (stencil (eql nil))
                              &key (x 0) (y 0) (width 0) (height 0) (stencil-dx 0) (stencil-dy 0))
  (declare (type fixnum x y width height)
	   (ignore stencil stencil-dx stencil-dy))
  (declare (type opticl-rgb-image-data image))
  (when (and (> width 0) (> height 0))
    (let ((max-y (+ y height -1))
          (max-x (+ x width -1)))
      (let ((source-fn (make-pixeled-rgba-octets-fn rgba-design)))
        (declare (type pixeled-design-fn source-fn))
        (loop for j from y to max-y do
             (loop for i from x to max-x do
                  (multiple-value-bind (red green blue alpha)
                      (funcall source-fn i j)
                    (if (> alpha 250)
                        (setf (opticl:pixel image j i) (values red green blue alpha))
                        (multiple-value-bind (r.bg g.bg b.bg a.bg)
                            (opticl:pixel image j i)
                          (multiple-value-bind (red green blue alpha)
                              (octet-blend-function r.bg g.bg b.bg a.bg red green blue alpha)
                            (setf (opticl:pixel image j i) (values red green blue alpha)))))))))))
  (make-rectangle* x y (+ x width) (+ y height)))

(defmethod opticl-fill-image (image (rgba-design pixeled-uniform-design) stencil
                              &key (x 0) (y 0) (width 0) (height 0) (stencil-dx 0) (stencil-dy 0))
  (declare (type fixnum x y width height))
  (declare (type opticl-rgb-image-data image))
  (declare (type opticl-stancil-image-data stencil))
  (when (and (> width 0) (> height 0))
    (let ((max-y (+ y height -1))
          (max-x (+ x width -1)))
      (multiple-value-bind (red green blue alpha)
          (values 
           (pixeled-uniform-design-red rgba-design)
           (pixeled-uniform-design-green rgba-design)
           (pixeled-uniform-design-blue rgba-design)
           (pixeled-uniform-design-alpha rgba-design))
        (loop for j from y to max-y do
             (loop for i from x to max-x do
                  (let* ((alpha-ste (opticl:pixel stencil (+ stencil-dy j) (+ stencil-dx i)))
                         (a (octet-mult alpha alpha-ste)))
                    (if (> a 250)
                        (setf (opticl:pixel image j i) (values red green blue a))
                        (multiple-value-bind (r.bg g.bg b.bg a.bg)
                            (opticl:pixel image j i)
                          (multiple-value-bind (red green blue alpha)
                              (octet-blend-function r.bg g.bg b.bg a.bg red green blue a)
                            (setf (opticl:pixel image j i) (values red green blue alpha)))))))))))
  (make-rectangle* x y (+ x width) (+ y height)))

(defmethod opticl-fill-image (image rgba-design stencil
                              &key (x 0) (y 0) (width 0) (height 0) (stencil-dx 0) (stencil-dy 0))
  (declare (type fixnum x y width height))
  (declare (type opticl-rgb-image-data image))
  (declare (type opticl-stancil-image-data stencil))
  (when (and (> width 0) (> height 0))
    (let ((max-y (+ y height -1))
          (max-x (+ x width -1)))
      (let ((source-fn (make-pixeled-rgba-octets-fn rgba-design)))
        (declare (type pixeled-design-fn source-fn))
        (loop for j from y to max-y do
             (loop for i from x to max-x do
                  (multiple-value-bind (red green blue alpha)
                      (funcall source-fn i j)
                    (let* ((alpha-ste (opticl:pixel stencil (+ stencil-dy j) (+ stencil-dx i)))
                           (a (octet-mult alpha alpha-ste)))
                      (if (> a 250)
                          (setf (opticl:pixel image j i) (values red green blue a))
                          (multiple-value-bind (r.bg g.bg b.bg a.bg)
                              (opticl:pixel image j i)
                            (multiple-value-bind (red green blue alpha)
                                (octet-blend-function r.bg g.bg b.bg a.bg red green blue a)
                              (setf (opticl:pixel image j i) (values red green blue alpha))))))))))))
  (make-rectangle* x y (+ x width) (+ y height)))

(defmethod fill-image ((image opticl-rgb-image) rgba-design mask
		       &key
			 (x 0) (y 0)
			 (width (image-width image)) (height (image-height image))
			 (mask-dx 0) (mask-dy 0))
  (opticl-fill-image (%image-pixels image) rgba-design
                     (if mask (%image-pixels mask) nil)
                     :x x :y y :width width :height height :stencil-dx mask-dx :stencil-dy mask-dy))


;;; private protocol
(defmethod %make-blend-draw-fn ((image opticl-rgb-image) clip-region design)
  (let ((source-fn (make-pixeled-rgba-octets-fn design)))
    (declare (type pixeled-design-fn source-fn))
    (%make-blend-draw-function-macro
     opticl-rgb-image-data
     (opticl-rgb-image-data-get-pixel data x y)
     (opticl-rgb-image-data-set-pixel data x y red green blue alpha)
     (funcall source-fn x y))))

(defmethod %make-blend-draw-span-fn ((image opticl-rgb-image) clip-region design)
  (let ((source-fn (make-pixeled-rgba-octets-fn design)))
    (declare (type pixeled-design-fn source-fn))
    (%make-blend-draw-span-function-macro
     opticl-rgb-image-data
     (opticl-rgb-image-data-get-pixel data x y)
     (opticl-rgb-image-data-set-pixel data x y red green blue alpha)
     (funcall source-fn x y))))

(defmethod %make-xor-draw-fn ((image opticl-rgb-image) clip-region design)
  (let ((source-fn (make-pixeled-rgba-octets-fn design)))
    (declare (type pixeled-design-fn source-fn))
    (%make-xor-draw-function-macro
     opticl-rgb-image-data
     (opticl-rgb-image-data-get-pixel data x y)
     (opticl-rgb-image-data-set-pixel data x y red green blue alpha)
     (funcall source-fn x y))))

(defmethod %make-xor-draw-span-fn ((image opticl-rgb-image) clip-region design)
  (let ((source-fn (make-pixeled-rgba-octets-fn design)))
    (declare (type pixeled-design-fn source-fn))
    (%make-xor-draw-span-function-macro
     opticl-rgb-image-data
     (opticl-rgb-image-data-get-pixel data x y)
     (opticl-rgb-image-data-set-pixel data x y red green blue alpha)
     (funcall source-fn x y))))

;;;
;;; Optimization
;;;

(defmethod %make-blend-draw-fn ((image opticl-rgb-image) clip-region (design pixeled-uniform-design))
  (let ((s-red (pixeled-uniform-design-red design))
	(s-green (pixeled-uniform-design-green design))
	(s-blue (pixeled-uniform-design-blue design))
	(s-alpha (pixeled-uniform-design-alpha design)))
    (%make-blend-draw-function-macro
     opticl-rgb-image-data
     (opticl-rgb-image-data-get-pixel data x y)
     (opticl-rgb-image-data-set-pixel data x y red green blue alpha)
     (values s-red s-green s-blue s-alpha))))

(defmethod %make-blend-draw-span-fn ((image opticl-rgb-image) clip-region (design pixeled-uniform-design))
  (let ((s-red (pixeled-uniform-design-red design))
	(s-green (pixeled-uniform-design-green design))
	(s-blue (pixeled-uniform-design-blue design))
	(s-alpha (pixeled-uniform-design-alpha design)))
    (%make-blend-draw-span-function-macro
     opticl-rgb-image-data
     (opticl-rgb-image-data-get-pixel data x y)
     (opticl-rgb-image-data-set-pixel data x y red green blue alpha)
     (values s-red s-green s-blue s-alpha))))


;;;
;;; Stencil Operations
;;;

(declaim (inline opticl-stancil-image-data-get-alpha)
	 (ftype (function (opticl-stancil-image-data fixnum fixnum) octet) opticl-stancil-image-data-get-alpha))
(defun opticl-stancil-image-data-get-alpha (data x y)
  (multiple-value-bind (alpha)
      (opticl:pixel data y x)
    alpha))
     
(declaim (inline rgb-image-data-set-alpha)
	 (ftype (function (opticl-stancil-image-data fixnum fixnum octet) t) opticl-stancil-image-data-set-alpha))
(defun opticl-stancil-image-data-set-alpha (data x y alpha)
  (setf (opticl:pixel data y x)
	alpha))

;;;
;;; drawing methods
;;;

(defgeneric %make-opticl-stancil-image-draw-fn (image clip-region))
(defgeneric %make-opticl-stancil-image-draw-span-fn (image clip-region)) 

;;; default implementation

(defmacro %make-opticl-stancil-image-draw-function-macro (data)
  `(let ((data ,data))
     (declare (type opticl-stancil-image-data data))
     (lambda (x y alpha)
       (declare (type fixnum x y)
		(type fixnum alpha))
       (when (or (null clip-region) (clim:region-contains-position-p clip-region x y))
	 (setf alpha (min (abs alpha) 255))
	 (when (plusp alpha)
	   (opticl-stancil-image-data-set-alpha data x y alpha))))))

(defmacro %make-opticl-stancil-image-draw-span-function-macro (data)
  `(let ((data ,data))
     (declare (type opticl-stancil-image-data data))
     (lambda (x1 x2 y alpha)
       (declare (type fixnum x1 x2 y)
		(type fixnum alpha))
       (setf alpha (min (abs alpha) 255))
       (when (plusp alpha)
	 (loop for x from x1 below x2 do
	      (when (or (null clip-region) (clim:region-contains-position-p clip-region x y))
		(opticl-stancil-image-data-set-alpha data x y alpha)))))))

(defmethod %make-opticl-stancil-image-draw-fn ((image opticl-stancil-image) clip-region)
  (%make-opticl-stancil-image-draw-function-macro
   (%image-pixels image)))

(defmethod %make-opticl-stancil-image-draw-span-fn ((image opticl-stancil-image) clip-region)
  (%make-opticl-stancil-image-draw-span-function-macro
   (%image-pixels image)))
