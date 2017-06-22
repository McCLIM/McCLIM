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

(defun copy-opticl-image (src-img sx sy width height dst-img x y)
  (declare (type opticl-image-data src-img)
           (type opticl-image-data dst-img))
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

(defmethod copy-image ((image opticl-image)
		       (src-image opticl-image)
		       &key (x 0) (y 0)
			 (width (image-width image))
			 (height (image-height image))
			 (src-dx 0)
			 (src-dy 0))
  (declare (type fixnum x y width height src-dx src-dy))
  (copy-opticl-image (%image-pixels src-image) (+ x src-dx) (+ y src-dy) width height
                     (%image-pixels image) x y)
  (make-rectangle* x y (+ x width) (+ y height)))

;;;
;;; fill image
;;;

(defgeneric opticl-fill-image (image rgba-design stencil &key x y width height stencil-dx stencil-dy))

(defmethod opticl-fill-image (image (rgba-design uniform-rgba-design) (stencil (eql nil))
                              &key (x 0) (y 0) (width 0) (height 0) (stencil-dx 0) (stencil-dy 0))
  (declare (type fixnum x y width height)
	   (ignore stencil stencil-dx stencil-dy))
  (declare (type opticl-image-data image))
  (when (and (> width 0) (> height 0))
    (let ((max-y (+ y height -1))
          (max-x (+ x width -1)))
      (multiple-value-bind (red green blue alpha)
          (values
           (uniform-rgba-design-red rgba-design)
           (uniform-rgba-design-green rgba-design)
           (uniform-rgba-design-blue rgba-design)
           (uniform-rgba-design-alpha rgba-design))
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
  (declare (type opticl-image-data image))
  (when (and (> width 0) (> height 0))
    (let ((max-y (+ y height -1))
          (max-x (+ x width -1)))
      (let ((source-fn (make-rgba-design-fn rgba-design)))
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

(defmethod opticl-fill-image (image (rgba-design uniform-rgba-design) stencil
                              &key (x 0) (y 0) (width 0) (height 0) (stencil-dx 0) (stencil-dy 0))
  (declare (type fixnum x y width height))
  (declare (type opticl-image-data image))
  (declare (type mask-image-data stencil))
  (when (and (> width 0) (> height 0))
    (let ((max-y (+ y height -1))
          (max-x (+ x width -1)))
      (multiple-value-bind (red green blue alpha)
          (values 
           (uniform-rgba-design-red rgba-design)
           (uniform-rgba-design-green rgba-design)
           (uniform-rgba-design-blue rgba-design)
           (uniform-rgba-design-alpha rgba-design))
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
  (declare (type opticl-image-data image))
  (declare (type mask-image-data stencil))
  (when (and (> width 0) (> height 0))
    (let ((max-y (+ y height -1))
          (max-x (+ x width -1)))
      (let ((source-fn (make-rgba-design-fn rgba-design)))
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

(defmethod fill-image ((image opticl-image) rgba-design mask
		       &key
			 (x 0) (y 0)
			 (width (image-width image)) (height (image-height image))
			 (mask-dx 0) (mask-dy 0))
  (opticl-fill-image (image-data image) rgba-design (if mask (image-data mask) nil)
                     :x x :y y :width width :height height :stencil-dx mask-dx :stencil-dy mask-dy))


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
