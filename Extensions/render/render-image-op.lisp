(in-package :mcclim-render)

(declaim (optimize speed))

;;;
;;; Image Operations
;;;

(declaim (inline round-coordinate))
(defun round-coordinate (x)
  (floor (+ x .5)))

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

(declaim (inline opticl-stencil-image-data-get-alpha)
	 (ftype (function (opticl-stencil-image-data fixnum fixnum) octet) opticl-stencil-image-data-get-alpha))
(defun opticl-stencil-image-data-get-alpha (data x y)
  (multiple-value-bind (alpha)
      (opticl:pixel data y x)
    alpha))

(declaim (inline rgb-image-data-set-alpha)
	 (ftype (function (opticl-stencil-image-data fixnum fixnum octet) t) opticl-stencil-image-data-set-alpha))
(defun opticl-stencil-image-data-set-alpha (data x y alpha)
  (setf (opticl:pixel data y x)
	alpha))

;;;
;;; drawing methods
;;;

(defgeneric %make-opticl-stencil-image-draw-fn (image clip-region))
(defgeneric %make-opticl-stencil-image-draw-span-fn (image clip-region)) 

;;; default implementation

(defmacro %make-opticl-stencil-image-draw-function-macro (data)
  `(let ((data ,data))
     (declare (type opticl-stencil-image-data data))
     (lambda (x y alpha)
       (declare (type fixnum x y)
		(type fixnum alpha))
       (when (or (null clip-region) (clim:region-contains-position-p clip-region x y))
	 (setf alpha (min (abs alpha) 255))
	 (when (plusp alpha)
	   (opticl-stencil-image-data-set-alpha data x y alpha))))))

(defmacro %make-opticl-stencil-image-draw-span-function-macro (data)
  `(let ((data ,data))
     (declare (type opticl-stencil-image-data data))
     (lambda (x1 x2 y alpha)
       (declare (type fixnum x1 x2 y)
		(type fixnum alpha))
       (setf alpha (min (abs alpha) 255))
       (when (plusp alpha)
	 (loop for x from x1 below x2 do
	      (when (or (null clip-region) (clim:region-contains-position-p clip-region x y))
		(opticl-stencil-image-data-set-alpha data x y alpha)))))))

(defmethod %make-opticl-stencil-image-draw-fn ((image opticl-stencil-image) clip-region)
  (%make-opticl-stencil-image-draw-function-macro
   (image-pixels image)))

(defmethod %make-opticl-stencil-image-draw-span-fn ((image opticl-stencil-image) clip-region)
  (%make-opticl-stencil-image-draw-span-function-macro
   (image-pixels image)))
