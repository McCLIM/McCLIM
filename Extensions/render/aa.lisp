(in-package :mcclim-render)

;;;
;;; macro to build drawing function
;;;

(defmacro %make-blend-draw-function-macro (data source-code)
  `(let ((data ,data))
     (declare (type rgba-image-data data))
     (lambda (x y alpha)
       ;;(declare (optimize (speed 3)))
       (when (or (null clip-region) (clim:region-contains-position-p clip-region x y))
	 (setf alpha (float (/ (min (abs alpha) 255) 255)))
	 (when (plusp alpha)
	   (multiple-value-bind (r.fg g.fg b.fg a.fg)
	       ,source-code
	     (if (> (* a.fg alpha) 0.99)
		 (multiple-value-bind (red green blue alpha)	  
		     (values r.fg g.fg b.fg 1.0)
		   (rgba-image-data-set-pixel data x y red green blue alpha)
		   (values red green blue alpha))
		 (multiple-value-bind (r.bg g.bg b.bg a.bg)
		     (rgba-image-data-get-pixel data x y)
		   (multiple-value-bind (red green blue alpha)
		       (float-blend r.bg g.bg b.bg a.bg r.fg g.fg b.fg a.fg alpha)
		     (rgba-image-data-set-pixel data x y red green blue alpha)
		     (values red green blue alpha))))))))))

(defmacro %make-blend-draw-span-function-macro (data source-code)
  `(let ((data ,data))
     (declare (type rgba-image-data data))
     (lambda (x1 x2 y alpha)
       ;;(declare (optimize (speed 3)))
       (setf alpha (float (/ (min (abs alpha) 255) 255)))
       (when (plusp alpha)
	 (loop for x from x1 below x2 do
	      (when (or (null clip-region) (clim:region-contains-position-p clip-region x y))
		(multiple-value-bind (r.fg g.fg b.fg a.fg)
		    ,source-code
		  (if (> (* a.fg alpha) 0.99)
		      (multiple-value-bind (red green blue alpha)	  
			  (values r.fg g.fg b.fg 1.0)
			(rgba-image-data-set-pixel data x y red green blue alpha)
			(values red green blue alpha))
		      (multiple-value-bind (r.bg g.bg b.bg a.bg)
			  (rgba-image-data-get-pixel data x y)
			(multiple-value-bind (red green blue alpha)	  
			    (float-blend r.bg g.bg b.bg a.bg r.fg g.fg b.fg a.fg alpha)
			  (rgba-image-data-set-pixel data x y red green blue alpha)
			  (values red green blue alpha)))))))))))
  
(defmacro %make-xor-draw-function-macro (data source-code)
  `(let ((data ,data))
     (declare (type rgba-image-data data))
     (lambda (x y alpha)
       ;;(declare (optimize (speed 3)))
       (when (or (null clip-region) (clim:region-contains-position-p clip-region x y))
	 (setf alpha (float (/ (min (abs alpha) 255) 255)))
	 (when (plusp alpha)
	   (multiple-value-bind (r.bg g.bg b.bg a.bg)
	       (rgba-image-data-get-pixel data x y)
	     (multiple-value-bind (r.fg g.fg b.fg a.fg)
		 ,source-code
	       (multiple-value-bind (red green blue alpha)
		   (float-blend r.bg g.bg b.bg a.bg
				(float-xor-pixel r.bg r.fg) (float-xor-pixel g.bg g.fg)
				(float-xor-pixel b.bg b.fg) a.fg alpha)
		 (rgba-image-data-set-pixel data x y red green blue alpha)
		 (values red green blue alpha)))))))))

(defmacro %make-xor-draw-span-function-macro (data source-code)
  `(let ((data ,data))
     (declare (type rgba-image-data data))
     (lambda (x1 x2 y alpha)
       ;;(declare (optimize (speed 3)))
       (setf alpha (float (/ (min (abs alpha) 255) 255)))
       (when (plusp alpha)
	 (loop for x from x1 below x2 do
	      (when (or (null clip-region) (clim:region-contains-position-p clip-region x y))
		(multiple-value-bind (r.bg g.bg b.bg a.bg)
		    (rgba-image-data-get-pixel data x y)
		  (multiple-value-bind (r.fg g.fg b.fg a.fg)
		      ,source-code
		    (multiple-value-bind (red green blue alpha)
			(float-blend r.bg g.bg b.bg a.bg
				     (float-xor-pixel r.bg r.fg) (float-xor-pixel g.bg g.fg)
				     (float-xor-pixel b.bg b.fg) a.fg alpha)
		      (rgba-image-data-set-pixel data x y red green blue alpha)
		      (values red green blue alpha))))))))))


(defmacro %make-mask-image-draw-function-macro (data)
  `(let ((data ,data))
     (declare (type mask-image-data data))
     (lambda (x y alpha)
       ;;(declare (optimize (speed 3)))
       (when (or (null clip-region) (clim:region-contains-position-p clip-region x y))
	 (setf alpha (float (/ (min (abs alpha) 255) 255)))
	 (when (plusp alpha)
	   (mask-image-data-set-alpha data x y alpha))))))

(defmacro %make-mask-image-draw-span-function-macro (data)
  `(let ((data ,data))
     (declare (type mask-image-data data))
     (lambda (x1 x2 y alpha)
       ;;(declare (optimize (speed 3)))
       (setf alpha (float (/ (min (abs alpha) 255) 255)))
       (when (plusp alpha)
	 (loop for x from x1 below x2 do
	      (when (or (null clip-region) (clim:region-contains-position-p clip-region x y))
		(mask-image-data-set-alpha data x y alpha)))))))

;;; private protocol

(defgeneric %make-blend-draw-fn (image clip-region rgba-design))
(defgeneric %make-blend-draw-span-fn (image clip-region ink)) 
(defgeneric %make-xor-draw-fn (image clip-region rgba-design))
(defgeneric %make-xor-draw-span-fn (image clip-region ink))
(defgeneric %make-mask-image-draw-fn (image clip-region))
(defgeneric %make-mask-image-draw-span-fn (image clip-region)) 

;;; default implementation

(defmethod %make-blend-draw-fn ((image rgba-image) clip-region design)
  (let ((source-fn (make-rgba-design-fn design)))
    (%make-blend-draw-function-macro
     (image-data image)
     (funcall source-fn x y))))

(defmethod %make-blend-draw-span-fn ((image rgba-image) clip-region design)
  (let ((source-fn (make-rgba-design-fn design)))
    (%make-blend-draw-span-function-macro
     (image-data image)
     (funcall source-fn x y))))

(defmethod %make-xor-draw-fn ((image rgba-image) clip-region design)
  (let ((source-fn (make-rgba-design-fn design)))
    (%make-xor-draw-function-macro
     (image-data image)
     (funcall source-fn x y))))

(defmethod %make-xor-draw-span-fn ((image rgba-image) clip-region design)
  (let ((source-fn (make-rgba-design-fn design)))
    (%make-xor-draw-span-function-macro
     (image-data image)
     (funcall source-fn x y))))

(defmethod %make-mask-image-draw-fn ((image mask-image) clip-region)
  (%make-mask-image-draw-function-macro
   (image-data image)))

(defmethod %make-mask-image-draw-span-fn ((image mask-image) clip-region)
  (%make-mask-image-draw-span-function-macro
   (image-data image)))

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
	 (image-data image)
	 (if (region-contains-position-p mask x y)
	     (values s-red s-green s-blue s-alpha)
	     (values 0.0 0.0 0.0 0.0)))
	(%make-blend-draw-function-macro
	 (image-data image)
	 (values s-red s-green s-blue s-alpha)))))

(defmethod %make-blend-draw-span-fn ((image rgba-image) clip-region (design uniform-rgba-design))
  (let ((s-red (uniform-rgba-design-red design))
	(s-green (uniform-rgba-design-green design))
	(s-blue (uniform-rgba-design-blue design))
	(s-alpha (uniform-rgba-design-alpha design))
	(mask (uniform-rgba-design-mask design)))
    (if mask
	(%make-blend-draw-span-function-macro
	 (image-data image)
	 (if (region-contains-position-p mask x y)
	     (values s-red s-green s-blue s-alpha)
	     (values 0.0 0.0 0.0 0.0)))
	(%make-blend-draw-span-function-macro
	 (image-data image)
	 (values s-red s-green s-blue s-alpha)))))

;;;
;;; function used by aa engine
;;;

(declaim (inline render-line-f))
(defun render-line-f (state transformation x1 y1 x2 y2)
  (with-transformed-position (transformation x1 y1)
    (with-transformed-position (transformation x2 y2)
      (aa::line-f state x1 y1 x2 y2))))

(defun render-update-state (state paths transformation)
  (if (listp paths)
      (dolist (path paths)
        (render-update-state state path transformation))
      (let ((iterator (vectors::path-iterator-segmented paths)))
        (multiple-value-bind (i1 k1 e1) (vectors::path-iterator-next iterator)
          (declare (ignore i1))
          (when (and k1 (not e1))
            ;; at least 2 knots
            (let ((first-knot k1))
              (loop
                 (multiple-value-bind (i2 k2 e2) (vectors::path-iterator-next iterator)
                   (declare (ignore i2))
                   (render-line-f state transformation
                                 (vectors::point-x k1) (vectors::point-y k1)
                                 (vectors::point-x k2) (vectors::point-y k2))
                   (setf k1 k2)
                   (when e2
                     (return))))
              (render-line-f state transformation
                            (vectors::point-x k1) (vectors::point-y k1)
                            (vectors::point-x first-knot) (vectors::point-y first-knot)))))))
  state)
