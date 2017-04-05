(in-package :mcclim-render)

(declaim (optimize speed))

;;;
;;; copy 
;;;

(defun rgb-image-copy (src dst &key (x 0) (y 0)
				 (width (image-width src))
				 (height (image-height src))
				 (x-dst 0)
				 (y-dst 0)
				 (clip-region nil))
  (declare ;;(optimize speed)
	   (type fixnum x y width height x-dst y-dst))
  (let ((clip-region  (if clip-region
			  (region-intersection clip-region
					       (make-rectangle* 0 0 (1- (image-width dst))
								(1- (image-height dst))))
			  (make-rectangle* 0 0 (1- (image-width dst)) (1- (image-height dst))))))
    (clim:with-bounding-rectangle* (min-x-dst min-y-dst max-x-dst max-y-dst)
	clip-region
      (let ((dx (- x-dst x))
	    (dy (- y-dst y))
	    (min-x-dst (round-coordinate min-x-dst))
	    (min-y-dst (round-coordinate min-y-dst))
	    (max-x-dst (round-coordinate max-x-dst))
	    (max-y-dst (round-coordinate max-y-dst))
	    (data-src (image-data src))
	    (data-dst (image-data dst))
	    (mask (if (rectanglep clip-region)
		      nil
		      clip-region)))
	(declare (type fixnum dx dy)
		 (type rgba-image-data data-src data-dst))
	(let ((x-min (max 0 x (- min-x-dst dx)))
	      (y-min (max 0 y (- min-y-dst dy)))
	      (x-max (min (+ x width) (1- (image-width src)) (- max-x-dst dx)))
	      (y-max (min (+ y height) (1- (image-height src)) (- max-y-dst dy))))
	  (flet ((copy-bb ()
		   (loop
		      for j from y-max downto y-min 
		      do
			(loop
			   for i from x-max downto x-min
			   do
			     (multiple-value-bind (red green blue alpha)
				 (rgba-image-data-get-pixel-octet data-src i j)
			       (rgba-image-data-set-pixel-octet data-dst (+ dx i) (+ dy j)
								red green blue alpha)))))
		 (copy-bf ()
		   (loop
		      for j from y-min to y-max
		      do
			(loop
			   for i from x-max downto x-min
			   do
			     (multiple-value-bind (red green blue alpha)
				 (rgba-image-data-get-pixel-octet data-src i j)
			       (rgba-image-data-set-pixel-octet data-dst (+ dx i) (+ dy j)
							 red green blue alpha)))))
		 (copy-fb ()
		   (loop
		      for j from y-max downto y-min
		      do
			(loop
			   for i from x-min to x-max
			   do
			     (multiple-value-bind (red green blue alpha)
				 (rgba-image-data-get-pixel-octet data-src i j)
			       (rgba-image-data-set-pixel-octet data-dst (+ dx i) (+ dy j)
							 red green blue alpha)))))
		 (copy-ff ()
		   (loop
		      for j from y-min to y-max
		      do
			(loop
			   for i from x-min to x-max
			   do
			     (multiple-value-bind (red green blue alpha)
				 (rgba-image-data-get-pixel-octet data-src i j)
			       (rgba-image-data-set-pixel-octet data-dst (+ dx i) (+ dy j)
							 red green blue alpha))))))
	    (when mask
	      (warn "mask not implemented"))
	    (if (eq data-src data-dst)
		(cond
		  ((and (<= x x-dst) (<= y y-dst))
		   (copy-bb))
		  ((and (<= x x-dst) (> y y-dst))
		   (copy-bf))
		  ((and (> x x-dst) (<= y y-dst))
		   (copy-fb))
		  ((and (> x x-dst) (> y y-dst))
		   (copy-ff)))
		(copy-ff))
	    (make-rectangle* (+ x-min dx) (+ y-min dy) (+ x-max dx) (+ y-max dy))))))))

;;;
;;; copy image
;;;

(defmacro make-copy-image-function (self src-dx src-dy image-get-code image-set-code)
  `(flet ((copy-ff ()
	    (when (and (> width 0)
		       (> height 0))
	      (let ((max-y (+ y height))
		    (max-x (+ x width)))
		(loop for j from y to max-y do
		     (loop for i from x to max-x do
			  (multiple-value-bind (red green blue alpha)
			      ,image-get-code
			    ,image-set-code))))))
	  (copy-bf ()
	    (when (and (> width 0)
		       (> height 0))
	      (let ((max-y (+ y height))
		    (max-x (+ x width)))
		(loop for j from y to max-y do
		     (loop for i from max-x downto x do
			  (multiple-value-bind (red green blue alpha)
			      ,image-get-code
			    ,image-set-code))))))
	  (copy-fb ()
	    (when (and (> width 0)
		       (> height 0))
	      (let ((max-y (+ y height))
		    (max-x (+ x width)))
		(loop for j from max-y downto y do
		     (loop for i from x to max-x do
			  (multiple-value-bind (red green blue alpha)
			      ,image-get-code
			    ,image-set-code))))))
	  (copy-bb ()
	    (when (and (> width 0)
		       (> height 0))
	      (let ((max-y (+ y height))
		    (max-x (+ x width)))
		(loop for j from max-y downto y do
		     (loop for i from max-x downto x do
			  (multiple-value-bind (red green blue alpha)
			      ,image-get-code
			    ,image-set-code)))))))
     (if ,self
	 (cond
	   ((and (<= src-dx 0) (<= src-dy 0))
	    (copy-bb))
	   ((and (<= src-dx 0) (> src-dy 0))
	    (copy-bf))
	   ((and (> src-dx 0) (<= src-dy 0))
	    (copy-fb))
	   ((and (> src-dx 0) (> src-dy 0))
	    (copy-ff)))
	 (copy-ff))))

(defgeneric copy-image (image src-image &key x y 
					  width 
					  height 
					  src-dx
					  src-dy))

(defmethod copy-image ((image rgba-image)
		       (src-image rgba-image)
		       &key (x 0) (y 0)
			 (width (climi::image-width image))
			 (height (climi::image-height image))
			 (src-dx 0)
			 (src-dy 0))
  (declare (type fixnum x y width height src-dx src-dy))
  (let ((data-image (image-data image))
	(src-data-image (image-data src-image)))
    (declare (type rgba-image-data data-image src-data-image))
    (make-copy-image-function
     (eql image src-image)
     src-dx src-dy
     (rgba-image-data-get-pixel-octet src-data-image (+ src-dx i) (+ src-dy j))
     (rgba-image-data-set-pixel-octet data-image i j red green blue alpha)))
  (make-rectangle* x y (+ x width) (+ y height)))

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
		     (if (> (imult aa-alpha alpha) 250)
			 ,image-set-code
			 (multiple-value-bind (r.bg g.bg b.bg a.bg)
			     ,image-get-code
			   (multiple-value-bind (red green blue alpha)	  
			       (octet-blend r.bg g.bg b.bg a.bg red green blue alpha aa-alpha)
			     ,image-set-code))))))))))
		      
(defgeneric fill-image (image design mask &key x y 
					    width 
					    height 
					    mask-dx
					    mask-dy))

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
     (rgba-image-data-get-pixel-octet data-image i j)
     (rgba-image-data-set-pixel-octet data-image i j red green blue alpha)
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
     (rgba-image-data-get-pixel-octet data-image i j)
     (rgba-image-data-set-pixel-octet data-image i j red green blue alpha)
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
     (rgba-image-data-get-pixel-octet data-image i j)
     (rgba-image-data-set-pixel-octet data-image i j red green blue alpha)
     (values 
      (uniform-rgba-design-red rgba-design)
      (uniform-rgba-design-green rgba-design)
      (uniform-rgba-design-blue rgba-design)
      (uniform-rgba-design-alpha rgba-design))
     (mask-image-data-get-alpha-octet data-mask (+ mask-dx i) (+ mask-dy j))))
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
     (rgba-image-data-get-pixel-octet data-image i j)
     (rgba-image-data-set-pixel-octet data-image i j red green blue alpha)
     (funcall source-fn i j)
     (mask-image-data-get-alpha-octet data-mask (+ mask-dx i) (+ mask-dy j))))
  (make-rectangle* x y (+ x width) (+ y height)))
