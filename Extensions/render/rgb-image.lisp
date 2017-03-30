(in-package :mcclim-render)

;;;
;;; color manipulation
;;;

(deftype image-data () '(simple-array (unsigned-byte 32) (* *)))
(declaim (inline round-coordinate))
(defun round-coordinate (x)
  (floor (+ x .5)))

(declaim (notinline float-blend))
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
;;; get/set functions
;;;


;;;
;;; copy 
;;;

(defun rgb-image-copy (src dst &key (x 0) (y 0)
				 (width (image-width src))
				 (height (image-height src))
				 (x-dst 0)
				 (y-dst 0)
				 (clip-region nil))
  (declare (optimize speed)
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
				 (rgba-image-data-get-pixel data-src i j)
			       (rgba-image-data-set-pixel data-dst (+ dx i) (+ dy j)
							 red green blue alpha)))))
		 (copy-bf ()
		   (loop
		      for j from y-min to y-max
		      do
			(loop
			   for i from x-max downto x-min
			   do
			     (multiple-value-bind (red green blue alpha)
				 (rgba-image-data-get-pixel data-src i j)
			       (rgba-image-data-set-pixel data-dst (+ dx i) (+ dy j)
							 red green blue alpha)))))
		 (copy-fb ()
		   (loop
		      for j from y-max downto y-min
		      do
			(loop
			   for i from x-min to x-max
			   do
			     (multiple-value-bind (red green blue alpha)
				 (rgba-image-data-get-pixel data-src i j)
			       (rgba-image-data-set-pixel data-dst (+ dx i) (+ dy j)
							 red green blue alpha)))))
		 (copy-ff ()
		   (loop
		      for j from y-min to y-max
		      do
			(loop
			   for i from x-min to x-max
			   do
			     (multiple-value-bind (red green blue alpha)
				 (rgba-image-data-get-pixel data-src i j)
			       (rgba-image-data-set-pixel data-dst (+ dx i) (+ dy j)
							 red green blue alpha))))))
	    (when mask
	      (warn "mask not implemented"))
	    (cond
	      ((and (<= x x-dst) (<= y y-dst))
	       (copy-bb))
	      ((and (<= x x-dst) (> y y-dst))
	       (copy-bf))
	      ((and (> x x-dst) (<= y y-dst))
	       (copy-fb))
	      ((and (> x x-dst) (> y y-dst))
	       (copy-ff)))
	    (make-rectangle* (+ x-min dx) (+ y-min dy) (+ x-max dx) (+ y-max dy))))))))

(defun rgb-image-fill (image mask-image &key (x 0) (y 0)
				    (width (climi::image-width src))
				    (height (climi::image-height src))
				    (x-dst 0)
				    (y-dst 0)
				    (clip-region nil)
				    (ink clim:+foreground-ink+)
				    (background clim:+yellow+)
				    (foreground clim:+blue+))
  (declare (optimize speed)
	   (type fixnum x y width height x-dst y-dst))
  (let ((clip-region  (if clip-region
			  (region-intersection clip-region
					       (make-rectangle* 0 0 (1- (image-width image))
								(1- (image-height image))))
			  (make-rectangle* 0 0 (1- (image-width image)) (1- (image-height image))))))
    (clim:with-bounding-rectangle* (min-x-dst min-y-dst max-x-dst max-y-dst)
	clip-region
      (let ((dx (- x-dst x))
	    (dy (- y-dst y))
	    (min-x-dst (round-coordinate min-x-dst))
	    (min-y-dst (round-coordinate min-y-dst))
	    (max-x-dst (round-coordinate max-x-dst))
	    (max-y-dst (round-coordinate max-y-dst))
	    (data-image (image-data image))
	    (data-mask (image-data mask-image))
	    (mask (if (rectanglep clip-region)
		      nil
		      clip-region)))
	(declare (type fixnum dx dy)
		 (type rgba-image-data data-image)
		 (type alpha-channel-data data-mask))
	(let ((x-min (max 0 x (- min-x-dst dx)))
	      (y-min (max 0 y (- min-y-dst dy)))
	      (x-max (min (+ x width) (1- (image-width mask-image)) (- max-x-dst dx)))
	      (y-max (min (+ y height) (1- (image-height mask-image)) (- max-y-dst dy))))
	  (let ((*background-design* background)
		(*foreground-design* foreground))
	    (let* ((rgba-design (make-rgba-design ink))
		   (source-fn (make-rgba-design-fn rgba-design)))
	      (flet ((fill-color ()
		       (loop
			  for j from y-min to y-max
			  do
			    (loop
			       for i from x-min to x-max
			       do
				 (multiple-value-bind (r.bg g.bg b.bg a.bg)
				     (rgba-image-data-get-pixel data-image (+ dx i) (+ dy j))
				   (multiple-value-bind (a.m)
				       (alpha-channel-data-get-alpha data-mask i j)
				     (multiple-value-bind (r.fg g.fg b.fg a.fg)
					 (funcall source-fn i j)
				       (multiple-value-bind (red green blue alpha)	  
					   (float-blend r.bg g.bg b.bg a.bg r.fg g.fg b.fg a.fg a.m)
					 (rgba-image-data-set-pixel data-image (+ dx i) (+ dy j)
								    red green blue alpha))))))))
		     (fill-function ()
		       (loop
			  for j from y-min to y-max
			  do
			    (loop
			       for i from x-min to x-max
			       do
				 (multiple-value-bind (r.bg g.bg b.bg a.bg)
				     (rgba-image-data-get-pixel data-image (+ dx i) (+ dy j))
				   (multiple-value-bind (a.m)
				       (alpha-channel-data-get-alpha data-mask i j)
				     (multiple-value-bind (r.fg g.fg b.fg a.fg)
					 (funcall source-fn i j)
				       (multiple-value-bind (red green blue alpha)	  
					   (float-blend r.bg g.bg b.bg a.bg r.fg g.fg b.fg a.fg a.m)
					 (rgba-image-data-set-pixel data-image (+ dx i) (+ dy j)
								    red green blue alpha)))))))))
		(when mask
		  (warn "mask not implemented"))
		(if (typep rgba-design 'uniform-rgba-design)
		    (fill-color)
		    (fill-function))
		(make-rectangle* (+ x-min dx) (+ y-min dy) (+ x-max dx) (+ y-max dy))))))))))
  
