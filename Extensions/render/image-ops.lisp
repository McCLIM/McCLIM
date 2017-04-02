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

(defun rgb-image-fill (image mask-image &key (x 0) (y 0)
				    (width (climi::image-width image))
				    (height (climi::image-height image))
				    (x-dst 0)
				    (y-dst 0)
				    (clip-region nil)
				    (ink clim:+foreground-ink+)
				    (background clim:+yellow+)
				    (foreground clim:+blue+))
  (declare ;;(optimize speed)
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
		 (type mask-image-data data-mask))
	(let ((x-min (max 0 x (- min-x-dst dx)))
	      (y-min (max 0 y (- min-y-dst dy)))
	      (x-max (min (+ x width) (1- (image-width mask-image)) (- max-x-dst dx)))
	      (y-max (min (+ y height) (1- (image-height mask-image)) (- max-y-dst dy))))
	  (let ((*background-design* background)
		(*foreground-design* foreground))
	    (let* ((rgba-design (make-rgba-design ink))
		   (source-fn (make-rgba-design-fn rgba-design)))
	      (declare (type design-fn source-fn))
	      (flet ((fill-color ()
		       (let ((s-red (uniform-rgba-design-red rgba-design))
			     (s-green (uniform-rgba-design-green rgba-design))
			     (s-blue (uniform-rgba-design-blue rgba-design))
			     (s-alpha (uniform-rgba-design-alpha rgba-design)))
			 (loop
			    for j from y-min to y-max
			    do
			      (loop
				 for i from x-min to x-max
				 do
				   (multiple-value-bind (r.bg g.bg b.bg a.bg)
				       (rgba-image-data-get-pixel-octet data-image (+ dx i) (+ dy j))
				     (multiple-value-bind (a.m)
					 (mask-image-data-get-alpha-octet data-mask i j)
				       (multiple-value-bind (red green blue alpha)	  
					   (octet-blend r.bg g.bg b.bg a.bg s-red s-green s-blue s-alpha a.m)
					 (rgba-image-data-set-pixel-octet data-image (+ dx i) (+ dy j)
								    red green blue alpha))))))))
		     (fill-function ()
		       (loop
			  for j from y-min to y-max
			  do
			    (loop
			       for i from x-min to x-max
			       do
				 (multiple-value-bind (r.bg g.bg b.bg a.bg)
				     (rgba-image-data-get-pixel-octet data-image (+ dx i) (+ dy j))
				   (multiple-value-bind (a.m)
				       (mask-image-data-get-alpha-octet data-mask i j)
				     (multiple-value-bind (r.fg g.fg b.fg a.fg)
					 (funcall source-fn i j)
				       (multiple-value-bind (red green blue alpha)	  
					   (octet-blend r.bg g.bg b.bg a.bg r.fg
							g.fg b.fg
							a.fg a.m)
					 (rgba-image-data-set-pixel-octet data-image (+ dx i) (+ dy j)
								    red green blue alpha)))))))))
		(when mask
		  (warn "mask not implemented"))
		(if (typep rgba-design 'uniform-rgba-design)
		    (fill-color)
		    (fill-function))
		(make-rectangle* (+ x-min dx) (+ y-min dy) (+ x-max dx) (+ y-max dy))))))))))
  
(defun rgb-image-fill2 (image &key (x 0) (y 0)
				(width (climi::image-width image))
				(height (climi::image-height image))
				(ink clim:+foreground-ink+)
				(background clim:+yellow+)
				(foreground clim:+blue+))
  (declare ;;(optimize speed)
	   (type fixnum x y width height))
  (let ((data-image (image-data image)))
    (declare (type rgba-image-data data-image))
    (let ((*background-design* background)
	  (*foreground-design* foreground))
      (let* ((rgba-design (make-rgba-design ink))
	     (source-fn (make-rgba-design-fn rgba-design))
	     (max-y (+ y height))
	     (max-x (+ x width)))
	(declare (type design-fn source-fn))
	(flet ((fill-color ()
		 (let ((s-red (uniform-rgba-design-red rgba-design))
		       (s-green (uniform-rgba-design-green rgba-design))
		       (s-blue (uniform-rgba-design-blue rgba-design))
		       (s-alpha (uniform-rgba-design-alpha rgba-design)))
		   (if (> s-alpha 250)
		       (loop
			  for j from y to max-y
			  do
			    (loop
			       for i from x to max-x
			       do
				 (rgba-image-data-set-pixel-octet data-image i j
								  s-red s-green s-blue s-alpha)))
		       (loop
			  for j from y to max-y
			  do
			    (loop
			       for i from x to max-x
			       do
				 (multiple-value-bind (r.bg g.bg b.bg a.bg)
				     (rgba-image-data-get-pixel-octet data-image i j)
				   (multiple-value-bind (red green blue alpha)	  
				       (octet-blend r.bg g.bg b.bg a.bg s-red s-green s-blue s-alpha 255)
				     (rgba-image-data-set-pixel-octet data-image i j
								red green blue alpha))))))))
	       (fill-function ()
		 (loop
		    for j from y to max-y
		    do
		      (loop
			 for i from x to max-x
			 do
			   (multiple-value-bind (r.bg g.bg b.bg a.bg)
			       (rgba-image-data-get-pixel-octet data-image i j)
			     (multiple-value-bind (r.fg g.fg b.fg a.fg)
				 (funcall source-fn i j)
			       (if (> a.fg 250)
				   (rgba-image-data-set-pixel-octet data-image i j
								    r.fg g.fg b.fg a.fg)
				   (multiple-value-bind (red green blue alpha)	  
				       (octet-blend r.bg g.bg b.bg a.bg r.fg g.fg b.fg a.fg 255)
				     (rgba-image-data-set-pixel-octet data-image i j
								red green blue alpha)))))))))
	  (if (typep rgba-design 'uniform-rgba-design)
	      (fill-color)
	      (fill-function))
	  (make-rectangle* x y (+ x width) (+ y height)))))))
