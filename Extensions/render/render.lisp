(in-package :mcclim-render)

;;;
;;; Render engine
;;;

(defclass render-mixin ()
  ((state :initform (aa:make-state))
   (state-lock :initform (climi::make-lock "state"))))

;;; protocol
   
(defgeneric %draw-paths (render mirrored-sheet paths clip-region ink backgound foreground))
(defgeneric %draw-image (render mirrored-sheet image x y width height x-dest y-dest))
(defgeneric %notify-image-updated (mirror region))

;;; private protocol

(defgeneric %make-blend-draw-fn (render mirrored-sheet clip-region rgba-design))
(defgeneric %make-blend-draw-span-fn (render mirrored-sheet clip-region ink)) 
(defgeneric %make-xor-draw-fn (render mirrored-sheet clip-region rgba-design))
(defgeneric %make-xor-draw-span-fn (render mirrored-sheet clip-region ink)) 

;;;
;;; Locking
;;;

(defmethod %draw-paths :around ((render render-mixin) msheet paths region ink background foreground)
  (when (and msheet (image-mirror-image render))
    (with-slots (state-lock) render
      (climi::with-lock-held (state-lock)
	(call-next-method)))))

(defmethod %draw-image :around ((render render-mixin) mirrored-sheet image x y width height x-dest y-dest)
  (when (and mirrored-sheet (image-mirror-image render))
    (with-slots (state-lock) render
      (climi::with-lock-held (state-lock)
	(call-next-method)))))
  
;;;
;;; Drawing
;;;

(defmethod %draw-paths ((render render-mixin) mirrored-sheet paths clip-region ink background foreground)
  ;; remove path outside the clipping region
  (let* ((current-paths-region +nowhere+)
	 (paths (remove-if
		 #'(lambda (path)
		     (multiple-value-bind (min-x min-y max-x max-y)
			 (path-extents path)
		       (setf current-paths-region
			     (region-union current-paths-region
					   (make-rectangle* min-x min-y max-x max-y)))
		       (region-equal
			(region-intersection clip-region
					     (make-rectangle* min-x min-y max-x max-y))
			+nowhere+)))
		 paths)))
    (with-slots (state)
	render
      (vectors:update-state state paths)
      (let ((*background-design* background)
	    (*foreground-design* foreground)
	    (current-clip-region
	     (if (rectanglep clip-region)
		 nil
		 clip-region)))
	(clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	    (region-intersection clip-region current-paths-region)
	  (let ((draw-function nil)
		(draw-span-function nil)
		(rgba-design (make-rgba-design ink)))
	    (setf draw-function
		  (if (typep ink 'standard-flipping-ink)
		      (%make-xor-draw-fn render mirrored-sheet current-clip-region
					 rgba-design)
		      (%make-blend-draw-fn render mirrored-sheet current-clip-region
					   rgba-design)))
	    (setf draw-span-function
		  (if (typep ink 'standard-flipping-ink)
		      (%make-xor-draw-span-fn render mirrored-sheet current-clip-region
					      rgba-design)
		      (%make-blend-draw-span-fn render mirrored-sheet current-clip-region
						rgba-design)))
	    (aa:cells-sweep/rectangle state
				      (floor min-x)
				      (floor min-y)
				      (ceiling max-x)
				      (ceiling max-y)
				      draw-function
				      draw-span-function))
	  (vectors::state-reset state)
	  (let ((region (make-rectangle* (floor min-x) (floor min-y)
					 (ceiling max-x) (ceiling max-y))))
	    (notify-image-updated render region)))))))

(defmethod %draw-image ((render render-mixin) mirrored-sheet image x y width height x-dest y-dest)
  ;; missing clip-region!!
  (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
      (sheet-region mirrored-sheet)
    (let ((get-dest-fn (%make-image-mirror-get-function (sheet-mirror image)))
	  (set-dest-fn (%make-image-mirror-set-function render))
	  (x-min (max 0 (- x-dest))) ;; to fix
	  (y-min (max 0 (- y-dest)))
	  (x-max (1- (round (min width (- max-x x-dest)))))
	  (y-max (1- (round (min height (- max-y y-dest))))))
      (cond
	((and (<= x x-dest) (<= y y-dest))
	 (loop
	    for j from y-max downto y-min 
	    do
	      (loop
		 for i from x-max downto x-min
		 do
		   (multiple-value-bind (red green blue alpha)
		       (funcall get-dest-fn (+ i x) (+ j y))
		     (funcall set-dest-fn (+ x-dest i) (+ y-dest j)
			      red green blue alpha)))))
	((and (<= x x-dest) (> y y-dest))
	 (loop
	    for j from y-min to y-max
	    do
	      (loop
		 for i from x-max downto x-min
		 do
		   (multiple-value-bind (red green blue alpha)
		       (funcall get-dest-fn (+ i x) (+ j y))
		     (funcall set-dest-fn (+ x-dest i) (+ y-dest j) red green blue alpha)))))
	((and (> x x-dest) (<= y y-dest))
	 (loop
	    for j from y-max downto y-min
	    do
	      (loop
		 for i from x-min to x-max
		 do
		   (multiple-value-bind (red green blue alpha)
		       (funcall get-dest-fn (+ i x) (+ j y))
		     (funcall set-dest-fn (+ x-dest i) (+ y-dest j) red green blue alpha)))))
	((and (> x x-dest) (> y y-dest))
	 (loop
	    for j from y-min to y-max
	    do
	      (loop
		 for i from x-min to x-max
		 do
		   (multiple-value-bind (red green blue alpha)
		       (funcall get-dest-fn (+ i x) (+ j y))
		     (funcall set-dest-fn (+ x-dest i) (+ y-dest j) red green blue alpha)))))))
    (let ((region (make-rectangle* x-dest y-dest (+ x-dest width) (+ y-dest height))))
      (notify-image-updated render region))))

;;;
;;; function used by aa engine
;;;

(declaim (inline float-blend))
(defun float-blend (r.bg g.bg b.bg a.bg r.fg g.fg b.fg a.fg alpha)
  (multiple-value-bind (red green blue alpha)
      (color-blend-function  r.fg g.fg b.fg (float (* alpha a.fg)) r.bg g.bg b.bg a.bg)
    (values (float red) (float green) (float blue) (float alpha))))

(declaim (inline float-xor-pixel))
(defun float-xor-pixel (d1 d2)
  (float (/ (logxor (floor (* 255 d1)) (floor (* 255 d2))) 255)))

;;;
;;; macro to build drawing function
;;;

(defmacro %make-blend-draw-function-macro (source-code get-dest-code set-dest-code)
  `(lambda (x y alpha)
     (declare (optimize (speed 3)))
     (when (or (null clip-region) (clim:region-contains-position-p clip-region x y))
       (setf alpha (float (/ (min (abs alpha) 255) 255)))
       (when (plusp alpha)
	 (multiple-value-bind (r.fg g.fg b.fg a.fg)
	     ,source-code
	   (if (> (* a.fg alpha) 0.99)
	       (multiple-value-bind (red green blue alpha)	  
		   (values r.fg g.fg b.fg 1.0)
		 ,set-dest-code
		 (values red green blue alpha))
	       (multiple-value-bind (r.bg g.bg b.bg a.bg)
		   ,get-dest-code
		 (multiple-value-bind (red green blue alpha)
		     (float-blend r.bg g.bg b.bg a.bg r.fg g.fg b.fg a.fg alpha)
		   ,set-dest-code
		   (values red green blue alpha)))))))))

(defmacro %make-blend-draw-span-function-macro (source-code get-dest-code set-dest-code)
  `(lambda (x1 x2 y alpha)
     (declare (optimize (speed 3)))
     (setf alpha (float (/ (min (abs alpha) 255) 255)))
     (when (plusp alpha)
       (loop for x from x1 below x2 do
	    (when (or (null clip-region) (clim:region-contains-position-p clip-region x y))
	      (multiple-value-bind (r.fg g.fg b.fg a.fg)
		  ,source-code
		(if (> (* a.fg alpha) 0.99)
		    (multiple-value-bind (red green blue alpha)	  
			(values r.fg g.fg b.fg 1.0)
		      ,set-dest-code
		      (values red green blue alpha))
		    (multiple-value-bind (r.bg g.bg b.bg a.bg)
			,get-dest-code
		      (multiple-value-bind (red green blue alpha)	  
			  (float-blend r.bg g.bg b.bg a.bg r.fg g.fg b.fg a.fg alpha)
			,set-dest-code
			(values red green blue alpha))))))))))

(defmacro %make-xor-draw-function-macro (source-code get-dest-code set-dest-code)
  `(lambda (x y alpha)
     (declare (optimize (speed 3)))
     (when (or (null clip-region) (clim:region-contains-position-p clip-region x y))
       (setf alpha (float (/ (min (abs alpha) 255) 255)))
       (when (plusp alpha)
	 (multiple-value-bind (r.bg g.bg b.bg a.bg)
	     ,get-dest-code
	   (multiple-value-bind (r.fg g.fg b.fg a.fg)
	       ,source-code
	     (multiple-value-bind (red green blue alpha)
		 (float-blend r.bg g.bg b.bg a.bg
			      (float-xor-pixel r.bg r.fg) (float-xor-pixel g.bg g.fg)
			      (float-xor-pixel b.bg b.fg) a.fg alpha)
	       ,set-dest-code
	       (values red green blue alpha))))))))

(defmacro %make-xor-draw-span-function-macro (source-code get-dest-code set-dest-code)
  `(lambda (x1 x2 y alpha)
     (declare (optimize (speed 3)))
     (setf alpha (float (/ (min (abs alpha) 255) 255)))
     (when (plusp alpha)
       (loop for x from x1 below x2 do
	    (when (or (null clip-region) (clim:region-contains-position-p clip-region x y))
	      (multiple-value-bind (r.bg g.bg b.bg a.bg)
		  ,get-dest-code
		(multiple-value-bind (r.fg g.fg b.fg a.fg)
		    ,source-code
		  (multiple-value-bind (red green blue alpha)
		      (float-blend r.bg g.bg b.bg a.bg
				   (float-xor-pixel r.bg r.fg) (float-xor-pixel g.bg g.fg)
				   (float-xor-pixel b.bg b.fg) a.fg alpha)
		    ,set-dest-code
		    (values red green blue alpha)))))))))

;;; default implementation
(defmethod %make-blend-draw-fn ((render render-mixin) msheet clip-region design)
  (let ((source-fn (make-rgba-design-fn design))
	(get-dest-fn (%make-image-mirror-get-function render))
	(set-dest-fn (%make-image-mirror-set-function render)))
    (%make-blend-draw-function-macro
     (funcall source-fn x y)
     (funcall get-dest-fn x y)
     (funcall set-dest-fn x y red green blue alpha))))

(defmethod %make-blend-draw-span-fn ((render render-mixin) msheet clip-region design)
  (let ((source-fn (make-rgba-design-fn design))
	(get-dest-fn (%make-image-mirror-get-function render))
	(set-dest-fn (%make-image-mirror-set-function render)))
    (%make-blend-draw-span-function-macro
     (funcall source-fn x y)
     (funcall get-dest-fn x y)
     (funcall set-dest-fn x y red green blue alpha))))

(defmethod %make-xor-draw-fn ((render render-mixin) msheet clip-region design)
  (let ((source-fn (make-rgba-design-fn design))
	(get-dest-fn (%make-image-mirror-get-function render))
	(set-dest-fn (%make-image-mirror-set-function render)))
    (%make-xor-draw-function-macro
     (funcall source-fn x y)
     (funcall get-dest-fn x y)
     (funcall set-dest-fn x y red green blue alpha))))

(defmethod %make-xor-draw-span-fn ((render render-mixin) msheet clip-region design)
  (let ((source-fn (make-rgba-design-fn design))
	(get-dest-fn (%make-image-mirror-get-function render))
	(set-dest-fn (%make-image-mirror-set-function render)))
    (%make-xor-draw-span-function-macro
     (funcall source-fn x y)
     (funcall get-dest-fn x y)
     (funcall set-dest-fn x y red green blue alpha))))


;;;
;;; Optimization
;;;

(defmethod %make-blend-draw-fn ((render render-mixin) msheet clip-region (design uniform-rgba-design))
  (let ((get-dest-fn (%make-image-mirror-get-function render))
	(set-dest-fn (%make-image-mirror-set-function render)))
    (let ((s-red (uniform-rgba-design-red design))
	  (s-green (uniform-rgba-design-green design))
	  (s-blue (uniform-rgba-design-blue design))
	  (s-alpha (uniform-rgba-design-alpha design))
	  (mask (uniform-rgba-design-mask design)))
      (if mask
	  (%make-blend-draw-function-macro
	   (if (region-contains-position-p mask x y)
	       (values s-red s-green s-blue s-alpha)
	       (values 0.0 0.0 0.0 0.0))
	   (funcall get-dest-fn x y)
	   (funcall set-dest-fn x y red green blue alpha))
	  (%make-blend-draw-function-macro
	   (values s-red s-green s-blue s-alpha)
	   (funcall get-dest-fn x y)
	   (funcall set-dest-fn x y red green blue alpha))))))

(defmethod %make-blend-draw-span-fn ((render render-mixin) msheet clip-region (design uniform-rgba-design))
  (let ((get-dest-fn (%make-image-mirror-get-function render))
	(set-dest-fn (%make-image-mirror-set-function render)))
    (let ((s-red (uniform-rgba-design-red design))
	  (s-green (uniform-rgba-design-green design))
	  (s-blue (uniform-rgba-design-blue design))
	  (s-alpha (uniform-rgba-design-alpha design))
	  (mask (uniform-rgba-design-mask design)))
      (if mask
	  (%make-blend-draw-span-function-macro
	   (if (region-contains-position-p mask x y)
	       (values s-red s-green s-blue s-alpha)
	       (values 0.0 0.0 0.0 0.0))
	   (funcall get-dest-fn x y)
	   (funcall set-dest-fn x y red green blue alpha))
	  (%make-blend-draw-span-function-macro
	   (values s-red s-green s-blue s-alpha)
	   (funcall get-dest-fn x y)
	   (funcall set-dest-fn x y red green blue alpha))))))



