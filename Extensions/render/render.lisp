(in-package :mcclim-render)

;;;
;;; Render engine
;;;

(defclass render-mixin ()
  ((state :initform (aa:make-state))
   (state-lock :initform (climi::make-lock "state"))
   (current-background :initform nil)
   (current-foreground :initform nil)
   (current-mirrored-sheet :initform nil)
   (current-clip-region :initform nil)
   (current-ink :initform nil)
   (current-paths-region :initform +nowhere+)))

;;; protocol
   
(defgeneric %draw-paths (render mirrored-sheet paths clip-region ink backgound foreground))
(defgeneric %flush (render))
(defgeneric %make-blend-draw-fn (render mirrored-sheet clip-region rgba-design))
(defgeneric %make-blend-draw-span-fn (render mirrored-sheet clip-region ink)) 
(defgeneric %make-xor-draw-fn (render mirrored-sheet clip-region rgba-design))
(defgeneric %make-xor-draw-span-fn (render mirrored-sheet clip-region ink)) 

;;;
;;; Locking
;;;

(defmethod %draw-paths :around ((render render-mixin) msheet paths region ink background foreground)
  (with-slots (state-lock) render
    (climi::with-lock-held (state-lock)
      (call-next-method))))

;;; unlocked %flush
(defgeneric %%flush (render))

(defmethod %flush ((render render-mixin))
  (with-slots (state-lock) render
    (climi::with-lock-held (state-lock)
      (%%flush render))))

;;;
;;; Drawing
;;;

(defmethod %draw-paths ((render render-mixin) msheet paths region ink background foreground)
  (with-slots (current-mirrored-sheet current-clip-region
	       current-ink current-background
	       current-foreground current-paths-region)
      render
      (when (or (not (and (eq current-mirrored-sheet msheet)
			  (eq current-clip-region region)
			  (eq current-ink ink)
			  (eq current-background background)
			  (eq current-foreground foreground)))
		(typep current-ink 'standard-flipping-ink))
	(%%flush render))
      (%%flush render)
      (setf current-mirrored-sheet msheet
	    current-clip-region region
	    current-ink ink
	    current-background background
	    current-foreground foreground)
      (let ((paths (remove-if 
		    #'(lambda (path)
			(multiple-value-bind (min-x min-y max-x max-y)
			    (path-extents path)
			  (setf current-paths-region
				(region-union current-paths-region
					      (make-rectangle* min-x min-y max-x max-y)))
			  (region-equal
			   (region-intersection region
						(make-rectangle* min-x min-y max-x max-y))
			   +nowhere+)))
		    paths)))
	(with-slots (state)
	    render
	  (vectors:update-state state paths)))))

(defmethod %%flush :around ((render render-mixin))
  (with-slots (current-mirrored-sheet)
      render
    (when current-mirrored-sheet
      (with-slots (state current-paths-region current-background current-foreground)
	  render
	(let ((*background-design* current-background)
	      (*foreground-design* current-foreground))
	  (call-next-method))
	;; reset
	(setf current-mirrored-sheet nil)
	(setf current-paths-region +nowhere+)
	(vectors::state-reset state)))))
	  
(defmethod %%flush ((render render-mixin))
  (with-slots (state current-mirrored-sheet current-clip-region current-ink
		     current-paths-region)
      render
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	(region-intersection current-clip-region current-paths-region)
      (when (rectanglep current-clip-region)
	(setf current-clip-region nil))
      (let ((draw-function nil)
	    (draw-span-function nil)
	    (rgba-design (make-rgba-design current-ink)))
	(setf draw-function
	      (if (typep current-ink 'standard-flipping-ink)
		  (%make-xor-draw-fn render current-mirrored-sheet current-clip-region
				     rgba-design)
		  (%make-blend-draw-fn render current-mirrored-sheet current-clip-region
				       rgba-design))) 
	(setf draw-span-function
	      (if (typep current-ink 'standard-flipping-ink)
		  (%make-xor-draw-span-fn render current-mirrored-sheet current-clip-region
					  rgba-design)					  
		  (%make-blend-draw-span-fn render current-mirrored-sheet current-clip-region
					    rgba-design)))					    
	(aa:cells-sweep/rectangle state
				  (floor min-x)
				  (floor min-y)
				  (ceiling max-x)
				  (ceiling max-y)
				  draw-function
				  draw-span-function)))))
  
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
	(get-dest-fn (%make-image-sheet-get-function msheet))
	(set-dest-fn (%make-image-sheet-set-function msheet)))
    (%make-blend-draw-function-macro
     (funcall source-fn x y)
     (funcall get-dest-fn x y)
     (funcall set-dest-fn x y red green blue alpha))))

(defmethod %make-blend-draw-span-fn ((render render-mixin) msheet clip-region design)
  (let ((source-fn (make-rgba-design-fn design))
	(get-dest-fn (%make-image-sheet-get-function msheet))
	(set-dest-fn (%make-image-sheet-set-function msheet)))
    (%make-blend-draw-span-function-macro
     (funcall source-fn x y)
     (funcall get-dest-fn x y)
     (funcall set-dest-fn x y red green blue alpha))))

(defmethod %make-xor-draw-fn ((render render-mixin) msheet clip-region design)
  (let ((source-fn (make-rgba-design-fn design))
	(get-dest-fn (%make-image-sheet-get-function msheet))
	(set-dest-fn (%make-image-sheet-set-function msheet)))
    (%make-xor-draw-function-macro
     (funcall source-fn x y)
     (funcall get-dest-fn x y)
     (funcall set-dest-fn x y red green blue alpha))))

(defmethod %make-xor-draw-span-fn ((render render-mixin) msheet clip-region design)
  (let ((source-fn (make-rgba-design-fn design))
	(get-dest-fn (%make-image-sheet-get-function msheet))
	(set-dest-fn (%make-image-sheet-set-function msheet)))
    (%make-xor-draw-span-function-macro
     (funcall source-fn x y)
     (funcall get-dest-fn x y)
     (funcall set-dest-fn x y red green blue alpha))))


;;;
;;; Optimization
;;;

(defmethod %make-blend-draw-fn ((render render-mixin) msheet clip-region (design uniform-rgba-design))
  (let ((get-dest-fn (%make-image-sheet-get-function msheet))
	(set-dest-fn (%make-image-sheet-set-function msheet)))
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
  (let ((get-dest-fn (%make-image-sheet-get-function msheet))
	(set-dest-fn (%make-image-sheet-set-function msheet)))
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



