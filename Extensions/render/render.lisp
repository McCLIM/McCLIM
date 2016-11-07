(in-package :mcclim-render)

;;;
;;; Render engine
;;;

(defclass render-mixin ()
  ((state :initform (aa:make-state))
   (current-background :initform nil)
   (current-foreground :initform nil)
   (current-msheet :initform nil)
   (current-region :initform nil)
   (current-ink :initform nil)))


(defgeneric %flush (render))
(defgeneric %draw-paths (render mirrored-sheet paths clip-region ink backgound foreground))
(defgeneric %make-draw-fn (render mirrored-sheet clip-region rbga-design))
(defgeneric %make-draw-span-fn (render mirrored-sheet clip-region ink)) 

;;;
;;;
;;;

(defmethod %draw-paths ((render render-mixin) msheet paths region ink background foreground)
  (with-slots (state current-msheet current-region current-ink current-background current-foreground) render
    (unless (and (eq current-msheet msheet)
		 (eq current-region region)
		 (eq current-ink ink)
		 (eq current-background background)
		 (eq current-foreground foreground))
      (%flush render))
    (setf current-msheet msheet
	  current-region region
	  current-ink ink
	  current-background background
	  current-foreground foreground)
    (vectors:update-state state paths)))
	  
      
(defmethod %flush ((render render-mixin))
  (with-slots (state current-msheet current-region current-ink current-background current-foreground) render
    (let ((*background-design* current-background)
	  (*foreground-design* current-foreground))
      (when current-msheet
	(clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	    current-region
	  (when (rectanglep current-region)
	    (setf current-region nil))
	  (let ((draw-function (%make-draw-fn render current-msheet current-region (make-rgba-design current-ink)))
		(draw-span-function (%make-draw-span-fn render current-msheet current-region current-ink)))
	    (aa:cells-sweep/rectangle state
				      (round min-x)
				      (round min-y)
				      (round max-x)
				      (round max-y)
				      draw-function
				      draw-span-function)))
	(setf current-msheet nil)
	(vectors::state-reset state)))))
    

;;;
;;; function used by aa engine
;;;

(declaim (inline float-colorize))
(defun float-colorize (r.bg g.bg b.bg a.bg r.fg g.fg b.fg a.fg alpha)
  (color-blend-function  r.fg g.fg b.fg (* alpha a.fg) r.bg g.bg b.bg a.bg))

;;;
;;; macro to build drawinf function
;;;
(defmacro %make-draw-function-macro (source-code get-dest-code set-dest-code)
  `(lambda (x y alpha)
     (declare (optimize (speed 3)))
     (when (or (null clip-region) (clim:region-contains-position-p clip-region x y))
       (setf alpha (float (/ (min (abs alpha) 255) 255)))
       (when (plusp alpha)
	 (multiple-value-bind (r.fg g.fg b.fg a.fg)
	     ,source-code
	   (multiple-value-bind (r.bg g.bg b.bg a.bg)
	       ,get-dest-code
	     (multiple-value-bind (red green blue alpha)	  
		 (float-colorize r.bg g.bg b.bg a.bg r.fg g.fg b.fg a.fg alpha)
	       ,set-dest-code
	       (values red green blue alpha))))))))

(defmacro %make-draw-span-function-macro (source-code get-dest-code set-dest-code)
  `(lambda (x1 x2 y alpha)
     (declare (optimize (speed 3)))
     (labels ((prelerp (p q a)
		(let ((v (- (+ p q) (* a p))))
		  (cond
		    ((< v 0.0) 0.0)
		    ((> v 1.0) 1.0)
		    (t v)))))
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
			    (float-colorize r.bg g.bg b.bg a.bg r.fg g.fg b.fg a.fg alpha)
			  ,set-dest-code
			  (values red green blue alpha)))))))))))


;;; default implementation
(defmethod %make-draw-fn ((render render-mixin) msheet clip-region design)
  (let ((source-fn (make-rgba-design-fn design))
	(get-dest-fn (%make-image-sheet-get-function msheet))
	(set-dest-fn (%make-image-sheet-set-function msheet)))
    (%make-draw-function-macro
     (funcall source-fn x y)
     (funcall get-dest-fn x y)
     (funcall set-dest-fn x y red green blue alpha))))

(defmethod %make-draw-span-fn ((render render-mixin) msheet clip-region ink)
  (let ((source-fn (make-rgba-design-fn ink))
	(get-dest-fn (%make-image-sheet-get-function msheet))
	(set-dest-fn (%make-image-sheet-set-function msheet)))
    (%make-draw-span-function-macro
     (funcall source-fn x y)
     (funcall get-dest-fn x y)
     (funcall set-dest-fn x y red green blue alpha))))

;;;
;;; Optimization
;;;

(defmethod %make-draw-fn ((render render-mixin) msheet clip-region (design uniform-rgba-design))
  (let ((get-dest-fn (%make-image-sheet-get-function msheet))
	(set-dest-fn (%make-image-sheet-set-function msheet)))
    (let ((s-red (uniform-rgba-design-red design))
	  (s-green (uniform-rgba-design-green design))
	  (s-blue (uniform-rgba-design-blue design))
	  (s-alpha (uniform-rgba-design-alpha design))
	  (mask (uniform-rgba-design-mask design)))
      (if mask
	  (%make-draw-function-macro
	   (if (region-contains-position-p mask x y)
	       (values s-red s-green s-blue s-alpha)
	       (values 0.0 0.0 0.0 0.0))
	   (funcall get-dest-fn x y)
	   (funcall set-dest-fn x y red green blue alpha))
	  (%make-draw-function-macro
	   (values s-red s-green s-blue s-alpha)
	   (funcall get-dest-fn x y)
	   (funcall set-dest-fn x y red green blue alpha))))))

(defmethod %make-draw-span-fn ((render render-mixin) msheet clip-region (design uniform-rgba-design))
  (let ((get-dest-fn (%make-image-sheet-get-function msheet))
	(set-dest-fn (%make-image-sheet-set-function msheet)))
    (let ((s-red (uniform-rgba-design-red design))
	  (s-green (uniform-rgba-design-green design))
	  (s-blue (uniform-rgba-design-blue design))
	  (s-alpha (uniform-rgba-design-alpha design))
	  (mask (uniform-rgba-design-mask design)))
      (if mask
	  (%make-draw-span-function-macro
	   (if (region-contains-position-p mask x y)
	       (values s-red s-green s-blue s-alpha)
	       (values 0.0 0.0 0.0 0.0))
	   (funcall get-dest-fn x y)
	   (funcall set-dest-fn x y red green blue alpha))
	  (%make-draw-span-function-macro
	   (values s-red s-green s-blue s-alpha)
	   (funcall get-dest-fn x y)
	   (funcall set-dest-fn x y red green blue alpha))))))
