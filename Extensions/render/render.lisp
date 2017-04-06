(in-package :mcclim-render)

;;;
;;; Render engine
;;;

(defclass rgb-image-render-engine ()
  ((state :initform (aa:make-state))
   (state-lock :initform (climi::make-lock "state"))))

;;; protocol
   
(defgeneric draw-paths (render image paths transformation clip-region ink backgound foreground))
(defgeneric paths->mask-image (render mask paths transformation clip-region))

;;;
;;; Locking
;;;

(defmethod draw-paths :around ((render rgb-image-render-engine) image paths transformation region ink background foreground)
  (with-slots (state-lock) render
    (climi::with-lock-held (state-lock)
      (call-next-method))))

;;;
;;; Drawing
;;;

(defmethod draw-paths ((render rgb-image-render-engine) (image opticl-image) paths transformation clip-region ink background foreground)
  ;;(declare (optimize speed))
  (with-slots (state)
      render
    (render-update-state state paths transformation)
    (let ((*background-design* background)
	  (*foreground-design* foreground)
	  (current-clip-region
	   (if (rectanglep clip-region)
	       nil
	       clip-region)))
      (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	clip-region
	(let ((draw-function nil)
	      (draw-span-function nil)
	      (rgba-design (make-rgba-design ink)))
	  (setf draw-function
		(if (typep ink 'standard-flipping-ink)
		    (%make-xor-draw-fn image current-clip-region
				       rgba-design)
		    (%make-blend-draw-fn image current-clip-region
					 rgba-design)))
	  (setf draw-span-function
		(if (typep ink 'standard-flipping-ink)
		    (%make-xor-draw-span-fn image current-clip-region
					    rgba-design)
		    (%make-blend-draw-span-fn image current-clip-region
					      rgba-design)))
	  (render-cells-sweep/rectangle (image-data image) state
				    (floor min-x)
				    (floor min-y)
				    (ceiling max-x)
				    (ceiling max-y)
				    draw-function
				    draw-span-function))
	(vectors::state-reset state)
	(let ((region (make-rectangle* (floor min-x) (floor min-y)
				       (ceiling max-x) (ceiling max-y))))
	  region)))))

(defmethod draw-paths ((render rgb-image-render-engine) (image rgba-image) paths transformation clip-region ink background foreground)
  ;;(declare (optimize speed))
  (with-slots (state)
      render
    (render-update-state state paths transformation)
    (let ((*background-design* background)
	  (*foreground-design* foreground)
	  (current-clip-region
	   (if (rectanglep clip-region)
	       nil
	       clip-region)))
      (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	clip-region
	(let ((draw-function nil)
	      (draw-span-function nil)
	      (rgba-design (make-rgba-design ink)))
	  (setf draw-function
		(if (typep ink 'standard-flipping-ink)
		    (%make-xor-draw-fn image current-clip-region
				       rgba-design)
		    (%make-blend-draw-fn image current-clip-region
					 rgba-design)))
	  (setf draw-span-function
		(if (typep ink 'standard-flipping-ink)
		    (%make-xor-draw-span-fn image current-clip-region
					    rgba-design)
		    (%make-blend-draw-span-fn image current-clip-region
					      rgba-design)))
	  (render-cells-sweep/rectangle (image-data image) state
				    (floor min-x)
				    (floor min-y)
				    (ceiling max-x)
				    (ceiling max-y)
				    draw-function
				    draw-span-function))
	(vectors::state-reset state)
	(let ((region (make-rectangle* (floor min-x) (floor min-y)
				       (ceiling max-x) (ceiling max-y))))
	  region)))))


(defmethod paths->mask-image ((render rgb-image-render-engine) (image mask-image) paths transformation clip-region)
  (with-slots (state)
      render
    (render-update-state state paths transformation)
    (let ((current-clip-region
	   (if (rectanglep clip-region)
	       nil
	       clip-region)))
      (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	clip-region
	(let ((draw-function
	       (%make-mask-image-draw-fn image current-clip-region))
	      (draw-span-function
	       (%make-mask-image-draw-span-fn image current-clip-region)))
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
	  region)))))



