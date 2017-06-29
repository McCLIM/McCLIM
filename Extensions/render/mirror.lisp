
(in-package :mcclim-render)

(defclass image-mirror-mixin ()
  ((image :initform nil :reader image-mirror-image)
   (image-lock :initform (climi::make-lock "image"))
   (resize-image-p :initform t :reader image-mirror-resize-image-p)
   (dirty-region :initform nil)
   (updating-p :initform nil)
   (state :initform (aa:make-state))))

(defmethod (setf image-mirror-image) (img (mirror image-mirror-mixin))
  (when img
    (with-slots (image resize-image-p) mirror
      (setf resize-image-p nil)
      (setf image img))))

;;;
;;; protocols
;;;

(defgeneric %make-image (mirror sheet))
(defgeneric %set-image-region (mirror region))
(defgeneric %create-mirror-image (mirror width height))
(defgeneric %notify-image-updated (mirror region))

(defgeneric %draw-image (mirror image x y width height x-dest y-dest clip-region))
(defgeneric %fill-paths (mirror paths transformation clip-region ink background foreground))
(defgeneric %stroke-paths (mirror paths line-style transformation clip-region ink background foreground))
(defgeneric %fill-image-mask (mirror image-mask x y width height x-dest y-dest clip-region ink background foreground))
(defgeneric %fill-image (mirror x y width height ink background foreground clip-region))

;;;
;;; implementation
;;;

(defmethod %make-image ((mirror image-mirror-mixin) sheet)
  (with-slots (image resize-image-p) mirror
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	(sheet-region sheet)
      (let ((width (ceiling (- max-x min-x)))
	    (height (ceiling (- max-y min-y))))
	(%create-mirror-image mirror (1+ width) (1+ height))))))

(defmethod %set-image-region ((mirror image-mirror-mixin) region)
  (with-slots (image resize-image-p) mirror
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
      region
      (let ((width (ceiling (- max-x min-x)))
	    (height (ceiling (- max-y min-y))))
	(if resize-image-p
	    (%create-mirror-image mirror (1+ width) (1+ height))
	    nil)))))

(defmethod %notify-image-updated ((mirror image-mirror-mixin) region)
  (when region
    (with-slots (dirty-region) mirror
      (if dirty-region
          (setf dirty-region (region-union dirty-region region))
          (setf dirty-region region)))))

(defmethod %draw-image :around ((mirror image-mirror-mixin) 
				image x y width height x-dest y-dest clip-region)
  (when (image-mirror-image mirror)
    (with-slots (image-lock) mirror
      (climi::with-lock-held (image-lock)
	(call-next-method)))))

(defmethod %fill-image-mask :around ((mirror image-mirror-mixin) 
				     image-mask x y width height x-dest y-dest clip-region ink background foreground)
  (when (image-mirror-image mirror)
    (with-slots (image-lock) mirror
      (climi::with-lock-held (image-lock)
	(call-next-method)))))

(defmethod %fill-image :around ((mirror image-mirror-mixin) 
				x y width height ink background foreground clip-region)
  (when (image-mirror-image mirror)
    (with-slots (image-lock) mirror
      (climi::with-lock-held (image-lock)
	(call-next-method)))))

(defmethod %fill-paths :around ((mirror image-mirror-mixin) paths transformation region ink background foreground)
  (when (image-mirror-image mirror)
    (with-slots (image-lock) mirror
      (climi::with-lock-held (image-lock)
	(call-next-method)))))

(defmethod %stroke-paths :around ((mirror image-mirror-mixin) paths line-style transformation region ink background foreground)
  (when (image-mirror-image mirror)
    (with-slots (image-lock) mirror
      (climi::with-lock-held (image-lock)
	(call-next-method)))))

(defmethod %draw-image ((mirror image-mirror-mixin)
			image x y width height
			to-x to-y
			clip-region)
  (when (or (not (rectanglep clip-region))
            (not (region-contains-region-p clip-region (make-rectangle* to-x to-y (+ to-x width) (+ to-y height)))))
    (warn "copy image not correct"))
  (let ((region
	 (copy-image image x y width height
                     (image-mirror-image mirror)
                     to-x to-y)))
    (%notify-image-updated mirror region)))

(defmethod %fill-image-mask ((mirror image-mirror-mixin)
			     image-mask x y width height x-dest y-dest clip-region ink background foreground)
  #+() (when (or (not (rectanglep clip-region))
            (not (region-contains-region-p clip-region (make-rectangle* x y (+ x width) (+ y height)))))
    (warn "fill image mask not correct [~A -> ~A]" clip-region (make-rectangle* x y (+ x width) (+ y height))))
  (let ((region
         (fill-image
          (image-mirror-image mirror)
          (make-pixeled-design ink :foreground foreground :background background)
          image-mask
          :x x :y y :width width :height height
          :stencil-dx x-dest :stencil-dy y-dest)))
    (%notify-image-updated mirror region)))

(defmethod %fill-image ((mirror image-mirror-mixin)
			x y width height ink background foreground clip-region)
  #+() (when (or (not (rectanglep clip-region))
            (not (region-contains-region-p clip-region (make-rectangle* x y (+ x width) (+ y height)))))
    (warn "fill image not correct [~A -> ~A]"
          clip-region
          (make-rectangle* x y (+ x width) (+ y height))))
  (let ((region (fill-image
                 (image-mirror-image mirror)
                 (make-pixeled-design ink :background background :foreground foreground)
                 nil
                 :x x :y y :width width :height height)))
    (%notify-image-updated mirror region)))

(defmethod %fill-paths ((mirror image-mirror-mixin) paths transformation region ink background foreground)
  (with-slots (state) mirror
    (let ((reg
           (aa-fill-paths (image-mirror-image mirror)
                          (and ink (make-pixeled-design ink :foreground foreground :background background))
                          paths
                          state
                          transformation
                          region)))
      (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
          reg
        (%notify-image-updated mirror (make-rectangle* (floor min-x) (floor min-y)
                                                       (ceiling max-x) (ceiling max-y)))))))

(defmethod %stroke-paths ((mirror image-mirror-mixin) paths line-style transformation region ink background foreground)
  (with-slots (state) mirror
    (let ((reg
           (aa-stroke-paths (image-mirror-image mirror)
                            (and ink (make-pixeled-design ink :foreground foreground :background background))
                            paths
                            line-style
                            state
                            transformation
                            region)))
      (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
          reg
        (%notify-image-updated mirror (make-rectangle* (floor min-x) (floor min-y)
                                                       (ceiling max-x) (ceiling max-y)))))))
