(in-package :mcclim-render)

(defclass image-mirror-mixin ()
  ((image :initform nil :reader image-mirror-image)
   (image-lock :initform (climi::make-lock "image"))
   (resize-image-p :initform t :reader image-mirror-resize-image-p)
   (dirty-region :initform nil)
   (render :initform (make-instance 'rgb-image-render-engine))))

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
(defgeneric %draw-paths (mirror paths transformation clip-region ink background foreground))
(defgeneric %fill-image-mask (mirror image-mask x y width height x-dest y-dest clip-region ink background foreground))


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

(defmethod %create-mirror-image ((mirror image-mirror-mixin) width height)
  (with-slots (image) mirror
    #|
    (let ((data (make-array (list height width)
			    :element-type '(unsigned-byte 32)
			    :initial-element #x00FFFFFF)))
      (setf image (make-instance 'climi::rgb-image
				 :width width
				 :height height
				 :alphap t
				 :data data))))
    |#
    (setf image (make-rgba-image width height)))
  (with-slots (dirty-region) mirror
    (setf dirty-region nil)))

(defmethod %notify-image-updated ((mirror image-mirror-mixin) region)
  (with-slots (dirty-region) mirror
    (if dirty-region
	(setf dirty-region (region-union dirty-region region))
	(setf dirty-region region))))

;;;
;;;
;;;

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

(defmethod %draw-paths :around ((mirror image-mirror-mixin) paths transformation region ink background foreground)
  (when (image-mirror-image mirror)
    (with-slots (image-lock) mirror
      (climi::with-lock-held (image-lock)
	(call-next-method)))))

(defmethod %draw-image ((mirror image-mirror-mixin)
			image x y width height x-dest y-dest clip-region)
  (let ((region (rgb-image-copy
		 image
		 (image-mirror-image mirror)
		 :x x :y y :width width :height height :x-dst x-dest :y-dst y-dest
		 :clip-region clip-region)))
    (%notify-image-updated mirror region)))

(defmethod %fill-image-mask ((mirror image-mirror-mixin)
			     image-mask x y width height x-dest y-dest clip-region ink background foreground)
  (let ((region (rgb-image-fill
		 (image-mirror-image mirror)
		 image-mask
		 :x x :y y :width width :height height :x-dst x-dest :y-dst y-dest
		 :clip-region clip-region
		 :ink ink
		 :background background
		 :foreground foreground)))
    (%notify-image-updated mirror region)))

(defmethod %draw-paths ((mirror image-mirror-mixin) paths transformation region ink background foreground)
  (with-slots (render) mirror
    (let ((region 
	   (draw-paths render (image-mirror-image mirror) paths transformation region ink background foreground)))
      (%notify-image-updated mirror region))))
