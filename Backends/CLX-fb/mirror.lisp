(in-package :clim-clx-fb)

(defclass clx-fb-mirror (mcclim-render::image-mirror-mixin)
  ((width :initform 0)
   (height :initform 0)
   (xmirror :initform nil
	    :initarg :xmirror)
   (clx-image :initform nil)
   (gcontext :initform nil)
   (updating-p :initform nil)
   (clx-image-data :initform nil)))


;;; for port
(defmethod mcclim-render::%create-mirror-image :after ((sheet clx-fb-mirror) w h)
  (with-slots (mcclim-render::dirty-region) sheet
    (setf mcclim-render::dirty-region nil))
  (let ((data (climi::image-data (image-mirror-image sheet))))
    (with-slots (width height clx-image clx-image-data) sheet
      (setf width w
	    height h)
      (setf clx-image-data (make-array (list height width)
			    :element-type '(unsigned-byte 32)
			    :initial-element #x00FFFFFF))
      (setf clx-image
	    (xlib:create-image :bits-per-pixel 32
			       :data clx-image-data
			       :depth 24
			       :width width
			       :height height
			       :format :z-pixmap)))))

(defgeneric image-mirror-to-x (sheet))

(defmethod image-mirror-to-x ((sheet mcclim-render::image-mirror-mixin))
  )

(defmethod image-mirror-to-x ((sheet xlib:window))
  )

(defun image-mirror-put (width height xmirror gcontext clx-image dirty-r)
  (map-over-region-set-regions #'(lambda (region)
				   (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
				     (region-intersection region
							  (make-rectangle* 0 0 width height))
				     (let ((width (round (- max-x min-x)))
					   (height (round (- max-y min-y))))
				       (when (and xmirror clx-image)
					 ;; to fix
					 (xlib::put-image xmirror
							  gcontext
							  clx-image
							  :src-x (round (max min-x 0)) :src-y (round (max min-y 0))
							  :x (round (max min-x 0)) :y (round (max min-y 0))
							  :width  (max 0 (- width (min 0 (- min-x))))
							  :height (max 0 (- height (min 0 (- min-y)))))))))
			       dirty-r))

(defun image-mirror-pre-put (width height xmirror sheet clx-image clx-image-data dirty-r)
  (map-over-region-set-regions #'(lambda (region)
				   (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
				       (region-intersection region (make-rectangle* 0 0 (1- width) (1- height)))
				     (when (and xmirror clx-image)
				       ;; to fix
				       (let ((img-s (mcclim-render::image-data (mcclim-render::image-mirror-image sheet)))
					     (img-d clx-image-data))
					 (declare (type mcclim-render::opticl-image-data img-s)
						  (type (simple-array (unsigned-byte 32) (* *)) img-d)
						  (type fixnum min-x max-x min-y max-y))
					 (loop for y from min-y to (max max-y)
					    do
					      (loop for x from min-x to (max max-x)
						 do
						   (multiple-value-bind (r g b a)
						       (opticl:pixel img-s y x)
						     (setf (aref img-d y x) 
							   (dpb b (byte 8 0)
								(dpb g (byte 8 8)
								     (dpb r (byte 8 16)
									  (dpb a (byte 8 24) 0))))))))))))
			       dirty-r))

(defmethod image-mirror-to-x ((sheet clx-fb-mirror))
  (declare (optimize speed))
  (with-slots (xmirror clx-image clx-image-data mcclim-render::image-lock gcontext
		       mcclim-render::dirty-region updating-p
		       width height)
      sheet
    (let ((dirty-r))
      (climi::with-lock-held (mcclim-render::image-lock)
	(setf dirty-r mcclim-render::dirty-region)
	(when (and (mcclim-render::image-mirror-image sheet)
		   clx-image
		   xmirror
		   (not updating-p))
	  (when mcclim-render::dirty-region
	    (image-mirror-pre-put width height xmirror sheet clx-image clx-image-data dirty-r)
	    (setf mcclim-render::dirty-region nil))))
      (when dirty-r
	(image-mirror-put width height xmirror gcontext clx-image dirty-r)))))


(defmethod clim-clx::port-set-mirror-region ((port clx-fb-port) (mirror clx-fb-mirror) mirror-region)
  (clim-clx::port-set-mirror-region port (slot-value mirror 'xmirror) mirror-region))

(defmethod clim-clx::port-set-mirror-transformation
    ((port clx-fb-port) (mirror clx-fb-mirror) mirror-transformation)
  (clim-clx::port-set-mirror-transformation port (slot-value mirror 'xmirror) mirror-transformation))

