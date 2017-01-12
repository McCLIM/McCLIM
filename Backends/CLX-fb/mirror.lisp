(in-package :clim-clx-fb)

(defclass clx-fb-mirror (mcclim-render::rgb-xlib-image-mirror-mixin)
  ((xmirror :initform nil
	    :initarg :xmirror)
   (clx-image :initform nil)
   (gcontext :initform nil)
   (updating-p :initform nil)
   ))


;;; for port
(defmethod mcclim-render::%create-mirror-image :after ((sheet clx-fb-mirror) width height)
  (let ((data (climi::image-data (image-mirror-image sheet))))
    (with-slots (clx-image) sheet
      (setf clx-image
	    (xlib:create-image :bits-per-pixel 32
			       :data data
			       :depth 24
			       :width width
			       :height height
			       :format :z-pixmap)))))



(defgeneric image-mirror-to-x (sheet))

(defmethod image-mirror-to-x ((sheet mcclim-render::rgb-image-mirror-mixin))
  )

(defmethod image-mirror-to-x ((sheet xlib:window))
  )

(defmethod image-mirror-to-x ((sheet clx-fb-mirror))
  (with-slots (xmirror clx-image mcclim-render::state-lock gcontext mcclim-render::dirty-region updating-p) sheet
    (climi::with-lock-held (mcclim-render::state-lock)    
      (when (and (mcclim-render::image-mirror-image sheet)
		 xmirror
		 (not updating-p))
	(when mcclim-render::dirty-region
	  (map-over-region-set-regions #'(lambda (region)
					   (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
					       region
					     (let ((width (round (- max-x min-x)))
						   (height (round (- max-y min-y))))
					       (when xmirror
						 ;; to fix
						 (xlib::put-image xmirror
								  gcontext
								  clx-image
								  :src-x (round (max min-x 0)) :src-y (round (max min-y 0))
								  :x (round (max min-x 0)) :y (round (max min-y 0))
								  :width  (- width (min 0 (- min-x)))
								  :height (- height (min 0 (- min-y))))))))
				       mcclim-render::dirty-region)
	    ;;(format *debug-io* "R: ~A ~A ~A ~A~%" (round min-x)
	    ;;(round min-y) (round max-x) (round max-y))
	  (setf mcclim-render::dirty-region nil))))))

(defmethod clim-clx::port-set-mirror-region ((port clx-fb-port) (mirror clx-fb-mirror) mirror-region)
  (clim-clx::port-set-mirror-region port (slot-value mirror 'xmirror) mirror-region))

(defmethod clim-clx::port-set-mirror-transformation
    ((port clx-fb-port) (mirror clx-fb-mirror) mirror-transformation)
  (clim-clx::port-set-mirror-transformation port (slot-value mirror 'xmirror) mirror-transformation))

