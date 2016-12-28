(in-package :clim-clx-fb)

(defclass clx-fb-mirrored-sheet-mixin (mcclim-render::image-sheet-mixin
				       standard-single-mirrored-sheet-mixin)
  ((clx-image :initform nil)
   (gcontext :initform nil)
   (updating-p :initform nil)
   (dirty-region :initform nil)))


;;;
;;; Updating
;;;

(defmethod repaint-sheet :around ((sheet clx-fb-mirrored-sheet-mixin) region)
  (with-slots (updating-p) sheet
    (let ((old-updating-p updating-p))
      (setf updating-p t)
      (call-next-method)
      (setf updating-p old-updating-p))))

(defmethod allocate-space :around ((sheet clx-fb-mirrored-sheet-mixin) width height)
   (with-slots (updating-p) sheet
    (let ((old-updating-p updating-p))
      (setf updating-p t)
      (call-next-method)
      (setf updating-p old-updating-p))))

;;; for port
(defmethod mcclim-render::%create-sheet-image ((sheet clx-fb-mirrored-sheet-mixin) width height)
  (let* ((data (make-array (list (1+ height) (1+ width))
			   :element-type '(unsigned-byte 32)
			   :initial-element #x00FFFFFF))
	 (image (make-instance 'climi::rgb-image
			       :width (1+ width)
			       :height (1+ height)
			       :alphap t
			       :data data)))
    (with-slots (clx-image) sheet
      (setf clx-image
	    (xlib:create-image :bits-per-pixel 32
			       :data data
			       :depth 24
			       :width width
			       :height height
			       :format :z-pixmap)))
    image))

;;;
;;; render 
;;;
(defmethod mcclim-render::%make-image-sheet-get-function ((sheet clx-fb-mirrored-sheet-mixin))
  (let ((data (climi::image-data (image-sheet-image sheet))))
    (lambda (x y)
      (let ((p (aref data y  x)))
       (let ((b.bg (ldb (byte 8 0) p))
             (g.bg (ldb (byte 8 8) p))
             (r.bg (ldb (byte 8 16) p))
             (a.bg (- 255 (ldb (byte 8 24) p))))
         (values 
          (float (/ r.bg 255)) (float (/ g.bg 255)) (float (/ b.bg 255)) (float (/ a.bg 255))))))))

(defmethod mcclim-render::%make-image-sheet-set-function ((sheet clx-fb-mirrored-sheet-mixin))
  (let ((data (climi::image-data (image-sheet-image sheet))))
    (lambda (x y red green blue alpha)
      (setf (aref data y x)
           (dpb (mcclim-render::float-octet blue) (byte 8 0)
                (dpb (mcclim-render::float-octet green) (byte 8 8)
                     (dpb (mcclim-render::float-octet red) (byte 8 16)
                          (dpb (- 255 (mcclim-render::float-octet alpha)) (byte 8 24) 0))))))))

(defmethod realize-mirror :after ((port render-port-mixin) (sheet clx-fb-mirrored-sheet-mixin))
  (with-slots (gcontext) sheet
     (setf gcontext (xlib:create-gcontext :drawable (sheet-mirror sheet)
					  :background (values 0 0 0)
					  :foreground (values 255 255 255)))))

(defmethod destroy-mirror :before ((port render-port-mixin) (sheet clx-fb-mirrored-sheet-mixin))
  (declare (ignore port))
  (with-slots (gcontext) sheet
    (xlib:free-gcontext gcontext)))

(defgeneric image-sheet-to-x (sheet))

(defmethod image-sheet-to-x ((sheet clx-fb-mirrored-sheet-mixin))
  (with-slots (clx-image mcclim-render::state-lock gcontext dirty-region updating-p) sheet
    (climi::with-lock-held (mcclim-render::state-lock)    
      (when (and (mcclim-render::image-sheet-image sheet)
		 (sheet-mirror sheet)
		 (not updating-p))
	(mcclim-render::%%flush sheet)
	(when dirty-region
	  (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	      dirty-region
	    ;;(format *debug-io* "R: ~A ~A ~A ~A~%" (round min-x)
	    ;;(round min-y) (round max-x) (round max-y))
	    (let ((width (round (- max-x min-x)))
		  (height (round (- max-y min-y))))
	      (let ((mirror  (sheet-mirror sheet)))
		(when mirror
		  (xlib::put-image mirror
				   gcontext
				   clx-image
				   :src-x (round min-x) :src-y (round min-y)
				   :x (round min-x) :y (round min-y)
				   :width  width
				   :height height)))))
	  (setf dirty-region nil))))))
  
(defmethod mcclim-render::%%flush :around ((sheet clx-fb-mirrored-sheet-mixin))
  (with-slots (dirty-region current-clip-region current-paths-region) sheet
    (when (and current-clip-region current-paths-region) 
      (let ((cr (region-intersection current-clip-region current-paths-region)))
	(call-next-method)
	(if dirty-region
	    (setf dirty-region (region-union dirty-region cr))
	    (setf dirty-region cr))))))

(defclass clx-fb-pixmap (rgb-image-pixmap-mixin permanent-medium-sheet-output-mixin basic-pane)
  ())
