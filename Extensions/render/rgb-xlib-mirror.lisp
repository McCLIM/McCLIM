(in-package :mcclim-render)

;;;
;;; Mirror
;;;
(defclass rgb-xlib-image-mirror-mixin (image-mirror-mixin)
  ())

;;;
;;; Protocols
;;;

;;; saving
(defmethod save-mirror-image-to-file ((mirror rgb-xlib-image-mirror-mixin) file format)
  (declare (ignore format))
  (error "Cannot write image file: ~S" file))
    
(defmethod save-mirror-image-to-stream ((mirror rgb-xlib-image-mirror-mixin) stream format)
  (declare (ignore format))
  (error "Cannot write image stream: ~S" stream))

;;; for port
(defmethod %create-mirror-image ((mirror rgb-xlib-image-mirror-mixin) width height)
  (with-slots (image) mirror
    (let ((data (make-array (list height width)
			    :element-type '(unsigned-byte 32)
			    :initial-element #x00FFFFFF)))
      (setf image (make-instance 'climi::rgb-image
				 :width width
				 :height height
				 :alphap t
				 :data data)))))
;;;
;;; render 
;;;

(defmethod %make-image-mirror-get-function ((mirror rgb-xlib-image-mirror-mixin))
  (let ((data (climi::image-data (image-mirror-image mirror))))
    (lambda (x y)
      (let ((p (aref data y  x)))
       (let ((b.bg (ldb (byte 8 0) p))
             (g.bg (ldb (byte 8 8) p))
             (r.bg (ldb (byte 8 16) p))
             (a.bg (- 255 (ldb (byte 8 24) p))))
         (values 
          (float (/ r.bg 255)) (float (/ g.bg 255)) (float (/ b.bg 255)) (float (/ a.bg 255))))))))

(defmethod %make-image-mirror-set-function ((mirror rgb-xlib-image-mirror-mixin))
  (let ((data (climi::image-data (image-mirror-image mirror))))
    (lambda (x y red green blue alpha)
      (setf (aref data y x)
           (dpb (mcclim-render::float-octet blue) (byte 8 0)
                (dpb (mcclim-render::float-octet green) (byte 8 8)
                     (dpb (mcclim-render::float-octet red) (byte 8 16)
                          (dpb (- 255 (mcclim-render::float-octet alpha)) (byte 8 24) 0))))))))
