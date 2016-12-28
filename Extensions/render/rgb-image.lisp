(in-package :mcclim-render)

;;;
;;; Sheet
;;;
(defclass rgb-image-sheet-mixin (image-sheet-mixin)
  ())



;;;
;;; Pixmap
;;;

(defclass rgb-image-pixmap-mixin (image-pixmap-mixin rgb-image-sheet-mixin)
  ())

;;;
;;; Protocols
;;;

;;; saving
(defmethod save-sheet-image-to-file ((sheet rgb-image-sheet-mixin) file format)
  (declare (ignore format))
  (error "Cannot write image file: ~S" file))
    
(defmethod save-sheet-image-to-stream ((sheet rgb-image-sheet-mixin) stream format)
  (declare (ignore format))
  (error "Cannot write image stream: ~S" stream))

;;; for port
(defmethod %create-sheet-image ((sheet rgb-image-sheet-mixin) width height)
  (let ((data (make-array (list (1+ height) (1+ width))
			  :element-type '(unsigned-byte 32)
			  :initial-element #x00FFFFFF)))
    (make-instance 'climi::rgb-image
		   :width (1+ width)
		   :height (1+ height)
		   :alphap t
		   :data data)))

;;;
;;; render 
;;;
(defmethod %make-image-sheet-get-function ((sheet rgb-image-sheet-mixin))
  (let ((data (climi::image-data (image-sheet-image sheet))))
    (lambda (x y)
      (let ((p (aref data y  x)))
       (let ((r.bg (ldb (byte 8 0) p))
             (g.bg (ldb (byte 8 8) p))
             (b.bg (ldb (byte 8 16) p))
             (a.bg (- 255 (ldb (byte 8 24) p))))
         (values 
          (float (/ r.bg 255)) (float (/ g.bg 255)) (float (/ b.bg 255)) (float (/ a.bg 255))))))))

(defmethod %make-image-sheet-set-function ((sheet rgb-image-sheet-mixin))
  (let ((data (climi::image-data (image-sheet-image sheet))))
    (lambda (x y red green blue alpha)
      (setf (aref data y x)
           (dpb (float-octet red) (byte 8 0)
                (dpb (float-octet green) (byte 8 8)
                     (dpb (float-octet blue) (byte 8 16)
                          (dpb (- 255 (float-octet alpha)) (byte 8 24) 0))))))))

;;;
;;; optimization
;;;
#|
(defmacro %make-rgb-image-draw-function-macro (source-code)
  `(%make-draw-function-macro
    ,source-code
    (let ((p (aref data y  x)))
      (let ((r.bg (ldb (byte 8 0) p))
	    (g.bg (ldb (byte 8 8) p))
	    (b.bg (ldb (byte 8 16) p))
	    (a.bg (- 255 (ldb (byte 8 24) p))))
	(values 
	 (float (/ r.bg 255)) (float (/ g.bg 255)) (float (/ b.bg 255)) (float (/ a.bg 255)))))
    (setf (aref data y x)
	  (dpb (float-octet red) (byte 8 0)
	       (dpb (float-octet green) (byte 8 8)
		    (dpb (float-octet blue) (byte 8 16)
			 (dpb (- 255 (float-octet alpha)) (byte 8 24) 0)))))))

(defmacro %make-rgb-image-draw-span-function-macro (source-code)
  `(%make-draw-span-function-macro
    ,source-code
    (let ((p (aref data y  x)))
      (let ((r.bg (ldb (byte 8 0) p))
	    (g.bg (ldb (byte 8 8) p))
	    (b.bg (ldb (byte 8 16) p))
	    (a.bg (- 255 (ldb (byte 8 24) p))))
	(values 
	 (float (/ r.bg 255)) (float (/ g.bg 255)) (float (/ b.bg 255)) (float (/ a.bg 255)))))
    (setf (aref data y x)
	  (dpb (float-octet red) (byte 8 0)
	       (dpb (float-octet green) (byte 8 8)
		    (dpb (float-octet blue) (byte 8 16)
			 (dpb (- 255 (float-octet alpha)) (byte 8 24) 0)))))))


(defmethod %make-draw-fn ((render render-mixin) (msheet rgb-image-sheet-mixin) clip-region design)
  (let ((source-fn (make-rgba-design-fn design))
	(data (climi::image-data (image-sheet-image msheet))))
    (%make-rgb-image-draw-function-macro
     (funcall source-fn x y))))

(defmethod %make-draw-span-fn ((render render-mixin) (msheet rgb-image-sheet-mixin) clip-region design)
  (let ((source-fn (make-rgba-design-fn design))
	(data (climi::image-data (image-sheet-image msheet))))
    (%make-rgb-image-draw-span-function-macro
     (funcall source-fn x y))))


(defmethod %make-draw-fn ((render render-mixin) (msheet rgb-image-sheet-mixin) clip-region (design uniform-rgba-design))
  (let ((s-red (uniform-rgba-design-red design))
	(s-green (uniform-rgba-design-green design))
	(s-blue (uniform-rgba-design-blue design))
	(s-alpha (uniform-rgba-design-alpha design))
	(mask (uniform-rgba-design-mask design))
	(data (climi::image-data (image-sheet-image msheet))))
    (if mask
	(%make-rgb-image-draw-function-macro
	 (if (region-contains-position-p mask x y)
	     (values s-red s-green s-blue s-alpha)
	     (values 0.0 0.0 0.0 0.0)))
	(%make-rgb-image-draw-function-macro
	 (values s-red s-green s-blue s-alpha)))))

(defmethod %make-draw-span-fn ((render render-mixin) (msheet rgb-image-sheet-mixin) clip-region (design uniform-rgba-design))
  (let ((s-red (uniform-rgba-design-red design))
	(s-green (uniform-rgba-design-green design))
	(s-blue (uniform-rgba-design-blue design))
	(s-alpha (uniform-rgba-design-alpha design))
	(mask (uniform-rgba-design-mask design))
	(data (climi::image-data (image-sheet-image msheet))))
    (if mask
	(%make-rgb-image-draw-span-function-macro
	 (if (region-contains-position-p mask x y)
	     (values s-red s-green s-blue s-alpha)
	     (values 0.0 0.0 0.0 0.0)))
	(%make-rgb-image-draw-span-function-macro
	   (values s-red s-green s-blue s-alpha)))))


|#
