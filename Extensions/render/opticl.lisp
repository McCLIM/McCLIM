(in-package :mcclim-render)

;;;
;;; Sheet
;;;

(defclass opticl-sheet-mixin (image-sheet-mixin)
  ())

;;;
;;; Pixmap
;;;

(defclass opticl-pixmap-mixin (image-pixmap-mixin opticl-sheet-mixin)
  ())

;;;
;;; Protocols
;;;

;;; for port
(defmethod %create-sheet-image ((sheet opticl-sheet-mixin) width height)
  (opticl:make-8-bit-rgba-image height width :initial-element 255))

;;; saving
(defparameter *image-stream-writer-hash-table* (make-hash-table))
(map nil (lambda (z)
           (destructuring-bind (x y) z
             (setf (gethash x *image-stream-writer-hash-table*) y)))
     '((:tiff opticl:write-tiff-stream)
       (:tif opticl:write-tiff-stream)
       (:jpeg opticl:write-jpeg-stream)
       (:jpg opticl:write-jpeg-stream)
       (:png opticl:write-png-stream)
       (:pbm opticl:write-pbm-stream)
       (:pgm opticl:write-pgm-stream)
       (:ppm opticl:write-ppm-stream)
       (:gif opticl:write-gif-stream)))

(defmethod save-sheet-image-to-file ((sheet opticl-sheet-mixin) file format)
  (declare (ignore format))
  (opticl:write-image-file file (image-sheet-image sheet)))
    
(defmethod save-sheet-image-to-stream ((sheet opticl-sheet-mixin) stream format)
  (let ((fn (gethash format *image-stream-writer-hash-table*)))
    (if fn
	(funcall fn stream (image-sheet-image sheet))
	(error "Cannot write image stream: ~S" stream))))

;;;
;;; render
;;;
(defmethod %make-image-sheet-get-function ((sheet opticl-sheet-mixin))
  (let ((data (image-sheet-image sheet)))
    (lambda (x y)
      (declare (type opticl:8-bit-rgba-image data))
      (multiple-value-bind (r.bg g.bg b.bg a.bg)
         (opticl:pixel data y x)
       (values
        (float (/ r.bg 255)) (float (/ g.bg 255)) (float (/ b.bg 255)) (float (/ a.bg 255)))))))
 
(defmethod %make-image-sheet-set-function ((sheet opticl-sheet-mixin))
  (let ((data (image-sheet-image sheet)))
    (lambda (x y red green blue alpha)
      (declare (type opticl:8-bit-rgba-image data))
      (setf (opticl:pixel data y x)
           (values
            (float-octet red)
            (float-octet green)
            (float-octet blue)
            (float-octet alpha))))))

;;;
;;; Optimization
;;;
#|
(defmacro %make-opticl-draw-function-macro (source-code)
  `(%make-draw-function-macro
    ,source-code
    (multiple-value-bind (r.bg g.bg b.bg a.bg)
	(opticl:pixel data y x)
      (values
       (float (/ r.bg 255)) (float (/ g.bg 255)) (float (/ b.bg 255)) (float (/ a.bg 255))))
    (setf (opticl:pixel data y x)
	  (values
	   (float-octet red)
	   (float-octet green)
	   (float-octet blue)
	   (float-octet alpha)))))

(defmacro %make-opticl-draw-span-function-macro (source-code)
  `(%make-draw-span-function-macro
    ,source-code
    (multiple-value-bind (r.bg g.bg b.bg a.bg)
	(opticl:pixel data y x)
      (values
       (float (/ r.bg 255)) (float (/ g.bg 255)) (float (/ b.bg 255)) (float (/ a.bg 255))))
    (setf (opticl:pixel data y x)
	  (values
	   (float-octet (float red))
	   (float-octet (float green))
	   (float-octet (float blue))
	   (float-octet (float alpha))))))

(defmethod %make-draw-fn ((render render-mixin) (msheet opticl-sheet-mixin) clip-region design)
  (let ((source-fn (make-rgba-design-fn design))
	(data (image-sheet-image msheet)))
    (declare (type opticl:8-bit-rgba-image data))
    (%make-opticl-draw-function-macro
     (funcall source-fn x y))))

(defmethod %make-draw-span-fn ((render render-mixin) (msheet opticl-sheet-mixin) clip-region design)
  (let ((source-fn (make-rgba-design-fn design))
	(data (image-sheet-image msheet)))
    (declare (type opticl:8-bit-rgba-image data))
    (%make-opticl-draw-span-function-macro
     (funcall source-fn x y))))

(defmethod %make-draw-fn ((render render-mixin) (msheet opticl-sheet-mixin) clip-region (design uniform-rgba-design))
  (let ((data (image-sheet-image msheet)))
    (declare (type opticl:8-bit-rgba-image data))
    (let ((s-red (uniform-rgba-design-red design))
	  (s-green (uniform-rgba-design-green design))
	  (s-blue (uniform-rgba-design-blue design))
	  (s-alpha (uniform-rgba-design-alpha design))
	  (mask (uniform-rgba-design-mask design)))
      (if mask
	  (%make-opticl-draw-function-macro
	   (if (region-contains-position-p mask x y)
	       (values s-red s-green s-blue s-alpha)
	       (values 0.0 0.0 0.0 0.0)))
	  (%make-opticl-draw-function-macro
	   (values s-red s-green s-blue s-alpha))))))

(defmethod %make-draw-span-fn ((render render-mixin) (msheet opticl-sheet-mixin) clip-region (design uniform-rgba-design))
  (let ((data (image-sheet-image msheet)))
    (declare (type opticl:8-bit-rgba-image data))
    (let ((s-red (uniform-rgba-design-red design))
	  (s-green (uniform-rgba-design-green design))
	  (s-blue (uniform-rgba-design-blue design))
	  (s-alpha (uniform-rgba-design-alpha design))
	  (mask (uniform-rgba-design-mask design)))
      (if mask
	  (%make-opticl-draw-span-function-macro
	   (if (region-contains-position-p mask x y)
	       (values s-red s-green s-blue s-alpha)
	       (values 0.0 0.0 0.0 0.0)))
	  (%make-opticl-draw-span-function-macro
	   (values s-red s-green s-blue s-alpha))))))

|#
