(in-package :mcclim-render)

;;;
;;; Mirror
;;;

(defclass opticl-mirror-mixin (image-mirror-mixin)
  ())

;;;
;;; Protocols
;;;

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

(defmethod save-mirror-image-to-file ((mirror opticl-mirror-mixin) file format)
  (declare (ignore format))
  (opticl:write-image-file file (image-mirror-image mirror)))
    
(defmethod save-mirror-image-to-stream ((mirror opticl-mirror-mixin) stream format)
  (let ((fn (gethash format *image-stream-writer-hash-table*)))
    (if fn
	(funcall fn stream (image-mirror-image mirror))
	(error "Cannot write image stream: ~S" stream))))

;;; for port
(defmethod %create-mirror-image ((mirror opticl-mirror-mixin) width height)
  (with-slots (image) mirror
    (setf image (opticl:make-8-bit-rgba-image height width :initial-element 255))))

;;;
;;; render
;;;
(defmethod %make-image-mirror-get-function ((mirror opticl-mirror-mixin))
  (let ((data (image-mirror-image mirror)))
    (lambda (x y)
      (declare (type opticl:8-bit-rgba-image data))
      (multiple-value-bind (r.bg g.bg b.bg a.bg)
         (opticl:pixel data y x)
       (values
        (float (/ r.bg 255)) (float (/ g.bg 255)) (float (/ b.bg 255)) (float (/ a.bg 255)))))))
 
(defmethod %make-image-mirror-set-function ((mirror opticl-mirror-mixin))
  (let ((data (image-mirror-image mirror)))
    (lambda (x y red green blue alpha)
      (declare (type opticl:8-bit-rgba-image data))
      (setf (opticl:pixel data y x)
           (values
            (float-octet red)
            (float-octet green)
            (float-octet blue)
            (float-octet alpha))))))
