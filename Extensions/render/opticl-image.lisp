(in-package :mcclim-render)

(declaim (optimize speed))

;;;
;;; Opticl
;;;
(defclass opticl-image (basic-image drawable-image)
  ())

;;;
;;; Opticl RGB
;;;
(deftype opticl-rgb-image-data () 'opticl-core:8-bit-rgba-image)

(defclass opticl-rgb-image (opticl-image)
  ((pixels :type (or null opticl-rgb-image-data))))

(defun make-opticl-rgb-image (width height)
  (let ((data (opticl:make-8-bit-rgba-image height width :initial-element 255)))
    (make-instance 'opticl-rgb-image
		   :width width
		   :height height
		   :pixels data)))

(defmethod map-rgb-color ((image opticl-rgb-image) fn)
  (let ((pixels (%image-pixels image)))
    (declare (type opticl-rgb-image-data pixels))
  (opticl:do-pixels (y x)
    pixels
    (multiple-value-bind (red green blue)
        (opticl:pixel pixels y x)
      (funcall fn x y red green blue)))))

(defmethod map-rgba-color ((image opticl-rgb-image) fn)
  (let ((pixels (%image-pixels image)))
    (declare (type opticl-rgb-image-data pixels))
    (opticl:do-pixels (y x)
      pixels
      (multiple-value-bind (red green blue alpha)
          (opticl:pixel pixels y x)
        (funcall fn x y red green blue alpha)))))

;;;
;;; Opticl Stancil
;;;
(deftype opticl-stancil-image-data () 'opticl-core:8-bit-gray-image)

(defclass opticl-stancil-image (opticl-image)
  ((pixels :type (or null opticl-stancil-image-data))))

(defun make-opticl-stancil-image (width height)
  (let ((data (opticl:make-8-bit-gray-image height width :initial-element 0)))
    (make-instance 'opticl-stancil-image
		   :width width
		   :height height
		   :pixels data)))
