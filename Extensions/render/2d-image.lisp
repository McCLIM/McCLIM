(in-package :mcclim-render)

(declaim (optimize speed))

;;;
;;; Opticl
;;;
(defclass 2d-image (basic-image drawable-image)
  ())

;;;
;;; 2D RGB
;;;
(deftype 2d-rgb-image-data () '(simple-array (unsigned-byte 32) (* *)))
(deftype clim-rgb-image-data () '(simple-array (unsigned-byte 32) (* *)))

(defclass 2d-rgb-image (2d-image)
  ((pixels :type (or null 2d-rgb-image-data))))

(defmethod initialize-instance :after ((image 2d-rgb-image)
                                       &key)
  (with-slots (width height)
      image
    (when (and width height (not (slot-boundp image 'pixels)))
      (setf (slot-value image 'pixels)
            (make-array (list height width)
                          :element-type '(unsigned-byte 32)
                          :initial-element #xFFFFFFFF)))))

(defun make-2d-rgb-image (width height)
  (let ((data (make-array (list height width)
                          :element-type '(unsigned-byte 32)
                          :initial-element #xFFFFFFFF)))
    (make-instance '2d-rgb-image
		   :width width
		   :height height
		   :pixels data)))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defmethod image-pixels-type ((image-class (eql '2d-rgb-image)))
    '2d-rgb-image-data)

  (defmethod make-get-rgba-octets-code ((image-class (eql '2d-rgb-image)) pixels-var x-var y-var)
    `(the (values octet octet octet octet)
          (let ((p (aref ,pixels-var ,y-var  ,x-var)))
            (values (ldb (byte 8 0) p)
                    (ldb (byte 8 8) p)
                    (ldb (byte 8 16) p)
                    (ldb (byte 8 24) p)))))

  (defmethod make-set-rgba-octets-code ((image-class (eql '2d-rgb-image)) pixels-var x-var y-var red-var green-var blue-var alpha-var)
    `(setf (aref ,pixels-var ,y-var ,x-var)
           (dpb ,red-var (byte 8 0)
                (dpb ,green-var (byte 8 8)
                     (dpb ,blue-var (byte 8 16)
                          (dpb ,alpha-var (byte 8 24) 0)))))))

;;;
;;; 2D Stencil
;;;
(deftype 2d-stencil-image-data () '(simple-array (unsigned-byte 8) (* *)))

(defclass 2d-stencil-image (2d-image)
  ((pixels :type (or null 2d-stencil-image-data))
   (alpha-p :initform t)))

(defun make-2d-stencil-image (width height)
  (let ((data (make-array (list height width)
                          :element-type '(unsigned-byte 8)
                          :initial-element #xFF)))
    (make-instance '2d-stencil-image
		   :width width
		   :height height
		   :pixels data)))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defmethod image-pixels-type ((image-class (eql '2d-stencil-image)))
    '2d-stencil-image-data)

  (defmethod make-get-alpha-octet-code ((image-class (eql '2d-stencil-image)) pixels-var x-var y-var)
    `(the octet (aref ,pixels-var ,y-var  ,x-var))))

;;;
;;; Pixeled Design
;;;

(defmethod  make-pixeled-image-rgba-octets-fn ((image 2d-rgb-image) dx dy region)
  (let ((data (image-pixels image)))
    (declare (type 2d-rgb-image-data data))
    (if (image-alpha-p image)
        (lambda (x y)
          (declare (type fixnum x y))
          (if (clim:region-contains-position-p region x y)
              (let ((p (aref data (+ y dy) (+ x dx))))
                (values (ldb (byte 8 0) p)
                        (ldb (byte 8 8) p)
                        (ldb (byte 8 16) p)
                        (ldb (byte 8 24) p)))
              (values 0 0 0 0)))
        (lambda (x y)
          (declare (type fixnum x y))
          (if (clim:region-contains-position-p region x y)
              (let ((p (aref data (+ y dy) (+ x dx))))
                (values (ldb (byte 8 0) p)
                        (ldb (byte 8 8) p)
                        (ldb (byte 8 16) p)
                        255))
              (values 0 0 0 0))))))

(defmethod  make-pixeled-image-rgba-octets-unsafe-fn ((image 2d-rgb-image) dx dy region)
  (let ((data (image-pixels image)))
    (declare (type 2d-rgb-image-data data))
    (if (image-alpha-p image)
        (lambda (x y)
          (declare (type fixnum x y))
          (let ((p (aref data (+ y dy) (+ x dx))))
            (values (ldb (byte 8 0) p)
                    (ldb (byte 8 8) p)
                    (ldb (byte 8 16) p)
                    (ldb (byte 8 24) p))))
        (lambda (x y)
          (declare (type fixnum x y))
          (let ((p (aref data (+ y dy) (+ x dx))))
            (values (ldb (byte 8 0) p)
                    (ldb (byte 8 8) p)
                    (ldb (byte 8 16) p)
                    255))))))

(defmethod  make-pixeled-image-rgba-octets-fn ((image 2d-stencil-image) dx dy region)
  (let ((data (image-pixels image)))
    (declare (type 2d-stencil-image-data data))
    (lambda (x y)
      (declare (type fixnum x y))
      (if (clim:region-contains-position-p region x y)
          (let ((p (aref data (+ y dy) (+ x dx))))
            (values 0 0 0 p))
          (values 0 0 0 0)))))

(defmethod  make-pixeled-image-rgba-octets-unsafe-fn ((image 2d-stencil-image) dx dy region)
  (let ((data (image-pixels image)))
    (declare (type 2d-stencil-image-data data))
    (lambda (x y)
      (declare (type fixnum x y))
      (let ((p (aref data (+ y dy) (+ x dx))))
        (values 0 0 0 p)))))

(defmethod %make-pixeled-design ((ink mcclim-image::rgb-pattern))
  (let* ((img (slot-value ink 'mcclim-image::image)))
    (make-pixeled-image-design :image
                               (make-instance '2d-rgb-image
                                              :width (mcclim-image::image-width img)
                                              :height (mcclim-image::image-height img)
                                              :pixels (mcclim-image::image-data img)))))

;;;
;;; Operations
;;;

(make-map-rgb-color 2d-rgb-image)
(make-copy-image 2d-rgb-image 2d-rgb-image)
(make-fill-image-without-stencil 2d-rgb-image)
(make-fill-image-with-stencil 2d-rgb-image 2d-stencil-image)

(defmethod coerce-image ((image basic-image) (image-class (eql 'mcclim-image::rgb-image)))
  (if (typep image 'mcclim-image::rgb-image)
      image
      (let ((img (coerce-image image '2d-rgb-image)))
        (make-instance 'mcclim-image::rgb-image
                       :width (image-width img)
                       :height (image-height img)
                       :data (image-pixels img)))))
