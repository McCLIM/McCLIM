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

(defclass opticl-rgb-image (opticl-image rgb-image-mixin)
  ((pixels :type (or null opticl-rgb-image-data))))

(defmethod initialize-instance :after ((image opticl-rgb-image)
                                       &key)
  (with-slots (width height)
      image
    (when (and width height (not (slot-boundp image 'pixels)))
      (setf (slot-value image 'pixels)
            (opticl:make-8-bit-rgba-image height width :initial-element 255)))))

(defun make-opticl-rgb-image (width height)
  (make-instance 'opticl-rgb-image
                 :width width
                 :height height))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defmethod image-pixels-type ((image-class (eql 'opticl-rgb-image)))
    'opticl-rgb-image-data)

  (defmethod make-get-rgba-octets-code ((image-class (eql 'opticl-rgb-image)) pixels-var x-var y-var)
    `(the (values octet octet octet octet) (opticl:pixel ,pixels-var ,y-var ,x-var)))

  (defmethod make-set-rgba-octets-code ((image-class (eql 'opticl-rgb-image)) pixels-var x-var y-var red-var green-var blue-var alpha-var)
    `(setf (opticl:pixel ,pixels-var ,y-var ,x-var) (values ,red-var ,green-var ,blue-var ,alpha-var))))

;;;
;;; Opticl Stencil
;;;
(deftype opticl-stencil-image-data () 'opticl-core:8-bit-gray-image)

(defclass opticl-stencil-image (opticl-image stencil-image-mixin)
  ((pixels :type (or null opticl-stencil-image-data))))

(defun make-opticl-stencil-image (width height)
  (let ((data (opticl:make-8-bit-gray-image height width :initial-element 0)))
    (make-instance 'opticl-stencil-image
		   :width width
		   :height height
		   :pixels data)))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defmethod image-pixels-type ((image-class (eql 'opticl-stencil-image)))
    'opticl-stencil-image-data)

  (defmethod make-get-alpha-octet-code ((image-class (eql 'opticl-stencil-image)) pixels-var x-var y-var)
    `(the octet (opticl:pixel ,pixels-var ,y-var ,x-var)))
  (defmethod make-set-alpha-octet-code ((image-class (eql 'opticl-stencil-image)) pixels-var x-var y-var alpha-var)
    `(setf (opticl:pixel ,pixels-var ,y-var ,x-var) ,alpha-var)))


;;;
;;; Pixeled Design
;;;

(defmethod  make-pixeled-image-rgba-octets-fn ((image opticl-rgb-image) dx dy region)
  (let ((data (image-pixels image)))
    (declare (type opticl-rgb-image-data data))
    (if (image-alpha-p image)
        (lambda (x y)
          (declare (type fixnum x y))
          (if (clim:region-contains-position-p region x y)
              (opticl:pixel data (+ y dy) (+ x dx))
              (values 0 0 0 0)))
        (lambda (x y)
          (declare (type fixnum x y))
          (if (clim:region-contains-position-p region x y)
              (multiple-value-bind (red green blue)
                  (opticl:pixel data (+ y dy) (+ x dx))
                (values red
                        green
                        blue
                        255))
              (values 0 0 0 0))))))

(defmethod  make-pixeled-image-rgba-octets-unsafe-fn ((image opticl-rgb-image) dx dy region)
  (let ((data (image-pixels image)))
    (declare (type opticl-rgb-image-data data))
    (if (image-alpha-p image)
        (lambda (x y)
          (declare (type fixnum x y))
          (opticl:pixel data (+ y dy) (+ x dx)))
        (lambda (x y)
          (declare (type fixnum x y))
          (multiple-value-bind (red green blue)
              (opticl:pixel data (+ y dy) (+ x dx))
            (values red
                    green
                    blue
                    255))))))

(defmethod  make-pixeled-image-rgba-octets-fn ((image opticl-stencil-image) dx dy region)
  (let ((data (image-pixels image)))
    (declare (type opticl-stencil-image-data data))
    (lambda (x y)
      (declare (type fixnum x y))
      (if (clim:region-contains-position-p region x y)
          (let ((p (opticl:pixel data (+ y dy) (+ x dx))))
            (values 0 0 0 p))
          (values 0 0 0 0)))))

(defmethod  make-pixeled-image-rgba-octets-unsafe-fn ((image opticl-stencil-image) dx dy region)
  (let ((data (image-pixels image)))
    (declare (type opticl-stencil-image-data data))
    (lambda (x y)
      (declare (type fixnum x y))
      (let ((p (opticl:pixel data (+ y dy) (+ x dx))))
        (values 0 0 0 p)))))

;;;
;;; Operations
;;;

(make-map-rgb-color opticl-rgb-image)
(make-copy-image opticl-rgb-image opticl-rgb-image)
(make-copy-image opticl-rgb-image 2d-rgb-image)
(make-copy-image 2d-rgb-image opticl-rgb-image)
(make-fill-image-with-stencil opticl-rgb-image opticl-stencil-image)
(make-fill-image-without-stencil opticl-rgb-image)

(make-make-aa-render-draw-fn opticl-rgb-image)
(make-make-aa-render-draw-span-fn opticl-rgb-image)
(make-make-aa-render-xor-draw-fn opticl-rgb-image)
(make-make-aa-render-xor-draw-span-fn opticl-rgb-image)
(make-make-aa-render-alpha-draw-fn opticl-stencil-image)
(make-make-aa-render-alpha-draw-span-fn opticl-stencil-image)

;;;
;;; I/O
;;;

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

(defmethod write-image ((image opticl-rgb-image) destination &key (type :pnm) (quality 1.0))
  (declare (ignore quality))
  (let ((fn (gethash type *image-stream-writer-hash-table*)))
    (if fn
        (if (streamp destination)
            (funcall fn destination (image-pixels image))
            (opticl:write-image-file destination (image-pixels image)))
        (error "Cannot write image to: ~S" destination))))

(defmethod write-image ((image 2d-rgb-image) destination &key (type :pnm) (quality 1.0))
  (write-image (coerce-image image 'opticl-rgb-image)
               destination
               :type type
               :quality quality))
