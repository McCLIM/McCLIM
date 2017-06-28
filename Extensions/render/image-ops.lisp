(in-package :mcclim-render)

(defgeneric fill-image (image design stencil &key x y width height 
                                               stencil-dx stencil-dy))
(defgeneric copy-image (src-image sx sy width height
                        dst-image x y))

(defgeneric coerce-image (image image-class))

(defgeneric read-image (source &key type width height))
(defgeneric write-image (image destination &key type quality))

;;;
;;; map functions
;;;
(defmacro make-map-rgb-color (image-class)
  `(defmethod map-rgb-color ((image ,image-class) fn)
     (let ((pixels (image-pixels image)))
       (declare (type ,(image-pixels-type image-class) pixels))
       (dotimes (x (1- (image-width image)))
         (dotimes (y (1- (image-height image)))
           (multiple-value-bind (red green blue)
               ,(make-get-rgba-octets-code image-class 'pixels 'x 'y)
             (funcall fn x y red green blue)))))))

;;;
;;; copy functions
;;;

(defmacro make-copy-image (src-image-class dst-image-class)
  `(defmethod copy-image ((src-img ,src-image-class) sx sy width height (dst-img ,dst-image-class) x y)
     (declare (type fixnum sx sy width height x y))
     (let ((src-pixels (image-pixels src-img))
           (dst-pixels (image-pixels dst-img)))
       (declare (type ,(image-pixels-type src-image-class) src-pixels)
                (type ,(image-pixels-type dst-image-class) dst-pixels))
       (let ((max-y (+ y height -1))
             (max-x (+ x width -1))
             (dy (- sy y))
             (dx (- sx x)))
         (declare (type fixnum max-x max-y dx dy))
         (flet ((copy-ff ()
                  (loop for j from y to max-y do
                       (loop for i from x to max-x do
                            (multiple-value-bind (red green blue alpha)
                                ,(make-get-rgba-octets-code src-image-class 'src-pixels `(+ i dx) `(+ j dy))
                              ,(make-set-rgba-octets-code dst-image-class 'dst-pixels 'i 'j 'red 'green 'blue 'alpha)))))
                (copy-bf ()
                  (loop for j from y to max-y do
                       (loop for i from max-x downto x do
                            (multiple-value-bind (red green blue alpha)
                                ,(make-get-rgba-octets-code src-image-class 'src-pixels `(+ i dx) `(+ j dy))
                              ,(make-set-rgba-octets-code dst-image-class 'dst-pixels 'i 'j 'red 'green 'blue 'alpha)))))
                (copy-fb ()
                  (loop for j from max-y downto y do
                       (loop for i from x to max-x do
                            (multiple-value-bind (red green blue alpha)
                                ,(make-get-rgba-octets-code src-image-class 'src-pixels `(+ i dx) `(+ j dy))
                              ,(make-set-rgba-octets-code dst-image-class 'dst-pixels 'i 'j 'red 'green 'blue 'alpha)))))
                (copy-bb ()
                  (loop for j from max-y downto y do
                       (loop for i from max-x downto x do
                            (multiple-value-bind (red green blue alpha)
                                ,(make-get-rgba-octets-code src-image-class 'src-pixels `(+ i dx) `(+ j dy))
                              ,(make-set-rgba-octets-code dst-image-class 'dst-pixels 'i 'j 'red 'green 'blue 'alpha))))))
           (when (and (> width 0) (> height 0))
             (if (eq src-img dst-img)
                 (cond
                   ((and (<= sx x) (<= sy y))
                    (copy-bb))
                   ((and (<= sx x) (> sy y))
                    (copy-bf))
                   ((and (> sx x) (<= sy y))
                    (copy-fb))
                   ((and (> sx x) (> sy y))
                    (copy-ff)))
                 (copy-ff)))))
       (make-rectangle* x y (+ x width) (+ y height)))))


;;;
;;; fill functions
;;;

(defmacro make-fill-image-with-stencil (image-class stencil-class)
  `(progn
     (defmethod fill-image ((image ,image-class) rgba-design (stencil ,stencil-class)
                            &key (x 0) (y 0) (width 0) (height 0) (stencil-dx 0) (stencil-dy 0))
       (declare (type fixnum x y width height stencil-dx stencil-dy))
       (let ((pixels (image-pixels image))
             (stencil-pixels (image-pixels stencil)))
         (declare (type ,(image-pixels-type image-class) pixels)
                  (type ,(image-pixels-type stencil-class) stencil-pixels))
         (when (and (> width 0) (> height 0))
           (let ((max-y (+ y height -1))
                 (max-x (+ x width -1)))
             (let ((source-fn (make-pixeled-rgba-octets-fn rgba-design)))
               (declare (type pixeled-design-fn source-fn))
               (loop for j from y to max-y do
                    (loop for i from x to max-x do
                         (multiple-value-bind (red green blue alpha)
                             (funcall source-fn i j)
                           (let* ((alpha-ste ,(make-get-alpha-octet-code stencil-class 'stencil-pixels `(+ stencil-dx i) `(+ stencil-dy j)))
                                  (a (octet-mult alpha alpha-ste)))
                             (if (> a 250)
                                 ,(make-set-rgba-octets-code image-class 'pixels 'i 'j 'red 'green 'blue 'a)
                                 (multiple-value-bind (r.bg g.bg b.bg a.bg)
                                     ,(make-get-rgba-octets-code image-class 'pixels 'i 'j)
                                   (multiple-value-bind (red green blue alpha)
                                       (octet-blend-function r.bg g.bg b.bg a.bg red green blue a)
                                     ,(make-set-rgba-octets-code image-class 'pixels 'i 'j 'red 'green 'blue 'alpha)))))))))))
         (make-rectangle* x y (+ x width) (+ y height))))
     (defmethod fill-image ((image ,image-class) (rgba-design pixeled-uniform-design) (stencil ,stencil-class)
                                   &key (x 0) (y 0) (width 0) (height 0) (stencil-dx 0) (stencil-dy 0))
       (declare (type fixnum x y width height stencil-dx stencil-dy))
       (let ((pixels (image-pixels image))
             (stencil-pixels (image-pixels stencil)))
         (declare (type ,(image-pixels-type image-class) pixels)
                  (type ,(image-pixels-type stencil-class) stencil-pixels))
         (when (and (> width 0) (> height 0))
           (let ((max-y (+ y height -1))
                 (max-x (+ x width -1)))
             (multiple-value-bind (red green blue alpha)
                 (values
                  (pixeled-uniform-design-red rgba-design)
                  (pixeled-uniform-design-green rgba-design)
                  (pixeled-uniform-design-blue rgba-design)
                  (pixeled-uniform-design-alpha rgba-design))
               (declare (type octet red green blue alpha))
               (loop for j from y to max-y do
                    (loop for i from x to max-x do
                         (let* ((alpha-ste ,(make-get-alpha-octet-code stencil-class 'stencil-pixels `(+ stencil-dx i) `(+ stencil-dy j)))
                                (a (octet-mult alpha alpha-ste)))
                           (if (> a 250)
                               ,(make-set-rgba-octets-code image-class 'pixels 'i 'j 'red 'green 'blue 'a)
                               (multiple-value-bind (r.bg g.bg b.bg a.bg)
                                   ,(make-get-rgba-octets-code image-class 'pixels 'i 'j)
                                 (multiple-value-bind (red green blue alpha)
                                     (octet-blend-function r.bg g.bg b.bg a.bg red green blue a)
                                   ,(make-set-rgba-octets-code image-class 'pixels 'i 'j 'red 'green 'blue 'alpha))))))))))
         (make-rectangle* x y (+ x width) (+ y height))))))

(defmacro make-fill-image-without-stencil (image-class)
  `(progn
     (defmethod fill-image ((image ,image-class) rgba-design (stencil (eql nil))
                                 &key (x 0) (y 0) (width 0) (height 0) (stencil-dx 0) (stencil-dy 0))
       (declare (type fixnum x y width height)
                (ignore stencil stencil-dx stencil-dy))
      (let ((pixels (image-pixels image)))
       (declare (type ,(image-pixels-type image-class) pixels))
       (when (and (> width 0) (> height 0))
         (let ((max-y (+ y height -1))
               (max-x (+ x width -1)))
           (let ((source-fn (make-pixeled-rgba-octets-fn rgba-design)))
             (declare (type pixeled-design-fn source-fn))
             (loop for j from y to max-y do
                  (loop for i from x to max-x do
                       (multiple-value-bind (red green blue alpha)
                           (funcall source-fn i j)
                         (if (> alpha 250)
                             ,(make-set-rgba-octets-code image-class 'pixels 'i 'j 'red 'green 'blue 'alpha)
                             (multiple-value-bind (r.bg g.bg b.bg a.bg)
                                 ,(make-get-rgba-octets-code image-class 'pixels 'i 'j)
                               (multiple-value-bind (red green blue alpha)
                                   (octet-blend-function r.bg g.bg b.bg a.bg red green blue alpha)
                                 ,(make-set-rgba-octets-code image-class 'pixels 'i 'j 'red 'green 'blue 'alpha)))))))))))
      (make-rectangle* x y (+ x width) (+ y height)))
     (defmethod fill-image ((image ,image-class) (rgba-design pixeled-uniform-design) (stencil (eql nil))
                                 &key (x 0) (y 0) (width 0) (height 0) (stencil-dx 0) (stencil-dy 0))
       (declare (type fixnum x y width height)
                (ignore stencil stencil-dx stencil-dy))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels))
         (when (and (> width 0) (> height 0))
           (let ((max-y (+ y height -1))
                 (max-x (+ x width -1)))
             (multiple-value-bind (red green blue alpha)
                 (values
                  (pixeled-uniform-design-red rgba-design)
                  (pixeled-uniform-design-green rgba-design)
                  (pixeled-uniform-design-blue rgba-design)
                  (pixeled-uniform-design-alpha rgba-design))
               (declare (type octet red green blue alpha))
               (loop for j from y to max-y do
                    (loop for i from x to max-x do
                         (if (> alpha 250)
                             ,(make-set-rgba-octets-code image-class 'pixels 'i 'j 'red 'green 'blue 'alpha)
                             (multiple-value-bind (r.bg g.bg b.bg a.bg)
                                 ,(make-get-rgba-octets-code image-class 'pixels 'i 'j)
                               (multiple-value-bind (red green blue alpha)
                                   (octet-blend-function r.bg g.bg b.bg a.bg red green blue alpha)
                                 ,(make-set-rgba-octets-code image-class 'pixels 'i 'j 'red 'green 'blue 'alpha))))))))))
       (make-rectangle* x y (+ x width) (+ y height)))))

;;;
;;; coerce functions
;;;

(defmethod coerce-image ((image basic-image) image-class)
  (if (typep image image-class)
      image
      (let ((dest (make-instance image-class
                                 :width (image-width image)
                                 :height (image-height image))))
        (copy-image image 0 0 (image-width image) (image-height image)
                    dest 0 0)
        dest)))
