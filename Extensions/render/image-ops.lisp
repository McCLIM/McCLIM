(in-package :mcclim-render-internals)

;;;
;;; Image I/O
;;;
(defmethod read-image (pathname &key format image-class image-family)
  (unless format
    (setf format (intern (string-upcase
                          (pathname-type (pathname pathname)))
                         (find-package :keyword))))
  (if (image-format-read-supported-p format)
      (let ((image (funcall (gethash format *image-file-readers*)
                            pathname)))
        (if (or image-class image-family)
            (coerce-image image image-class image-family)
            image))
      (error "image format not supproted, yet")))

(defmethod write-image (image destination &key format quality)
  (declare (ignorable quality))
  (unless format
    (setf format (intern (string-upcase
                          (pathname-type (pathname destination)))
                         (find-package :keyword))))
  (if (image-format-write-supported-p format)
      (funcall (gethash format *image-file-writer*)
               image destination)
      (error "image format not supproted, yet")))

;;;
;;; copy functions
;;;
(defmacro do-copy-image (src-img sx sy width height dst-img x y (i-var j-var) &body code)
  `(progn
     (let ((max-y (+ ,y ,height -1))
           (max-x (+ ,x ,width -1)))
       (declare (type fixnum max-x max-y))
       (flet ((copy-ff ()
                (loop for ,j-var fixnum from y to max-y do
                     (loop for ,i-var fixnum from x to max-x do
                          ,@code)))
              (copy-bf ()
                  (loop for ,j-var fixnum from y to max-y do
                       (loop for ,i-var fixnum from max-x downto x do
                            ,@code)))
              (copy-fb ()
                  (loop for ,j-var fixnum from max-y downto y do
                       (loop for ,i-var fixnum from x to max-x do
                            ,@code)))
                (copy-bb ()
                  (loop for ,j-var fixnum from max-y downto y do
                       (loop for ,i-var fixnum from max-x downto x do
                            ,@code))))
         (when (and (> ,width 0) (> ,height 0))
           (if (eq ,src-img ,dst-img)
               (cond
                 ((and (<= ,sx ,x) (<= ,sy ,y))
                  (copy-bb))
                 ((and (<= ,sx ,x) (> ,sy ,y))
                  (copy-bf))
                 ((and (> ,sx ,x) (<= ,sy ,y))
                  (copy-fb))
                 ((and (> ,sx ,x) (> ,sy ,y))
                  (copy-ff)))
               (copy-ff))))
       (make-rectangle* ,x ,y (+ ,x ,width) (+ ,y ,height)))))

(defmacro do-copy-image-fn (src-img src-fn sx sy width height dst-img dst-fn x y (i-var j-var)
                            &body code)
  `(progn
     (let ((dy (- ,sy ,y))
           (dx (- ,sx ,x)))
       (declare (type fixnum ,sx ,sy))
       (let ((src-get-fn (,src-fn src-img :dx dx :dy dy))
             (dst-set-fn (,dst-fn dst-img)))
         (declare (type ,src-fn src-get-fn)
                  (type ,dst-fn dst-set-fn))
         (do-copy-image ,src-img ,sx ,sy ,width ,height ,dst-img ,x ,y (,i-var ,j-var)
                        ,@code)))))

(defmethod copy-image :around ((src-img image-mixin) sx sy width height
                               (dst-img image-mixin) x y)
  ;; TO FIX: check image bounds
  (call-next-method src-img (round sx) (round sy) (round width) (round height)
                    dst-img (round x) (round y)))

(defmacro def-copy-image (src-image-class dst-image-class src-fn dst-fn channels)
  `(defmethod copy-image ((src-img ,src-image-class) sx sy width height
                          (dst-img ,dst-image-class) x y)
     (declare (type fixnum width height x y))
     (do-copy-image-fn src-img ,src-fn sx sy width height dst-img ,dst-fn x y (i j)
         ,(case channels
            (4
             `(multiple-value-bind (red green blue alpha)
                  (funcall src-get-fn i j)
                (funcall dst-set-fn i j red green blue alpha)))
            (3 `(multiple-value-bind (red green blue)
                    (funcall src-get-fn i j)
                  (funcall dst-set-fn i j red green blue)))
            (1
             `(funcall dst-set-fn i j (funcall src-get-fn i j)))))))

(def-copy-image image-mixin rgba-image-mixin image-rgba-get-fn image-rgba-set-fn 4)
(def-copy-image image-mixin rgb-image-mixin image-rgb-get-fn image-rgb-set-fn 3)
(def-copy-image image-mixin gray-image-mixin image-gray-get-fn image-gray-set-fn 1)

;;;
;;; blend functions
;;;
(defmethod blend-image :around ((src-img image-mixin) sx sy width height
                                (dst-img image-mixin) x y &key (alpha 255))
  ;; TO FIX: check image bounds
  (call-next-method src-img (round sx) (round sy) (round width) (round height)
                    dst-img (round x) (round y) :alpha (round alpha)))

(defmacro def-blend-image (src-image-class dst-image-class src-fn dst-fn channels)
  `(defmethod blend-image ((src-img ,src-image-class) sx sy width height
                           (dst-img ,dst-image-class) x y &key (alpha 255))
     (declare (type fixnum width height x y))
     (do-copy-image-fn src-img ,src-fn sx sy width height dst-img ,dst-fn x y (i j)
       ,(case channels
          (4
           `(multiple-value-bind (red green blue a)
                (funcall src-get-fn i j)
              (funcall dst-set-fn i j red green blue (octet-mult a alpha))))
          (3 `(multiple-value-bind (red green blue)
                  (funcall src-get-fn i j)
                (funcall dst-set-fn i j red green blue alpha)))
          (2
           `(multiple-value-bind (gray a)
                (funcall src-get-fn i j)
              (funcall dst-set-fn i j gray (octet-mult alpha a))))
          (1
           `(funcall dst-set-fn i j (funcall src-get-fn i j) alpha))))))

(def-blend-image image-mixin rgba-image-mixin
  image-rgba-get-fn image-rgba-blend-fn 4)
(def-blend-image image-mixin rgb-image-mixin
  image-rgb-get-fn image-rgb-blend-fn 3)
(def-blend-image rgba-image-mixin rgb-image-mixin
  image-rgba-get-fn image-rgb-blend-fn 4)
(def-blend-image image-mixin gray-image-mixin
  image-gray-get-fn image-gray-blend-fn 1)
(def-blend-image rgba-image-mixin gray-image-mixin
  image-gray-alpha-get-fn image-gray-blend-fn 2)

;;;
;;; images type and family
;;;

(defun %image-class (image image-class-or-type image-family)
  (if (keywordp image-class-or-type)
      (find-image-class
       (or image-family (image-family image))
       (if (eql image-class-or-type :default)
           (image-type image)
           image-class-or-type))
      image-class-or-type))

(defmethod make-image :around (image-class-or-type width height &optional (image-family *default-image-family*))
  (if (keywordp image-class-or-type)
      (let ((image-class (%image-class nil image-class-or-type image-family)))
        (call-next-method image-class width height))
      (call-next-method)))

(defmethod coerce-image :around ((image image-mixin) image-class-or-type &optional image-family)
  (if (keywordp image-class-or-type)
      (let ((image-class (%image-class image image-class-or-type image-family)))
        (call-next-method image image-class))
      (call-next-method)))

(defmethod clone-image :around ((image image-mixin) image-class-or-type &optional image-family)
  (if (keywordp image-class-or-type)
      (let ((image-class (%image-class image image-class-or-type image-family)))
        (call-next-method image image-class))
      (call-next-method)))

(defmethod crop-image :around ((image image-mixin) sx sy width height &optional image-class-or-type image-family)
  (if (keywordp image-class-or-type)
      (let ((image-class (%image-class image image-class-or-type image-family)))
        (call-next-method image sx sy width height image-class))
      (call-next-method)))

(defmethod coerce-alpha-channel :around ((image image-mixin) &optional (image-class-or-type :gray) image-family)
  (if (keywordp image-class-or-type)
      (let ((image-class (%image-class image image-class-or-type image-family)))
        (call-next-method image image-class))
      (call-next-method)))

(defmethod clone-alpha-channel :around ((image image-mixin) &optional (image-class-or-type :gray) image-family)
  (if (keywordp image-class-or-type)
      (let ((image-class (%image-class image image-class-or-type image-family)))
        (call-next-method image image-class))
      (call-next-method)))

;;;
;;; make
;;;
(defmethod make-image (image-class-or-type width height &optional image-family)
  (declare (ignore image-family))
  (make-instance image-class-or-type
                 :width width
                 :height height))

;;;
;;; coerce
;;;
(defmethod coerce-image ((image image-mixin) image-class-or-type &optional image-family)
  (declare (ignore image-family))
  (if (typep image image-class-or-type)
      image
      (clone-image image  image-class-or-type)))

;;;
;;; clone
;;;
(defmethod clone-image ((image image-mixin) image-class-or-type &optional image-family)
  (declare (ignore image-family))
  (let ((dest (make-image image-class-or-type
                          (image-width image)
                          (image-height image))))
    (copy-image image 0 0 (image-width image) (image-height image)
                dest 0 0)
    dest))

;;;
;;; crop
;;;
(defmethod crop-image ((image image-mixin) sx sy width height &optional image-class-or-type image-family)
  (declare (ignore image-family))
  (let ((dest (make-image (or image-class-or-type (class-of image))
                          width
                          height)))
    (copy-image image sx sy width height dest 0 0)
    dest))

;;;
;;; alpha channel
;;;
(defmethod copy-alpha-channel :around ((src-img image-mixin) sx sy width height
                                       (dst-img image-mixin) x y)
  ;; TO FIX: check image bounds
  (call-next-method src-img (round sx) (round sy) (round width) (round height)
                    dst-img (round x) (round y)))

(defmacro def-copy-alpha-channel (src-image-class dst-image-class)
  `(defmethod copy-alpha-channel ((src-img ,src-image-class) sx sy width height
                                  (dst-img ,dst-image-class) x y)
     (declare (type fixnum width height x y))
     (do-copy-image-fn src-img image-alpha-get-fn sx sy width height
         dst-img image-alpha-set-fn x y (i j)
       (funcall dst-set-fn i j (funcall src-get-fn i j)))))

(def-copy-alpha-channel image-mixin image-mixin)

(defmethod coerce-alpha-channel ((image image-mixin) &optional image-class image-family)
  (declare (ignore image-family))
  (if (typep image image-class)
      image
      (clone-alpha-channel image image-class)))

(defmethod clone-alpha-channel ((image image-mixin) &optional image-class image-family)
  (declare (ignore image-family))
  (let ((dest (make-image image-class
                          (image-width image)
                          (image-height image))))
    (copy-alpha-channel image 0 0 (image-width image) (image-height image)
                        dest 0 0)
    dest))

;;;
;;; Fill
;;;
(defmacro do-fill-image-fn (image set-fn x y width height design (i-var j-var src-fn dst-fn)
                            &body code)
  `(progn
     (let ((max-y (+ ,y ,height -1))
           (max-x (+ ,x ,width -1)))
       (let ((,src-fn (pixeled-rgba-fn ,design))
             (,dst-fn (,set-fn ,image)))
         (declare (type pixeled-design-fn ,src-fn)
                  (type ,set-fn ,dst-fn)
                  (ignorable ,src-fn))
         (loop for ,j-var from ,y to max-y do
              (loop for ,i-var from ,x to max-x do
                   ,@code))))))

(defmacro do-fill-image-with-stencil-fn (image set-fn x y width height design
                                         stencil stencil-dx stencil-dy
                                         (i-var j-var alpha src-fn dst-fn)
                                         &body code)
  `(let ((stencil-fn (image-alpha-get-fn ,stencil :dx ,stencil-dx :dy ,stencil-dy)))
     (declare (type image-alpha-get-fn stencil-fn))
     (do-fill-image-fn ,image ,set-fn ,x ,y ,width ,height ,design (,i-var ,j-var ,src-fn ,dst-fn)
       (let ((,alpha (funcall stencil-fn ,i-var ,j-var)))
         (declare (ignorable ,alpha))
         ,@code))))

(defmethod fill-image :around ((image image-mixin) (design pixeled-design) stencil
                               &key (x 0) (y 0)
                                 (width (image-width image))
                                 (height (image-height image))
                                 (stencil-dx 0) (stencil-dy 0))
  ;; TO FIX: check image bounds
  (call-next-method image design stencil :x (round x) :y (round y)
                    :width (round width) :height (round height)
                    :stencil-dx (and stencil-dx (round stencil-dx))
                    :stencil-dy (and stencil-dy (round stencil-dy))))

(defmacro def-fill-image-without-stencil (image-class fn channels)
  `(progn
     (defmethod fill-image ((image ,image-class) (design pixeled-design) (stencil (eql nil))
                            &key x y width height stencil-dx stencil-dy)
       (declare
        (ignore stencil stencil-dx stencil-dy)
        (type fixnum x y width height))
       (do-fill-image-fn image ,fn x y width height design (i j src-fn dst-fn)
         ,(case channels
            (4
             `(multiple-value-bind (red green blue a)
                  (funcall src-fn i j)
                (funcall dst-fn i j red green blue a)))
            (3 `(multiple-value-bind (red green blue)
                    (funcall src-fn i j)
                  (funcall dst-fn i j red green blue 255)))
            (2
             `(multiple-value-bind (gray a)
                  (funcall src-fn i j)
                (funcall dst-fn i j gray a)))
            (1
             `(funcall dst-fn i j (funcall src-fn i j) 255))))
       (make-rectangle* x y (+ x width) (+ y height)))
     (defmethod fill-image ((image ,image-class) (design pixeled-uniform-design) (stencil (eql nil))
                            &key x y width height stencil-dx stencil-dy)
       (declare
        (ignore stencil stencil-dx stencil-dy)
        (type fixnum x y width height))
       (multiple-value-bind (red green blue a)
           (values
            (pixeled-uniform-design-red design)
            (pixeled-uniform-design-green design)
            (pixeled-uniform-design-blue design)
            (pixeled-uniform-design-alpha design))
         (do-fill-image-fn image ,fn x y width height design (i j src-fn dst-fn)
           ,(case channels
              (4
               `(funcall dst-fn i j red green blue a))
              (3
               `(funcall dst-fn i j red green blue 255))
              (2
               `(funcall dst-fn i j (rgb->gray red green blue) a))
              (1
               `(funcall dst-fn i j (rgb->gray red green blue) 255)))))
       (make-rectangle* x y (+ x width) (+ y height)))))

(defmacro def-fill-image-with-stencil (image-class stencil-class fn channels)
  `(progn
     (defmethod fill-image ((image ,image-class) (design pixeled-design) (stencil ,stencil-class)
                            &key x y width height stencil-dx stencil-dy)
       (declare
        (type fixnum x y width height stencil-dx stencil-dy))
       (do-fill-image-with-stencil-fn image ,fn x y width height design
           stencil stencil-dx stencil-dy
           (i j alpha src-get-fn dst-set-fn)
           ,(case channels
              (4
               `(multiple-value-bind (red green blue a)
                    (funcall src-get-fn i j)
                  (funcall dst-set-fn i j red green blue (octet-mult a alpha))))
              (3 `(multiple-value-bind (red green blue)
                      (funcall src-get-fn i j)
                    (funcall dst-set-fn i j red green blue alpha)))
              (2
               `(multiple-value-bind (gray a)
                    (funcall src-get-fn i j)
                  (funcall dst-set-fn i j gray (octet-mult a alpha))))
              (1
               `(funcall dst-set-fn i j (funcall src-get-fn i j) alpha))))
       (make-rectangle* x y (+ x width) (+ y height)))
     (defmethod fill-image ((image ,image-class) (design pixeled-uniform-design)
                            (stencil ,stencil-class)
                            &key x y width height stencil-dx stencil-dy)
       (declare
        (type fixnum x y width height stencil-dx stencil-dy))
       (multiple-value-bind (red green blue a)
           (values
            (pixeled-uniform-design-red design)
            (pixeled-uniform-design-green design)
            (pixeled-uniform-design-blue design)
            (pixeled-uniform-design-alpha design))
         (declare (type octet red green blue a))
         (do-fill-image-with-stencil-fn image ,fn x y width height design
             stencil stencil-dx stencil-dy
             (i j alpha src-fn dst-fn)
           ,(case channels
              (4
               `(funcall dst-fn i j red green blue (octet-mult alpha a)))
              (3
               `(funcall dst-fn i j red green blue alpha))
              (2
               `(funcall dst-fn i j (rgb->gray red green blue) (octet-mult a alpha)))
              (1
               `(funcall dst-fn i j (rgb->gray red green blue) alpha)))))
       (make-rectangle* x y (+ x width) (+ y height)))))

(def-fill-image-without-stencil rgba-image-mixin image-rgba-blend-fn 4)
(def-fill-image-without-stencil rgb-image-mixin image-rgb-blend-fn 4)
(def-fill-image-with-stencil rgba-image-mixin gray-image-mixin image-rgba-blend-fn 4)
(def-fill-image-with-stencil rgb-image-mixin gray-image-mixin image-rgb-blend-fn 4)
