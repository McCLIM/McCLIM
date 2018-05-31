(in-package :mcclim-render-internals)

(declaim (optimize speed))
;;
;; fast copy
;;
(defmacro def-fast-copy-image (src-image-class dst-image-class get-code set-code channels)
  `(defmethod copy-image ((src-img ,src-image-class) sx sy width height
                          (dst-img ,dst-image-class) x y)
     (declare (type fixnum sx sy width height x y))
     (let ((src-pixels (image-pixels src-img))
           (dst-pixels (image-pixels dst-img)))
       (declare (type ,(image-pixels-type src-image-class) src-pixels)
                (type ,(image-pixels-type dst-image-class) dst-pixels))
       (let ((dy (- sy y))
             (dx (- sx x)))
         ,(case channels
            (4
             `(macrolet ((get-code-m (i j)
                           (,get-code ',src-image-class `src-pixels i j))
                         (set-code-m (i j r g b a)
                           (,set-code ',dst-image-class `dst-pixels i j r g b a)))
                (declare (type fixnum dx dy))
                (do-copy-image src-img sx sy width height dst-img x y (i j)
                  (multiple-value-bind (red green blue alpha)
                      (get-code-m (+ i dx) (+ j dy))
                    (set-code-m i j red green blue alpha)))))
            (3
             `(macrolet ((get-code-m (i j)
                           (,get-code ',src-image-class `src-pixels i j))
                         (set-code-m (i j r g b)
                           (,set-code ',dst-image-class `dst-pixels i j r g b)))
                (declare (type fixnum dx dy))
                (do-copy-image src-img sx sy width height dst-img x y (i j)
                  (multiple-value-bind (red green blue)
                      (get-code-m (+ i dx) (+ j dy))
                    (set-code-m i j red green blue)))))
            (1
             `(macrolet ((get-code-m (i j)
                           (,get-code ',src-image-class `src-pixels i j))
                         (set-code-m (i j v)
                           (,set-code ',dst-image-class `dst-pixels i j v)))
                (declare (type fixnum dx dy))
                (do-copy-image src-img sx sy width height dst-img x y (i j)
                  (set-code-m i j (get-code-m (+ i dx) (+ j dy)))))))))))

(defmacro def-fast-rgba-copy-image (src-image-class dst-image-class)
  `(def-fast-copy-image ,src-image-class ,dst-image-class
     image-rgba-get-code image-rgba-set-code 4))

(defmacro def-fast-rgb-copy-image (src-image-class dst-image-class)
  `(def-fast-copy-image ,src-image-class ,dst-image-class
     image-rgb-get-code image-rgb-set-code 3))

(defmacro def-fast-gray-copy-image (src-image-class dst-image-class)
  `(def-fast-copy-image ,src-image-class ,dst-image-class
     image-gray-get-code image-gray-set-code 1))

;;;
;;; fast alpha channel
;;;
(defmacro def-fast-copy-alpha-channel (src-image-class dst-image-class)
  `(defmethod copy-alpha-channel ((src-img ,src-image-class) sx sy width height
                                  (dst-img ,dst-image-class) x y)
     (declare (type fixnum sx sy width height x y))
     (let ((src-pixels (image-pixels src-img))
           (dst-pixels (image-pixels dst-img)))
       (declare (type ,(image-pixels-type src-image-class) src-pixels)
                (type ,(image-pixels-type dst-image-class) dst-pixels))
       (let ((dy (- sy y))
             (dx (- sx x)))
         (macrolet ((get-code-m (i j)
                      (image-alpha-get-code ',src-image-class `src-pixels i j))
                    (set-code-m (i j v)
                      (image-alpha-set-code ',dst-image-class `dst-pixels i j v)))
           (declare (type fixnum dx dy))
           (do-copy-image src-img sx sy width height dst-img x y (i j)
                          (set-code-m i j (get-code-m (+ i dx) (+ j dy)))))))))

;;;
;;; fast fill
;;;

(defmacro do-fill-image-code (x y width height design (i-var j-var src-fn)
                              &body code)
  `(let ((max-y (+ ,y ,height -1))
         (max-x (+ ,x ,width -1)))
     (declare (type fixnum max-x max-y))
     (let ((,src-fn (pixeled-rgba-fn ,design)))
       (declare (type pixeled-design-fn ,src-fn)
                (ignorable ,src-fn))
       (loop for ,j-var fixnum from ,y to max-y do
            (loop for ,i-var fixnum from ,x to max-x do
                 ,@code)))))

(defmacro def-fast-fill-image-without-stencil (image-class set-code channels)
  `(progn
     (defmethod fill-image ((image ,image-class) (design pixeled-design) (stencil (eql nil))
                            &key x y width height stencil-dx stencil-dy)
       (declare
        (ignore stencil stencil-dx stencil-dy)
        (type fixnum x y width height))
       (let ((dst-pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) dst-pixels))
         (macrolet ((dst-fn (i j r g b a)
                      (,set-code ',image-class 'dst-pixels i j r g b a)))
           (do-fill-image-code x y width height design (i j src-fn)
             ,(case channels
                (4
                 `(multiple-value-bind (red green blue a)
                      (funcall src-fn i j)
                    (dst-fn i j red green blue a)))
                (3
                 `(multiple-value-bind (red green blue)
                        (funcall src-fn i j)
                      (dst-fn i j red green blue 255)))
                (2
                 `(multiple-value-bind (gray a)
                      (funcall src-fn i j)
                    (dst-fn i j gray gray gray a)))
                (1
                 `(let ((gray (funcall src-fn i j)))
                    (dst-fn i j gray gray gray 255)))))))
       (make-rectangle* x y (+ x width) (+ y height)))
     (defmethod fill-image ((image ,image-class) (design pixeled-uniform-design) (stencil (eql nil))
                            &key x y width height stencil-dx stencil-dy)
       (declare
        (ignore stencil stencil-dx stencil-dy)
        (type fixnum x y width height))
       (let ((dst-pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) dst-pixels))
         (macrolet ((dst-fn (i j r g b alpha)
                      `(if (> ,alpha 250)
                           ,(image-rgb-set-code ',image-class 'dst-pixels i j r g b)
                           ,(,set-code ',image-class 'dst-pixels i j r g b alpha))))
           (multiple-value-bind (red green blue a)
               (values
                (pixeled-uniform-design-red design)
                (pixeled-uniform-design-green design)
                (pixeled-uniform-design-blue design)
                (pixeled-uniform-design-alpha design))
             (declare (type octet red green blue a))
             (let ((max-y (+ y height -1))
                   (max-x (+ x width -1)))
               (declare (type fixnum max-x max-y))
               (loop for j fixnum from y to max-y do
                    (loop for i fixnum from x to max-x do
                         (dst-fn i j red green blue a)))))))
       (make-rectangle* x y (+ x width) (+ y height)))))

(defmacro def-fast-fill-image-with-stencil (image-class set-code stencil-class channels)
  `(progn
     (defmethod fill-image ((image ,image-class) (design pixeled-design)
                            (stencil ,stencil-class)
                            &key x y width height stencil-dx stencil-dy)
       (declare
        (type fixnum x y width height))
       (let ((dst-pixels (image-pixels image))
             (stencil-pixels (image-pixels stencil)))
         (declare (type ,(image-pixels-type image-class) dst-pixels)
                  (type ,(image-pixels-type stencil-class) stencil-pixels))
         (macrolet ((dst-fn (i j r g b a)
                      (,set-code ',image-class 'dst-pixels i j r g b a))
                    (ste-fn (i j)
                      (image-alpha-get-code ',stencil-class 'stencil-pixels i j)))
           (do-fill-image-code x y width height design (i j src-fn)
             (let ((alpha (ste-fn (+ i stencil-dx) (+ j stencil-dy))))
               ,(case channels
                  (4
                   `(multiple-value-bind (red green blue a)
                        (funcall src-fn i j)
                      (let ((aa (octet-mult a alpha)))
                        (dst-fn i j red green blue aa)))))))))
       (make-rectangle* x y (+ x width) (+ y height)))
     (defmethod fill-image ((image ,image-class) (design pixeled-uniform-design) (stencil ,stencil-class)
                            &key x y width height stencil-dx stencil-dy)
       (declare
        (type fixnum x y width height))
       (let ((dst-pixels (image-pixels image))
             (stencil-pixels (image-pixels stencil)))
         (declare (type ,(image-pixels-type image-class) dst-pixels)
                  (type ,(image-pixels-type stencil-class) stencil-pixels))
         (macrolet ((dst-fn (i j r g b a)
                      (,set-code ',image-class 'dst-pixels i j r g b a))
                    (ste-fn (i j)
                      (image-alpha-get-code ',stencil-class 'stencil-pixels i j)))
           (multiple-value-bind (red green blue a)
               (values
                (pixeled-uniform-design-red design)
                (pixeled-uniform-design-green design)
                (pixeled-uniform-design-blue design)
                (pixeled-uniform-design-alpha design))
             (declare (type octet red green blue a))
             (do-fill-image-code x y width height design (i j src-fn)
               (let ((alpha (ste-fn (+ i stencil-dx) (+ j stencil-dy))))
                 ,(case channels
                    (4
                     `(dst-fn i j red green blue (octet-mult a alpha)))))))))
       (make-rectangle* x y (+ x width) (+ y height)))))
