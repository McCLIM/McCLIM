(in-package :mcclim-render-internals)

;;;
;;; image manipulation functions
;;;
(defgeneric image-pixels-type (image-class))
;;rgba
(defgeneric image-rgba-get-code (image-class pixels-var x-var y-var))
(defgeneric image-gray-alpha-get-code (image-class pixels-var x-var y-var))
(defgeneric image-rgba-set-code (image-class pixels-var x-var y-var
                                 red-var green-var blue-var alpha-var))
(defgeneric image-rgba-blend-code (image-class pixels-var x-var y-var
                                   red-var green-var blue-var alpha-var))
;; rgb
(defgeneric image-rgb-get-code (image-class pixels-var x-var y-var))
(defgeneric image-rgb-set-code (image-class pixels-var x-var y-var
                                red-var green-var blue-var))
(defgeneric image-rgb-blend-code (image-class pixels-var x-var y-var
                                  red-var green-var blue-var alpha-var))
(defgeneric image-rgb-xor-blend-code (image-class pixels-var x-var y-var
                                  red-var green-var blue-var alpha-var))
;; gray
(defgeneric image-gray-get-code (image-class pixels-var x-var y-var))

(defgeneric image-gray-set-code (image-class pixels-var x-var y-var
                                 gray-var))
(defgeneric image-gray-blend-code (image-class pixels-var x-var y-var
                                   gray-var alpha-var))
;; alpha
(defgeneric image-alpha-get-code (image-class pixels-var x-var y-var))
(defgeneric image-alpha-set-code (image-class pixels-var x-var y-var
                                 gray-var))
(defgeneric image-alpha-blend-code (image-class pixels-var x-var y-var
                                    alpha-var))
;; rgba
(deftype image-rgba-get-fn () '(function (fixnum fixnum) (values octet octet octet octet)))
(deftype image-gray-alpha-get-fn () '(function (fixnum fixnum) (values octet octet)))
(deftype image-rgba-set-fn () '(function (fixnum fixnum octet octet octet octet)))
(deftype image-rgba-blend-fn () '(function (fixnum fixnum octet octet octet octet)))
;; rgb
(deftype image-rgb-get-fn () '(function (fixnum fixnum) (values octet octet octet)))
(deftype image-rgb-set-fn () '(function (fixnum fixnum octet octet octet)))
(deftype image-rgb-blend-fn () '(function (fixnum fixnum octet octet octet octet)))
(deftype image-rgb-xor-blend-fn () '(function (fixnum fixnum octet octet octet octet)))
;; gray
(deftype image-gray-get-fn () '(function (fixnum fixnum) octet))
(deftype image-gray-set-fn () '(function (fixnum fixnum octet)))
(deftype image-gray-blend-fn () '(function (fixnum fixnum octet octet)))
;; alpha
(deftype image-alpha-get-fn () '(function (fixnum fixnum) octet))
(deftype image-alpha-set-fn () '(function (fixnum fixnum octet )))
(deftype image-alpha-blend-fn () '(function (fixnum fixnum octet octet)))

;; rgba
(defgeneric image-rgba-get-fn (image &key dx dy region))
(defgeneric image-gray-alpha-get-fn (image &key dx dy region))
(defgeneric image-rgba-set-fn (image &key dx dy))
(defgeneric image-rgba-blend-fn (image &key dx dy))
;; rgb
(defgeneric image-rgb-get-fn (image &key dx dy region))
(defgeneric image-rgb-set-fn (image &key dx dy))
(defgeneric image-rgb-blend-fn (image &key dx dy))
(defgeneric image-rgb-xor-blend-fn (image &key dx dy))
;; gray
(defgeneric image-gray-get-fn (image &key dx dy region))
(defgeneric image-gray-set-fn (image &key dx dy))
(defgeneric image-gray-blend-fn (image &key dx dy))
;; alpha
(defgeneric image-alpha-get-fn (image &key dx dy region))
(defgeneric image-alpha-set-fn (image &key dx dy))
(defgeneric image-alpha-blend-fn (image &key dx dy))

;;;
;;; def image's primirives
;;;
(defmacro def-rgba-image-primitives (image-class pixels-type pixels-var x-var y-var
                                     red-var green-var blue-var alpha-var
                                     get-code set-code set-alpha-code)
  `(progn
     (defmethod image-pixels-type ((image-class (eql ',image-class)))
       ',pixels-type)
     (defmethod image-rgba-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       ,get-code)
     (defmethod image-rgba-set-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var
                                     ,red-var ,green-var ,blue-var alpha-var)
       ,set-code)
     (defmethod image-alpha-set-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var
                                      ,alpha-var)
       ,set-alpha-code)
     (defmethod image-alpha-blend-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var
                                        ,alpha-var)
        (let ((r (gensym "red"))
              (g (gensym "green"))
              (b (gensym "blue"))
              (a (gensym "alpha")))
         `(multiple-value-bind (,r ,g ,b ,a)
              ,,get-code
            (multiple-value-bind (,,alpha-var)
                (octet-alpha-blend-function ,,alpha-var ,a)
              ,,set-alpha-code))))
     (defmethod image-rgba-blend-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var
                                       ,red-var ,green-var ,blue-var ,alpha-var)
       (let ((r (gensym "red"))
             (g (gensym "green"))
             (b (gensym "blue"))
             (a (gensym "alpha")))
         `(multiple-value-bind (,r ,g ,b ,a)
              ,,get-code
            (multiple-value-bind (,red-var ,green-var ,blue-var ,,alpha-var)
                (octet-rgba-blend-function ,,red-var ,,green-var ,,blue-var ,,alpha-var
                                           ,r ,g ,b ,a)
              ,,set-code))))
     (defmethod image-rgb-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       (let ((r (gensym "red"))
             (g (gensym "green"))
             (b (gensym "blue"))
             (a (gensym "alpha")))
         `(multiple-value-bind (,r ,g ,b ,a)
              ,,get-code
            (rgba->rgb ,r ,g ,b ,a))))
     (defmethod image-gray-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       (let ((r (gensym "red"))
             (g (gensym "green"))
             (b (gensym "blue"))
             (a (gensym "alpha")))
         `(multiple-value-bind (,r ,g ,b ,a)
              ,,get-code
            (rgba->gray ,r ,g ,b ,a))))
     (defmethod image-gray-alpha-get-code ((image-class (eql ',image-class))
                                           ,pixels-var ,x-var ,y-var)
       (let ((r (gensym "red"))
             (g (gensym "green"))
             (b (gensym "blue"))
             (a (gensym "alpha")))
         `(multiple-value-bind (,r ,g ,b ,a)
              ,,get-code
            (rgba->gray-alpha ,r ,g ,b ,a))))
     (defmethod image-alpha-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       (let ((r (gensym "red"))
             (g (gensym "green"))
             (b (gensym "blue"))
             (a (gensym "alpha")))
         `(multiple-value-bind (,r ,g ,b ,a)
              ,,get-code
            (rgba->alpha ,r ,g ,b ,a))))))

(defmacro def-rgb-image-primitives (image-class pixels-type pixels-var x-var y-var
                                   red-var green-var blue-var alpha-var get-code set-code)
  `(progn
     (defmethod image-pixels-type ((image-class (eql ',image-class)))
       ',pixels-type)
     (defmethod image-rgb-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       ,get-code)
     (defmethod image-rgb-set-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var
                                    ,red-var ,green-var ,blue-var)
       ,set-code)
     (defmethod image-rgb-blend-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var
                                      ,red-var ,green-var ,blue-var ,alpha-var)
       (let ((r (gensym "red"))
             (g (gensym "green"))
             (b (gensym "blue")))
         `(if (> ,alpha-var 252)
              ,,set-code
              (multiple-value-bind (,r ,g ,b)
                  ,,get-code
                (multiple-value-bind (,,red-var ,,green-var ,,blue-var)
                    (octet-rgb-blend-function ,,red-var ,,green-var ,,blue-var ,,alpha-var
                                              ,r ,g ,b)
                  ,,set-code)))))
     (defmethod image-rgb-xor-blend-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var
                                      ,red-var ,green-var ,blue-var ,alpha-var)
       (let ((r (gensym "red"))
             (g (gensym "green"))
             (b (gensym "blue")))
         `(multiple-value-bind (,r ,g ,b)
              ,,get-code
            (multiple-value-bind (,,red-var ,,green-var ,,blue-var)
                (octet-rgb-blend-function (color-octet-xor ,r ,,red-var)
                                          (color-octet-xor ,g ,,green-var)
                                          (color-octet-xor ,b ,,blue-var)
                                          ,,alpha-var
                                          ,r ,g ,b)
              ,,set-code))))
     (defmethod image-rgba-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       (let ((r (gensym "red"))
             (g (gensym "green"))
             (b (gensym "blue")))
         `(multiple-value-bind (,r ,g ,b)
              ,,get-code
            (rgb->rgba ,r ,g ,b))))
     (defmethod image-gray-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       (let ((r (gensym "red"))
             (g (gensym "green"))
             (b (gensym "blue")))
         `(multiple-value-bind (,r ,g ,b)
              ,,get-code
            (rgb->gray ,r ,g ,b))))
     (defmethod image-alpha-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       (let ((r (gensym "red"))
             (g (gensym "green"))
             (b (gensym "blue")))
         `(multiple-value-bind (,r ,g ,b)
              ,,get-code
            (rgb->alpha ,r ,g ,b))))
     ;; to remove... compatibility
     (defmethod image-rgba-set-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var
                                     ,red-var ,green-var ,blue-var alpha-var)
       (declare (ignore alpha-var))
       ,set-code)))

(defmacro def-gray-image-primitives (image-class pixels-type pixels-var x-var y-var
                                    gray-var alpha-var get-code set-code set-alpha-code)
  `(progn
     (defmethod image-pixels-type ((image-class (eql ',image-class)))
       ',pixels-type)
     (defmethod image-gray-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       ,get-code)
     (defmethod image-gray-set-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var
                                     ,gray-var)
       ,set-code)
     (defmethod image-alpha-set-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var
                                      ,alpha-var)
       ,set-alpha-code)
     (defmethod image-alpha-blend-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var
                                        ,alpha-var)
       (let ((a (gensym "alpha")))
         `(multiple-value-bind (,a)
              ,,get-code
            (multiple-value-bind (,,alpha-var)
                (octet-alpha-blend-function ,,alpha-var ,a)
              ,,set-alpha-code))))
     (defmethod image-gray-blend-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var
                                       ,gray-var ,alpha-var)
       (let ((g (gensym "gray")))
         `(multiple-value-bind (,g)
              ,,get-code
            (multiple-value-bind (,,gray-var)
                (octet-gray-blend-function ,,gray-var ,,alpha-var ,g)
              ,,set-code))))
     (defmethod image-rgb-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       `(gray->rgb ,,get-code))
     (defmethod image-rgba-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       `(gray->rgba ,,get-code))
     (defmethod image-alpha-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       `(gray->alpha ,,get-code))))

;;;
;;; def image's functions
;;;
(defmacro def-image-functions (image-class)
  `(progn
     (defmethod image-rgba-get-fn ((image ,image-class) &key (dx 0) (dy 0) (region nil))
       (declare (ignorable dx dy))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels)
                  (type fixnum dx dy)
                  (ignorable pixels))
         (lambda (x y)
           (declare (type fixnum x y))
           (if (or (not region) (clim:region-contains-position-p region x y))
               ,(image-rgba-get-code image-class 'pixels '(+ x dx) '(+ y dy))
               (values 0 0 0)))))
     (defmethod image-rgb-get-fn ((image ,image-class) &key (dx 0) (dy 0) (region nil))
       (declare (ignorable dx dy))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels)
                  (type fixnum dx dy)
                  (ignorable pixels))
         (lambda (x y)
           (declare (type fixnum x y))
           (if (or (not region) (clim:region-contains-position-p region x y))
               ,(image-rgb-get-code image-class 'pixels '(+ x dx) '(+ y dy))
               (values 0 0 0)))))
     (defmethod image-gray-get-fn ((image ,image-class) &key (dx 0) (dy 0) (region nil))
       (declare (ignorable dx dy))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels)
                  (type fixnum dx dy)
                  (ignorable pixels))
         (lambda (x y)
           (declare (type fixnum x y))
           (if (or (not region) (clim:region-contains-position-p region x y))
               ,(image-gray-get-code image-class 'pixels '(+ x dx) '(+ y dy))
               0))))
     (defmethod image-alpha-get-fn ((image ,image-class) &key (dx 0) (dy 0) (region nil))
       (declare (ignorable dx dy))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels)
                  (type fixnum dx dy)
                  (ignorable pixels))
         (lambda (x y)
           (declare (type fixnum x y))
           (if (or (not region) (clim:region-contains-position-p region x y))
               ,(image-alpha-get-code image-class 'pixels '(+ x dx) '(+ y dy))
               0))))
     (defmethod map-rgb-color ((image ,image-class) fn &key (x 0) (y 0)
                                                         (width (image-width image))
                                                         (height (image-height image)))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels)
                  (type fixnum x y width height)
                  (ignorable pixels))
         (loop for i from x to (+ x width -1) do
              (loop for j from y to (+ y height -1) do
                   (multiple-value-bind (red green blue)
                       ,(image-rgb-get-code image-class 'pixels 'i 'j)
                     (funcall fn i j red green blue))))))))

(defmacro def-rgba-image-functions (image-class)
  `(progn
     (defmethod image-rgba-set-fn ((image ,image-class) &key (dx 0) (dy 0))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels))
         (lambda (x y red green blue alpha)
           (declare (type fixnum x y red green blue alpha))
           ,(image-rgba-set-code image-class 'pixels '(+ x dx) '(+ y dy) 'red 'green 'blue 'alpha))))
     (defmethod image-rgba-blend-fn ((image ,image-class) &key (dx 0) (dy 0))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels))
         (lambda (x y red green blue alpha)
           (declare (type fixnum x y red green blue alpha))
           ,(image-rgba-blend-code image-class 'pixels '(+ x dx) '(+ y dy) 'red 'green 'blue 'alpha))))
     (defmethod image-alpha-set-fn ((image ,image-class) &key (dx 0) (dy 0))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels))
         (lambda (x y alpha)
           (declare (type fixnum x y alpha))
           ,(image-alpha-set-code image-class 'pixels '(+ x dx) '(+ y dy) 'alpha))))
     (defmethod image-gray-alpha-get-fn ((image ,image-class) &key (dx 0) (dy 0) (region nil))
       (declare (ignorable dx dy))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels)
                  (type fixnum dx dy)
                  (ignorable pixels))
         (lambda (x y)
           (declare (type fixnum x y))
           (if (or (not region) (clim:region-contains-position-p region x y))
               (multiple-value-bind (g a)
                   ,(image-gray-alpha-get-code image-class 'pixels '(+ x dx) '(+ y dy))
                 (values g a))
               (values 0 0)))))
     (def-image-functions ,image-class)))

(defmacro def-rgb-image-functions (image-class)
  `(progn
     (defmethod image-rgb-set-fn ((image ,image-class) &key (dx 0) (dy 0))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels))
         (lambda (x y red green blue)
           (declare (type fixnum x y red green blue))
           ,(image-rgb-set-code image-class 'pixels '(+ x dx) '(+ y dy) 'red 'green 'blue))))
     (defmethod image-rgb-blend-fn ((image ,image-class) &key (dx 0) (dy 0))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels))
         (lambda (x y red green blue alpha)
           (declare (type fixnum x y red green blue alpha))
           ,(image-rgb-blend-code image-class 'pixels '(+ x dx) '(+ y dy) 'red 'green 'blue 'alpha))))
     (defmethod image-rgb-xor-blend-fn ((image ,image-class) &key (dx 0) (dy 0))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels))
         (lambda (x y red green blue alpha)
           (declare (type fixnum x y red green blue alpha))
           ,(image-rgb-xor-blend-code image-class 'pixels '(+ x dx) '(+ y dy)
                                      'red 'green 'blue 'alpha))))
     (def-image-functions ,image-class)))

(defmacro def-gray-image-functions (image-class)
  `(progn
     (defmethod image-gray-set-fn ((image ,image-class) &key (dx 0) (dy 0))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels))
         (lambda (x y gray)
           (declare (type fixnum x y gray))
           ,(image-gray-set-code image-class 'pixels '(+ x dx) '(+ y dy) 'gray))))
     (defmethod image-alpha-set-fn ((image ,image-class) &key (dx 0) (dy 0))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels))
         (lambda (x y alpha)
           (declare (type fixnum x y alpha))
           ,(image-alpha-set-code image-class 'pixels '(+ x dx) '(+ y dy) 'alpha))))
      (defmethod image-gray-blend-fn ((image ,image-class) &key (dx 0) (dy 0))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels))
         (lambda (x y gray alpha)
           (declare (type fixnum x y gray alpha))
           ,(image-gray-blend-code image-class 'pixels '(+ x dx) '(+ y dy) 'gray 'alpha))))
     (def-image-functions ,image-class)))
