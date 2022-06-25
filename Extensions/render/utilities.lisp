(in-package #:mcclim-render)

(defmacro do-regions (((src-j dest-j y1s y1d y2)
                       (src-i dest-i x1s x1d x2)
                       &key backward) &body body)
  "Region mapping macro. Iterates over two regions synchronously. When BACKWARD
is T iteration starts from the bottom-right corner, otherwise from the
top-left. Useful when we iterate over the same array and mutate its state."
  (check-type backward (member t nil))
  (if (null backward)
      `(loop
          for ,src-j fixnum from ,y1s
          for ,dest-j fixnum from ,y1d below ,y2
          do (loop
                for ,src-i fixnum from ,x1s
                for ,dest-i fixnum from ,x1d below ,x2
                do (progn ,@body)))
      `(loop
          for ,src-j fixnum from (+ ,y1s (- ,y2 ,y1d)) downto ,y1s
          for ,dest-j fixnum from ,y2 downto ,y1d
          do (loop
                for ,src-i fixnum from (+ ,x1s (- ,x2 ,x1d)) above ,x1s
                for ,dest-i fixnum from ,x2 above ,x1d
                do (progn ,@body)))))

(defmacro do-regions* (((src-j src-i x1 y1 x2 y2)) &body body)
  `(loop for ,src-j fixnum from ,y1 below ,y2 do
    (loop for ,src-i fixnum from ,x1 below ,x2 do
      (progn ,@body))))

;;; Expected variable: CLIPPING-REGION.
(defun expand-clip (clip form)
  (let ((form-clip/false
          `(macrolet ((with-clip ((x y) form)
                        (declare (ignore x y))
                        form))
             ,form))
        (form-clip/true
          `(macrolet ((with-clip ((x y) form)
                        `(when (region-contains-position-p clipping-region ,x ,y)
                           ,form)))
             ,form)))
    (if clip
        form-clip/true
        form-clip/false)))

;;; Expected variable: DESIGN.
(defun expand-rgba (type form)
  (case type
    (color
     `(let* ((rgba (climi::%rgba-value design))
             (a.fg #xff) ;; for color #xff
             (r.fg (ldb (byte 8 16) rgba))
             (g.fg (ldb (byte 8 8)  rgba))
             (b.fg (ldb (byte 8 0)  rgba)))
        (declare (type argb-pixel rgba)
                 (type octet a.fg r.fg g.fg b.fg)
                 (ignorable rgba a.fg r.fg g.fg b.fg))
        (macrolet ((with-rgba ((x y) body-form)
                     (declare (ignore x y))
                     body-form))
          ,form)))
    ((opacity climi::uniform-compositum)
     `(let* ((rgba (climi::%rgba-value design))
             (a.fg (ldb (byte 8 24) rgba))
             (r.fg (ldb (byte 8 16) rgba))
             (g.fg (ldb (byte 8 8)  rgba))
             (b.fg (ldb (byte 8 0)  rgba)))
        (declare (type argb-pixel rgba)
                 (type octet a.fg r.fg g.fg b.fg)
                 (ignorable rgba a.fg r.fg g.fg b.fg))
        (macrolet ((with-rgba ((x y) body-form)
                     (declare (ignore x y))
                     body-form))
          ,form)))
    (climi::%rgba-pattern
     `(let ((design-array (pattern-array design)))
        (declare (type argb-pixel-array design-array))
        (macrolet ((with-rgba ((x y) body-form)
                     `(let* ((rgba (aref design-array ,y ,x))
                             (a.fg (ldb (byte 8 24) rgba))
                             (r.fg (ldb (byte 8 16) rgba))
                             (g.fg (ldb (byte 8 8) rgba))
                             (b.fg (ldb (byte 8 0) rgba)))
                        (declare (type argb-pixel rgba)
                                 (type octet a.fg r.fg g.fg b.fg)
                                 (ignorable rgba a.fg r.fg g.fg b.fg))
                        ,body-form)))
          ,form)))
    (standard-flipping-ink
     `(let* ((rgba (logxor (climi::%rgba-value
                            (slot-value design 'climi::design1))
                           (climi::%rgba-value
                            (slot-value design 'climi::design2))))
             (a.fg #xff))
        (declare (type argb-pixel rgba)
                 (ignorable rgba a.fg))
        (macrolet ((with-rgba ((x y) body-form)
                     (declare (ignore x y))
                     body-form))
          ,form)))
    (otherwise
     `(macrolet ((with-rgba ((x y) body-form)
                   `(let* ((rgba (climi::%rgba-value (climi::design-ink* design ,x ,y)))
                           (a.fg (ldb (byte 8 24) rgba))
                           (r.fg (ldb (byte 8 16) rgba))
                           (g.fg (ldb (byte 8 8) rgba))
                           (b.fg (ldb (byte 8 0) rgba)))
                      (declare (type argb-pixel rgba)
                               (type octet a.fg r.fg g.fg b.fg)
                               (ignorable rgba a.fg r.fg g.fg b.fg))
                      ,body-form)))
        ,form))))

;;; Expected variables: STENCIL, STENCIL-DX, STENCIL-DY
(defun expand-mask (mask form)
  (let ((mask-form/false
          `(macrolet ((if-alpha ((x y) solid-form other-form)
                        `(with-clip (,x ,y)
                           (with-rgba (,x ,y)
                             (etypecase a.fg
                               ((integer #x00 #x00))
                               ((integer #x01 #xfe) ,other-form)
                               ((integer #xff #xff) ,solid-form))))))
             ,form))
        (mask-form/true
          `(let ((stencil-max-x (1- (array-dimension stencil-array 1)))
                 (stencil-max-y (1- (array-dimension stencil-array 0))))
             (macrolet ((if-alpha ((x y) solid-form other-form)
                          `(locally
                               (declare (type stencil-array stencil-array)
                                        (type image-index-displacement stencil-dx stencil-dy)
                                        (type image-dimension stencil-max-x stencil-max-y))
                             (let ((stencil-x (+ stencil-dx ,x))
                                   (stencil-y (+ stencil-dy ,y)))
                               (declare (type fixnum stencil-x stencil-y))
                               (when (and (<= 0 stencil-x stencil-max-x)
                                          (<= 0 stencil-y stencil-max-y))
                                 (with-clip (,x ,y)
                                   (with-rgba (,x ,y)
                                     (let ((a.fg (octet-mult
                                                  a.fg
                                                  (aref stencil-array stencil-y stencil-x))))
                                       (declare (type octet a.fg))
                                       (etypecase a.fg
                                         ((integer #x00 #x00))
                                         ((integer #x01 #xfe) ,other-form)
                                         ((integer #xff #xff) ,solid-form))))))))))
               ,form)))
        (mask-form/symbol
          `(macrolet ((if-alpha ((x y) solid-form other-form)
                        `(with-clip (,x ,y)
                           (with-rgba (,x ,y)
                             (let ((a.fg (octet-mult a.fg ,',mask)))
                               (etypecase a.fg
                                 ((integer #x00 #x00))
                                 ((integer #x01 #xfe) ,other-form)
                                 ((integer #xff #xff) ,solid-form)))))))
             ,form)))
    (case mask
      ((nil)     mask-form/false)
      ((t)       mask-form/true)
      (otherwise mask-form/symbol))))

;;; Expected variable: IMAGE.
(defun expand-value (type form)
  (let ((fill-form/flip
          `(macrolet ((set-value (x y)
                        `(if-alpha (,x ,y)
                           (setf (aref image-array ,y ,x)
                                 (logxor rgba (aref image-array ,y ,x)))
                           (setf (aref image-array ,y ,x)
                                 (let* ((rgba/dest (aref image-array ,y ,x))
                                        (rgba/flip (logxor rgba rgba/dest)))
                                   (let-rgba ((r.fg g.fg b.fg) rgba/flip)
                                     (let-rgba ((r.bg g.bg b.bg a.bg) rgba/dest)
                                       (octet-blend-function* r.fg g.fg b.fg a.fg
                                                              r.bg g.bg b.bg a.bg))))))))
             ,form))
        (fill-form/fill
          `(macrolet ((set-value (x y)
                        `(if-alpha (,x ,y)
                           (setf (aref image-array ,y ,x) rgba)
                           (setf (aref image-array ,y ,x)
                                 (let-rgba ((r.bg g.bg b.bg a.bg) (aref image-array ,y ,x))
                                   (octet-blend-function* r.fg g.fg b.fg a.fg
                                                          r.bg g.bg b.bg a.bg))))))
             ,form)))
    (if (eq type 'standard-flipping-ink)
        fill-form/flip
        fill-form/fill)))

;;; This macro assumes that the following variables are bound:
;;; IMAGE  - target array
;;; DESIGN - source design
;;; CLIPPING-REGION - a clipping region or nil
;;; STENCIL{,-DX,-DY} - drawing stencil or nil
(defmacro with-brush ((type clip mask) &body body)
  (expand-rgba type
    (expand-clip clip
      (expand-mask mask
        (expand-value type
          `(progn ,@body))))))

;;; Expects variables: IMAGE-ARRAY, DESIGN, CLIPPING-REGION, X1, Y1, X2, Y2
(defmacro with-brushes ((mask) &body body)
  (let ((designs '(color opacity climi::uniform-compositum
                   standard-flipping-ink climi::%rgba-pattern otherwise)))
    `(locally
         (declare (optimize (speed 3) (safety 0)))
       (maxf x1 0)
       (maxf y1 0)
       (minf x2 (array-dimension image-array 1))
       (minf y2 (array-dimension image-array 0))
       (typecase design
         (bounded-region
          (with-bounding-rectangle* (a b c d) design
            (maxf x1 a) (maxf y1 b)
            (minf x2 c) (minf y2 d)))
         (indirect-ink
          (setf design (indirect-ink-ink design))))
       (when (region-contains-region-p clipping-region (make-rectangle* x1 y1 x2 y2))
         (setf clipping-region nil))
       (setf x1 (floor x1)
             y1 (floor y1)
             x2 (ceiling x2)
             y2 (ceiling y2))
       (locally
           (declare (optimize (speed 3) (safety 0))
                    (type image-index x1 y1 x2 y2)
                    (type (or null region) clipping-region))
         (typecase design
           ,@(loop for type in designs
                   collect `(,type (if (null clipping-region)
                                       (with-brush (,type nil ,mask)
                                         ,@body)
                                       (with-brush (,type t ,mask)
                                         ,@body)))))
         (make-rectangle* x1 y1 x2 y2)))))

(declaim (inline %check-coords))
;;; Returns T for valid arguments, NIL for malformed width/height and signals an
;;; error if coordinates go out of arrays bounds.
(defun %check-coords (src-array dst-array sx sy dx dy width height)
  (when (or (<= width 0) (<= height 0))
    (return-from %check-coords nil))
  (unless (and (array-in-bounds-p src-array (1- (+ sy height)) (1- (+ sx width)))
               (array-in-bounds-p dst-array (1- (+ dy height)) (1- (+ dx width))))
    (error "mcclim-render operation: some coordinates are out of image bounds:~@
             src array ~s, P1 ~s, P2 ~s,~@
             dst array ~s, P1 ~s, P2 ~s."
           (reverse (array-dimensions src-array))
           (cons sx sy) (cons (+ sx width) (+ sy height))
           (reverse (array-dimensions dst-array))
           (cons dx dy)
           (cons (+ dx width) (+ dy height))))
  t)

;;; color functions

(defmacro let-rgba (((r g b &optional (a nil a-supplied-p)) rgba-integer)
                    &body body)
  (alexandria:once-only (rgba-integer)
    `(let (,@(when a-supplied-p
               `((,a (ldb (byte 8 24) ,rgba-integer))))
           (,r (ldb (byte 8 16) ,rgba-integer))
           (,g (ldb (byte 8  8) ,rgba-integer))
           (,b (ldb (byte 8  0) ,rgba-integer)))
       ,@body)))

(declaim (inline color-value->octet)
         (ftype (function (color-value) octet) color-value->octet))
(defun color-value->octet (v)
  (round (* 255 v)))

(declaim (inline color-octet->value)
         (ftype (function (octet) color-value) color-octet->value))
(defun color-octet->value (o)
  (/ o 255))

(declaim (inline color-octet-xor)
         (ftype (function (octet octet) octet) color-octet-xor))
(defun color-octet-xor (d1 d2)
  (logxor d1 d2))

(declaim (inline octet-mult)
         (ftype (function (octet octet) octet) octet-mult))
(defun octet-mult (a b)
  (let ((temp (+ (* a b) #x80)))
    (logand #xFF (ash (+ (ash temp -8) temp) -8))))

;;; blend functions

(declaim (inline %lerp)
         (ftype (function (octet octet octet) octet) %lerp))
(defun %lerp (p q a)
  (logand #xFF (if (>= q p)
                   (+ p (octet-mult a (- q p)))
                   (- p (octet-mult a (- p q))))))

(declaim (inline %prelerp)
         (ftype (function (octet octet octet) octet) %prelerp))
(defun %prelerp (p q a)
  (logand #xFF (- (+ p q) (octet-mult a p))))

(declaim (inline %byte-blend-value)
         (ftype (function (octet octet octet octet) octet) %byte-blend-value))
(defun %byte-blend-value (fg bg a.fg a.bg)
  (let ((gamma (%prelerp a.fg a.bg a.bg)))
    (when (= gamma 0)
      (setf gamma 1))
    (let ((value (%lerp (octet-mult bg a.bg) fg a.fg)))
      (floor (* 255 value) gamma))))

(declaim (inline octet-blend-function)
         (ftype (function (octet octet octet octet octet octet octet octet)
                          (values octet octet octet octet))
                octet-blend-function))
(defun octet-blend-function (r.fg g.fg b.fg a.fg r.bg g.bg b.bg a.bg)
  (values (%byte-blend-value r.fg r.bg a.fg a.bg)
          (%byte-blend-value g.fg g.bg a.fg a.bg)
          (%byte-blend-value b.fg b.bg a.fg a.bg)
          (%prelerp a.fg a.bg a.bg)))

(defun octet-blend-function* (r.fg g.fg b.fg a.fg r.bg g.bg b.bg a.bg)
  (logior (ash (%prelerp a.fg a.bg a.bg)               24)
          (ash (%byte-blend-value r.fg r.bg a.fg a.bg) 16)
          (ash (%byte-blend-value g.fg g.bg a.fg a.bg)  8)
          (ash (%byte-blend-value b.fg b.bg a.fg a.bg)  0)))

(declaim (inline octet-rgba-blend-function)
         (ftype (function (octet octet octet octet octet octet octet octet)
                          (values octet octet octet octet))
                octet-rgba-blend-function))
(defun octet-rgba-blend-function (r.fg g.fg b.fg a.fg r.bg g.bg b.bg a.bg)
  (octet-blend-function r.fg g.fg b.fg a.fg r.bg g.bg b.bg a.bg))

(declaim (inline octet-rgb-blend-function)
         (ftype (function (octet octet octet octet octet octet octet)
                          (values octet octet octet))
                octet-rgb-blend-function))
(defun octet-rgb-blend-function (r.fg g.fg b.fg a.fg r.bg g.bg b.bg)
  (values (%byte-blend-value r.fg r.bg a.fg 255)
          (%byte-blend-value g.fg g.bg a.fg 255)
          (%byte-blend-value b.fg b.bg a.fg 255)))

(declaim (inline octet-gray-blend-function)
         (ftype (function (octet octet octet) octet)
                octet-gray-blend-function))
(defun octet-gray-blend-function (g.fg a.fg g.bg)
  (%byte-blend-value g.fg g.bg a.fg 255))

(declaim (inline octet-alpha-blend-function)
         (ftype (function (octet octet) octet)
                octet-alpha-blend-function))
(defun octet-alpha-blend-function (a.fg a.bg)
  (%prelerp a.fg a.bg a.bg))

;;; conversion

(defgeneric color->octets (color)
  (:method ((color standard-color))
    (multiple-value-bind (r g b) (color-rgb color)
      (values (color-value->octet r)
              (color-value->octet g)
              (color-value->octet b)))))

;;; rgba->
(declaim (inline rgba->rgb)
         (ftype (function (octet octet octet octet)
                          (values octet octet octet))
                rgba->rgb))
(defun rgba->rgb (red green blue alpha)
  (declare (ignore alpha))
  (values red green blue))

(declaim (inline rgba->gray)
         (ftype (function (octet octet octet octet) octet) rgba->gray))
(defun rgba->gray (red green blue alpha)
  (declare (ignore alpha))
  (values (round (+ red green blue) 3)))

(declaim (inline rgba->gray-alpha)
         (ftype (function (octet octet octet octet) (values octet octet))
                rgba->gray-alpha))
(defun rgba->gray-alpha (red green blue alpha)
  (values (round (+ red green blue) 3) alpha))

(declaim (inline rgba->alpha)
         (ftype (function (octet octet octet octet) octet) rgba->alpha))
(defun rgba->alpha (red green blue alpha)
  (declare (ignore red green blue))
  alpha)

;;; rgb->
(declaim (inline rgb->rgba)
         (ftype (function (octet octet octet)
                          (values octet octet octet octet))
                rgb->rgba))
(defun rgb->rgba (red green blue)
  (values red green blue 255))

(declaim (inline rgb->gray)
         (ftype (function (octet octet octet)
                          octet)
                rgb->gray))
(defun rgb->gray (red green blue)
  (values (round (+ red green blue) 3)))

(declaim (inline rgb->alpha)
         (ftype (function (octet octet octet)
                          octet)
                rgb->alpha))
(defun rgb->alpha (red green blue)
   (rgb->gray red blue green))

;;; gray->
(declaim (inline gray->rgba)
         (ftype (function (octet) (values octet octet octet octet))
                gray->rgba))
(defun gray->rgba (gray)
  (values gray gray gray 255))

(declaim (inline gray->rgb)
         (ftype (function (octet) (values octet octet octet)) gray->rgb))
(defun gray->rgb (gray)
  (values gray gray gray))

(declaim (inline gray->alpha)
         (ftype (function (octet) octet) gray->alpha))
(defun gray->alpha (gray)
  gray)

(declaim (inline %rgba->vals %vals->rgba))

(defun %vals->rgba (r g b &optional (a #xff))
  (declare (type octet r g b a)
           (optimize (speed 3) (safety 0)))
  (logior (ash a 24) (ash r 16) (ash g 8) (ash b 0)))

(defun %rgba->vals (rgba)
  (declare (type argb-pixel rgba)
           (optimize (speed 3) (safety 0)))
  (values (ldb (byte 8 16) rgba)
          (ldb (byte 8 08) rgba)
          (ldb (byte 8 00) rgba)
          (ldb (byte 8 24) rgba)))
