(cl:in-package #:mcclim-render-internals)

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
          for ,dest-j fixnum from ,y1d to ,y2
          do (loop
                for ,src-i fixnum from ,x1s
                for ,dest-i fixnum from ,x1d to ,x2
                do (progn ,@body)))
      `(loop
          for ,src-j fixnum from (+ ,y1s (- ,y2 ,y1d)) downto ,y1s
          for ,dest-j fixnum from ,y2 downto ,y1d
          do (loop
                for ,src-i fixnum from (+ ,x1s (- ,x2 ,x1d)) downto ,x1s
                for ,dest-i fixnum from ,x2 downto ,x1d
                do (progn ,@body)))))

(declaim (inline %check-coords))

;;; Returns T for valid arguments, NIL for malformed width/height and signals an
;;; error if coordinates go out of arrays bounds.
(defun %check-coords (src-array dst-array sx sy dx dy width height)
  (when (or (<= width 0) (<= height 0))
    (warn "mcclim-render operation called with width = ~s and height = ~s; doing nothing."
          width height)
    (return-from %check-coords nil))
  (unless (and (array-in-bounds-p src-array (1- (+ sy height)) (1- (+ sx width)))
               (array-in-bounds-p dst-array (1- (+ dy height)) (1- (+ dx width))))
    (error "mcclim-render operation: some coordinates are out of image bounds:~@
             src array ~s, P1 ~s, P2 ~s,~@
             dst array ~s, P1 ~s, P2 ~s."
           (reverse (array-dimensions src-array)) (cons sx sy) (cons (+ sx width) (+ sy height))
           (reverse (array-dimensions dst-array)) (cons dx dy) (cons (+ dx width) (+ dy height))))
  t)

;;; color functions

(deftype octet ()
  '(unsigned-byte 8))

(defmacro let-rgba (((r g b a) elt) &body body)
  (alexandria:once-only (elt)
    `(let ((,r (ldb (byte 8 24) ,elt))
           (,g (ldb (byte 8 16) ,elt))
           (,b (ldb (byte 8 08) ,elt))
           (,a (ldb (byte 8 00) ,elt)))
       ,@body)))

(declaim (inline color-value->octet)
         (ftype (function (real) octet) color-value->octet))
(defun color-value->octet (v)
  (round (* 255 v)))

(declaim (inline color-octet->value)
         (ftype (function (octet) real) color-octet->value))
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
  (dpb (%byte-blend-value r.fg r.bg a.fg a.bg) (byte 8 24)
       (dpb (%byte-blend-value g.fg g.bg a.fg a.bg) (byte 8 16)
            (dpb (%byte-blend-value b.fg b.bg a.fg a.bg) (byte 8 8)
                 (%prelerp a.fg a.bg a.bg)))))

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
    (multiple-value-bind (r g b) (climi::color-rgb color)
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
  (dpb r (byte 8 24) (dpb g (byte 8 16) (dpb b (byte 8 8) a))))

(defun %rgba->vals (rgba)
  (declare (type (unsigned-byte 32) rgba)
           (optimize (speed 3) (safety 0)))
  (values (ldb (byte 8 24) rgba)
          (ldb (byte 8 16) rgba)
          (ldb (byte 8 08) rgba)
          (ldb (byte 8 00) rgba)))
