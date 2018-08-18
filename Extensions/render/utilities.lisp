(in-package :mcclim-render-internals)

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
          for ,src-j fixnum from ,y2 downto ,y1s
          for ,dest-j fixnum from ,y2 downto ,y1d
          do (loop
                for ,src-i fixnum from ,x2 downto ,x1s
                for ,dest-i fixnum from ,x2 downto ,x1d
                do (progn ,@body)))))

(defmacro let-rgba (((r g b a) elt) &body body)
  (alexandria:once-only (elt)
    `(let ((,r (ldb (byte 8 24) ,elt))
           (,g (ldb (byte 8 16) ,elt))
           (,b (ldb (byte 8 08) ,elt))
           (,a (ldb (byte 8 00) ,elt)))
       ,@body)))

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

(declaim (inline %rgba->vals %vals->rgba))

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
           (nreverse (array-dimensions src-array)) (cons sx sy) (cons (+ sx width) (+ sy height))
           (nreverse (array-dimensions dst-array)) (cons dx dy) (cons (+ dx width) (+ dy height))))
  T)

;; https://stackoverflow.com/questions/1944095/how-to-mix-two-argb-pixels
;;
;; rOut = (rA * aA / 255) + (rB * aB * (255 - aA) / (255*255))
;; gOut = (gA * aA / 255) + (gB * aB * (255 - aA) / (255*255))
;; bOut = (bA * aA / 255) + (bB * aB * (255 - aA) / (255*255))
;; aOut = aA + (aB * (255 - aA) / 255)
(defun %blend-octets (r.fg g.fg b.fg a.fg
                      r.bg g.bg b.bg a.bg)
  (declare (type (unsigned-byte 8) r.fg g.fg b.fg a.fg r.bg g.bg b.bg a.bg)
           (optimize (speed 3) (safety 0)))
  (dpb (+ (ash (* r.fg a.fg) -8)
          (ash (* r.bg a.bg (- #xff a.fg)) -16))
       (byte 8 24)
       (dpb (+ (ash (* g.fg a.fg) -8)
               (ash (* g.bg a.bg (- #xff a.fg)) -16))
            (byte 8 16)
            (dpb (+ (ash (* b.fg a.fg) -8)
                    (ash (* b.bg a.bg (- #xff a.fg)) -16))
                 (byte 8 8)
                 (dpb (+ (ash (* b.fg a.fg) -8)
                         (ash (* b.bg a.bg (- #xff a.fg)) -16))
                      (byte 8 0)
                      0)))))

(declaim (ftype (function (octet octet) octet) octet-mult))
(defun %octet-mult (a b)
  (let ((temp (+ (* a b) #x80)))
    (logand #xFF (ash (+ (ash temp -8) temp) -8))))

(declaim (inline %check-coords %blend-octets %octet-mult))
