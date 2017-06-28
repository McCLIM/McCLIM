(in-package :mcclim-render)

(declaim (optimize speed))

;;;
;;; color utility functions
;;;

(deftype octet ()
  '(unsigned-byte 8))

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

;;;
;;; blend functions
;;;

(declaim (inline %lerp)
	 (ftype (function (octet octet octet) octet) %lerp))
(defun %lerp (p q a)
  (logand #xFF (+ p (octet-mult a (- q p)))))

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
(defun octet-blend-function (r.bg g.bg b.bg a.bg r.fg g.fg b.fg a.fg)
  (values
   (%byte-blend-value r.fg r.bg a.fg a.bg)
   (%byte-blend-value g.fg g.bg a.fg a.bg)
   (%byte-blend-value b.fg b.bg a.fg a.bg)
   (%prelerp a.fg a.bg a.bg)))
