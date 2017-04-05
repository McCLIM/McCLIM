(in-package :mcclim-render)

(declaim (optimize speed))

;;;
;;; color
;;;

(deftype octet ()
  '(unsigned-byte 8))

(declaim (inline color->octet)
	 (ftype (function (real) octet) color->octet))
(defun color->octet (color)
  (coerce (round (* 255 color)) 'octet))

(declaim (inline float-blend)
	 (ftype (function (single-float single-float single-float single-float single-float 
					single-float single-float single-float single-float)
			  (values single-float single-float single-float single-float))
		float-blend))
(defun float-blend (r.bg g.bg b.bg a.bg r.fg g.fg b.fg a.fg alpha)
  (let ((a (if (< alpha 0.001)
	       0.001
	       alpha)))
    (multiple-value-bind (red green blue alpha)
	(color-blend-function  r.fg g.fg b.fg (float (* a a.fg)) r.bg g.bg b.bg a.bg)
      (values (float red) (float green) (float blue) (float alpha)))))

(declaim (inline float-xor-pixel))
(defun float-xor-pixel (d1 d2)
  (float (/ (logxor (floor (* 255 d1)) (floor (* 255 d2))) 255)))

(declaim (inline float-xor-pixel))
(defun octet-xor-pixel (d1 d2)
  (logxor d1 d2))

;;;
;;; 
;;;

(declaim (inline imult)
	 (ftype (function (octet octet) octet) imult))
(defun imult (a b)
  (let ((temp (+ (* a b) #x80)))
    (logand #xFF (ash (+ (ash temp -8) temp) -8))))

(declaim (inline lerp)
	 (ftype (function (octet octet octet) octet) prelerp))
(defun lerp (p q a)
  (logand #xFF (+ p (imult a (- q p)))))

(declaim (inline prelerp)
	 (ftype (function (octet octet octet) octet) prelerp))
(defun prelerp (p q a)
  (logand #xFF (- (+ p q) (imult a p))))

(declaim (inline byte-blend-value)
	 (ftype (function (octet octet octet octet) octet) byte-blend-value))
(defun byte-blend-value (fg bg a.fg a.bg)
  (let ((gamma (prelerp a.fg a.bg a.bg)))
    (when (= gamma 0)
      (setf gamma 1))
    (let ((value (lerp (imult bg a.bg) fg a.fg)))
      (floor (* 255 value) gamma))))

(declaim (inline octet-blend)
	 (ftype (function (octet octet octet octet octet octet octet octet octet)
			  (values octet octet octet octet))
		octet-blend))
(defun octet-blend (r.bg g.bg b.bg a.bg r.fg g.fg b.fg a.fg alpha)
  (let ((a (imult alpha a.fg)))
    (values
     (byte-blend-value r.fg r.bg a a.bg)
     (byte-blend-value g.fg g.bg a a.bg)
     (byte-blend-value b.fg b.bg a a.bg)
     (prelerp a a.bg a.bg))))

;;;
;;;
;;;

(deftype clim-rgb-image-data () '(simple-array (unsigned-byte 32) (* *)))
