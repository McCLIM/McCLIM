;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998,1999,2003 Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 2019 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; The coordinate type and associated functions.

(in-package #:clim-internals)

#+ (or)
(progn
  (deftype coordinate () 'double-float)

  (defun coordinate (n)
    "Coerces N to be a coordinate."
    (declare (type number n))
    (coerce n 'coordinate))

  (defun coordinate-epsilon ()
    ;; tweak if you like
    (* #.(expt 2 10) double-float-epsilon))

  (defun coordinate= (x y)
    (< (abs (- x y)) (coordinate-epsilon)))

  (defun coordinate<= (x y)
    (<= (- x y) (coordinate-epsilon)))

  (declaim (inline coordinate-between))
  (defun coordinate-between (c1 x c2)
    (or (and (coordinate<= c1 x) (coordinate<= x c2))
        (and (coordinate<= c2 x) (coordinate<= x c1))))

  (declaim (inline coordinate-between*))
  (defun coordinate-between* (low x high)
    (and (coordinate<= low x) (coordinate<= x high)))

  (defun coordinate/= (x y)
    (not (coordinate= x y))))

(deftype coordinate () 'real)

(declaim (inline coordinate))
(defun coordinate (n) n)

(declaim (inline coordinate-epsilon))
(defun coordinate-epsilon ()
  0)

(declaim (inline coordinate=))
(defun coordinate= (x y)
  (= x y))

(declaim (inline coordinate<=))
(defun coordinate<= (x y)
  (<= x y))

(declaim (inline coordinate-between))
(defun coordinate-between (c1 x c2)
  (or (<= c1 x c2)
      (>= c1 x c2)))

(declaim (inline coordinate-between*))
(defun coordinate-between* (low x high)
  (<= low x high))

(declaim (inline coordinate/=))
(defun coordinate/= (x y)
  (/= x y))

(deftype standard-rectangle-coordinate-vector ()
  '(simple-array coordinate (4)))
