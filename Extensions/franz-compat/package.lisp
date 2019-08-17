(in-package :cl-user)

(defpackage :clim-franz-compat
  (:nicknames :climf :clim-2.2)
  (:use :climi :clim :clim-lisp)
  (:export
   #:pointer-place-rubber-band-line*
   #:pointer-input-rectangle*
   #:pointer-input-rectangle))
