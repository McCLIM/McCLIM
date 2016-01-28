(in-package :cl-user)

(defpackage #:clim-mop
  (:use #:mop))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for sym being the symbols of :clim-mop
     do (export sym :clim-mop)))

(gray::redefine-cl-functions)
