(in-package :cl-user)

(defpackage #:clim-mop
  (:use #:mop)
  (:import-from #:mop #:compute-applicable-methods #:method-qualifiers #:class-name))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for sym being the symbols of :clim-mop
     do (export sym :clim-mop)))

(gray::redefine-cl-functions)
