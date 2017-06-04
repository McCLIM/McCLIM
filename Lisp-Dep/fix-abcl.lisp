(in-package :cl-user)

(defpackage clim-mop
  (:use cl mop)
  (:import-from mop
                compute-applicable-methods method-qualifiers class-name))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop :for symbol :being :the :symbols :of :clim-mop
     do (export symbol :clim-mop))
    (loop :for symbol :being :the :external-symbols :of #:mop
     do (export symbol #:mop)))
  (loop :for symbol :being :the :external-symbols :of #:mop
     do (export symbol #:mop)))


