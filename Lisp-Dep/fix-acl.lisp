(defpackage #:clim-mop
  (:use #:aclmop))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for sym being the external-symbols of :aclmop
	do (export sym :clim-mop)))
