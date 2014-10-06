(cl:in-package #:clim-internals)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun fundoc (name string)
    (setf (documentation name 'function) (format nil string))
    (setf (documentation (fdefinition name) 'function)
	  (documentation name 'function))))

(fundoc 'activation-gesture-p
	"Lambda list: (OBJECT).~@
         Returns true if OBJECT is an activation gesture.~@
	 Returns false otherwise.")
