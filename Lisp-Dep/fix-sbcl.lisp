(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(clim-lisp-patch::defconstant
            clim-lisp-patch::defclass)
          :clim-lisp-patch))

(defmacro clim-lisp-patch:defconstant (symbol value &optional docu)
  `(defvar ,symbol ,value ,@(and docu (list docu))))

(defvar clim-lisp-patch::*compile-time-clos-names* (make-hash-table))

(defun clim-lisp-patch::compile-time-clos-class-p (name)
  (gethash name clim-lisp-patch::*compile-time-clos-names* nil))

(defmacro clim-lisp-patch:defclass (name &rest args)
  `(progn
     (eval-when (:compile-toplevel)
       (setf (gethash ',name clim-lisp-patch::*compile-time-clos-names*) t))
     (cl:defclass ,name ,@args)))
