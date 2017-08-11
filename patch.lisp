(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :clim-lisp-patch)
    (make-package :clim-lisp-patch :use nil)))

(export '(clim-lisp-patch::defconstant
          clim-lisp-patch::defclass
          clim-lisp-patch::describe
          clim-lisp-patch::describe-object 
          clim-lisp-patch::interactive-stream-p) 
        :clim-lisp-patch)

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
