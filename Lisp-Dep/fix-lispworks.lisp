;;; -*- Mode: Lisp; Package: User -*-

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *packages-for-warn-on-redefinition*
        (remove-if 
         (lambda (p) (member p '("COMMON-LISP"
                                 "CLIM"
                                 "CLIM-INTERNALS"
                                 "CLIM-SYS"
                                 "CLIM-UTILS"
                                 "CLIM-LISP"
                                 "POSTSCRIPT-CLIM") :test #'equal))
         *packages-for-warn-on-redefinition*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(clim-lisp-patch::defclass)
          :clim-lisp-patch))

(defvar clim-lisp-patch::*compile-time-clos-names* (make-hash-table))

(defun clim-lisp-patch::compile-time-clos-class-p (name)
  (gethash name clim-lisp-patch::*compile-time-clos-names* nil))

(defmacro clim-lisp-patch:defclass (name &rest args)
  `(progn
     (eval-when (:compile-toplevel)
       (setf (gethash ',name clim-lisp-patch::*compile-time-clos-names*) t))
     (cl:defclass ,name ,@args)))
