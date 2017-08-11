;;; -*- Mode: Lisp; Package: User -*-

(in-package :common-lisp-user)

;;; Needed to keep ACL from issuing warnings about toplevel (shadow ...) forms
(setq comp:*cltl1-compile-file-toplevel-compatibility-p* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :loop))

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


