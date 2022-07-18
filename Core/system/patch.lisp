(in-package :cl-user)

(defpackage clim-lisp-patch
  (:use)
  (:export #:defconstant
           #:defclass
           #:describe
           #:describe-object
           #:interactive-stream-p))

;;; Not a DEFCONSTANT that we want, but a DEFCONSTANT that we need.
;;;
;;; FIXME what we really want is "our own" macro that defines also a load form
;;; and such.
(defmacro clim-lisp-patch:defconstant (symbol value &optional docu (test '(constantly t)))
  `(alexandria:define-constant ,symbol ,value :test ,test
     ,@(and docu (list :documentation docu))))

(defvar clim-lisp-patch::*compile-time-clos-names* (make-hash-table))

(defun clim-lisp-patch::compile-time-clos-class-p (name)
  (gethash name clim-lisp-patch::*compile-time-clos-names* nil))

(defmacro clim-lisp-patch:defclass (name &rest args)
  `(progn
     (eval-when (:compile-toplevel)
       (setf (gethash ',name clim-lisp-patch::*compile-time-clos-names*) t))
     (cl:defclass ,name ,@args)))
