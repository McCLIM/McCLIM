(defpackage #:clim-mop
  (:use #:sb-pcl))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for sym being the external-symbols of :sb-pcl
	do (export sym :clim-mop)))

;;; In SBCL the Common Lisp versions of CLASS-OF and FIND-CLASS return
;;; wrappers which the MOP can't grok, so use the PCL versions
;;; instead.

(defpackage #:clim-lisp-patch
    (:use)
    (:import-from #:sb-pcl
		  #:class-of
		  #:find-class
		  #:standard-class)
    (:export
     #:class-of
     #:defclass
     #:defconstant
     #:find-class
     #:standard-class))

(defmacro clim-lisp-patch:defconstant (symbol value &optional docu)
  `(defparameter ,symbol ,value ,@(and docu (list docu))))

(defvar clim-lisp-patch::*compile-time-clos-names* (make-hash-table))

(defun clim-lisp-patch::compile-time-clos-class-p (name)
  (gethash name clim-lisp-patch::*compile-time-clos-names* nil))

(defmacro clim-lisp-patch:defclass (name &rest args)
  `(progn
     (eval-when (:compile-toplevel)
       (setf (gethash ',name clim-lisp-patch::*compile-time-clos-names*) t))
     (cl:defclass ,name ,@args)))
