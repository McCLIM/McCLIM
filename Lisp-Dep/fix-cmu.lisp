(defpackage #:clim-mop
  (:use #:mop))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for sym being the external-symbols of :mop
	do (export sym :clim-mop)))

;;; In CMUCL the Common Lisp versions of class-of and find-class
;;; return wrappers which the MOP can't grok, so use the PCL versions
;;; instead.

(defpackage #:clim-lisp-patch
    (:use)
    (:import-from #:pcl
		  #:class-of
		  #:find-class
		  #:standard-class)
    (:export
     #:class-of
     #:defconstant
     #:defclass
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
     ;; CMU's compiler barks much less this way (so the original
     ;; comment says; we should try turning this off in 18d.
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (cl:defclass ,name ,@args))))
