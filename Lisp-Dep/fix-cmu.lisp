
;;; In CMUCL the Common Lisp versions of class-of and find-class
;;; return wrappers which the MOP can't grok, so use the PCL versions
;;; instead.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((reexport (symbols)
           (import symbols :clim-lisp-patch)
           (export symbols :clim-lisp-patch)))
    (reexport '(pcl:class-name pcl:class-of
                pcl:find-class pcl::standard-class)))
  (export '(clim-lisp-patch::defclass)
          :clim-lisp-patch))

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
