(in-package :cl-user)

(eval-when (:compile-toplevel :execute)
  (when (find-package "SB-MOP")
    (pushnew :sb-mop *features*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:clim-mop)
    (make-package '#:clim-mop :use '(#+sb-mop #:sb-mop
                                     #-sb-mop #:sb-pcl))
    (shadowing-import 'sb-pcl::eql-specializer-object '#:clim-mop)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for sym being the symbols of :clim-mop
     do (export sym :clim-mop))
  #-sb-mop
  (loop for other-symbol in '("EQL-SPECIALIZER" "FUNCALLABLE-STANDARD-CLASS")
     unless (find-symbol other-symbol :clim-mop)
     do (let ((sym (intern other-symbol :sb-pcl)))
          (import sym :clim-mop)
          (export sym :clim-mop))))


;;; In SBCL the Common Lisp versions of CLASS-OF and FIND-CLASS used
;;; to return wrappers which the MOP couldn't grok.  This has been fixed
;;; for some time, certainly in sbcl 0.9.4.
#+nil
(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((reexport (symbols)
           (import symbols :clim-lisp-patch)
           (export symbols :clim-lisp-patch)))
    (reexport '(sb-pcl:class-name sb-pcl:class-of
                sb-pcl:find-class sb-pcl::standard-class))))

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
