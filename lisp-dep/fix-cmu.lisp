(defpackage #:clim-lisp-patch
    (:use)
    (:export
     #:defconstant
     #:defclass))

(defmacro clim-lisp-patch:defconstant (symbol value &optional docu)
  `(defparameter ,symbol ,value ,@(and docu (list docu))))

(defmacro clim-lisp-patch:defclass (&rest args)
  ;; CMU's compiler barks much less this way
  `(eval-when (compile load eval)
    (defclass ,@args)))