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

(defmacro clim-lisp-patch:defclass (&rest args)
  ;; CMU's compiler barks much less this way
  `(eval-when (compile load eval)
    (defclass ,@args)))