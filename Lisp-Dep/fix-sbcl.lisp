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
     #:defconstant
     #:find-class
     #:standard-class))

(defmacro clim-lisp-patch:defconstant (symbol value &optional docu)
  `(defparameter ,symbol ,value ,@(and docu (list docu))))

