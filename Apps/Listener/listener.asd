;;; -*- Lisp -*- mode

(defpackage :clim-listener
  (:use :clim :clim-lisp)
  (:export :run-listener :run-listener-process))

(in-package :clim-listener)

(defparameter *icon-path* (merge-pathnames #P"icons/" #.*load-truename*))

(load (compile-file (merge-pathnames #P"Experimental/xpm.lisp" cl-user::*clim-directory*)))

(asdf::defsystem clim-listener
    :depends-on (clim)
    :depends-on (clim)
    :serial t
    :components
    ((:file "hotfixes")
     (:file "util")
     (:file "icons")
     (:file "file-types")
     (:file "dev-commands")
     (:file "listener")
     #+CMU (:file "cmu-hacks")))
