;; -*- Mode: Lisp -*-

#+SBCL
(require :sb-posix)

(defpackage "CLIM-LISTENER"
  (:use "CLIM" "CLIM-LISP")
  (:export #:run-listener #:run-listener-process #:dev-commands))


(in-package :clim-listener)

(defparameter *icon-path* (merge-pathnames #P"icons/" #.*load-truename*))

(load (compile-file (merge-pathnames #P"Experimental/xpm.lisp" cl-user::*clim-directory*)))

(asdf::defsystem clim-listener
    ; I'm told some people don't use ASDF for all their defsystem needs. :)
    ; Therefore this dependency breaks because ASDF didn't load CLIM.
    ; :depends-on (clim)
    :serial t
    :components
    (;(:file #.(concatenate 'string #.cl-user::*clim-directory* "Experimental/xpm")) ;; OOF.
     (:file "hotfixes")
     (:file "util")
     (:file "icons")
     (:file "file-types")
     (:file "dev-commands")
     (:file "listener")
     ;; cmu-hacks.lisp contains some changes to make the CMUCL debugger mostly
     ;; work over a CLIM stream. If you want to try this, you'll still have to
     ;; set *debug-io* to *standard-output* inside the listener.
     #+CMU (:file "cmu-hacks")))
