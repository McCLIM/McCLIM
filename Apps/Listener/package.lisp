;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
(in-package :cl-user)

(defpackage "CLIM-LISTENER"
  (:use "CLIM" "CLIM-LISP")
  (:export #:run-listener #:dev-commands))

(in-package :clim-listener)

(eval-when (:load-toplevel)
;  (format t "~&~%!@#%^!@#!@ ... ~A~%~%" *load-truename*)
  (defparameter *icon-path* (merge-pathnames #P"icons/" *load-truename*)))
