;;; -*- Mode: Lisp; Package: User -*-

(in-package :common-lisp-user)

(defpackage :clim-mop
  (:use :clos)
  (:import-from :common-lisp
		#:class-name)
  (:export #:class-name))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for sym being the external-symbols of :clos
	do (export sym :clim-mop)))
