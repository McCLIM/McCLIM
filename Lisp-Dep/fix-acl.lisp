;;; -*- Mode: Lisp; Package: User -*-

(in-package :common-lisp-user)

;;; Needed to keep ACL from issuing warnings about toplevel (shadow ...) forms
(setq comp:*cltl1-compile-file-toplevel-compatibility-p* nil)

(defpackage :clim-mop
  (:use :clos)
  (:import-from :common-lisp
		#:class-name)
  (:export #:class-name))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-external-symbols (sym :clos)
	(export sym :clim-mop)))
