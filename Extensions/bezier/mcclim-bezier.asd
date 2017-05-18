;;;
;;; Copyright (c) 2017, Cyrus Harmon (ch-lisp@bobobeach.com)
;;;
;;; This is the ASDF defsystem file for Robert Strandh's bezier curve work in mcclim.
;;;
;;; In May 2017, the relevant code was moved into the Extensions directory.
;;;
;;; See file 'LICENSE' for the copyright details
;;;

(defsystem #:mcclim-bezier
  :description "Support for various bezier curves in McCLIM."
  :depends-on (#:clim #:mcclim-null #:mcclim-render)
  :components ((:file "package")
               (:file "bezier")))
