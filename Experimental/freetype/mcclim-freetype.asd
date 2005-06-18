;;;; -*- Lisp -*-

(defpackage :mcclim-freetype-system (:use :cl :asdf))
(in-package :mcclim-freetype-system)

(defclass uncompiled-cl-source-file (cl-source-file) ())

(defmethod perform ((o compile-op) (f uncompiled-cl-source-file))
  t)

(defmethod output-files ((operation compile-op) (c uncompiled-cl-source-file))
  (list (component-pathname c)))

(defsystem :mcclim-freetype
  :depends-on (:clim :clx)
  :serial t
  :components
  ((:file "freetype-package")
   (:uncompiled-cl-source-file "freetype-ffi")
   (:file "freetype-fonts")))
