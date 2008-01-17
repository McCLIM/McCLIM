;;;; -*- Lisp -*-

#|
To autoload mcclim-freetype after mcclim, link this file to a
directory in your asdf:*central-registry* and add the following to
your lisp's init file:

 (defmethod asdf:perform :after ((o asdf:load-op) (s (eql (asdf:find-system :clim-clx))))
   (asdf:oos 'asdf:load-op :mcclim-freetype))
|#

(defpackage :mcclim-freetype-system (:use :cl :asdf))
(in-package :mcclim-freetype-system)

(defclass uncompiled-cl-source-file (source-file) ())

(defmethod perform ((o compile-op) (f uncompiled-cl-source-file))
  t)
(defmethod perform ((o load-op) (f uncompiled-cl-source-file))
  (mapcar #'load (input-files o f)))
(defmethod output-files ((operation compile-op) (c uncompiled-cl-source-file))
  nil)
(defmethod input-files ((operation load-op) (c uncompiled-cl-source-file))
  (list (component-pathname c)))
(defmethod operation-done-p ((operation compile-op) (c uncompiled-cl-source-file))
  t)
(defmethod source-file-type ((c uncompiled-cl-source-file) (s module))
  "lisp")

(defsystem :mcclim-freetype
  :depends-on (:clim-clx :mcclim #-sbcl :cffi)
  :serial t
  :components
  #+sbcl
  ((:file "freetype-package")
   (:uncompiled-cl-source-file "freetype-ffi")
   (:file "freetype-fonts")
   (:file "fontconfig"))
  #-sbcl
  ((:file "freetype-package-cffi")
   (:uncompiled-cl-source-file "freetype-cffi")
   (:file "freetype-fonts-cffi")))


(defmethod perform :after ((o load-op) (s (eql (asdf:find-system :mcclim-freetype))))
  "Detect fonts using fc-match"
  (funcall (find-symbol (symbol-name '#:autoconfigure-fonts) :mcclim-freetype)))

