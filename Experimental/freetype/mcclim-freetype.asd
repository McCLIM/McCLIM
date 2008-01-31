;;;; -*- Lisp -*-

#|

Mcclim-freetype adds truetype font support to the CLX backend
using libfreetype. 

To autoload mcclim-truetype after mcclim, link this file to a
directory in your asdf:*central-registry* and add the following to
your lisp's init file:

 (defmethod asdf:perform :after ((o asdf:load-op) 
                                 (s (eql (asdf:find-system :clim-clx))))
   (asdf:oos 'asdf:load-op :mcclim-freetype))

By default, the native 'Alien' FFI is used on CMUCL, SBCL, and SBCL, while
CFFI is used on other lisps. To force the use of CFFI, load the 
:mcclim-freetype-cffi system instead. This shouldn't be necessary except
for testing the CFFI code.

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
  :depends-on (:clim-clx :mcclim #-(or sbcl cmucl scl) :cffi)
  :serial t
  :components
  #+(or cmucl sbcl scl)
  ((:file "truetype-package")
   (:file "xrender-fonts")
   (:uncompiled-cl-source-file "freetype-ffi")
   (:file "freetype-fonts-alien")
   (:file "fontconfig"))
  #-(or cmucl sbcl scl)
  ((:file "truetype-package")
   (:file "xrender-fonts")
   (:file "freetype-package-cffi")
   (:uncompiled-cl-source-file "freetype-cffi")
   (:file "fontconfig")
   (:file "freetype-fonts-cffi")))

(defsystem :mcclim-freetype-cffi
  :depends-on (:clim-clx :mcclim :cffi)
  :serial t
  :components
  ((:file "truetype-package")
   (:file "xrender-fonts")
   (:file "freetype-package-cffi")
   (:uncompiled-cl-source-file "freetype-cffi")
   (:file "fontconfig")
   (:file "freetype-fonts-cffi")))


(defmethod perform :after ((o load-op) (s (eql (asdf:find-system :mcclim-freetype))))
  "Detect fonts using fc-match"
  (funcall (find-symbol (symbol-name '#:autoconfigure-fonts) :mcclim-truetype)))


(defmethod perform :after ((o load-op) (s (eql (asdf:find-system :mcclim-freetype-cffi))))
  "Detect fonts using fc-match"
  (funcall (find-symbol (symbol-name '#:autoconfigure-fonts) :mcclim-truetype)))
