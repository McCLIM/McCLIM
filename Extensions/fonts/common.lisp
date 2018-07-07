(defpackage :mcclim-font
  (:use :cl)
  (:export #:find-replacement-text-styles
           #:find-replacement-fonts-from-port))

(in-package :mcclim-font)

(defgeneric find-replacement-fonts-from-port (port text-style string)
  (:method (port text-style string)
    (cons string '(nil nil))))

(defun find-replacement-text-styles (stream string &key text-style)
  (clim:with-sheet-medium (medium stream)
    (find-replacement-fonts-from-port (clim:port medium) (or text-style (clim:medium-text-style stream)) string)))
