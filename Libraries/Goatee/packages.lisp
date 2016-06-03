(in-package #:common-lisp-user)

(defpackage #:goatee
  (:use :clim :clim-lisp :clim-sys)
  (:import-from :clim-internals #:letf)
  (:shadow #:point)
  (:export
   #:execute-gesture-command
   #:goatee-input-editing-mixin
   #:simple-screen-area))
