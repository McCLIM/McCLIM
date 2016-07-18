;;;
;;; Copyright (c) 2016, Robert Strandh (robert.strandh@gmail.com)
;;;

(defsystem #:mcclim-clx-core
  :depends-on (#:clx #:mcclim)
  :license "LGPL-2.1+"
  :serial t
  :components
  ((:file "packages")
   (:file "port")
   (:file "medium")))
