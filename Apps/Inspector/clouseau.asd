;;;
;;; Copyright (c) 2005, Robert Strandh (strandh@labri.fr)
;;; Copyright (c) 2005, Vincent Arkesteijn
;;; Copyright (c) 2005, Peter Scott (sketerpot@gmail.com)
;;;

(defsystem #:clouseau
  :description "CLIM inspector application."
  :long-description "CLIM inspector application

Common Lisp Inspector – McCLIM application. This application allows
inspecting arbitrary Common Lisp objects. Provides also a disassembler
wrapper."
  :license "LGPL-2.1+"
  :depends-on (#:mcclim)
  :serial t
  :components ((:file "package")
               (:file "disassembly")
               (:file "inspector")))
