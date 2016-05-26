;;;
;;; Copyright (c) 2005-2006, Robert Strandh (strandh@labri.u-bordeaux.fr)
;;; Copyright (c) 2006, Troels Henriksen (athas@sigkill.dk)
;;;           

;;; ASDF system definition for ESA.

(defsystem #:esa
  :license "LGPL-2.1+"
  :depends-on (#:mcclim)
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "colors")
               (:file "esa")
               (:file "esa-buffer")
               (:file "esa-io")
               (:file "esa-command-parser")))
