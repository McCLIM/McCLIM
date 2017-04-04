;;;
;;; Copyright (c) 2004, Peter Mechlenborg (metch@daimi.au.dk)
;;;

(defsystem #:clim-debugger
  :description "CLIM debugger application."
  :long-description "CLIM debugger application

This is a Common Lisp debugger implemented in McCLIM. It uses the
portable debugger interface developed for the Slime project, and the
graphical layout is also heavily inspired by Slime. Because of Slime I
hope that this works on other implementations than SBCL."
  :license "LGPL-2.1+"
  :depends-on (#:mcclim #:clouseau #:swank #:slim)
  :components ((:file "clim-debugger")))
