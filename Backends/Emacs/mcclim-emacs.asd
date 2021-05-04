(in-package #:asdf-user)

(defsystem "mcclim-emacs"
  :depends-on ("clim-basic"
               "mcclim-backend-common")
  :serial t
  :components ((:file "emacs")))

