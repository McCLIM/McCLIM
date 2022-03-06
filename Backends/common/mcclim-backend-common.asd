(in-package #:asdf-user)

(defsystem "mcclim-backend-common"
  :depends-on ("clim")
  :components ((:file "ports")
               (:file "medium")))
