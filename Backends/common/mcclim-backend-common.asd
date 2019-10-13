(cl:in-package #:asdf-user)

(defsystem #:mcclim-backend-common
  :depends-on (#:clim)
  :components
  ((:file "ports")
   (:file "fonts")
   (:file "medium" :depends-on ("fonts"))))
