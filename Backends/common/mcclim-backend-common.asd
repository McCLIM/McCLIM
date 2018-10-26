(cl:in-package #:asdf-user)

(defsystem #:mcclim-backend-common
  :depends-on (#:clim)
  :components
  ((:file "ports")
   (:file "grafts")
   (:file "fonts")
   (:file "events" :depends-on ("ports"))))
