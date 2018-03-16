
(defsystem #:mcclim-backend-common
  :depends-on (#:clim)
  :components
  ((:file "package")
   (:file "ports" :depends-on ("package"))
   (:file "grafts" :depends-on ("package"))
   (:file "events" :depends-on ("ports" "package"))
   (:file "mirrored-sheets" :depends-on ("package"))))
