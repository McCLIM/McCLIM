
(defsystem #:mcclim-standard
  :depends-on (#:clim)
  :components
  ((:file "package")
   (:file "port" :depends-on ("package"))
   (:file "graft" :depends-on ("package"))
   (:file "event-manager" :depends-on ("port" "package"))
   (:file "mirrored-sheet" :depends-on ("package"))))
