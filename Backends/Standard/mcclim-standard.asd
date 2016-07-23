
(defsystem #:mcclim-standard
  :depends-on (#:clim)
  :components
  ((:file "package")
   (:file "port" :depends-on ("package"))
   (:file "mirrored-sheet" :depends-on ("package"))))
