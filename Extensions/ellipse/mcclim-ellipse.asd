
(asdf:defsystem #:mcclim-ellipse
  :serial t
  :depends-on (#:clim #:clim-basic)
  :components
  ((:file "package")
   (:file "ellipse")))
