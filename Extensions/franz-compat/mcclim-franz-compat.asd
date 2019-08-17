
(defsystem #:mcclim-franz-compat
  :depends-on (#:clim)
  :description "Implementation of CLIM 2.2 --  Franz CLIM Manual"
  :components ((:file "package")
               (:file "franz-compat")))
