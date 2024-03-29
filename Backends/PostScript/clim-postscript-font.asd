(in-package #:asdf-user)

(defsystem "clim-postscript-font"
  :depends-on ("clim"
               "mcclim-backend-common") ; for font abstractions
  :serial t
  :components ((:module "font"
                :components ((:file "package")
                             (:file "encoding")
                             (:file "font")
                             (:file "afm")
                             (:file "standard-metrics")))))
