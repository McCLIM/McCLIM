
(defsystem #:clim-postscript-font
  :depends-on (#:clim-basic)
  :serial t
  :components ((:module "font"
                        :components
                        ((:file "package")
                         (:file "encoding")
                         (:file "font")
                         (:file "afm")
                         (:file "standard-metrics")))))
