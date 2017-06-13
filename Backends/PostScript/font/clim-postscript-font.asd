
(defsystem #:clim-postscript-font
  :depends-on (#:clim-basic)
  :serial t
  :components
  ((:file "font")
   (:file "afm")
   (:file "standard-metrics")
   (:file "encoding")))
