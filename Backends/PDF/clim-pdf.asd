
(asdf:defsystem #:clim-pdf
  :depends-on (#:clim-basic #:cl-pdf #:flexi-streams #:clim-postscript-font #:mcclim-ellipse)
  :serial t
  :components
  ((:file "package")
   (:file "paper")
   (:file "class")
   (:file "graphics")
   (:file "sheet")))
