
(defsystem #:clim-pdf
  :depends-on (#:clim-basic #:cl-pdf #:flexi-streams #:clim-postscript-font)
  :serial t
  :components
  ((:file "package")
   (:file "paper")
   (:file "class")
   (:file "ellipse")
   (:file "graphics")
   (:file "sheet")))
