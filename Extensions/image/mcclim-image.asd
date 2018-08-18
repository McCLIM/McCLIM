
(defsystem #:mcclim-image
  :description "Support for raster images McCLIM."
  :depends-on (#:clim-basic #:opticl #:mcclim-bitmaps)
  :serial t
  :components ((:file "package")
               (:file "image")))
