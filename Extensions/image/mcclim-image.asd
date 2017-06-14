
(defsystem #:mcclim-image
  :description "Support for raster images McCLIM."
  :depends-on (#:clim-basic #:opticl #:mcclim-bitmaps)
  :serial t
  :components ((:file "package")
               (:file "image")
               (:file "xpm")
               (:file "bitmap")))


(defsystem #:mcclim-image/clx
  :depends-on (#:mcclim-image #:mcclim-clx/output)
  :serial t
  :components ((:file "clx-image")))
