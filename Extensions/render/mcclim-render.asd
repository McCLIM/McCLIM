
(defsystem #:mcclim-render
    :depends-on (#:clim-basic #:mcclim-fonts/truetype #:opticl)
    :serial t
    :components
    ((:file "package")
     (:file "design")
     (:file "image")
     (:file "prim-arc")
     (:file "prim-text")
     (:file "prim-path")
     
     (:file "rgb-image")
     (:file "rgb-image-io")
     (:file "aa")
     (:file "rgb-image-render")
     (:file "mirrored-sheet")     
     (:file "mirror")
     (:file "pixmap")
     (:file "render")
     (:file "medium")
     (:file "fonts")
     (:file "port")
     ))


