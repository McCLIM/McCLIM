
(defsystem #:mcclim-render
    :depends-on (#:clim-basic #:mcclim-fonts/truetype #:opticl)
    :serial t
    :components
    ((:file "package")
     (:file "design")
     (:file "image")
     (:file "image-ops")
     (:file "prim-arc")
     (:file "prim-text")
     (:file "prim-path")
     (:file "aa")
     (:file "mirrored-sheet")     
     (:file "mirror")
     (:file "pixmap")
     (:file "render")
     (:file "medium")
     (:file "fonts")
     (:file "port")
     ))


