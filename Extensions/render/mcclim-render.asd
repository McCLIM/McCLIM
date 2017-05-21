
(defsystem #:mcclim-render
    :depends-on (#:clim-basic #:mcclim-fonts/truetype #:opticl)
    :serial t
    :components
    ((:file "package")
     (:file "color")
     (:file "design")
     (:file "image")
     (:file "aa")
    
     (:file "mask-image")
     (:file "opticl-image")
     (:file "rgba-image")
     (:file "xlib-image")
     
     (:file "prim-arc")
     (:file "prim-text")
     (:file "prim-path")
    
     (:file "mirrored-sheet")     
     (:file "mirror")
     (:file "opticl-mirror")
     (:file "rgba-mirror")
   
     (:file "pixmap")
     (:file "render")
     (:file "medium")
     (:file "fonts")
     (:file "port")
     ))


