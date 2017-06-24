
(defsystem #:mcclim-render/image
    :depends-on (#:clim-basic #:mcclim-fonts/truetype #:opticl #:mcclim-image)
    :serial t
    :components
    ((:file "package")
     (:file "color")
     (:file "image")
     (:file "opticl-image")
     (:file "pixeled-design")
     (:file "recording")))

(defsystem #:mcclim-render
    :depends-on (#:mcclim-render/image)
    :serial t
    :components
    ((:file "aa")
     (:file "render-image-op")
     (:file "prim-arc")
     (:file "prim-text")
     (:file "prim-path")
    
     (:file "mirrored-sheet")     
     (:file "mirror")
     (:file "opticl-mirror")
   
     (:file "pixmap")
     (:file "render")
     (:file "medium")
     (:file "fonts")
     (:file "port")
     ))

(defsystem #:mcclim-render/clx
    :depends-on (#:mcclim-render)
    :serial t
    :components
    ((:file "clx-extension")))


