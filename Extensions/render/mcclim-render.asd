
(defsystem #:mcclim-render
    :depends-on (#:clim-basic #:mcclim-fonts/truetype)
    :serial t
    :components
    ((:file "package")
     (:file "prim-arc")
     (:file "prim-text")
     (:file "prim-path")
     (:file "mirror")
     (:file "mirrored-sheet")
     ;(:file "sheet")
     (:file "pixmap")
     (:file "design")
     (:file "render")
     (:file "medium")
     (:file "fonts")
     (:file "port")
     (:file "rgb-mirror")
     (:file "rgb-xlib-mirror")))

(defsystem #:mcclim-render/opticl
    :depends-on (#:mcclim-render #:opticl)
    :serial t
    :components
    ((:file "opticl-mirror")))

