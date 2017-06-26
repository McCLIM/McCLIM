
(defsystem #:mcclim-render/image
    :depends-on (#:clim-basic #:mcclim-fonts/truetype #:opticl #:mcclim-image)
    :serial t
    :components
    ((:file "package")
     (:file "color")
     (:file "image")
     (:file "pixeled-design")
     (:file "image-ops")
     (:file "2d-image")
     (:file "opticl-image")
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
     (:file "render")
     ))

(defsystem #:mcclim-render/backend
    :depends-on (#:mcclim-render)
    :serial t
    :components
    ((:file "mirror")
     (:file "opticl-mirror")
     (:file "mirrored-sheet")
     (:file "pixmap")
     (:file "medium")
     (:file "fonts")
     (:file "port")
     ))

(defsystem #:mcclim-render/clx
    :depends-on (#:mcclim-render/backend)
    :serial t
    :components
    ((:file "clx-extension")))
