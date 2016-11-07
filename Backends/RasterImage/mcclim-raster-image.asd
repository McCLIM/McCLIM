
(defsystem #:mcclim-raster-image
    :depends-on (#:mcclim-render #:mcclim-single-mirrored-standard)
    :serial t
    :components
    ((:file "package")
     (:file "graft")
     (:file "medium")
     (:file "top-level-pane")     
     (:file "port")
     (:file "stream")
     (:file "output-to-png")
     (:file "rgb-image")
     ))

(defsystem #:mcclim-raster-image/opticl
    :depends-on (#:mcclim-raster-image #:mcclim-render/opticl)
    :serial t
    :components
    ((:file "opticl")))

