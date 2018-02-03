
(defsystem #:mcclim-mezzano
    :depends-on (#:mcclim-single-mirrored-standard
		 #:mcclim-render
                 #:mcclim-render/backend)
    :serial t
    :components
    ((:file "package")
     (:file "events")
     (:file "graft")
     (:file "medium")
     (:file "port")
     (:file "mirror")
     (:file "mirrored-sheets")
     (:file "frame-manager")))
