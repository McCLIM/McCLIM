
(defsystem #:clim-opengl
  :depends-on (#:clim)
  :serial t
  :components ((:file "opengl-x-frame-manager")
               (:file "opengl-frame-manager")
               (:file "opengl-x-port-before")
               (:file "opengl-port")
               (:file "opengl-x-port-after")
               (:file "opengl-medium")
               (:file "opengl-x-graft")))
