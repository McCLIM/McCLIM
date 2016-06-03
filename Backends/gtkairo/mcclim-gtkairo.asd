
(defsystem #:mcclim-gtkairo
  :description "McCLIM gtk cairo backend"
  :depends-on (#:clim #:cffi)
  :serial t
  :components ((:file "clim-fix")
               (:file "package")
               (:file "gtk-ffi")
               (:file "cairo-ffi")
               (:file "ffi")
               (:file "graft")
               (:file "port")
               (:file "event")
               (:file "keys")
               (:file "medium")
               (:file "pango")
               (:file "cairo")
               (:file "gdk")
               (:file "pixmap")
               (:file "frame-manager")
               (:file "gadgets")))
