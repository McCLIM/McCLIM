(defsystem #:mcclim-gtk
  :depends-on ("clim" "log4cl" "cl-cffi-gtk")
  :components ((:file "package")
               (:file "basic" :depends-on ("package"))
               (:file "port" :depends-on ("basic"))
               (:file "event" :depends-on ("port"))
               (:file "medium" :depends-on ("port" "package"))
               (:file "graft" :depends-on ("port" "package"))
               (:file "frame-manager" :depends-on ("medium" "port" "package"))))
