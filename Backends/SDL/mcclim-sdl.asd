
(defsystem #:mcclim-sdl
  :depends-on ("clim" "sdl2" "cl-cairo2")
  :components
  ((:file "package")
   (:file "basic" :depends-on ("package"))
   (:file "port" :depends-on ("basic"))
   (:file "medium" :depends-on ("port" "package"))
   (:file "graft" :depends-on ("port" "package"))
   (:file "frame-manager" :depends-on ("medium" "port" "package"))))
