
(defsystem #:mcclim-clx
  :depends-on (#:clim
	       #:mcclim-multi-mirrored-standard
               #+(or cmu ecl) (:require :clx)
               #+(or sbcl clozure ecl clisp allegro) #:clx)
  :components
  ((:file "package")
   (:file "image" :depends-on ("package"))
   (:file "keysyms-common" :depends-on ("package"))
   (:file "keysyms" :depends-on ("keysyms-common" "package"))
   (:file "keysymdef" :depends-on ("keysyms-common" "package"))
   (:file "port" :depends-on ("keysyms-common" "keysyms" "package"))
   (:file "medium" :depends-on ("port" "keysyms" "package"))
   (:file "graft" :depends-on ("port" "package"))
   (:file "mirrored-sheets" :depends-on ("port" "package"))
   (:file "frame-manager" :depends-on ("medium" "port" "package" "mirrored-sheets"))))

(defsystem #:mcclim-clx/pretty
  :depends-on (#:mcclim-clx
               #:mcclim-fonts/truetype
               #:mcclim-looks/pixie))
