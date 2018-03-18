
(defsystem #:mcclim-clx
  :depends-on (#:mcclim-clx/basic
	       #:mcclim-clx/input
	       #:mcclim-clx/output
               #:mcclim-image/clx
	       #:mcclim-clx/text-selection)
  :components
  ((:file "port")
   (:file "frame-manager" :depends-on ("port"))))

(defsystem #:mcclim-clx/basic
  :depends-on (#:clx
               #:clim
               #:mcclim-backend-common)
  :components
  ((:file "package")
   (:file "basic" :depends-on ("package"))
   (:file "keysyms-common" :depends-on ("basic" "package"))
   (:file "keysyms" :depends-on ("keysyms-common"))
   (:file "keysymdef" :depends-on ("keysyms-common"))
   (:file "graft" :depends-on ("basic"))
   (:file "cursor" :depends-on ("basic"))
   (:file "mirror" :depends-on ("basic"))))

(defsystem #:mcclim-clx/input
  :depends-on (#:mcclim-clx/basic)
  :components
  ((:file "input" :depends-on ())))

(defsystem #:mcclim-clx/output
  :depends-on (#:mcclim-clx/basic)
  :components
  ((:file "fonts" :depends-on ())
   (:file "medium" :depends-on ("fonts"))))

(defsystem #:mcclim-clx/text-selection
  :depends-on (#:mcclim-clx/input)
  :components
  ((:file "text-selection" :depends-on ())))

(defsystem #:mcclim-clx/pretty
  :depends-on (#:mcclim-clx
               #:mcclim-fonts/clx-truetype))
