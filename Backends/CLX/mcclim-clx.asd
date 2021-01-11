(in-package #:asdf-user)

(defsystem #:mcclim-clx
  :depends-on (#:alexandria
               #:cl-unicode
               #:zpb-ttf
               #:clx
               #:mcclim-fonts/truetype
               #:mcclim-backend-common)
  :serial t
  :components
  ((:module "basic" :pathname "" :components
            ((:file "package")
             (:file "clipboard")
             (:file "basic" :depends-on ("package"))
             (:file "port" :depends-on ("package" "graft" "basic" "mirror"))
             (:file "frame-manager" :depends-on ("port"))
             (:file "keysyms-common" :depends-on ("basic" "package"))
             (:file "keysyms" :depends-on ("keysyms-common"))
             (:file "keysymdef" :depends-on ("keysyms-common"))
             (:file "graft" :depends-on ("basic"))
             (:file "cursor" :depends-on ("basic"))
             (:file "mirror" :depends-on ("basic"))))
   (:module "output" :pathname "" :components
            ((:file "bidi" :depends-on ())
             (:file "fonts" :depends-on ("bidi"))
             (:file "medium" :depends-on ("fonts"))
             (:file "fonts-xrender")
             (:file "medium-xrender" :depends-on ("medium" "fonts-xrender"))
             (:file "pixmap" :depends-on ("medium"))))
   (:file "input")))

(defsystem #:mcclim-clx/freetype
  :depends-on (#:mcclim-clx
               #:mcclim-fonts/clx-freetype))
