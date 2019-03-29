(in-package #:asdf-user)

(defsystem #:mcclim-clx
  :depends-on (#:clx
               #:cl-unicode
               #:mcclim-backend-common
               #:mcclim-fonts)
  :serial t
  :components
  ((:module "basic" :pathname "" :components
            ((:file "package")
             (:file "port" :depends-on ("package" "graft"))
             (:file "frame-manager" :depends-on ("port"))
             (:file "basic" :depends-on ("package"))
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
             (:file "medium-xrender" :depends-on ("medium"))))
   (:file "input")))

(defsystem #:mcclim-clx/truetype
  :depends-on (#:mcclim-clx
               #:mcclim-fonts/clx-truetype))

(defsystem #:mcclim-clx/freetype
  :depends-on (#:mcclim-clx
               #:mcclim-fonts/clx-freetype))
