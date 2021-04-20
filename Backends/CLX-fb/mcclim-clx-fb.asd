(in-package #:asdf-user)

(defsystem "mcclim-clx-fb"
  :depends-on ("mcclim-backend-common"
               "mcclim-clx"
               "mcclim-render")
  :components
  ((:file "package")
   (:file "port" :depends-on ("package" "medium"))
   (:file "frame-manager" :depends-on ("port" "package"))
   (:file "medium" :depends-on ("package"))
   (:file "mirror" :depends-on ("port" "package"))))
