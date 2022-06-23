(in-package #:asdf-user)

(defsystem "mcclim-wayland"
  :depends-on ("alexandria"
               "com.andrewsoutar.cl-wayland-client"
               "com.andrewsoutar.cl-wayland-client.protocol/stable/xdg-shell/xdg-shell"
               "com.andrewsoutar.cl-wayland-client/egl-core"
               "cl-egl"
               "mcclim-backend-common"
               "cl-opengl")
  :serial t
  :components
  ((:module "core" :pathname "" :components
            ((:file "package")
             (:file "port" :depends-on ("package"))
             (:file "mirror" :depends-on ("port"))))
   (:file "input")))
