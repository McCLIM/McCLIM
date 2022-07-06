(in-package #:asdf-user)

(defsystem "mcclim-render"
  :description "Support for raster images McCLIM."
  :depends-on ("alexandria"
               "cl-vectors"
               "clim"
               "mcclim-fonts/truetype"
               "mcclim-backend-common")
  :serial t
  :components ((:file "package")
               (:file "types")
               (:file "utilities")
               (:file "image")
               (:file "vectors")
               (:module "render"
                :serial t
                :components ((:file "prim-arc")
                             (:file "prim-text")))
               (:module "backend"
                :serial t
                :components ((:file "mirror")
                             (:file "medium")
                             (:file "pixmap")
                             (:file "fonts")
                             (:file "port")))))
