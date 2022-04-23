(in-package #:asdf-user)

(defsystem "mcclim-render"
  :description "Support for raster images McCLIM."
  :depends-on ("alexandria"
               "cl-vectors"
               "clim-basic"
               "mcclim-fonts/truetype"
               "mcclim-backend-common")
  :serial t
  :components ((:file "package")
               (:file "types")
               (:file "utilities")
               (:file "image")
               (:module "render"
                :serial t
                :components ((:file "prim-arc")
                             (:file "prim-text")))
               (:module "cl-vectors"
                :serial t
                :components ((:file "vectors")
                             (:file "vectors-image-ops")))
               (:module "backend"
                :serial t
                :components ((:file "mirror")
                             (:file "medium")
                             (:file "pixmap")
                             (:file "fonts")
                             (:file "port")))))
