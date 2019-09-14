(defsystem "mcclim-raster-image"
  :depends-on ("mcclim-render"
               "mcclim-backend-common")
  :serial t
  :components ((:file "package")
               (:file "graft")
               (:file "medium")
               (:file "top-level-pane")
               (:file "port")
               (:file "stream")
               (:file "output-to-png")
               (:file "rgb-port"))
  :in-order-to ((test-op (test-op "mcclim-raster-image/test"))))

(defsystem "mcclim-raster-image/test"
  :depends-on ("mcclim-raster-image"
               "mcclim-image" ; for raster image format support
               "fiveam"
               "mcclim/test-util")
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "smoke"))))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:mcclim-raster-image.test '#:run-tests)))
