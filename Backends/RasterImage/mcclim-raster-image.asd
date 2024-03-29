(in-package #:asdf-user)

(defsystem "mcclim-raster-image"
  :depends-on ("clim" ; for CLIMB:FILE-DESTINATION, CLIMB:REGISTER-OUTPUT-DESTINATION-TYPE
               "mcclim-render"
               "mcclim-backend-common")
  :serial t
  :components ((:file "package")
               (:file "sheets")
               (:file "port")
               (:file "output-to-image")
               (:file "output-destination"))
  :in-order-to ((test-op (test-op "mcclim-raster-image/test"))))

(defsystem "mcclim-raster-image/test"
  :depends-on ("mcclim-raster-image"
               "fiveam"
               "mcclim/test-util")
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "smoke")
                             (:file "output-to-image")
                             (:file "output-destination"))))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:mcclim-raster-image.test '#:run-tests)))
