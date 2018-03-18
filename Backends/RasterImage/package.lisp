(in-package :common-lisp-user)

(defpackage :mcclim-raster-image
    (:use :clim :clim-lisp :clim-backend :mcclim-render)
    (:import-from :climi
		  #:port-grafts
		  #:updating-output-stream-mixin
		  #:do-sequence
		  #:with-transformed-position
		  #:with-transformed-positions
		  #:port-register-mirror
		  #:port-unregister-mirror
		  #:port-lookup-sheet
		  #:destroy-mirror
		  #:realize-mirror
		  #:unmanaged-top-level-sheet-pane
		  #:vbox-pane
		  )
    (:import-from :mcclim-render-internals
		  #:render-medium-mixin
		  #:render-port-mixin
		  #:image-mirror-image
		  #:image-sheet-mixin
		  #:image-mirror-mixin
		  #:image-pixmap-mixin
                  #:%make-image
                  #:image-mirror-image
                  #:image-pattern
		  ;;#:save-image-to-file
		  ;;#:save-image-to-stream
		  )
    (:import-from :mcclim-image
                  #:rgb-pattern
		  )
    (:import-from :mcclim-truetype
		  #:truetype-font-size
		  #:truetype-font-face
		  #:glyph-pixarray
		  #:ensure-gethash
		  #:invoke-with-truetype-path-restart
		  #:*truetype-font-path*
		  #:*family-names*
		  #:zpb-ttf-font-loader
		  #:*zpb-font-lock* 
		  #:*fontconfig-faces*
		  #:*families/faces*
		  #:truetype-device-font-name 
		  #:fontconfig-font-name
		  #:make-truetype-device-font-name 
		  #:make-fontconfig-font-name
		  #:truetype-font-family
		  #:truetype-font
		  #:truetype-face
		  #:truetype-font-size
		  #:truetype-font-ascent
		  #:truetype-font-descent
		  )
    (:import-from :clim-standard
		  #:standard-mirrored-sheet-mixin)
    (:export
     #:with-output-to-raster-image-stream
     #:with-output-to-rgb-pattern
     #:with-output-to-image-pattern))


