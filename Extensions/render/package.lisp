(in-package :common-lisp-user)

(defpackage :mcclim-render
    (:use :clim :clim-lisp :clim-backend)
    (:import-from :climi
		  #:with-transformed-position
		  #:with-transformed-positions
		  #:destroy-mirror
		  #:realize-mirror
		  #:mirrored-pixmap
		  #:image-width
		  #:image-height
		  #:port-register-mirror
		  #:port-lookup-mirror
		  #:port-lookup-sheet
		  #:pixmap-mirror
		  #:named-color
		  #:%transparent-ink
		  #:standard-opacity
		  #:in-compositum
		  #:out-compositum
		  #:over-compositum
		  #:compositum-foreground
		  #:compositum-background
		  #:color-blend-function
		  #:compositum-ink
		  #:compositum-mask
		  #:opacity-value
		  #:indexed-pattern
		  #:pattern-designs
		  #:rectangular-tile
		  #:rectangular-tile-design
		  #:rectangular-tile-width
		  #:rectangular-tile-height
		  #:transformed-design
		  #:transformed-design-design
		  #:transformed-design-transformation
		  )
    (:import-from :clim-backend
		  #:port-set-mirror-region
		  #:port-set-mirror-transformation)
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
    (:export
     ))

