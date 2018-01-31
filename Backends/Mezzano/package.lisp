;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

(in-package :common-lisp-user)

(defpackage :clim-mezzano
    (:use :clim :clim-lisp :clim-backend :mcclim-render-extensions)
  (:import-from :climi
                #:+alt-key+
                ;;
                #:port-text-style-mappings
                #:port-lookup-mirror
                #:port-register-mirror
                #:port-event-process
                #:port-grafts
		#:%%sheet-native-transformation
		#:%%set-sheet-native-transformation
                ;;
                #:clamp
                #:get-environment-variable
                #:pixmap-sheet
                #:port-lookup-sheet
                #:port-unregister-mirror
		#:port-pointer-sheet
                #:map-repeated-sequence
                #:pixmap-mirror
		#:do-sequence
                #:with-double-buffering
                #:with-transformed-position
                #:with-transformed-positions
                #:with-medium-options
                ;;
                #:border-pane
                #:pixmap
                #:top-level-sheet-pane
                #:unmanaged-top-level-sheet-pane
                #:menu-frame
                ;;
                #:frame-managers        ;used as slot
                #:top-level-sheet       ;used as slot
                #:medium-device-region
                #:draw-image
                #:height                ;this seems bogus
                #:width                 ;dito
                #:coordinate=
                #:get-transformation
                ;;
                #:invoke-with-special-choices
                #:medium-miter-limit
                ;; classes:
                #:mirrored-pixmap
                #:window-destroy-event
                #:pointer-ungrab-event
		#:pointer-motion-hint-event
                #:device-font-text-style
                ;;
		#:make-medium
                )
  (:import-from :mcclim-render-internals
                  #:%create-mirror-image
		  #:render-medium-mixin
		  #:render-port-mixin
		  #:image-mirror-image
		  #:image-sheet-mixin
		  #:image-pixmap-mixin
                  #:image-pixels
                  #:image-pixmap-mixin
                  #:image-mirror-mixin
                  #:opticl-rgb-image-pixels
		  )
  (:import-from :clim-standard
		#:standard-handled-event-port-mixin
		#:standard-single-mirrored-sheet-mixin
		#:%sheet-mirror-region
                #:%sheet-mirror-transformation
		#:*configuration-event-p*
		#:standard-port
		#:standard-handled-event-port-mixin
		))

;;; TODO removed from CLX-fb version of package - probably need to
;;; define these symbols somewhere for Mezzano

   ;; (:import-from :clim-clx
   ;;      	 #:CLX-PORT-DISPLAY
   ;;      	 #:clx-medium
   ;;      	 #:initialize-clx
   ;;      	#:design-gcontext
   ;;      	#:medium-draw-rectangle-using-ink*
   ;;      	#:REGION->CLIPPING-VALUES
   ;;      	#:ROUND-COORDINATE
   ;;      	#:with-clx-graphics
   ;;      	#:clx-port-screen
   ;;      	#:clx-graft
   ;;      	#:clx-port-window
   ;;      	#:sheet-xmirror
   ;;      	#:sheet-direct-xmirror
   ;;      	)
