;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

(in-package :common-lisp-user)

(defpackage :clim-xcommon
  (:use :clim :clim-lisp)
  (:export #:keysym-port-mixin
	   #:lookup-keysym
	   #:modifier-mapping
	   #:reverse-lookup-keysym
	   #:x-event-state-modifiers
	   #:x-keysym-to-clim-modifiers))

(defpackage :clim-clx
    (:use :clim :clim-lisp :clim-backend)
  (:import-from :climi
                #:+alt-key+
                ;;
                #:port-text-style-mappings
                #:port-lookup-mirror
                #:port-register-mirror
                #:port-event-process
                #:port-grafts
                #:update-mirror-geometry
                #:%sheet-mirror-region
                #:%sheet-mirror-transformation
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
                ) )
