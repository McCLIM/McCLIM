;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

(in-package :common-lisp-user)

(defpackage :beagle
  (:use :clim :clim-lisp :clim-backend)
  (:import-from :climi
                #:+alt-key+
                ;;
                #:port-text-style-mappings
                #:port-lookup-mirror
                #:port-register-mirror

                #:port-event-process
                #:port-grafts
                #:set-sheet-pointer-cursor
                ;;
                #:update-mirror-geometry
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
                #:get-next-event
                ;;
                #:frame-managers        ;used as slot
                #:top-level-sheet       ;used as slot
                ;; #:space-requirement     ;used as slot, very bogus
                ;; fbound
                #:medium-device-region
                #:draw-image
                #:text-style-character-width
                #:height                ;this seems bogus
                #:width                 ;dito
                #:coordinate=
                #:get-transformation
                #:port-grab-pointer
                #:port-ungrab-pointer
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
		#:synthesize-pointer-motion-event
                ;;
		#:vrack-pane
		#:hrack-pane
                )
  (:import-from :ccl
				#:@class
				#:define-objc-method
				#:get-selector-for
				#:make-cstring
				#:%make-nsstring
				#:ns-make-point
				#:%null-ptr
				#:pref
				#:rlet
				#:send
				#:send-super
				#:slet
				#:with-cstrs
				#:with-nsstr)
  (:export #:beagle-standard-frame-manager
	   #:beagle-aqua-frame-manager))
