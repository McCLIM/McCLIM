
;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;;; START - Cribbed from framework/cocoa-support.lisp
(in-package "CCL")
(defun nslog (c)
  "Writes a string message to the OSX console log."
  (let* ((rep (format nil "~a" c)))
    (with-cstrs ((str rep))
      (with-nsstr (nsstr str (length rep))
	(#_NSLog #@"Logging: %@" :address nsstr)))))
;;; END

(in-package :common-lisp-user)

(defpackage :beagle
  (:use :clim :clim-lisp :clim-backend)
  (:import-from :climi
                #:+alt-key+
                ;;
                #:mirror-transformation
                #:port-set-sheet-region
                #:port-set-sheet-transformation
                #:port-text-style-mappings
                #:port-lookup-mirror
                #:port-register-mirror
                #:port-allocate-pixmap
                #:port-deallocate-pixmap
                #:port-mirror-width
                #:port-mirror-height
                #:port-event-process
                #:port-grafts
                #:port-enable-sheet
                #:port-disable-sheet
				#:port-motion-hints
                #:port-force-output
                #:%set-port-keyboard-focus
                #:set-sheet-pointer-cursor
                ;;
                #:port-set-mirror-region
                #:port-set-mirror-transformation
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
                #:get-next-event
                ;;
                #:frame-managers        ;used as slot
                #:top-level-sheet       ;used as slot
                ;; #:space-requirement     ;used as slot, very bogus
                ;; fbound
                #:medium-device-region
                #:medium-draw-circle*
                #:draw-image
                #:text-style-character-width
                #:height                ;this seems bogus
                #:width                 ;dito
                #:coordinate=
                #:get-transformation
                #:keyboard-input-focus
                #:port-grab-pointer
                #:port-ungrab-pointer
                ;;
                #:invoke-with-special-choices
                #:medium-draw-glyph
                #:medium-miter-limit
                #:make-graft
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
				#:description
				#:get-selector-for
				#:make-cstring
				#:%make-nsstring
				#:nslog
				#:ns-make-point
				#:%null-ptr
				#:pref
				#:rlet
				#:send
				#:send-super
				#:slet
				#:with-cstrs
				#:with-nsstr))
		