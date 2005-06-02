;; -*- Mode: Lisp; -*-

;; $Id: beagle-backend.asd,v 1.3 2005/06/02 22:17:27 drose Exp $

(defpackage "BEAGLE"
  (:use "CLIM" "CLIM-LISP")
;  (:export #:run-listener #:run-listener-process #:dev-commands)
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
                #:device-font-text-style))

(in-package :beagle)

(require "OBJC-SUPPORT")

(asdf::defsystem #:beagle
		 :version "0.1"
		 :serial t
		 :components ((:file "package")
			      (:module "Native"
				       :pathname #.(make-pathname :directory '(:relative "native"))
				       :components
				       ((:file "lisp-bezier-path")
					(:file "lisp-window")
					(:file "lisp-window-delegate")
					(:file "lisp-view" :depends-on ("lisp-bezier-path"))
					(:file "lisp-view-additional" :depends-on ("lisp-view"))
					(:file "lisp-image")))
;;;					(:file "lisp-unmanaged-view")))
			      (:file "cocoa-util")
			      (:module "Windowing"
				       :depends-on ("Native")
				       :pathname #.(make-pathname :directory '(:relative "windowing"))
				       :components
				       ((:file "port")
					(:file "frame-manager")
					(:file "mirror")
					(:file "graft")))
			      (:module "Output"
				       :depends-on ("Windowing")
				       :pathname #.(make-pathname :directory '(:relative "output"))
				       :components
				       ((:file "medium")
					(:file "fonts")))
			      (:module "Input"
				       :depends-on ("Windowing")
				       :pathname #.(make-pathname :directory '(:relative "input"))
				       :components
				       ((:file "events")
					(:file "keysymdef")))
			      (:module "Glimpse"
				       :pathname #.(make-pathname :directory '(:relative "glimpse"))
				       :components
				       ((:file "glimpse")
					(:file "glimpse-support")
					(:file "glimpse-command-tables")
					(:file "glimpse-present-process"   :depends-on ("glimpse" "glimpse-support"))
					(:file "glimpse-present-window"    :depends-on ("glimpse" "glimpse-support"))
					(:file "glimpse-modeless-commands" :depends-on ("glimpse" "glimpse-support"))
					(:file "glimpse-process-commands"  :depends-on ("glimpse" "glimpse-support"))
					(:file "glimpse-window-commands"   :depends-on ("glimpse" "glimpse-support"))))
			      (:module "Profile"
				       :pathname #.(make-pathname :directory '(:relative "profile"))
				       :components
				       ((:file "profile")))
			      (:module "Tests"
				       :pathname #.(make-pathname :directory '(:relative "tests"))
				       :components
				       ((:file "drawing-tests")
					(:file "graft-tests")))
			      ))

