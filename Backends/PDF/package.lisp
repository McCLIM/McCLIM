;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2017 by Cyrus Harmon <cyrus@bobobeach.com>
;;;
;;; ---------------------------------------------------------------------------
;;;

(defpackage #:clim-pdf
  (:use #:clim #:clim-extensions #:clim-lisp #:clim-backend)
  (:export #:with-output-to-pdf-stream)
  (:import-from #:clim-postscript
                #:medium-color-rgb)
  (:import-from #:clim-internals
                #:map-repeated-sequence
                #:with-transformed-position
                #:get-environment-variable
                #:port-text-style-mappings
                ;; ellipses
                #:reparameterize-ellipse
                #:ellipse-cubic-bezier-points
                #:transform-angle
                #:native-transformation
                #:device-transformation
                #:native-region
                #:device-region
                #:port-grafts))
