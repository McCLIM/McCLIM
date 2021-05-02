;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2002 by Alexey Dejneka <adejneka@comail.ru>
;;;
;;; ---------------------------------------------------------------------------
;;;

(defpackage #:clim-postscript
  (:use #:clim #:clime #:climb #:clim-lisp #:clim-postscript-font)
  (:import-from #:clim-internals
                #:get-environment-variable
                #:map-repeated-sequence
                #:atan*

                #:ellipse-normal-radii*

                #:get-transformation
                #:untransform-angle
                #:with-transformed-position

                #:maxf

                #:port-text-style-mappings
                #:native-transformation
                #:device-transformation
                #:native-region
                #:device-region
                #:port-grafts)
  (:export #:load-afm-file
           #:with-output-to-postscript-stream))
