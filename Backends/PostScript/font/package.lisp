;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2017 by Cyrus Harmon <cyrus@bobobeach.com>
;;;
;;; ---------------------------------------------------------------------------
;;;

(defpackage #:clim-postscript-font
  (:use #:clim #:clim-extensions #:clim-lisp)
  (:export #:postscript-font-medium
           #:postscript-font-port
           #:device-fonts
           #:postscript-font-name
           #:postscript-device-font-name
           #:postscript-device-font-name-font-file
           #:text-size-in-font
           #:get-font-info
           #:font-info-name
           #:font-name-name
           #:font-name-size
           #:font-name-metrics-key
           #:*iso-latin-1-symbolic-names*)
  (:import-from #:clim-internals
                #:maxf
                #:port-text-style-mappings))
