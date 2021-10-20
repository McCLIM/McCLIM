(defpackage :mcclim-truetype
  (:use :climi :clim :clim-lisp)
  (:import-from :alexandria
                :ensure-gethash
                :when-let
                :if-let)
  (:export #:*truetype-font-path*
           #:*families/faces*
           #:*zpb-font-lock*
           #:truetype-device-font-name
           #:fontconfig-font-name
           #:make-truetype-device-font-name
           #:make-fontconfig-font-name
           #:find-fontconfig-font
           #:invoke-with-truetype-path-restart)
  (:export #:truetype-font
           #:truetype-font-family
           #:truetype-face
           #:cached-truetype-font)
  (:export #:font-glyph-id
           #:font-generate-glyph
           #:glyph-pixarray)
  (:export #:glyph-info
           #:glyph-info-id
           #:glyph-info-advance-width*
           #:glyph-info-advance-height*))
