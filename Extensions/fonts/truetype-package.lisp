(defpackage #:mcclim-truetype
  (:use #:climi #:clim #:clim-lisp #:climb #:clime)
  (:import-from :alexandria
                #:ensure-gethash
                #:when-let
                #:if-let
                #:minf
                #:maxf
                #:assoc-value
                #:read-file-into-byte-vector
                #:maphash-values)
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
           #:font-glyph-dx
           #:font-glyph-info
           #:font-ascent
           #:font-descent
           #:font-generate-glyph
           #:glyph-pixarray
           #:font-string-glyph-codes)
  (:export #:glyph-info
           #:glyph-info-id
           #:glyph-info-pixarray
           #:glyph-info-dx
           #:glyph-info-dy
           #:glyph-info-left
           #:glyph-info-top
           #:glyph-info-advance-width*
           #:glyph-info-advance-height*
           #:glyph-info-advance-width
           #:glyph-info-advance-height)
  (:export #:ttf-port-mixin
           #:ttf-medium-mixin))
