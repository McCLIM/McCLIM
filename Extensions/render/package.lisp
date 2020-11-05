(defpackage #:mcclim-render
  (:nicknames #:clim-render)
  (:use)
  (:export
   ;; colors
   #:color->octets
   ;; image
   #:draw-image*
   ;; image ops
   #:make-image
   #:clone-image
   #:copy-image
   #:blend-image
   #:fill-image))

(defpackage #:mcclim-render-extensions
  (:use)
  (:export
   ;; colors
   #:octet
   #:color-octet-xor
   #:octet-mult
   #:octet-blend-function
   #:octet-rgba-blend-function
   #:octet-rgb-blend-function
   #:octet-gray-blend-function
   #:octet-alpha-blend-function
   #:color-value->octet
   #:color-octet->value
   #:rgba->rgb
   #:rgba->gray
   #:rgba->gray-alpha
   #:rgba->alpha
   #:rgb->rgba
   #:rgb->gray
   #:rgb->alpha
   #:gray->rgba
   #:gray->rgb
   #:gray->alpha))

(defpackage #:mcclim-render-internals
  (:use #:clim #:clim-lisp #:mcclim-render #:mcclim-render-extensions)
  (:import-from #:alexandria
                #:minf
                #:maxf
                #:when-let
                #:when-let*)
  (:import-from #:clim-internals
                #:standard-color
                #:standard-flipping-ink
                ;; backend
                #:realize-mirror
                #:mirrored-pixmap
                #:port-lookup-sheet
                #:pixmap-medium
                #:pixmap-mirror)
  (:import-from #:mcclim-truetype
                #:glyph-info
                #:font-glyph-info
                #:font-generate-glyph
                #:glyph-info-left
                #:glyph-info-top
                #:glyph-info-advance-height
                #:glyph-info-advance-width
                #:glyph-info-pixarray
                #:glyph-pixarray
                #:ensure-gethash
                #:invoke-with-truetype-path-restart
                #:*truetype-font-path*
                #:*zpb-font-lock*
                #:*families/faces*
                #:truetype-face)
  (:import-from #:clim-backend
                #:port-set-mirror-region
                #:port-set-mirror-transformation))
