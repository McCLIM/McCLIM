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
  (:use #:clim #:clime #:clim-lisp #:mcclim-render #:mcclim-render-extensions #:mcclim-truetype)
  (:import-from #:alexandria
                #:minf
                #:maxf
                #:when-let
                #:when-let*
                #:ensure-gethash)
  (:import-from #:clim-internals
                #:standard-color
                #:named-color
                #:standard-flipping-ink
                #:%transparent-ink
                #:standard-opacity
                #:opacity-value
                #:pattern
                #:indexed-pattern
                #:rectangular-tile
                #:rectangular-tile-design
                #:transformed-design
                #:transformed-design-design
                #:transformed-design-transformation
                #:in-compositum
                #:out-compositum
                #:over-compositum
                #:compositum-ink
                #:compositum-mask
                #:compositum-foreground
                #:compositum-background)
  (:import-from #:clim-backend
                #:port-set-mirror-geometry
                #:medium-native-transformation
                #:medium-device-transformation
                #:medium-native-region
                #:medium-device-region))
