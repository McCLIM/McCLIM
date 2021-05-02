(defpackage #:mcclim-bezier
  (:use #:clim #:clim-lisp)

  (:import-from #:clim-internals
                #:with-transformed-position
                #:medium-native-transformation)

  (:import-from #:clim-null
                #:null-medium)

  (:import-from #:mcclim-render-internals
                #:render-medium-mixin
                #:make-path
                #:curve-to
                #:%medium-fill-paths
                #:%medium-stroke-paths)

  (:import-from #:clim-postscript
                #:postscript-medium
                #:postscript-actualize-graphics-state
                #:write-coordinates
                #:with-graphics-state)

  (:export #:bezier-design
           #:bezier-curve
           #:bezier-area
           #:bezier-union
           #:bezier-difference

           #:polygonalize
           #:polygon-points

           #:transformation
           #:areas
           #:positive-areas
           #:negative-areas

           #:make-bezier-area
           #:make-bezier-area*
           #:make-bezier-curve
           #:make-bezier-curve*

           #:relative-to-absolute-coord-seq
           #:segments
           #:region-difference
           #:convolve-regions

           #:draw-bezier-design*
           #:medium-draw-bezier-design*))
