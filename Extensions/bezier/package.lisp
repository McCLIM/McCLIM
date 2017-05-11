
(defpackage :mcclim-bezier
  (:use #:clim #:clim-lisp)

  (:import-from #:clim-null
                #:null-medium)

  (:import-from #:mcclim-render
                #:render-medium-mixin
                #:make-path
                #:line-to
                #:curve-to
                #:%medium-fill-paths
                #:%medium-stroke-paths)

  (:import-from #:clim-postscript
                #:*transformation*
                #:postscript-medium
                #:postscript-medium-file-stream
                #:postscript-actualize-graphics-state
                #:write-coordinates)

  (:export #:bezier-design
           #:bezier-curve
           #:bezier-area
           #:bezier-union
           #:bezier-difference
           
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
           #:p0 #:p1 #:p2 #:p3
           #:region-difference
           #:convolve-regions

           #:draw-bezier-design*
           #:medium-draw-bezier-design*))
