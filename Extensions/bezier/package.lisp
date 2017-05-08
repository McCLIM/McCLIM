
(defpackage :mcclim-bezier
  (:use #:clim #:clim-lisp #:climi)
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
           
           #:segments
           #:p0 #:p1 #:p2 #:p3
           #:region-difference
           #:convolve-regions
           
           #:draw-bezier-design*
           #:medium-draw-bezier-design*))
