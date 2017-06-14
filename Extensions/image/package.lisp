
(defpackage :mcclim-image
  (:use #:clim #:clim-lisp #:clim-extensions)
  (:import-from #:clim-internals
                #:design
                #:pattern
                #:with-medium-options
                #:pattern-width
                #:pattern-height
                #:opticl-read-bitmap-file
                #:medium-draw-pattern*)
  (:export
   #:image
   #:rgb-image
   #:rgb-pattern
   #:image-width
   #:image-height
   #:image-data
   #:image-alpha-p
   #:mage-alpha-p
   #:rgb-image-design
   #:make-rgb-image-design
   #:medium-free-image-design
   #:medium-draw-image-design*
   #:sheet-rgb-image
   #:sheet-rgb-data
   #:draw-design
   #:xpm-parse-file
   #:make-pattern-from-bitmap-file
   #:medium-data
   ))
