
(defpackage :mcclim-image
  (:use #:clim #:clim-lisp #:clim-extensions)
  (:import-from #:clim-internals
                #:design
                #:pattern
                #:with-medium-options
                #:pattern-width
                #:pattern-height
                #:opticl-read-bitmap-file
                #:opticl-read-gif-file
                #:opticl-read-jpg-file
                #:opticl-read-pbm-file
                #:opticl-read-pgm-file
                #:opticl-read-png-file
                #:opticl-read-pnm-file
                #:opticl-read-ppm-file
                #:opticl-read-tiff-file
                #:def-grecording
                #:defrecord-predicate
                #:output-record-position
                #:defmethod*
                #:coordinate=
                #:if-supplied
                #:record
                #:with-standard-rectangle*)
  (:export
   #:image
   #:rgb-image
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
   #:*xpm-x11-colors*
   #:make-pattern-from-bitmap-file
   #:medium-data
   ))
