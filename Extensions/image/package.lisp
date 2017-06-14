
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

(in-package :mcclim-image)

;;;
;;; import symbols into climi
;;; 

(in-package #:clim-internals)

(cl:import 'mcclim-image:rgb-image)
(cl:import 'mcclim-image:image-width)
(cl:import 'mcclim-image:image-height)
(cl:import 'mcclim-image:image-alpha-p)
(cl:import 'mcclim-image:image-data)
(cl:import 'mcclim-image:rgb-pattern)
(cl:import 'mcclim-image:rgb-image-design)
(cl:import 'mcclim-image:image)
(cl:import 'mcclim-image:make-pattern-from-bitmap-file)
(cl:import 'mcclim-image:xpm-parse-file)
(cl:import 'mcclim-image:medium-draw-image-design*)
(cl:import 'mcclim-image:medium-data)
