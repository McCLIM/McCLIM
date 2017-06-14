
(defpackage :mcclim-image
  (:use #:clim #:clim-lisp)

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

(cl:import 'mcclim-image:rgb-image
           (find-package 'clim-internals))
(cl:import 'mcclim-image:image-width
           (find-package 'clim-internals))
(cl:import 'mcclim-image:image-height
           (find-package 'clim-internals))
(cl:import 'mcclim-image:image-alpha-p
           (find-package 'clim-internals))
(cl:import 'mcclim-image:image-data
           (find-package 'clim-internals))
(cl:import 'mcclim-image:rgb-pattern
           (find-package 'clim-internals))
(cl:import 'mcclim-image:rgb-image-design
           (find-package 'clim-internals))
(cl:import 'mcclim-image:image
           (find-package 'clim-internals))
(cl:import 'mcclim-image:make-pattern-from-bitmap-file
           (find-package 'clim-internals))
(cl:import 'mcclim-image:xpm-parse-file
           (find-package 'clim-internals))
(cl:import 'mcclim-image:medium-draw-image-design*
           (find-package 'clim-internals))
(cl:import 'mcclim-image:medium-data
           (find-package 'clim-internals))
