;;; CLIM-PostScript is not a backend in the normal sense.
;;; It is an extension (Chap. 35.1 of the spec) and is an
;;; "included" part of McCLIM. Hence the defsystem is here.
;;;
;;; Move the whole system to the Modules/ directory - dk

(defsystem #:clim-postscript
  :depends-on (#:clim-basic
               #:clim-core
               #:clim-postscript-font)
  :serial t
  :components ((:file "package")
               (:file "paper")
               (:file "class")
               (:file "graphics")
               (:file "sheet")
               (:file "output-destination")))
