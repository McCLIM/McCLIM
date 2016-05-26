
;;; CLIM-PostScript is not a backend in the normal sense.
;;; It is an extension (Chap. 35.1 of the spec) and is an
;;; "included" part of McCLIM. Hence the defsystem is here.
;;; 
;;; Move the whole system to the Modules/ directory - dk

(defsystem #:clim-postscript
  :depends-on (#:clim-basic)
  :components
  ((:file "package")
   (:file "encoding" :depends-on ("package"))
   (:file "paper" :depends-on ("package"))
   (:file "class" :depends-on ("paper" "package"))
   (:file "font" :depends-on ("encoding" "class" "paper" "package"))
   (:file "graphics" :depends-on ("encoding" "paper" "class" "font" "package"))
   (:file "sheet" :depends-on ("paper" "class" "graphics" "package"))
   (:file "afm" :depends-on ("class" "paper" "font" "package"))
   (:file "standard-metrics" :depends-on ("font" "package"))))
