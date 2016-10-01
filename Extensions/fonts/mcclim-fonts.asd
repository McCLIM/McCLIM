
#| dummy system to make Quicklisp happy |#
(defsystem #:mcclim-fonts)

;;;; The mcclim-truetype system extends the CLX backend with
;;;; antialiased font rendering in 100% Common Lisp (no foreign code),
;;;; using the XRender extension and the libraries zpb-ttf and
;;;; cl-vectors.
;;;;

(defsystem #:mcclim-fonts/truetype
  :depends-on (#:mcclim-clx #:zpb-ttf #:cl-vectors #:cl-paths-ttf #:cl-aa #:alexandria)
  :components
  ((:file "truetype-package")
   (:file "xrender-fonts" :depends-on ("truetype-package" "fontconfig"))
   (:file "fontconfig" :depends-on ("truetype-package"))
   (:file "mcclim-native-ttf" :depends-on ("truetype-package" "xrender-fonts"))))

(defmethod perform :after ((o load-op)
                           (s (eql (find-system :mcclim-fonts/truetype))))
  (uiop:symbol-call :mcclim-truetype :autoconfigure-fonts))
