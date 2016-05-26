
;;;; The mcclim-truetype system extends the CLX backend with
;;;; antialiased font rendering in 100% Common Lisp (no foreign code),
;;;; using the XRender extension and the libraries zpb-ttf and
;;;; cl-vectors.
;;;;
;;;; To autoload mcclim-truetype after mcclim, make sure ASDF can find
;;;; it, and then add the following code to the init file for your
;;;; Lisp implementation:
;;;;
;;;;  (defmethod asdf:perform :after ((o asdf:load-op)
;;;;                                  (s (eql (asdf:find-system :clim-clx))))
;;;;    (asdf:oos 'asdf:load-op :mcclim-truetype))

(in-package :asdf-user)

(defsystem :mcclim-truetype
  :depends-on (:mcclim :clim-clx :zpb-ttf :cl-vectors :cl-paths-ttf :cl-aa)
  :serial t
  :components
  ((:file "truetype-package")
   (:file "xrender-fonts")
   (:file "fontconfig")
   (:file "mcclim-native-ttf")))

(defmethod perform :after ((o load-op) (s (eql (asdf:find-system :mcclim-truetype))))
  "Detect fonts using fc-match"
  (let ((autoconfig (find-symbol (symbol-name '#:autoconfigure-fonts) :mcclim-truetype)))
    (unless autoconfig
      (error "Couldn't find autoconfigure-fonts. This shouldn't happen."))
  (funcall autoconfig)))
