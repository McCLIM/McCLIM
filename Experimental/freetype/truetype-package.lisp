
(cl:defpackage :mcclim-truetype
  (:use :climi :clim :clim-lisp)
  (:export :*truetype-font-path*
           :*family-names*
           :*fontconfig-faces*
           :truetype-device-font-name 
           :fontconfig-font-name
           :make-truetype-device-font-name 
           :make-fontconfig-font-name
           :truetype-face-filename
           :truetype-face-size
           :truetype-face-ascent
           :truetype-face-descent
  )
  ;; Hmm. Replaced by import/use-package in freetype-fonts-alien
  ;; and freetype-fonts-cffi, so that I can load with either alien
  ;; or cffi on SBCL.
  ;;#+(or cmu scl sbcl)
  #+NIL
  (:import-from #+(or cmu scl) :alien
                #+sbcl :sb-alien
                :slot :make-alien :alien :deref))
