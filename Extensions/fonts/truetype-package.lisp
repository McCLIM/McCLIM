
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
           :truetype-face-descent))
