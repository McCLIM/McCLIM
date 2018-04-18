(defpackage :mcclim-fontconfig
  (:use :cl)
  (:documentation "CFFI interface to Fontconfig")
  (:export #:get-version
           #:init-fontconfig
           #:init-reinitialize
           #:init-bring-up-to-date
           #:match-font
           #:font-list
           ))
