(defpackage :fontconfig
  (:use :cl)
  (:documentation "CFFI interface to Fontconfig")
  (:export #:get-version
           #:init-fontconfig
           #:init-reinitialize
           #:init-bring-up-to-date
           #:make-pattern
           #:pattern-get
           #:config-home
           #:match-font))
