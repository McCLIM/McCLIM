
(defsystem #:esa-mcclim
  :depends-on (#:clim-core)
  :components ((:file "packages")
               (:file "utils" :depends-on ("packages"))
               (:file "colors" :depends-on ("packages"))
               (:file "esa" :depends-on ("colors" "packages" "utils"))
               (:file "esa-buffer" :depends-on ("packages" "esa"))
               (:file "esa-io" :depends-on ("packages" "esa" "esa-buffer"))
               (:file "esa-command-parser" :depends-on ("packages" "esa"))))
