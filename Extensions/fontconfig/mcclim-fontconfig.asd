(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem #:mcclim-fontconfig
  :description "CFFI interface to Fontconfig"
  :license "Apache"
  :serial t
  :depends-on (:cffi
               :alexandria
               :trivial-garbage)
  :components ((:module src
                        :serial t
                        :components ((:file "package")
                                     (cffi-grovel:grovel-file "grovel")
                                     (:file "conditions")
                                     (:file "functions")
                                     (:file "fontconfig")))))
