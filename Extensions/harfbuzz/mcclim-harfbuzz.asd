(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem #:mcclim-harfbuzz
  :description "CFFI interface to Harfbuzz"
  :license "Apache"
  :serial t
  :depends-on (:cffi
               :alexandria
               :trivial-garbage)
  :components ((:module src
                        :serial t
                        :components ((:file "package")
                                     (cffi-grovel:grovel-file "grovel")
                                     (:file "functions")
                                     (:file "harfbuzz")))))
