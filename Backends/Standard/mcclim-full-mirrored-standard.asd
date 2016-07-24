
(defsystem #:mcclim-full-mirrored-standard
  :depends-on (#:mcclim-multi-mirrored-standard)
  :components
  ((:file "package")
   (:file "fm-sheets" :depends-on ("package"))))
