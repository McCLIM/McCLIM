
(defsystem #:mcclim-multi-mirrored-standard
  :depends-on (#:mcclim-standard)
  :components
  ((:file "package")
   (:file "multi-mirrored-sheets" :depends-on ("package"))))
