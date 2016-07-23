
(defsystem #:mcclim-single-mirrored-standard
  :depends-on (#:mcclim-standard)
  :components
  ((:file "package")
   (:file "single-mirrored-sheets" :depends-on ("package"))))

