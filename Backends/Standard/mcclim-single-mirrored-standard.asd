
(defsystem #:mcclim-single-mirrored-standard
  :depends-on (#:mcclim-standard)
  :components
  ((:file "package")
   (:file "sm-sheets" :depends-on ("package"))))

