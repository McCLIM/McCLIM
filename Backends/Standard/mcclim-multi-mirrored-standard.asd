
(defsystem #:mcclim-multi-mirrored-standard
  :depends-on (#:mcclim-standard)
  :components
  ((:file "package")
   (:file "mm-sheets" :depends-on ("package"))))
