
(defsystem #:mcclim-full-mirrored-standard
  :depends-on (#:mcclim-standard)
  :components
  ((:file "package")
   (:file "fm-sheets" :depends-on ("package"))))
