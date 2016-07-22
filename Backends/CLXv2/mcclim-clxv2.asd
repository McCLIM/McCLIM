
(defsystem #:mcclim-clxv2
  :depends-on (#:mcclim-clx)

  :components
  ((:file "package")
   (:file "port" :depends-on ("package"))
   (:file "frame-manager" :depends-on ("port" "package" "mirrored-sheets"))
   (:file "event-manager" :depends-on ("port" "package"))
   (:file "mirrored-sheets" :depends-on ("port" "package"))))

(defsystem #:mcclim-clxv2/pretty
    :depends-on (#:mcclim-clxv2
		 #:mcclim-clx/pretty))

