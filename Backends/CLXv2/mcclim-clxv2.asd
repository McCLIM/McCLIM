
(defsystem #:mcclim-clxv2
  :depends-on (#:mcclim-clx
	       #:mcclim-multi-mirrored-standard)

  :components
  ((:file "package")
   (:file "port" :depends-on ("package" "medium"))
   (:file "frame-manager" :depends-on ("port" "package" "mirrored-sheets"))
   (:file "medium" :depends-on ("package"))
   (:file "mirrored-sheets" :depends-on ("port" "package"))))

(defsystem #:mcclim-clxv2/pretty
    :depends-on (#:mcclim-clxv2
		 #:mcclim-clx/pretty))

