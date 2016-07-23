
(defsystem #:mcclim-clxv3
  :depends-on (#:mcclim-clx
	       #:mcclim-single-mirrored-standard)

  :components
  ((:file "package")
   (:file "port" :depends-on ("package"))
   (:file "frame-manager" :depends-on ("port" "package" "mirrored-sheets"))
   (:file "event-manager" :depends-on ("port" "package"))
   (:file "mirrored-sheets" :depends-on ("port" "package"))))

(defsystem #:mcclim-clxv3/pretty
    :depends-on (#:mcclim-clxv3
		 #:mcclim-clx/pretty))

