
(defsystem #:mcclim-clxv3
  :depends-on (#:mcclim-backend-common
               #:mcclim-clx)

  :components
  ((:file "package")
   (:file "port" :depends-on ("package" "medium"))
   (:file "frame-manager" :depends-on ("port" "package" "mirrored-sheets"))
   (:file "medium" :depends-on ("package"))
   (:file "mirrored-sheets" :depends-on ("port" "package"))))

(defsystem #:mcclim-clxv3/pretty
    :depends-on (#:mcclim-clxv3
		 #:mcclim-clx/pretty))

