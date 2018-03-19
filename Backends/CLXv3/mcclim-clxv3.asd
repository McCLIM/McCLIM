
(defsystem #:mcclim-clxv3
  :depends-on (#:mcclim-backend-common
               #:mcclim-clx)

  :components
  ((:file "package")
   (:file "port" :depends-on ("package" "medium"))
   (:file "frame-manager" :depends-on ("port" "package"))
   (:file "medium" :depends-on ("package"))))

(defsystem #:mcclim-clxv3/pretty
    :depends-on (#:mcclim-clxv3
		 #:mcclim-clx/pretty))

