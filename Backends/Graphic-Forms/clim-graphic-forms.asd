
;;; XXX: what is graphic-forms-uitoolkit?

(defsystem #:clim-graphic-forms
  :depends-on (#:clim #:graphic-forms-uitoolkit)
  :components
  ((:file "package")
   (:file "utils" :depends-on ("package"))
   (:file "graft" :depends-on ("package"))
   (:file "port" :depends-on ("utils" "graft"))
   (:file "medium" :depends-on ("port"))
   (:file "pixmap" :depends-on ("medium"))
   (:file "frame-manager" :depends-on ("medium"))
   (:file "gadgets" :depends-on ("port"))))
