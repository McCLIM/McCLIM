
(defsystem #:clim
  :depends-on (#:clim-core #:goatee-core #:drei-mcclim)
  :components ((:file "input-editing-goatee")
               (:file "input-editing-drei")
               (:file "text-editor-gadget")))
