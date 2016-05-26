
(defsystem #:clim
  :depends-on (#:clim-core #:goatee-core #:clim-postscript #:drei-mcclim)
  :components ((:file "input-editing-goatee")
               (:file "input-editing-drei")
               (:file "text-editor-gadget")))
