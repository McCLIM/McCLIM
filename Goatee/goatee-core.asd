
(defsystem #:goatee-core
  :depends-on (#:clim-basic)
  :serial t
  :components ((:file "packages")
               (:file "conditions")
               (:file "dbl-list")
               (:file "flexivector")
               (:file "buffer")
               (:file "editable-buffer")
               (:file "editable-area")
               (:file "clim-area")
               (:file "kill-ring")
               (:file "goatee-command")
               (:file "editing-stream")
               (:file "presentation-history")))
