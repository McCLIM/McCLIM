
(defsystem #:slim
  :depends-on (#:clim-core)
  :components ((:file "slim")
               (:static-file "slim.md")))
