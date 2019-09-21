(defsystem "clim-pdf"
  :depends-on ("clim-basic" "cl-pdf" "flexi-streams" "clim-postscript-font")
  :serial t
  :components ((:file "package")
               (:file "paper")
               (:file "class")
               (:file "graphics")
               (:file "sheet"))
  :in-order-to ((test-op (test-op "clim-pdf/test"))))

(defsystem "clim-pdf/test"
  :depends-on ("clim-pdf"
               "fiveam"
               "mcclim/test-util")
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "smoke"))))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:clim-pdf.test '#:run-tests)))
