
(defsystem #:clim-looks/pixie
  :depends-on (#:clim #:clim-layouts/tab)
  :components ((:file "pixie")))

(defmethod perform :after ((o load-op)
                           (s (eql (find-system :clim-looks/pixie))))
  (symbol-call :climi :use-pixie))
