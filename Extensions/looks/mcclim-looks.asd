
(defsystem #:mcclim-looks/pixie
  :depends-on (#:clim #:mcclim-layouts/tab)
  :components ((:file "pixie")))

#+ (or)
(defmethod perform :after ((o load-op)
                           (s (eql (find-system :mcclim-looks/pixie))))
  (symbol-call :climi :use-pixie))
