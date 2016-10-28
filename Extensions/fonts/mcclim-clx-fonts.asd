
(defsystem #:mcclim-clx-fonts
  :depends-on (#:mcclim-fonts/truetype #:mcclim-clx)
  :components
  ((:file "xrender-fonts")))
