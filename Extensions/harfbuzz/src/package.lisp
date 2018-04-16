(defpackage :mcclim-harfbuzz
  (:use :cl)
  (:documentation "CFFI interface to Harfbuzz")
  (:export
   #:with-buffer
   #:hb-buffer-create
   #:hb-buffer-destroy
   #:hb-buffer-set-direction
   #:buffer-add-string
   #:hb-ft-font-create
   #:hb-shape
   #:hb-buffer-get-glyph-positions
   #:hb-buffer-get-glyph-infos))
