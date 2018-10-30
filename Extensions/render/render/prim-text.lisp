(in-package :mcclim-render-internals)

;;;
;;; Converting string into paths
;;;

(defun string-primitive-paths (x y string font size fn)
  (declare (ignore size))
  (loop
     for origin-x fixnum = (round x) then (+ origin-x (climb:font-glyph-dx font code))
     for origin-y fixnum = (round y) then (+ origin-y (climb:font-glyph-dy font code))
     for code across (climb:font-string-glyph-codes font string)
     for dx fixnum = (climb:font-glyph-left font code)
     for dy fixnum = (climb:font-glyph-top font code)
     for opacity-image = (font-glyph-opacity-image font code)
     do
       (funcall fn opacity-image dx dy (make-translation-transformation origin-x origin-y))))
