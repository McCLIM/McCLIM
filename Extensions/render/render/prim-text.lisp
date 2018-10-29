(in-package :mcclim-render-internals)

;;;
;;; Converting string into paths
;;;

;;; Font's utilities
(defparameter *text-sizes* '(:normal         12
                             :tiny            8
                             :very-small      8
                             :small          10
                             :large          14
                             :very-large     18
                             :huge           24))


(defun string-primitive-paths (x y string font size fn)
  (declare (ignore size))
  (let ((scale (zpb-ttf-font-units->pixels font)))
    (declare (ignore scale))
    (paths-from-string font string fn
                       :offset (paths:make-point x y))))


(defun paths-from-string (font text fn &key
                                         (offset (make-point 0 0))
                                         (kerning t))
  "Extract paths from a string."
  (let ((font-loader (zpb-ttf-font-loader (climb:font-face font)))
        (scale (zpb-ttf-font-units->pixels font)))
    (loop
       for previous-char = nil then char
       for char across text
       for code = (char-code char)
       for opacity-image = (font-glyph-opacity-image font code)
       for dx = (climb:font-glyph-left font code)
       for dy = (climb:font-glyph-top font code)
       for previous-width = nil then width
       for width = (max
                    (climb:font-glyph-right font code)
                    (climb:font-glyph-width font code))
       do (when previous-char
            (setf offset
                  (paths-ttf::p+ offset
                                 (paths:make-point (* 1
                                                      (+ previous-width
                                                         (* scale (if kerning
                                                                      (paths-ttf::kerning-offset previous-char
                                                                                                 char
                                                                                                 font-loader)
                                                                      0))))
                                                   0))))
         (funcall fn opacity-image dx dy
                  (make-translation-transformation (paths:point-x offset)
                                                   (paths:point-y offset))))))
