(in-package :mcclim-render-internals)

;;;
;;; Converting string into paths
;;;

(defun string-primitive-paths (medium x y string font)
  (loop
     for origin-x fixnum = (round x) then (+ origin-x (climb:font-glyph-dx font code))
     for origin-y fixnum = (round y) then (+ origin-y (climb:font-glyph-dy font code))
     for code across (climb:font-string-glyph-codes font string)
     for dx fixnum = (climb:font-glyph-left font code)
     for dy fixnum = (climb:font-glyph-top font code)
     for opacity-image = (font-glyph-opacity-image font code)
     do
       (let ((msheet (sheet-mirrored-ancestor (medium-sheet medium)))
             (opacity-image (make-instance 'climi::%rgba-pattern :array opacity-image))
             (transformation (make-translation-transformation origin-x origin-y)))
         (when (and msheet (sheet-mirror msheet))
           (multiple-value-bind (x1 y1)
               (transform-position
                (clim:compose-transformations transformation
                                              (sheet-native-transformation
                                               (medium-sheet medium)))
                (+ dx ) (- dy))
             (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
                 (region-intersection
                  (climi::medium-device-region medium)
                  (make-rectangle* x1 y1
                                   (+ x1 (pattern-width opacity-image))
                                   (+ y1 (pattern-height opacity-image))))
               (%medium-fill-image-mask medium opacity-image
                                        min-x min-y
                                        (- max-x min-x) (- max-y min-y)
                                        (- (round x1)) (- (round y1)))))))))
