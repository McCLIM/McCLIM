(in-package #:mcclim-render)

;;; Converting string into paths

(defun string-primitive-paths (medium x y glyph-codes transformation port font)
  (loop
    with glyph-tr = (multiple-value-bind (x0 y0)
                        (transform-position transformation 0 0)
                      (compose-transformation-with-translation
                       transformation (- x0) (- y0)))
    for code across glyph-codes
    for origin-x fixnum = (round x) then (+ origin-x (glyph-info-advance-width info))
    for origin-y fixnum = (round y) then (+ origin-y (glyph-info-advance-height info))
    for info = (if (translation-transformation-p transformation)
                   (font-glyph-info font code)
                   (font-generate-glyph port font code :transformation glyph-tr))
    for dx fixnum = (glyph-info-left info)
    for dy fixnum = (glyph-info-top info)
    for opacity-image = (glyph-info-pixarray info)
    do (let* ((opacity-image (make-instance 'climi::%ub8-stencil :array opacity-image))
              (x1 (+ origin-x dx))
              (y1 (+ origin-y (- dy)))
              (x2 (+ x1 (pattern-width opacity-image)))
              (y2 (+ y1 (pattern-height opacity-image))))
         (clim:with-bounding-rectangle* (min-x min-y :width width :height height)
             (region-intersection (medium-device-region medium)
                                  (make-rectangle* x1 y1 x2 y2))
           (%medium-fill-image-mask medium opacity-image
                                    min-x min-y width height
                                    (- (round x1)) (- (round y1)))))))
