(cl:in-package #:mcclim-render-internals)

;;; Converting string into paths

(defun string-primitive-paths (medium x y string align-x align-y transform-glyphs)
  (let* ((device-region (climi::medium-device-region medium))

         (sheet (medium-sheet medium))
         (msheet (sheet-mirrored-ancestor sheet))
         (transformation (sheet-device-transformation sheet))

         (text-style (medium-text-style medium))
         (font (text-style-mapping (port medium) text-style)))
    (when (sheet-mirror msheet)
      (flet ((adjust-positions ()
               (ecase align-x
                 (:left)
                 (:center (let ((origin-x (climb:text-size medium string :text-style text-style)))
                            (decf x (/ origin-x 2.0))))
                 (:right  (let ((origin-x (climb:text-size medium string :text-style text-style)))
                            (decf x origin-x))))
               (ecase align-y
                 (:top (incf y (climb:font-ascent font)))
                 (:baseline)
                 (:center (let* ((ascent (climb:font-ascent font))
                                 (descent (climb:font-descent font))
                                 (height (+ ascent descent))
                                 (middle (- ascent (/ height 2.0s0))))
                            (incf y middle)))
                 (:baseline*)
                 (:bottom (decf y (climb:font-descent font))))))
        (if transform-glyphs
            (progn (adjust-positions)
                   (multiple-value-setq (x y)
                     (transform-position transformation x y)))
            (progn (multiple-value-setq (x y)
                     (transform-position transformation x y))
                   (adjust-positions))))
      (loop
        with glyph-transformation = (when transform-glyphs
                                      (multiple-value-bind (x0 y0)
                                          (transform-position transformation 0 0)
                                        (compose-translation-with-transformation
                                         transformation (- x0) (- y0))))
        for code across (climb:font-string-glyph-codes font string)
        for origin-x fixnum = (round x) then (+ origin-x (glyph-info-advance-width info))
        for origin-y fixnum = (round y) then (+ origin-y (glyph-info-advance-height info))
        for info = (if (null glyph-transformation)
                       (font-glyph-info font code)
                       (font-generate-glyph font code glyph-transformation))
        for dx fixnum = (glyph-info-left info)
        for dy fixnum = (glyph-info-top info)
        for opacity-image = (glyph-info-pixarray info)
        do (let ((opacity-image (make-instance 'climi::%ub8-stencil :array opacity-image))
                 (x1 (+ origin-x dx))
                 (y1 (- origin-y dy)))
             (with-bounding-rectangle* (min-x min-y max-x max-y)
                 (region-intersection
                  device-region
                  (make-rectangle* x1 y1
                                   (+ x1 (pattern-width opacity-image))
                                   (+ y1 (pattern-height opacity-image))))
               (%medium-fill-image-mask medium opacity-image
                                        min-x min-y
                                        (- max-x min-x) (- max-y min-y)
                                        (- (round x1)) (- (round y1)))))))))
