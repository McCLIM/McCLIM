(in-package #:clim-tests)

(def-suite* :mcclim.bezier
  :in :mcclim)

(test bezier.polygonalize.smoke
  "Smoke test for the `polygonalize' function."

  ;; The curve is transformed into a polygon by recursive subdivision
  ;; until a given precision is reached. If the entire curve is
  ;; smaller than the permitted error, no polygon is produced.
  (let* ((original    (mcclim-bezier:make-bezier-area*
                       '(0 0 1 1 2 1 3 0 2 -1 1 -1 0 0)))
         (transform   (make-scaling-transformation .01 .01))
         (transformed (transform-region transform original)))
    (is-true (typep (mcclim-bezier:polygonalize original) 'standard-polygon))
    (is (eq +nowhere+ (mcclim-bezier:polygonalize transformed)))))
