;;;; mcclim-dot smoke tests
;;;;
;;;; This file is part of the mcclim-dot extension. See
;;;; Extensions/dot/README.md and Extensions/dot/LICENSE for more information.

(cl:in-package #:mcclim-dot.test)

(def-test edge-label-1 (:suite :mcclim-dot)
  (mcclim-raster-image:with-output-to-raster-image-file
      (s (asdf:system-relative-pathname :mcclim-dot "edge-label-1.png"))
    (clim:surrounding-output-with-border (s :background clim:+white+)
      (clim:format-graph-from-roots
       '(a)
       (lambda (obj stream)
         (clim:surrounding-output-with-border (stream :shape :rounded)
           (princ obj stream)))
       (lambda (obj)
         (cdr (assoc obj '((a b) (b c)))))
       :stream s
       :arc-drawer (make-instance 'mcclim-dot::dot-arc-drawer
                                  :edge-label-printer
                                  (lambda (drawer stream from to)
                                    (declare (ignore drawer to))
                                    (ecase from
                                      (a
                                       (format stream "A to B~%A"))
                                      (b
                                       (format stream "B to C~%D")))))
       :orientation :vertical
       :graph-type :dot-digraph
       :merge-duplicates t))))
