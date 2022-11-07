;;;; mcclim-dot smoke tests
;;;;
;;;; This file is part of the mcclim-dot extension. See
;;;; Extensions/dot/README.md and Extensions/dot/LICENSE for more information.

(cl:in-package #:mcclim-dot.test)

(defparameter *simple-graph*
  '((s1 10 2)
    (s8 9)
    (s24 27 25)
    (s30 31 33)
    (s35 36 43)
    (2 3 16 17 t1 18)
    (3 4)
    (4 5)
    (5 6 t35 23)
    (6 7)
    (7 t8)
    (9 42 t1)
    (10 11 14 t1 13 12)
    (11 4)
    (12 29)
    (13 19)
    (14 15)
    (15 t1)
    (16 15)
    (17 19)
    (18 29)
    (19 21 20 28)
    (20 15)
    (21 22)
    (22 23 t35)
    (23 t24 t1)
    (25 t1 26)
    (26 4)
    (27 t24)
    (28 29)
    (29 t30)
    (31 t1 32)
    (32 23)
    (33 t30 34)
    (34 29)
    (36 19)
    (37 39 41 38 40)
    (38 4)
    (39 15)
    (40 19)
    (41 29)
    (42 4)
    (43 38 40))
  "Figure 1-2b from \"A Technique for Drawing Directed Graphs\" by Emden
R. Gansner, Eleftherios Koutsofios, Stephen C. North, Kiem-phong Vo")

(defparameter *simple-graph-2*
  '((s1 10 2)
    (s8 9)
    (s24 27 25)
    (s30 31 33)
    (s35 36 43)
    (2 3 16 17 t1 18)
    (3 4)
    (4 5)
    (5 t35 23)
    (9 42 t1)
    (10 11 14 t1 13 12)
    (11 4)
    (12 29)
    (13 19)
    (14)
    (16)
    (17 19)
    (18 29)
    (19 21 20 28)
    (20)
    (21 22)
    (22 23 t35)
    (23 t24 t1)
    (25 t1 26)
    (26 4)
    (27 t24)
    (28 29)
    (29 t30)
    (31 t1 32)
    (32 23)
    (33 t30 34)
    (34 29)
    (36 19)
    (37 39 41 38 40)
    (38 4)
    (39)
    (40 19)
    (41 29)
    (42 4)
    (43 38 40))
  "A reduced form of *SIMPLE-GRAPH*")

(defun format-graph (graph stream type &rest args)
  (apply
   #'clim:format-graph-from-roots
   '(s8 s24 37 s35 s1 s30)
   (lambda (obj stream)
     (clim:surrounding-output-with-border (stream :shape :rounded)
       (princ obj stream)))
   (lambda (obj)
     (cdr (assoc obj graph)))
   :stream stream
   :maximize-generations t
   :orientation :vertical
   :graph-type type
   :merge-duplicates t
   args))

(def-test smoke-test-1 (:suite :mcclim-dot)
  (finishes
    (clim-extensions:with-output-to-drawing-stream
        (s :raster (asdf:system-relative-pathname :mcclim-dot "smoke-test-1-default.png"))
      (clim:surrounding-output-with-border (s :background clim:+white+)
        (format-graph *simple-graph* s :digraph))))
  (finishes
    (clim-extensions:with-output-to-drawing-stream
        (s :raster (asdf:system-relative-pathname :mcclim-dot "smoke-test-1-dot.png"))
      (clim:surrounding-output-with-border (s :background clim:+white+)
        (format-graph *simple-graph* s :dot-digraph)))))

(def-test layout-override-1 (:suite :mcclim-dot)
  (let (override)
    (finishes
      (clim-extensions:with-output-to-drawing-stream
          (s :raster (asdf:system-relative-pathname :mcclim-dot "smoke-test-1-dot-override-1.png"))
        (clim:surrounding-output-with-border (s :background clim:+white+)
          (setf override (mcclim-dot:make-layout-override
                          (format-graph *simple-graph* s :dot-digraph)))))
      (clim-extensions:with-output-to-drawing-stream
          (s :raster (asdf:system-relative-pathname :mcclim-dot "smoke-test-1-dot-override-2.png"))
        (clim:surrounding-output-with-border (s :background clim:+white+)
          (format-graph *simple-graph-2* s :dot-digraph :layout-override override))))

    (finishes
      (clim-extensions:with-output-to-drawing-stream
          (s :raster (asdf:system-relative-pathname :mcclim-dot "smoke-test-1-dot-override-3.png"))
        (clim:surrounding-output-with-border (s :background clim:+white+)
          (setf override (mcclim-dot:make-layout-override
                          (format-graph *simple-graph* s :dot-digraph)
                          :include-edges-p t))))

      (clim-extensions:with-output-to-drawing-stream
          (s :raster (asdf:system-relative-pathname :mcclim-dot "smoke-test-1-dot-override-4.png"))
        (clim:surrounding-output-with-border (s :background clim:+white+)
          (format-graph *simple-graph-2* s :dot-digraph :layout-override override))))))
