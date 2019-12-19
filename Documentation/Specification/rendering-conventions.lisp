;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10; Lowercase: Yes -*-

(cl:defpackage #:clim-specification
  (:use
   #:clim-lisp
   #:clim))
(cl:in-package #:clim-specification)

;;; Called in primary quadrant
(defun show-scan-conversion (pieces &key (scale 25) (edges '(0 0 5 5))
                                         (control-points nil) (show-centers t)
                                         (y-translation 50)
                                         (stream *standard-output*))
  (destructuring-bind (dleft dtop dright dbottom) edges
    (with-translation (stream 0 y-translation)
      (with-scaling (stream scale scale)
        (with-drawing-options (stream   ; :line-thickness 1
                                        ; :scale-thickness nil
                               :transform (make-transformation 1 0 0 -1 (- dleft) dbottom))
          (loop for (function gray) in pieces
                do (mcclim-raster-image:with-output-to-raster-image-file (stream "/tmp/bla.png"
                                                                                 :width (- dright dleft)
                                                                                 :height (- dbottom dtop)
                                                                                 :border-width 0)
                     (with-translation (stream 5 5)
                       (let ((*standard-output* stream))
                         (funcall function))))
                when gray
                do (let* ((raster (mcclim-raster-image:with-output-to-rgba-pattern
                                      (stream :width (- dright dleft)
                                              :height (- dbottom dtop)
                                              :border-width 0)
                                    (with-translation (stream 5 5)
                                      (let ((*standard-output* stream))
                                        (break "~A" stream)
                                        (funcall function)))))
                          (width (pattern-width raster))
                          (height (pattern-height raster))
                          (array (climi::pattern-array raster)))
                     (loop for j below height
                           for y from dtop
                           do (loop for i below width
                                    for x from dleft
                                    when (< (print (ldb (byte 8 8) (print (aref array i j)))) 1)
                                    do   (draw-rectangle* stream x y (1+ x) (1+ y)
                                                          :ink (make-gray-color gray))))))
          (loop for (function gray) in pieces
                when (null gray)
                do   (let ((*standard-output* stream))
                       (funcall function)))

          (loop for (x y) in control-points
                do (draw-circle* stream x y (/ 2 scale)))

          (let ((character-style (make-text-style nil nil (cond ((< scale 10) :tiny)
                                                                ((< scale 25) :small)
                                                                (t nil))))
                (text-margin (/ 4 scale)))

            (draw-rectangle* stream dleft dtop dright dbottom
                             :line-thickness 2 :filled nil :line-joint-shape :round)
            (loop for x from dleft to dright
                  do (draw-line* stream x dtop x dbottom)
                     (draw-text* stream (format nil "~D" x) x dtop
                                 :text-style character-style
                                 :align-x :center :align-y :bottom))
            (loop for y from dtop to dbottom
                  do (draw-line* stream dleft y dright y)
                     (draw-text* stream (format nil "~D" y) (- dleft text-margin) y
                                 :text-style character-style
                                 :align-x :right :align-y :center)))

          (when show-centers
            (loop for x from dleft below dright
                  do (loop for y from dtop below dbottom
                           do (draw-point* stream (+ x .5) (+ y .5))))))))))

(defmacro with-postscript-stream ((stream pathname) &body body)
  `(with-postscript-stream-1 ,pathname #'(lambda (,stream) ,@body)))

(defun with-postscript-stream-1 (pathname continuation)
  (let ((edges nil))
    #+eps-bbox
    (let ((stream *standard-output*))
      (send stream :clear-history)
      (funcall continuation stream)
      (let ((left nil)
            (top nil)
            (right nil)
            (bottom nil))
        (flet ((compute-box (presentations)
                 (map nil #'(lambda (presentation)
                              (multiple-value-bind (ll tt rr bb)
                                  (dw:box-edges (dw:presentation-displayed-box presentation))
                                (dw::minf-or left ll)
                                (dw::minf-or top  tt)
                                (dw::maxf-or right  rr)
                                (dw::maxf-or bottom bb)))
                      presentations)))
          (compute-box (send stream :displayed-presentations))
          (compute-box (send stream :displayed-strings))
          (compute-box (send stream :displayed-graphics)))
        (setq edges (list left top right bottom))))
    (with-open-stream (stream (hardcopy:make-hardcopy-stream (list :file pathname)))
      #+eps-bbox (send stream :set-eps-bounding-box edges)
      (funcall continuation stream))))

(defun two-triangles (&optional (path #p"two-triangles.ps"))
  (clim-postscript:with-output-to-postscript-stream (*standard-output* path)
    (flet ((t1 ()
             (draw-triangle* t 0 0 0 5 5 5 :filled nil))
           (t2 ()
             (draw-triangle* t 0 0 5 0 5 5 :filled nil))
           (t1f ()
             (draw-triangle* t 0 0 0 5 5 5))
           (t2f ()
             (draw-triangle* t 0 0 5 0 5 5)))
      (with-translation (t 50 0)
        (show-scan-conversion `((,#'t1) (,#'t1f .25))))
      (with-translation (t 250 0)
        (show-scan-conversion `((,#'t2) (,#'t2f .25)))))))

(defun corner-circle (&optional (path #p"corner-circle.ps"))
  (with-open-file (stream path :direction :output
                               :if-does-not-exist :create
                               :if-exists :supersede)
    (clim-postscript:with-output-to-postscript-stream (stream stream)
      (labels ((outline ()
                 (draw-circle* *standard-output* 0.5 0.5 4 :filled nil :ink +black+))
               (point-f (x y)
                 (- (+ (expt x 2) (expt y 2)) (expt 4 2)))
               (inside-points ()
                 (loop for y from -5 to +5
                       do (loop for x from -5 to +5
                                when (minusp (point-f x y))
                                do (draw-point* *standard-output* x y :ink +black+))))
               (border-points ()
                 (loop for y from -5 to +5
                       do (loop for x from -5 to +5
                                when (zerop (point-f x y))
                                do (draw-point* *standard-output* x y :line-thickness .1 :ink +black+)))))
        (with-room-for-graphics (stream)
          (show-scan-conversion `((,#'outline) (,#'inside-points .5) (,#'border-points .25))
                                :scale 15 :control-points '((0.5 0.5)) :edges '(-5 -5 +5 +5)
                                :stream stream))))))

(defun inscribed-circle (&optional (path #p"sys:clim;rel-2;specs;inscribed-circle.ps"))
  (with-postscript-stream (*standard-output* path)
    (labels ((outline ()
               (graphics:draw-circle 0 0 4 :filled nil)
               (graphics:draw-rectangle -4 -4 +4 +4 :filled nil))
             (outline2 ()
               (graphics:draw-circle 0.5 0.5 4.5 :filled nil)
               (graphics:draw-rectangle -4 -4 +4 +4 :filled nil))
             (square ()
               (graphics:draw-rectangle -4 -4 +4 +4))
             (slice (width height x y)
               (graphics:draw-rectangle x y (+ x width) (+ y height)))
             (circle ()
               (graphics:draw-circle-driver 0 0 4 #'slice))
             (circle2 ()
               (graphics:draw-circle-driver 0.5 0.5 4.5 #'slice)))
      (graphics:with-graphics-translation (t 50 0)
        (show-scan-conversion `((,#'outline) (,#'square .25) (,#'circle .5))
                              :scale 15 :edges '(-5 -5 +5 +5)))
      (graphics:with-graphics-translation (t 250 0)
        (show-scan-conversion `((,#'outline2) (,#'square .25) (,#'circle2 .5))
                              :scale 15 :edges '(-5 -5 +5 +5))))))

(defun correct-circle (&optional (path #p"correct-circle.ps"))
  (with-open-file (stream path :direction :output
                               :if-does-not-exist :create
                               :if-exists :supersede)
    (clim-postscript:with-output-to-postscript-stream (stream stream)
      (labels ((outline ()
                 (draw-circle* *standard-output* 0 0 4 :filled nil))
               (slice (width height x y)
                 (draw-rectangle* *standard-output* x y (+ x width) (+ y height)))
               (circle ()
                 (draw-circle-driver *standard-output* 0 0 4 #'slice)))
        (with-translation (stream 50 0)
          (let ((*standard-output* stream))
            (show-scan-conversion `((,#'outline) (,#'circle .5))
                                  :scale 15 :control-points '((0 0)) :edges '(-5 -5 +5 +5))))))))

#+ignore
(defun offset-circles (&optional (path #p"sys:clim;rel-2;specs;offset-circles.ps"))
  (with-postscript-stream (*standard-output* path)
    (labels ((outline ()
               (graphics:draw-circle 0.5 0.5 4.5 :filled nil))
             (outline2 ()
               (graphics:draw-circle 0.25 0.25 4.25 :filled nil))
             (slice (width height x y)
               (graphics:draw-rectangle x y (+ x width) (+ y height)))
             (circle ()
               (graphics:draw-circle-driver 0.5 0.5 4.5 #'slice))
             (circle2 ()
               (graphics:draw-circle-driver 0.25 0.25 4.25 #'slice)))
      (graphics:with-graphics-translation (t 50 0)
        (show-scan-conversion `((,#'outline) (,#'circle .5))
                              :scale 15 :control-points '((0.5 0.5)) :edges '(-5 -5 +5 +5)))
      (graphics:with-graphics-translation (t 250 0)
        (show-scan-conversion `((,#'outline2) (,#'circle2 .5))
                              :scale 15 :control-points '((0.25 0.25)) :edges '(-5 -5 +5 +5))))))

#+ignore
(defun unfilled-circles (&optional (path #p"sys:clim;rel-2;specs;unfilled-circles.ps"))
  (with-postscript-stream (*standard-output* path)
    (labels ((outline ()
               (graphics:draw-circle 0 0 4.5 :filled nil)
               (graphics:draw-circle 0 0 3.5 :filled nil))
             (outline2 ()
               (graphics:draw-circle 0.5 0.5 4.5 :filled nil)
               (graphics:draw-circle 0.5 0.5 3.5 :filled nil))
             (slice (width height x y)
               (graphics:draw-rectangle x y (+ x width) (+ y height)))
             (circle ()
               (graphics:draw-circular-ring-driver 0 0 3.5 4.5 #'slice))
             (circle2 ()
               (send *standard-output* :draw-circle 0 0 4)
               #+ignore
               (graphics:draw-circular-ring-driver 0.5 0.5 3.5 4.5 #'slice)))
      (graphics:with-graphics-translation (t 50 0)
        (show-scan-conversion `((,#'outline) (,#'circle .5))
                              :scale 15 :control-points '((0 0)) :edges '(-5 -5 +5 +5)))
      (graphics:with-graphics-translation (t 250 0)
        (show-scan-conversion `((,#'outline2) (,#'circle2 .5))
                              :scale 15 :control-points '((0.5 0.5)) :edges '(-5 -5 +5 +5))))))

(defun thin-lines (&optional (path #p"sys:clim;rel-2;specs;thin-lines.ps"))
  (with-postscript-stream (*standard-output* path)
    (labels ((outline ()
               (graphics:with-graphics-rotation (t (atan 3 4))
                 (graphics:draw-rectangle -.5 -.5 5.5 +.5 :filled nil)))
             (slice (width height x y)
               (graphics:draw-rectangle x y (+ x width) (+ y height)))
             (line ()
               (graphics:draw-line-driver 0 0 4 3 t #'slice))
             (rect ()
               (graphics:with-graphics-rotation (t (atan 3 4))
                 (graphics:draw-rectangle -.5 -.5 5.5 +.5))))
      (graphics:with-graphics-translation (t 50 0)
        (show-scan-conversion `((,#'outline) (,#'line .5))))
      (graphics:with-graphics-translation (t 250 0)
        (show-scan-conversion `((,#'outline) (,#'rect .5)))))))

(defun thick-lines (&optional (path #p"sys:clim;rel-2;specs;thick-lines.ps"))
  (with-postscript-stream (*standard-output* path)
    (labels ((outline ()
               (graphics:with-graphics-rotation (t (atan 3 4))
                 (graphics:draw-rectangle 0 -1 5 1 :filled nil)))
             (rounded-outline ()
               (graphics:draw-polygon '(1 -1 -1 1 3 4 5 2) :filled nil))
             (exact ()
               (graphics:with-graphics-rotation (t (atan 3 4))
                 (graphics:draw-rectangle 0 -1 5 1)))
             (rounded ()
               (send *standard-output* :draw-rectangle 6 6 -1 -1 :erase)
               (send *standard-output* :draw-wide-curve '#(0 4) '#(0 3) 2)))
      (graphics:with-graphics-translation (t 50 0)
        (show-scan-conversion `((,#'outline) (,#'exact .5))))
      (graphics:with-graphics-translation (t 250 0)
        (show-scan-conversion `((,#'rounded-outline) (,#'rounded .5)))))))

(defun horizontal-lines (&optional (path #p"sys:clim;rel-2;specs;horizontal-lines.ps"))
  (with-postscript-stream (*standard-output* path)
    (labels ((outline ()
               (graphics:draw-rectangle .5 2.5 4.5 3.5 :filled nil))
             (rect ()
               (graphics:draw-rectangle .5 2.5 4.5 3.5))
             (line ()
               (send *standard-output* :draw-line 1 3 4 3)))
      (graphics:with-graphics-translation (t 50 0)
        (show-scan-conversion `((,#'outline) (,#'line .5))))
      (graphics:with-graphics-translation (t 250 0)
        (show-scan-conversion `((,#'outline) (,#'rect .5)))))))

#+ignore
(defun line-clipping (&optional (path #p"sys:clim;rel-2;specs;clipped-lines.ps"))
  (with-postscript-stream (*standard-output* path)
    (labels ((outline ()
               (graphics:draw-line -1 -1 4 1 :thickness 4))
             (right ()
               (graphics:draw-line -1 -1 4 1)
               (send *standard-output* :draw-rectangle 1 1 -1 -1 tv:alu-andca))
             (wrong ()
               (send *standard-output* :draw-line 0 0 4 1)))
      (graphics:with-graphics-translation (t 50 0)
        (show-scan-conversion `((,#'outline) (,#'right .5))
                              :edges '(-2 -2 6 3)))
      (graphics:with-graphics-translation (t 350 0)
        (show-scan-conversion `((,#'outline) (,#'wrong .5))
                              :edges '(-2 -2 6 3))))))

#+ignore
(defun circle-and-outlines (&optional (path #p"sys:clim;rel-2;specs;outlined-circles.ps"))
  (with-postscript-stream (*standard-output* path)
    (labels ((outline1 () (graphics:draw-circle 0 0 6 :filled nil))
             (outline2 () (graphics:draw-circle 0 0 7 :filled nil)
                       (graphics:draw-circle 0 0 6 :filled nil))
             (slice (width height x y)
               (graphics:draw-rectangle x y (+ x width) (+ y height)))
             (circle ()
               (graphics:draw-circle-driver 0 0 6 #'slice))
             (ring ()
               (graphics:draw-circular-ring-driver 0 0 6 7 #'slice)))
      (graphics:with-graphics-translation (t 50 0)
        (show-scan-conversion `((,#'outline1) (,#'circle .5))
                              :scale 10 :edges '(-10 -10 +10 +10)))
      (graphics:with-graphics-translation (t 350 0)
        (show-scan-conversion `((,#'outline2) (,#'ring .5))
                              :scale 10 :edges '(-10 -10 +10 +10))))))


(defun line-cap-shapes (&optional (path #p"sys:clim;rel-2;specs;line-cap-shapes.ps"))
  (with-postscript-stream (*standard-output* path)
    (graphics:with-room-for-graphics (t 200)
      (graphics:draw-line 200 40 200 140
                          :thickness 15 :line-end-shape :butt)
      (graphics:draw-line 230 40 230 140
                          :thickness 15 :line-end-shape :no-end-point)
      (graphics:draw-line 260 40 260 140
                          :thickness 15 :line-end-shape :square)
      (graphics:draw-line 290 40 290 140
                          :thickness 15 :line-end-shape :round)
      (graphics:draw-line 190 30 300 30)
      (graphics:draw-line 190 150 300 150))))

(defun line-joint-shapes (&optional (path #p"sys:clim;rel-2;specs;line-joint-shapes.ps"))
  (with-postscript-stream (*standard-output* path)
    (graphics:with-room-for-graphics (t 200)
      (graphics:draw-regular-polygon 40 40 80 40 5
                                     :filled nil :thickness 10
                                     :line-joint-shape :miter)
      (graphics:draw-regular-polygon 150 40 190 40 5
                                     :filled nil :thickness 10
                                     :line-joint-shape :bevel)
      (graphics:draw-regular-polygon 260 40 300 40 5
                                     :filled nil :thickness 10
                                     :line-joint-shape :round)
      (graphics:draw-regular-polygon 370 40 410 40 5
                                     :filled nil :thickness 10
                                     :line-joint-shape :none))))
