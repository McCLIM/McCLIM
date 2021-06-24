;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998-2001 Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 1998-1999 Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 2000 Iban Hatchondo <hatchond@emi.u-bordeaux.fr>
;;;  (c) copyright 2000 Julien Boninfante <boninfan@emi.u-bordeaux.fr>
;;;  (c) copyright 2000,2014 Robert Strandh <robert.strandh@gmail.com>
;;;  (c) copyright 2021 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; This file implements "legacy" x11 drawing routines. We are not concerned
;;; with the ink - we expect the gc to be created beforehand. Functions defined
;;; in this file are responsible for transforming and rounding coordinates to
;;; values acceptable by clx.

(in-package #:clim-clx)

(defun clx-draw-point (mi gc tr x y)
  (with-round-positions (tr x y)
    (when (and (typep x 'clx-coordinate)
               (typep y 'clx-coordinate))
      (xlib:draw-point mi gc x y))))

(defun clx-draw-line (mi gc tr x1 y1 x2 y2)
  (with-clipped-line (tr x1 y1 x2 y2)
    (xlib:draw-line mi gc x1 y1 x2 y2)))

(defun clx-draw-polygon (mi gc tr coords closed filled)
  (with-clipped-poly (tr coords closed filled)
    (xlib:draw-lines mi gc coords :fill-p filled)))

(defun clx-draw-rectangle (mi gc tr x1 y1 x2 y2 filled)
  (with-clipped-rect (tr x1 y1 x2 y2)
    (xlib:draw-rectangle mi gc x1 y1 (- x2 x1) (- y2 y1) filled)))

(defun clx-draw-ellipse (mi gc tr
                         center-x center-y rdx1 rdy1 rdx2 rdy2
                         start-angle end-angle filled)
  (with-transformed-distance (tr rdx1 rdy1)
    (with-transformed-distance (tr rdx2 rdy2)
      (with-transformed-position (tr center-x center-y)
        (with-transformed-angles (tr nil start-angle end-angle)
          (if (or (= rdx2 rdy1 0) (= rdx1 rdy2 0))
              (clx-draw-aligned-ellipse mi gc
                                        center-x center-y rdx1 rdy1 rdx2 rdy2
                                        start-angle end-angle filled)
              (clx-draw-bonkers-ellipse mi gc
                                        center-x center-y rdx1 rdy1 rdx2 rdy2
                                        start-angle end-angle filled)))))))

(defun clx-draw-aligned-ellipse (mi gc center-x center-y
                                 rdx1 rdy1 rdx2 rdy2
                                 start-angle end-angle filled)
  (let ((rdx (abs (+ rdx1 rdx2)))
        (rdy (abs (+ rdy1 rdy2))))
    (let* ((x1 (round-coordinate (- center-x rdx)))
           (y1 (round-coordinate (- center-y rdy)))
           (x2 (round-coordinate (+ center-x rdx)))
           (y2 (round-coordinate (+ center-y rdy)))
           (arc-angle (- end-angle start-angle))
           (arc-angle (if (< arc-angle 0)
                          (+ (* pi 2) arc-angle)
                          arc-angle))
           (start-angle (mod start-angle (* 2 pi))))
      (when (and (typep x1 'clx-coordinate)
                 (typep y1 'clx-coordinate))
        (xlib:draw-arc mi gc x1 y1 (- x2 x1) (- y2 y1)
                       start-angle arc-angle filled)))))

;;; FIXME this code has a quality and functionality of a potato and should be
;;; replaced by drawing a polygon (after approximating the ellipse with it).
;;; Alternatively we could take a proper renderer and do it with scanlines.
;;; Alternatively (xrender-only) transform the xy-aligned ellipse picture.
(defun clx-draw-bonkers-ellipse (mi gc center-x center-y
                                 radius-1-dx radius-1-dy
                                 radius-2-dx radius-2-dy
                                 start-angle end-angle filled)
  (let ((ellipse (make-ellipse* center-x center-y
                                radius-1-dx radius-1-dy
                                radius-2-dx radius-2-dy
                                :start-angle start-angle :end-angle end-angle))
        (radius (truncate (xlib:gcontext-line-width gc) 2)))
    (multiple-value-bind (x1 y1 width height)
        (region->clipping-values (bounding-rectangle ellipse))
      (labels ((ellipse-border-p (ellipse x-orig y-orig)
                 (with-slots (climi::tr climi::start-angle climi::end-angle) ellipse
                   (multiple-value-bind (x y)
                       (untransform-position climi::tr x-orig y-orig)
                     (and (<= (- 1.0 .05) (+ (* x x) (* y y)) (+ 1.0 .05))
                          (or (null climi::start-angle)
                              (climi::arc-contains-angle-p
                               (climi::%ellipse-position->angle ellipse x-orig y-orig)
                               climi::start-angle climi::end-angle))))))
               (draw-point (x y)
                 (if (< radius 1)
                     (let ((x (round-coordinate x))
                           (y (round-coordinate y)))
                       (xlib:draw-point mi gc x y))
                     (let* ((min-x (round-coordinate (- x radius)))
                            (min-y (round-coordinate (- y radius)))
                            (max-x (round-coordinate (+ x radius)))
                            (max-y (round-coordinate (+ y radius))))
                       (xlib:draw-arc mi gc min-x min-y
                                      (- max-x min-x) (- max-y min-y)
                                      0 (* 2 pi) t))))
               (maybe-draw-border-points (line)
                 (multiple-value-bind (lx1 ly1) (line-start-point* line)
                   (when (ellipse-border-p ellipse lx1 ly1) (draw-point lx1 ly1)))
                 (multiple-value-bind (lx2 ly2) (line-end-point* line)
                   (when (ellipse-border-p ellipse lx2 ly2) (draw-point lx2 ly2))))
               (draw-line-1 (line)
                 (multiple-value-bind (lx1 ly1) (line-start-point* line)
                   (multiple-value-bind (lx2 ly2) (line-end-point* line)
                     (xlib:draw-line mi gc
                                     (round-coordinate lx1)
                                     (round-coordinate ly1)
                                     (round-coordinate lx2)
                                     (round-coordinate ly2)))))
               (draw-lines (scan-line)
                 ;; XXX: this linep masks a problem with region-intersection.
                 (when (linep scan-line)
                   (cond
                     ((region-equal scan-line +nowhere+))
                     (filled (map-over-region-set-regions #'draw-line-1 scan-line))
                     (t (map-over-region-set-regions #'maybe-draw-border-points scan-line))))))
        ;; O(n+m) because otherwise we may skip some points (better drawing quality)
        (progn ;; if (<= width height)
          (loop for x from x1 to (+ x1 width) do
            (draw-lines (region-intersection
                         ellipse
                         (make-line* x y1 x (+ y1 height)))))
          (loop for y from y1 to (+ y1 height) do
            (draw-lines (region-intersection
                         ellipse
                         (make-line* x1 y (+ x1 width) y)))))))))

#+ (or)
(defun clx-draw-bonkers-ellipse
    (mi gc center-x center-y rdx1 rdy1 rdx2 rdy2 start-angle arc-angle filled)
  (let ((coords (polygonize-ellipse center-x center-y
                                    rdx1 rdy1 rdx2 rdy2
                                    start-angle arc-angle)))
    (clx-draw-polygon mi gc +identity-transformation+ coords nil filled)))

;;; This function is different from clx-draw-ellipse in the fact that the radius
;;; is not transformed and that always a full circle is drawn.
(defun clx-draw-circle (mi gc tr x y radius filled)
  (with-transformed-position (tr x y)
    (let* ((x1 (round-coordinate (- x radius)))
           (y1 (round-coordinate (- y radius)))
           (x2 (round-coordinate (+ x radius)))
           (y2 (round-coordinate (+ y radius))))
      (when (and (typep x1 'clx-coordinate)
                 (typep y1 'clx-coordinate))
        (xlib:draw-arc mi gc x1 y1 (- x2 x1) (- y2 y1) 0 (* 2 pi) filled)))))
