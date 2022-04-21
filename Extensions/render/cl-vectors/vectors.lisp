(in-package #:mcclim-render)

;;; path utility

(defun make-path (x y)
  (let ((path (paths:create-path :open-polyline)))
    (paths:path-reset path (paths:make-point x y))
    path))

(defun line-to (path x y)
  (paths:path-extend path (paths:make-straight-line)
                     (paths:make-point x y)))

(defun close-path (path )
  (setf (paths::path-type path) :closed-polyline))

(defun stroke-path (path line-style medium)
  (let* ((effective-thickness (line-style-effective-thickness line-style medium))
         (effective-dashes (line-style-effective-dashes line-style medium))
         (dashes-vector (typecase effective-dashes
                          (null nil)
                          (sequence (coerce effective-dashes 'vector))))
         (joint-shape (line-style-joint-shape line-style))
         (cap-shape (line-style-cap-shape line-style)))
    (when dashes-vector
      (setf path (paths:dash-path path dashes-vector)))
    (paths:stroke-path path (max 1 effective-thickness)
                       :joint (if (eq joint-shape :bevel)
                                  :none
                                  joint-shape)
                       :caps (if (eq cap-shape :no-end-point)
                                 :butt
                                 cap-shape))))

(defun aa-cells-sweep/rectangle (image ink state clip-region)
  (let* ((complex-clip-region (if (rectanglep clip-region)
                                  nil
                                  clip-region))
         (draw-function (if (typep ink 'standard-flipping-ink)
                            (aa-render-xor-draw-fn image complex-clip-region ink)
                            (aa-render-draw-fn image complex-clip-region ink))))
    (with-bounding-rectangle* (min-x min-y max-x max-y) clip-region
      (%aa-cells-sweep/rectangle state
                                 (floor min-x)
                                 (floor min-y)
                                 (ceiling max-x)
                                 (ceiling max-y)
                                 draw-function))))

(defun aa-cells-alpha-sweep/rectangle (image state clip-region)
  (let ((draw-function (aa-render-alpha-draw-fn image (if (rectanglep clip-region)
                                                          nil
                                                          clip-region))))
    (with-bounding-rectangle* (min-x min-y max-x max-y) clip-region
      (%aa-cells-sweep/rectangle state
                                 (floor min-x)
                                 (floor min-y)
                                 (ceiling max-x)
                                 (ceiling max-y)
                                 draw-function))))

(defun aa-stroke-paths (medium image design paths line-style state transformation clip-region)
  (vectors::state-reset state)
  (let ((paths (car (mapcar (lambda (path)
                              (stroke-path path line-style medium))
                            paths))))
    (aa-update-state state paths transformation)
    (aa-cells-sweep/rectangle image design state clip-region)))

(defun aa-fill-paths (image design paths state transformation clip-region)
  (vectors::state-reset state)
  (dolist (path paths)
    (setf (paths::path-type path) :closed-polyline))
  (aa-update-state state paths transformation)
  (aa-cells-sweep/rectangle image design state clip-region))

(defun %aa-scanline-sweep (scanline function start end)
  "Call FUNCTION for each pixel on the polygon covered by
SCANLINE. The pixels are scanned in increasing X. The sweep can
be limited to a range by START (included) or/and END (excluded)."
  (declare (optimize speed (debug 0) (safety 0) (space 2))
           (type (function (fixnum fixnum fixnum) *) function)
           (type fixnum start end))
  (let* ((x-min (max start (aa::cell-x (first scanline))))
         (x-max x-min)
         (cover 0)
         (y (aa::scanline-y scanline))
         (cells scanline)
         (last-x nil))
    (declare (type fixnum x-min x-max)
             (type (or null fixnum) last-x))
    ;; Skip initial cells that are before START.
    (loop while (and cells (< (aa::cell-x (car cells)) start))
          do (incf cover (aa::cell-cover (car cells)))
             (setf last-x (aa::cell-x (car cells))
                   cells (cdr cells)))
    (dolist (cell cells)
      (let ((x (aa::cell-x cell)))
        (when (and last-x (> x (1+ last-x)))
          (let ((alpha (aa::compute-alpha cover 0)))
            (unless (zerop alpha)
              (let ((start-x (max start (1+ last-x)))
                    (end-x   (min end x)))
                (maxf x-max end-x)
                (loop for ix from start-x below end-x
                      do (funcall function ix y alpha))))))
        (when (>= x end)
          (return (values x-min x-max)))
        (incf cover (aa::cell-cover cell))
        (let ((alpha (aa::compute-alpha cover (aa::cell-area cell))))
          (unless (zerop alpha)
            (funcall function x y alpha)))
        (setf last-x x)))
    (values x-min x-max)))

(defun %aa-cells-sweep/rectangle (state x1 y1 x2 y2 function)
  "Call FUNCTION for each pixel on the polygon described by
previous call to LINE or LINE-F. The pixels are scanned in
increasing Y, then on increasing X. This is limited to the
rectangle region specified with (X1,Y1)-(X2,Y2) (where X2 must be
greater than X1 and Y2 must be greater than Y1, to describe a
non-empty region.)"
  (let ((scanlines (aa::freeze-state state))
        (x-min x2)
        (x-max x1)
        (y-min y2)
        (y-max y1))
    (dolist (scanline scanlines)
      (let ((y (aa::scanline-y scanline)))
        (when (<= y1 y (1- y2))
          (minf y-min y)
          (maxf y-max y)
          (multiple-value-bind (xa xb)
              (%aa-scanline-sweep scanline function x1 x2)
            (minf x-min xa)
            (maxf x-max xb)))))
    (make-rectangle* x-min y-min x-max y-max)))

(declaim (inline aa-line-f))
(defun aa-line-f (state mxx mxy myx myy tx ty x1 y1 x2 y2)
  (declare (type coordinate mxx mxy myx myy tx ty))
  (let ((x1 (+ (* mxx x1) (* mxy y1) tx))
        (y1 (+ (* myx x1) (* myy y1) ty))
        (x2 (+ (* mxx x2) (* mxy y2) tx))
        (y2 (+ (* myx x2) (* myy y2) ty)))
    (aa::line-f state x1 y1 x2 y2)))

(defun aa-update-state (state paths transformation)
  (multiple-value-bind (mxx mxy myx myy tx ty)
      (climi::get-transformation transformation)
    (if (listp paths)
        (dolist (path paths)
          (%aa-update-state state path mxx mxy myx myy tx ty))
        (%aa-update-state state paths mxx mxy myx myy tx ty))))

(defun %aa-update-state (state paths mxx mxy myx myy tx ty)
  (let ((iterator (vectors::path-iterator-segmented paths)))
    (multiple-value-bind (i1 k1 e1) (vectors::path-iterator-next iterator)
      (declare (ignore i1))
      (when (and k1 (not e1))
        ;; at least 2 knots
        (let ((first-knot k1))
          (loop
             (multiple-value-bind (i2 k2 e2) (vectors::path-iterator-next iterator)
               (declare (ignore i2))
               (aa-line-f state mxx mxy myx myy tx ty
                              (vectors::point-x k1) (vectors::point-y k1)
                              (vectors::point-x k2) (vectors::point-y k2))
               (setf k1 k2)
               (when e2
                 (return))))
          (aa-line-f state mxx mxy myx myy tx ty
                         (vectors::point-x k1) (vectors::point-y k1)
                         (vectors::point-x first-knot) (vectors::point-y first-knot)))))
    state))
