(in-package #:clim-clx)

(declaim (inline round-coordinate))
(defun round-coordinate (x)
  "Function used for rounding coordinates."
  ;; When in doubt we use "round half up" rounding, instead of the CL:ROUND
  ;; "round half to even".
  ;;
  ;; Reason: As the CLIM drawing model is specified, you quite often want to
  ;; operate with coordinates, which are multiples of 1/2.  Using CL:ROUND gives
  ;; "random" results. Using "round half up" gives you more consistent results.
  ;;
  ;; Note that CLIM defines pixel coordinates to be at the corners, while in X11
  ;; they are at the centers. We don't do much about the discrepancy, but
  ;; rounding up at half pixel boundaries seems to work well.
  (etypecase x
    (integer      x)
    (single-float (values (floor (+ x .5f0))))
    (double-float (values (floor (+ x .5d0))))
    (long-float   (values (floor (+ x .5l0))))
    (ratio        (values (floor (+ x 1/2))))))

(defmacro with-round-positions ((transformation &rest coordinates) &body body)
  (destructuring-bind (x y &rest rest-coords) coordinates
    (if (null rest-coords)
        `(with-transformed-position (,transformation ,x ,y)
           (setf ,x (round-coordinate ,x)
                 ,y (round-coordinate ,y))
           ,@body)
        `(with-transformed-position (,transformation ,x ,y)
           (setf ,x (round-coordinate ,x)
                 ,y (round-coordinate ,y))
           (with-round-positions (,transformation ,@rest-coords)
             ,@body)))))

(defmacro with-round-coordinates ((transformation coordinates) &body body)
  `(with-transformed-positions (,transformation ,coordinates)
     (map-into ,coordinates #'round-coordinate ,coordinates)
     ,@body))

(defconstant +clx-clip+
  (load-time-value (make-rectangle* #x-8000 #x-8000 #x7FFF #x7FFF)))

;;; FIXME it is undefined behavior when we provide (valid) coordinates that do
;;; not fall inside the drawable region. Experiments show that rendering works
;;; fine for rectangles and breaks for polygons. -- jd 2021-04-07

(defun clipped-line (tr x1 y1 x2 y2)
  (with-round-positions (tr x1 y1 x2 y2)
    (if (and (<= #x-8000 x1 #x7FFF) (<= #x-8000 y1 #x7FFF)
             (<= #x-8000 x2 #x7FFF) (<= #x-8000 y2 #x7FFF))
        (values x1 y1 x2 y2)
        (let* ((src-line (make-line* x1 y1 x2 y2))
               (dst-line (region-intersection +clx-clip+ src-line)))
          (unless (region-equal dst-line +nowhere+)
            (with-bounding-rectangle* (x1 y1 x2 y2) dst-line
              (values (round-coordinate x1)
                      (round-coordinate y1)
                      (round-coordinate x2)
                      (round-coordinate y2))))))))

(defun clipped-rect (tr x1 y1 x2 y2)
  (with-round-positions (tr x1 y1 x2 y2)
    (let ((x1 (clamp (min x1 x2) #x-8000 #x7FFF))
          (y1 (clamp (min y1 y2) #x-8000 #x7FFF))
          (x2 (clamp (max x1 x2) #x-8000 #x7FFF))
          (y2 (clamp (max y1 y2) #x-8000 #x7FFF)))
      (values x1 y1 (- x2 x1) (- y2 y1)))))

(defun clipped-poly (tr coords closed)
  (with-round-coordinates (tr coords)
    (if (every (lambda (n) (<= #x-8000 n #x7fff)) coords)
        (if (null closed)
            coords
            (concatenate 'vector coords
                         (vector (elt coords 0) (elt coords 1))))
        (let* ((src-poly (make-polygon* coords))
               (dst-poly (region-intersection +clx-clip+ src-poly)))
          (flet ((disassemble-polygon (polygon)
                   (climi::collect (result)
                     (map-over-polygon-coordinates
                      (lambda (x y)
                        (result (round-coordinate x)
                                (round-coordinate y)))
                      polygon)
                     (when closed
                       (let ((point (elt (polygon-points polygon) 0)))
                         (result (round-coordinate (point-x point))
                                 (round-coordinate (point-y point)))))
                     (result))))
            (etypecase dst-poly
              (polygon
               (disassemble-polygon dst-poly))
              (climi::nowhere-region
               nil)
              (standard-region-union
               (loop for r in (region-set-regions dst-poly :normalize t)
                     collect (disassemble-polygon r) into result
                     finally (return (values result t))))))))))
