(in-package #:clim-clx)

(declaim (inline round-coordinate))
(defun round-coordinate (x)
  "Function used for rounding coordinates."
  ;; We use "mercantile rounding", instead of the CL round to nearest
  ;; even number, when in doubt.
  ;;
  ;; Reason: As the CLIM drawing model is specified, you quite often
  ;; want to operate with coordinates, which are multiples of 1/2.
  ;; Using CL:ROUND gives you "random" results. Using "mercantile
  ;; rounding" gives you consistent results.
  ;;
  ;; Note that CLIM defines pixel coordinates to be at the corners,
  ;; while in X11 they are at the centers. We don't do much about the
  ;; discrepancy, but rounding up at half pixel boundaries seems to
  ;; work well.
  (floor (+ x .5)))

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
          (unless (region-equal dst-poly +nowhere+)
            (climi::collect (result)
              (map-over-polygon-coordinates
               (lambda (x y)
                 (result (round-coordinate x)
                         (round-coordinate y)))
               dst-poly)
              (when closed
                (let ((point (elt (polygon-points dst-poly) 0)))
                  (result (round-coordinate (point-x point))
                          (round-coordinate (point-y point)))))
              (result)))))))
