(in-package #:climi)


;;; utilities
(defun region-exclusive-or (a b)
  (region-union (region-difference a b) (region-difference b a)))


(defmethod region-union ((a bounding-rectangle) (b bounding-rectangle))
  (make-instance 'standard-region-union :regions (list a b)))

;;; dimensionally rule
(defmethod region-union ((a area) (b path)) a)
(defmethod region-union ((a path) (b point)) a)
(defmethod region-union ((a area) (b point)) a)
(defmethod region-union ((a path) (b area)) b)
(defmethod region-union ((a point) (b path)) b)
(defmethod region-union ((a point) (b area)) b)

;;; points
(defmethod region-union ((a point) (b point))
  (cond ((region-equal a b)
         a)
        (t
         (make-instance 'standard-region-union :regions (list a b)))))

;;; paths
(defmethod region-union ((a standard-line) (b standard-line))
  (multiple-value-bind (x1 y1) (line-start-point* a)
    (multiple-value-bind (x2 y2) (line-end-point* a)
      (multiple-value-bind (u1 v1) (line-start-point* b)
        (multiple-value-bind (u2 v2) (line-end-point* b)
          (cond ((and (coordinate= 0 (geraden-gleichung x1 y1 x2 y2 u1 v1))
                      (coordinate= 0 (geraden-gleichung x1 y1 x2 y2 u2 v2)))
                 (let ((k1 (position->geraden-fktn-parameter x1 y1 x2 y2 u1 v1))
                       (k2 (position->geraden-fktn-parameter x1 y1 x2 y2 u2 v2)))
                   (psetq k1 (min k1 k2)
                          k2 (max k1 k2))
                   (cond ((and (<= k1 1) (>= k2 0))
                          (let ((k1 (min 0 k1))
                                (k2 (max 1 k2)))
                            (make-line* (+ x1 (* k1 (- x2 x1))) (+ y1 (* k1 (- y2 y1)))
                                        (+ x1 (* k2 (- x2 x1))) (+ y1 (* k2 (- y2 y1))))))
                         (t
                          (make-instance 'standard-region-union :regions (list a b))))))
                ((and (coordinate= x1 u1) (coordinate= y1 v1))
                 (make-polyline* (list u2 v2 x1 y1 x2 y2)))
                ((and (coordinate= x2 u2) (coordinate= y2 v2))
                 (make-polyline* (list x1 y1 x2 y2 u1 v1)))
                ((and (coordinate= x1 u2) (coordinate= y1 v2))
                 (make-polyline* (list u1 v1 x1 y1 x2 y2)))
                ((and (coordinate= x2 u1) (coordinate= y2 v1))
                 (make-polyline* (list x1 y1 x2 y2 u2 v2)))
                (t
                 (make-instance 'standard-region-union :regions (list a b)))))))))

(defmethod region-union ((a standard-polyline) (b standard-polyline))
  (with-slots ((a-points points)) a
    (with-slots ((b-points points)) b
      (cond ((polyline-closed a)
             (make-instance 'standard-region-union :regions (list a b)))
            ((polyline-closed b)
             (make-instance 'standard-region-union :regions (list a b)))
            ((region-equal (car a-points) (car b-points))
             (make-polyline (append (reverse (cdr a-points)) b-points)))
            ((region-equal (car (last a-points)) (car (last b-points)))
             (make-polyline (append a-points (reverse (cdr b-points)))))
            ((region-equal (car a-points) (car (last b-points)))
             (make-polyline (append b-points (cdr a-points))))
            ((region-equal (car (last a-points)) (car b-points))
             (make-polyline (append a-points (cdr b-points))))
            (t
             (make-instance 'standard-region-union :regions (list a b)))))))

(defmethod region-union ((a standard-polyline) (b standard-line))
  (with-slots (points) a
    (cond ((polyline-closed a)
           (make-instance 'standard-region-union :regions (list a b)))
          ((region-equal (car points) (line-end-point b))
           (make-polyline (cons (line-start-point b) points)))
          ((region-equal (car points) (line-start-point b))
           (make-polyline (cons (line-end-point b) points)))
          ((region-equal (car (last points)) (line-end-point b))
           (make-polyline (append points (list (line-start-point b)))))
          ((region-equal (car (last points)) (line-start-point b))
           (make-polyline (append points (list (line-end-point b)))))
          (t
           (make-instance 'standard-region-union :regions (list a b))))))

(defmethod region-union ((a standard-line) (b standard-polyline))
  (region-union b a))

;;; areas
(defmethod region-union ((xs standard-rectangle) (ys standard-rectangle))
  (region-union (rectangle->standard-rectangle-set xs)
                (rectangle->standard-rectangle-set ys)))

(defmethod region-union ((a standard-polygon) (b standard-polygon))
  (polygon-op a b #'logior))

(defmethod region-union ((a standard-polygon) (b standard-rectangle))
  (polygon-op a b #'logior))

(defmethod region-union ((a standard-rectangle) (b standard-polygon))
  (polygon-op a b #'logior))


;;; IHMO the CLIM dimensionality rule is brain dead! --gb

(defmethod region-intersection ((a bounding-rectangle) (b bounding-rectangle))
  (make-instance 'standard-region-intersection :regions (list a b)))

;;; points
(defmethod region-intersection ((a bounding-rectangle) (p point))
  (multiple-value-bind (x y) (point-position p)
    (if (region-contains-position-p a x y)
        p
        +nowhere+)))

(defmethod region-intersection ((p point) (a bounding-rectangle))
  (region-intersection a p))

(defmethod region-intersection ((a point) (b point))
  (cond
    ((region-equal a b) a)
    (t +nowhere+)))

;;; paths
(defmethod region-intersection ((a standard-polyline) (b bounding-rectangle))
  (let ((res +nowhere+))
    ;; hack alert
    (map-over-polygon-segments
     (lambda (x1 y1 x2 y2)
       (setf res
             (region-union
              res (region-intersection (make-line* x1 y1 x2 y2) b))))
     a)
    res))

(defmethod region-intersection ((b bounding-rectangle) (a standard-polyline))
  (region-intersection a b))

(defmethod region-intersection ((a standard-line) (b standard-line))
  (multiple-value-bind (x1 y1) (line-start-point* a)
    (multiple-value-bind (x2 y2) (line-end-point* a)
      (multiple-value-bind (u1 v1) (line-start-point* b)
        (multiple-value-bind (u2 v2) (line-end-point* b)
          (multiple-value-bind (r sx1 sy1 sx2 sy2)
              (line-intersection* x1 y1 x2 y2 u1 v1 u2 v2)
            (case r
              (:hit (make-point sx1 sy1))
              (:coincident (make-line* sx1 sy1 sx2 sy2))
              ((nil) +nowhere+))))))))

;;; paths/areas
(defmethod region-intersection ((line line) (ellipse standard-ellipse))
  (let (p1x p1y p2x p2y)
    (multiple-value-setq (p1x p1y) (line-start-point* line))
    (multiple-value-setq (p2x p2y) (line-end-point* line))
    (let ((region (if (and (region-contains-position-p ellipse p1x p1y)
                           (region-contains-position-p ellipse p2x p2y))
                      line
                      (multiple-value-bind (x1 y1 x2 y2)
                          (cond ((= p1x p2x) (intersection-vline/ellipse ellipse p1x))
                                ((= p1y p2y) (intersection-hline/ellipse ellipse p1y))
                                (t (intersection-line/ellipse ellipse p1x p1y p2x p2y)))
                        (if (some #'complexp (list x1 y1 x2 y2))
                            +nowhere+
                            (make-line* x1 y1 x2 y2))))))
      (with-slots (start-angle end-angle) ellipse
        (when (or (null start-angle) (region-equal region +nowhere+))
          (return-from region-intersection region))
        (multiple-value-bind (cx cy) (ellipse-center-point* ellipse)
          (multiple-value-bind (sx sy) (%ellipse-angle->position ellipse start-angle)
            (multiple-value-bind (ex ey) (%ellipse-angle->position ellipse end-angle)
              (let* ((start-ray (make-line* cx cy sx sy))
                     (end-ray (make-line* cx cy ex ey))
                     (si (region-intersection region start-ray))
                     (ei (region-intersection region end-ray))
                     (sip (not (region-equal +nowhere+ si)))
                     (eip (not (region-equal +nowhere+ ei)))
                     (p1 (line-start-point region))
                     (p2 (line-end-point region))
                     (p1p (multiple-value-call
                              #'region-contains-position-p ellipse (point-position p1)))
                     (p2p (multiple-value-call
                              #'region-contains-position-p ellipse (point-position p2))))
                (cond
                  ;; line goes through the center. Only in this case line may be
                  ;; coincident with angle rays, so we don't have to bother with
                  ;; checking later.
                  ((region-contains-position-p region cx cy)
                   (make-line (if p1p p1 (make-point cx cy))
                              (if p2p p2 (make-point cx cy))))
                  ;; line doesn't intersect any of angle rays
                  ((and (not sip) (not eip))
                   ;; p1p implies p2p here, but rounding may say otherwise
                   (if (or p1p p2p) region +nowhere+))
                  ;; line intersects with both angle rays
                  ((and sip eip)
                   ;; region difference may not work here due to float rounding
                   (let ((guess-line (make-line p1 si)))
                     (let ((intersection-line
                             (if (not (region-intersects-region-p guess-line end-ray))
                                 (region-union guess-line (make-line p2 ei))
                                 (region-union (make-line p1 ei) (make-line p2 si)))))
                       intersection-line)))
                  ;; line intersect only one angle ray
                  (t (make-line (if p1p p1 p2)
                                (if sip si ei))))))))))))

(defmethod region-intersection ((ellipse standard-ellipse) (line standard-line))
  (region-intersection line ellipse))

(defmethod region-intersection ((b polygon) (a line))
  (multiple-value-bind (x1 y1) (line-start-point* a)
    (multiple-value-bind (x2 y2) (line-end-point* a)
      (intersection-segment/polygon x1 y1 x2 y2 b))))

(defmethod region-intersection ((a line) (b polygon))
  (region-intersection b a))

;;; areas
(defmethod region-intersection ((xr rectangle) (yr rectangle))
  (region-intersection (rectangle->standard-rectangle-set xr)
                       (rectangle->standard-rectangle-set yr)))

(defmethod region-intersection ((a standard-polygon) (b standard-polygon))
  (polygon-op a b #'logand))

(defmethod region-intersection ((a standard-polygon) (b standard-rectangle))
  (polygon-op a b #'logand))

(defmethod region-intersection ((a standard-rectangle) (b standard-polygon))
  (polygon-op a b #'logand))


(defmethod region-difference ((x bounding-rectangle) (y bounding-rectangle))
  (make-instance 'standard-region-difference :a x :b y))

;;; dimensionality rule
(defmethod region-difference ((x area) (y path)) x)
(defmethod region-difference ((x area) (y point)) x)
(defmethod region-difference ((x path) (y point)) x)

;;; points
(defmethod region-difference ((x point) (y bounding-rectangle))
  (multiple-value-bind (px py) (point-position x)
    (if (region-contains-position-p y px py)
        +nowhere+
        x)))

;;; paths
(defmethod region-difference ((a standard-polyline) (b bounding-rectangle))
  (let ((res +nowhere+))
    (map-over-polygon-segments
     (lambda (x1 y1 x2 y2)
       (setf res
             (region-union
              res (region-difference (make-line* x1 y1 x2 y2) b))))
     a)
    res))

(defmethod region-difference ((a bounding-rectangle) (b standard-polyline))
  (map-over-polygon-segments
   (lambda (x1 y1 x2 y2)
     (setf a (region-difference a (make-line* x1 y1 x2 y2))))
   b)
  a)

(defmethod region-difference ((a standard-line) (b standard-line))
  (multiple-value-bind (x1 y1) (line-start-point* a)
    (multiple-value-bind (x2 y2) (line-end-point* a)
      (multiple-value-bind (u1 v1) (line-start-point* b)
        (multiple-value-bind (u2 v2) (line-end-point* b)
          (cond ((and (coordinate= 0 (geraden-gleichung x1 y1 x2 y2 u1 v1))
                      (coordinate= 0 (geraden-gleichung x1 y1 x2 y2 u2 v2)))
                 (let ((k1 (position->geraden-fktn-parameter x1 y1 x2 y2 u1 v1))
                       (k2 (position->geraden-fktn-parameter x1 y1 x2 y2 u2 v2)))
                   (psetq k1 (max 0 (min k1 k2))
                          k2 (min 1 (max k1 k2)))
                   (let ((r (nconc (if (> k1 0)
                                       (list (make-line* x1
                                                         y1
                                                         (+ x1 (* k1 (- x2 x1)))
                                                         (+ y1 (* k1 (- y2 y1)))))
                                       nil)
                                   (if (< k2 1)
                                       (list (make-line* (+ x1 (* k2 (- x2 x1)))
                                                         (+ y1 (* k2 (- y2 y1)))
                                                         x2
                                                         y2))
                                       nil))))
                     (cond ((null r) +nowhere+)
                           ((null (cdr r)) (car r))
                           (t (make-instance 'standard-region-union :regions r))))))
                (t
                 a)))))))

;;; paths/areas
(defmethod region-difference ((a line) (b polygon))
  (multiple-value-bind (x1 y1) (line-start-point* a)
    (multiple-value-bind (x2 y2) (line-end-point* a)
      (difference-segment/polygon x1 y1 x2 y2 b))))

;;; areas
(defmethod region-difference ((xs standard-rectangle) (ys standard-rectangle))
  (region-difference (rectangle->standard-rectangle-set xs)
                     (rectangle->standard-rectangle-set ys)))

(defmethod region-difference ((a standard-polygon) (b standard-polygon))
  (polygon-op a b #'logandc2))

(defmethod region-difference ((a standard-polygon) (b standard-rectangle))
  (polygon-op a b #'logandc2))

(defmethod region-difference ((a standard-rectangle) (b standard-polygon))
  (polygon-op a b #'logandc2))
