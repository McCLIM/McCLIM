(in-package #:climi)


(defun rectangle-set->polygon-union (rs)
  (let ((res nil))
    (map-over-region-set-regions (lambda (r) (push r res)) rs)
    (make-instance 'standard-region-union :regions res)))

(defmethod region-exclusive-or ((a region) (b region))
  (region-union (region-difference a b) (region-difference b a)))

(defmethod region-union ((a point) (b point))
  (cond ((region-equal a b)
         a)
        (t
         (make-instance 'standard-region-union :regions (list a b)))))

(defmethod region-union ((xs standard-rectangle-set) (ys standard-rectangle-set))
  (make-standard-rectangle-set
   (bands-union (standard-rectangle-set-bands xs)
                (standard-rectangle-set-bands ys))))

(defmethod region-union ((xs standard-rectangle-set) (ys standard-rectangle))
  (region-union xs (rectangle->standard-rectangle-set ys)))

(defmethod region-union ((xs standard-rectangle) (ys standard-rectangle-set))
  (region-union (rectangle->standard-rectangle-set xs) ys))

(defmethod region-union ((xs standard-rectangle) (ys standard-rectangle))
  (region-union (rectangle->standard-rectangle-set xs)
                (rectangle->standard-rectangle-set ys)))

;;; Trivial set operations
(defmethod region-union ((a everywhere-region) (b region)) +everywhere+)
(defmethod region-union ((a region) (b everywhere-region)) +everywhere+)
(defmethod region-union ((a nowhere-region) (b region)) b)
(defmethod region-union ((a region) (b nowhere-region)) a)

;;; dimensionally rule
(defmethod region-union ((a area) (b path)) a)
(defmethod region-union ((a path) (b point)) a)
(defmethod region-union ((a area) (b point)) a)
(defmethod region-union ((a path) (b area)) b)
(defmethod region-union ((a point) (b path)) b)
(defmethod region-union ((a point) (b area)) b)


(defmethod region-union ((a standard-polygon) (b standard-polygon))
  (polygon-op a b #'logior))

(defmethod region-union ((a standard-polygon) (b standard-rectangle))
  (polygon-op a b #'logior))

(defmethod region-union ((a standard-rectangle) (b standard-polygon))
  (polygon-op a b #'logior))

(defmethod region-union ((a standard-region-union) (b nowhere-region))
  a)

(defmethod region-union ((b nowhere-region) (a standard-region-union))
  a)

(defmethod region-union ((a standard-region-union) (b region))
  (assert (not (eq b +nowhere+)))
  (make-instance 'standard-region-union
    :regions (cons b (standard-region-set-regions a))))

(defmethod region-union ((b region) (a standard-region-union))
  (assert (not (eq b +nowhere+)))
  (make-instance 'standard-region-union
    :regions (cons b (standard-region-set-regions a))))

(defmethod region-union ((a standard-region-union) (b standard-region-union))
  (assert (not (eq b +nowhere+)))
  (assert (not (eq a +nowhere+)))
  (make-instance 'standard-region-union
    :regions (append (standard-region-set-regions a)
                     (standard-region-set-regions b))))

(defmethod region-union ((a region) (b region))
  (make-instance 'standard-region-union :regions (list a b)))

(defmethod region-union ((a standard-rectangle-set) (b path)) a)
(defmethod region-union ((b path) (a standard-rectangle-set)) a)
(defmethod region-union ((a standard-rectangle-set) (b point)) a)
(defmethod region-union ((b point) (a standard-rectangle-set)) a)

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

(defmethod region-union ((a standard-rectangle-set) (b polygon))
  (region-union (rectangle-set->polygon-union a) b))

(defmethod region-union ((a polygon) (b standard-rectangle-set))
  (region-union a (rectangle-set->polygon-union b)))

(defmethod region-union ((a standard-region-difference) (b region))
  (make-instance 'standard-region-union :regions (list a b)))

(defmethod region-union ((a region) (b standard-region-difference))
  (make-instance 'standard-region-union :regions (list a b)))

(defmethod region-union ((a nowhere-region) (b nowhere-region))
  +nowhere+)


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

(defmethod region-intersection ((a point) (b point))
  (cond
    ((region-equal a b) a)
    (t +nowhere+)))

(defmethod region-intersection ((xs standard-rectangle-set) (ys standard-rectangle-set))
  (make-standard-rectangle-set
   (bands-intersection (standard-rectangle-set-bands xs)
                       (standard-rectangle-set-bands ys))))

(defmethod region-intersection ((xs standard-rectangle-set) (ys standard-rectangle))
  (region-intersection xs (rectangle->standard-rectangle-set ys)))

(defmethod region-intersection ((xs standard-rectangle) (ys standard-rectangle-set))
  (region-intersection (rectangle->standard-rectangle-set xs) ys))

(defmethod region-intersection ((xs standard-rectangle) (ys standard-rectangle))
  (region-intersection (rectangle->standard-rectangle-set xs)
                       (rectangle->standard-rectangle-set ys)))

(defmethod region-intersection ((xr rectangle) (yr rectangle))
  (region-intersection (rectangle->standard-rectangle-set xr)
                       (rectangle->standard-rectangle-set yr)))

;;; Trivial set operations
(defmethod region-intersection ((a everywhere-region) (b region)) b)
(defmethod region-intersection ((a region) (b everywhere-region)) a)
(defmethod region-intersection ((a nowhere-region) (b region)) +nowhere+)
(defmethod region-intersection ((a region) (b nowhere-region)) +nowhere+)

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

;;; IHMO the CLIM dimensionality rule is brain dead! --gb

(defmethod region-intersection ((a standard-polyline) (b region))
  (let ((res +nowhere+))
    ;; hack alert
    (map-over-polygon-segments
     (lambda (x1 y1 x2 y2)
       (setf res
             (region-union
              res (region-intersection (make-line* x1 y1 x2 y2) b))))
     a)
    res))

(defmethod region-intersection ((b region) (a standard-polyline))
  (region-intersection a b))

(defmethod region-intersection ((a region) (p point))
  (multiple-value-bind (x y) (point-position p)
    (if (region-contains-position-p a x y)
        p
      +nowhere+)))

(defmethod region-intersection ((p point) (a region))
  (region-intersection a p))

(defmethod region-intersection ((a standard-region-union) (b region))
  (let ((res +nowhere+))
    (map-over-region-set-regions
     (lambda (r) (setf res (region-union res (region-intersection r b)))) a)
    res))

(defmethod region-intersection ((a region) (b standard-region-union))
  (region-intersection b a))

(defmethod region-intersection ((a standard-rectangle-set) (b region))
  (let ((res +nowhere+))
    (map-over-region-set-regions (lambda (r) (setf res (region-union res (region-intersection r b)))) a)
    res))

(defmethod region-intersection ((a region) (b standard-rectangle-set))
  (region-intersection b a))

(defmethod region-intersection ((a region) (b standard-region-intersection))
  (map-over-region-set-regions (lambda (r) (setf a (region-intersection a r))) b)
  a)

(defmethod region-intersection ((a standard-region-intersection) (b region))
  (region-intersection b a))

(defmethod region-intersection ((a region) (b region))
  (make-instance 'standard-region-intersection :regions (list a b)))


(defmethod region-intersection ((x region) (y standard-region-difference))
  (with-slots (a b) y
    (region-difference (region-intersection x a) b)))

(defmethod region-intersection ((x standard-region-difference) (y region))
  (with-slots (a b) x
    (region-difference (region-intersection y a) b)))

(defmethod region-intersection ((a standard-polygon) (b standard-polygon))
  (polygon-op a b #'logand))

(defmethod region-intersection ((a standard-polygon) (b standard-rectangle))
  (polygon-op a b #'logand))

(defmethod region-intersection ((a standard-rectangle) (b standard-polygon))
  (polygon-op a b #'logand))

(defmethod region-intersection ((a standard-line) (b standard-polygon))
  (multiple-value-bind (x1 y1) (line-start-point* a)
    (multiple-value-bind (x2 y2) (line-end-point* a)
      (schnitt-line/polygon x1 y1 x2 y2 b))))

(defmethod region-intersection ((b standard-polygon) (a standard-line))
  (multiple-value-bind (x1 y1) (line-start-point* a)
    (multiple-value-bind (x2 y2) (line-end-point* a)
      (schnitt-line/polygon x1 y1 x2 y2 b))))

(defmethod region-intersection ((a standard-line) (b standard-rectangle))
  (multiple-value-bind (x1 y1) (line-start-point* a)
    (multiple-value-bind (x2 y2) (line-end-point* a)
      (schnitt-line/polygon x1 y1 x2 y2 b))))

(defmethod region-intersection ((b standard-rectangle) (a standard-line))
  (multiple-value-bind (x1 y1) (line-start-point* a)
    (multiple-value-bind (x2 y2) (line-end-point* a)
      (schnitt-line/polygon x1 y1 x2 y2 b))))


(defun differenz-line/polygon (x1 y1 x2 y2 polygon)
  (let ((ks (schnitt-gerade/polygon-prim x1 y1 x2 y2 (polygon-points polygon))))
    (assert (evenp (length ks)))
    (let ((res nil)
          (res2 nil))
      (push 0d0 res)
      (do ((q ks (cddr q)))
          ((null q))
        (let ((k1 (max 0d0 (min 1d0 (car q))))
              (k2 (max 0d0 (min 1d0 (cadr q)))))
          (when (/= k1 k2)
            (push k1 res)
            (push k2 res))))
      (push 1d0 res)
      (setf res (nreverse res))
      (do ((q res (cddr q)))
          ((null q))
        (let ((k1 (car q))
              (k2 (cadr q)))
          (when (/= k1 k2)
            (push (make-line* (+ x1 (* k1 (- x2 x1))) (+ y1 (* k1 (- y2 y1)))
                              (+ x1 (* k2 (- x2 x1))) (+ y1 (* k2 (- y2 y1))))
                  res2))))
      (cond ((null res2) +nowhere+)
            ((null (cdr res2)) (car res2))
            (t (make-instance 'standard-region-union :regions res2))))))

(defmethod region-difference ((xs standard-rectangle-set) (ys standard-rectangle-set))
  (make-standard-rectangle-set
   (bands-difference (standard-rectangle-set-bands xs)
                     (standard-rectangle-set-bands ys))))

(defmethod region-difference ((xs standard-rectangle-set) (ys standard-rectangle))
  (region-difference xs (rectangle->standard-rectangle-set ys)))

(defmethod region-difference ((xs standard-rectangle) (ys standard-rectangle-set))
  (region-difference (rectangle->standard-rectangle-set xs) ys))

(defmethod region-difference ((xs standard-rectangle) (ys standard-rectangle))
  (region-difference (rectangle->standard-rectangle-set xs)
                     (rectangle->standard-rectangle-set ys)))

;;; Trivial set operations
(defmethod region-difference ((a region) (b everywhere-region)) +nowhere+)   ;mit ohne alles
(defmethod region-difference ((a nowhere-region) (b region)) +nowhere+)
(defmethod region-difference ((a region) (b nowhere-region)) a)

(defmethod region-difference ((a standard-polyline) (b region))
  (let ((res +nowhere+))
    (map-over-polygon-segments
     (lambda (x1 y1 x2 y2)
       (setf res
             (region-union
              res (region-difference (make-line* x1 y1 x2 y2) b))))
     a)
    res))

(defmethod region-difference ((a region) (b standard-polyline))
  (map-over-polygon-segments
     (lambda (x1 y1 x2 y2)
       (setf a (region-difference a (make-line* x1 y1 x2 y2))))
     b)
  a)

(defmethod region-difference ((x area) (y path)) x)
(defmethod region-difference ((x area) (y point)) x)
(defmethod region-difference ((x path) (y point)) x)

(defmethod region-difference ((x everywhere-region) (y region))
  (make-instance 'standard-region-difference :a x :b y))

(defmethod region-difference ((x everywhere-region) (y nowhere-region))
  x)

(defmethod region-difference ((x everywhere-region) (y everywhere-region))
  +nowhere+)

(defmethod region-difference ((x region) (y standard-region-difference))
  (with-slots (a b) y
    (region-union (region-difference x a) (region-intersection x b))))

(defmethod region-difference ((x region) (y standard-region-union))
  ;; A \ (B1 u B2 .. u Bn) = ((((A \ B1) \ B2) ... ) \ Bn)
  (let ((res x))
    (map-over-region-set-regions (lambda (a)
                                   (setf res (region-difference res a)))
                                 y)
    res))

(defmethod region-difference ((x standard-region-union) (y region))
  ;; (A u B) \ C = A\C u B\C
  (let ((res +nowhere+))
    (map-over-region-set-regions
     (lambda (a)
       (setf res (region-union res (region-difference a y))))
     x)
    res))

(defmethod region-difference ((x region) (y standard-rectangle-set))
  (let ((res x))
    (map-over-region-set-regions
     (lambda (a)
       (setf res (region-difference res a)))
     y)
    res))

(defmethod region-difference ((x standard-rectangle-set) (y region))
  (let ((res +nowhere+))
    (map-over-region-set-regions
     (lambda (a)
       (setf res (region-union res (region-difference a y))))
     x)
    res))

(defmethod region-difference ((x point) (y region))
  (multiple-value-bind (px py) (point-position x)
    (if (region-contains-position-p y px py)
        +nowhere+
      x)))

(defmethod region-difference ((x standard-region-difference) (y region))
  ;; (A\B)\C = A \ (B u C)
  (with-slots (a b) x
    (region-difference a (region-union b y))))

(defmethod region-difference ((x region) (y standard-region-intersection))
  (let ((res +nowhere+))
    (map-over-region-set-regions
     (lambda (b)
       (setf res (region-union res (region-difference x b))))
     y)
    res))

(defmethod region-difference ((a standard-polygon) (b standard-polygon))
  (polygon-op a b #'logandc2))

(defmethod region-difference ((a standard-polygon) (b standard-rectangle))
  (polygon-op a b #'logandc2))

(defmethod region-difference ((a standard-rectangle) (b standard-polygon))
  (polygon-op a b #'logandc2))

(defmethod region-difference ((a standard-line) (b standard-polygon))
  (multiple-value-bind (x1 y1) (line-start-point* a)
    (multiple-value-bind (x2 y2) (line-end-point* a)
      (differenz-line/polygon x1 y1 x2 y2 b))))

(defmethod region-difference ((a standard-line) (b standard-rectangle))
  (multiple-value-bind (x1 y1) (line-start-point* a)
    (multiple-value-bind (x2 y2) (line-end-point* a)
      (differenz-line/polygon x1 y1 x2 y2 b))))

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
                                       (list (make-line* x1 y1 (+ x1 (* k1 (- x2 x1))) (+ y1 (* k1 (- y2 y1)))))
                                     nil)
                                   (if (< k2 1)
                                       (list (make-line* (+ x1 (* k2 (- x2 x1))) (+ y1 (* k2 (- y2 y1))) x2 y2))
                                     nil))))
                     (cond ((null r) +nowhere+)
                           ((null (cdr r)) (car r))
                           (t (make-instance 'standard-region-union :regions r))))))
                (t
                 a)))))))

