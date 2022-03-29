;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998 Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 1998-2000 Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2014 Robert Strandh <robert.strandh@gmail.com>
;;;  (c) copyright 2019 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Region algebra methods for the various region classes.
;;;

(in-package #:climi)

(defmethod region-union ((a bounding-rectangle) (b bounding-rectangle))
  (make-instance 'standard-region-union :regions (list a b)))

(defmethod region-union
    ((a standard-region-complement) (b standard-region-complement))
  (region-complement (region-intersection (region-complement a)
                                          (region-complement b))))

(define-commutative-method region-union
    ((a standard-region-complement) (b bounding-rectangle))
  (if (region-equal (region-complement a) b)
      +everywhere+
      (region-complement (region-intersection a (region-complement b)))))

;;; Dimensionality rule

(define-commutative-method region-union ((a area) (b path)) a)
(define-commutative-method region-union ((a path) (b point)) a)
(define-commutative-method region-union ((a area) (b point)) a)

;;; Points

(defmethod region-union ((a point) (b point))
  (cond ((region-equal a b)
         a)
        (t
         (make-instance 'standard-region-union :regions (list a b)))))

;;; Paths

(defmethod region-union ((a standard-line) (b standard-line))
  (let ((s1 (line-start-point a))
        (e1 (line-end-point a))
        (s2 (line-start-point b))
        (e2 (line-end-point b)))
    (cond ((and (region-equal s1 s2)
                (region-equal e1 e2))
           a)
          ((region-equal e1 s2)
           (make-polyline (list s1 e1 s2 e2)))
          ((region-equal e2 s1)
           (make-polyline (list s2 e2 s1 e1)))
          (t
           (make-instance 'standard-region-union :regions (list a b))))))

(defmethod region-union ((a standard-polyline) (b standard-polyline))
  (with-slots ((a-points points)) a
    (with-slots ((b-points points)) b
      (cond ((region-contains-region-p a b)
             a)
            ((region-contains-region-p b a)
             b)
            ((or (polyline-closed a)
                 (polyline-closed b))
             (make-instance 'standard-region-union :regions (list a b)))
            ((region-equal (car (last a-points)) (car b-points))
             (make-polyline (append b-points (rest a-points))))
            ((region-equal (car (last b-points)) (car a-points))
             (make-polyline (append a-points (rest b-points))))
            (t
             (make-instance 'standard-region-union :regions (list a b)))))))

(define-commutative-method region-union ((a standard-polyline) (b standard-line))
  (let ((lsp (line-start-point b))
        (lep (line-end-point b))
        (points (polygon-points a)))
    (cond ((region-contains-region-p a b)
           a)
          ((polyline-closed a)
           (make-instance 'standard-region-union :regions (list a b)))
          ((region-equal lep (first points))
           (make-polyline (list* lsp points)))
          ((region-equal (first (last points)) lsp)
           (make-polyline (append points (list lep))))
          (t
           (make-instance 'standard-region-union :regions (list a b))))))

;;; Areas

(defmethod region-union ((xs standard-rectangle) (ys standard-rectangle))
  (region-union (rectangle->standard-rectangle-set xs)
                (rectangle->standard-rectangle-set ys)))

(defmethod region-union ((a standard-polygon) (b standard-polygon))
  (polygon-op a b #'logior))

(define-commutative-method region-union
    ((a standard-polygon) (b standard-rectangle))
  (polygon-op a b #'logior))

;;; IMHO the CLIM dimensionality rule is brain dead! --gb

(defmethod region-intersection ((a bounding-rectangle) (b bounding-rectangle))
   (cond ((region-contains-region-p a b) b)
         ((region-contains-region-p b a) a)
         (t (make-instance 'standard-region-intersection :regions (list a b)))))

(define-commutative-method region-intersection
    ((bbox bounding-rectangle) (rdif standard-region-complement))
  (if (region-equal bbox (region-complement rdif))
      +nowhere+
      (make-instance 'standard-region-intersection :regions (list bbox rdif))))

(defmethod region-intersection
    ((x standard-region-complement) (y standard-region-complement))
  (make-instance 'standard-region-complement
                 :complement (region-union (region-complement x)
                                           (region-complement y))))

;;; Points

(define-commutative-method region-intersection
    ((region bounding-rectangle) (point point))
  (multiple-value-bind (x y) (point-position point)
    (if (region-contains-position-p region x y)
        point
        +nowhere+)))

(defmethod region-intersection ((a point) (b point))
  (if (region-equal a b)
      a
      +nowhere+))

;;; Paths

(define-commutative-method region-intersection
    ((polyline standard-polyline) (region bounding-rectangle))
  (let ((res +nowhere+))
    ;; hack alert
    (map-over-polygon-segments
     (lambda (x1 y1 x2 y2)
       (setf res
             (region-union
              res (region-intersection (make-line* x1 y1 x2 y2) region))))
     polyline)
    res))

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

;;; Paths/areas

(define-commutative-method region-intersection
    ((line-var line) (ellipse-var ellipse))
  (let (p1x p1y p2x p2y)
    (multiple-value-setq (p1x p1y) (line-start-point* line-var))
    (multiple-value-setq (p2x p2y) (line-end-point* line-var))
    (let ((region
            (if (and (region-contains-position-p ellipse-var p1x p1y)
                     (region-contains-position-p ellipse-var p2x p2y))
                line-var
                (multiple-value-bind (x1 y1 x2 y2)
                    (cond ((= p1x p2x)
                           (intersection-vline/ellipse ellipse-var p1x))
                          ((= p1y p2y)
                           (intersection-hline/ellipse ellipse-var p1y))
                          (t
                           (intersection-line/ellipse ellipse-var p1x p1y p2x p2y)))
                  (if (some #'complexp (list x1 y1 x2 y2))
                      +nowhere+
                      (make-line* x1 y1 x2 y2)))))
          (start-angle (ellipse-start-angle ellipse-var))
          (end-angle (ellipse-end-angle ellipse-var)))
      (when (or (null start-angle) (region-equal region +nowhere+))
        (return-from region-intersection region))
      (multiple-value-bind (cx cy) (ellipse-center-point* ellipse-var)
        (multiple-value-bind (sx sy)
            (%ellipse-angle->position ellipse-var start-angle)
          (multiple-value-bind (ex ey)
              (%ellipse-angle->position ellipse-var end-angle)
            (let* ((start-ray (make-line* cx cy sx sy))
                   (end-ray (make-line* cx cy ex ey))
                   (si (region-intersection region start-ray))
                   (ei (region-intersection region end-ray))
                   (sip (not (region-equal +nowhere+ si)))
                   (eip (not (region-equal +nowhere+ ei)))
                   (p1 (line-start-point region))
                   (p2 (line-end-point region))
                   (p1p (multiple-value-call
                            #'region-contains-position-p ellipse-var
                          (point-position p1)))
                   (p2p (multiple-value-call
                            #'region-contains-position-p ellipse-var
                          (point-position p2))))
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
                              (if sip si ei)))))))))))

(define-commutative-method region-intersection
    ((polygon-var polygon) (line-var line))
  (multiple-value-bind (x1 y1) (line-start-point* line-var)
    (multiple-value-bind (x2 y2) (line-end-point* line-var)
      (intersection-segment/polygon x1 y1 x2 y2 polygon-var))))

;;; Areas

(defmethod region-intersection ((xr rectangle) (yr rectangle))
  (region-intersection (rectangle->standard-rectangle-set xr)
                       (rectangle->standard-rectangle-set yr)))

(defmethod region-intersection ((a standard-polygon) (b standard-polygon))
  (polygon-op a b #'logand))

(define-commutative-method region-intersection
    ((a standard-polygon) (b standard-rectangle))
  (polygon-op a b #'logand))

(defmethod region-difference ((x bounding-rectangle) (y bounding-rectangle))
  (let ((cy (make-instance 'standard-region-complement :complement y)))
    (make-instance 'standard-region-intersection :regions (list x cy))))

(defmethod region-difference
    ((x standard-region-complement) (y standard-region-complement))
  (region-intersection x (region-complement y)))

(defmethod region-difference ((x bounding-rectangle) (y standard-region-complement))
  (region-intersection x (region-complement y)))

(defmethod region-difference ((x standard-region-complement) (y bounding-rectangle))
  ;; (U\A)\C = U\(AuC)
  (make-instance 'standard-region-complement
                 :complement (region-union (region-complement x) y)))

;;; Dimensionality rule

(defmethod region-difference ((x area) (y path)) x)
(defmethod region-difference ((x area) (y point)) x)
(defmethod region-difference ((x path) (y point)) x)

;;; Points

(defmethod region-difference ((x point) (y bounding-rectangle))
  (multiple-value-bind (px py) (point-position x)
    (if (region-contains-position-p y px py)
        +nowhere+
        x)))

;;; Paths

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
          (cond ((and (coordinate= 0 (line-equation x1 y1 x2 y2 u1 v1))
                      (coordinate= 0 (line-equation x1 y1 x2 y2 u2 v2)))
                 (let ((k1 (position->line-fktn x1 y1 x2 y2 u1 v1))
                       (k2 (position->line-fktn x1 y1 x2 y2 u2 v2)))
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

;;; Paths/areas

(defmethod region-difference ((a line) (b polygon))
  (multiple-value-bind (x1 y1) (line-start-point* a)
    (multiple-value-bind (x2 y2) (line-end-point* a)
      (difference-segment/polygon x1 y1 x2 y2 b))))

;;; Areas

(defmethod region-difference ((xs standard-rectangle) (ys standard-rectangle))
  (region-difference (rectangle->standard-rectangle-set xs)
                     (rectangle->standard-rectangle-set ys)))

(defmethod region-difference ((a standard-polygon) (b standard-polygon))
  (polygon-op a b #'logandc2))

(defmethod region-difference ((a standard-polygon) (b standard-rectangle))
  (polygon-op a b #'logandc2))

(defmethod region-difference ((a standard-rectangle) (b standard-polygon))
  (polygon-op a b #'logandc2))
