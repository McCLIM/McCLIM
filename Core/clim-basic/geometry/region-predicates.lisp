(in-package #:climi)


(defmethod region-contains-position-p ((self standard-point) px py)
  (with-slots (x y) self
    (and (coordinate= x px) (coordinate= y py))))

(defmethod region-contains-position-p ((self standard-polyline) x y)
  (setf x (coordinate x)
        y (coordinate y))
  (block nil
    (map-over-polygon-segments
     (lambda (x1 y1 x2 y2)
       (when (line-contains-point-p* x1 y1 x2 y2 x y)
         (return t)))
     self)
    nil))

(defmethod region-contains-position-p ((self standard-line) x y)
  (multiple-value-bind (x1 y1) (line-start-point* self)
    (multiple-value-bind (x2 y2) (line-end-point* self)
      (line-contains-point-p* x1 y1 x2 y2 x y))))

(defmethod region-contains-position-p ((self standard-rectangle) x y)
  (with-standard-rectangle (x1 y1 x2 y2)
      self
    (and (<= x1 (coordinate x) x2)
         (<= y1 (coordinate y) y2))))

(defmethod region-contains-position-p ((self standard-ellipse) x-orig y-orig)
  (with-slots (tr start-angle end-angle) self
    (multiple-value-bind (x y) (untransform-position tr x-orig y-orig)
      (and (<= (+ (* x x) (* y y)) (+ 1.0 (* 4 single-float-epsilon)))
           (or (and (zerop y) (zerop x))
               ;; start-angle being null implies that end-angle is null as well
               (null start-angle)
               ;; we check angle in screen coordinates
               (%angle-between-p (%ellipse-position->angle self x-orig y-orig)
                                 start-angle
                                 end-angle))))))

(defmethod region-contains-position-p ((self standard-rectangle-set) x y)
  (block nil
    (map-over-bands (lambda (y1 y2 isum)
                      (when (<= y1 y y2)
                        (when (isum-member x isum)
                          (return t)))
                      (when (< y y2)
                        (return nil)))
                    (standard-rectangle-set-bands self))
    nil))

(defmethod region-contains-position-p ((self everywhere-region) x y)
  (declare (ignore x y))
  t)

(defmethod region-contains-position-p ((self nowhere-region) x y)
  (declare (ignore x y))
  nil)

(defmethod region-contains-position-p ((self standard-region-union) x y)
  (some (lambda (r) (region-contains-position-p r x y))
        (standard-region-set-regions self)))

(defmethod region-contains-position-p ((self standard-region-intersection) x y)
  (every (lambda (r) (region-contains-position-p r x y))
         (standard-region-set-regions self)))

(defmethod region-contains-position-p ((self standard-region-difference) x y)
  (and (region-contains-position-p (standard-region-difference-a self) x y)
       (not (region-contains-position-p (standard-region-difference-b self) x y))))

(defmethod region-contains-position-p ((self standard-polygon) x y)
  (and (region-contains-position-p (bounding-rectangle self) x y)
       ;; The following algorithm is a Winding Number (wn) method implementation
       ;; based on a description by Dan Sunday "Inclusion of a Point in a
       ;; Polygon" (http://geomalgorithms.com/a03-_inclusion.html).
       (flet ((is-left (x0 y0 x1 y1 x2 y2)
                (- (* (- x1 x0) (- y2 y0))
                   (* (- x2 x0) (- y1 y0)))))
         (let ((x (coordinate x))
               (y (coordinate y))
               (wn 0))
           (map-over-polygon-segments
            (lambda (x1 y1 x2 y2)
              (if (<= y1 y)
                  (when (and (> y2 y)
                             (> (is-left x1 y1 x2 y2 x y) 0))
                    (incf wn))
                  (when (and (<= y2 y)
                             (< (is-left x1 y1 x2 y2 x y) 0))
                    (decf wn))))
            self)
           (not (zerop wn))))))


;;   REGION-CONTAINS-REGION-P region1 region2
;;
;;        Returns t if all points in the region region2 are members of the
;;        region region1; otherwise, it returns nil.
;;
;;        aka region2 ist teilmenge von region1  aka B\A = 0
;;

;;; "generic" version
(defmethod region-contains-region-p ((a region) (b region))
  (or (eq a b)
      (region-equal +nowhere+ (region-difference b a))))

(defmethod region-contains-region-p ((a standard-ellipse) (b standard-ellipse))
  (multiple-value-bind (bcx bcy) (ellipse-center-point* b)
    (and (region-contains-position-p a bcx bcy)
         (null (intersection-ellipse/ellipse a b))
         (or (null (ellipse-start-angle a))
             (multiple-value-bind (sx sy) (%ellipse-angle->position a (ellipse-start-angle a))
               (multiple-value-bind (ex ey) (%ellipse-angle->position a (ellipse-end-angle a))
                 (multiple-value-bind (cx cy) (ellipse-center-point* a)
                   (and (null (region-intersection b (make-line* sx sy cx cy)))
                        (null (region-intersection b (make-line* ex ey cx cy)))))))))))

;;; Ellipse is a convex object. That Implies that if each of the rectangle
;;; vertexes lies inside it, then whole rectangle fits as well. We take a
;;; special care for ellipses with start/end angle.
(defmethod region-contains-region-p ((a standard-ellipse) (b standard-rectangle))
  (with-standard-rectangle (x1 y1 x2 y2) b
    (if (null (ellipse-start-angle a))
        (and (region-contains-position-p a x1 y1)
             (region-contains-position-p a x2 y1)
             (region-contains-position-p a x1 y2)
             (region-contains-position-p a x2 y2))
        (flet ((fits (l) (region-equal l (region-intersection l a))))
          (and (fits (make-line* x1 y1 x2 y1))
               (fits (make-line* x2 y1 x2 y2))
               (fits (make-line* x2 y2 x1 y2))
               (fits (make-line* x1 y2 x1 y1)))))))

(defmethod region-contains-region-p ((a standard-ellipse) (polygon standard-polygon))
  (if (null (ellipse-start-angle a))
      (map-over-polygon-coordinates
       #'(lambda (x y)
           (unless (region-contains-position-p a x y)
             (return-from region-contains-region-p nil)))
       polygon)
      (map-over-polygon-segments
       #'(lambda (x1 y1 x2 y2
                  &aux (line (make-line* x1 y1 x2 y2)))
           (unless (region-equal line (region-intersection line a))
             (return-from region-contains-region-p nil)))
       polygon))
  T)

(defmethod region-contains-region-p ((xs standard-rectangle-set) (point point))
  (multiple-value-bind (x y) (point-position point)
    (region-contains-position-p xs x y)))

(defmethod region-contains-region-p ((a region) (b point))
  (region-contains-position-p a (point-x b) (point-y b)))

;; xxx what about (region-contains-region-p x +nowhere+) ?

(defmethod region-contains-region-p ((a everywhere-region) (b region))
  t)

(defmethod region-contains-region-p ((a nowhere-region) (b region))
  nil)

(defmethod region-contains-region-p ((a everywhere-region) (b everywhere-region))
  t)

(defmethod region-contains-region-p ((a region) (b everywhere-region))
  ;; ??? what about (region-union (region-difference +everywhere+ X) X) ???
  nil)

(defmethod region-contains-region-p ((a region) (b nowhere-region))
  t)




;;   REGION-INTERSECTS-REGION-P region1 region2
;;
;;        Returns nil if region-intersection of the two regions region1 and
;;        region2 would be +nowhere+; otherwise, it returns t.
;;
;;        aka region1 und region2 sind nicht disjunkt  aka AB /= 0
;;

;; "generic" version
(defmethod region-intersects-region-p ((a region) (b region))
  (not (region-equal +nowhere+ (region-intersection a b))))

(defmethod region-intersects-region-p :around ((a bounding-rectangle) (b bounding-rectangle))
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* a)
    (multiple-value-bind (u1 v1 u2 v2) (bounding-rectangle* b)
      (cond ((and (<= u1 x2) (<= x1 u2)
                  (<= v1 y2) (<= y1 v2))
             (call-next-method))
            (t
             nil)))))

(defmethod region-intersects-region-p ((a standard-rectangle) (b standard-rectangle))
  (declare (ignorable a b))
  ;; for rectangles, the bounding rectangle test is correct, so if we
  ;; wind up here, we just can return T.
  t
  ;;(multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* a)
  ;;  (multiple-value-bind (u1 v1 u2 v2) (rectangle-edges* b)
  ;;    (and (<= u1 x2) (<= x1 u2)
  ;;         (<= v1 y2) (<= y1 v2))))
  )



;; "generic" version
(defmethod region-equal ((a region) (b region))
  (region-equal +nowhere+ (region-exclusive-or a b)))

(defmethod region-equal ((a point) (b point))
  (and (coordinate= (point-x a) (point-x b))
       (coordinate= (point-y a) (point-y b))))

(defmethod region-equal ((xs standard-rectangle-set) (ys standard-rectangle-set))
  ;; Our bands representation is canonic
  (equal (standard-rectangle-set-bands xs)
         (standard-rectangle-set-bands ys)))

(defmethod region-equal ((a everywhere-region) (b everywhere-region))
  t)

(defmethod region-equal ((a nowhere-region) (b nowhere-region))
  t)

(defmethod region-equal ((a everywhere-region) (b region))
  nil)

(defmethod region-equal ((a nowhere-region) (b region))
  nil)

(defmethod region-equal ((a region) (b everywhere-region))
  nil)

(defmethod region-equal ((a region) (b nowhere-region))
  nil)

(defmethod region-equal ((a standard-rectangle) (b standard-rectangle))
  (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* a)
    (multiple-value-bind (u1 v1 u2 v2) (rectangle-edges* b)
      (and (coordinate= x1 u1)
           (coordinate= y1 v1)
           (coordinate= x2 u2)
           (coordinate= y2 v2)))))

(defmethod region-equal ((a standard-rectangle) (b path)) nil)
(defmethod region-equal ((a path) (b standard-rectangle)) nil)

(defmethod region-equal ((a standard-line) (b standard-line))
  (or (and (region-equal (line-start-point a) (line-start-point b))
           (region-equal (line-end-point a) (line-end-point b)))
      (and (region-equal (line-start-point a) (line-end-point b))
           (region-equal (line-end-point a) (line-start-point b)))))

