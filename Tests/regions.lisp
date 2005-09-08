(defpackage :clim-tests
  (:use :clim-lisp :clim))

(in-package :clim-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; region

(assert (subtypep 'region 'design))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; path

(assert (subtypep 'path 'region))
(assert (subtypep 'path 'bounding-rectangle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; area

(assert (subtypep 'area 'region))
(assert (subtypep 'area 'bounding-rectangle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; coordinate

(assert (or (and (subtypep 'coordinate t)
		 (subtypep t 'coordinate))
	    (subtypep 'coordinate 'real)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; point

(assert (subtypep 'standard-point 'point))

(let ((p (make-point 1/2 55/33)))
  (assert (typep p 'standard-point))
  (assert (pointp p))
  (assert (not (pathp p)))
  (assert (not (areap p)))
  (assert (typep p 'region))
  (assert (regionp p))
  (multiple-value-bind (x y) (point-position p)
    (assert (= (point-x p) x))
    (assert (= (point-y p) y))
    (assert (typep x 'coordinate))
    (assert (typep y 'coordinate))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; +everywhere+, +nowhere+

(assert (not (region-equal +everywhere+ +nowhere+)))

(assert (region-contains-region-p +everywhere+ +nowhere+))
(assert (not (region-contains-region-p +nowhere+ +everywhere+)))
(assert (not (region-intersects-region-p +nowhere+ +everywhere+)))
(assert (region-contains-position-p +everywhere+ 10 -10))
(assert (not (region-contains-position-p +nowhere+ -10 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; region set

(assert (subtypep 'region-set 'region))
(assert (subtypep 'region-set 'bounding-rectangle))
(assert (subtypep 'standard-region-union 'region-set))
(assert (subtypep 'standard-region-intersection 'region-set))
(assert (subtypep 'standard-region-difference 'region-set))

;;; union of two different points
(let* ((p1 (make-point 10 -10))
       (p2 (make-point -10 10))
       (u (region-union p1 p2))
       (regions (region-set-regions u)))
  (assert (typep u 'standard-region-union))
  (assert (= (length regions) 2))
  (assert (member p1 regions :test #'region-equal))
  (assert (member p2 regions :test #'region-equal)))

;;; intersection of two different points
;;; this test fails.  It loops forever
#+(or)
(let* ((p1 (make-point 10 -10))
       (p2 (make-point -10 10))
       (i (region-intersection p1 p2)))
  (assert (region-equal i +nowhere+)))

;;; difference of two different points
(let* ((p1 (make-point 10 -10))
       (p2 (make-point -10 10))
       (d (region-difference p1 p2))
       (regions (region-set-regions d)))
  (assert (or (typep d 'standard-region-difference)
	      (pointp d)))
  (assert (member (length regions) '(1 2)))
  (assert (member p1 regions :test #'region-equal))
  (let* ((regions2 '()))
    (map-over-region-set-regions
     (lambda (region) (push region regions2))
     d)
    (assert (null (set-difference regions regions2 :test #'region-equal)))
    (assert (null (set-difference regions2 regions :test #'region-equal)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; polyline

(assert (subtypep 'polyline 'path))
(assert (subtypep 'standard-polyline 'polyline))

(let* ((x1 10) (y1 22) (x2 30) (y2 30) (x3 50) (y3 5)
       (p1 (make-point x1 y1)) (p2 (make-point x2 y2)) (p3 (make-point x3 y3))
       (pl1 (make-polyline (list p1 p2 p3)))
       (pl2 (make-polyline* (list x1 y1 x2 y2 x3 y3)))
       (pl3 (make-polyline (list p1 p2 p3) :closed t))
       (pl4 (make-polyline* (list x1 y1 x2 y2 x3 y3) :closed t))
       (points '()))
  (assert (typep pl1 'standard-polyline))
  (assert (polylinep pl1))
  (assert (typep pl2 'standard-polyline))
  (assert (polylinep pl2))
  (assert (region-equal pl1 pl2))
  (assert (typep pl3 'standard-polyline))
  (assert (polylinep pl3))
  (assert (typep pl4 'standard-polyline))
  (assert (polylinep pl4))
  (assert (region-equal pl3 pl4))
  (assert (null (set-difference (polygon-points pl1) (list p1 p2 p3) :test #'region-equal)))
  (assert (null (set-difference (list p1 p2 p3) (polygon-points pl1) :test #'region-equal)))
  (assert (null (set-difference (polygon-points pl2) (list p1 p2 p3) :test #'region-equal)))
  (assert (null (set-difference (list p1 p2 p3) (polygon-points pl2) :test #'region-equal)))
  (assert (null (set-difference (polygon-points pl3) (list p1 p2 p3) :test #'region-equal)))
  (assert (null (set-difference (list p1 p2 p3) (polygon-points pl3) :test #'region-equal)))
  (assert (null (set-difference (polygon-points pl4) (list p1 p2 p3) :test #'region-equal)))
  (assert (null (set-difference (list p1 p2 p3) (polygon-points pl4) :test #'region-equal)))
  (map-over-polygon-coordinates
   (lambda (x y)
     (push (make-point x y) points))
   pl1)
  (assert (null (set-difference (list p1 p2 p3) points :test #'region-equal)))
  (assert (null (set-difference points (list p1 p2 p3) :test #'region-equal))))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; polygon

(assert (subtypep 'polygon 'area))
(assert (subtypep 'standard-polygon 'polygon))

(let* ((x1 10) (y1 22) (x2 30) (y2 30) (x3 50) (y3 5)
       (p1 (make-point x1 y1)) (p2 (make-point x2 y2)) (p3 (make-point x3 y3))
       (pg1 (make-polygon (list p1 p2 p3)))
       (pg2 (make-polygon* (list x1 y1 x2 y2 x3 y3)))
       (points '()))
  (assert (typep pg1 'standard-polygon))
  (assert (polygonp pg1))
  (assert (typep pg2 'standard-polygon))
  (assert (polygonp pg2))
  (assert (region-equal pg1 pg2))
  (assert (null (set-difference (polygon-points pg1) (list p1 p2 p3) :test #'region-equal)))
  (assert (null (set-difference (list p1 p2 p3) (polygon-points pg1) :test #'region-equal)))
  (assert (null (set-difference (polygon-points pg2) (list p1 p2 p3) :test #'region-equal)))
  (assert (null (set-difference (list p1 p2 p3) (polygon-points pg2) :test #'region-equal)))
  (map-over-polygon-coordinates
   (lambda (x y)
     (push (make-point x y) points))
   pg1)
  (assert (null (set-difference (list p1 p2 p3) points :test #'region-equal)))
  (assert (null (set-difference points (list p1 p2 p3) :test #'region-equal))))
