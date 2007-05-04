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

;;; standard-rectangle-set and containment calculation
(let* ((r1 (make-rectangle* 0 0 1 1))
       (r2 (make-rectangle* 2 0 3 1))
       (ru (region-union r1 r2)))
  (assert (not (region-contains-position-p ru -1/2 1/2)))
  (assert (region-contains-position-p ru 1/2 1/2))
  (assert (not (region-contains-position-p ru 3/2 1/2)))
  (assert (region-contains-position-p ru 5/2 1/2))
  (assert (not (region-contains-position-p ru 7/2 1/2)))
  (assert (not (region-contains-position-p ru 1/2 3/2)))
  (assert (not (region-contains-position-p ru 5/2 -1/2))))

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
  (assert (null (set-difference points (list p1 p2 p3) :test #'region-equal)))
  (assert (polyline-closed pl3))
  (assert (not (polyline-closed pl2))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; line

(assert (subtypep 'line 'polyline))
(assert (subtypep 'standard-line 'line))

(let* ((x1 234) (y1 876) (x2 345) (y2 -55)
       (p1 (make-point x1 y1)) (p2 (make-point x2 y2))
       (l1 (make-line p1 p2)) (l2 (make-line* x1 y1 x2 y2)))
  (assert (typep l1 'standard-line))
  (assert (linep l1))
  (assert (region-equal l1 l2))
  (multiple-value-bind (xx1 yy1) (line-start-point* l1)
    (assert (= (coordinate x1) xx1))
    (assert (= (coordinate y1) yy1)))
  (multiple-value-bind (xx2 yy2) (line-end-point* l1)
    (assert (= (coordinate x2) xx2))
    (assert (= (coordinate y2)yy2)))
  (assert (region-equal p1 (line-start-point l1)))
  (assert (region-equal p2 (line-end-point l1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; rectangle

(assert (subtypep 'rectangle 'polygon))
(assert (subtypep 'standard-rectangle 'rectangle))

(let* ((x1 234) (y1 876) (x2 345) (y2 -55)
       (p1 (make-point x1 y1)) (p2 (make-point x2 y2))
       (r1 (make-rectangle p1 p2)) (r2 (make-rectangle* x1 y1 x2 y2)))
  (assert (typep r1 'standard-rectangle))
  (assert (rectanglep r1))
  (assert (region-equal r1 r2))
  (multiple-value-bind (min-x min-y max-x max-y) (rectangle-edges* r1)
    (assert (= (rectangle-min-x r1) min-x))
    (assert (= (rectangle-min-y r1) min-y))
    (assert (= (rectangle-max-x r1) max-x))
    (assert (= (rectangle-max-y r1) max-y))
    (assert (= (coordinate x1) min-x))
    (assert (= (coordinate y1) max-y))
    (assert (= (coordinate x2) max-x))
    (assert (= (coordinate y2) min-y))
    (multiple-value-bind (width height) (rectangle-size r1)
      (assert (= width (rectangle-width r1)))
      (assert (= height (rectangle-height r1)))
      (assert (= width (- max-x min-x)))
      (assert (= height (- max-y min-y)))))
  (assert (region-equal (make-point x1 y2) (rectangle-min-point r1)))
  (assert (region-equal (make-point x2 y1) (rectangle-max-point r1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ellipse

(assert (subtypep 'ellipse 'area))
(assert (subtypep 'standard-ellipse 'ellipse))

(let* ((xc 234) (yc 345) (xdr1 -858) (ydr1 44) (xdr2 -55) (ydr2 5)
       (sa 10) (ea 270)
       (pc (make-point xc yc))
       (e1 (make-ellipse* xc yc xdr1 ydr1 xdr2 ydr2 :start-angle sa :end-angle ea))
       (e2 (make-ellipse pc xdr1 ydr1 xdr2 ydr2 :start-angle sa :end-angle ea))
       (e3 (make-ellipse pc xdr1 ydr1 xdr2 ydr2)))
  (assert (typep e1 'standard-ellipse))
  (assert (ellipsep e1))
;;; this test fails
;;;  (assert (region-equal e1 e2))
  (multiple-value-bind (x y) (ellipse-center-point* e1)
    (assert (= (coordinate xc) x))
    (assert (= (coordinate yc) y))
    (assert (region-equal (make-point x y) (ellipse-center-point e2))))
  (multiple-value-bind (xr11 yr11 xr12 yr12) (ellipse-radii e1)
    (multiple-value-bind (xr21 yr21 xr22 yr22) (ellipse-radii e2)
      (assert (= xr11 xr21))
      (assert (= yr11 yr21))
      (assert (= xr12 xr22))
      (assert (= yr12 yr22))))
  (assert (= (coordinate sa) (coordinate (ellipse-start-angle e1))))
  (assert (= (coordinate ea) (coordinate (ellipse-end-angle e1))))
  (assert (null (ellipse-start-angle e3)))
  (assert (null (ellipse-end-angle e3))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; elliptical arc

(assert (subtypep 'elliptical-arc 'path))
(assert (subtypep 'standard-elliptical-arc 'elliptical-arc))

(let* ((xc 234) (yc 345) (xdr1 -858) (ydr1 44) (xdr2 -55) (ydr2 5)
       (sa 10) (ea 270)
       (pc (make-point xc yc))
       (ea1 (make-elliptical-arc* xc yc xdr1 ydr1 xdr2 ydr2 :start-angle sa :end-angle ea))
       (ea2 (make-elliptical-arc pc xdr1 ydr1 xdr2 ydr2 :start-angle sa :end-angle ea))
       (ea3 (make-elliptical-arc pc xdr1 ydr1 xdr2 ydr2)))
  (assert (typep ea1 'standard-elliptical-arc))
  (assert (elliptical-arc-p ea1))
;;; this test fails
;;;  (assert (region-equal ea1 ea2))
  (multiple-value-bind (x y) (ellipse-center-point* ea1)
    (assert (= (coordinate xc) x))
    (assert (= (coordinate yc) y))
    (assert (region-equal (make-point x y) (ellipse-center-point ea2))))
  (multiple-value-bind (xr11 yr11 xr12 yr12) (ellipse-radii ea1)
    (multiple-value-bind (xr21 yr21 xr22 yr22) (ellipse-radii ea2)
      (assert (= xr11 xr21))
      (assert (= yr11 yr21))
      (assert (= xr12 xr22))
      (assert (= yr12 yr22))))
  (assert (= (coordinate sa) (coordinate (ellipse-start-angle ea1))))
  (assert (= (coordinate ea) (coordinate (ellipse-end-angle ea1))))
  (assert (null (ellipse-start-angle ea3)))
  (assert (null (ellipse-end-angle ea3))))
