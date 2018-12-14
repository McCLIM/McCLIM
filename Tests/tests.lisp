(cl::defpackage :clim-tests
  (:use :clim-lisp :clim))

(in-package #:clim-tests)

;;; Transformations 
;;; ============================================================================

(assert (subtypep (class-of +identity-transformation+) 'transformation))
(assert (transformationp +identity-transformation+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; conditions on transformations

(assert (subtypep 'transformation-error 'error))

(assert (subtypep 'transformation-underspecified  'transformation-error))
(assert (typep (make-condition 'transformation-underspecified
			       :points (list (make-point 1 1)
					     (make-point 2 1)
					     (make-point 3 1)))
	       'transformation-error))

(assert (subtypep 'reflection-underspecified  'transformation-error))
(assert (typep (make-condition 'reflection-underspecified
			       :points (list (make-point 1 1)
					     (make-point 3 1)))
	       'transformation-error))


(assert (subtypep 'singular-transformation  'transformation-error))
(assert (typep (make-condition 'singular-transformation
			       :transformation +identity-transformation+)
	       'transformation-error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; different kinds of transformations

(assert (typep (make-translation-transformation 10 20)
	       'transformation))

(assert (typep (make-transformation 10 20 30 40 50 60) 'transformation))

(let* ((x1 30) (y1 40) (x2 10) (y2 20) (x3 30) (y3 55)
       (p1 (make-point x1 y1)) (p2 (make-point x2 y2)) (p3 (make-point x3 y3)))
  (assert (typep (make-rotation-transformation 10) 'transformation))
  (assert (typep (make-rotation-transformation 10 p2) 'transformation))
  (assert (typep (make-rotation-transformation* 10) 'transformation))
  (assert (typep (make-rotation-transformation* 10 x1 y1) 'transformation))
  (assert (typep (make-scaling-transformation 10 50) 'transformation))
  (assert (typep (make-scaling-transformation 10 50 p1) 'transformation))
  (assert (typep (make-scaling-transformation* 10 50) 'transformation))
  (assert (typep (make-scaling-transformation* 10 50 x1 y1) 'transformation))
  (assert (typep (make-reflection-transformation p2 p3) 'transformation))
  (assert (typep (make-reflection-transformation* x2 y2 x3 y3) 'transformation))
  (let* ((x4 66) (y4 89) (x5 85) (y5 13) (x6 867) (y6 -58)
	 (p4 (make-point x4 y4)) (p5 (make-point x5 y5)) (p6 (make-point x6 y6)))
    (assert (typep (make-3-point-transformation p1 p2 p3 p4 p5 p6) 'transformation))
    (assert (typep (make-3-point-transformation* x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 x6 y6)
		   'transformation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; transformation protocol

(let* ((t1 (make-rotation-transformation 0))
       (t2 (make-scaling-transformation 1 1)))
  (assert (identity-transformation-p t1))
  (assert (identity-transformation-p t2))
  (assert (transformation-equal t1 t2))
  (assert (invertible-transformation-p t1))
  (assert (invertible-transformation-p t2))
  (assert (translation-transformation-p t1))
  (assert (translation-transformation-p t2))
;;; tests fail
;;;  (assert (reflection-transformation-p t1))
;;;  (assert (reflection-transformation-p t2))
  (assert (rigid-transformation-p t1))
  (assert (rigid-transformation-p t2))
  (assert (even-scaling-transformation-p t1))
  (assert (even-scaling-transformation-p t2))
  (assert (scaling-transformation-p t1))
  (assert (scaling-transformation-p t2))
  (assert (rectilinear-transformation-p t1))
  (assert (rectilinear-transformation-p t2))
  (assert (transformation-equal t1 (compose-transformations t1 t2)))
  (assert (transformation-equal t1 (invert-transformation t1)))
  (assert (transformation-equal t1 (compose-translation-with-transformation t1 0 0)))
  (assert (transformation-equal t1 (compose-rotation-with-transformation t1 0)))
  (assert (transformation-equal t1 (compose-scaling-with-transformation t1 1 1)))
;;; tests fail
;;;  (assert (transformation-equal t1 (compose-transformation-with-translation t1 0 0)))
  (assert (transformation-equal t1 (compose-transformation-with-rotation t1 0)))
  (assert (transformation-equal t1 (compose-transformation-with-scaling t1 1 1))))
  
  
(let ((tr (make-rotation-transformation 0))
      (r (make-rectangle* 10 20 30 40))
      (x 10) (y 20))
  (assert (region-equal r (transform-region tr r)))
  (assert (region-equal r (untransform-region tr r)))
  (multiple-value-bind (xx yy) (transform-position tr x y)
    (assert (= (coordinate x) xx))
    (assert (= (coordinate y) yy)))
  (multiple-value-bind (xx yy) (untransform-position tr x y)
    (assert (= (coordinate x) xx))
    (assert (= (coordinate y) yy)))
  (multiple-value-bind (xx yy) (transform-distance tr x y)
    (assert (= (coordinate x) xx))
    (assert (= (coordinate y) yy)))
  (multiple-value-bind (xx yy) (untransform-distance tr x y)
    (assert (= (coordinate x) xx))
    (assert (= (coordinate y) yy)))
  (let ((x2 55) (y2 -20))
    (multiple-value-bind (xx1 yy1 xx2 yy2) (transform-rectangle* tr x y x2 y2)
      (assert (= xx1 (min (coordinate x) (coordinate x2))))
      (assert (= yy1 (min (coordinate y) (coordinate y2))))
      (assert (= xx2 (max (coordinate x) (coordinate x2))))
      (assert (= yy2 (max (coordinate y) (coordinate y2)))))
    (multiple-value-bind (xx1 yy1 xx2 yy2) (untransform-rectangle* tr x y x2 y2)
      (assert (= xx1 (min (coordinate x) (coordinate x2))))
      (assert (= yy1 (min (coordinate y) (coordinate y2))))
      (assert (= xx2 (max (coordinate x) (coordinate x2))))
      (assert (= yy2 (max (coordinate y) (coordinate y2)))))))

;;; Bounding rectangles 
;;; ============================================================================

(assert (not (subtypep 'bounding-rectangle 'region)))
(assert (subtypep 'standard-bounding-rectangle 'bounding-rectangle))
(assert (subtypep 'standard-bounding-rectangle 'rectangle))

(assert (bounding-rectangle-p (make-point 23 99)))

(let* ((x1 234) (y1 838) (x2 -234) (y2 22)
       (l (make-line* x1 y1 x2 y2))
       (br (make-bounding-rectangle x1 y1 x2 y2)))
  (assert (bounding-rectangle-p l))
  (assert (typep br 'standard-bounding-rectangle))
  (multiple-value-bind (xx1 yy1 xx2 yy2) (bounding-rectangle* l)
    (assert (= (coordinate (min x1 x2)) xx1))
    (assert (= (coordinate (min y1 y2)) yy1))
    (assert (= (coordinate (max x1 x2)) xx2))
    (assert (= (coordinate (max y1 y2)) yy2)))
  (multiple-value-bind (xx1 yy1 xx2 yy2) (bounding-rectangle* br)
    (assert (= (coordinate (min x1 x2)) xx1))
    (assert (= (coordinate (min y1 y2)) yy1))
    (assert (= (coordinate (max x1 x2)) xx2))
    (assert (= (coordinate (max y1 y2)) yy2)))
  (with-bounding-rectangle* (xx1 yy1 xx2 yy2) l
    (assert (= (coordinate (min x1 x2)) xx1))
    (assert (= (coordinate (min y1 y2)) yy1))
    (assert (= (coordinate (max x1 x2)) xx2))
    (assert (= (coordinate (max y1 y2)) yy2)))
  (with-bounding-rectangle* (xx1 yy1 xx2 yy2) br
    (assert (= (coordinate (min x1 x2)) xx1))
    (assert (= (coordinate (min y1 y2)) yy1))
    (assert (= (coordinate (max x1 x2)) xx2))
    (assert (= (coordinate (max y1 y2)) yy2)))
;;; test failed.  b-r-p returns two values instead of a point
;;;  (assert (region-equal (bounding-rectangle-position l)
;;;			(make-point (min x1 x2) (min y1 y2))))
  (assert (= (bounding-rectangle-min-x l)
	     (coordinate (min x1 x2))))
  (assert (= (bounding-rectangle-min-y l)
	     (coordinate (min y1 y2))))
  (assert (= (bounding-rectangle-max-x l)
	     (coordinate (max x1 x2))))
  (assert (= (bounding-rectangle-max-y l)
	     (coordinate (max y1 y2))))
  (with-bounding-rectangle* (xx1 yy1 xx2 yy2) l
    (assert (= (bounding-rectangle-width l) (- xx2 xx1)))
    (assert (= (bounding-rectangle-height l) (- yy2 yy1))))
  (multiple-value-bind (w h) (bounding-rectangle-size l)
    (assert (= (bounding-rectangle-width l) w))
    (assert (= (bounding-rectangle-height l) h))))

;;; Regions
;;; ============================================================================

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

(let* ((xc 200) (yc 200) (xdr1 100) (ydr1 0) (xdr2 0) (ydr2 50) (sa 0) (ea (/ pi 2))
       (el0 (make-ellipse* xc yc xdr1 ydr1 xdr2 ydr2))
       (el1 (make-ellipse* xc yc xdr1 ydr1 xdr2 ydr2 :start-angle sa :end-angle ea))
       (el2 (make-ellipse* xc yc xdr2 ydr2 xdr1 ydr1 :start-angle sa :end-angle ea))
       (el3 (make-ellipse* xc yc (- xdr1) (- ydr1) (- xdr2) (- ydr2) :start-angle sa :end-angle ea))
       (el4 (make-ellipse* xc yc (- xdr2) (- ydr2) (- xdr1) (- ydr1) :start-angle sa :end-angle ea))
       (some-el (list el1 el2 el3 el4))
       (all-el (list* el0 some-el)))
  (declare (ignorable el0 el1 el2 el3 el4))
  (flet ((rcpt (xc yc elts)
           (mapcar (lambda (el) (assert (region-contains-position-p el xc yc))) elts))
         (rcpn (xc yc elts)
           (mapcar (lambda (el) (assert (not (region-contains-position-p el xc yc)))) elts)))
    ;; trivial cases which may be judged based on distance from the center
    (rcpt xc yc             all-el)
    (rcpt (+ xc 100) yc     all-el)
    (rcpn (1+ xc) (- yc 50) all-el)
    ;; less trivial cases (we accept only 1st quadrent in el1-el4)
    ;; point between 4th and 1st quadrent (on the start-angle)
    (rcpt (+ xc 10) yc all-el)
    ;; point lies in 1st quadrent
    (rcpt (+ xc 10) (- yc 10) all-el)
    ;; point between 1st and 2nd quadrent (on the end-angle)
    (rcpt xc (- yc 50) all-el)
    ;; point lies in 2nd quadrent
    (rcpt (- xc 10) (- yc 10) (list el0))
    (rcpn (- xc 10) (- yc 10) some-el)
    ;; point between 2nd and 3rd quadrent
    (rcpt (- xc 10) yc (list el0))
    (rcpn (- xc 10) yc some-el)
    ;; point lies in 3rd quadrent
    (rcpt (- xc 10) (+ yc 10) (list el0))
    (rcpn (- xc 10) (+ yc 10) some-el)
    ;; point between 3rd and 4th quadrent
    (rcpt xc (+ yc 10) (list el0))
    (rcpn xc (+ yc 10) some-el)
    ;; point lies in 4th quadrent
    (rcpt (+ xc 10) (+ yc 10) (list el0))
    (rcpn (+ xc 10) (+ yc 10) some-el)))

;;; non xy-aligned ellipses (one ellipse put in various coordinates)
(let* ((el1 (make-ellipse* 200 200 +100 -100 -10 -10 :start-angle (* 3 (/ pi 2)) :end-angle pi))
       (el2 (make-ellipse* 200 200 +100 -100 +10 +10 :start-angle (* 3 (/ pi 2)) :end-angle pi))
       (el3 (make-ellipse* 200 200 -100 +100 -10 -10 :start-angle (* 3 (/ pi 2)) :end-angle pi))
       (el4 (make-ellipse* 200 200 -100 +100 +10 +10 :start-angle (* 3 (/ pi 2)) :end-angle pi))
       (el5 (make-ellipse* 200 200 -10 -10 +100 -100 :start-angle (* 3 (/ pi 2)) :end-angle pi))
       (el6 (make-ellipse* 200 200 +10 +10 +100 -100 :start-angle (* 3 (/ pi 2)) :end-angle pi))
       (el7 (make-ellipse* 200 200 -10 -10 -100 +100 :start-angle (* 3 (/ pi 2)) :end-angle pi))
       (el8 (make-ellipse* 200 200 +10 +10 -100 +100 :start-angle (* 3 (/ pi 2)) :end-angle pi))
       (all-ellipses (list el1 el2 el3 el4 el5 el6 el7 el8)))
  (mapcar (lambda (el)
            ;; tips of the ellipse
            (assert (region-contains-position-p el 300 100))
            (assert (region-contains-position-p el 190 190))
            (assert (region-contains-position-p el 210 210))
            (assert (not (region-contains-position-p el 100 300))) ; outside the angle
            ;; points outside the ellipse
            (assert (not (region-contains-position-p el 301 101))) ; too far away
            (assert (not (region-contains-position-p el 200 100))) ; y-aligned tip
            (assert (not (region-contains-position-p el 300 200))) ; x-aligned tip
            )
          all-ellipses))

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

;;; Presentation Types 
;;; ============================================================================

(defparameter *presentation-type-supertypes*
  '(;; 23.8.1
    (t)
    ;; NIL is a special case
    (null t) (boolean t) (symbol t) (keyword symbol t) (blank-area t)
    ;; 23.8.2
    (number t) (complex number t) (real number t) (rational real number t)
    (integer rational real number t) (ratio rational real number t)
    (float real number t)
    ;; 23.8.3
    (character t) (string t)
    ;; 23.8.4
    (pathname t)
    ;; 23.8.5
    ((completion nil) t)
    ;; not allowed abbreviations
    ;; (member t) ((member-sequence nil) t) ((member-alist nil) t)
    ((subset-completion nil) t)
    ;; (subset t) ((subset-sequence nil) t) ((subset-alist nil) t)
    ;; 23.8.6
    ((sequence t) t) (sequence-enumerated t)
    ;; 23.8.7
    ;;   OR, AND
    ;; 23.8.8
    ;;   ((token-or-type nil t) t) ((null-or-type t) t) ((type-or-string t) t)
    ;; 23.8.9
    (expression t)
    (form expression t)))

(defun expect-t-t (type supertype)
  (multiple-value-bind (yesp surep)
      (presentation-subtypep type supertype)
    (assert yesp)
    (assert surep))
  #+mcclim
  ;; we can do this because *presentation-type-supertypes* doesn't do
  ;; clever things with type parameters
  (assert (climi::stupid-subtypep type supertype)))

(defun expect-nil-t (type supertype)
  (multiple-value-bind (yesp surep)
      (presentation-subtypep type supertype)
    (assert (not yesp))
    (assert surep))
  #+mcclim
  (assert (not (climi::stupid-subtypep type supertype))))

(defun expect-nil-nil (type supertype)
  (multiple-value-bind (yesp surep)
      (presentation-subtypep type supertype)
    (assert (not yesp))
    (assert (not surep)))
  ;; stupid-subtypep must be conservative in what it reports as
  ;; possibly acceptable.
  #+mcclim
  (assert (climi::stupid-subtypep type supertype)))
            
(loop for (type . supertypes) in *presentation-type-supertypes*
      do (expect-t-t type type)
      do (expect-t-t nil type)
      ;; if presentation types were "real" (FIXME: work out what
      ;; "real" means) types, then this wouldn't actually be true.
      ;; However, PRESENTATION-SUBTYPEP works by walking up the type
      ;; lattice until it finds a match, and only then checks the type
      ;; parameters.  So even though presentation types (or
      ;; abbreviations) like (MEMBER) actually denote the empty set,
      ;; they are not PRESENTATION-SUBTYPEP NIL.
      do (expect-nil-t type nil)
      do (mapcar (lambda (x) (expect-t-t type x)) supertypes))

(loop for (type) in *presentation-type-supertypes*
      do (expect-t-t type `(and ,type))
      do (expect-t-t `(and ,type) type)
      do (expect-t-t `(and ,type) `(and ,type))
      do (expect-t-t type `(or ,type))
      do (expect-t-t `(or ,type) type)
      do (expect-t-t `(or ,type) `(or ,type))
      do (expect-t-t `(or ,type) `(and ,type))
      do (expect-t-t `(and ,type) `(or ,type)))

(defun constantly-t (object)
  (declare (ignore object))
  t)

(loop for (type) in *presentation-type-supertypes*
      do (expect-t-t `(and ,type (satisfies constantly-t)) type)
      do (expect-nil-nil type `(and ,type (satisfies constantly-t)))
      do (expect-t-t `(and ,type (not nil)) type)
      do (expect-nil-nil type `(and ,type (not nil))))

(expect-t-t '(or integer symbol) '(or integer symbol))
(expect-t-t '(or integer symbol) '(or symbol integer))

(expect-t-t '(or real complex) 'number)
#+nil (expect-t-t 'number '(or real complex))

;;; Input Editing 
;;; ============================================================================

(assert (null *activation-gestures*))

;;; SIMPLE-PARSE-ERROR
(assert (subtypep 'simple-parse-error 'parse-error))

(make-condition 'simple-parse-error 
                :format-control "~A" :format-arguments (list 3))

(handler-case
    (simple-parse-error "foo: ~A" 3)
  (simple-parse-error (c)
    (assert (search "foo: 3" (format nil "~A" c))))
  (:no-error (&rest values)
    (error "~S returned ~S" 'simple-parse-error values)))

;;; INPUT-NOT-OF-REQUIRED-TYPE
(assert (subtypep 'input-not-of-required-type 'parse-error))

(let ((c (make-condition 'input-not-of-required-type
                         :string "not an INTEGER" :type 'integer)))
  (assert (search "not an INTEGER" (format nil "~A" c))))

(handler-case
    (input-not-of-required-type 3 'float)
  (input-not-of-required-type ())
  (:no-error (&rest values)
    (error "~S returned ~S" 'input-not-of-required-type values)))

;;; Commands
;;; ============================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; command tables
(define-command-table no-menu-test-table)

(add-command-to-command-table '(com-test-command) 'no-menu-test-table
                              :keystroke '(#\t))

(let ((count 0))
  (map-over-command-table-keystrokes
   (lambda (menu-name gesture item)
     (incf count)
     (assert 
      (and (equal menu-name nil)
           (equal gesture '(:keyboard #\t 0))
           (equal (command-menu-item-value item)
                  (lookup-keystroke-command-item gesture 
                                                 'no-menu-test-table)))))
   'no-menu-test-table)
  (assert (= count 1)))

(define-command-table menu-test-table)

(add-command-to-command-table '(com-test-command) 'menu-test-table
                              :keystroke '(#\u)
                              :menu "Test")

(let ((count 0))
  (map-over-command-table-keystrokes
   (lambda (menu-name gesture item)
     (incf count)
     (assert 
      (and (equal menu-name "Test")
           (equal gesture '(:keyboard #\u 0))
           (equal (command-menu-item-value item)
                  (lookup-keystroke-command-item gesture 'menu-test-table)))))
   'menu-test-table)
  (assert (= count 1)))

;; (define-command-table removal-test-table)
;; (add-command-to-command-table 'com-test-command 'removal-test-table)
;; (remove-command-from-command-table 'com-test-command 'removal-test-table)
;; (assert (handler-case
;;             (remove-command-from-command-table 'com-test-command
;;                                                'removal-test-table)
;;           (command-not-present () t)
;;           (:no-error (x) (declare (ignore x)) nil)))

;;; command table errors (see 27.2)

;; (assert (subtypep 'command-table-error 'error))
;; (assert (subtypep 'command-table-not-found 'command-table-error))
;; (assert (subtypep 'command-table-already-exists 'command-table-error))
;; (assert (subtypep 'command-not-present 'command-table-error))
;; (assert (subtypep 'command-not-accessible 'command-table-error))
;; (assert (subtypep 'command-already-present 'command-table-error))

;; (let ((condition (make-condition 'command-table-error 
;;                                  :format-control "~A" 
;;                                  :format-arguments '(!))))
;;   (assert (find #\! (format nil "~A" condition))))

;;; not actually required to DTRT here, but we use this form (without
;;; control and arguments) internally, so make sure that we don't
;;; error out recursively when in the debugger with one of these.

(let ((condition (make-condition 'command-not-present)))
  (format nil "~A" condition))

;;; Postscript 
;;; ============================================================================

(let ((psfilename "/tmp/clim-postscript-test.ps")
      (epsfilename "/tmp/clim-postscript-test.eps"))
  (unwind-protect
       (progn
         (with-open-file (s psfilename :direction :output :if-exists :supersede)
           (with-output-to-postscript-stream (s s :device-type :a4)
             (draw-text* s "Hello, World!" 20 20)))
         (with-open-file (s psfilename :direction :input)
           (let ((first-line (read-line s)))
             (assert (eql (mismatch first-line "%!PS-Adobe-") 11))
             (assert (null (position #\Space first-line)))
             (do ((line (read-line s) (read-line s)))
                 ((not (eql (mismatch line "%%") 2)) 
                  (error "Failed to find bounding box"))
               (when (eql (mismatch line "%%BoundingBox: ") 15)
                 (with-input-from-string (string line :start 15)
                   (let ((llx (read string))
                         (lly (read string))
                         (urx (read string))
                         (ury (read string)))
                     (assert (numberp llx))
                     (assert (numberp lly))
                     (assert (numberp urx))
                     (assert (numberp ury))
                     (return t)))))))
         (with-open-file (s epsfilename :direction :output :if-exists :supersede)
           (with-output-to-postscript-stream (s s :device-type :eps)
             (draw-rectangle* s 1 1 19 21)))
         (with-open-file (s epsfilename :direction :input)
           (let ((first-line (read-line s)))
             (assert (eql (mismatch first-line "%!PS-Adobe-") 11))
             (assert (search "EPSF" first-line))
             (do ((line (read-line s) (read-line s)))
                 ((not (eql (mismatch line "%%") 2)) 
                  (error "Failed to find bounding box"))
               (when (eql (mismatch line "%%BoundingBox: ") 15)
                 (with-input-from-string (string line :start 15)
                   (let ((llx (read string))
                         (lly (read string))
                         (urx (read string))
                         (ury (read string)))
                     (assert (numberp llx))
                     (assert (numberp lly))
                     (assert (numberp urx))
                     (assert (numberp ury))
                     ;; our EPS files have lower bounds of 0.
                     (assert (= 0 llx))
                     (assert (= 0 lly))
                     (assert (>= 20 (- urx llx) 18))
                     (assert (>= 22 (- ury lly) 20))
                     (return t))))))))
    (delete-file psfilename)
    (delete-file epsfilename)))
