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
;;; coordiante

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
  (assert (member p1 regions :test #'region-equal)))
