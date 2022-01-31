(in-package #:clim-tests)

;;; TODO
;;;
;;; - add tests for the standard region sets (union, difference,
;;;   intersection and rectangle-set). Sets should be tested for
;;;   elements of different dimensions (points, paths and areas).
;;; - add tests for unbounded regions (should signal error).
;;; - add tests which verify correctness of a bounding rectangle.
;;; - add tests for rotated regions
;;; - randomize coordinates (translation and scaling affect the
;;;   bounding-rectangle in a predictable way)
;;; - test other instationable bounding-rectangle subclasses


(def-suite* :mcclim.bounding-rectangles
  :in :mcclim)

(defun has-valid-bounding-rectangle (region x1 y1 x2 y2)
  (is (bounding-rectangle-p (bounding-rectangle region)))
  (with-bounding-rectangle* (rx1 ry1 rx2 ry2) region
    (is (= x1 rx1))
    (is (= y1 ry1))
    (is (= x2 rx2))
    (is (= y2 ry2))))

;;; This test is a variation of the above where we ensure that the bounding
;;; rectangle is at least big enough to contain the region. It is used for
;;; mathematically imprecise results.
(defun has-valid-bounding-rectangle* (region x1 y1 x2 y2)
  (is (bounding-rectangle-p (bounding-rectangle region)))
  (with-bounding-rectangle* (rx1 ry1 rx2 ry2) region
    (is (>= x1 rx1))
    (is (>= y1 ry1))
    (is (<= x2 rx2))
    (is (<= y2 ry2))))

(test bounding-rectangle.point
  (let ((point (make-point 0 0)))
    (has-valid-bounding-rectangle point 0 0 0 0)))

(test bounding-rectangle.polyline
  (let ((polyline1 (make-polyline* '(0 0 1 1 4 0)))
        (polyline2 (make-polyline* '(0 0 1 1 4 0) :closed t)))
    (has-valid-bounding-rectangle polyline1 0 0 4 1)
    (has-valid-bounding-rectangle polyline2 0 0 4 1)))

(test bounding-rectangle.line
  (let ((line (make-line* 0 0 1 1)))
    (has-valid-bounding-rectangle line 0 0 1 1)))

(test bounding-rectangle.elliptical-arc
  (let ((elliptical-arc1 (make-elliptical-arc* 0 0 10 0 0 15))
        (elliptical-arc2 (make-elliptical-arc* 0 0 10 0 0 15
                                               :start-angle 0
                                               :end-angle (/ pi 2))))
    (has-valid-bounding-rectangle elliptical-arc1 -10 -15 10 15)
    (has-valid-bounding-rectangle elliptical-arc2 0 -15 10 0)))

(test bounding-rectangle.polygon
  (let ((polygon (make-polygon* '(0 0 1 0 1 1 -1 1))))
    (has-valid-bounding-rectangle polygon -1 0 1 1)))

(test bounding-rectangle.rectangle
  (let ((rectangle (make-rectangle* 5 5 0 0)))
    (has-valid-bounding-rectangle rectangle 0 0 5 5)))

(test bounding-rectangle.ellipse
  (let ((ellipse1 (make-ellipse* 0 0 10 0 0 15))
        (ellipse2 (make-ellipse* 0 0 10 0 0 15
                                 :start-angle 0
                                 :end-angle (/ pi 2))))
    (has-valid-bounding-rectangle ellipse1 -10 -15 10 15)
    (has-valid-bounding-rectangle ellipse2 0 -15 10 0)))

(test bounding-rectangle.region-sets
  ;; For region set tests we take a set of an ellipse and a polygon. Such sets
  ;; are never canonicalised to a non-set region. The polygon is a square with
  ;; the edge length the same as the ellipse and rotated under (/ pi 4). This
  ;; test case is constructed to make evident some issues with how we compute
  ;; the bounding rectangle for the standard sets intersection and difference.
  (let* ((region1 (make-ellipse* 0 0 50 0 0 50))
         (region2 (make-polygon* '(0 0 50 50 100 0 50 -50)))
         (intersection (region-intersection region1 region2))
         (difference (region-difference region1 region2))
         (union (region-union region1 region2))
         (ray-to-xy (/ 50 (sqrt 2))))
    (flet ((test-imprecise (region x1 y1 x2 y2)
             (fails (has-valid-bounding-rectangle region x1 y1 x2 y2))
             (has-valid-bounding-rectangle* region x1 y1 x2 y2)))
      (test-imprecise intersection 0 (- ray-to-xy) 50 ray-to-xy)
      (test-imprecise difference -50 -50 ray-to-xy 50)
      (has-valid-bounding-rectangle union -50 -50 100 50))))
