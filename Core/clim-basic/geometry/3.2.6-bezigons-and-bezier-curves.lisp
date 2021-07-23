;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2014 Robert Strandh <robert.strandh@gmail.com>
;;;  (c) copyright 2016-2018 Cyrus Harmon <ch-github@bobobeach.com>
;;;  (c) copyright 2021 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; 3.2.6 Bezigons and (composite) bezier curves
;;;

(in-package #:clim-internals)

(define-protocol-class polybezier (path) ())
(define-protocol-class bezigon    (area) ())

;;; Basically, when the region protocol is truly integrated, the protocol class
;;; polyline should be a subclass of the protocol class polybezier and the
;;; protocol class polygon should be a subclass of the protocol class bezigon.

(defclass bezier-thing ()
  ((points
    :type list
    :initarg :points
    :initform '())))

(defgeneric bezigon-points (object)
  (:method ((object polyline))
    (polygon-points object))
  (:method ((object polygon))
    (polygon-points object))
  (:method ((object bezier-thing))
    (slot-value object 'points)))

(defgeneric bezigon-order (object)
  (:method ((object polyline))     2)
  (:method ((object polygon))      2)
  (:method ((object bezier-thing)) 4))

(defgeneric map-over-bezigon-segments (function bezigon-or-polybezier)
  (:method (function (object polyline))
    (map-over-polygon-segments function object))
  (:method (function (object polygon))
    (map-over-polygon-segments function object))
  (:method (function (object bezier-thing))
    (loop for (p0 p1 p2 p3) on (bezigon-points object) by #'cdddr
          until (null p1)
          do (funcall function
                      (point-x p0) (point-y p0)
                      (point-x p1) (point-y p1)
                      (point-x p2) (point-y p2)
                      (point-x p3) (point-y p3))
          finally (assert (and p0 (null p1) (null p2) (null p3))))))

(defclass standard-polybezier (cached-bbox-mixin bezier-thing) ())
(defclass standard-bezigon (cached-bbox-mixin bezier-thing) ())

(defun make-polybezier (point-seq)
  (assert (= (mod (length point-seq) 3) 1))
  (make-instance 'standard-polybezier :points point-seq))

(defun make-polybezier* (coord-seq)
  (assert (= (mod (length coord-seq) 6) 2))
  (make-polybezier (coord-seq->point-seq coord-seq)))

(defun make-bezigon (point-seq)
  (assert (region-equal (car point-seq) (car (last point-seq))))
  (make-instance 'standard-bezigon :points point-seq))

(defun make-bezigon* (coord-seq)
  (assert (and (climi::coordinate= (car coord-seq) (car (last coord-seq 2)))
               (climi::coordinate= (cadr coord-seq) (car (last coord-seq)))))
  (make-bezigon (coord-seq->point-seq coord-seq)))

#+ (or)
;;; Here be dragons. Basically this means: don't mix bezier things with other
;;; regions until the following methods are implemented. The existing
;;; implementation is still useful for rendering purposes though. The list of
;;; the specialized methods below is not exhaustive, i.e it is possible to
;;; represent a rectangle with a bezigon (or a polyline with a polybezier).
(progn
  ;; Check whether x/y meets the equation of any segment.
  (defmethod region-contains-position-p ((r polybezier) x y)
    (error "IMPLEMENT ME!"))

  ;; See "Inclusion test for curved-edge polygons" by J. Ruiz De Miras and
  ;; F. R. Feito.
  (defmethod region-contains-position-p ((r bezigon) x y)
    (error "IMPLEMENT ME!"))

  ;; A default method based on region-difference should do.
  (defmethod region-equal ((r1 polybezier) (r2 polybezier))
    (error "IMPLEMENT ME!"))

  (defmethod region-equal ((r1 bezigon) (r2 bezigon))
    (error "IMPLEMENT ME!"))

  ;; Trivial
  (defmethod region-contains-region-p ((r1 polybezier) (r2 polybezier))
    (error "IMPLEMENT ME!"))

  ;; Ha ha, good luck (check out convex hull property of bezier curves).
  ;;
  ;; Alternatively throw in the bucket, polygonize everything and treat bezier
  ;; things as polygons.
  (defmethod region-contains-region-p ((r1 bezigon) (r2 bezigon))
    (error "IMPLEMENT ME!"))

  (defmethod region-contains-region-p ((r1 bezigon) (r2 bounding-rectangle))
    (error "IMPLEMENT ME!"))

  (defmethod region-contains-region-p ((r1 bounding-rectangle) (r2 bezigon))
    (error "IMPLEMENT ME!"))

  ;; This method should also check whether r2 ends at the start of r1 or whether
  ;; we could match them.
  (defmethod region-union ((r1 polybezier) (r2 polybezier))
    (error "IMPLEMENT ME!")
    (let* ((r1-points (bezigon-points r1))
           (r2-points (bezigon-points r2))
           (pn-a (last-elt r1-points))
           (pn-b (first    r2-points)))
      (if (region-equal pn-a pn-b)
          (make-instance 'standard-polybezier :points (append r1-points r2-points))
          (call-next-method))))

  ;; This method should find "equal" segments (may be also in reverse order) and
  ;; based on that split r1 into multiple bezier curves.
  (defmethod region-difference ((r1 polybezier) (r2 polybezier))
    (error "IMPLEMENT ME!"))

  ;; This method should first try to collect "equal" and if there are some
  ;; return this as an union of (possibly) disjoint curves. Otherwise it should
  ;; return a points for individual segments intersections (see "Complete
  ;; Subdivision Algorithms, I: Intersection of Bezier Curves" by Chee
  ;; K. Yap). The latter should be also applied to polylines.
  (defmethod region-intersection ((r1 polybezier) (r2 polybezier))
    (error "IMPLEMENT ME!")))

(defmethod transform-region (transformation (object bezier-thing))
  (let ((points (bezigon-points object)))
    (make-instance (class-of object)
                   :points (mapcar (curry #'transform-region transformation)
                                   points))))

(defmethod bounding-rectangle* ((object bezier-thing))
  (labels ((solve-quadratic (a2 a1 a0 &key complex-roots multiple-roots)
             (when (zerop a2)
               (return-from solve-quadratic (- (/ a0 a1))))
             (unless (= a2 1)
               (setf a1 (/ a1 a2)
                     a0 (/ a0 a2)))
             (let* ((-a1/2 (- (/ a1 2.0)))
                    (r (- (* -a1/2 -a1/2) a0)))
               (cond ((zerop r)
                      (if multiple-roots
                          (values -a1/2 -a1/2)
                          -a1/2))
                     ((minusp r)
                      (if complex-roots
                          (values (+ -a1/2 (sqrt r)) (- -a1/2 (sqrt r)))
                          (values)))
                     (t
                      (values (+ -a1/2 (sqrt r)) (- -a1/2 (sqrt r)))))))
           (evaluate-bezier (w0 w1 w2 w3 a)
             (let ((1-a (- 1.0 a)))
               (+ (* 1-a 1-a 1-a w0)
                  (* 3.0 1-a 1-a a w1)
                  (* 3.0 1-a a a w2)
                  (* a a a w3))))
           (dimension-min-max (w0 w1 w2 w3)
             (when (> w0 w3)
               (rotatef w0 w3)
               (rotatef w1 w2))
             (when (and (<= w0 w1 w3)
                        (<= w0 w2 w3))
               (return-from dimension-min-max
                 (values w0 w3)))
             (let ((a (+ (- w0) (* 3 w1) (* -3 w2) w3))
                   (b (+ (* 2 w0) (* -4 w1) (* 2 w2)))
                   (c (- w1 w0)))
               (if (zerop a)
                   (if (zerop b)
                       (values w0 w3)
                       (let ((candidate (/ (- c) b)))
                         (if (or (<= candidate 0.0)
                                 (>= candidate 1.0))
                             (values w0 w3)
                             (let ((w (evaluate-bezier w0 w1 w2 w3 candidate)))
                               (values (min w w0) (max w w3))))))
                   (multiple-value-bind (candidate0 candidate1)
                       (solve-quadratic a b c :multiple-roots t)
                     (if (null candidate0)
                         (values w0 w3)
                         (let ((wa (evaluate-bezier w0 w1 w2 w3 candidate0))
                               (wb (evaluate-bezier w0 w1 w2 w3 candidate1)))
                           (if (or (<= candidate0 0.0) (>= candidate0 1.0))
                               (if (or (<= candidate1 0.0) (>= candidate1 1.0))
                                   (values w0 w3)
                                   (values (min wb w0) (max wb w3)))
                               (if (or (<= candidate1 0.0) (>= candidate1 1.0))
                                   (values (min wa w0) (max wa w3))
                                   (values (min wa wb w0) (max wa wb w3)))))))))))
    (let* ((points (bezigon-points object))
           (p0 (car points))
           (minx (point-x p0))
           (miny (point-y p0))
           (maxx minx)
           (maxy miny))
      (map-over-bezigon-segments
       (lambda (x0 y0 x1 y1 x2 y2 x3 y3)
         (multiple-value-bind (x1 x2) (dimension-min-max x0 x1 x2 x3)
           (minf minx x1)
           (maxf maxx x2))
         (multiple-value-bind (y1 y2) (dimension-min-max y0 y1 y2 y3)
           (minf miny y1)
           (maxf maxy y2)))
       points)
      (values minx miny maxx maxy))))
