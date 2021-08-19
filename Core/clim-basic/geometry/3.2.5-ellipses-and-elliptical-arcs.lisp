;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998-2002 Gilbert Baumann <gbaumann@common-lisp.net>
;;;  (c) copyright 2001 Arnaud Rouanet <rouanet@emi.u-bordeaux.fr>
;;;  (c) copyright 2001 Julien Boninfan
;;;  (c) copyright 2002-2004 Timothy Moore <tmoore@common-lisp.net>
;;;  (c) copyright 2002 Alexey Dejneka
;;;  (c) copyright 2004-2009 Andy Hefner <ahefner@common-lisp.net>
;;;  (c) copyright 2006-2008 Christophe Rhodes <crhodes@common-lisp.net>
;;;  (c) copyright 2014-2016 Robert Strandh <robert.strandh@gmail.com>
;;;  (c) copyright 2017 Peter <craven@gmx.net>
;;;  (c) copyright 2017-2019 Daniel Kochmański <daniel@turtleware.eu>
;;;  (c) copyright 2017,2018 Cyrus Harmon <cyrus@bobobeach.com>
;;;  (c) copyright 2018,2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; 3.2.5 Ellipses and Elliptical Arcs

(in-package #:clim-internals)

;;; Internal protocol
(defgeneric polar->screen (ellipse)
  ;; Specialized on t, we expect that ellipse protocol is implemented.
  (:method (ellipse)
    (nest
     (multiple-value-bind (rdx1 rdy1 rdx2 rdy2) (ellipse-radii ellipse))
     (multiple-value-bind (cx cy) (ellipse-center-point* ellipse))
     (let ((cx (coordinate cx))
           (cy (coordinate cy))
           (rdx1 (coordinate rdx1))
           (rdy1 (coordinate rdy1))
           (rdx2 (coordinate rdx2))
           (rdy2 (coordinate rdy2)))
       (make-3-point-transformation* 0 0 1 0 0 1
                                     cx cy
                                     (+ cx rdx1) (+ cy rdy1)
                                     (+ cx rdx2) (+ cy rdy2))))))

(defclass elliptical-thing (cached-bbox-mixin)
  ((start-angle :initarg :start-angle)
   (end-angle   :initarg :end-angle)
   ;; A transformation from the unit circle to get the elliptical
   ;; object.
   (tr          :initarg :tr :reader polar->screen)))

(defclass standard-ellipse (elliptical-thing ellipse) ())
(defclass standard-elliptical-arc (elliptical-thing elliptical-arc) ())

(defmethod slots-for-pprint-object append ((object elliptical-thing))
  '(start-angle end-angle tr))

(defmethod print-object ((ell elliptical-thing) stream)
  (maybe-print-readably (ell stream)
    (print-unreadable-object (ell stream :type t :identity t)
      (let ((start-angle (ellipse-start-angle ell))
            (end-angle (ellipse-end-angle ell))
            (tr (polar->screen ell)))
        (format stream "[~A ~A] ~A"
                (and start-angle (* (/ 180 pi) start-angle))
                (and end-angle (* (/ 180 pi) end-angle))
                tr)))))

(defun make-ellipse (center-point
                     radius-1-dx radius-1-dy
                     radius-2-dx radius-2-dy
                     &key start-angle end-angle)
  (make-ellipse* (point-x center-point) (point-y center-point)
                 radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                 :start-angle start-angle
                 :end-angle end-angle))

(defun make-ellipse* (center-x center-y
                      radius-1-dx radius-1-dy
                      radius-2-dx radius-2-dy
                      &key start-angle end-angle)
  (make-elliptical-thing 'standard-ellipse
                        center-x center-y
                        radius-1-dx radius-1-dy
                        radius-2-dx radius-2-dy
                        start-angle end-angle))

(defun make-elliptical-arc (center-point
                            radius-1-dx radius-1-dy
                            radius-2-dx radius-2-dy
                            &key start-angle end-angle)
  (make-elliptical-arc* (point-x center-point) (point-y center-point)
                        radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                        :start-angle start-angle
                        :end-angle end-angle))

(defun make-elliptical-arc* (center-x center-y
                             radius-1-dx radius-1-dy
                             radius-2-dx radius-2-dy
                             &key start-angle end-angle)
  (make-elliptical-thing 'standard-elliptical-arc
                        center-x center-y
                        radius-1-dx radius-1-dy
                        radius-2-dx radius-2-dy
                        start-angle end-angle))

(defun make-elliptical-thing (class
                             center-x center-y
                             radius-1-dx radius-1-dy
                             radius-2-dx radius-2-dy
                             start-angle end-angle)
  (setf center-x (coordinate center-x)
        center-y (coordinate center-y)
        radius-1-dx (coordinate radius-1-dx)
        radius-1-dy (coordinate radius-1-dy)
        radius-2-dx (coordinate radius-2-dx)
        radius-2-dy (coordinate radius-2-dy)
        start-angle (and start-angle (coordinate start-angle))
        end-angle (and end-angle (coordinate end-angle)))
  (let ((tr (make-3-point-transformation*
             0 0 1 0 0 1
             center-x center-y
             (+ center-x radius-1-dx) (+ center-y radius-1-dy)
             (+ center-x radius-2-dx) (+ center-y radius-2-dy))))
    (cond ((and (null start-angle) (null end-angle)))
          ((null start-angle) (setf start-angle 0))
          ((null end-angle) (setf end-angle (* 2 pi))))
    (make-instance class :tr tr :start-angle start-angle :end-angle end-angle)))

(defmethod ellipse-center-point* ((region elliptical-thing))
  (let ((tr (polar->screen region)))
    (transform-position tr 0 0)))

(defmethod ellipse-center-point ((region elliptical-thing))
  (let ((tr (polar->screen region)))
    (transform-region tr (make-point 0 0))))

(defmethod ellipse-radii ((region elliptical-thing))
  (let ((tr (polar->screen region)))
    (multiple-value-bind (dx1 dy1) (transform-distance tr 1 0)
      (multiple-value-bind (dx2 dy2) (transform-distance tr 0 1)
        (values dx1 dy1 dx2 dy2)))))

(defmethod ellipse-start-angle ((region elliptical-thing))
  (slot-value region 'start-angle))

(defmethod ellipse-end-angle ((region elliptical-thing))
  (slot-value region 'end-angle))

(defmethod region-contains-position-p ((region elliptical-arc) x y)
  (flet ((position-contains-p (polar->screen)
           (multiple-value-bind (polar-x polar-y)
               (untransform-position polar->screen x y)
             ;; FIXME we don't need to factor the additional epsilon
             ;; but rotated elliptoids are naively rendered in clx.
             (multiple-value-bind (polar-dx polar-dy)
                 (untransform-distance polar->screen 1 1)
               (let ((point-radii (+ (square polar-dx) (square polar-dy))))
                 (coordinate-between* (- 1 point-radii)
                                      (+ (square polar-x) (square polar-y))
                                      (+ 1 point-radii)))))))
    (if-let ((alpha (ellipse-start-angle region)))
      (and (multiple-value-bind (cx cy) (ellipse-center-point* region)
             (arc-contains-point-p alpha (ellipse-end-angle region) (- x cx) (- y cy)))
           (position-contains-p (polar->screen region)))
      (position-contains-p (polar->screen region)))))

(defmethod region-contains-position-p ((region ellipse) x y)
  (flet ((position-contains-p (polar->screen)
           (multiple-value-bind (polar-x polar-y)
               (untransform-position polar->screen x y)
             ;; FIXME we don't need to factor the additonal epsilon
             ;; but rotated elliptoids are naively rendered in clx.
             (multiple-value-bind (polar-dx polar-dy)
                 (untransform-distance polar->screen 1 1)
               (coordinate<= (+ (square polar-x) (square polar-y))
                             (+ 1 (square polar-dx) (square polar-dy)))))))
    (if-let ((alpha (ellipse-start-angle region)))
      (and (multiple-value-bind (cx cy) (ellipse-center-point* region)
             (arc-contains-point-p alpha (ellipse-end-angle region) (- x cx) (- y cy)))
           (position-contains-p (polar->screen region)))
      (position-contains-p (polar->screen region)))))

(defun ellipse-bounding-rectangle (el)
  ;; Return bounding rectangle of ellipse centered at (0, 0) with
  ;; radii h and v rotated by the angle phi.
  (multiple-value-bind (cx cy h v phi) (ellipse-simplified-representation el)
    (let* ((sin (sin phi))
           (cos (cos phi))
           (ax (+ (expt (* v sin) 2)
                  (expt (* h cos) 2)))
           (ay (+ (expt (* v cos) 2)
                  (expt (* h sin) 2)))
           (numerator-x (- (* ax h h v v)))
           (numerator-y (- (* ay h h v v)))
           (denominator-common (expt (* cos
                                        sin
                                        (- (* v v) (* h h)))
                                     2))
           (x (sqrt (/ numerator-x
                       (- denominator-common
                          (* ax (+ (expt (* v cos) 2)
                                   (expt (* h sin) 2)))))))
           (y (sqrt (/ numerator-y
                       (- denominator-common
                          (* ay (+ (expt (* v sin) 2)
                                   (expt (* h cos) 2))))))))
      (values (- cx x) (- cy y) (+ cx x) (+ cy y)))))

(defmethod bounding-rectangle* ((region elliptical-thing))
  (let ((tr (polar->screen region))
        (start-angle (ellipse-start-angle region))
        (end-angle (ellipse-end-angle region)))
    (multiple-value-bind (cx cy) (ellipse-center-point* region)
      (if (every #'zerop (multiple-value-list (ellipse-radii region)))
          (values cx cy cx cy)
          (ellipse-bounding-rectangle region)))))

(defmethod transform-region (transformation (region elliptical-thing))
  (let ((tr (polar->screen region))
        (start-angle (ellipse-start-angle region))
        (end-angle (ellipse-end-angle region)))
    ;; I think this should be untransform-angle below, as the ellipse angles
    ;; go counter-clockwise in screen coordinates, whereas our transformations
    ;; rotate clockwise..  -Hefner
    (let ((start-angle* (and start-angle
                             (untransform-angle transformation start-angle)))
          (end-angle*   (and end-angle
                             (untransform-angle transformation end-angle))))
      (when (reflection-transformation-p transformation)
        (rotatef start-angle* end-angle*))
      (make-instance (type-of region) :tr (compose-transformations transformation tr)
                                      :start-angle start-angle*
                                      :end-angle end-angle*))))
