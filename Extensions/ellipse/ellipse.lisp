;;; -*- Mode: Lisp; Package: MCCLIM-ECLIPSE -*-

;;;  (c) copyright 2017 by
;;;           Cyrus Harmon (cyrus@bobobeach.com)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package #:mcclim-ellipse)

;;; Ellipses

;;;
;;; Many backends, such as PDF and PostScript don't provide shape
;;; drawing functions per se, but rather primitives for working with
;;; paths such as lines and bezier curves. One can closely approximate
;;; arbitrary ellipse path with appropriate bezier curves. A good
;;; primer on drawing ellipses with lines, or quadratic or cubic
;;; bezier curves can be found here:
;;;
;;; <https://www.spaceroots.org/documents/ellipse/index.html>.
;;;
;;; We use the algorithm described in the above reference to construct
;;; cubic bezier curves.

;;;
;;; this probably exists elsewhere in McCLIM and, either way, could be
;;; replaced with in-line expansion, or a simple macro. But efficiency
;;; isn't a key concern here so we'll leave this here for readability.

;;;
;;; first some math utlitily functions
(defun square (x)
  "Returns the number X squared."
  (* x x))

(defun normalize-angle (angle)
  "Takes an angle ANGLE and returns the corresponding non-negative angle
less than or equal to 2pi. Note that 4pi would be normalized to 0, not
2pi, but 2pi is normalized to 2pi."
  (if (or (< angle 0)
          (> angle (* pi 2)))
      (mod angle (* pi 2))
      angle))

(defun find-angle* (x1 y1 x2 y2)
  "Returns the angle between two vectors described by x1, y1 and x2,
y2."
  (let ((theta (- (phase (complex y2 x2))
                  (phase (complex y1 x1)))))
    (normalize-angle theta)))

;;
;; CLIM describes general (not neccessarily axis-aligned) ellipses by
;; their center and two vectors describing the radii. The formulation
;; for general ellipses described by Luc Maisonobe, referenced above,
;; computes the ellipse paths based on the center, the length of each
;; radius vectors, and the angle between the two radii. Just as a
;; circle could be (over-)described by its center and a specific 2-d
;; radius, but only the length of the radius is needed to definitively
;; describe the circle, any two (non-colinear) radius vectors could
;; describe a general ellipse. Maisonobe describes an ellipse by two
;; vectors, the semi-major axis, the semi-minor axis at a right angle
;; to the semi-major axis, and the angle of the semi-major axis
;; relative to the positive x-axis. So, given two radii, we call the
;; code in clim-basic/region.lisp that gives a, b, and theta.
(defun reparameterize-ellipse (radius1-dx radius1-dy radius2-dx radius2-dy)
  "Returns three values, the length of radius 1, the length of radius
2, and the angle (CCW in cartesian coordinates) between the two
vectors."

  (let ((ell (climi::make-ellipse* 0 0 radius1-dx radius1-dy radius2-dx radius2-dy)))
    (multiple-value-bind (cx cy a b theta)
        (climi::ellipse-simplified-representation ell)
      (declare (ignore cx cy))
      (values a b theta))))

(defun ellipse-point (lambda0 center-x center-y a b theta)
  "Given an ellipse having center CENTER-X, CENTER-Y, and two radii of
length A and B, with angle THETA between the radii from the center of
the ellipse, returns two values, the x and y coordinates of a point on
the ellipse having angle LAMBDA0 (CCW) relative to the major axis of the
ellipse."
  (let ((eta (atan (/ (sin lambda0) b)
                   (/ (cos lambda0) a))))
    (values (+ center-x
               (* a (cos theta) (cos eta))
               (- (* b (sin theta) (sin eta))))
            (+ center-y
               (* a (sin theta) (cos eta))
               (* b (cos theta) (sin eta))))))

(defun ellipse-point* (lambda0
                       center-x center-y
                       radius1-dx radius1-dy radius2-dx radius2-dy)
  "Given an ellipse having center CENTER-X, CENTER-Y, and two radii,
one described by RADIUS1-DX and RADIUS1-DY, and the other described by
RADIUS2-DX and RADIUS2-DY, returns two values, the x and y coordinates
of a point on the ellipse having angle LAMBDA0 (CCW) relative to the
positive direction of the line parallel to the x-axis that runs
through the center of the ellipse. Note that this parameterization of
LAMBDA0 is different from that used in ELLIPSE-POINT, which is relative
to the major axis."
  (multiple-value-bind (a b theta)
      (reparameterize-ellipse radius1-dx radius1-dy radius2-dx radius2-dy)
    (ellipse-point (- lambda0 theta) center-x center-y a b theta)))

(defun ellipse-derivative (eta a b theta)
  "Given an ellipse having two radii of length A and B, with angle
THETA between the radii from the center of the ellipse, returns two
values, the x and y coordinates of the derivative of the parametric
curve of ellipse at the parametric angle eta. Note that this eta is
not the angle ANGLE, as in ellipse-derivative*, but rather is computed
parametricly from theta. See the paper from Luc Maisonobe for
details."
  (values (+ (- (* a (cos theta) (sin eta)))
             (- (* b (sin theta) (cos eta))))
          (+ (- (* a (sin theta) (sin eta)))
             (* b (cos theta) (cos eta)))))

(defun ellipse-cubic-bezier-control-points (lambda1 lambda2 a b theta)
  "Given two angles, LAMBDA1 and LAMBDA2 of an ellipse having two
radii of length A and B, with angle THETA between the radii from the
center of the ellipse, returns 4 values, the relative x and y
distances of two control points from each of two edge points of a
quadratic bezier curve approximating the ellipse."
  (let ((eta1 (atan (/ (sin lambda1) b)
                    (/ (cos lambda1) a)))
        (eta2 (atan (/ (sin lambda2) b)
                    (/ (cos lambda2) a))))
    (let ((alpha (* (sin (- eta2 eta1))
                    (/ (- (sqrt (+ 4 (* 3 (square (tan (/ (- eta2 eta1) 2)))))) 1)
                       3))))
      (multiple-value-bind (e1x e1y)
          (ellipse-derivative eta1 a b theta)
        (multiple-value-bind (e2x e2y)
            (ellipse-derivative eta2 a b theta)
          (values (* alpha e1x)
                  (* alpha e1y)
                  (* alpha e2x)
                  (* alpha e2y)))))))

(defun ellipse-cubic-bezier-control-points* (lambda1 lambda2
                                             radius1-dx radius1-dy
                                             radius2-dx radius2-dy)
  "Given an ellipse having center CENTER-X, CENTER-Y, and two radii,
one described by RADIUS1-DX and RADIUS1-DY, and the other described by
RADIUS2-DX and RADIUS2-DY, returns four values corresponding to x1,
y1, and x2, y2, of the two control points of a cubic bezier curve
approximation of the elliptical arc from angle lambda1 to lambda2."
  (multiple-value-bind (a b theta)
      (reparameterize-ellipse radius1-dx radius1-dy radius2-dx radius2-dy)
    (let ((lambda1 (- lambda1 theta))
          (lambda2 (- lambda2 theta)))
      (ellipse-cubic-bezier-control-points lambda1 lambda2 a b theta))))

(defun ellipse-cubic-bezier-points (lambda1 lambda2
                                    center-x center-y
                                    a b theta)
  "Returns 8 values, the x and y points of ellipse point 1, control
point 1, control point 2 and ellipse point 2 of a cubic bezier curve
approximating the elliptical arc from angle lambda1 to lambda2 of the
ellipse having center CENTER-X, CENTER-Y, and two radii of length A
and B, with angle THETA between the radii from the center of the
ellipse."
  (multiple-value-bind (p1x p1y)
      (ellipse-point lambda1 center-x center-y a b theta)
    (multiple-value-bind (p2x p2y)
        (ellipse-point lambda2 center-x center-y a b theta)
      (multiple-value-bind (e1x e1y e2x e2y)
          (ellipse-cubic-bezier-control-points lambda1 lambda2 a b theta)
        (values p1x p1y
                (+ p1x e1x) (+ p1y e1y)
                (- p2x e2x) (- p2y e2y)
                p2x p2y)))))

(defun ellipse-cubic-bezier-points* (lambda1 lambda2
                                     center-x center-y
                                     radius1-dx radius1-dy
                                     radius2-dx radius2-dy)
  "Returns 8 values, the x and y points of ellipse point 1, control
point 1, control point 2 and ellipse point 2 of a cubic bezier curve
approximating the elliptical arc from angle lambda1 to lambda2 of the
ellipse having center CENTER-X, CENTER-Y, and two radii, one described
by RADIUS1-DX and RADIUS1-DY, and the other described by RADIUS2-DX
and RADIUS2-DY"
  (multiple-value-bind (p1x p1y)
      (ellipse-point* lambda1 center-x center-y
                      radius1-dx radius1-dy radius2-dx radius2-dy)
    (multiple-value-bind (p2x p2y)
        (ellipse-point* lambda2 center-x center-y
                        radius1-dx radius1-dy radius2-dx radius2-dy)
      (multiple-value-bind (e1x e1y e2x e2y)
          (ellipse-cubic-bezier-control-points* lambda1 lambda2
                                                radius1-dx radius1-dy radius2-dx radius2-dy)
        (values p1x p1y
                (+ p1x e1x) (+ p1y e1y)
                (- p2x e2x) (- p2y e2y)
                p2x p2y)))))

(defun transformation-angle (tr)
  "Returns the angle (in radians) described by the given
transformation."
  ;; We can't just use the parameters of the transformation for this,
  ;; so we take two orthogonal vectors, transform them with tr and
  ;; compute the angle between the two transformed vectors.
  (let ((x1 0) (y1 1)
        (x2 1) (y2 0))
    (multiple-value-bind (tx1 ty1)
        (transform-position tr x1 y1)
      (multiple-value-bind (tx2 ty2)
          (transform-position tr x2 y2)
        (+ (find-angle* tx1 ty1 tx2 ty2))))))

(defun transform-angle (tr angle)
  "Returns the angle corresponding to the direction of a vector having
angle ANGLE after transforming the vector with TR. Note that this is
independent of the specific (non-zero) vector and only the
transformation and angle are needed."
  (+ angle (transformation-angle tr)))

