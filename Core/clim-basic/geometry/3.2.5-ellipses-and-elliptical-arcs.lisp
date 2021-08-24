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
;;;  (c) copyright 2017,2018 Cyrus Harmon <cyrus@bobobeach.com>
;;;  (c) copyright 2018,2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;  (c) copyright 2017-2021 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; 3.2.5 Ellipses and Elliptical Arcs
;;;

;;; A representation of the ellipse with its conjugate diameters is advertised
;;; as a good thing in CLIM spec but in practice it is troublesome because most
;;; algorithms in the literature are specified to work on ellipses with
;;; axes. Internally McCLIM represents an ellipse as its center, axes, rotation
;;; and angles.

(in-package #:clim-internals)

(defclass elliptical-thing (cached-bbox-mixin)
  ((center-point
    :initarg :center-point
    :reader ellipse-center-point)
   (conjugate-diamater-1
    :initarg :conjugate-diameter-1
    :reader ellipse-conjugate-diameter-1)
   (conjugate-diamater-2
    :initarg :conjugate-diameter-2
    :reader ellipse-conjugate-diameter-2)
   (start-angle
    :initarg :start-angle
    :reader ellipse-start-angle)
   (end-angle
    :initarg :end-angle
    :reader ellipse-end-angle)
   ;; Redundant user-friendly representation.
   (theta
    :initarg :theta
    :reader ellipse-rotation)
   (radius-x
    :initarg :radius-x
    :reader ellipse-radius-x)
   (radius-y
    :initarg :radius-y
    :reader ellipse-radius-y)
   ;; Unit circle to the elliptical object transformation.
   (polar->screen
    :initarg :polar->screen
    :reader polar->screen)))

(defclass standard-ellipse        (elliptical-thing ellipse) ())
(defclass standard-elliptical-arc (elliptical-thing elliptical-arc) ())

(defmethod print-object ((ell elliptical-thing) stream)
  (print-unreadable-object (ell stream :type t :identity nil)
    (with-slots (center-point radius-x radius-y theta start-angle end-angle) ell
      (format stream "[~,2f ~,2f] :RX ~,2f :RY ~,2f :THETA ~A°"
              (point-x center-point)
              (point-y center-point)
              radius-x radius-y (round (* (/ 180 pi) theta)))
      (when (and start-angle end-angle)
        (format stream " :ETA1 ~A° :ETA2 ~A°"
                (round (* (/ 180 pi) start-angle))
                (round (* (/ 180 pi) end-angle)))))))

;;; 2.5.6.1 Constructor Functions for Ellipses and Elliptical Arcs in CLIM

(defun make-elliptical-thing (class cx cy rdx1 rdy1 rdx2 rdy2 start end)
  (multiple-value-bind (rx ry theta cdx1 cdy1 cdx2 cdy2)
      (ellipse-normalized-representation* rdx1 rdy1 rdx2 rdy2)
    (if (null rx)
        +nowhere+
        (make-instance class
         :center-point (make-point cx cy)
         :conjugate-diameter-1 (make-point cdx1 cdy1)
         :conjugate-diameter-2 (make-point cdx2 cdy2)
         :start-angle start :end-angle end
         :theta theta :radius-x rx :radius-y ry
         :polar->screen (%polar->screen cx cy cdx1 cdy1 cdx2 cdy2)))))

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
                         radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                         start-angle end-angle))


;;; 2.5.6.2 Accessors for CLIM Elliptical Objects

(defmethod ellipse-center-point* ((object elliptical-thing))
  (point-position (ellipse-center-point object)))

(defmethod ellipse-radii ((object elliptical-thing))
  (multiple-value-bind (rdx1 rdy1)
      (point-position (ellipse-conjugate-diameter-1 object))
    (multiple-value-bind (rdx2 rdy2)
        (point-position (ellipse-conjugate-diameter-2 object))
      (values rdx1 rdy1 rdx2 rdy2))))

;;; Remaining protocols

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
             (arc-contains-point-p alpha (ellipse-end-angle region)
                                   (- x cx) (- y cy)))
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
             (arc-contains-point-p alpha (ellipse-end-angle region)
                                   (- x cx) (- y cy)))
           (position-contains-p (polar->screen region)))
      (position-contains-p (polar->screen region)))))

(defmethod bounding-rectangle* ((region ellipse))
  (multiple-value-call #'ellipse-bounding-rectangle*
    (ellipse-center-point* region)
    (ellipse-radii region)
    (ellipse-start-angle region)
    (ellipse-end-angle region)
    t))

(defmethod bounding-rectangle* ((region elliptical-arc))
  (multiple-value-call #'ellipse-bounding-rectangle*
    (ellipse-center-point* region)
    (ellipse-radii region)
    (ellipse-start-angle region)
    (ellipse-end-angle region)
    nil))

(defmethod transform-region (tr (region elliptical-thing))
  (multiple-value-bind (cx cy) (ellipse-center-point* region)
    (multiple-value-bind (rdx1 rdy1 rdx2 rdy2)
        (ellipse-radii region)
      (multiple-value-bind (cx cy rdx1 rdy1 rdx2 rdy2 eta1 eta2)
          (transform-ellipse tr cx cy rdx1 rdy1 rdx2 rdy2
                             (ellipse-start-angle region)
                             (ellipse-end-angle region))
        (make-elliptical-thing (class-of region)
                               cx cy rdx1 rdy1 rdx2 rdy2 eta1 eta2)))))
