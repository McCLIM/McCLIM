;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)

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

(in-package :CLIM-INTERNALS)

;;; REGION class

(eval-when (eval load compile)

  (defclass region (design)
    ()
    )
  )

(defun regionp (x)
  (typep x 'region))

(defvar +everywhere+ (make-instance 'region))

(defvar +nowhere+ (make-instance 'region))

(defmethod bounding-rectangle* ((region region))
  (let ((pts (region-points region)))
    (loop for (x y) on pts by #'cddr
	minimize x into min-x
	minimize y into min-y
	maximize x into max-x
	maximize y into max-y
	finally (return (values min-x min-y max-x max-y)))))

(defmethod bounding-rectangle ((region region))
  (multiple-value-bind (min-x min-y max-x max-y) (bounding-rectangle* region)
    (make-rectangle* min-x min-y max-x max-y)))

(defmethod region-intersection ((r1 (eql +nowhere+)) (r2 region))
  +nowhere+)

(defmethod region-intersection ((r1 region) (r2 (eql +nowhere+)))
  +nowhere+)

(defmethod region-intersection ((r1 (eql +everywhere+)) (r2 region))
  r2)

(defmethod region-intersection ((r1 region) (r2 (eql +everywhere+)))
  r1)

(defmethod region-intersection ((r1 (eql +nowhere+)) (r2 (eql +everywhere+)))
  +nowhere+)

(defmethod region-intersection ((r1 (eql +everywhere+)) (r2 (eql +nowhere+)))
  +nowhere+)

(defmethod region-intersection ((r1 region) (r2 region))
  +nowhere+)


;;; PATH region class - non closed regions

(defclass path (region bounding-rectangle)
  ((points :initform nil
	     :initarg :points
	     :reader region-points)
     )
   )

(defun pathp (x)
  (typep x 'path))

;;; AREA region class - closed regions

(defclass area (region bounding-rectangle)
  ((points :initform nil
	   :initarg :points
	   :reader region-points)
   ))

(defun areap (x)
  (typep x 'area))

(deftype coordinate (x)
  (typep x 'single-float))

(defmethod region-equal ((r1 region) (r2 region))
  (let ((p1 (region-points r1))
	(p2 (region-points r2)))
    (and (= (length p1) (length p2))
	 (not (null (search p1 (append p2 p2)))))))

(defmethod region-contains-region-p ((r1 region) (r2 region))
  (null (set-difference (region-points r2) (region-points r1))))

(defmethod region-contains-position-p ((r1 region) x y)
  (declare (ignore x y))
  nil)

(defun on-segment-p (x y x1 y1 x2 y2)
  (or (and (= x x1)
	   (= y y1))
      (and (= x x2)
	   (= y y2))))

(defmethod region-contains-position-p ((r1 path) x y)
  (loop for (x1 y1 x2 y2) on (region-points r1) by #'cddr
      until (null x2)
      if (on-segment-p x y x1 y1 x2 y2)
	 return t
      finally (return nil)))

(defmethod region-contains-position-p ((r1 area) x y)
  (declare (ignore x y))
  nil)

(defmethod region-intersects-region-p ((r1 region) (r2 region))
  (not (eq (region-intersection r1 r2) +nowhere+)))

;;; POINT region class

(defclass point (region bounding-rectangle)
  ((x :initarg :x
      :reader point-x
      :initform 0)
   (y :initarg :y
      :reader point-y
      :initform 0)
   ))

(defun pointp (x)
  (typep x 'point))

(defmethod region-points ((point point))
  (vector point))

(defmethod point-position ((point point))
  (values (point-x point) (point-y point)))

(defclass standard-point (point)
  (
   ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-point (x y)
    (make-instance 'standard-point :x (coerce x 'short-float) :y (coerce y 'short-float)))
  )

(defconstant +origin+ (make-point 0.0 0.0))

;;; LINE region class

(defclass line (path)
  (
   ))

(defun linep (x)
  (typep x 'line))

(defmethod line-start-point* ((line line))
  (point-position (line-start-point line)))

(defmethod line-end-point* ((line line))
  (point-position (line-end-point line)))

(defmethod line-start-point ((line line))
  (elt (region-points line) 0))

(defmethod line-end-point ((line line))
  (elt (region-points line) 1))

(defclass standard-line (line)
  (
   ))

(defun make-line (start-point end-point)
  (make-instance 'standard-line
    :points (vector start-point end-point)))

(defun make-line* (start-x start-y end-x end-y)
  (make-instance 'standard-line
    :points (list (coerce start-x 'short-float) (coerce start-y 'short-float)
		  (coerce end-x 'short-float) (coerce end-y 'short-float))))

;;; RECTANGLE region class

(defclass rectangle (area)
  (
   ))

(defun rectanglep (x)
  (typep x 'rectangle))

(defmethod rectangle-min-x ((rectangle rectangle))
  (min (elt (region-points rectangle) 0)
       (elt (region-points rectangle) 2)
       (elt (region-points rectangle) 4)
       (elt (region-points rectangle) 6)))

(defmethod rectangle-min-y ((rectangle rectangle))
  (min (elt (region-points rectangle) 1)
       (elt (region-points rectangle) 3)
       (elt (region-points rectangle) 5)
       (elt (region-points rectangle) 7)))

(defmethod rectangle-max-x ((rectangle rectangle))
  (max (elt (region-points rectangle) 0)
       (elt (region-points rectangle) 2)
       (elt (region-points rectangle) 4)
       (elt (region-points rectangle) 6)))

(defmethod rectangle-max-y ((rectangle rectangle))
  (max (elt (region-points rectangle) 1)
       (elt (region-points rectangle) 3)
       (elt (region-points rectangle) 5)
       (elt (region-points rectangle) 7)))

(defmethod rectangle-edges* ((rectangle rectangle))
  (values (rectangle-min-x rectangle)
	  (rectangle-min-y rectangle)
	  (rectangle-max-x rectangle)
	  (rectangle-max-y rectangle)))

(defmethod bounding-rectangle* ((rectangle rectangle))
  (rectangle-edges* rectangle))

(defmethod rectangle-min-point ((rectangle rectangle))
  (make-point (rectangle-min-x rectangle) (rectangle-min-y rectangle)))

(defmethod rectangle-max-point ((rectangle rectangle))
  (make-point (rectangle-max-x rectangle) (rectangle-max-y rectangle)))

(defmethod rectangle-width ((rectangle rectangle))
  (multiple-value-bind (x-min y-min x-max y-max) (rectangle-edges* rectangle)
    (declare (ignore y-min y-max))
    (- x-max x-min)))

(defmethod rectangle-height ((rectangle rectangle))
  (multiple-value-bind (x-min y-min x-max y-max) (rectangle-edges* rectangle)
    (declare (ignore x-min x-max))
    (- y-max y-min)))

(defmethod rectangle-size ((rectangle rectangle))
  (multiple-value-bind (x-min y-min x-max y-max) (rectangle-edges* rectangle)
    (values (- x-max x-min) (- y-max y-min))))

(defclass standard-rectangle (rectangle)
  (
   ))

(defun make-rectangle (point1 point2)
  (make-instance 'standard-rectangle
    :points (list (point-x point1) (point-y point1)
		  (point-x point2) (point-y point1)
		  (point-x point2) (point-y point2)
		  (point-x point1) (point-y point2))))

(defun make-rectangle* (x1 y1 x2 y2)
  (setq x1 (coerce x1 'short-float)
	y1 (coerce y1 'short-float)
	x2 (coerce x2 'short-float)
	y2 (coerce y2 'short-float))
  (make-instance 'standard-rectangle
    :points (list x1 y1 x2 y1 x2 y2 x1 y2)))

