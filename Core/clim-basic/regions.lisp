;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-INTERNALS; -*-
;;; ----------------------------------------------------------------------------
;;;     Title: The CLIM Region Datatype
;;;   Created: 1998-12-02 19:26
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;;       $Id: regions.lisp,v 1.39 2009/06/03 20:33:16 ahefner Exp $
;;; ----------------------------------------------------------------------------
;;;  (c) copyright 1998,1999,2001 by Gilbert Baumann
;;;  (c) copyright 2001 by Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;  (c) copyright 2014 by Robert Strandh (robert.strandh@gmail.com)

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

;;; ---- TODO ------------------------------------------------------------------

;;; - ellipses: The intersection of two ellipses is there, but
;;;   handling the start/end angle is not implemented.

;;; - This code is anything else than well organized.

;;; - provide better (faster) implementations for REGION-EQUAL,
;;;   REGION-CONTAINS-REGION-P, and REGION-INTERSECTS-REGION-P.

;;; - Compute a union/intersection/difference of an union of polygon
;;;   vs another polygon or union of polygons directly via POLYGON-OP.

;;; - STANDARD-REGION-UNION should either become a subclass
;;;   'STANDARD-DISJUNCT-REGION-UNION' or a flag. Some set operations
;;;   could take advantage out the information, if the subregions of
;;;   an union are disjunct.

;;; - provide sensible PRINT-OBJECT methods.

;;; - while you are are at it; provide a reasonable fast vertical scan
;;;   routine.  polygons should make use of the sweep line algorithm.

;;; - implement bounding rectangle cache for polygons and polylines

;;; - make REGION-CONTAINS-POSITION-P for polygons faster by handling
;;;   the special case of the intersection of a horizontal line and
;;;   the polygons

;;; - MAKE-POLY{LINE,GON} should canonise its arguments; no edges of
;;;   length 0 and no co-linear vertexes. Maybe: canonise rectangles?
;;;   Also a polygon of less than three vertexes is to be considered
;;;   empty aka +nowhere+.

(in-package :clim-internals)

(defclass nowhere-region (region nowhere-mixin) ())
(defclass everywhere-region (region everywhere-mixin) ())

;;; coordinate is defined in coordinates.lisp

(setf +everywhere+ (make-instance 'everywhere-region))
(setf +nowhere+ (make-instance 'nowhere-region))

(defmethod bounding-rectangle* ((x nowhere-region))
  (values 0 0 0 0))

;;; 2.5.1.1 Region Predicates in CLIM

(defgeneric region-equal (region1 region2))
(defgeneric region-contains-region-p (region1 region2))
(defgeneric region-contains-position-p (region x y))
(defgeneric region-intersects-region-p (region1 region2))

;;; 2.5.1.2 Composition of CLIM Regions

(defclass standard-region-union (region-set) 
  ((regions :initarg :regions :reader standard-region-set-regions)))

(defclass standard-region-intersection (region-set) 
  ((regions :initarg :regions :reader standard-region-set-regions)))

(defclass standard-region-difference (region-set) 
  ((a :initarg :a :reader standard-region-difference-a)
   (b :initarg :b :reader standard-region-difference-b)))

;;; Protocol:
(defgeneric region-set-regions (region &key normalize))
(defgeneric map-over-region-set-regions (function region &key normalize))
(defgeneric region-union (region1 region2))
(defgeneric region-intersection (region1 region2)
  (:method :around ((a region) (b region))
           (cond ((ignore-errors (region-contains-region-p a b)) b)
                 ((ignore-errors (region-contains-region-p b a)) a)
                 (t (call-next-method)))))
(defgeneric region-difference (region1 region2))

;;; -- 2.5.2 CLIM Point Objects ----------------------------------------------

(defclass standard-point (point)
  ((x :type coordinate :initarg :x)
   (y :type coordinate :initarg :y)))

(defun make-point (x y)
  (make-instance 'standard-point
    :x (coerce x 'coordinate)
    :y (coerce y 'coordinate)))

(defmethod print-object ((self standard-point) sink)
  (with-slots (x y) self
    (format sink "#<~S ~S ~S>" 'standard-point x y)))

;;; Point protocol: point-position

(defgeneric point-position (point))

(defmethod point-position ((self standard-point))
  (with-slots (x y) self
    (values x y)))

(defmethod point-x ((self point))
  (nth-value 0 (point-position self)))

(defmethod point-y ((self point))
  (nth-value 1 (point-position self)))

(defmethod transform-region (transformation (self standard-point))
  (with-slots (x y) self
    (multiple-value-bind (x* y*) (transform-position transformation x y)
      (make-point x* y*))))

(defmethod region-contains-position-p ((self standard-point) px py)
  (with-slots (x y) self
    (and (coordinate= x px) (coordinate= y py))))

;;; -- 2.5.3 Polygons and Polylines in CLIM ----------------------------------

;; Protocol: 
(defclass standard-polyline (polyline)
  ((points :initarg :points)
   (closed :initarg :closed)))

(defclass standard-polygon (polygon)
  ((points :initarg :points)) )

;;; -- 2.5.3.1 Constructors for CLIM Polygons and Polylines  -----------------

(defun coord-seq->point-seq (sequence)
  (let ((res nil))
    (do-sequence ((x y) sequence)
      (push (make-point x y) res))
    (nreverse res)))

(defun make-polyline (point-seq &key closed)
  (assert (every #'pointp point-seq))
  (setq point-seq (coerce point-seq 'list))
  (cond ((every (lambda (x) (region-equal x (car point-seq)))
                (cdr point-seq))
         +nowhere+)
        (t
         (make-instance 'standard-polyline :points point-seq :closed closed))))

(defun make-polyline* (coord-seq &key closed)
  (make-polyline (coord-seq->point-seq coord-seq) :closed closed))

(defun make-polygon (point-seq)
  (assert (every #'pointp point-seq))
  (setq point-seq (coerce point-seq 'list))
  (cond ((every (lambda (x) (region-equal x (car point-seq)))
                (cdr point-seq))
         +nowhere+)
        (t
         (make-instance 'standard-polygon :points point-seq))))

(defun make-polygon* (coord-seq)
  (make-polygon (coord-seq->point-seq coord-seq)))

(defmethod polygon-points ((self standard-polygon))
  (with-slots (points) self
    points))

(defmethod map-over-polygon-coordinates (fun (self standard-polygon))
  (with-slots (points) self
    (mapc (lambda (p) (funcall fun (point-x p) (point-y p))) points)))

(defmethod map-over-polygon-segments (fun (self standard-polygon))
  (with-slots (points) self
    (do ((q points (cdr q)))
        ((null (cdr q))
         (funcall fun
		  (point-x (car q)) (point-y (car q))
		  (point-x (car points)) (point-y (car points))))
      (funcall fun
	       (point-x (car q)) (point-y (car q))
	       (point-x (cadr q)) (point-y (cadr q))))))

(defmethod polygon-points ((self standard-polyline))
  (with-slots (points) self
    points))

(defmethod map-over-polygon-coordinates (fun (self standard-polyline))
  (with-slots (points) self
    (mapc (lambda (p) (funcall fun (point-x p) (point-y p))) points)))

(defmethod map-over-polygon-segments (fun (self standard-polyline))
  (with-slots (points closed) self
    (do ((q points (cdr q)))
        ((null (cdr q))
         (when closed
           (funcall fun
		    (point-x (car q)) (point-y (car q))
		    (point-x (car points)) (point-y (car points)))))
      (funcall fun (point-x (car q)) (point-y (car q))
	       (point-x (cadr q)) (point-y (cadr q))))))

(defmethod polyline-closed ((self standard-polyline))
  (with-slots (closed) self
    closed))

(defmethod transform-region (transformation (self standard-polyline))
  (with-slots (points closed) self
    (make-polyline
     (mapcar (lambda (p)
	       (multiple-value-bind (x* y*)
		   (transform-position transformation (point-x p) (point-y p))
		 (make-point x* y*)))
	     points)
     :closed closed)))

(defmethod transform-region (transformation (self standard-polygon))
  (with-slots (points) self
    (make-polygon
     (mapcar (lambda (p)
	       (multiple-value-bind (x* y*)
		   (transform-position transformation (point-x p) (point-y p))
		 (make-point x* y*)))
	     points))))

(defmethod region-contains-position-p ((self standard-polyline) x y)
  (setf x (coerce x 'coordinate)
        y (coerce y 'coordinate))
  (block nil
    (map-over-polygon-segments
     (lambda (x1 y1 x2 y2)
       (when (line-contains-point-p* x1 y1 x2 y2 x y)
	 (return t)))
     self)
    nil))

(defun line-contains-point-p* (x1 y1 x2 y2 px py)
  (and (or (<= x1 px x2) (>= x1 px x2))
       (or (<= y1 py y2) (>= y1 py y2))
       (coordinate= (* (- py y1) (- x2 x1))
                    (* (- px x1) (- y2 y1)))))

(defun line-contains-point-p** (x1 y1 x2 y2 px py)
  (coordinate= (* (- py y1) (- x2 x1))
               (* (- px x1) (- y2 y1))))


;;; -- 2.5.4 Lines in CLIM ---------------------------------------------------

;;; Line protocol: line-start-point* line-end-point* 

(defclass standard-line (line)
  ((x1 :type coordinate :initarg :x1)
   (y1 :type coordinate :initarg :y1)
   (x2 :type coordinate :initarg :x2)
   (y2 :type coordinate :initarg :y2)))

(defun make-line (start-point end-point)
  (make-line* (point-x start-point) (point-y start-point)
	      (point-x end-point) (point-y end-point)))

(defun make-line* (start-x start-y end-x end-y)
  (setf start-x (coerce start-x 'coordinate)
        start-y (coerce start-y 'coordinate)
        end-x (coerce end-x 'coordinate)
        end-y (coerce end-y 'coordinate))
  (if (and (coordinate= start-x end-x)
           (coordinate= start-y end-y))
      +nowhere+
    (make-instance 'standard-line :x1 start-x :y1 start-y :x2 end-x :y2 end-y)))

(defmethod line-start-point* ((line standard-line))
  (with-slots (x1 y1 x2 y2) line
    (values x1 y1)))

(defmethod line-end-point* ((line standard-line))
  (with-slots (x1 y1 x2 y2) line
    (values x2 y2)))

(defmethod line-start-point ((line line))
  (multiple-value-bind (x y) (line-start-point* line)
    (make-point x y)))

(defmethod line-end-point ((line line))
  (multiple-value-bind (x y) (line-end-point* line)
    (make-point x y)))

;;; polyline protocol for standard-line's:

(defmethod polygon-points ((line standard-line))
  (with-slots (x1 y1 x2 y2) line
    (list (make-point x1 y1) (make-point x2 y2))))

(defmethod map-over-polygon-coordinates (fun (line standard-line))
  (with-slots (x1 y1 x2 y2) line
    (funcall fun x1 y1)
    (funcall fun x2 y2)))

(defmethod map-over-polygon-segments (fun (line standard-line))
  (with-slots (x1 y1 x2 y2) line
    (funcall fun x1 y1 x2 y2)))

(defmethod polyline-closed ((line standard-line))
  nil)

(defmethod transform-region (transformation (line standard-line))
  (with-slots (x1 y1 x2 y2) line
    (multiple-value-bind (x1* y1*) (transform-position transformation x1 y1)
      (multiple-value-bind (x2* y2*) (transform-position transformation x2 y2)
        (make-line* x1* y1* x2* y2*)))))

(defmethod region-contains-position-p ((self standard-line) x y)
  (multiple-value-bind (x1 y1) (line-start-point* self)
    (multiple-value-bind (x2 y2) (line-end-point* self)
      (line-contains-point-p* x1 y1 x2 y2 x y))))

(defmethod print-object ((self standard-line) sink)
  (with-slots (x1 y1 x2 y2) self
    (format sink "#<~S ~D ~D ~D ~D>" (type-of self) x1 y1 x2 y2)))

;;; -- 2.5.5 Rectangles in CLIM ----------------------------------------------

;;; protocol:
;;;     rectangle-edges*

(defclass standard-rectangle (rectangle)
  ((coordinates :initform (make-array 4 :element-type 'coordinate))))

(defmethod initialize-instance :after ((obj standard-rectangle)
				       &key (x1 0.0d0) (y1 0.0d0)
				       (x2 0.0d0) (y2 0.0d0))
  (let ((coords (slot-value obj 'coordinates)))
    (setf (aref coords 0) x1)
    (setf (aref coords 1) y1)
    (setf (aref coords 2) x2)
    (setf (aref coords 3) y2)))

(defmacro with-standard-rectangle ((x1 y1 x2 y2) rectangle &body body)
  (with-gensyms (coords)
    `(let ((,coords (slot-value ,rectangle 'coordinates)))
       (declare (type (simple-array coordinate (4)) ,coords))
       (let ((,x1 (aref ,coords 0))
	     (,y1 (aref ,coords 1))
	     (,x2 (aref ,coords 2))
	     (,y2 (aref ,coords 3)))
	 (declare (type coordinate ,x1 ,y1 ,x2 ,y2))
	 ,@body))))

(defmacro with-standard-rectangle* ((&key x1 y1 x2 y2) rectangle &body body)
  (with-gensyms (coords)
    `(let ((,coords (slot-value ,rectangle 'coordinates)))
       (declare (type (simple-array coordinate (4)) ,coords))
       (let (,@(and x1 `((,x1 (aref ,coords 0))))
	     ,@(and y1 `((,y1 (aref ,coords 1))))
	     ,@(and x2 `((,x2 (aref ,coords 2))))
	     ,@(and y2 `((,y2 (aref ,coords 3)))))
	 (declare (type coordinate
			,@(and x1 `(,x1))
			,@(and y1 `(,y1))
			,@(and x2 `(,x2))
			,@(and y2 `(,y2))))
	 ,@body))))

(defun make-rectangle (point1 point2)
  (make-rectangle* (point-x point1) (point-y point1)
		   (point-x point2) (point-y point2)))

(defun make-rectangle* (x1 y1 x2 y2)
  (psetq x1 (coerce (min x1 x2) 'coordinate)
         x2 (coerce (max x1 x2) 'coordinate)
         y1 (coerce (min y1 y2) 'coordinate)
	 y2 (coerce (max y1 y2) 'coordinate))
  (if (or (coordinate= x1 x2)
          (coordinate= y1 y2))
      +nowhere+
    (make-instance 'standard-rectangle :x1 x1 :x2 x2 :y1 y1 :y2 y2)))

(defmethod rectangle-edges* ((rect standard-rectangle))
  (with-standard-rectangle (x1 y1 x2 y2)
      rect
    (values x1 y1 x2 y2)))

;;; standard-rectangles are immutable and all that, but we still need
;;; to set their positions and dimensions (in output recording)
(defgeneric* (setf rectangle-edges*) (x1 y1 x2 y2 rectangle))

(defmethod* (setf rectangle-edges*)
  (x1 y1 x2 y2 (rectangle standard-rectangle))
  (let ((coords (slot-value rectangle 'coordinates)))
    (declare (type (simple-array coordinate (4)) coords))
    (setf (aref coords 0) x1)
    (setf (aref coords 1) y1)
    (setf (aref coords 2) x2)
    (setf (aref coords 3) y2))
  (values x1 y1 x2 y2))

(defmethod rectangle-min-point ((rect rectangle))
  (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* rect)
    (declare (ignore x2 y2))
    (make-point x1 y1)))

(defmethod rectangle-min-point ((rect standard-rectangle))
  (with-standard-rectangle* (:x1 x1 :y1 y1)
      rect
    (make-point x1 y1)))

(defmethod rectangle-max-point ((rect rectangle))
  (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* rect)
    (declare (ignore x1 y1))
    (make-point x2 y2)))

(defmethod rectangle-max-point ((rect standard-rectangle))
  (with-standard-rectangle* (:x2 x2 :y2 y2)
      rect
    (make-point x2 y2)))

(defmethod rectangle-min-x ((rect rectangle))
  (nth-value 0 (rectangle-edges* rect)))

(defmethod rectangle-min-x ((rect standard-rectangle))
  (with-standard-rectangle* (:x1 x1)
      rect
    x1))

(defmethod rectangle-min-y ((rect rectangle))
  (nth-value 1 (rectangle-edges* rect)))

(defmethod rectangle-min-y ((rect standard-rectangle))
  (with-standard-rectangle* (:y1 y1)
      rect
    y1))

(defmethod rectangle-max-x ((rect rectangle))
  (nth-value 2 (rectangle-edges* rect)))

(defmethod rectangle-max-x ((rect standard-rectangle))
  (with-standard-rectangle* (:x2 x2)
      rect
    x2))

(defmethod rectangle-max-y ((rect rectangle))
  (nth-value 3 (rectangle-edges* rect)))

(defmethod rectangle-max-y ((rect standard-rectangle))
  (with-standard-rectangle* (:y2 y2)
      rect
    y2))

(defmethod rectangle-width ((rect rectangle))
  (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* rect)
    (declare (ignore y1 y2))
    (- x2 x1)))

(defmethod rectangle-width ((rect standard-rectangle))
  (with-standard-rectangle* (:x1 x1 :x2 x2)
      rect
    (- x2 x1)))

(defmethod rectangle-height ((rect rectangle))
  (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* rect)
    (declare (ignore x1 x2))
    (- y2 y1)))

(defmethod rectangle-height ((rect standard-rectangle))
  (with-standard-rectangle* (:y1 y1 :y2 y2)
      rect
    (- y2 y1)))

(defmethod rectangle-size ((rect rectangle))
  (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* rect)
    (values (- x2 x1) (- y2 y1))))

(defmethod rectangle-size ((rect standard-rectangle))
  (with-standard-rectangle (x1 y1 x2 y2)
      rect
    (values (- x2 x1) (- y2 y1))))

;;; polyline/polygon protocol for STANDARD-RECTANGLEs

(defmethod polygon-points ((rect standard-rectangle))
  (with-standard-rectangle (x1 y1 x2 y2)
      rect
    (list (make-point x1 y1)
          (make-point x1 y2)
          (make-point x2 y2)
          (make-point x2 y1))))


(defmethod map-over-polygon-coordinates (fun (rect standard-rectangle))
  (with-standard-rectangle (x1 y1 x2 y2)
      rect
    (funcall fun x1 y1)
    (funcall fun x1 y2)
    (funcall fun x2 y2)
    (funcall fun x2 y1)))

(defmethod map-over-polygon-segments (fun (rect standard-rectangle))
  (with-standard-rectangle (x1 y1 x2 y2)
      rect
    (funcall fun x1 y1 x1 y2)
    (funcall fun x1 y2 x2 y2)
    (funcall fun x2 y2 x2 y1)
    (funcall fun x2 y1 x1 y1)))

(defmethod transform-region (transformation (rect standard-rectangle))
  (cond ((rectilinear-transformation-p transformation)
	 (with-standard-rectangle (x1 y1 x2 y2)
	       rect
           (multiple-value-bind (x1* y1*)
	       (transform-position transformation x1 y1)
             (multiple-value-bind (x2* y2*)
		 (transform-position transformation x2 y2)
               (make-rectangle* x1* y1* x2* y2*)))))
        (t
         (make-polygon (mapcar (lambda (p) (transform-region transformation p))
                               (polygon-points rect)))) ))

(defmethod region-contains-position-p ((self standard-rectangle) x y)
  (with-standard-rectangle (x1 y1 x2 y2)
      self
    (and (<= x1 (coerce x 'coordinate) x2)
         (<= y1 (coerce y 'coordinate) y2))))

;;; -- 2.5.6 Ellipses and Elliptical Arcs in CLIM ----------------------------

(defclass elliptical-thing ()
  ((start-angle :initarg :start-angle)
   (end-angle   :initarg :end-angle)
   ;; A transformation from the unit circle to get the elliptical
   ;; object.
   (tr          :initarg :tr)))         

(defmethod print-object ((ell elliptical-thing) stream)
  (with-slots (start-angle end-angle tr) ell
    (format stream "#<~A [~A ~A] ~A>"
            (type-of ell)
            (and start-angle (* (/ 180 pi) start-angle))
            (and end-angle (* (/ 180 pi) end-angle))
            tr)))

(defclass standard-ellipse (elliptical-thing ellipse) ())
(defclass standard-elliptical-arc (elliptical-thing elliptical-arc) ())

;;; -- 2.5.6.1 Constructor Functions for Ellipses and Elliptical Arcs in CLIM -

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
  (make-ellipical-thing 'standard-ellipse 
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
  (make-ellipical-thing 'standard-elliptical-arc 
                        center-x center-y
			radius-1-dx radius-1-dy
			radius-2-dx radius-2-dy 
                        start-angle end-angle))

(defun make-ellipical-thing (class 
                             center-x center-y
			     radius-1-dx radius-1-dy
			     radius-2-dx radius-2-dy
                             start-angle end-angle)
  (setf center-x (coerce center-x 'coordinate)
        center-y (coerce center-y 'coordinate)
        radius-1-dx (coerce radius-1-dx 'coordinate)
        radius-1-dy (coerce radius-1-dy 'coordinate)
        radius-2-dx (coerce radius-2-dx 'coordinate)
        radius-2-dy (coerce radius-2-dy 'coordinate)
        start-angle (and start-angle (coerce start-angle 'coordinate))
        end-angle (and end-angle (coerce end-angle 'coordinate)) )

  (let ((tr (make-3-point-transformation*
	     0 0 1 0 0 1
	     center-x center-y
	     (+ center-x radius-1-dx) (+ center-y radius-1-dy)
	     (+ center-x radius-2-dx) (+ center-y radius-2-dy))))
    (cond ((and (null start-angle) (null end-angle)))
          ((null start-angle) (setf start-angle 0))
          ((null end-angle) (setf end-angle (* 2 pi))))
    (make-instance class :tr tr :start-angle start-angle :end-angle end-angle) ))

(defmethod transform-region (transformation (self elliptical-thing))
  (with-slots (start-angle end-angle tr) self
    ;; I think this should be untransform-angle below, as the ellipse angles
    ;; go counter-clockwise in screen coordinates, whereas our transformations
    ;; rotate clockwise..  -Hefner
    (let ((start-angle* (and start-angle
			     (untransform-angle transformation start-angle)))
          (end-angle*   (and end-angle
			     (untransform-angle transformation end-angle))))      
      (when (reflection-transformation-p transformation)
        (rotatef start-angle* end-angle*))
      (make-instance (type-of self)
        :tr (compose-transformations transformation tr)
        :start-angle start-angle*
        :end-angle end-angle*))))

(defun %ellipse-angle->position (ellipse angle)
  (with-slots (tr) ellipse
    (let* ((base-angle (untransform-angle tr (- (* 2 pi) angle)))
           (x0 (cos base-angle))
           (y0 (sin base-angle)))
      (transform-position tr x0 y0))))

(defun %ellipse-position->angle (ellipse x y)
  (multiple-value-bind (xc yc) (ellipse-center-point* ellipse)
    ;; remember, that y-axis is reverted
    (coordinate (atan* (- x xc) (- (- y yc))))))

(defun %angle-between-p (angle start-angle end-angle &aux (eps single-float-epsilon))
  (if (<= start-angle end-angle)
      (<= (- start-angle eps) angle (+ end-angle eps))
      (or (<= (- start-angle eps) angle)
          (<= angle (+ end-angle eps)))))

(defmethod region-contains-position-p ((self standard-ellipse) x-orig y-orig)
  (with-slots (tr start-angle end-angle) self
    (multiple-value-bind (x y) (untransform-position tr x-orig y-orig)
      (and (<= (+ (* x x) (* y y)) (+ 1.0 (* 4 single-float-epsilon)))
           (or (and (zerop y) (zerop x))
               ;; start-angle being null implies that end-angle is null as well
               (null start-angle)
               ;; we check angle in screen coordinates
               (%angle-between-p (%ellipse-position->angle self x-orig y-orig)
                                 start-angle
                                 end-angle))))))

(defmethod bounding-rectangle* ((region standard-ellipse))
  (with-slots (tr start-angle end-angle) region
    (multiple-value-bind (cx cy) (ellipse-center-point* region)
      (if (every #'zerop (multiple-value-list (ellipse-radii region)))
          (values cx cy cx cy)
          (ellipse-bounding-rectangle region)))))

(defun intersection-hline/ellipse (el y)
  "Returns coordinates where ellipse intersects with a horizontal line."
  (multiple-value-bind (cx cy h v phi) (ellipse-simplified-representation el)
    (let* ((y (- y cy))
           (cos (cos phi))
           (sin (sin phi))
           (a (+ (expt (* v cos) 2)
                 (expt (* h sin) 2)))
           (b (* 2 y cos sin
                 (- (* v v) (* h h))))
           (c (- (+ (expt (* y v sin) 2)
                    (expt (* y h cos) 2))
                 (expt (* h v) 2)))
           (dc (sqrt (- (* b b) (* 4 a c))))
           (x1 (/ (- (- b) dc)
                  (* 2 a)))
           (x2 (/ (+ (- b) dc)
                  (* 2 a))))
      (values (+ cx x1) (+ cy y) (+ cx x2) (+ cy y)))))

(defun intersection-vline/ellipse (el x)
  "Returns coordinates where ellipse intersects with a vertical line."
  (multiple-value-bind (cx cy h v phi) (ellipse-simplified-representation el)
    (let* ((x (- x cx))
           (cos (cos phi))
           (sin (sin phi))
           (a (+ (expt (* v sin) 2)
                 (expt (* h cos) 2)))
           (b (* 2 x cos sin
                 (- (* v v) (* h h))))
           (c (- (+ (expt (* x v cos) 2)
                    (expt (* x h sin) 2))
                 (expt (* h v) 2)))
           (dc (sqrt (- (* b b) (* 4 a c))))
           (y1 (/ (- (- b) dc)
                  (* 2 a)))
           (y2 (/ (+ (- b) dc)
                  (* 2 a))))
      (values (+ cx x) (+ cy y1) (+ cx x) (+ cy y2)))))

(defun intersection-line/ellipse (el lx1 ly1 lx2 ly2)
  "Returns coordinates where ellipse intersects with arbitral line (except vertical)."
  (multiple-value-bind (cx cy h v phi) (ellipse-simplified-representation el)
    (let* ((lx1 (- lx1 cx)) (ly1 (- ly1 cy)) (lx2 (- lx2 cx)) (ly2 (- ly2 cy))
           (m-slope (/ (- ly1 ly2) (- lx1 lx2)))
           (b-slope (- ly1 (* m-slope lx1)))
           (cos (cos phi))
           (sin (sin phi))
           (a (+ (* v v
                    (+ (* cos cos)
                       (* 2 m-slope cos sin)
                       (expt (* m-slope sin) 2)))
                 (* h h
                    (+ (expt (* m-slope cos) 2)
                       (* -2 m-slope cos sin)
                       (* sin sin)))))
           (b (+ (* 2 v v b-slope
                    (+ (* cos sin) (* m-slope sin sin)))
                 (* 2 h h b-slope
                    (- (* m-slope cos cos) (* cos sin)))))
           (c (- (* b-slope b-slope
                    (+ (expt (* v sin) 2)
                       (expt (* h cos) 2)))
                 (* h h v v)))
           (dc (sqrt (- (* b b) (* 4 a c))))
           (x1 (/ (- (- b) dc)
                  (* 2 a)))
           (y1 (+ (* m-slope x1) b-slope))
           (x2 (/ (+ (- b) dc)
                  (* 2 a)))
           (y2 (+ (* m-slope x2) b-slope)))
      (values (+ cx x1) (+ cy y1) (+ cx x2) (+ cy y2)))))

(defmethod region-intersection ((line line) (ellipse standard-ellipse))
  (let (p1x p1y p2x p2y)
    (multiple-value-setq (p1x p1y) (line-start-point* line))
    (multiple-value-setq (p2x p2y) (line-end-point* line))
    (let ((region (if (and (region-contains-position-p ellipse p1x p1y)
                           (region-contains-position-p ellipse p2x p2y))
                      line
                      (multiple-value-bind (x1 y1 x2 y2)
                          (cond ((= p1x p2x) (intersection-vline/ellipse ellipse p1x))
                                ((= p1y p2y) (intersection-hline/ellipse ellipse p1y))
                                (t (intersection-line/ellipse ellipse p1x p1y p2x p2y)))
                        (if (some #'complexp (list x1 y1 x2 y2))
                            +nowhere+
                            (make-line* x1 y1 x2 y2))))))
      (with-slots (start-angle end-angle) ellipse
        (when (or (null start-angle) (region-equal region +nowhere+))
          (return-from region-intersection region))
        (multiple-value-bind (cx cy) (ellipse-center-point* ellipse)
          (multiple-value-bind (sx sy) (%ellipse-angle->position ellipse start-angle)
            (multiple-value-bind (ex ey) (%ellipse-angle->position ellipse end-angle)
              (let* ((start-ray (make-line* cx cy sx sy))
                     (end-ray (make-line* cx cy ex ey))
                     (si (region-intersection region start-ray))
                     (ei (region-intersection region end-ray))
                     (sip (not (region-equal +nowhere+ si)))
                     (eip (not (region-equal +nowhere+ ei)))
                     (p1 (line-start-point region))
                     (p2 (line-end-point region))
                     (p1p (multiple-value-call
                              #'region-contains-position-p ellipse (point-position p1)))
                     (p2p (multiple-value-call
                              #'region-contains-position-p ellipse (point-position p2))))
                (cond
                  ;; line goes through the center. Only in this case line may be
                  ;; coincident with angle rays, so we don't have to bother with
                  ;; checking later.
                  ((region-contains-position-p region cx cy)
                   (make-line (if p1p p1 (make-point cx cy))
                              (if p2p p2 (make-point cx cy))))
                  ;; line doesn't intersect any of angle rays
                  ((and (not sip) (not eip))
                   ;; p1p implies p2p here, but rounding may say otherwise
                   (if (or p1p p2p) region +nowhere+))
                  ;; line intersects with both angle rays
                  ((and sip eip)
                   ;; region difference may not work here due to float rounding
                   (let ((guess-line (make-line p1 si)))
                     (if (not (region-intersects-region-p guess-line end-ray))
                         (region-union guess-line (make-line p2 ei))
                         (region-union (make-line p1 ei) (make-line p2 si)))))
                  ;; line intersect only one angle ray
                  (t (make-line (if p1p p1 p2)
                                (if sip si ei))))))))))))

(defmethod region-intersection ((ellipse standard-ellipse) (line standard-line))
  (region-intersection line ellipse))

(defmethod region-contains-region-p ((a standard-ellipse) (b standard-ellipse))
  (multiple-value-bind (bcx bcy) (ellipse-center-point* b)
    (and (region-contains-position-p a bcx bcy)
         (null (intersection-ellipse/ellipse a b))
         (or (null (ellipse-start-angle a))
             (multiple-value-bind (sx sy) (%ellipse-angle->position a (ellipse-start-angle a))
               (multiple-value-bind (ex ey) (%ellipse-angle->position a (ellipse-end-angle a))
                 (multiple-value-bind (cx cy) (ellipse-center-point* a)
                   (and (null (region-intersection b (make-line* sx sy cx cy)))
                        (null (region-intersection b (make-line* ex ey cx cy)))))))))))

;;; Ellipse is a convex object. That Implies that if each of the rectangle
;;; vertexes lies inside it, then whole rectangle fits as well. We take a
;;; special care for ellipses with start/end angle.
(defmethod region-contains-region-p ((a standard-ellipse) (b standard-rectangle))
  (with-standard-rectangle (x1 y1 x2 y2) b
    (if (null (ellipse-start-angle a))
        (and (region-contains-position-p a x1 y1)
             (region-contains-position-p a x2 y1)
             (region-contains-position-p a x1 y2)
             (region-contains-position-p a x2 y2))
        (flet ((fits (l) (region-equal l (region-intersection l a))))
          (and (fits (make-line* x1 y1 x2 y1))
               (fits (make-line* x2 y1 x2 y2))
               (fits (make-line* x2 y2 x1 y2))
               (fits (make-line* x1 y2 x1 y1)))))))

(defmethod region-contains-region-p ((a standard-ellipse) (polygon standard-polygon))
  (if (null (ellipse-start-angle a))
      (map-over-polygon-coordinates
       #'(lambda (x y)
           (unless (region-contains-position-p a x y)
             (return-from region-contains-region-p nil)))
       polygon)
      (map-over-polygon-segments
       #'(lambda (x1 y1 x2 y2
                  &aux (line (make-line* x1 y1 x2 y2)))
           (unless (region-equal line (region-intersection line a))
             (return-from region-contains-region-p nil)))
       polygon))
  T)

;;; -- 2.5.6.2 Accessors for CLIM Elliptical Objects -------------------------

(defmethod ellipse-center-point* ((self elliptical-thing))
  (with-slots (tr) self
    (transform-position tr 0 0)))

(defmethod ellipse-center-point ((self elliptical-thing))
  (with-slots (tr) self
    (transform-region tr (make-point 0 0))))

(defmethod ellipse-radii ((self elliptical-thing))
  (with-slots (tr) self
    (multiple-value-bind (dx1 dy1) (transform-distance tr 1 0)
      (multiple-value-bind (dx2 dy2) (transform-distance tr 0 1)
        (values dx1 dy1 dx2 dy2)))))

(defmethod ellipse-start-angle ((self elliptical-thing))
  (with-slots (start-angle) self 
    start-angle))

(defmethod ellipse-end-angle ((self elliptical-thing))
  (with-slots (end-angle) self 
    end-angle))

(defun ellipse-coefficients (ell)
  ;; Returns the coefficients of the equation specifing the ellipse as in
  ;;  ax^2 + by^2 + cxy + dx + dy - f = 0

  ;; Note 1:
  ;;   The `f' here may seem to be superfluous, since you
  ;;   could simply multiply the whole equation by 1/f. But this is
  ;;   not the case, since `f' may as well be 0.

  ;; Note 2:
  ;;   In the literature you often find something like
  ;;   (x^2)/a + (y^2)/b - 1 = 0 for an axis aligned ellipse, but
  ;;   I rather choose to treat all coefficients as simple factors instead
  ;;   of denominators.

  (with-slots (tr) ell
    ;; Why the inverse here?
    (multiple-value-bind (a b d e c f)
	(get-transformation (invert-transformation tr))
      (values
       (+ (* a a) (* d d))              ; x**2
       (+ (* b b) (* e e))              ; y**2
       (+ (* 2 a b) (* 2 d e))          ; xy
       (+ (* 2 a c) (* 2 d f))          ; x
       (+ (* 2 b c) (* 2 e f))          ; y
       (+ (* c c) (* f f) -1)))) )

;;; Straight from the horse's mouth -- moore
;;;
;;; Axis of an ellipse
;;; -------------------------

;;; Given an ellipse with its center at the origin, as

;;;    ax^2 + by^2 + cxy - 1 = 0

;;; The two axis of an ellipse are characterized by minimizing and
;;; maximizing the radius. Let (x,y) be a point on the delimiter of
;;; the ellipse. It's radius (distance from the origin) then is:

;;;    r^2 = x^2 + y^2

;;; To find the axis can now be stated as an minimization problem with
;;; constraints. So mechanically construct the auxiliarry function H:

;;;   H = x^2 + y^2 - k(ax^2 + by^2 + cxy - 1)

;;; So the following set of equations remain to be solved

;;;   (I)   dH/dx = 0 = 2x + 2kax + kcy 
;;;  (II)   dH/dy = 0 = 2y + 2kby + kcx
;;; (III)   dH/dk = 0 = ax^2 + by^2 + cxy - 1

;;; Unfortunately, as I always do the math work - hopelessly, even -
;;; Maxima is the tool of my choice:

;;; g1: 2*x + 2*k*a*x + k*c*y$
;;; g2: 2*y + 2*k*b*y + k*c*x$
;;; g3: a*x*x + b*y*y + c*x*y -1$

;;; sol1: solve ([g1,g2],[k,y])$

;;; /* This yields two solutions because of the squares with occur. The
;;;  * last equation (G3) must therefore be handled for both solutions for
;;;  * y.
;;;  */

;;; y1: rhs(first(rest(first(sol1))))$
;;; y2: rhs(first(rest(first(rest(sol1)))))$

;;; /* Substitute the 'y' found. */
;;; sol2: solve(subst(y1,y,g3),x);
;;; x11: rhs(first(sol2));
;;; x12: rhs(first(rest(sol2)));

;;; sol3: solve(subst(y2,y,g3),x);
;;; x21: rhs(first(sol3));
;;; x22: rhs(first(rest(sol3)));

;;; /* dump everything */
;;; dumpsol([[x=x11,y=y1], [x=x12,y=y1], [x=x21,y=y2], [x=x22,y=y2]]);

(defun ellipse-normal-radii* (ell)
  (multiple-value-bind (a b c) (ellipse-coefficients ell)
    (cond ((coordinate= 0 c)
           ;; this is the unit circle
           (values  0 (sqrt (/ 1 b))
                    (sqrt (/ 1 a)) 0))
          (t
	   (let* ((x1 (- (/ c
			    (sqrt (+ (- (* (* c c)
					   (sqrt (+ (* c c)
						    (* b b)
						    (- (* 2 a b)) (* a a)))))
				     (- (* 2 (* b b)
					   (sqrt (+ (* c c) (* b b)
						    (- (* 2 a b)) (* a a)))))
				     (* 2 a b (sqrt (+ (* c c) (* b b)
						       (- (* 2 a b))
						       (* a a))))
				     (* 2 b (* c c))
				     (* 2 (expt b 3))
				     (- (* 4 a (* b b))) (* 2 (* a a) b))))))
		  (y1 (- (/ (+ (* (sqrt (+ (* c c)
					   (* b b)
					   (- (* 2 a b))
					   (* a a)))
				  x1)
			       (- (* b x1)) (* a x1)) 
			    c)))
		  (x2 (- (/ c
			    (sqrt (+ (* (* c c)
					(sqrt (+ (* c c)
						 (* b b)
						 (- (* 2 a b))
						 (* a a))))
				     (* 2 (* b b) (sqrt (+ (* c c)
							   (* b b)
							   (- (* 2 a b))
							   (* a a))))
				     (- (* 2 a b (sqrt (+ (* c c)
							  (* b b)
							  (- (* 2 a b))
							  (* a a)))))
				     (* 2 b (* c c))
				     (* 2 (expt b 3))
				     (- (* 4 a (* b b))) (* 2 (* a a) b))))))
		  (y2 (- (/ (+ (- (* (sqrt (+ (* c c)
					      (* b b)
					      (- (* 2 a b))
					      (* a a)))
				     x2))
			       (- (* b x2)) (* a x2))
			    c))))
	     (values x1 y1 x2 y2))))))

;;; this function is used in `ellipse-simplified-representation' to fixup
;;; normalized radius lengths. Can't be interchanged with
;;; `%ellipse-angle->position' because of the rotation inversion.
(defun %ellipse-simplified-representation/radius (ellipse angle)
  (declare (optimize (speed 3)) (inline))
  (with-slots (tr) ellipse
    (let* ((base-angle (untransform-angle tr angle))
           (x (cos base-angle))
           (y (sin base-angle)))
      (with-slots (mxx mxy myx myy tx ty) tr
        (values (+ (* mxx x) (* mxy y) tx)
                (+ (* myx x) (* myy y) ty))))))

(defun ellipse-simplified-representation (el)
  ;; returns H (horizontal radius), V (vertical radius) and rotation angle in
  ;; screen coordinates. `ellipse-normal-radii*' returns vectors with correct
  ;; direction, but radius length is shorter than in reality (verified with
  ;; experimentation, not analitically), so we compute radius from phi.
  ;; If the length were right, we'd compute h/v with the following:
  ;;   (sqrt (+ (expt (* hx (cos phi)) 2) (expt (* hy (sin phi)) 2)))
  ;;   (sqrt (+ (expt (* vx (sin phi)) 2) (expt (* vy (cos phi)) 2)))
  (multiple-value-bind (center-x center-y) (ellipse-center-point* el)
    (multiple-value-bind (hx hy) (ellipse-normal-radii* el)
      (let* ((phi (atan* hx hy)))
        ;(multiple-value-bind (hx hy vx vy) (%ellipse-angle->distance el phi))
        (multiple-value-bind (hx hy)
            (%ellipse-simplified-representation/radius el phi)
          (multiple-value-bind (vx vy)
              (%ellipse-simplified-representation/radius el (+ phi (/ pi 2)))
           (values center-x
                   center-y
                   (sqrt (+ (expt (- center-x hx) 2) (expt (- center-y hy) 2)))
                   (sqrt (+ (expt (- center-x vx) 2) (expt (- center-y vy) 2)))
                   phi)))))))

(defun ellipse-bounding-rectangle (el)
  ;; returns bounding rectangle of ellipse centered at (0, 0) with radii h and v
  ;; rotated by the angle phi.
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

;;; -- Intersection of Ellipse vs. Ellipse -----------------------------------

;;; This entire thing is so incomprehensible, that I have to look for
;;; my notes, to present the derivation for the solution of the
;;; conic section problem.

(defun intersection-ellipse/ellipse (e1 e2)
  ;; We reduce one of the two ellipses to the unit circle.
  (let ((a (invert-transformation (slot-value e1 'tr))))
    (let ((r (intersection-ellipse/unit-circle (transform-region a e2))))
      (if (atom r)
          r
        (mapcar (lambda (p)
                  (multiple-value-bind (x y)
		      (transform-position (slot-value e1 'tr) (car p) (cdr p))
                    (make-point x y)))
                r)))))

(defun intersection-ellipse/unit-circle (ell)
  (multiple-value-bind (a b c d e f) (ellipse-coefficients ell)
    (let ((pn (elli-polynom ell)))
      (cond ((= (length pn) 0)
             :coincident)
            (t
             (let ((ys (newton-iteration pn 0d0))
                   (res nil))
               (dolist (y ys)
                 (let ((x (sqrt (- 1 (* y y)))))
                   (when (realp x)
                     (when (coordinate= 0 (ellipse-equation a b c d e f x y))
                       (pushnew (cons x y) res :test #'equal))
                     (when (coordinate= 0 (ellipse-equation a b c d e f (- x) y))
                       (pushnew (cons (- x) y) res :test #'equal)) )))
               res)) ))))

(defun ellipse-equation (a b c d e f x y)
  (+ (* a x x) (* b y y) (* c x y) (* d x) (* e y) f))

(defun elli-polynom (ell)
  ;; It is rather funny that for two circles we always get a polynomial
  ;; of degree two.
  (multiple-value-bind (a b c d e f) (ellipse-coefficients ell)
    (canonize-polynom
     (vector (+ (* (- b a) (- b a)) (* c c))
             (+ (* 2 b e) (* -2 a e) (* 2 c d))
             (+ (* e e) (* 2 (- b a) (+ a f)) (* -1 c c) (* d d))
             (+ (* 2 e a) (* 2 e f) (* -2 c d))
             (+ (* (+ a f) (+ a f)) (* -1 d d)) ))) )

;;; We just build ourselves a simple newton iteration. Sometimes we fail
;;; desperately at local minima. But apart from that convergence behaviour for
;;; our problem is quite good. But we partly still obtain sizable errors by
;;; dividing at the function roots; I'm trying to alleviate this by executing a
;;; few newton steps (newton-ziel-gerade, meaning "newton home stretch") with
;;; the original polynomial after finding a root.

;;; I shouldn't be so lazy and consult the comprehensive literature; there must
;;; be something better than newton iteration. I vaguely remember a numerics
;;; lecture ...

(defun newton-ziel-gerade (pn x &optional (n 4))
  (cond ((= n 0) x)
        ((multiple-value-bind (f p2) (horner-schema pn x)
           (multiple-value-bind (f*) (horner-schema p2 x)
             (newton-ziel-gerade pn (- x (/ f f*)) (- n 1)))))))

(defun solve-p1 (b c)
  (if (= b 0)
      nil
    (list (- (/ c b)))))

(defun solve-p2 (a b c)
  (cond ((= a 0)
         (solve-p1 b c))
        (t
         (let* ((p (/ b a))
                (q (/ c a))
                (d (- (/ (* p p) 4) q)))
           (cond ((< d 0)
                  nil)
                 ((= d 0)
                  (list (/ p 2)))
                 (t
                  (list (+ (/ p 2) (sqrt d))
                        (- (/ p 2) (sqrt d))))))) ))

(defun maybe-solve-polynom-trivially (pn)
  (case (length pn)
    (0 (values nil t))
    (1 (values nil t))
    (2 (values (solve-p1 (aref pn 0) (aref pn 1)) t))
    (3 (values (solve-p2 (aref pn 0) (aref pn 1) (aref pn 2)) t))
    (t (values nil nil))))

(defun canonize-polynom (pn)
  (cond ((= (length pn) 0) pn)
        ((coordinate= (aref pn 0) 0)
         (canonize-polynom (subseq pn 1)))
        (t pn)))

(defun newton-iteration (polynom x-start)
  ;; ATTENTION: Adapted specifically to our problem, do not use this without
  ;; reading!
  (multiple-value-bind (sol done?) (maybe-solve-polynom-trivially polynom)
    (cond (done?
           sol)
          (t
           (let ((x x-start)
                 x1
                 (n 0)
                 (pn polynom)
                 (eps-f 0d0)
                 (eps-f* 0d-16)
                 (eps-x 1d-20)
                 (m 20)                 ;maximum number of steps
                 (res nil) )
             (loop
               (cond ((> n m)
                      (return)))
               (multiple-value-bind (f p2) (horner-schema pn x)
                 (multiple-value-bind (f*) (horner-schema p2 x)
                   (cond ((<= (abs f*) eps-f*)
                          ;; We are stuck at an extremum -- continue with random
                          ;; starting value
                          (setf x1 (+ 1d0 (random 2d0))))
                         (t
                          (setf x1 (- x (/ f f*)))
                          (cond ((or (<= (abs f) eps-f) 
                                     (<= (abs (- x1 x)) eps-x))
                                 ;; a few more steps of newton, to improve
                                 ;; the result
                                 (setf x1 (newton-ziel-gerade polynom x1))
                                 (push x1 res)
                                 ;; divide (roots)
                                 (multiple-value-bind (f p2) (horner-schema pn x1)
                                   f
                                   (setq pn (canonize-polynom p2))
                                   (multiple-value-bind (sol done?)
				       (maybe-solve-polynom-trivially pn)
                                     (when done?
                                       ;; iterate more nonetheless here -- is
                                       ;; this a good idea?
                                       (setf sol
					     (mapcar (lambda (x)
						       (newton-ziel-gerade
							polynom x))
						     sol))
                                       (setf res (nconc sol res))
                                       (return))))
                                 (setf x1 x-start)
                                 (setq n 0)) ))))
                 (setf x (min 1d0 (max -1d0 x1)))        ;Is this allowed?
                 (incf n)))
             res)) )))

(defun horner-schema (polynom x)
  ;; Evaluates the polynomial `polynom' by means of horner's method at the
  ;; place `x'; returns two values:
  ;; - the value of the function
  ;; - the last line of horner's method (result of division)
  (let ((n (length polynom)))
    (cond ((= n 0) (values 0))
          ((= n 1) (values (aref polynom 0) '#()))
          (t
           (let ((b (make-array (1- n))))
             (setf (aref b 0) (aref polynom 0))
             (do ((i 1 (+ i 1)))
                 ((= i (- n 1)) 
                  (values 
                   (+ (* (aref b (- i 1)) x) (aref polynom i))
                   b))
               (setf (aref b i) (+ (* (aref b (- i 1)) x) (aref polynom i))))))) ))



;;;; ===========================================================================

(defmethod region-union ((a point) (b point))
  (cond ((region-equal a b)
         a)
        (t
         (make-instance 'standard-region-union :regions (list a b)))))

(defmethod region-intersection ((a point) (b point))
  (cond
    ((region-equal a b) a)
    (t +nowhere+)))

(defmethod region-equal ((a point) (b point))
  (and (coordinate= (point-x a) (point-x b))
       (coordinate= (point-y a) (point-y b))))

;;; ============================================================================

;;; -- Rectangle Sets --------------------------------------------------------

(defclass standard-rectangle-set (region-set bounding-rectangle)
  ((bands
    ;; Represents the set of rectangles. This is list like:
    ;;
    ;;  ((<y_1> . <x_band_1>)        
    ;;   (<y_2> . <x_band_2>) 
    ;;   :
    ;;   (<y_n>))
    ;;
    ;; <x_band_i> := (x_i_1 u_i_1  x_i_2 u_i_2 ... x_i_m u_i_m)
    ;;
    ;; Now a point (x,y) is member of the rectangle set, if there is an
    ;; i, such that y member of [y_i, y_(i+1)] and x member of x_band_i.
    ;;
    ;; An x is member of an band i, if there is an j, such that x
    ;; member [x_i_j, u_i_j].
    ;;
    ;; That is <x_band_i> describes the possible x-coordinates in the
    ;; y-range [y_i, y_(i+1)].
    ;;
    :initarg :bands
    :reader  standard-rectangle-set-bands)
   ;;
   (bounding-rectangle 
    ;; Caches the regions bounding-rectangle. Is either NIL or the
    ;; bounding-rectangle, represented by a list (x1 y1 x2 y2).
    :initform nil)))

(defmethod map-over-region-set-regions
    (fun (self standard-rectangle-set) &key normalize)
  (with-slots (bands) self
    (cond ((or (null normalize) (eql normalize :x-banding))
           (map-over-bands-rectangles
	    (lambda (x1 y1 x2 y2)
	      (funcall fun (make-rectangle* x1 y1 x2 y2)))
	    bands))
          ((eql normalize :y-banding)
           (map-over-bands-rectangles
	    (lambda (y1 x1 y2 x2)
	      (funcall fun (make-rectangle* x1 y1 x2 y2)))
	    (xy-bands->yx-bands bands)))
          (t
           (error "Bad ~S argument to ~S: ~S"
                  :normalize 'map-over-region-set-regions normalize)) )))

(defmethod region-set-regions ((self standard-rectangle-set) &key normalize)
  (let ((res nil))
    (map-over-region-set-regions
     (lambda (r) (push r res))
     self :normalize normalize)
    res))

(defun make-standard-rectangle-set (bands)
  (cond ((null bands) +nowhere+)
        ((and (= (length bands) 2)
              (null (cdr (second bands)))
              (= (length (cdr (first bands))) 2))
         (make-rectangle* (first (cdar bands)) (caar bands)
                          (second (cdar bands)) (caadr bands)))
        ((= (length (first bands)) 1)
         (make-standard-rectangle-set (rest bands)))
        (t
         (make-instance 'standard-rectangle-set :bands bands)) ))
    
;;; rectangle-set vs. rectangle-set

(defmethod region-union ((xs standard-rectangle-set) (ys standard-rectangle-set))
  (make-standard-rectangle-set
   (bands-union (standard-rectangle-set-bands xs)
		(standard-rectangle-set-bands ys))))

(defmethod region-intersection ((xs standard-rectangle-set) (ys standard-rectangle-set))
  (make-standard-rectangle-set
   (bands-intersection (standard-rectangle-set-bands xs)
		       (standard-rectangle-set-bands ys))))

(defmethod region-difference ((xs standard-rectangle-set) (ys standard-rectangle-set))
  (make-standard-rectangle-set
   (bands-difference (standard-rectangle-set-bands xs)
		     (standard-rectangle-set-bands ys))))
         
;;; rectangle-set vs. rectangle and vice versa

(defmethod region-union ((xs standard-rectangle-set) (ys standard-rectangle))
  (region-union xs (rectangle->standard-rectangle-set ys)))

(defmethod region-union ((xs standard-rectangle) (ys standard-rectangle-set))
  (region-union (rectangle->standard-rectangle-set xs) ys))

(defmethod region-difference ((xs standard-rectangle-set) (ys standard-rectangle))
  (region-difference xs (rectangle->standard-rectangle-set ys)))

(defmethod region-difference ((xs standard-rectangle) (ys standard-rectangle-set))
  (region-difference (rectangle->standard-rectangle-set xs) ys))

(defmethod region-intersection ((xs standard-rectangle-set) (ys standard-rectangle))
  (region-intersection xs (rectangle->standard-rectangle-set ys)))

(defmethod region-intersection ((xs standard-rectangle) (ys standard-rectangle-set))
  (region-intersection (rectangle->standard-rectangle-set xs) ys))

;;; rectangle vs rectangle

(defmethod region-union ((xs standard-rectangle) (ys standard-rectangle))
  (region-union (rectangle->standard-rectangle-set xs)
		(rectangle->standard-rectangle-set ys)))

(defmethod region-difference ((xs standard-rectangle) (ys standard-rectangle))
  (region-difference (rectangle->standard-rectangle-set xs)
		     (rectangle->standard-rectangle-set ys)))

(defmethod region-intersection ((xs standard-rectangle) (ys standard-rectangle))
  (region-intersection (rectangle->standard-rectangle-set xs)
		       (rectangle->standard-rectangle-set ys)))

(defmethod region-intersection ((xr rectangle) (yr rectangle))
  (region-intersection (rectangle->standard-rectangle-set xr)
		       (rectangle->standard-rectangle-set yr)))

;;;

(defmethod region-equal ((xs standard-rectangle-set) (ys standard-rectangle-set))
  ;; Our bands representation is canonic
  (equal (standard-rectangle-set-bands xs)
         (standard-rectangle-set-bands ys)))

(defmethod region-contains-position-p ((self standard-rectangle-set) x y)
  (block nil
    (map-over-bands (lambda (y1 y2 isum)
                      (when (<= y1 y y2)
                        (when (isum-member x isum)
                          (return t)))
                      (when (< y y2)
                        (return nil)))
                    (standard-rectangle-set-bands self))
    nil))

(defmethod region-contains-region-p ((xs standard-rectangle-set) (point point))
  (multiple-value-bind (x y) (point-position point)
    (region-contains-position-p xs x y)))

;;; -- interval sums ---------------------------------------------------------

(defun isum-union* (xs ys)        (isum-op xs ys boole-ior   0 0 nil))
(defun isum-difference* (xs ys)   (isum-op xs ys boole-andc2 0 0 nil))
(defun isum-intersection* (xs ys) (isum-op xs ys boole-and   0 0 nil))

;;; You could optimize all this like hell, but I better let the code
;;; alone.
;;; BTW this is the first time I make use of boole-xyz

(defun isum-op (as bs boole-op in-a in-b x0)
  (let (x)
    (cond ((and (null as) (null bs))
           nil)
          (t
           (cond ((null bs)
                  (setq in-a (- 1 in-a))
                  (setq x (pop as)))

                 ((null as)
                  (setq in-b (- 1 in-b))
                  (setq x (pop bs)))

                 ((< (first as) (first bs))
                  (setq in-a (- 1 in-a))
                  (setq x (pop as)))
                 
                 ((< (first bs) (first as))
                  (setq in-b (- 1 in-b))
                  (setq x (pop bs)))

                 (t
                  (setq in-a (- 1 in-a)
                        in-b (- 1 in-b))
                  (setq x (pop as))
                  (pop bs)))
           
           (cond ((zerop (boole boole-op in-a in-b))
                  (if x0 
                      (list* x0 x (isum-op as bs boole-op in-a in-b nil))
                    (isum-op as bs boole-op in-a in-b x0)))
                 (t
                  (if (null x0)
                      (isum-op as bs boole-op in-a in-b x)
                    (isum-op as bs boole-op in-a in-b x0))))))))

;;; -- Bands -----------------------------------------------------------------


;;; A band list is represented by

;;;  ((x_0 . a_0) (x_1 . a_1) ... (x_n . nil))

;;; The a_i are the relevant interval sums for x in [x_i, x_(i+1)].

;;; The empty band could have been representated as
;;;  ((x . nil))  x arbitrary
;;; But to get a cononic representation, I'll choose simply NIL.

;;; A better representation would be
;;;  (x_0 a_0 x_1 a_1 ... x_n)
;;; Pro: Unlimited bands could be represented by simply skipping the
;;; first or last 'x'. So similar representation could apply to
;;; interval sums also. But I let the representation as it is, since
;;; this version is well tested.

(defun bands-op (as bs isum-op z0 a b)
  (let (z1)
    (cond ((and (null as) (null bs))
           (if z0
               (list (cons z0 nil))
             nil))
          (t
           (setq z1 (cond ((null as) (caar bs))
                          ((null bs) (caar as))
                          (t (min (caar as) (caar bs)))))
           (let ((rest (bands-op (if (and as (= z1 (caar as))) (cdr as) as)
                                 (if (and bs (= z1 (caar bs))) (cdr bs) bs)
                                 isum-op
                                 z1
                                 (if (and as (= z1 (caar as))) (cdar as) a)
                                 (if (and bs (= z1 (caar bs))) (cdar bs) b)))
                 (isum (funcall isum-op a b)))
             (if z0  
                 (if (and rest (equal isum (cdar rest)))
                     (cons (cons z0 isum)
                           (cdr rest))
                   (cons (cons z0 isum)
                         rest))
               rest))) )))

(defun canon-empty-bands (x)
  (cond ((null (cdr x)) nil)
        (t x)))

(defun bands-union (as bs)
  (canon-empty-bands (bands-op as bs #'isum-union* nil nil nil)))

(defun bands-intersection (as bs)
  (canon-empty-bands (bands-op as bs #'isum-intersection* nil nil nil)))

(defun bands-difference (as bs)
  (canon-empty-bands (bands-op as bs #'isum-difference* nil nil nil)))


(defun rectangle->xy-bands* (x1 y1 x2 y2)
  (list (list y1 x1 x2)
        (cons y2 nil)))

(defun rectangle->yx-bands* (x1 y1 x2 y2)
  (list (list x1 y1 y2)
        (cons x2 nil)))

(defun xy-bands->yx-bands (bands)
  ;; Das kann man sicherlich noch viel geschicker machen ...
  (let ((res nil))
    (map-over-bands-rectangles
     (lambda (x1 y1 x2 y2)
       (setf res (bands-union res (rectangle->yx-bands* x1 y1 x2 y2))))
     bands)
    res))

(defun map-over-bands-rectangles (fun bands)
  (map-over-bands (lambda (y1 y2 isum)
                    (do ((p isum (cddr p)))
                        ((null p))
                      (funcall fun (car p) y1 (cadr p) y2)))
                  bands))

(defun map-over-bands (fun bands)
  (do ((q bands (cdr q)))
      ((null (cdr q)))
    (funcall fun (caar q) (caadr q) (cdar q))))

(defun isum-member (elt isum)
  (cond ((null isum) nil)
        ((< elt (car isum)) nil)
        ((<= elt (cadr isum)) t)
        (t (isum-member elt (cddr isum)))))

(defun rectangle->standard-rectangle-set (rect)
  (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* rect)
    (make-instance 'standard-rectangle-set
      :bands (rectangle->xy-bands* x1 y1 x2 y2))))

(defmethod transform-region (tr (self standard-rectangle-set))
  (cond ((scaling-transformation-p tr)
         (multiple-value-bind (mxx mxy myx myy tx ty)
             (get-transformation tr)
           (declare (ignore mxy myx))
           (let ((rev-x-p (< mxx 0))
                 (rev-y-p (< myy 0)))
             (flet ((correct (bands)
                      (loop for ((y . nil) (nil . xs)) on (nreverse bands)
                         collect `(,y . ,xs))))
               (make-standard-rectangle-set
                (loop for band in (standard-rectangle-set-bands self)
                   for new-band = (loop for x in (cdr band)
                                     collect (+ (* mxx x) tx) into new-xs
                                     finally (return (cons (+ (* myy (car band)) ty)
                                                           (if rev-x-p
                                                               (nreverse new-xs)
                                                               new-xs))))
                   collect new-band into new-bands
                   finally (return (if rev-y-p
                                       (correct new-bands)
                                       new-bands))))))))
        (t
         ;; We have insufficient knowledge about the transformation,
         ;; so we have to take the union of all transformed rectangles.
         ;; Maybe there is a faster way to do this.
         (let ((res +nowhere+))
           (map-over-region-set-regions
	    (lambda (rect)
	      (setf res (region-union res (transform-region tr rect))))
	    self)
           res)) ))

;;; ============================================================================

(defclass standard-bounding-rectangle (standard-rectangle) ())

(defmethod region-equal ((a everywhere-region) (b everywhere-region))
  t)

(defmethod region-equal ((a nowhere-region) (b nowhere-region))
  t)

(defmethod region-equal ((a everywhere-region) (b region))
  nil)

(defmethod region-equal ((a nowhere-region) (b region))
  nil)

(defmethod region-equal ((a region) (b everywhere-region))
  nil)

(defmethod region-equal ((a region) (b nowhere-region))
  nil)

(defmethod region-equal ((a standard-rectangle) (b standard-rectangle))
  (multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* a)
    (multiple-value-bind (u1 v1 u2 v2) (rectangle-edges* b)
      (and (coordinate= x1 u1)
           (coordinate= y1 v1)
           (coordinate= x2 u2)
           (coordinate= y2 v2)))))

(defmethod region-equal ((a standard-rectangle) (b path)) nil)
(defmethod region-equal ((a path) (b standard-rectangle)) nil)

(defmethod transform-region (tr (self everywhere-region))
  (declare (ignore tr))
  +everywhere+)

(defmethod transform-region (tr (self nowhere-region))
  (declare (ignore tr))
  +nowhere+)

(defmethod region-contains-position-p ((self everywhere-region) x y)
  (declare (ignore x y))
  t)

(defmethod region-contains-position-p ((self nowhere-region) x y)
  (declare (ignore x y))
  nil)

(defmethod region-contains-position-p ((self standard-region-union) x y)
  (some (lambda (r) (region-contains-position-p r x y))
        (standard-region-set-regions self)))

(defmethod region-contains-position-p ((self standard-region-intersection) x y)
  (every (lambda (r) (region-contains-position-p r x y))
         (standard-region-set-regions self)))

(defmethod region-contains-position-p ((self standard-region-difference) x y)
  (and (region-contains-position-p (standard-region-difference-a self) x y)
       (not (region-contains-position-p (standard-region-difference-b self) x y))))

;;; Trivial set operations

(defmethod region-union ((a everywhere-region) (b region)) +everywhere+)
(defmethod region-union ((a region) (b everywhere-region)) +everywhere+)
(defmethod region-union ((a nowhere-region) (b region)) b)
(defmethod region-union ((a region) (b nowhere-region)) a)

(defmethod region-intersection ((a everywhere-region) (b region)) b)
(defmethod region-intersection ((a region) (b everywhere-region)) a)
(defmethod region-intersection ((a nowhere-region) (b region)) +nowhere+)
(defmethod region-intersection ((a region) (b nowhere-region)) +nowhere+)

(defmethod region-difference ((a region) (b everywhere-region)) +nowhere+)   ;mit ohne alles
(defmethod region-difference ((a nowhere-region) (b region)) +nowhere+)
(defmethod region-difference ((a region) (b nowhere-region)) a)


;;; dimensionally rule
(defmethod region-union ((a area) (b path)) a)
(defmethod region-union ((a path) (b point)) a)
(defmethod region-union ((a area) (b point)) a)
(defmethod region-union ((a path) (b area)) b)
(defmethod region-union ((a point) (b path)) b)
(defmethod region-union ((a point) (b area)) b)

(defmethod transform-region (tr (self standard-region-difference))
  (with-slots (a b) self
    (make-instance 'standard-region-difference
      :a (transform-region tr a)
      :b (transform-region tr b))))

(defmethod transform-region (tr (self standard-region-union))
  (with-slots (regions) self
    (make-instance 'standard-region-union
      :regions (mapcar (lambda (r) (transform-region tr r)) regions))))

(defmethod transform-region (tr (self standard-region-intersection))
  (with-slots (regions) self
    (make-instance 'standard-region-intersection
      :regions (mapcar (lambda (r) (transform-region tr r)) regions))))

(defmethod region-set-regions ((self standard-region-union) &key normalize)
  (declare (ignorable normalize))
  (standard-region-set-regions self))

(defmethod region-set-regions ((self standard-region-intersection) &key normalize)
  (declare (ignorable normalize))
  (standard-region-set-regions self))

(defmethod region-set-regions ((self standard-region-difference) &key normalize)
  (declare (ignorable normalize))
  (list (standard-region-difference-a self)
        (standard-region-difference-b self)))

(defmethod region-set-regions ((self region) &key normalize)
  (declare (ignorable normalize))
  (list self))

(defmethod map-over-region-set-regions
    (fun (self standard-region-union) &key normalize)
  (declare (ignorable normalize))
  (mapc fun (standard-region-set-regions self)))

(defmethod map-over-region-set-regions
    (fun (self standard-region-intersection) &key normalize)
  (declare (ignorable normalize))
  (mapc fun (standard-region-set-regions self)))

(defmethod map-over-region-set-regions
    (fun (self standard-region-difference) &key normalize)
  (declare (ignorable normalize))
  (funcall fun (standard-region-difference-a self))
  (funcall fun (standard-region-difference-b self)))

(defmethod map-over-region-set-regions (fun (self region) &key normalize)
  (declare (ignorable normalize))
  (funcall fun self))

(defun line-intersection* (x1 y1 x2 y2 u1 v1 u2 v2)
  (let ((dx (- x2 x1)) (dy (- y2 y1))
        (du (- u2 u1)) (dv (- v2 v1)))
    (let ((q (- (* dx dv) (* du dy))))
      (cond ((not (and (<= (min x1 x2) (max u1 u2)) (<= (min u1 u2) (max x1 x2))
                       (<= (min y1 y2) (max v1 v2)) (<= (min v1 v2) (max y1 y2))))
             nil)
            ((coordinate= 0 q)
             (cond ((coordinate= (* (- v1 y1) dx) (* (- u1 x1) dy))
                    ;; koninzident
                    (cond ((> (abs dx) (abs dy))
                           (let* ((sx1 (max (min x1 x2) (min u1 u2)))
                                  (sx2 (min (max x1 x2) (max u1 u2)))
                                  (sy1 (+ (* (- sx1 x1) (/ dy dx)) x1))
                                  (sy2 (+ (* (- sx2 x1) (/ dy dx)) x1)))
                             (values :coincident sx1 sy1 sx2 sy2)))
                          (t
                           (let* ((sy1 (max (min y1 y2) (min v1 v2)))
                                  (sy2 (min (max y1 y2) (max v1 v2)))
                                  (sx1 (+ (* (- sy1 y1) (/ dx dy)) y1))
                                  (sx2 (+ (* (- sy2 y1) (/ dx dy)) y1)))
                             (values :coincident sx1 sy1 sx2 sy2)))))
                   (t
                    ;;paralell -- kein Schnitt
                    nil)))
            ((or (<= (abs dx) single-float-epsilon) (<= (abs du) single-float-epsilon))
             ;; infinite slope (vertical line) - previous case covers two vlines
             (let (a b x y)
               (if (zerop dx)           ; ugly setf - I'm ashamed
                   (setf a (/ dv du)
                         b (- v1 (* a u1))
                         x x1
                         y (+ (* a x1) b))
                   (setf a (/ dy dx)
                         b (- y1 (* a x1))
                         x u1
                         y (+ (* a x) b)))
               (if (and (or (<= y1 y y2) (<= y2 y y1))
                        (or (<= v1 y v2) (<= v2 y v1)))
                   (values :hit x y)
                   nil)))
            (t
             (let ((x (/ (+ (* dx (- (* u1 dv) (* v1 du)))
			    (* du (- (* y1 dx) (* x1 dy))))
			 q))
                   (y (/ (+ (* dy (- (* u1 dv) (* v1 du)))
			    (* dv (- (* y1 dx) (* x1 dy))))
			 q)))
               (if (and (or (<= x1 x x2) (<= x2 x x1))
                        (or (<= u1 x u2) (<= u2 x u1)))
                   (values :hit x y)
                 nil)) ) )) ))

(defmethod region-intersection ((a standard-line) (b standard-line))
  (multiple-value-bind (x1 y1) (line-start-point* a)
    (multiple-value-bind (x2 y2) (line-end-point* a)
      (multiple-value-bind (u1 v1) (line-start-point* b)
        (multiple-value-bind (u2 v2) (line-end-point* b)
          (multiple-value-bind (r sx1 sy1 sx2 sy2)
	      (line-intersection* x1 y1 x2 y2 u1 v1 u2 v2)
            (case r
              (:hit (make-point sx1 sy1))
              (:coincident (make-line* sx1 sy1 sx2 sy2))
              ((nil) +nowhere+))))))))

;;; IHMO the CLIM dimensionality rule is brain dead!

(defmethod region-intersection ((a standard-polyline) (b region))
  (let ((res +nowhere+))
    ;; hack alert
    (map-over-polygon-segments 
     (lambda (x1 y1 x2 y2)
       (setf res
	     (region-union
	      res (region-intersection (make-line* x1 y1 x2 y2) b))))
     a)
    res))

(defmethod region-difference ((a standard-polyline) (b region))
  (let ((res +nowhere+))
    (map-over-polygon-segments 
     (lambda (x1 y1 x2 y2)
       (setf res
	     (region-union
	      res (region-difference (make-line* x1 y1 x2 y2) b))))
     a)
    res))

(defmethod region-difference ((a region) (b standard-polyline))
  (map-over-polygon-segments 
     (lambda (x1 y1 x2 y2)
       (setf a (region-difference a (make-line* x1 y1 x2 y2))))
     b)
  a)

(defmethod region-intersection ((b region) (a standard-polyline))
  (region-intersection a b))

(defmethod region-intersection ((a region) (p point))
  (multiple-value-bind (x y) (point-position p)
    (if (region-contains-position-p a x y)
        p
      +nowhere+)))

(defmethod region-intersection ((p point) (a region))
  (region-intersection a p))

(defmethod region-intersection ((a standard-region-union) (b region))
  (let ((res +nowhere+))
    (map-over-region-set-regions
     (lambda (r) (setf res (region-union res (region-intersection r b)))) a)
    res))

(defmethod region-intersection ((a region) (b standard-region-union))
  (region-intersection b a))

(defmethod region-intersection ((a standard-rectangle-set) (b region))
  (let ((res +nowhere+))
    (map-over-region-set-regions (lambda (r) (setf res (region-union res (region-intersection r b)))) a)
    res))

(defmethod region-intersection ((a region) (b standard-rectangle-set))
  (region-intersection b a))

(defmethod region-intersection ((a region) (b standard-region-intersection))
  (map-over-region-set-regions (lambda (r) (setf a (region-intersection a r))) b)
  a)

(defmethod region-intersection ((a standard-region-intersection) (b region))
  (region-intersection b a))

(defmethod region-intersection ((a region) (b region))
  (make-instance 'standard-region-intersection :regions (list a b)))


(defmethod region-intersection ((x region) (y standard-region-difference))
  (with-slots (a b) y
    (region-difference (region-intersection x a) b)))

(defmethod region-intersection ((x standard-region-difference) (y region))
  (with-slots (a b) x
    (region-difference (region-intersection y a) b)))

(defmethod region-difference ((x area) (y path)) x)
(defmethod region-difference ((x area) (y point)) x)
(defmethod region-difference ((x path) (y point)) x)

(defmethod region-difference ((x everywhere-region) (y region))
  (make-instance 'standard-region-difference :a x :b y))

(defmethod region-difference ((x everywhere-region) (y nowhere-region))
  x)

(defmethod region-difference ((x everywhere-region) (y everywhere-region))
  +nowhere+)

(defmethod region-difference ((x region) (y standard-region-difference))
  (with-slots (a b) y
    (region-union (region-difference x a) (region-intersection x b))))

(defmethod region-difference ((x region) (y standard-region-union))
  ;; A \ (B1 u B2 .. u Bn) = ((((A \ B1) \ B2) ... ) \ Bn)
  (let ((res x))
    (map-over-region-set-regions (lambda (a)
                                   (setf res (region-difference res a)))
                                 y)
    res))

(defmethod region-difference ((x standard-region-union) (y region))
  ;; (A u B) \ C = A\C u B\C
  (let ((res +nowhere+))
    (map-over-region-set-regions
     (lambda (a)
       (setf res (region-union res (region-difference a y))))
     x)
    res))

(defmethod region-difference ((x region) (y standard-rectangle-set))
  (let ((res x))
    (map-over-region-set-regions
     (lambda (a)
       (setf res (region-difference res a)))
     y)
    res))

(defmethod region-difference ((x standard-rectangle-set) (y region))
  (let ((res +nowhere+))
    (map-over-region-set-regions
     (lambda (a)
       (setf res (region-union res (region-difference a y))))
     x)
    res))

(defmethod region-difference ((x point) (y region))
  (multiple-value-bind (px py) (point-position x)
    (if (region-contains-position-p y px py)
        +nowhere+
      x)))

(defmethod region-difference ((x standard-region-difference) (y region))
  ;; (A\B)\C = A \ (B u C)
  (with-slots (a b) x
    (region-difference a (region-union b y))))

(defmethod region-difference ((x region) (y standard-region-intersection))
  (let ((res +nowhere+))
    (map-over-region-set-regions
     (lambda (b)
       (setf res (region-union res (region-difference x b))))
     y)
    res))

;;; This CLIM dimensionality rule is inconsistent to the highest degree and
;;; introduces more problems than it solves

;;; -- Set operations on polygons --------------------------------------------

(defstruct (pg-edge (:constructor make-pg-edge* (x1 y1 x2 y2 extra)))
  x1 y1 x2 y2 extra)

(defstruct pg-splitter
  links                                 ; "links" means "left"
                                        ;list of points
  rechts)                               ; "rechts" means "right"
                                        ; from the top down

(defun make-pg-edge (p1 p2 extra)
  (multiple-value-bind (x1 y1) (point-position p1)
    (multiple-value-bind (x2 y2) (point-position p2)
      (make-pg-edge* x1 y1 x2 y2 extra))))


(defmethod region-intersection ((a standard-polygon) (b standard-polygon))
  (polygon-op a b #'logand))

(defmethod region-union ((a standard-polygon) (b standard-polygon))
  (polygon-op a b #'logior))

(defmethod region-difference ((a standard-polygon) (b standard-polygon))
  (polygon-op a b #'logandc2))

(defmethod region-intersection ((a standard-polygon) (b standard-rectangle))
  (polygon-op a b #'logand))

(defmethod region-union ((a standard-polygon) (b standard-rectangle))
  (polygon-op a b #'logior))

(defmethod region-difference ((a standard-polygon) (b standard-rectangle))
  (polygon-op a b #'logandc2))

(defmethod region-intersection ((a standard-rectangle) (b standard-polygon))
  (polygon-op a b #'logand))

(defmethod region-union ((a standard-rectangle) (b standard-polygon))
  (polygon-op a b #'logior))

(defmethod region-difference ((a standard-rectangle) (b standard-polygon))
  (polygon-op a b #'logandc2))

(defun polygon-op (pg1 pg2 &optional logop)
  (let ((sps nil))
    (over-sweep-bands pg1 pg2
                      (lambda (sy0 sy1 S &aux (ys nil))
                        (setq ys (list sy0 sy1))
                        (dolist (k1 S)
                          (dolist (k2 S)
                            (multiple-value-bind (px py)
                                (line-intersection**
				 (pg-edge-x1 k1) (pg-edge-y1 k1)
				 (pg-edge-x2 k1) (pg-edge-y2 k1)
				 (pg-edge-x1 k2) (pg-edge-y1 k2)
				 (pg-edge-x2 k2) (pg-edge-y2 k2))
                              (when (and px (< sy0 py sy1))
                                (pushnew py ys :test #'coordinate=)))))
                        (setq ys (sort ys #'<))
                        (do ((q ys (cdr q)))
                            ((null (cdr q)))
                          (let ((by0 (car q)) (by1 (cadr q))
                                (R nil))
                            (dolist (k S)
                              (when (> (pg-edge-y2 k) (pg-edge-y1 k))
                                (multiple-value-bind (x1 y1 x2 y2) 
                                    (restrict-line-on-y-interval*
				     (pg-edge-x1 k) (pg-edge-y1 k)
				     (pg-edge-x2 k) (pg-edge-y2 k)
				     by0 by1)
                                  (declare (ignore y1 y2))
                                  (push (list x1 x2 (pg-edge-extra k)) R))))
                            (setq R (sort R #'<
					  :key (lambda (x) (+ (first x) (second x)))))
                            (labels
                                ((add (lo lu ro ru)
                                   (dolist (s sps
                                             ;; otherwise
                                             (push (make-pg-splitter
						    :links  (list lu lo) 
						    :rechts (list ru ro))
                                                   sps) )
                                     (when (and (region-equal
						 lo (car (pg-splitter-links s)))
                                                (region-equal
						 ro (car (pg-splitter-rechts s))))
                                       (push lu (pg-splitter-links s))
                                       (push ru (pg-splitter-rechts s))
                                       (return))) ))
                              (let ((eintritt nil)
                                    (ina 0)
                                    (inb 0))
                                (dolist (k R)
                                  (ecase (third k)
                                    (:a (setq ina (- 1 ina)))
                                    (:b (setq inb (- 1 inb))))
                                  (cond ((/= 0 (funcall logop ina inb))
                                         (when (null eintritt)
                                           (setq eintritt k)))
                                        (t
                                         (when eintritt
                                           (add (make-point (first eintritt) by0)
                                                (make-point (second eintritt) by1)
                                                (make-point (first k) by0)
                                                (make-point (second k) by1))
                                           (setq eintritt nil)) )))) ) )) ) )
    (setq sps (delete +nowhere+ (mapcar #'pg-splitter->polygon sps)))
    (cond ((null sps) +nowhere+)
          ((null (cdr sps))
           (car sps))
          ((make-instance 'standard-region-union :regions sps))) ))

(defun over-sweep-bands (pg1 pg2 fun)
  (let ((es (nconc (polygon->pg-edges pg1 :a) (polygon->pg-edges pg2 :b))))
    (setq es (sort es #'< :key #'pg-edge-y1))
    (let ((ep es)
          (sy (pg-edge-y1 (car es)))
          (S nil))
      (do () ((null ep))
        (setq S (delete-if (lambda (e)
                             (<= (pg-edge-y2 e) sy))
                           S))
      
        (do () ((or (null ep) (/= sy (pg-edge-y1 (car ep)))))
          (push (pop ep) S))

        (let ((sy2 (or (and ep (pg-edge-y1 (car ep)))
                       (reduce #'max (mapcar #'pg-edge-y2 S)))))
          
          (funcall fun sy sy2 S)
          (setq sy sy2)) ))))

(defun polygon->pg-edges (pg extra)
  (let ((pts (polygon-points pg))
        (res nil))
    (let ((prev pts)
          (cur (cdr pts))
          (next (cddr pts)))
      (loop
        (when (or (> (point-y (car next)) (point-y (car cur)))
                  (and (= (point-y (car next)) (point-y (car cur)))
                       (> (point-x (car next)) (point-x (car cur)))))
          (push (make-pg-edge (car cur) (car next) extra) res))
        (when (or (> (point-y (car prev)) (point-y (car cur)))
                  (and (= (point-y (car prev)) (point-y (car cur)))
                       (> (point-x (car prev)) (point-x (car cur)))))
          (push (make-pg-edge (car cur) (car prev) extra) res))
        (when (not (or (> (point-y (car next)) (point-y (car cur)))
                       (and (= (point-y (car next)) (point-y (car cur)))
                            (> (point-x (car next)) (point-x (car cur))))
                       (> (point-y (car next)) (point-y (car cur)))
                       (and (= (point-y (car next)) (point-y (car cur)))
                            (> (point-x (car next)) (point-x (car cur))))))
          (push (make-pg-edge (car cur) (car cur) extra) res))
        (psetq prev cur
               cur next
               next (or (cdr next) pts))
        (when (eq prev pts)
          (return)) ))
    res))

(defun restrict-line-on-y-interval* (x1 y1 x2 y2 ry0 ry1)
  (let ((dx (- x2 x1))
        (dy (- y2 y1)))
    (values (+ (* (- ry0 y1) (/ dx dy)) x1) ry0
            (+ (* (- ry1 y1) (/ dx dy)) x1) ry1)))

(defun pg-splitter->polygon (s)
  (make-polygon (clean-up-point-sequence (nconc (pg-splitter-links s) (reverse (pg-splitter-rechts s))))))

(defun clean-up-point-sequence (pts)
  (cond ((null (cdr pts)) pts)
        ((region-equal (car pts) (cadr pts))
         (clean-up-point-sequence (cdr pts)))
        ((null (cddr pts)) pts)
        ((colinear-p (car pts) (cadr pts) (caddr pts))
         (clean-up-point-sequence (list* (car pts) (caddr pts) (cdddr pts))))
        (t
         (cons (car pts) (clean-up-point-sequence (cdr pts)))) ))

(defun colinear-p (p1 p2 p3)
  (multiple-value-bind (x1 y1) (point-position p1)
    (multiple-value-bind (x2 y2) (point-position p2)
      (multiple-value-bind (x3 y3) (point-position p3)
        (coordinate= (* (- x2 x1) (- y3 y2))
                     (* (- x3 x2) (- y2 y1)))))))

(defun line-intersection** (x1 y1 x2 y2 u1 v1 u2 v2)
  (let ((dx (- x2 x1)) (dy (- y2 y1))
        (du (- u2 u1)) (dv (- v2 v1)))
    (let ((q (- (* dx dv) (* du dy))))
      (cond ((coordinate= 0 q)
             nil)
            (t
             (let ((x (/ (+ (* dx (- (* u1 dv) (* v1 du)))
			    (* du (- (* y1 dx) (* x1 dy))))
			 q))
                   (y (/ (+ (* dy (- (* u1 dv) (* v1 du)))
			    (* dv (- (* y1 dx) (* x1 dy))))
			 q)))
               (values x y)))))))

;;; ----------------------------------------------------------------------------

(defmethod region-union ((a standard-region-union) (b nowhere-region))
  a)

(defmethod region-union ((b nowhere-region) (a standard-region-union))
  a)

(defmethod region-union ((a standard-region-union) (b region))
  (assert (not (eq b +nowhere+)))
  (make-instance 'standard-region-union
    :regions (cons b (standard-region-set-regions a))))

(defmethod region-union ((b region) (a standard-region-union))
  (assert (not (eq b +nowhere+)))
  (make-instance 'standard-region-union
    :regions (cons b (standard-region-set-regions a))))

(defmethod region-union ((a standard-region-union) (b standard-region-union))
  (assert (not (eq b +nowhere+)))
  (assert (not (eq a +nowhere+)))
  (make-instance 'standard-region-union 
    :regions (append (standard-region-set-regions a)
	      (standard-region-set-regions b))))

(defmethod region-union ((a region) (b region))
  (make-instance 'standard-region-union :regions (list a b)))

(defmethod region-union ((a standard-rectangle-set) (b path)) a)
(defmethod region-union ((b path) (a standard-rectangle-set)) a)
(defmethod region-union ((a standard-rectangle-set) (b point)) a)
(defmethod region-union ((b point) (a standard-rectangle-set)) a)

;;; -- Intersection Line/Polygon ---------------------------------------------

(defun geraden-schnitt/prim (x1 y1 x12 y12  x2 y2 x22 y22)
  (let ((dx1 (- x12 x1)) (dy1 (- y12 y1))
        (dx2 (- x22 x2)) (dy2 (- y22 y2)))
    ;; two straights (lines) given as
    ;; g : s -> (x1 + s*dx1, y1 + s*dy1)
    ;; h : t -> (x2 + t*dx2, y2 + t*dy2)
    ;; -> NIL | (s ; t)
    (let ((quot (- (* DX2 DY1) (* DX1 DY2))))
      (if (coordinate= quot 0)
          nil
        (values
         (- (/ (+ (* DX2 (- Y1 Y2)) (* DY2 X2) (- (* DY2 X1))) quot))
         (- (/ (+ (* DX1 (- Y1 Y2)) (* DY1 X2) (- (* DY1 X1))) quot)))) )) )

(defun geraden-gleichung (x0 y0 x1 y1 px py)
  ;; ??? This somehow tries to calculate the distance between a point
  ;; and a line. The sign of the result depends upon the side the point
  ;; is on wrt to the line. --GB
  (- (* (- py y0) (- x1 x0))
     (* (- px x0) (- y1 y0))))

(defun position->geraden-fktn-parameter (x0 y0 x1 y1 px py)
  (let ((dx (- x1 x0)) (dy (- y1 y0)))
    (if (> (abs dx) (abs dy))
        (/ (- px x0) dx)
        (/ (- py y0) dy))))

(defun map-over-schnitt-gerade/polygon (fun x1 y1 x2 y2 points)
  ;; This calles 'fun' with the "Geradenfunktionsparameter" of each
  ;; intersection of the line (x1,y1),(x2,y2) and the polygon denoted
  ;; by 'points' in a "sensible" way. --GB
  (let ((n (length points)))
    (dotimes (i n)
      (let ((pv  (elt points (mod (- i 1) n)))          ;the point before
            (po  (elt points (mod i n)))                ;the "current" point
            (pn  (elt points (mod (+ i 1) n)))          ;the point after
            (pnn (elt points (mod (+ i 2) n))))         ;the point after**2
        (cond
         ;; The line goes directly thru' po
         ((line-contains-point-p** x1 y1 x2 y2 (point-x po) (point-y po))
           (let ((sign-1 (geraden-gleichung x1 y1 x2 y2 (point-x pn) (point-y pn)))
                 (sign-2 (geraden-gleichung x1 y1 x2 y2 (point-x pv) (point-y pv))))
             (cond ((or (and (> sign-1 0) (< sign-2 0))
                        (and (< sign-1 0) (> sign-2 0)))
                    ;; clear cases: the line croses the polygon's border
                    (funcall fun (position->geraden-fktn-parameter x1 y1 x2 y2 (point-x po) (point-y po)) ))
                   ((= sign-1 0)
                    ;; more difficult:
                    ;; The line is coincident with the edge po/pn
                    (let ((sign-1 (geraden-gleichung x1 y1 x2 y2 (point-x pnn) (point-y pnn))))
                      (cond ((or (and (> sign-1 0) (< sign-2 0))
                                 (and (< sign-1 0) (> sign-2 0)))
                             ;; The line goes through the polygons border, by edge po/pn
                             (funcall fun (position->geraden-fktn-parameter x1 y1 x2 y2 (point-x po) (point-y po)) ))
                            (t
                             ;; otherwise the line touches the polygon at the edge po/pn,
                             ;; return both points
                             (funcall fun (position->geraden-fktn-parameter x1 y1 x2 y2 (point-x po) (point-y po)) )
                             (funcall fun (position->geraden-fktn-parameter x1 y1 x2 y2 (point-x pn) (point-y pn)) ) ))))
                   (t
                    ;; all other cases: Line either touches polygon in
                    ;; a point or in an edge [handled above]. --GB
                    nil) )))
         ((line-contains-point-p** x1 y1 x2 y2 (point-x pn) (point-y pn))
          nil)
         (t
          (multiple-value-bind (k m) 
              (geraden-schnitt/prim x1 y1 x2 y2 (point-x po) (point-y po) (point-x pn) (point-y pn))
            (when (and k (<= 0 m 1))    ;Possible numerical instability
              (funcall fun k)))))))))

(defun schnitt-gerade/polygon-prim (x1 y1 x2 y2 points)
  (let ((res nil))
    (map-over-schnitt-gerade/polygon (lambda (k) (push k res)) x1 y1 x2 y2 points)
    (sort res #'<)))

(defun schnitt-line/polygon (x1 y1 x2 y2 polygon)
  (let ((ks (schnitt-gerade/polygon-prim x1 y1 x2 y2 (polygon-points polygon))))
    (assert (evenp (length ks)))
    (let ((res nil))
      (do ((q ks (cddr q)))
          ((null q))
        (let ((k1 (max 0d0 (min 1d0 (car q))))
              (k2 (max 0d0 (min 1d0 (cadr q)))))
          (when (/= k1 k2)
            (push (make-line* (+ x1 (* k1 (- x2 x1))) (+ y1 (* k1 (- y2 y1)))
                              (+ x1 (* k2 (- x2 x1))) (+ y1 (* k2 (- y2 y1))))
                  res))))
      (cond ((null res) +nowhere+)
            ((null (cdr res)) (car res))
            (t (make-instance 'standard-region-union :regions res)) ))))

(defmethod region-contains-position-p ((pg polygon) x y)
  (setf x (coerce x 'coordinate))
  (setf y (coerce y 'coordinate))
  (let ((n 0) (m 0))
    (map-over-schnitt-gerade/polygon (lambda (k) 
                                       (when (>= k 0) (incf n))
                                       (incf m))
                                     x y (+ x 1) y (polygon-points pg))
    (assert (evenp m))
    (oddp n)))

(defmethod region-intersection ((a standard-line) (b standard-polygon))
  (multiple-value-bind (x1 y1) (line-start-point* a)
    (multiple-value-bind (x2 y2) (line-end-point* a)
      (schnitt-line/polygon x1 y1 x2 y2 b))))

(defmethod region-intersection ((b standard-polygon) (a standard-line))
  (multiple-value-bind (x1 y1) (line-start-point* a)
    (multiple-value-bind (x2 y2) (line-end-point* a)
      (schnitt-line/polygon x1 y1 x2 y2 b))))

(defmethod region-intersection ((a standard-line) (b standard-rectangle))
  (multiple-value-bind (x1 y1) (line-start-point* a)
    (multiple-value-bind (x2 y2) (line-end-point* a)
      (schnitt-line/polygon x1 y1 x2 y2 b))))

(defmethod region-intersection ((b standard-rectangle) (a standard-line))
  (multiple-value-bind (x1 y1) (line-start-point* a)
    (multiple-value-bind (x2 y2) (line-end-point* a)
      (schnitt-line/polygon x1 y1 x2 y2 b))))



(defmethod region-difference ((a standard-line) (b standard-polygon))
  (multiple-value-bind (x1 y1) (line-start-point* a)
    (multiple-value-bind (x2 y2) (line-end-point* a)
      (differenz-line/polygon x1 y1 x2 y2 b))))

(defmethod region-difference ((a standard-line) (b standard-rectangle))
  (multiple-value-bind (x1 y1) (line-start-point* a)
    (multiple-value-bind (x2 y2) (line-end-point* a)
      (differenz-line/polygon x1 y1 x2 y2 b))))

(defun differenz-line/polygon (x1 y1 x2 y2 polygon)
  (let ((ks (schnitt-gerade/polygon-prim x1 y1 x2 y2 (polygon-points polygon))))
    (assert (evenp (length ks)))
    (let ((res nil)
          (res2 nil))
      (push 0d0 res)
      (do ((q ks (cddr q)))
          ((null q))
        (let ((k1 (max 0d0 (min 1d0 (car q))))
              (k2 (max 0d0 (min 1d0 (cadr q)))))
          (when (/= k1 k2)
            (push k1 res)
            (push k2 res))))
      (push 1d0 res)
      (setf res (nreverse res))
      (do ((q res (cddr q)))
          ((null q))
        (let ((k1 (car q))
              (k2 (cadr q)))
          (when (/= k1 k2)
            (push (make-line* (+ x1 (* k1 (- x2 x1))) (+ y1 (* k1 (- y2 y1)))
                              (+ x1 (* k2 (- x2 x1))) (+ y1 (* k2 (- y2 y1))))
                  res2))))
      (cond ((null res2) +nowhere+)
            ((null (cdr res2)) (car res2))
            (t (make-instance 'standard-region-union :regions res2)) ))))


(defmethod region-difference ((a standard-line) (b standard-line))
  (multiple-value-bind (x1 y1) (line-start-point* a)
    (multiple-value-bind (x2 y2) (line-end-point* a)
      (multiple-value-bind (u1 v1) (line-start-point* b)
        (multiple-value-bind (u2 v2) (line-end-point* b)
          (cond ((and (coordinate= 0 (geraden-gleichung x1 y1 x2 y2 u1 v1))
                      (coordinate= 0 (geraden-gleichung x1 y1 x2 y2 u2 v2)))
                 (let ((k1 (position->geraden-fktn-parameter x1 y1 x2 y2 u1 v1))
                       (k2 (position->geraden-fktn-parameter x1 y1 x2 y2 u2 v2)))
                   (psetq k1 (max 0 (min k1 k2))
                          k2 (min 1 (max k1 k2)))
                   (let ((r (nconc (if (> k1 0)
                                       (list (make-line* x1 y1 (+ x1 (* k1 (- x2 x1))) (+ y1 (* k1 (- y2 y1)))))
                                     nil)
                                   (if (< k2 1)
                                       (list (make-line* (+ x1 (* k2 (- x2 x1))) (+ y1 (* k2 (- y2 y1))) x2 y2))
                                     nil))))
                     (cond ((null r) +nowhere+)
                           ((null (cdr r)) (car r))
                           (t (make-instance 'standard-region-union :regions r)) ))))
                (t
                 a)))))))

(defmethod region-union ((a standard-line) (b standard-line))
  (multiple-value-bind (x1 y1) (line-start-point* a)
    (multiple-value-bind (x2 y2) (line-end-point* a)
      (multiple-value-bind (u1 v1) (line-start-point* b)
        (multiple-value-bind (u2 v2) (line-end-point* b)
          (cond ((and (coordinate= 0 (geraden-gleichung x1 y1 x2 y2 u1 v1))
                      (coordinate= 0 (geraden-gleichung x1 y1 x2 y2 u2 v2)))
                 (let ((k1 (position->geraden-fktn-parameter x1 y1 x2 y2 u1 v1))
                       (k2 (position->geraden-fktn-parameter x1 y1 x2 y2 u2 v2)))
                   (psetq k1 (min k1 k2)
                          k2 (max k1 k2))
                   (cond ((and (<= k1 1) (>= k2 0))
                          (let ((k1 (min 0 k1))
                                (k2 (max 1 k2)))
                            (make-line* (+ x1 (* k1 (- x2 x1))) (+ y1 (* k1 (- y2 y1)))
                                        (+ x1 (* k2 (- x2 x1))) (+ y1 (* k2 (- y2 y1))))))
                         (t
                          (make-instance 'standard-region-union :regions (list a b))))))
                ((and (coordinate= x1 u1) (coordinate= y1 v1))
                 (make-polyline* (list u2 v2 x1 y1 x2 y2)))
                ((and (coordinate= x2 u2) (coordinate= y2 v2))
                 (make-polyline* (list x1 y1 x2 y2 u1 v1)))
                ((and (coordinate= x1 u2) (coordinate= y1 v2))
                 (make-polyline* (list u1 v1 x1 y1 x2 y2)))
                ((and (coordinate= x2 u1) (coordinate= y2 v1))
                 (make-polyline* (list x1 y1 x2 y2 u2 v2)))
                (t
                 (make-instance 'standard-region-union :regions (list a b))) ))))))

(defmethod region-union ((a standard-polyline) (b standard-line))
  (with-slots (points) a
    (cond ((polyline-closed a)
           (make-instance 'standard-region-union :regions (list a b)))
          ((region-equal (car points) (line-end-point b))
           (make-polyline (cons (line-start-point b) points)))
          ((region-equal (car points) (line-start-point b))
           (make-polyline (cons (line-end-point b) points)))
          ((region-equal (car (last points)) (line-end-point b))
           (make-polyline (append points (list (line-start-point b)))))
          ((region-equal (car (last points)) (line-start-point b))
           (make-polyline (append points (list (line-end-point b)))))
          (t
           (make-instance 'standard-region-union :regions (list a b))))))

(defmethod region-union ((a standard-line) (b standard-polyline))
  (region-union b a))

(defmethod region-union ((a standard-polyline) (b standard-polyline))
  (with-slots ((a-points points)) a
    (with-slots ((b-points points)) b
      (cond ((polyline-closed a)
             (make-instance 'standard-region-union :regions (list a b)))
            ((polyline-closed b)
             (make-instance 'standard-region-union :regions (list a b)))
            ((region-equal (car a-points) (car b-points))
             (make-polyline (append (reverse (cdr a-points)) b-points)))
            ((region-equal (car (last a-points)) (car (last b-points)))
             (make-polyline (append a-points (reverse (cdr b-points)))))
            ((region-equal (car a-points) (car (last b-points)))
             (make-polyline (append b-points (cdr a-points))))
            ((region-equal (car (last a-points)) (car b-points))
             (make-polyline (append a-points (cdr b-points))))
            (t
             (make-instance 'standard-region-union :regions (list a b)))))))

(defmethod region-union ((a standard-rectangle-set) (b polygon))
  (region-union (rectangle-set->polygon-union a) b))

(defmethod region-union ((a polygon) (b standard-rectangle-set))
  (region-union a (rectangle-set->polygon-union b)))

(defun rectangle-set->polygon-union (rs)
  (let ((res nil))
    (map-over-region-set-regions (lambda (r) (push r res)) rs)
    (make-instance 'standard-region-union :regions res)))

(defmethod region-union ((a standard-region-difference) (b region))
  (make-instance 'standard-region-union :regions (list a b)))

(defmethod region-union ((a region) (b standard-region-difference))
  (make-instance 'standard-region-union :regions (list a b)))



(defmethod region-equal ((a standard-line) (b standard-line))
  (or (and (region-equal (line-start-point a) (line-start-point b))
           (region-equal (line-end-point a) (line-end-point b)))
      (and (region-equal (line-start-point a) (line-end-point b))
           (region-equal (line-end-point a) (line-start-point b)))))

(defmethod region-union ((a nowhere-region) (b nowhere-region))
  +nowhere+)

;;; The generic function REGION-EXCLUSIVE-OR is not part of the CLIM
;;; II specification, which is why it does not have a corresponding
;;; DEFGENERIC form in the "Protocol" section in the beginning of this
;;; file.  However, to avoid a style warning emitted by certain
;;; compilers, we still want an explicit DEFGENERIC form.  This is why
;;; we include it here.
(defgeneric region-exclusive-or (region1 region2))

(defmethod region-exclusive-or ((a region) (b region))
  (region-union (region-difference a b) (region-difference b a)))
  

(defmethod region-contains-region-p ((a region) (b point))
  (region-contains-position-p a (point-x b) (point-y b)))

;; xxx what about (region-contains-region-p x +nowhere+) ?

(defmethod region-contains-region-p ((a everywhere-region) (b region))
  t)

(defmethod region-contains-region-p ((a nowhere-region) (b region))
  nil)

(defmethod region-contains-region-p ((a everywhere-region) (b everywhere-region))
  t)

(defmethod region-contains-region-p ((a region) (b everywhere-region))
  ;; ??? what about (region-union (region-difference +everywhere+ X) X) ???
  nil)

(defmethod region-contains-region-p ((a region) (b nowhere-region))
  t)

;;   REGION-CONTAINS-REGION-P region1 region2
;;
;;        Returns t if all points in the region region2 are members of the
;;        region region1; otherwise, it returns nil. 
;;    
;;        aka region2 ist teilmenge von region1  aka B\A = 0
;;
;;   REGION-INTERSECTS-REGION-P region1 region2
;;
;;        Returns nil if region-intersection of the two regions region1 and
;;        region2 would be +nowhere+; otherwise, it returns t. 
;;
;;        aka region1 und region2 sind nicht disjunkt  aka AB /= 0
;;


;; generic versions
(defmethod region-equal ((a region) (b region))
  (region-equal +nowhere+ (region-exclusive-or a b)))

(defmethod region-intersects-region-p ((a region) (b region))
  (not (region-equal +nowhere+ (region-intersection a b))))

(defmethod region-contains-region-p ((a region) (b region))
  (or (eq a b)
      (region-equal +nowhere+ (region-difference b a))))
;;;; ===========================================================================

(defmethod bounding-rectangle* ((a standard-line))
  (with-slots (x1 y1 x2 y2) a
    (values (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2))))

(defmethod bounding-rectangle* ((a standard-rectangle))
  (with-standard-rectangle (x1 y1 x2 y2)
      a
    (values x1 y1 x2 y2)))

(defmethod bounding-rectangle* ((self standard-rectangle-set))
  (with-slots (bands bounding-rectangle) self
    (values-list (or bounding-rectangle
                     (setf bounding-rectangle
                       (let (bx1 by1 bx2 by2)
                         (map-over-bands-rectangles (lambda (x1 y1 x2 y2)
                                                      (setf bx1 (min (or bx1 x1) x1)
                                                            bx2 (max (or bx2 x2) x2)
                                                            by1 (min (or by1 y1) y1)
                                                            by2 (max (or by2 y2) y2)))
                                                    bands)
                         (list bx1 by1 bx2 by2)))))))

(defmethod bounding-rectangle* ((self standard-polygon))
  (values (reduce #'min (mapcar #'point-x (polygon-points self)))
          (reduce #'min (mapcar #'point-y (polygon-points self)))
          (reduce #'max (mapcar #'point-x (polygon-points self)))
          (reduce #'max (mapcar #'point-y (polygon-points self)))))

(defmethod bounding-rectangle* ((self standard-polyline))
  (values (reduce #'min (mapcar #'point-x (polygon-points self)))
          (reduce #'min (mapcar #'point-y (polygon-points self)))
          (reduce #'max (mapcar #'point-x (polygon-points self)))
          (reduce #'max (mapcar #'point-y (polygon-points self)))))
          
(defmethod bounding-rectangle* ((self standard-point))
  (with-slots (x y) self
    (values x y x y)))

(defmethod bounding-rectangle* ((self standard-region-union))
  (let (bx1 by1 bx2 by2)
    (map-over-region-set-regions (lambda (r)
                                   (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* r)
                                     (setf bx1 (min (or bx1 x1) x1)
                                           bx2 (max (or bx2 x2) x2)
                                           by1 (min (or by1 y1) y1)
                                           by2 (max (or by2 y2) y2))))
                                 self)
    (values bx1 by1 bx2 by2)))

(defmethod bounding-rectangle* ((self standard-region-difference))
  (with-slots (a b) self
    (cond ((eq a +everywhere+)
           (bounding-rectangle* b))
          (t
           (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* a)
             (multiple-value-bind (u1 v1 u2 v2) (bounding-rectangle* b)
               (values (min x1 u1) (min y1 v1)
                       (max x2 u2) (min y2 v2))))) )))

(defmethod bounding-rectangle* ((self standard-region-intersection))
  ;; kill+yank alert
  (let (bx1 by1 bx2 by2)
    (map-over-region-set-regions (lambda (r)
                                   (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* r)
                                     (setf bx1 (min (or bx1 x1) x1)
                                           bx2 (max (or bx2 x2) x2)
                                           by1 (min (or by1 y1) y1)
                                           by2 (max (or by2 y2) y2))))
                                 self)
    (values bx1 by1 bx2 by2)))

;;;; ===========================================================================

(defun make-bounding-rectangle (x1 y1 x2 y2)
  (setf x1 (coerce x1 'coordinate)
        y1 (coerce y1 'coordinate)
        x2 (coerce x2 'coordinate)
        y2 (coerce y2 'coordinate))
  (make-instance 'standard-bounding-rectangle :x1 (min x1 x2) :y1 (min y1 y2) :x2 (max x1 x2) :y2 (max y1 y2)))

(defmethod bounding-rectangle ((region rectangle))
  region)

(defmethod bounding-rectangle ((region region))
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
    (make-bounding-rectangle x1 y1 x2 y2)))

(defmacro with-bounding-rectangle* ((min-x min-y max-x max-y) region &body body)
  ;; What is the purpose of this macro; IHMO m.-v.-b. looks as nice as with-b.-.r. .
  `(multiple-value-bind (,min-x ,min-y ,max-x ,max-y) (bounding-rectangle* ,region)
     ,@body))

(defmethod bounding-rectangle-position (bounding-rectangle)
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* bounding-rectangle)
    (declare (ignore x2 y2))
    (values x1 y1)))

(defmethod bounding-rectangle-min-x (bounding-rectangle)
  (nth-value 0 (bounding-rectangle* bounding-rectangle)))

(defmethod bounding-rectangle-min-y (bounding-rectangle)
  (nth-value 1 (bounding-rectangle* bounding-rectangle)))

(defmethod bounding-rectangle-max-x (bounding-rectangle)
  (nth-value 2 (bounding-rectangle* bounding-rectangle)))

(defmethod bounding-rectangle-max-y (bounding-rectangle)
  (nth-value 3 (bounding-rectangle* bounding-rectangle)))

(defmethod bounding-rectangle-width (bounding-rectangle)
  (nth-value 0 (bounding-rectangle-size bounding-rectangle)))

(defmethod bounding-rectangle-height (bounding-rectangle)
  (nth-value 1 (bounding-rectangle-size bounding-rectangle)))

(defmethod bounding-rectangle-size (bounding-rectangle)
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* bounding-rectangle)
    (values (- x2 x1) (- y2 y1))))

;;;

(defmethod print-object ((self standard-rectangle) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (with-standard-rectangle (x1 y1 x2 y2)
      self
      (format stream "X ~S:~S Y ~S:~S" x1 x2 y1 y2))))

;;;;

(defmethod region-intersects-region-p :around ((a bounding-rectangle) (b bounding-rectangle))
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* a)
    (multiple-value-bind (u1 v1 u2 v2) (bounding-rectangle* b)
      (cond ((and (<= u1 x2) (<= x1 u2)
                  (<= v1 y2) (<= y1 v2))
             (call-next-method))
            (t
             nil)))))

(defmethod region-intersects-region-p ((a standard-rectangle) (b standard-rectangle))
  (declare (ignorable a b))
  ;; for rectangles, the bounding rectangle test is correct, so if we
  ;; wind up here, we just can return T.
  t
  ;;(multiple-value-bind (x1 y1 x2 y2) (rectangle-edges* a)
  ;;  (multiple-value-bind (u1 v1 u2 v2) (rectangle-edges* b)
  ;;    (and (<= u1 x2) (<= x1 u2)
  ;;         (<= v1 y2) (<= y1 v2))))
  )

;;; Internal helpers

(defmacro with-grown-rectangle* (((out-x1 out-y1 out-x2 out-y2)
                                  (in-x1 in-y1 in-x2 in-y2)
                                  &key
                                  radius
                                  (radius-x radius)
                                  (radius-y radius)
                                  (radius-left  radius-x)
                                  (radius-right radius-x)
                                  (radius-top    radius-y)
                                  (radius-bottom radius-y))
                                  &body body)
  `(multiple-value-bind (,out-x1 ,out-y1 ,out-x2 ,out-y2)
    (values (- ,in-x1 ,radius-left)
     (- ,in-y1 ,radius-top)
     (+ ,in-x2 ,radius-right)
     (+ ,in-y2 ,radius-bottom))
    ,@body))
