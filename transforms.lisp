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

;;; TRANSFORMATION class

;;; Transformation's implement the following pair of equations:
;;;	x1 = mxx * x + mxy * y + tx
;;;	y1 = myx * x + myy * y + ty
;;; The transformation class uses a homogeneous coordinate system to implement these equations.

(eval-when (eval load compile)

  (defclass transformation ()
    ((matrix :initform (make-array '(3 3)
				   :element-type 'short-float
				   :initial-contents '((1.0 0.0 0.0)
						       (0.0 1.0 0.0)
						       (0.0 0.0 1.0)))
	     :initarg :matrix
	     :reader transformation-matrix)
     ))
  )

(defmethod initialize-instance :after ((transformation transformation) &rest args
				       &key mxx mxy myx myy tx ty
				       &allow-other-keys)
  (declare (ignore args))
  (let ((matrix (transformation-matrix transformation)))
    (if mxx
	(setf (aref matrix 0 0) mxx))
    (if mxy
	(setf (aref matrix 0 1) mxy))
    (if tx
	(setf (aref matrix 0 2) tx))
    (if myx
	(setf (aref matrix 1 0) myx))
    (if myy
	(setf (aref matrix 1 1) myy))
    (if ty
	(setf (aref matrix 1 2) ty))
    transformation))
    
(defun transformationp (x)
  (typep x 'transformation))

(defconstant +identity-transformation+ (make-instance 'transformation))

;;; TRANSFORMATION error conditions

(define-condition transformation-error (error)
  (
   ))

(define-condition transformation-underspecified (transformation-error)
  (
   ))

(define-condition reflection-underspecified (transformation-error)
  (
   ))

(define-condition singular-transformation (transformation-error)
  (
   ))

;;; TRANSFORMATION constructors

(defun make-translation-transformation (translation-x translation-y)
  (make-instance 'transformation :tx (coerce translation-x 'short-float) :ty (coerce translation-y 'short-float)))

(defun make-rotation-transformation (angle &optional (origin (make-point 0.0 0.0)))
  (check-type origin point)
  (let ((s (sin angle))
	(c (cos angle)))
    (make-instance 'transformation :mxx c :mxy (- s) :myx s :myy c)))

(defun make-rotation-transformation* (angle &optional (origin-x 0.0) (origin-y 0.0))
  (check-type origin-x real)
  (check-type origin-y real)
  (let ((s (sin angle))
	(c (cos angle)))
    (make-instance 'transformation :mxx c :mxy (- s) :myx s :myy c)))

(defun make-scaling-transformation (scale-x scale-y &optional (origin (make-point 0.0 0.0)))
  (check-type origin point)
  (make-instance 'transformation :mxx (coerce scale-x 'short-float) :myy (coerce scale-y 'short-float)))

(defun make-scaling-transformation* (scale-x scale-y &optional (origin-x 0.0) (origin-y 0.0))
  (check-type origin-x real)
  (check-type origin-y real)
  (make-instance 'transformation :mxx (coerce scale-x 'short-float) :myy (coerce scale-y 'short-float)))

(defun make-reflection-transformation (point1 point2)
  (declare (ignore point1 point2))
  (error "Make-Reflection-Transformation is not implemented"))

(defun make-reflection-transformation* (x1 y1 x2 y2)
  (declare (ignore x1 y1 x2 y2))
  (error "Make-Reflection-Transformation* is not implemented"))

(defun make-transformation (mxx mxy myx myy tx ty)
  (make-instance 'transformation :mxx (coerce mxx 'short-float) :mxy (coerce mxy 'short-float) :tx (coerce tx 'short-float) :myx (coerce myx 'short-float) :myy (coerce myy 'short-float) :ty (coerce ty 'short-float)))

(defun make-3-point-transformation (point-1 point-2 point-3
				    point-1-image point-2-image point-3-image)
  (declare (ignore point-1 point-2 point-3 point-1-image point-2-image point-3-image))
  (error "Make-3-Point-Transformation is not implemented"))

(defun make-3-point-transformation* (x1 y1 x2 y2 x3 y3
				     x1-image y1-image x2-image y2-image x3-image y3-image)
  (declare (ignore x1 y1 x2 y2 x3 y3 x1-image y1-image x2-image y2-image x3-image y3-image))
  (error "Make-3-Point-Transformation* is not implemented"))

;;; TRANSFORMATION operations

(defmethod transformation-equal ((transformation-1 transformation) (transformation-2 transformation))
  (let ((matrix-1 (transformation-matrix transformation-1))
	(matrix-2 (transformation-matrix transformation-2)))
    (block check-equality
      (loop for i below 2
	  do (loop for j below 3
		 if (not (= (aref matrix-1 i j)
			    (aref matrix-2 i j)))
		 do (return-from check-equality nil))
	  finally (return-from check-equality t)))))

(defmethod identity-transformation-p ((transformation transformation))
  (or (eq transformation +identity-transformation+)
      (transformation-equal transformation +identity-transformation+)))

(defmethod invertible-transformation-p ((transformation transformation))
  (error "Invertible-Transformation-P is not implemented"))

(defmethod translation-transformation-p ((transformation transformation))
  (let ((matrix (transformation-matrix transformation)))
    (and (= (aref matrix 0 0) 1.0)
	 (= (aref matrix 0 1) 0.0)
	 (= (aref matrix 1 0) 0.0)
	 (= (aref matrix 1 1) 1.0))))

(defmethod y-inverting-transformation-p ((transformation transformation))
  (let ((matrix (transformation-matrix transformation)))
    (and (= (aref matrix 0 0) 1.0)
	 (= (aref matrix 0 1) 0.0)
	 (= (aref matrix 1 0) 0.0)
	 (= (aref matrix 1 1) -1.0))))

(defmethod reflection-transformation-p ((transformation transformation))
  (error "Reflection-Transformation-P is not implemented"))

(defmethod rigid-transformation-p ((transformation transformation))
  (error "Rigid-Transformation-P is not implemented"))

(defmethod even-scaling-transformation-p ((transformation transformation))
  (let ((matrix (transformation-matrix transformation)))
    (and (= (abs (aref matrix 0 0)) (abs (aref matrix 1 1)))
	 (= (aref matrix 0 1) 0.0)
	 (= (aref matrix 1 0) 0.0)
	 (= (aref matrix 0 2) 0.0)
	 (= (aref matrix 1 2) 0.0))))

(defmethod scaling-transformation-p ((transformation transformation))
  (let ((matrix (transformation-matrix transformation)))
    (and (= (aref matrix 0 1) 0.0)
	 (= (aref matrix 1 0) 0.0)
	 (= (aref matrix 0 2) 0.0)
	 (= (aref matrix 1 2) 0.0))))

(defmethod rectilinear-transformation-p ((transformation transformation))
  (or (translation-transformation-p transformation)
      (scaling-transformation-p transformation)
      (let ((matrix (transformation-matrix transformation)))
	(and (= (aref matrix 0 0) 0.0)
	     (= (aref matrix 1 1) 0.0)
	     (= (aref matrix 0 2) 0.0)
	     (= (aref matrix 1 2) 0.0)))))

(defmethod compose-transformations ((transformation1 transformation) (transformation2 transformation))
  (flet ((matrix-multiply (m1 m2)
	   (let ((result (make-array '(3 3) :element-type 'short-float)))
	     (dotimes (i 3)
	       (dotimes (j 3)
		 (setf (aref result i j) 0.0)
		 (dotimes (k 3)
		   (incf (aref result i j) (* (aref m1 i k) (aref m2 k j))))))
;	     (print result)
	     result)))
    (make-instance 'transformation :matrix (matrix-multiply (transformation-matrix transformation1)
							    (transformation-matrix transformation2)))))

(defmethod invert-transformation ((transformation transformation))
  (if (not (invertible-transformation-p transformation))
      (error 'singular-transformation))
  (error "Invert-Transformation is not implemented"))

(defmethod compose-translation-with-transformation ((transformation transformation) dx dy)
  (compose-transformations (make-translation-transformation dx dy) transformation))

(defmethod compose-scaling-with-transformation ((transformation transformation) sx sy &optional (origin (make-point 0.0 0.0)))
  (compose-transformations (make-scaling-transformation sx sy origin) transformation))

(defmethod compose-rotation-with-transformation ((transformation transformation) angle &optional (origin (make-point 0.0 0.0)))
  (compose-transformations (make-rotation-transformation angle origin) transformation))

(defmethod compose-transformation-with-translation ((transformation transformation) dx dy)
  (compose-transformations transformation (make-translation-transformation dx dy)))

(defmethod compose-transformation-with-scaling ((transformation transformation) sx sy &optional (origin (make-point 0.0 0.0)))
  (compose-transformations transformation (make-scaling-transformation sx sy origin)))

(defmethod compose-transformation-with-rotation ((transformation transformation) angle &optional (origin (make-point 0.0 0.0)))
  (compose-transformations transformation (make-rotation-transformation angle origin)))

(defmethod transform-region ((transformation transformation) (region region))
  (make-instance (type-of region) :points (loop for (x y) on (region-points region) by #'cddr
					      nconcing (multiple-value-list (transform-position transformation x y)))))

(defmethod untransform-region ((transformation transformation) (region region))
  (transform-region (invert-transformation transformation) region))

(defmethod transform-position ((transformation transformation) x y)
  (let ((matrix (transformation-matrix transformation)))
    (values (+ (* (aref matrix 0 0) (coerce x 'short-float)) (* (aref matrix 0 1) (coerce y 'short-float)) (aref matrix 0 2))
	    (+ (* (aref matrix 1 0) (coerce x 'short-float)) (* (aref matrix 1 1) (coerce y 'short-float)) (aref matrix 1 2)))))

(defmethod untransform-position ((transformation transformation) x y)
  (transform-position (invert-transformation transformation) x y))

(defmethod transform-distance ((transformation transformation) dx dy)
  (declare (ignore dx dy))
  (error "Transform-Distance is not implemented"))

(defmethod untransform-distance ((transformation transformation) dx dy)
  (transform-distance (invert-transformation transformation) dx dy))

(defmethod transform-rectangle* ((transformation transformation) x1 y1 x2 y2)
  (if (not (rectilinear-transformation-p transformation))
      (error 'transformation-error))
  (multiple-value-bind (nx1 ny1) (transform-position transformation x1 y1)
    (multiple-value-bind (nx2 ny2) (transform-position transformation x2 y2)
      (values (min nx1 nx2) (min ny1 ny2) (max nx1 nx2) (max ny1 ny2)))))

(defmethod untransform-rectangle* ((transformation transformation) x1 y1 x2 y2)
  (transform-rectangle* (invert-transformation transformation) x1 y1 x2 y2))
