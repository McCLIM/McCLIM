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

(defconstant +identity-transformation+
    (make-instance 'transformation))

;;; TRANSFORMATION error conditions

(define-condition transformation-error (error)
  ())

(define-condition transformation-underspecified (transformation-error)
  ((coords :initarg :coords))
  (:report (lambda (self sink)
             (with-slots (coords) self
               (apply #'format sink "The three points (~D,~D), (~D,~D), and (~D,~D) are propably collinear."
                      (subseq coords 0 6))))))

(define-condition reflection-underspecified (transformation-error)
  ((coords :initarg :coords))
  (:report (lambda (self sink)
             (with-slots (coords) self
               (apply #'format sink "The two points (~D,~D) and (~D,~D) are coincident."
                      coords)))))

(define-condition singular-transformation (transformation-error) 
  ((transformation :initarg :transformation))
  (:report (lambda (self sink)
             (with-slots (transformation why) self
                 (format sink "Attempt to invert the probably singular transformation ~S."
                         transformation)))))

;;; TRANSFORMATION constructors

(defun make-translation-transformation (translation-x translation-y)
  (make-instance 'transformation :tx (coerce translation-x 'short-float) :ty (coerce translation-y 'short-float)))

(defun make-rotation-transformation (angle &optional (origin +origin+))
  "Returns a rotation by the given angle around the given origin."
  (make-rotation-transformation* angle (point-x origin) (point-y origin)))

(defun make-rotation-transformation* (angle &optional (origin-x 0.0)
                                                      (origin-y 0.0))
  "Returns a rotation by the given angle around the given coordinates."
  (let* ((cos (cos angle))
         (sin (sin angle))
         (xsin (* origin-x sin))
         (xcos (* origin-x cos))
         (ysin (* origin-y sin))
         (ycos (* origin-y cos)))
    (make-instance 'transformation
      :mxx cos :mxy (- sin) :tx (- xcos    ysin    origin-x)
      :myx sin :myy    cos  :ty (+ xsin    ycos (- origin-y)))))

(defun make-scaling-transformation (scale-x scale-y &optional (origin +origin+))
  (check-type origin point)
  (make-scaling-transformation* scale-x scale-y (point-x origin) (point-y origin)))

(defun make-scaling-transformation* (scale-x scale-y &optional (origin-x 0.0) (origin-y 0.0))
  (check-type origin-x real)
  (check-type origin-y real)
  (make-instance 'transformation
    :mxx (coerce scale-x 'short-float) :tx (* origin-x (1- scale-x))
    :myy (coerce scale-y 'short-float) :ty (* origin-y (1- scale-y))))

(defun make-reflection-transformation (point1 point2)
  (make-reflection-transformation* (point-x point1) (point-y point1)
				   (point-x point2) (point-y point2)))

(defun make-reflection-transformation* (x1 y1 x2 y2)
  (check-type x1 real)
  (check-type y1 real)
  (check-type x2 real)
  (check-type y2 real)
  (when (and (= x1 x2) (= y1 y2))
    (error 'reflection-underspecified :coords (list x1 y1 x2 y2)))
  (let* ((dx (- x1 x2))
         (dy (- y1 y2))
         (dx2 (* dx dx))
         (dy2 (* dy dy))
         (pxy1 (* x1 y2))
         (pxy2 (* x2 y1))
         (sum (+ dx2 dy2)))
    (make-instance 'transformation :mxx (/ (- dx2 dy2) sum)
		                   :mxy (/ (* 2 dx dy) sum)
				   :tx  (/ (* 2 dy (- pxy2 pxy1)) sum)
				   :myx (/ (* 2 dx dy) sum)
				   :myy (/ (- dy2 dx2) sum)
				   :ty  (/ (* 2 dx (- pxy1 pxy2)) sum))))
    
(defun make-transformation (mxx mxy myx myy tx ty)
  (make-instance 'transformation :mxx (coerce mxx 'short-float) :mxy (coerce mxy 'short-float) :tx (coerce tx 'short-float) :myx (coerce myx 'short-float) :myy (coerce myy 'short-float) :ty (coerce ty 'short-float)))

(defun make-3-point-transformation (point-1 point-2 point-3
                                    point-1-image point-2-image point-3-image)
  (make-3-point-transformation* (point-x point-1) (point-y point-1)
                                (point-x point-2) (point-y point-2)
                                (point-x point-3) (point-y point-3)
                                (point-x point-1-image) (point-y point-1-image)
                                (point-x point-2-image) (point-y point-2-image)
                                (point-x point-3-image) (point-y point-3-image)))

(defun make-3-point-transformation* (x1 y1 x2 y2 x3 y3
                                     x1-image y1-image x2-image y2-image
                                     x3-image y3-image)
  (let ((determinant (determinant x1 y1 x2 y2 x3 y3)))
    (when (= determinant 0.0)
      (error 'transformation-underspecified :coords (list x1 y1 x2 y2 x3 y3)))
    (let ((dx1 (- x1 x3))
          (dx2 (- x2 x1))
          (dx3 (- x3 x2))
          (dy1 (- y1 y2))
          (dy2 (- y2 y3))
          (dy3 (- y3 y1))
          (dxy1 (- (* x1 y2) (* x2 y1)))
          (dxy2 (- (* x2 y3) (* x3 y2)))
          (dxy3 (- (* x3 y1) (* x1 y3))))
      (make-transformation (+ (* x1-image dy2) (* x2-image dy3)
			      (* x3-image dy1))
			   (+ (* x1-image dx3) (* x2-image dx1)
			      (* x3-image dx2))
			   (+ (* y1-image dy2) (* y2-image dy3)
			      (* y3-image dy1))
			   (+ (* y1-image dx3) (* y2-image dx1)
			      (* y3-image dx2))
			   (+ (* x1-image dxy2) (* x2-image dxy3)
			      (* x3-image dxy1))
			   (+ (* y1-image dxy2) (* y2-image dxy3)
			      (* y3-image dxy1))))))
        
(defun determinant (a0 a1 b0 b1 c0 c1)
  (- (+ (* a0 b1) (* b0 c1) (* c0 a1))
     (+ (* b1 c0) (* c1 a0) (* a1 b0))))


;;; TRANSFORMATION operations

(defun fuzzy-equal (&rest pairs)
  (loop for sublist on pairs by #'cddr
        sum (abs (- (car sublist) (cadr sublist))) into differences
        count 1 into number-of-pairs
        finally (return (<= differences (* 2 number-of-pairs single-float-epsilon)))))

(defmethod transformation-equal ((transformation-1 transformation) (transformation-2 transformation))
  (let ((matrix-1 (transformation-matrix transformation-1))
	(matrix-2 (transformation-matrix transformation-2)))
    (fuzzy-equal (aref matrix-1 0 0) (aref matrix-2 0 0)
                 (aref matrix-1 0 1) (aref matrix-2 0 1)
                 (aref matrix-1 0 2) (aref matrix-2 0 2)
                 (aref matrix-1 1 0) (aref matrix-2 1 0)
                 (aref matrix-1 1 1) (aref matrix-2 1 1)
                 (aref matrix-1 1 2) (aref matrix-2 1 2))))

(defmethod identity-transformation-p ((transformation transformation))
  (or (eq transformation +identity-transformation+)
      (transformation-equal transformation +identity-transformation+)))

(defun invert-matrix (m)
  (let* ((a0 (aref m 0 0))
         (a1 (aref m 0 1))
         (a2 (aref m 0 2))
         (b0 (aref m 1 0))
         (b1 (aref m 1 1))
         (b2 (aref m 1 2))
         (det (- (* a0 b1) (* a1 b0))))
    (if (= det 0.0)
	nil
      (make-array '(2 3)
                  :element-type 'single-float
                  :initial-contents (list (list (/ b1 det)
                                                (- (/ a1 det))
                                                (/ (- (* a1 b2) (* a2 b1)) det))
                                          (list (- (/ b0 det))
                                                (/ a0 det)
                                                (/ (- (* a2 b0) (* a0 b2)) det)))))))

(defmethod invertible-transformation-p ((transformation transformation))
  (invert-matrix (transformation-matrix transformation)))

(defmethod translation-transformation-p ((transformation transformation))
  (let ((matrix (transformation-matrix transformation)))
    (fuzzy-equal (aref matrix 0 0) 1.0
		 (aref matrix 0 1) 0.0
		 (aref matrix 1 0) 0.0
		 (aref matrix 1 1) 1.0)))

(defmethod y-inverting-transformation-p ((transformation transformation))
  (let ((matrix (transformation-matrix transformation)))
    (and (= (aref matrix 0 0) 1.0)
	 (= (aref matrix 0 1) 0.0)
	 (= (aref matrix 1 0) 0.0)
	 (= (aref matrix 1 1) -1.0))))

(defmethod reflection-transformation-p ((transformation transformation))
  (let ((matrix (transformation-matrix transformation)))
    (< (- (* (aref matrix 0 0) (aref matrix 1 1))
          (* (aref matrix 0 1) (aref matrix 1 0))) 0.0)))

(defmethod rigid-transformation-p ((transformation transformation))
  (let ((matrix (transformation-matrix transformation)))
    (fuzzy-equal (abs (- (* (aref matrix 0 0) (aref matrix 1 1))
                         (* (aref matrix 0 1) (aref matrix 1 0)))) 1.0)))

(defmethod even-scaling-transformation-p ((transformation transformation))
  (let ((matrix (transformation-matrix transformation)))
    (fuzzy-equal (aref matrix 0 1) 0.0
                 (aref matrix 1 0) 0.0
                 (- (aref matrix 0 0) (aref matrix 1 1)) 0.0)))

(defmethod scaling-transformation-p ((transformation transformation))
  (let ((matrix (transformation-matrix transformation)))
    (fuzzy-equal (aref matrix 0 1) 0.0
                 (aref matrix 1 0) 0.0)))

(defmethod rectilinear-transformation-p ((transformation transformation))
  (let ((matrix (transformation-matrix transformation)))
    (or (scaling-transformation-p transformation)
        (fuzzy-equal (aref matrix 0 0) 0.0
                     (aref matrix 1 1) 0.0))))

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
  (make-instance 'transformation :matrix (invert-matrix (transformation-matrix transformation))))

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
