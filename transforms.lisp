;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-INTERNALS; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: The CLIM Transformations
;;;   Created: 1998-09-29 20:23
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; --------------------------------------------------------------------------------------
;;;  (c) copyright 1998,1999 by Gilbert Baumann
;;;  (c) copyright 2000 by 
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)

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

(in-package :clim-internals)

;;;; Changes

;;;  When        Who    What
;;; --------------------------------------------------------------------------------------
;;;  2001-07-16  GB     added a cache for the inverse transformation

;;; nobody bothers to use the log above ;-(

;; The CLIM 2 spec says:
;;    "Implementations are encouraged to allow transformations that are not
;;    numerically equal due to floating-point roundoff errors to be
;;    TRANSFORMATION-EQUAL. An appropriae level of 'fuzziness' is
;;    single-float-epsilon, or some small multiple of single-float-epsilon."

;; Note: All the predicates like RIGID-TRANSFORMATION-P,
;; RECTILINEAR-TRANSFORMATION-P etc. inherit the "fuzziness" defined by
;; COORDINATE-EPSILON. An implementation of a medium probably invoke these
;; predicates to decide, whether the graphics primitives provided by the
;; underlying windowing system could be used; or if they have to use an own
;; implementation, which may be much slower, since individual pixels may have to
;; be transferred. So I trade speed for precision here.

;; Of course it would be better to assume some resomable maximal device
;; coordinate for instance 40" * 2400dpi. Now two transformations could be said
;; to be practically equal, if the (rounded) images of any point within that
;; range are equal.

;;;; ----------------------------------------------------------------------------------------------------
;;;;  Transformations
;;;;

(define-protocol-class transformation ())

(defclass standard-transformation (transformation)
  ((mxx :type coordinate)
   (mxy :type coordinate)
   (myx :type coordinate)
   (myy :type coordinate)
   (tx  :type coordinate)
   (ty  :type coordinate)
   (inverse :type (or nil standard-transformation)
            :initform nil
            :documentation "Cached inverse transformation.") ))

(defmethod print-object ((self standard-transformation) sink)
  (print-unreadable-object (self sink :identity nil :type t)
    (apply #'format sink "~S ~S ~S ~S ~S ~S"
           (multiple-value-list (get-transformation self)))))

(defun make-transformation (mxx mxy myx myy tx ty)
  ;; Make a transformation, which will map a point (x,y) into
  ;;  x' = mxx*x + mxy*y + tx
  ;;  y' = myx*x + myy*y + ty
  (let ((res (make-instance 'standard-transformation)))
    (setf (slot-value res 'mxx) (coerce mxx 'coordinate)
          (slot-value res 'mxy) (coerce mxy 'coordinate)
          (slot-value res 'myx) (coerce myx 'coordinate)
          (slot-value res 'myy) (coerce myy 'coordinate)
          (slot-value res 'tx)  (coerce tx 'coordinate)
          (slot-value res 'ty)  (coerce ty 'coordinate))
    res))

(defmethod get-transformation ((self standard-transformation))
  ;; Get the values of the transformation matrix as multiple values.
  ;; This is not an exported function!
  (with-slots (mxx mxy myx myy tx ty) self
    (values mxx mxy myx myy tx ty)))

(defun make-translation-transformation (translation-x translation-y)
  (make-transformation 1 0 0 1 translation-x translation-y))

(defun make-rotation-transformation (angle &optional origin)
  (if origin
      (make-rotation-transformation* angle (point-x origin) (point-y origin))
    (make-rotation-transformation* angle 0 0)))

(defun make-rotation-transformation* (angle &optional origin-x origin-y)
  (let ((origin-x (or origin-x 0))
        (origin-y (or origin-y 0)))
    (let ((s (coerce (sin angle) 'coordinate))
          (c (coerce (cos angle) 'coordinate)))
      ;; This clamping should be done more sensible -- And: is this actually a good thing?
      (when (coordinate= s 0) (setq s 0))
      (when (coordinate= c 0) (setq c 0))
      (when (coordinate= s 1) (setq s 1))
      (when (coordinate= c 1) (setq c 1))
      (when (coordinate= s -1) (setq s -1))
      (when (coordinate= c -1) (setq c -1))
      ;; Wir stellen uns hier mal ganz dumm:
      (make-3-point-transformation* origin-x origin-y  (+ origin-x 1) origin-y        origin-x (+ origin-y 1)
                                    origin-x origin-y  (+ origin-x c) (+ origin-y s)  (- origin-x s) (+ origin-y c)) )))

(defun make-scaling-transformation (scale-x scale-y &optional origin)
  (make-scaling-transformation* scale-x scale-y 
                                (if origin (point-x origin) 0)
                                (if origin (point-y origin) 0)))

(defun make-scaling-transformation* (scale-x scale-y &optional origin-x origin-y)
  (let ((origin-x (or origin-x 0))
        (origin-y (or origin-y 0)))
    (make-transformation scale-x 0 0 scale-y (- origin-x (* scale-x origin-x)) (- origin-y (* scale-y origin-y)))) )

(defun make-reflection-transformation (point1 point2)
  (make-reflection-transformation* (point-x point1) (point-y point1) (point-x point2) (point-y point2)))

(defun make-reflection-transformation* (x1 y1 x2 y2)
  (let ((dx (- x2 x1))
        (dy (- y2 y1)))
    (handler-case (make-3-point-transformation* x1 y1  x2 y2  (- x1 dy) (+ y1 dx)
                                                x1 y1  x2 y2  (+ x1 dy) (- y1 dx))
      (transformation-underspecified (c)
        (error 'reflection-underspecified :why c :coords (list x1 y1 x2 y2))))))

(defun make-3-point-transformation (point-1 point-2 point-3 point-1-image point-2-image point-3-image)
  (make-3-point-transformation* (point-x point-1) (point-y point-1)
                                (point-x point-2) (point-y point-2)
                                (point-x point-3) (point-y point-3)
                                (point-x point-1-image) (point-y point-1-image)
                                (point-x point-2-image) (point-y point-2-image)
                                (point-x point-3-image) (point-y point-3-image)))

(defun make-3-point-transformation* (x1 y1 x2 y2 x3 y3 x1-image y1-image x2-image y2-image x3-image y3-image)
  ;; Find a transformation matrix, which transforms each of the three
  ;; points (x_i, y_i) to its image (y_i_image, y_i_image)
  ;; 
  ;; Therefore, we have to solve these two linear equations:
  ;; 
  ;;      / x1 y1 1 \        / mxx \   / x1-image \          / myx \   / y1-image \
  ;;  A:= | x2 y2 1 | ;   A  | mxy | = | x2-image |   and A  | myy | = | y2-image |  ;
  ;;      \ x3 y3 1 /        \ tx  /   \ x3-image /          \ ty  /   \ y3-image /
  ;;
  ;; These matrices are small enough to simply calculate A^-1 = |A|^-1 (adj A).
  ;; 
  (let ((det (+ (* x1 y2) (* y1 x3) (* x2 y3) (- (* y2 x3)) (- (* y1 x2)) (- (* x1 y3)))))
    (if (coordinate/= 0 det)
        (let* ((/det (/ det))
               ;; a thru' i is (adj A) 
               (a (- y2 y3))                (b (- y3 y1))               (c (- y1 y2))               
               (d (- x3 x2))                (e (- x1 x3))               (f (- x2 x1))               
               (g (- (* x2 y3) (* x3 y2)))  (h (- (* x3 y1) (* x1 y3))) (i (- (* x1 y2) (* x2 y1)))
               ;; calculate 1/|A| * (adj A) * (x1-image x2-image x3-image)^t 
               (mxx (* /det (+ (* a x1-image) (* b x2-image) (* c x3-image))))
               (mxy (* /det (+ (* d x1-image) (* e x2-image) (* f x3-image))))
               (tx  (* /det (+ (* g x1-image) (* h x2-image) (* i x3-image))))
               ;; finally 1/|A| * (adj A) * (y1-image y2-image y3-image)^t 
               (myx (* /det (+ (* a y1-image) (* b y2-image) (* c y3-image))))
               (myy (* /det (+ (* d y1-image) (* e y2-image) (* f y3-image))))
               (ty  (* /det (+ (* g y1-image) (* h y2-image) (* i y3-image)))))
          ;; we're done
          (make-transformation mxx mxy myx myy tx ty) )
      ;; determinant was zero, so signal error
      (error 'transformation-underspecified
             :coords (list x1 y1 x2 y2 x3 y3 x1-image y1-image x2-image y2-image x3-image y3-image)) )))

(defparameter +identity-transformation+
  (make-transformation 1 0 0 1 0 0))

(define-condition transformation-error (error)
  ())

(define-condition transformation-underspecified (transformation-error)
  ((coords :initarg :coords
	   :reader transformation-error-coords))
  (:report
   (lambda (self sink)
     (apply #'format sink "The three points (~D,~D), (~D,~D), and (~D,~D) are propably collinear."
	    (subseq (transformation-error-coords self) 0 6)))))

(define-condition reflection-underspecified (transformation-error)
  ((coords :initarg :coords
	   :reader transformation-error-coords)
   (why :initarg :why :initform nil
	:reader transformation-error-why))
  (:report (lambda (self sink)
	     (apply #'format sink "The two points (~D,~D) and (~D,~D) are coincident."
		    (transformation-error-coords self))
	     (when (transformation-error-why self)
	       (format sink " (That was determined by the following error:~%~A)"
		       (transformation-error-why self))))))

(define-condition singular-transformation (transformation-error) 
  ((transformation :initarg :transformation
		   :reader transformation-error-transformation)
   (why :initarg :why :initform nil
	:reader transformation-error-why))
  (:report (lambda (self sink)
	     (format sink "Attempt to invert the probably singular transformation ~S."
		     (transformation-error-transformation self))
	     (when (transformation-error-why self)
	       (format sink "~%Another error occurred while computing the inverse:~%    ~A"
		       (transformation-error-why self))))))

(define-condition rectangle-transformation-error (transformation-error)
  ((transformation :initarg :transformation
		   :reader transformation-error-transformation)
   (rect :initarg :rect
	 :reader transformation-error-rect))
  (:report (lambda (self sink)
	     (format sink "Attempt to transform the rectangle ~S through the non-rectilinear transformation ~S."
		     (transformation-error-rect self)
		     (transformation-error-transformation self)))))

(defmethod transformation-equal ((transformation1 standard-transformation)
                                 (transformation2 standard-transformation))
  (every #'coordinate=
         (multiple-value-list (get-transformation transformation1))
         (multiple-value-list (get-transformation transformation2))))

(defmethod identity-transformation-p ((transformation standard-transformation))
  (transformation-equal transformation +identity-transformation+))

(defmethod translation-transformation-p ((transformation standard-transformation))
  (multiple-value-bind (mxx mxy myx myy) (get-transformation transformation)
    (and (coordinate= mxx 1)
         (coordinate= mxy 0)
         (coordinate= myx 0)
         (coordinate= myy 1))))

(defun transformation-determinant (tr)
  (multiple-value-bind (mxx mxy myx myy) (get-transformation tr)
    (- (* mxx myy) (* mxy myx))))

(defmethod invertible-transformation-p ((transformation standard-transformation))
  (coordinate/= 0 (transformation-determinant transformation)))

(defmethod reflection-transformation-p ((transformation standard-transformation))
  (< (transformation-determinant transformation) 0))

(defmethod rigid-transformation-p ((transformation standard-transformation))
  (multiple-value-bind (a b c d) (get-transformation transformation)
    (and (coordinate= 1 (+ (* a a) (* c c)))            ; |A(1,0)| = 1
         (coordinate= 1 (+ (* b b) (* d d)))            ; |A(0,1)| = 1
         (coordinate= 0 (+ (* a b) (* c d))))))         ; (A(1,0))(A(0,1)) = 0

(defmethod even-scaling-transformation-p ((transformation standard-transformation))
  (and (scaling-transformation-p transformation)
       (multiple-value-bind (mxx myx mxy myy) (get-transformation transformation)
         (declare (ignore mxy myx))
         (coordinate= (abs mxx) (abs myy)))))

(defmethod scaling-transformation-p ((transformation standard-transformation))
  ;; Q: was ist mit dem translationsanteil?
  ;; what gives (scaling-transformation-p (make-translation-transformation 17 42))
  ;; I think it would be strange if (s-t-p (make-s-t* 2 1 1 0)) is not T. -- APD
  (multiple-value-bind (mxx mxy myx myy tx ty) (get-transformation transformation)
    (and (coordinate= 0 mxy) (coordinate= 0 myx)
         (coordinate/= 0 mxx) (coordinate/= 0 myy)))) ; ?

(defmethod rectilinear-transformation-p ((transformation standard-transformation))
  ;; Das testen wir einfach ganz brutal
  ;;; ist das auch richtig?
  (multiple-value-bind (mxx mxy myx myy) (get-transformation transformation)
    (or (and (coordinate= mxx 0) (coordinate/= mxy 0)
             (coordinate/= myx 0) (coordinate= myy 0))
        (and (coordinate/= mxx 0) (coordinate= mxy 0)
             (coordinate= myx 0) (coordinate/= myy 0)))))

(defmethod y-inverting-transformation-p ((transformation standard-transformation))
  (multiple-value-bind (mxx mxy myx myy) (get-transformation transformation)
    (and (coordinate= mxx 1)
         (coordinate= mxy 0)
         (coordinate= myx 0)
         (coordinate= myy -1))))

(defmethod compose-transformations ((transformation2 standard-transformation) (transformation1 standard-transformation))
  ;; (compose-transformations A B)x = (A o B)x = ABx
  (multiple-value-bind (a1 b1 d1 e1 c1 f1) (get-transformation transformation1)
    (multiple-value-bind (a2 b2 d2 e2 c2 f2) (get-transformation transformation2)
      (make-transformation (+ (* a2 a1) (* b2 d1))
                           (+ (* a2 b1) (* b2 e1))
                           (+ (* d2 a1) (* e2 d1))
                           (+ (* d2 b1) (* e2 e1))
                           (+ (* a2 c1) (* b2 f1) c2)
                           (+ (* d2 c1) (* e2 f1) f2) ))))

(defmethod invert-transformation ((transformation standard-transformation))
  (with-slots (inverse) transformation
    (or inverse
        (let ((computed-inverse
               (restart-case
                (or
                 (handler-case (multiple-value-bind (mxx mxy myx myy tx ty) (get-transformation transformation)
                                 (let ((det (- (* mxx myy) (* myx mxy))))
                                   (if (coordinate= 0 det)
                                       nil
                                     (let ((/det (/ det)))
                                       (let ((mxx (* /det myy))
                                             (mxy (* /det (- mxy)))
                                             (myx (* /det (- myx)))
                                             (myy (* /det mxx)))
                                         (make-transformation mxx mxy myx myy
                                                              (+ (* -1 mxx tx) (* -1 mxy ty))
                                                              (+ (* -1 myx tx) (* -1 myy ty))))))))
                               (error (c)
                                      (error 'singular-transformation :why c :transformation transformation)))
                 (error 'singular-transformation :transformation transformation))
                (use-value (value)
                           :report (lambda (sink)
                                     (format sink "Supply a transformation to use instead of the inverse."))
                           value))))
          (setf (slot-value computed-inverse 'inverse) transformation)
          (setf inverse computed-inverse)
          computed-inverse))))

(defun compose-translation-with-transformation (transformation dx dy)
  (compose-transformations (make-translation-transformation dx dy) transformation))

(defun compose-scaling-with-transformation (transformation sx sy &optional origin)
  (compose-transformations (make-scaling-transformation sx sy origin) transformation))

(defun compose-rotation-with-transformation (transformation angle &optional origin)
  (compose-transformations (make-rotation-transformation angle origin) transformation))

(defun compose-transformation-with-translation (transformation dx dy)
  (compose-transformations transformation (make-translation-transformation dx dy)))

(defun compose-transformation-with-scaling (transformation sx sy &optional origin)
  (compose-transformations transformation (make-scaling-transformation sx sy origin)))

(defun compose-transformation-with-rotation (transformation angle &optional origin)
  (compose-transformations transformation (make-rotation-transformation angle origin)))

(defmacro with-translation ((medium dx dy) &body body)
  `(with-drawing-options (,medium :transformation (make-translation-transformation ,dx ,dy))
     ,@body))

(defmacro with-scaling ((medium sx &optional sy origin) &body body)
  (if sy
      `(with-drawing-options (,medium :transformation (make-scaling-transformation ,sx ,sy ,@(if origin (list origin) nil)))
         ,@body)
    (let ((sx-var (make-symbol "SX")))
      `(let* ((,sx-var ,sx))
         (with-drawing-options (,medium 
                                :transformation (make-scaling-transformation ,sx-var ,sx-var))
           ,@body)) )))

(defmacro with-rotation ((medium angle &optional origin) &body body)
  `(with-drawing-options (,medium 
                          :transformation (make-rotation-transformation ,angle ,@(if origin (list origin) nil)))
     ,@body))

(defmacro with-identity-transformation ((medium) &body body)
  `(with-drawing-options (,medium 
                          :transformation +identity-transformation+)
     ,@body))


;;(defmacro with-local-coordinates ((medium &optional x y) &body body)) -- what are local coordinates?
;;(defmacro with-first-quadrant-coordinates ((medium &optional x y) &body body))

;;(defgeneric transform-region (transformation region))

(defmethod untransform-region ((transformation transformation) region)
  (transform-region (invert-transformation transformation) region))

(defmethod transform-position ((transformation standard-transformation) x y)
  (let ((x (coordinate x))
        (y (coordinate y)))
    (multiple-value-bind (mxx mxy myx myy tx ty)
        (get-transformation transformation)
      (declare (type coordinate mxx mxy myx myy tx ty))
      (values (+ (* mxx x) (* mxy y) tx)
              (+ (* myx x) (* myy y) ty)))))

(defmethod untransform-position ((transformation transformation) x y)
  (transform-position (invert-transformation transformation) x y))

(defmethod transform-distance ((transformation standard-transformation) dx dy)
  (let ((dx (coordinate dx))
        (dy (coordinate dy)))
    (multiple-value-bind (mxx mxy myx myy) (get-transformation transformation)
      (declare (type coordinate mxx mxy myx myy))
      (values (+ (* mxx dx) (* mxy dy))
              (+ (* myx dx) (* myy dy))))))

(defmethod untransform-distance ((transformation transformation) dx dy)
  (transform-distance (invert-transformation transformation) dx dy))

(defun transform-positions (transformation coord-seq)
  (map-repeated-sequence (type-of coord-seq)
                         2 (lambda (x y)
                             (transform-position transformation x y))
                         coord-seq))

(defun transform-position-sequence (seq-type transformation coord-seq)
  (map-repeated-sequence seq-type
                         2 (lambda (x y)
                             (transform-position transformation x y))
                         coord-seq))

(defmethod transform-rectangle* ((transformation transformation) x1 y1 x2 y2)
  (if (rectilinear-transformation-p transformation)
      (multiple-value-bind (x1 y1) (transform-position transformation x1 y1)
        (multiple-value-bind (x2 y2) (transform-position transformation x2 y2)
          (values (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2))))
    (error 'rectangle-transformation-error
           :transformation transformation
           :rect (list x1 y2 x2 y2))))

(defmethod untransform-rectangle* ((transformation transformation) x1 y1 x2 y2)
  (transform-rectangle* (invert-transformation transformation) x1 y1 x2 y2))

(defmethod transformation-transformator ((transformation standard-transformation)
                                         &optional (input-type 'real))
  ;; returns a function, which transforms its arguments
  (multiple-value-bind (mxx mxy myx myy tx ty) (get-transformation transformation)
    (labels ((s* (x y)
               (cond ((coordinate= 0 x) nil)
                     ((coordinate= 1 x) (list y))
                     ((coordinate= -1 x) (list `(- ,y)))
                     ((list `(* ,x ,y)))))
             (s+ (args)
               (cond ((null args)
                      (coerce 0 'coordinate))
                     ((null (cdr args))
                      (car args))
                     (t
                      `(+ .,args)))))
      (compile nil
               `(lambda (x y)
                  (declare (ignorable x y)
                           (type ,input-type x y)
                           (optimize (speed 3) (space 0) (safety 0)))
                  (values
                   ,(s+ (nconc (s* mxx 'x)
                               (s* mxy 'y)
                               (if (coordinate/= 0 tx) (list tx) nil)))
                   ,(s+ (nconc (s* myx 'x)
                               (s* myy 'y)
                               (if (coordinate/= 0 ty) (list ty) nil))))) ))))

(defmethod transformation-transformator ((transformation transformation) &optional (input-type 'real))
  (declare (ignore input-type))
  #'(lambda (x y)
      (transform-position transformation x y)))

(defun atan* (x y)
  ;; atan so, wie wir es brauchen.
  ;; Bei uns ist phi=0 entlang der x-axis und Winkel sind immer zwischen 0 und 2pi.
  (let ((r (atan y x)))
    (if (< r 0) (+ r (* 2 pi)) r)))

(defun correct-angle (a phi)
  (if (< a phi)
      (+ a (* 2 pi))
    a))

(defun transform-angle (transformation phi)
  (multiple-value-bind (rotations remainder) (ffloor phi (* 2 pi))
    (when (reflection-transformation-p transformation)
      (setq rotations  (ffloor (- phi) (* 2 pi))))
    (multiple-value-bind (ix iy) (transform-distance transformation (cos remainder) (sin remainder))
      (multiple-value-bind (x0 y0) (transform-distance transformation 1 0)
        (let ((my-angle (atan* ix iy))
              (null-angle (atan* x0 y0)))
          (+ (* rotations 2 pi) (correct-angle my-angle null-angle)))))))

(defun untransform-angle (transformation phi)
  (multiple-value-bind (rotations remainder) (ffloor phi (* 2 pi))
    (when (reflection-transformation-p transformation)
      (setq rotations  (ffloor (- phi) (* 2 pi))))
    (multiple-value-bind (ix iy) (untransform-distance transformation (cos remainder) (sin remainder))
      (multiple-value-bind (x0 y0) (untransform-distance transformation 1 0)
        (let ((my-angle (atan* ix iy))
              (null-angle (atan* x0 y0)))
          (+ (* rotations 2 pi) (correct-angle my-angle null-angle)))))))

;;; Exports

;;!EXPORT transformation
;;!EXPORT transformationp
;;!EXPORT make-transformation
;;!EXPORT make-translation-transformation
;;!EXPORT make-rotation-transformation
;;!EXPORT make-rotation-transformation*
;;!EXPORT make-scaling-transformation
;;!EXPORT make-scaling-transformation*
;;!EXPORT make-reflection-transformation
;;!EXPORT make-reflection-transformation*
;;!EXPORT make-3-point-transformation
;;!EXPORT make-3-point-transformation*
;;!EXPORT +identity-transformation+
;;!EXPORT transformation-error
;;!EXPORT transformation-underspecified
;;!EXPORT reflection-underspecified
;;!EXPORT singular-transformation
;;!EXPORT transformation-equal
;;!EXPORT identity-transformation-p
;;!EXPORT translation-transformation-p
;;!EXPORT invertible-transformation-p
;;!EXPORT reflection-transformation-p
;;!EXPORT rigid-transformation-p
;;!EXPORT even-scaling-transformation-p
;;!EXPORT scaling-transformation-p
;;!EXPORT rectilinear-transformation-p
;;!EXPORT compose-transformations
;;!EXPORT invert-transformation
;;!EXPORT compose-translation-with-transformation
;;!EXPORT compose-scaling-with-transformation
;;!EXPORT compose-rotation-with-transformation
;;!EXPORT compose-transformation-with-translation
;;!EXPORT compose-transformation-with-scaling
;;!EXPORT compose-transformation-with-rotation
;;!EXPORT with-translation
;;!EXPORT with-scaling
;;!EXPORT with-rotation
;;!EXPORT with-local-coordinates
;;!EXPORT with-first-quadrant-coordinates
;;!EXPORT transform-region
;;!EXPORT untransform-region
;;!EXPORT transform-position
;;!EXPORT untransform-position
;;!EXPORT transform-distance
;;!EXPORT untransform-distance
;;!EXPORT transform-rectangle*
;;!EXPORT untransform-rectangle*

