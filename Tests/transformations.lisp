(in-package :clim-tests)

(assert (subtypep (class-of +identity-transformation+) 'transformation))
(assert (transformationp +identity-transformation+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; conditions on transformations

(assert (subtypep 'transformation-error 'error))

(assert (subtypep 'transformation-underspecified  'transformation-error))
(assert (typep (make-condition 'transformation-underspecified
			       :points (list (make-point 1 1)
					     (make-point 2 1)
					     (make-point 3 1)))
	       'transformation-error))

(assert (subtypep 'reflection-underspecified  'transformation-error))
(assert (typep (make-condition 'reflection-underspecified
			       :points (list (make-point 1 1)
					     (make-point 3 1)))
	       'transformation-error))


(assert (subtypep 'singular-transformation  'transformation-error))
(assert (typep (make-condition 'singular-transformation
			       :transformation +identity-transformation+)
	       'transformation-error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; different kinds of transformations

(assert (typep (make-translation-transformation 10 20)
	       'transformation))

(assert (typep (make-transformation 10 20 30 40 50 60) 'transformation))

(let* ((x1 30) (y1 40) (x2 10) (y2 20) (x3 30) (y3 55)
       (p1 (make-point x1 y1)) (p2 (make-point x2 y2)) (p3 (make-point x3 y3)))
  (assert (typep (make-rotation-transformation 10) 'transformation))
  (assert (typep (make-rotation-transformation 10 p2) 'transformation))
  (assert (typep (make-rotation-transformation* 10) 'transformation))
  (assert (typep (make-rotation-transformation* 10 x1 y1) 'transformation))
  (assert (typep (make-scaling-transformation 10 50) 'transformation))
  (assert (typep (make-scaling-transformation 10 50 p1) 'transformation))
  (assert (typep (make-scaling-transformation* 10 50) 'transformation))
  (assert (typep (make-scaling-transformation* 10 50 x1 y1) 'transformation))
  (assert (typep (make-reflection-transformation p2 p3) 'transformation))
  (assert (typep (make-reflection-transformation* x2 y2 x3 y3) 'transformation))
  (let* ((x4 66) (y4 89) (x5 85) (y5 13) (x6 867) (y6 -58)
	 (p4 (make-point x4 y4)) (p5 (make-point x5 y5)) (p6 (make-point x6 y6)))
    (assert (typep (make-3-point-transformation p1 p2 p3 p4 p5 p6) 'transformation))
    (assert (typep (make-3-point-transformation* x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 x6 y6)
		   'transformation))))
