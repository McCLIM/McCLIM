#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test cases

(defparameter *r1* (make-bezier-area* '(10 10 20 20 30 20 40 10 30 5 20 5 10 10)))

(defparameter *r2* (make-bezier-area* '(15 10 20 12 30 15 35 10 30 8 20 8 15 10)))

(defparameter *r3* (region-difference *r1* *r2*))

(defparameter *r4* (make-bezier-curve* '(100 100 120 150 160 160 170 160)))

(defparameter *r5* (convolute-regions *r2* *r4*))
|#
