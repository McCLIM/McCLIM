;;;
;;; recording for draw-image-design
;;;

(in-package :mcclim-image)

(def-grecording draw-image-design (() image-design x y) ()
  (let ((width (image-width (image image-design)))
        (height (image-height (image image-design)))
	(transform (medium-transformation medium)))
    (setf (values x y) (transform-position transform x y))
    (values x y (+ x width) (+ y height))))

(defmethod* (setf output-record-position) :around
            (nx ny (record draw-image-design-output-record))
  (with-standard-rectangle* (:x1 x1 :y1 y1) record
    (with-slots (x y) record
      (let ((dx (- nx x1))
            (dy (- ny y1)))
        (multiple-value-prog1 (call-next-method)
          (incf x dx)
          (incf y dy))))))

(defrecord-predicate draw-image-design-output-record (x y image-design)
  (and (if-supplied (x coordinate)
	 (coordinate= (slot-value record 'x) x))
       (if-supplied (y coordinate)
	 (coordinate= (slot-value record 'y) y))
       (if-supplied (image-design rgb-image-design)
         (eq (slot-value record 'image-design) image-design))))
