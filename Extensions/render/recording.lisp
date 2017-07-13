(in-package :mcclim-render-internals)

(def-grecording draw-image (() image x y) ()
  (let ((width (image-width image))
        (height (image-height image))
	(transform (medium-transformation medium)))
    (setf (values x y) (transform-position transform x y))
    (values x y (+ x width) (+ y height))))

(defmethod* (setf output-record-position) :around
                 (nx ny (record draw-image-output-record))
  (with-standard-rectangle* (:x1 x1 :y1 y1) record
    (with-slots (x y) record
      (let ((dx (- nx x1))
            (dy (- ny y1)))
        (multiple-value-prog1 (call-next-method)
          (incf x dx)
          (incf y dy))))))

(defrecord-predicate draw-image-output-record (x y image)
  (and (if-supplied (x coordinate)
	 (coordinate= (slot-value climi::record 'x) x))
       (if-supplied (y coordinate)
	 (coordinate= (slot-value climi::record 'y) y))
       (if-supplied (image image)
         (eq (slot-value climi::record 'image) image))))
