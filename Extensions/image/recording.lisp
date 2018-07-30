;;;
;;; recording for draw-image-design
;;;

(in-package :mcclim-image)

(def-grecording draw-image-design ((climi::draw-text-transform-mixin) image-design x y transformation)
    (:replay-fn nil)
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

(defmethod replay-output-record
    ((record draw-image-design-output-record) stream
     &optional (region +everywhere+) (x-offset 0) (y-offset 0))
  (declare (ignore x-offset y-offset region))
  (with-slots (image-design x y transformation)
      record
    (let* ((medium (sheet-medium stream))
           (dx (climi::draw-text-transform-mixin-transformed-dx record))
           (dy (climi::draw-text-transform-mixin-transformed-dy record))
           (updated-transform (clim:compose-transformations (clim:make-translation-transformation dx dy)
                                                            transformation)))
      (medium-draw-image-design* medium image-design x y updated-transform))))

(defrecord-predicate draw-image-design-output-record (x y image-design)
  (and (if-supplied (x coordinate)
	 (coordinate= (slot-value record 'x) x))
       (if-supplied (y coordinate)
	 (coordinate= (slot-value record 'y) y))
       (if-supplied (image-design rgb-image-design)
         (eq (slot-value record 'image-design) image-design))))
