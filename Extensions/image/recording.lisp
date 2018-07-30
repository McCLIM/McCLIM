;;;
;;; recording for draw-image-design
;;;

(in-package :mcclim-image)

(def-grecording draw-image-design ((climi::position-transform-mixin) image-design x y transformation)
    (:replay-fn nil)
  (let ((width (image-width (image image-design)))
        (height (image-height (image image-design))))
    (climi::enclosing-transform-polygon transformation (list x y
                                                             (+ x width) y
                                                             (+ x width) (+ y height)
                                                             x (+ y height)))))

(defmethod replay-output-record
    ((record draw-image-design-output-record) stream
     &optional (region +everywhere+) (x-offset 0) (y-offset 0))
  (declare (ignore x-offset y-offset region))
  (with-slots (image-design x y transformation)
      record
    (let* ((medium (sheet-medium stream))
           (dx (climi::transformed-dx record))
           (dy (climi::transformed-dy record))
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
