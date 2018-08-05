(in-package :mcclim-image)

(defmethod medium-free-image-design ((sheet sheet-with-medium-mixin) design)
  (medium-free-image-design (sheet-medium sheet) design))

(defmethod medium-draw-image-design* :before (current-medium design x y)
  (with-slots (medium medium-data) design
    (unless (eq medium current-medium)
      (when medium
	(medium-free-image-design medium design))
      (setf medium current-medium)
      (setf medium-data nil))))

(defmethod medium-draw-image-design*
    ((medium sheet-with-medium-mixin) design x y)
  (medium-draw-image-design* (sheet-medium medium) design x y))
