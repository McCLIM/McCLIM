(in-package :clim-clxv3)

;;; CLXv3-MEDIUM class

(defclass clxv3-medium (clx-medium)
  ())

(defmethod medium-draw-rectangle-using-ink* ((medium clxv3-medium) (ink t) left top right bottom filled)
  (let ((tr (sheet-native-transformation (medium-sheet medium)))
	(clipping-region (medium-device-region medium)))
    (cond
      ((region-equal clipping-region +nowhere+)
       nil)
      ((typep clipping-region 'standard-rectangle)
       (multiple-value-bind (x1 y1 width height)
           (region->clipping-values clipping-region)
	 (with-transformed-position (tr left top)
	   (with-transformed-position (tr right bottom)
	     (with-clx-graphics () medium
	       (if (< right left)
		   (rotatef left right))
	       (if (< bottom top)
		   (rotatef top bottom))
	       (let ((left   (max x1 (min (+ x1 width) (round-coordinate left))))
		     (top    (max y1 (min (+ y1 height) (round-coordinate top))))
		     (right  (max x1 (min (+ x1 width) (round-coordinate right))))
		     (bottom (max y1 (min (+ y1 height) (round-coordinate bottom)))))
		 ;; To clip rectangles, we just need to clamp the
		 ;; coordinates
		 (xlib:draw-rectangle clim-clx::mirror clim-clx::gc
				      (max #x-8000 (min #x7FFF left))
				      (max #x-8000 (min #x7FFF top))
				      (max 0 (min #xFFFF (- right left)))
				      (max 0 (min #xFFFF (- bottom top)))
				      filled))))))))))
