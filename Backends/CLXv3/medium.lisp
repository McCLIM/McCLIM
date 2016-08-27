(in-package :clim-clxv3)

;;; CLXv3-MEDIUM class

(defclass clxv3-medium (clx-medium)
  ())

(defmethod clim-clx::medium-gcontext ((medium clxv3-medium) (ink climi::indexed-pattern))
  (multiple-value-bind (mx my)
      ;; For unmirrored sheet we need to apply the native transformation.
      ;; May be it is the wrong place to do it.
      (transform-position (sheet-native-transformation (medium-sheet medium)) 0 0)
    (let ((gc-x (round-coordinate mx))
	  (gc-y (round-coordinate my))
	  (gc (design-gcontext medium ink)))
      (setf (xlib:gcontext-ts-x gc) gc-x
	    (xlib:gcontext-ts-y gc) gc-y
	    (xlib:gcontext-clip-x gc) gc-x
	    (xlib:gcontext-clip-y gc) gc-y)
      gc)))
 
(defmethod clim-clx::medium-gcontext ((medium clxv3-medium) (ink climi::rectangular-tile))
  (multiple-value-bind (mx my)
      ;; For unmirrored sheet we need to apply the native transformation.
      ;; May be it is the wrong place to do it.
      (transform-position (sheet-native-transformation (medium-sheet medium)) 0 0)
    (let ((gc-x (round-coordinate mx))
	  (gc-y (round-coordinate my))
	  (gc (design-gcontext medium ink)))
      (setf (xlib:gcontext-ts-x gc) gc-x
	    (xlib:gcontext-ts-y gc) gc-y
	    (xlib:gcontext-clip-x gc) gc-x
	    (xlib:gcontext-clip-y gc) gc-y)
      gc)))

(defmethod clim-clx::medium-gcontext ((medium clxv3-medium) (ink climi::transformed-design))
  (let ((transformation (climi::transformed-design-transformation ink))
        (design (climi::transformed-design-design ink)))
    (unless (translation-transformation-p transformation)
      (error "Sorry, not yet implemented."))
    ;; Bah!
    (typecase design
      ((or climi::indexed-pattern climi::rectangular-tile)
       (multiple-value-bind (tx ty)
	   (transform-position transformation 0 0)
	 (let ((gc-x (round-coordinate tx))
	       (gc-y (round-coordinate ty))
	       (gc (clim-clx::medium-gcontext medium design)))
	   (setf (xlib:gcontext-ts-x gc) (+ gc-x (xlib:gcontext-ts-x gc))
		 (xlib:gcontext-ts-y gc) (+ gc-y (xlib:gcontext-ts-y gc))
		 (xlib:gcontext-clip-x gc) (+ gc-x (xlib:gcontext-clip-x gc))
		 (xlib:gcontext-clip-y gc) (+ gc-y (xlib:gcontext-clip-y gc)))
	   gc)))
      (t
       (error "You lost, we not yet implemented transforming an ~S."
              (type-of ink))))))

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
