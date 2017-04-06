(in-package :mcclim-render)



(defclass render-medium-mixin (basic-medium)
  ())

(defmethod initialize-instance :after ((medium render-medium-mixin)
				       &rest initargs)
  (declare (ignore initargs))
  nil)

;;;
;;; protocols
;;;

(defgeneric %medium-stroke-paths (medium paths &optional transform-p))
(defgeneric %medium-fill-paths (medium paths &optional transform-p transformation))
(defgeneric %medium-draw-paths (medium paths transformation))

(defmethod %medium-stroke-paths ((medium render-medium-mixin) paths
				 &optional (transform-p t))
  (let ((paths (mapcar
		(lambda (path)
		  (stroke-path path
			       (medium-line-style medium)))
		paths)))
    (%medium-draw-paths medium (car paths)
			(if transform-p
			    (sheet-native-transformation
			     (medium-sheet medium))
			    +identity-transformation+))))

(defmethod %medium-fill-paths ((medium render-medium-mixin) paths
			       &optional (transform-p t) (transformation +identity-transformation+))
  (let ((paths (mapcar
		(lambda (path)
		  (setf (paths::path-type path) :closed-polyline)
		  path)
		paths)))
    (%medium-draw-paths medium paths (if transform-p
					 (compose-transformations
					  (sheet-native-transformation
					   (medium-sheet medium))
					  transformation)
					 transformation))))

(defmethod %medium-draw-paths ((medium render-medium-mixin) paths transformation)
  (let ((msheet (sheet-mirrored-ancestor (medium-sheet medium))))
    (when (and msheet (sheet-mirror msheet))
      (%draw-paths (sheet-mirror msheet) paths transformation
		   (climi::medium-device-region medium)
		   (transform-region (sheet-native-transformation (medium-sheet medium))
				     (medium-ink medium))
		   (medium-background medium)
		   (medium-foreground medium)))))

(defmethod %medium-draw-image ((medium render-medium-mixin) image
			       to-x to-y width height dst-dx dst-dy)
  (let ((msheet (sheet-mirrored-ancestor (medium-sheet medium))))
    (when (and msheet (sheet-mirror msheet))
      (%draw-image (sheet-mirror msheet)
		   (image-mirror-image (sheet-mirror image))
		   (round to-x) (round to-y)
		   (round width)
		   (round height)
		   (round dst-dx)
		   (round dst-dy)
		   (climi::medium-device-region medium)))))

(defmethod %medium-fill-image-mask ((medium render-medium-mixin) image from-x from-y width height to-x to-y)
  (let ((msheet (sheet-mirrored-ancestor (medium-sheet medium))))
    (when (and msheet (sheet-mirror msheet))
      (%fill-image-mask (sheet-mirror msheet)
			image

			(round from-x) (round from-y)	
			(round width)
			(round height)
			(round to-x) (round to-y)			
			(climi::medium-device-region medium)
			(transform-region (sheet-native-transformation (medium-sheet medium))
					  (medium-ink medium))
			(medium-background medium)
			(medium-foreground medium)))))

(defmethod %medium-fill-image ((medium render-medium-mixin) x y width height)
  (let ((msheet (sheet-mirrored-ancestor (medium-sheet medium))))
    (when (and msheet (sheet-mirror msheet))
      (%fill-image (sheet-mirror msheet)
		   (round x) (round y)
		   (round width)
		   (round height)
		   (transform-region (sheet-native-transformation (medium-sheet medium))
				     (medium-ink medium))
		   (medium-background medium)
		   (medium-foreground medium)))))

;;;
;;; standard medium protocol
;;;

(defgeneric medium-draw-rectangle-using-ink*
    (medium ink left top right bottom filled))

(defmethod medium-draw-rectangle* ((medium render-medium-mixin) left top right bottom filled)
  (medium-draw-rectangle-using-ink* medium (medium-ink medium)
                                    left top right bottom filled))

(defmethod medium-draw-rectangle-using-ink* ((medium render-medium-mixin) (ink t) left top right bottom filled)
  (if (< right left)
      (rotatef left right))
  (if (< bottom top)
      (rotatef top bottom))
  (let* ((clip-region (climi::medium-device-region medium))
	 (region
	  (region-intersection
	   clip-region
	   (transform-region (sheet-native-transformation (medium-sheet medium))
			     (make-rectangle* left top right bottom)))))
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	region
      (if (and
	   filled
	   (not (typep ink 'standard-flipping-ink))
	   (clim:rectanglep region)
	   (< (abs (- min-x (round min-x))) 0.01)
	   (< (abs (- max-x (round max-x))) 0.01)
	   (< (abs (- min-y (round min-y))) 0.01)
	   (< (abs (- max-y (round max-y))) 0.01))
	  (progn
	    (%medium-fill-image medium min-x min-y (- max-x min-x) (- max-y min-y)))
	  (let ((path (make-path left top)))
	    (line-to path right top)
	    (line-to path right bottom)
	    (line-to path left bottom)
	    (close-path path)
	    (if filled
		(%medium-fill-paths medium (list path))
		(%medium-stroke-paths medium (list path))))))))

(defmethod medium-draw-rectangles* ((medium render-medium-mixin) position-seq filled)
  (assert (evenp (length position-seq)))
  (do ((v 0 (+ 4 v)))
      ((>= v (length position-seq)))
    (let ((x1 (elt position-seq (+ 0 v)))
	  (y1 (elt position-seq (+ 1 v)))
	  (x2 (elt position-seq (+ 2 v)))
	  (y2 (elt position-seq (+ 3 v))))
      (medium-draw-rectangle* medium x1 y1 x2 y2 filled))))
  
(defmethod medium-draw-polygon* ((medium render-medium-mixin) coord-seq closed filled)
  (let ((x (elt coord-seq 0))
	(y (elt coord-seq 1)))	  
    (let ((path (make-path x y)))
      (do ((v 2 (+ 2 v)))
	  ((>= v (length coord-seq)))
	(let ((x (elt coord-seq v))
	      (y (elt coord-seq (1+ v))))
	  (line-to path x y)))
      (when closed
	(close-path path))
      (if filled
	  (%medium-fill-paths medium (list path))
	  (%medium-stroke-paths medium (list path))))))

(defmethod medium-draw-line* ((medium render-medium-mixin) x1 y1 x2 y2)
  (let ((path (make-path x1 y1)))
    (line-to path x2 y2)
    (%medium-stroke-paths medium (list path))))

(defmethod medium-draw-point* ((medium render-medium-mixin) x y)
  (let ((path (arc x y
		   (max 1 (/ (line-style-thickness (medium-line-style medium)) 2))
		   pi
		   (+ pi (* 2 pi)))))
    (%medium-fill-paths medium (list path))))

(defmethod medium-draw-circle* ((medium render-medium-mixin)
				center-x center-y radius start-angle end-angle
				filled)
  (let ((path (arc center-x center-y radius (+ pi start-angle) (+ pi end-angle))))
    (if filled
	(%medium-fill-paths medium (list path))
	(%medium-stroke-paths medium (list path)))))
  
(defmethod medium-draw-ellipse* ((medium render-medium-mixin) center-x center-y
				 radius-1-dx radius-1-dy
				 radius-2-dx radius-2-dy
				 start-angle end-angle filled)
  (let* ((arc-angle (- end-angle start-angle))
	 (arc-angle (if (< arc-angle 0)
			(+ (* pi 2) arc-angle)
			arc-angle)))
    ;;; error!!
    (let* ((radius-dx (abs (+ radius-1-dx radius-2-dx)))
	   (radius-dy (abs (+ radius-1-dy radius-2-dy))))
      (let ((path (ellipse-arc center-x center-y radius-dx radius-dy
			       0 ;;(- end-angle start-angle)
			       (- end-angle)
			       (- start-angle)
			       )))
	(when filled
	  (line-to path center-x center-y))
	(if filled
	    (%medium-fill-paths medium (list path))
	    (%medium-stroke-paths medium (list path)))))))

(defmethod medium-draw-text* ((medium render-medium-mixin) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (let ((xfont (text-style-to-font (port medium) (medium-text-style medium))))
    (let ((size (text-style-size (medium-text-style medium))))
      (setf size   (or size :normal)
	    size (getf *text-sizes* size size))
      (when (characterp string)
	(setq string (make-string 1 :initial-element string)))
      (when (null end) (setq end (length string)))
      (multiple-value-bind (text-width text-height x-cursor y-cursor baseline)
	  (text-size medium string :start start :end end)
	(declare (ignore x-cursor y-cursor))
	(unless (and (eq align-x :left) (eq align-y :baseline))
	  (setq x (- x (ecase align-x
			 (:left 0)
			 (:center (round text-width 2))
			 (:right text-width))))
	  (setq y (ecase align-y
		    (:top (+ y (- baseline text-height)
			     (+ text-height)))
		    (:center (+ y (- baseline text-height)
				(+ (floor text-height 2))))
		    (:baseline y)
		    (:bottom (+ y (- baseline text-height))))))
	(let ((paths (string-primitive-paths x y string xfont size
					     (lambda (paths opacity-image dx dy transformation)
					       (let ((msheet (sheet-mirrored-ancestor (medium-sheet medium))))
						 (when (and msheet (sheet-mirror msheet))
						   (multiple-value-bind (x1 y1)
						       (transform-position
							(clim:compose-transformations transformation
										      (sheet-native-transformation
										       (medium-sheet medium)))
							(+ dx ) (-  dy))
						     
						     (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
							 (region-intersection
							  (climi::medium-device-region medium)
							  (make-rectangle* x1 y1 (+ -1 x1 (climi::image-width opacity-image)) (+ -1 y1 (climi::image-height opacity-image))))
						       
						       (%medium-fill-image-mask
							medium
							opacity-image
							min-x min-y
							(- max-x min-x) (- max-y min-y)
							(- (round x1)) (- (round y1))
							)))))
					       
					       #+nil(%medium-fill-paths medium paths t transformation)
					       )))))))))

(defmethod medium-copy-area ((from-drawable render-medium-mixin) from-x from-y width height
                             (to-drawable render-medium-mixin) to-x to-y)
  (medium-force-output from-drawable)
  (medium-force-output to-drawable)
  (let* ((msheet (sheet-mirrored-ancestor (medium-sheet to-drawable)))
	 (from-sheet (medium-sheet from-drawable))
	 (from-transformation (sheet-native-transformation from-sheet))
	 (to-sheet (medium-sheet to-drawable))
	 (to-transformation (sheet-native-transformation to-sheet)))
    (when (and msheet (sheet-mirror msheet))
      (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	  (region-intersection
	   (climi::medium-device-region to-drawable)
	   (transform-region
	    (sheet-native-transformation (medium-sheet to-drawable))
	    (make-rectangle* to-x to-y (+ to-x width) (+ to-y height))))
	(multiple-value-bind (x1 y1)
	    (transform-position
	     (sheet-native-transformation (medium-sheet to-drawable))
	     to-x to-y)
	  (multiple-value-bind (x2 y2)
	    (transform-position
	     (sheet-native-transformation (medium-sheet from-drawable))
	     from-x from-y)
	  (%medium-draw-image to-drawable
			      (medium-sheet from-drawable)
			      min-x min-y
			      (- max-x min-x) (- max-y min-y)
			      (- x2 x1) (- y2 y1))))))))
    

(defmethod medium-copy-area ((from-drawable render-medium-mixin) from-x from-y width height
                             (to-drawable image-sheet-mixin) to-x to-y)
  (medium-force-output from-drawable)
  (let* ((msheet (sheet-mirrored-ancestor to-drawable))
	 (from-sheet (medium-sheet from-drawable))
	 (from-transformation (sheet-native-transformation from-sheet)))
    (when (and msheet (sheet-mirror msheet))
      (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	  (region-intersection
	   (climi::sheet-native-region to-drawable)
	   (transform-region
	    (sheet-native-transformation to-drawable)
	    (make-rectangle* to-x to-y (+ to-x width) (+ to-y height))))
	(multiple-value-bind (x1 y1)
	    (transform-position
	     (sheet-native-transformation to-drawable)
	     to-x to-y)
	  (multiple-value-bind (x2 y2)
	      (transform-position
	       (sheet-native-transformation (medium-sheet from-drawable))
	       from-x from-y)
	    (climi::with-pixmap-medium (to-medium to-drawable)
	      (%medium-draw-image (sheet-medium to-drawable)
				  (medium-sheet from-drawable)
				  min-x min-y
				  (- max-x min-x) (- max-y min-y)
				  (- x2 x1) (- y2 y1)))))))))

(defmethod medium-copy-area ((from-drawable image-sheet-mixin) from-x from-y width height
                             (to-drawable render-medium-mixin) to-x to-y)
  (medium-force-output (sheet-medium from-drawable))
  (medium-force-output to-drawable)
  (let ((msheet (sheet-mirrored-ancestor (medium-sheet to-drawable))))
    (when (and msheet (sheet-mirror msheet))
      (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	  (region-intersection
	   (climi::medium-device-region to-drawable)
	   (transform-region
	    (sheet-native-transformation (medium-sheet to-drawable))
	    (make-rectangle* to-x to-y (+ to-x width) (+ to-y height))))
	(multiple-value-bind (x1 y1)
	    (transform-position
	     (sheet-native-transformation (medium-sheet to-drawable))
	     to-x to-y)
	  (%medium-draw-image to-drawable
			      from-drawable
			      min-x min-y
			      (- max-x min-x) (- max-y min-y)
			      (- from-x x1) (- from-y y1)))))
    (medium-force-output to-drawable)))

(defmethod medium-draw-image-design* ((medium render-medium-mixin)
				      (design climi::rgb-image-design) to-x to-y)
  (let* ((image (slot-value design 'climi::image))
	 (width (climi::image-width image))
	 (height (climi::image-height image))
	 (to-sheet (medium-sheet medium)))
    (with-drawing-options (medium :ink (climi::transform-region
					(make-translation-transformation
					 to-x to-y)
					design))
      
      (medium-draw-rectangle* medium
			      to-x to-y
			      (+ to-x width) (+ to-y height)
			      t))))

(defmethod medium-finish-output ((medium render-medium-mixin))
  )

(defmethod medium-force-output ((medium render-medium-mixin))
  )

;;;
;;;
;;;


