(in-package :mcclim-render-internals)

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

(defmethod %medium-stroke-paths ((medium render-medium-mixin) paths
				 &optional (transform-p t))
  (let ((msheet (sheet-mirrored-ancestor (medium-sheet medium))))
    (when (and msheet (sheet-mirror msheet))
      (%stroke-paths (sheet-mirror msheet) paths
                     (medium-line-style medium)
                     (if transform-p
                         (sheet-native-transformation
                          (medium-sheet medium))
                         +identity-transformation+)
                     (climi::medium-device-region medium)
                     (transform-region (sheet-native-transformation (medium-sheet medium))
                                       (medium-ink medium))
                     (medium-background medium)
                     (medium-foreground medium)))))

(defmethod %medium-fill-paths ((medium render-medium-mixin) paths
			       &optional (transform-p t) (transformation +identity-transformation+))
  (declare (ignore transformation))
  (let ((msheet (sheet-mirrored-ancestor (medium-sheet medium))))
    (when (and msheet (sheet-mirror msheet))
      (%fill-paths (sheet-mirror msheet) paths
                   (if transform-p
                       (sheet-native-transformation
                        (medium-sheet medium))
                       +identity-transformation+)
                   (climi::medium-device-region medium)
                   (transform-region (sheet-native-transformation (medium-sheet medium))
                                     (medium-ink medium))
                   (medium-background medium)
                   (medium-foreground medium)))))

(defmethod %medium-draw-image ((medium render-medium-mixin) (image basic-sheet)
                               x y width height to-x to-y)
  (let ((msheet (sheet-mirrored-ancestor (medium-sheet medium))))
    (when (and msheet (sheet-mirror msheet))
      (%draw-image (sheet-mirror msheet)
                   (image-mirror-image (sheet-mirror image))
		   (round x) (round y)
		   (round width) (round height)
		   (round to-x) (round to-y)
		   (climi::medium-device-region medium)))))

(defmethod %medium-draw-image ((medium render-medium-mixin) (image basic-image)
                               x y width height to-x to-y)
  (let ((msheet (sheet-mirrored-ancestor (medium-sheet medium))))
    (when (and msheet (sheet-mirror msheet))
      (%draw-image (sheet-mirror msheet)
                   image
		   (round x) (round y)
		   (round width) (round height)
		   (round to-x) (round to-y)
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
		   (medium-foreground medium)
                   (climi::medium-device-region medium)))))

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
  (let* ((region
	  (region-intersection
	   (climi::medium-device-region medium)
	   (transform-region (sheet-native-transformation (medium-sheet medium))
			     (make-rectangle* left top right bottom)))))
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	region
      (if (and
	   filled
	   (not (typep ink 'standard-flipping-ink))
	   (clim:rectanglep region))
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
				 start-angle end-angle filled
                                 &aux (el (make-ellipse*
                                           center-x center-y
                                           radius-1-dx radius-1-dy
                                           radius-2-dx radius-2-dy
                                           :start-angle start-angle
                                           :end-angle end-angle)))
  (multiple-value-bind (cx cy hx hy theta) (climi::ellipse-simplified-representation el)
    (declare (ignorable cx cy))
    (let* ((sa (- (* 2 pi) end-angle theta))
           (dalpha (- end-angle start-angle))
           (path (ellipse-arc center-x center-y hx hy theta
                              sa (+ sa dalpha))))
      (when filled
        (line-to path center-x center-y))
      (if filled
          (%medium-fill-paths medium (list path))
          (%medium-stroke-paths medium (list path))))))

(defmethod medium-draw-text* ((medium render-medium-mixin) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs
                              transformation)
  (multiple-value-bind (x y)
      (transform-position transformation x y)
    (flet ((draw-font-glypse (paths opacity-image dx dy transformation)
             (declare (ignore paths))
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
		        (make-rectangle* x1 y1 (+ -1 x1 (image-width opacity-image)) (+ -1 y1 (image-height opacity-image))))
		     (%medium-fill-image-mask
		      medium
		      opacity-image
		      min-x min-y
		      (- max-x min-x) (- max-y min-y)
		      (- (round x1)) (- (round y1)))))))))
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
	    (string-primitive-paths x y string xfont size #'draw-font-glypse)))))))
					 
(defmethod medium-copy-area ((from-drawable render-medium-mixin) from-x from-y width height
                             (to-drawable render-medium-mixin) to-x to-y)
  (medium-force-output from-drawable)
  (multiple-value-bind (w2 h2)
      (untransform-distance (medium-transformation from-drawable)
                            width height)
    (multiple-value-bind (w h)
        (transform-distance (sheet-transformation (medium-sheet to-drawable))
                            w2 h2)
      (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
          (region-intersection
           (climi::medium-device-region to-drawable)
           (transform-region
            (sheet-native-transformation (medium-sheet to-drawable))
            (make-rectangle* to-x to-y (+ to-x w) (+ to-y h))))
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
                                (+ x2 (- min-x x1))
                                (+ y2 (- min-y y1))
                                (- max-x min-x) (- max-y min-y)
                                min-x min-y)))))))

(defmethod medium-copy-area ((from-drawable render-medium-mixin) from-x from-y width height
                             (to-drawable image-sheet-mixin) to-x to-y)
  (medium-force-output from-drawable)
  (let* ((msheet (sheet-mirrored-ancestor to-drawable)))
    (when (and msheet (sheet-mirror msheet))
      (multiple-value-bind (w2 h2)
          (untransform-distance (medium-transformation from-drawable)
                                width height)
        (multiple-value-bind (w h)
            (transform-distance (sheet-transformation to-drawable)
                                w2 h2)
          (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
              (region-intersection
               (climi::sheet-native-region to-drawable)
               (transform-region
                (sheet-native-transformation to-drawable)
                (make-rectangle* to-x to-y (+ to-x w) (+ to-y h))))
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
                                      (+ x2 (- min-x x1))
                                      (+ y2 (- min-y y1))
                                      (- max-x min-x) (- max-y min-y)
                                      min-x min-y))))))))))

(defmethod medium-copy-area ((from-drawable image-sheet-mixin) from-x from-y width height
                             (to-drawable render-medium-mixin) to-x to-y)
  (medium-force-output (sheet-medium from-drawable))
  (let ((msheet (sheet-mirrored-ancestor (medium-sheet to-drawable))))
    (when (and msheet (sheet-mirror msheet))
      (multiple-value-bind (w2 h2)
          (untransform-distance (medium-transformation (sheet-medium from-drawable))
                                width height)
        (multiple-value-bind (w h)
            (transform-distance (sheet-transformation (medium-sheet to-drawable))
                                w2 h2)
          (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
              (region-intersection
               (climi::medium-device-region to-drawable)
               (transform-region
                (sheet-native-transformation (medium-sheet to-drawable))
                (make-rectangle* to-x to-y (+ to-x w) (+ to-y h))))
            (multiple-value-bind (x1 y1)
                (transform-position
                 (sheet-native-transformation (medium-sheet to-drawable))
                 to-x to-y)
              (multiple-value-bind (x2 y2)
                  (transform-position
                   (sheet-native-transformation from-drawable)
                   from-x from-y)
                (%medium-draw-image to-drawable
                                    from-drawable
                                    (+ x2 (- min-x x1))
                                    (+ y2 (- min-y y1))
                                    (- max-x min-x) (- max-y min-y)
                                    min-x min-y)))))))))

(defmethod mcclim-image::medium-draw-image-design* ((medium render-medium-mixin)
                                                    (design mcclim-image::rgb-image-design) to-x to-y)
  (let* ((image (slot-value design 'mcclim-image::image))
	 (width (mcclim-image::image-width image))
	 (height (mcclim-image::image-height image))
	 (to-sheet (medium-sheet medium))
         (region
          (region-intersection
           (climi::medium-device-region medium)
           (transform-region (sheet-native-transformation to-sheet)
                             (make-rectangle* to-x to-y (+ to-x width) (+ to-y height))))))
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
        region
      (if (clim:rectanglep region)
          (multiple-value-bind (x1 y1)
              (transform-position
               (sheet-native-transformation to-sheet)
               to-x to-y)
            (%medium-draw-image medium
                                (if (typep image 'image)
                                    image
                                    (coerce-image image 'rgb-image))
                                (+ 0 (- min-x x1))
                                (+ 0 (- min-y y1))
                                (- max-x min-x)
                                (- max-y min-y)
                                min-x min-y))
          (with-drawing-options (medium :ink (climi::transform-region
                                              (make-translation-transformation
                                               to-x to-y)
                                              design))
            (medium-draw-rectangle* medium
                                    to-x to-y
                                    (+ to-x width) (+ to-y height)
                                    t))))))

(defmethod medium-draw-image* ((medium render-medium-mixin)
                               (image drawable-image) to-x to-y)
  (let* ((width (image-width image))
	 (height (image-height image))
	 ;; (to-sheet (medium-sheet medium))
         (region
          (region-intersection
           (climi::medium-device-region medium)
           (transform-region ;;(compose-transformations
                              ;;(sheet-native-transformation to-sheet)
                              (sheet-device-transformation (medium-sheet medium));;)
                             (make-rectangle* to-x to-y (+ to-x width) (+ to-y height))))))
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
        region
      (if (clim:rectanglep region)
          (multiple-value-bind (x1 y1)
              (transform-position
               ;;(compose-transformations
                ;;(sheet-native-transformation to-sheet)
                (sheet-device-transformation (medium-sheet medium));;)
               to-x to-y)
            (%medium-draw-image medium
                                (if (typep image 'image)
                                    image
                                    (coerce-image image 'rgb-image))
                                (+ 0 (- min-x x1))
                                (+ 0 (- min-y y1))
                                (- max-x min-x)
                                (- max-y min-y)
                                min-x min-y))
          (with-drawing-options (medium :ink (climi::transform-region
                                              (make-translation-transformation
                                               to-x to-y)
                                              (make-image-design image)))
            (medium-draw-rectangle* medium
                                    to-x to-y
                                    (+ to-x width) (+ to-y height)
                                    t))))))

(defmethod medium-finish-output ((medium render-medium-mixin))
  (when (sheet-mirror (medium-sheet medium))
    (%mirror-force-output (sheet-mirror (medium-sheet medium)))))

(defmethod medium-force-output ((medium render-medium-mixin))
  (when (sheet-mirror (medium-sheet medium))
    (%mirror-force-output (sheet-mirror (medium-sheet medium)))))
