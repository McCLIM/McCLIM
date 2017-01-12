(in-package :mcclim-render)

(declaim (inline round-coordinate))
(defun round-coordinate (x)
  (floor (+ x .5)))

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
(defgeneric %medium-fill-paths (medium paths &optional transform-p))
(defgeneric %medium-draw-paths (medium paths))

(defmethod %medium-stroke-paths ((medium render-medium-mixin) paths
				 &optional (transform-p t))
  (let ((paths (mapcar
		(lambda (path)
		  (if transform-p
		      (stroke-path (transform-path path
						   (sheet-native-transformation
						    (medium-sheet medium)))
				   (medium-line-style medium))
		      (stroke-path path
				   (medium-line-style medium))))
		paths)))
    (%medium-draw-paths medium (car paths))))

(defmethod %medium-fill-paths ((medium render-medium-mixin) paths
			       &optional (transform-p t))
  (let ((paths (mapcar
		(lambda (path)
		  (setf (paths::path-type path) :closed-polyline)
		  (if transform-p
		      (transform-path path
				      (sheet-native-transformation
				       (medium-sheet medium)))
		      path))
		paths)))
    (%medium-draw-paths medium paths)))

(defmethod %medium-draw-paths ((medium render-medium-mixin) paths)
  (let ((msheet (sheet-mirrored-ancestor (medium-sheet medium))))
    (when (and msheet (sheet-mirror msheet))
      (%draw-paths (sheet-mirror msheet) msheet paths
		   ;; to fix
		   ;;(region-intersection
		    (climi::medium-device-region medium)
		    ;;(sheet-region msheet))
		   (transform-region (sheet-native-transformation (medium-sheet medium))
				     (medium-ink medium))
		   (medium-background medium)
		   (medium-foreground medium)))))

(defmethod %medium-draw-image ((medium render-medium-mixin) image from-x from-y width height to-x to-y)
  (let ((msheet (sheet-mirrored-ancestor (medium-sheet medium))))
    (when (and msheet (sheet-mirror msheet))
      (%draw-image (sheet-mirror msheet) msheet image
		   (round from-x) (round from-y)
		   (round width)
		   (round height)
		   (round to-x) (round to-y)))))
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
  (let ((path (make-path left top)))
    (line-to path right top)
    (line-to path right bottom)
    (line-to path left bottom)
    (close-path path)
    (if filled
	(%medium-fill-paths medium (list path))
	(%medium-stroke-paths medium (list path)))))


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
	(multiple-value-bind (x y)
	    (transform-position (sheet-native-transformation
				   (medium-sheet medium))
				x y)
	  (let ((paths (string-primitive-paths x y string xfont size)))
	    (%medium-fill-paths medium paths nil)))))))

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
      (with-transformed-position (from-transformation from-x from-y)
	(with-transformed-position (to-transformation to-x to-y)
	  (multiple-value-bind (width height)
	      (transform-distance (medium-transformation from-drawable)
				  width height)
	    (%medium-draw-image to-drawable
			      (medium-sheet from-drawable)
			      from-x from-y width height to-x to-y)))))))

(defmethod medium-copy-area ((from-drawable render-medium-mixin) from-x from-y width height
                             (to-drawable image-sheet-mixin) to-x to-y)
  (medium-force-output from-drawable)
  (let* ((msheet (sheet-mirrored-ancestor to-drawable))
	 (from-sheet (medium-sheet from-drawable))
	 (from-transformation (sheet-native-transformation from-sheet)))
    (when (and msheet (sheet-mirror msheet))
      (with-transformed-position (from-transformation from-x from-y)
	(climi::with-pixmap-medium (to-medium to-drawable)
	  (%medium-draw-image (sheet-medium to-drawable)
			      (medium-sheet from-drawable)
			      from-x from-y width height to-x to-y))))))

(defmethod medium-copy-area ((from-drawable image-sheet-mixin) from-x from-y width height
                             (to-drawable render-medium-mixin) to-x to-y)
  (medium-force-output (sheet-medium from-drawable))
  (medium-force-output to-drawable)
  (let ((msheet (sheet-mirrored-ancestor (medium-sheet to-drawable))))
    (when (and msheet (sheet-mirror msheet))
      (with-transformed-position ((sheet-native-transformation (medium-sheet to-drawable))
				  to-x to-y)
	(%medium-draw-image to-drawable
			    from-drawable
			    from-x from-y width height to-x to-y))))
  (medium-force-output to-drawable))

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

(defmethod clim:transform-region (transformation (design named-color))
  design)

(defmethod clim:transform-region (transformation (design standard-flipping-ink))
  design)

