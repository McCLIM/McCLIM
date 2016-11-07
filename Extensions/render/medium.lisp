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

(defgeneric %medium-stroke-paths (medium paths))
(defgeneric %medium-fill-paths (medium paths))
(defgeneric %medium-draw-paths (medium paths))

(defmethod %medium-stroke-paths ((medium render-medium-mixin) paths)
  (let ((paths (mapcar
		(lambda (path)
		  (stroke-path (transform-path path
						(sheet-native-transformation
						 (medium-sheet medium)))
			       (medium-line-style medium)))
		paths)))
    (%medium-draw-paths medium paths)))

(defmethod %medium-fill-paths ((medium render-medium-mixin) paths)
  (let ((paths (mapcar
		(lambda (path)
		  (setf (paths::path-type path) :closed-polyline)
		  (transform-path path
				  (sheet-native-transformation
				   (medium-sheet medium))))
		paths)))
    (%medium-draw-paths medium paths)))


(defmethod %medium-draw-paths ((medium render-medium-mixin) paths)
  (let ((msheet (sheet-mirrored-ancestor (medium-sheet medium))))
    (%draw-paths msheet msheet paths
		 (climi::medium-device-region medium)
		 (medium-ink medium)
		 (medium-background medium)
		 (medium-foreground medium))))



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
  (medium-draw-circle* medium x y
		       (max 1 (/ (line-style-thickness (medium-line-style medium)) 2))
		       0
		       6.28
		       t))

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
      (let ((loader (zpb-ttf-font-loader (truetype-font-face xfont))))
	(let* ((box (string-primitive-box 0 0 string loader size))
	       (text-width (- (elt box 2) (elt box 0)))
	       (text-height (- (elt box 3) (elt box 1)))
	       (baseline (elt box 1)))
	  (setq x (- x (ecase align-x
			 (:left 0)
			 (:center (round text-width 2))
			 (:right text-width))))
	  (setq y (ecase align-y
		    (:top (+ y baseline (+ text-height)))
		    (:center (+ y baseline (+ (floor text-height 2))))
		    (:baseline y)
		    (:bottom (+ y baseline)))))
	(multiple-value-bind (x y)
	    (transform-position (sheet-native-transformation
				   (medium-sheet medium))
				x y)
	  (let ((paths (string-primitive-paths x y string loader size)))
	    (%medium-fill-paths medium paths)))))))

(defmethod medium-copy-area ((from-drawable render-medium-mixin) from-x from-y width height
                             (to-drawable render-medium-mixin) to-x to-y)
  (break)
	  (medium-draw-rectangle* to-drawable
				  to-x to-y
				  (+ to-x width) (+ to-y height)
				  :ink (medium-sheet from-drawable)
				  :filled t))

(defmethod medium-copy-area ((from-drawable image-sheet-mixin) from-x from-y width height
                             (to-drawable render-medium-mixin) to-x to-y)
  (with-drawing-options (to-drawable :ink (climi::transform-region
					   (make-translation-transformation (- to-x) (- to-y))
					   from-drawable))
    (medium-draw-rectangle* (medium-sheet to-drawable)
			    to-x to-y
			    (+ to-x width) (+ to-y height)
			    t)))

(defmethod medium-finish-output ((medium render-medium-mixin))
  (let ((msheet (sheet-mirrored-ancestor (medium-sheet medium))))
    (%flush msheet)))

(defmethod medium-force-output ((medium render-medium-mixin))
  (let ((msheet (sheet-mirrored-ancestor (medium-sheet medium))))
    (%flush msheet)))
