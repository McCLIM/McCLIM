(in-package :clim-internals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities

(defun point-to-complex (point)
  "convert a point to a complex number"
  (complex (point-x point) (point-y point)))

(defun complex-to-point (complex)
  "convert a complex number to a point"
  (make-point (realpart complex) (imagpart complex)))

(defun distance (p0 p1)
  "return the euclidian distance between two points"
  (multiple-value-bind (x0 y0) (point-position p0)
    (multiple-value-bind (x1 y1) (point-position p1)
      (let* ((dx (- x1 x0))
	     (dx2 (* dx dx))
	     (dy (- y1 y0))
	     (dy2 (* dy dy)))
	(sqrt (+ dx2 dy2))))))

(defun part-way (p0 p1 alpha)
  "return a point that is part way between two other points"
  (multiple-value-bind (x0 y0) (point-position p0)
    (multiple-value-bind (x1 y1) (point-position p1)
      (make-point (+ (* (- 1 alpha) x0) (* alpha x1))
		  (+ (* (- 1 alpha) y0) (* alpha y1))))))

(defun dot-dist (p p0 p1)
  "dot distance between a point and a line"
  (let ((dx (- (point-x p1) (point-x p0)))
	(dy (- (point-y p1) (point-y p0))))
    (- (* (point-x p) dy)
       (* (point-y p) dx))))

(defun solve-quadratic (a2 a1 a0 &key complex-roots multiple-roots)
  (when (zerop a2)
    (return-from solve-quadratic (- (/ a0 a1))))
  (unless (= a2 1)
    (setf a1 (/ a1 a2)
	  a0 (/ a0 a2)))
  (let* ((-a1/2 (- (/ a1 2.0)))
	 (r (- (* -a1/2 -a1/2) a0)))
    (cond ((zerop r)
	   (if multiple-roots (values -a1/2 -a1/2) -a1/2))
	  ((minusp r)
	   (if complex-roots (values (+ -a1/2 (sqrt r)) (- -a1/2 (sqrt r))) (values)))
	  (t
	   (values (+ -a1/2 (sqrt r)) (- -a1/2 (sqrt r)))))))
  
(defun dist (v z)
  "compute the distance between a point and a vector represented as a complex number"
  (- (* (realpart z) (point-y v))
     (* (imagpart z) (point-x v))))

(defclass bezier-design (design) 
  ((%or :accessor original-region :initform nil)))

(defgeneric medium-draw-bezier-design* (stream design))

(defclass bezier-design-output-record (standard-graphics-displayed-output-record)
  ((stream :initarg :stream)
   (design :initarg :design)))

(defmethod initialize-instance :after ((record bezier-design-output-record) &key)
  (with-slots (design) record
    (setf (rectangle-edges* record)
	  (bounding-rectangle* design))))

(defmethod medium-draw-bezier-design* :around ((stream output-recording-stream) design)
  (with-sheet-medium (medium stream)
    (let ((transformed-design (transform-region (medium-transformation medium) design)))
      (when (stream-recording-p stream)
	(let ((record (make-instance 'bezier-design-output-record
				     :stream stream
				     :design transformed-design)))
	  (stream-add-output-record stream record)))
      (when (stream-drawing-p stream)
	(medium-draw-bezier-design* medium design)))))

(defmethod medium-draw-bezier-design* :around 
    ((medium transform-coordinates-mixin) design)
  (let* ((tr (medium-transformation medium))
         (design (transform-region tr design)))
    (call-next-method medium design)))

(defmethod replay-output-record ((record bezier-design-output-record) stream &optional
				 (region +everywhere+) (x-offset 0) (y-offset 0))
  (declare (ignore x-offset y-offset region))
  (with-slots (design) record
    (medium-draw-bezier-design* (sheet-medium stream) design)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Bezier curves and areas

(defclass bezier-segment ()
  ((p0 :initarg :p0)
   (p1 :initarg :p1)
   (p2 :initarg :p2)
   (p3 :initarg :p3)))

(defun make-bezier-segment (p0 p1 p2 p3)
  (make-instance 'bezier-segment
		 :p0 p0 :p1 p1 :p2 p2 :p3 p3))

(defclass bounding-rectangle-mixin ()
  ((min-x) (min-y) (max-x) (max-y)))

(defmethod bounding-rectangle* ((region bounding-rectangle-mixin))
  (with-slots (min-x min-y max-x max-y) region
    (values min-x min-y max-x max-y)))

(defclass segments-mixin (bounding-rectangle-mixin)
  ((%segments :initarg :segments :initform '() :reader %segments)))

(defmethod compute-bounding-rectangle* ((segments-mixin segments-mixin))
  (multiple-value-bind (final-min-x final-min-y final-max-x final-max-y)
      (segment-bounding-rectangle (car (%segments segments-mixin)))
    (loop for segment in (cdr (%segments segments-mixin))
	  do (multiple-value-bind (min-x min-y max-x max-y)
		 (segment-bounding-rectangle segment)
	       (setf final-min-x (min final-min-x min-x)
		     final-min-y (min final-min-y min-y)
		     final-max-x (max final-max-x max-x)
		     final-max-y (max final-max-y max-y))))
    (values final-min-x final-min-y final-max-x final-max-y)))

(defmethod initialize-instance :after ((region segments-mixin) &rest args)
  (declare (ignore args))
  (multiple-value-bind (computed-min-x computed-min-y computed-max-x computed-max-y)
      (compute-bounding-rectangle* region)
    (with-slots (min-x min-y max-x max-y) region
      (setf min-x computed-min-x
	    min-y computed-min-y
	    max-x computed-max-x
	    max-y computed-max-y))))

;;; a path defined as a sequence of Bezier curve segments
(defclass bezier-curve (path segments-mixin bounding-rectangle-mixin) ())

(defun make-bezier-thing (class point-seq)
  (assert (= (mod (length point-seq) 3) 1))
  (make-instance class
		 :segments (loop for (p0 p1 p2 p3) on point-seq by #'cdddr
				 until (null p1)
				 collect (make-bezier-segment p0 p1 p2 p3))))

(defun make-bezier-thing* (class coord-seq)
  (assert (= (mod (length coord-seq) 6) 2))
  (make-instance class
		 :segments (loop for (x0 y0 x1 y1 x2 y2 x3 y3 x4 y4)
				 on coord-seq by #'(lambda (x) (nthcdr 6 x))
				 until (null x1)
				 collect (make-bezier-segment
					  (make-point x0 y0)
					  (make-point x1 y1)
					  (make-point x2 y2)
					  (make-point x3 y3)))))

(defun make-bezier-curve (point-seq)
  (make-bezier-thing 'bezier-curve point-seq))

(defun make-bezier-curve* (coord-seq)
  (make-bezier-thing* 'bezier-curve coord-seq))

(defun transform-segment (transformation segment)
  (with-slots (p0 p1 p2 p3) segment
    (make-bezier-segment (transform-region transformation p0)
			 (transform-region transformation p1)
			 (transform-region transformation p2)
			 (transform-region transformation p3))))

(defmethod transform-region (transformation (path bezier-curve))
  (make-instance 'bezier-curve
		 :segments (mapcar (lambda (segment)
				     (transform-segment transformation segment))
				   (%segments path))))

(defmethod region-equal ((p1 point) (p2 point))
  (let ((coordinate-epsilon (* #.(expt 2 10) double-float-epsilon)))
    (and (<= (abs (- (point-x p1) (point-x p2))) coordinate-epsilon)
	 (<= (abs (- (point-y p1) (point-y p2))) coordinate-epsilon))))	 

(defmethod region-union ((r1 bezier-curve) (r2 bezier-curve))
  (let ((p (slot-value (car (last (%segments r1))) 'p3))
	(seg (car (%segments r2))))
    (if (region-equal p (slot-value seg 'p0))
	(with-slots (p1 p2 p3) seg
	  (make-instance 'bezier-curve
			 :segments (append (%segments r1)
					   (cons (make-bezier-segment p p1 p2 p3)
						 (cdr (%segments r2))))))
	(call-next-method))))

;;; an area defined as a closed path of Bezier curve segments
(defclass bezier-area (area bezier-design segments-mixin bounding-rectangle-mixin) 
  ((%trans :initarg :transformation :reader transformation :initform +identity-transformation+)))

(defgeneric close-path (path))

(defmethod close-path ((path bezier-curve))
  (let ((segments (%segments path)))
    (assert (region-equal (slot-value (car segments) 'p0)
			  (slot-value (car (last segments)) 'p3)))
    (make-instance 'bezier-area :segments segments)))

(defun path-start (path)
  (slot-value (car (%segments path)) 'p0))

(defun path-end (path)
  (slot-value (car (last (%segments path))) 'p3))

(defun make-bezier-area (point-seq)
  (assert (region-equal (car point-seq) (car (last point-seq))))
  (make-bezier-thing 'bezier-area point-seq))

(defun make-bezier-area* (coord-seq)
  (assert (and (coordinate= (car coord-seq) (car (last coord-seq 2)))
	       (coordinate= (cadr coord-seq) (car (last coord-seq)))))
  (make-bezier-thing* 'bezier-area coord-seq))

(defmethod segments ((area bezier-area))
  (let ((tr (transformation area)))
    (mapcar (lambda (s) (transform-segment tr s)) (%segments area))))

(defmethod transform-region (transformation (area bezier-area))
  (let* ((tr (transformation area))
         (result (if (translation-transformation-p transformation)
                     (make-instance 'bezier-area :segments (%segments area)
                                    :transformation 
                                    (compose-transformations transformation tr))
                     (make-instance 'bezier-area 
                                    :segments (mapcar (lambda (s) (transform-segment transformation s)) (segments area))))))
    (when (translation-transformation-p transformation)
      (setf (original-region result) (or (original-region area) area)))
    result))

(defmethod compute-bounding-rectangle* ((area bezier-area))
  (multiple-value-bind (lx ly ux uy) (call-next-method)
    (let ((tr (transformation area)))
      (transform-rectangle* tr lx ly ux uy))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Special cases of combined Bezier areas

;;; A union of bezier areas.  This is not itself a bezier area.
(defclass bezier-union (area bezier-design)
  ((%trans :initarg :transformation :reader transformation :initform +identity-transformation+)
   (%areas :initarg :areas :initform '() :reader areas)))

(defmethod transform-region (transformation (union bezier-union))
  (let* ((tr (transformation union))
         (new-tr (compose-transformations transformation tr))
         (result (if (translation-transformation-p transformation)
                     (make-instance 'bezier-union :areas (areas union)
                                    :transformation new-tr)
                     (make-instance 'bezier-union
                                    :areas (loop for area in (areas union) collect (transform-region new-tr area))))))
    (when (translation-transformation-p transformation)
      (setf (original-region result) (or (original-region union) union)))
    result))

(defun bounding-rectangle-of-areas (areas)
  (multiple-value-bind (final-min-x final-min-y final-max-x final-max-y)
      (bounding-rectangle* (car areas))
    (loop for area in (cdr areas)
	  do (multiple-value-bind (min-x min-y max-x max-y)
		 (bounding-rectangle* area)
	       (setf final-min-x (min final-min-x min-x)
		     final-min-y (min final-min-y min-y)
		     final-max-x (max final-max-x max-x)
		     final-max-y (max final-max-y max-y))))
    (values final-min-x final-min-y final-max-x final-max-y)))

(defmethod bounding-rectangle* ((design bezier-union))
  (multiple-value-bind (lx ly ux uy)
      (bounding-rectangle-of-areas (areas design))
    (transform-rectangle* (transformation design) lx ly ux uy)))

(defmethod region-union ((r1 bezier-area) (r2 bezier-area))
  (make-instance 'bezier-union :areas (list r1 r2)))

(defmethod region-union ((r1 bezier-union) (r2 bezier-area))
  (let ((tr (transformation r1)))
    (make-instance 'bezier-union 
                   :areas (cons (untransform-region tr r2) (areas r1))
                   :transformation tr)))

(defmethod region-union ((r1 bezier-area) (r2 bezier-union))
  (let ((tr (transformation r2)))
    (make-instance 'bezier-union 
                   :areas (cons (untransform-region tr r1) (areas r2))
                   :transformation tr)))

(defmethod region-union ((r1 bezier-union) (r2 bezier-union))
  (let ((tr1 (transformation r1))
        (tr2 (transformation r2)))
    (if (transformation-equal tr1 tr2)
        (make-instance 'bezier-union 
                       :areas (append (areas r1) (areas r2))
                       :transformation tr1)
        (let ((len1 (length (areas r1)))
              (len2 (length (areas r2))))
          (if (> len2 len1)
              (make-instance 'bezier-union
                             :areas (append (mapcar (lambda (r) (untransform-region tr2 (transform-region tr1 r))) (areas r1)) (areas r2))
                             :transformation tr2)
              (make-instance 'bezier-union
                             :areas (append (mapcar (lambda (r) (untransform-region tr1 (transform-region tr2 r))) (areas r2)) (areas r1))
                             :transformation tr1))))))

(defclass bezier-difference (area bezier-design)
  ((%positive-areas :initarg :positive-areas :initform '() :reader positive-areas)
   (%negative-areas :initarg :negative-areas :initform '() :reader negative-areas)))

(defmethod transform-region (transformation (area bezier-difference))
  (let* ((pareas (loop for area in (positive-areas area)
                       collect (transform-region transformation area)))
         (nareas (loop for area in (negative-areas area)
                       collect (transform-region transformation area)))
         (result (make-instance 'bezier-difference
                                :positive-areas pareas
                                :negative-areas nareas)))
    (when (translation-transformation-p transformation)
      (setf (original-region result) (or (original-region area) area)))
    result))

(defmethod bounding-rectangle* ((design bezier-difference))
  (bounding-rectangle-of-areas (positive-areas design)))

(defmethod region-difference ((r1 bezier-area) (r2 bezier-area))
  (make-instance 'bezier-difference
		 :positive-areas (list r1)
		 :negative-areas (list r2)))

(defmethod region-difference ((r1 bezier-area) (r2 bezier-union))
  (let ((tr (transformation r2)))
    (make-instance 'bezier-difference
                   :positive-areas (list r1)
                   :negative-areas (mapcar (lambda (r) (transform-region tr r)) (areas r2)))))

(defmethod region-difference ((r1 bezier-union) (r2 bezier-area))
  (let ((tr (transformation r1)))
    (make-instance 'bezier-difference
                   :positive-areas (mapcar (lambda (r) (transform-region tr r)) (areas r1))
                   :negative-areas (list r2))))

(defmethod region-difference ((r1 bezier-union) (r2 bezier-union))
  (let ((tr1 (transformation r1))
        (tr2 (transformation r2)))
    (make-instance 'bezier-difference
                   :positive-areas (mapcar (lambda (r) (transform-region tr1 r)) (areas r1))
                   :negative-areas (mapcar (lambda (r) (transform-region tr2 r)) (areas r2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a path to a polyline or an area to a polygon

;;; convert a cubic bezier segment to a list of 
;;; line segments
(defun %polygonalize (p0 p1 p2 p3 &key (precision 0.01))
  (if (< (- (+ (distance p0 p1)
	       (distance p1 p2)
	       (distance p2 p3))
	    (distance p0 p3))
	 precision)
      (list p3)
      (let* ((p01 (part-way p0 p1 0.5))
	     (p12 (part-way p1 p2 0.5))
	     (p23 (part-way p2 p3 0.5))
	     (p012 (part-way p01 p12 0.5))
	     (p123 (part-way p12 p23 0.5))
	     (p0123 (part-way p012 p123 0.5)))
	(nconc (%polygonalize p0 p01 p012 p0123 :precision precision)
	       (%polygonalize p0123 p123 p23 p3 :precision precision)))))

(defgeneric polygonalize (thing))

(defmethod polygonalize ((segment bezier-segment))
  (with-slots (p0 p1 p2 p3) segment
    (%polygonalize p0 p1 p2 p3)))

(defmethod polygonalize ((path bezier-curve))
  (let ((segments (%segments path)))
    (make-polyline
     (cons (slot-value (car segments) 'p0)
	   (mapcan #'polygonalize segments)))))

(defmethod polygonalize ((area bezier-area))
  (let ((segments (segments area)))
    (make-polygon (mapcan #'polygonalize segments))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reversing a path

(defgeneric reverse-path (path))

(defun reverse-segment (bezier-segment)
  (with-slots (p0 p1 p2 p3) bezier-segment
    (make-bezier-segment p3 p2 p1 p0)))

(defmethod reverse-path ((path bezier-curve))
  (make-instance 'bezier-curve
		 :segments (reverse (mapcar #'reverse-segment (%segments path)))))

(defmethod reverse-path ((path bezier-area))
  (make-instance 'bezier-area
		 :segments (reverse (mapcar #'reverse-segment (%segments path)))
                 :transformation (transformation path)))

;;; slanting transformation are used by Metafont
(defun make-slanting-transformation (slant)
  (make-transformation 1.0 slant 0.0 1.0 0.0 0.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Bounding rectangle

(defun evaluate-bezier (w0 w1 w2 w3 a)
  (let ((1-a (- 1.0 a)))
    (+ (* 1-a 1-a 1-a w0)
       (* 3.0 1-a 1-a a w1)
       (* 3.0 1-a a a w2)
       (* a a a w3))))

(defun dimension-min-max (w0 w1 w2 w3)
  (when (> w0 w3)
    (rotatef w0 w3)
    (rotatef w1 w2))
  (when (and (<= w0 w1 w3)
	     (<= w0 w2 w3))
    (return-from dimension-min-max
      (values w0 w3)))
  (let ((a (+ (- w0) (* 3 w1) (* -3 w2) w3))
	(b (+ (* 2 w0) (* -4 w1) (* 2 w2)))
	(c (- w1 w0)))
    (if (zerop a)
	(if (zerop b)
	    (values w0 w3)
	    (let ((candidate (/ (- c) b)))
	      (if (or (<= candidate 0.0)
		      (>= candidate 1.0))
		  (values w0 w3)
		  (let ((w (evaluate-bezier w0 w1 w2 w3 candidate)))
		    (values (min w w0) (max w w3))))))
	(multiple-value-bind (candidate0 candidate1)
	    (solve-quadratic a b c :multiple-roots t)
	  (if (null candidate0)
	      (values w0 w3)
	      (let ((wa (evaluate-bezier w0 w1 w2 w3 candidate0))
		    (wb (evaluate-bezier w0 w1 w2 w3 candidate1)))
		(if (or (<= candidate0 0.0) (>= candidate0 1.0))
		    (if (or (<= candidate1 0.0) (>= candidate1 1.0))
			(values w0 w3)
			(values (min wb w0) (max wb w3)))
		    (if (or (<= candidate1 0.0) (>= candidate1 1.0))
			(values (min wa w0) (max wa w3))
			(values (min wa wb w0) (max wa wb w3))))))))))

(defun segment-bounding-rectangle (segment)
  (with-slots (p0 p1 p2 p3) segment
    (let ((x0 (point-x p0))
	  (x1 (point-x p1))
	  (x2 (point-x p2))
	  (x3 (point-x p3))
	  (y0 (point-y p0))
	  (y1 (point-y p1))
	  (y2 (point-y p2))
	  (y3 (point-y p3)))
    (multiple-value-bind (min-x max-x)
	(dimension-min-max x0 x1 x2 x3)
      (multiple-value-bind (min-y max-y)
	  (dimension-min-max y0 y1 y2 y3)
	(values min-x min-y max-x max-y))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convolution

(defun find-split-points-for-side (aa bb cc)
  (let ((roots '()))
    (multiple-value-bind (r1 r2)
	(solve-quadratic aa bb cc)
      (unless (or (null r1) (<= r1 0.0) (>= r1 1.0)) (push r1 roots))
      (unless (or (null r2) (<= r2 0.0) (>= r2 1.0)) (push r2 roots))
      roots)))

(defun find-split-points (sides segment)
  (let ((split-points '()))
    (with-slots (p0 p1 p2 p3) segment
      (let ((x0 (point-x p0)) (y0 (point-y p0))
	    (x1 (point-x p1)) (y1 (point-y p1))
	    (x2 (point-x p2)) (y2 (point-y p2))
	    (x3 (point-x p3)) (y3 (point-y p3)))
	(let ((xa (+ (- x0) (* 3 x1) (* -3 x2) x3))
	      (ya (+ (- y0) (* 3 y1) (* -3 y2) y3))
	      (xb (* 2 (+ x0 (* -2 x1) x2)))
	      (yb (* 2 (+ y0 (* -2 y1) y2)))
	      (xc (- x1 x0))
	      (yc (- y1 y0)))
	  (loop for side in sides
		do (let* ((sr (realpart side))
			  (si (imagpart side))
			  (aa (- (* xa si)
				 (* ya sr)))
			  (bb (- (* xb si)
				 (* yb sr)))
			  (cc (- (* xc si)
				 (* yc sr))))
		     (setf split-points
			   (append (find-split-points-for-side aa bb cc) split-points))))))
      (sort (remove-duplicates split-points) #'<))))

(defun split-segment (segment split-points)
  (if (null split-points)
      (list segment)
      (with-slots (p0 p1 p2 p3) segment
	(let* ((n (floor (length split-points) 2))
	       (pivot (nth n split-points))
	       (left (mapcar (lambda (x) (/ x pivot))
			     (subseq split-points 0 n)))
	       (right (mapcar (lambda (x) (/ (- x pivot) (- 1.0 pivot)))
			      (subseq split-points (1+ n))))
	       (p01 (part-way p0 p1 pivot))
	       (p12 (part-way p1 p2 pivot))
	       (p23 (part-way p2 p3 pivot))
	       (p012 (part-way p01 p12 pivot))
	       (p123 (part-way p12 p23 pivot))
	       (p0123 (part-way p012 p123 pivot)))
	  (append (split-segment (make-bezier-segment p0 p01 p012 p0123) left)
		  (split-segment (make-bezier-segment p0123 p123 p23 p3) right))))))

(defun mid-derivative (p0 p1 p2 p3)
  (setf p0 (point-to-complex p0)
	p1 (point-to-complex p1)
	p2 (point-to-complex p2)
	p3 (point-to-complex p3))
  (let ((a 0.5))
    (+ (* a a (+ (- p0) (* 3 p1) (* -3 p2) p3))
       (* 2 a (+ p0 (* -2 p1) p2))
       (- p1 p0))))

(defun make-line-segment (p0 p1)
  (make-bezier-segment p0 (part-way p0 p1 1/3) (part-way p0 p1 2/3) p1))

(defun add-points (p0 p1)
  (make-point (+ (point-x p0) (point-x p1)) (+ (point-y p0) (point-y p1))))

(defun convert-primitive-segment-to-bezier-area (polygon segment)
  (with-slots (p0 p1 p2 p3) segment
    (let* ((m (mid-derivative p0 p1 p2 p3))
	   (right (reduce (lambda (a b) (if (> (dist a m) (dist b m)) a b))
			  polygon))
	   (left (reduce (lambda (a b) (if (< (dist a m) (dist b m)) a b))
			 polygon)))
      (make-instance 'bezier-area
        :segments 
	(list (make-bezier-segment (add-points p0 right) (add-points p1 right)
				   (add-points p2 right) (add-points p3 right))
	      (make-line-segment (add-points p3 right) (add-points p3 left))
	      (make-bezier-segment (add-points p3 left) (add-points p2 left)
				   (add-points p1 left) (add-points p0 left))
	      (make-line-segment (add-points p0 left) (add-points p0 right)))))))

(defun area-at-point (area point)
  (let ((transformation 
	 (make-translation-transformation (point-x point) (point-y point))))
    (transform-region transformation area)))

(defun convolve-polygon-and-segment (area polygon segment first)
  (declare (optimize debug))
  (let* ((points (polygon-points polygon))
	 (sides (loop for (p0 p1) on (append (last points) points)
		      until (null p1)
		      collect (- (point-to-complex p1) (point-to-complex p0))))
	 (split-points (find-split-points sides segment))
	 (segments (split-segment segment split-points)))
    (loop for segment in segments 
	  if first collect (area-at-point area (slot-value segment 'p0))
	  collect (convert-primitive-segment-to-bezier-area 
		   (polygon-points polygon) segment)
	  collect (area-at-point area (slot-value segment 'p3)))))

(defgeneric convolve-regions (area path))

(defmethod convolve-regions ((area bezier-area) (path bezier-curve))
  (let ((polygon (polygonalize area)))
    (make-instance 
     'bezier-union :areas 
     (loop for segment in (%segments path)
	   for first = t then nil
	   append (convolve-polygon-and-segment area polygon segment first)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Rendering

(defclass scanlines ()
  ((%first-line :initform 0 :accessor first-line)
   (%chain :initform (make-instance 'flexichain:standard-flexichain) :reader chain)))

(defun nb-lines (lines)
  (flexichain:nb-elements (chain lines)))

(defun crossings (lines i)
  (flexichain:element* (chain lines) (- i (first-line lines))))

(defun line-number-to-index (lines line-number)
  (let* ((chain (chain lines))
	 (size (flexichain:nb-elements chain)))
    ;; make sure there is an element corresponding to the line number
    (cond ((zerop size)
	   (flexichain:insert* chain 0 '())
	   (setf (first-line lines) line-number))
	  ((< line-number (first-line lines))
	   (loop for i from line-number below (first-line lines)
		 do (flexichain:insert* chain 0 '()))
	   (setf (first-line lines) line-number))
	  ((>= line-number (+ (first-line lines) size))
	   (loop for i from (+ (first-line lines) size) to line-number
		 do (flexichain:insert* chain size '()))))
    (- line-number (first-line lines))))

;;; insert a single crossing into LINES
(defun insert-crossing (lines line-number x inverse-p)
  (let ((chain (chain lines))
	(index (line-number-to-index lines line-number)))
    (setf (flexichain:element* chain index)
	  (merge 'list
		 (flexichain:element* chain index)
		 (list (cons x inverse-p)) #'< :key #'car))))

;;; compute the crossings of a line segment and insert
;;; them into LINES
(defun compute-crossings (lines p0 p1)
  (let ((inverse-p nil))
    (when (< (point-y p1) (point-y p0))
      (rotatef p0 p1)
      (setf inverse-p t))
    (let ((x0 (point-x p0)) (y0 (point-y p0))
	  (x1 (point-x p1)) (y1 (point-y p1)))
      (loop for y from (round y0) below (round y1)
	    for x = (+ x0 (* (- x1 x0) (/ (- (+ y 0.5) y0) (- y1 y0))))
	    do (insert-crossing lines y x inverse-p)))))

(defun scan-lines (polygon)
  (let ((lines (make-instance 'scanlines))
	(points (polygon-points polygon)))
    (loop for (p0 p1) on (append (last points) points)
	  until (null p1)
	  do (compute-crossings lines p0 p1))
    lines))

(defun render-scan-lines (array pixel-value line crossings min-x min-y)
  (let ((level 0)
	(start nil)
	(height (array-dimension array 0))
	(width (array-dimension array 1)))
    (loop for (x . inverse-p) in crossings
	  do (when (zerop level)
	       (setf start x))
	  do (setf level (if inverse-p (1+ level) (1- level)))
	  do (when (zerop level)
	       (loop for c from (round start) below (round x)
		     do (when (and (<= 0 (round (- line min-y)) (1- height))
				   (<= 0 (- c min-x) (1- width)))
			  (setf (aref array (round (- line min-y)) (- c min-x))
				pixel-value)))))))

(defun render-polygon (array polygon pixel-value min-x min-y)
  (let ((lines (scan-lines polygon)))
    (loop for i from (first-line lines)
	  repeat (nb-lines lines)
	  do (render-scan-lines array pixel-value i (crossings lines i) min-x min-y))))

(defgeneric positive-negative-areas (design))

(defmethod positive-negative-areas ((design bezier-area))
  (values (list design) '()))

(defmethod positive-negative-areas ((design bezier-union))
  (values (areas design) '()))

(defmethod positive-negative-areas ((design bezier-difference))
  (values (positive-areas design) (negative-areas design)))

(defun render-to-array (design)
  (multiple-value-bind (positive-areas negative-areas)
      (positive-negative-areas design)
    (multiple-value-bind (min-x min-y max-x max-y)
	(bounding-rectangle-of-areas positive-areas)
      (setf min-x (* 4 (floor min-x))
	    min-y (* 4 (floor min-y))
	    max-x (* 4 (ceiling max-x))
	    max-y (* 4 (ceiling max-y)))
      (let ((result (make-array (list (- max-y min-y) (- max-x min-x))
				:element-type 'bit :initial-element 1))
	    (transformation (make-scaling-transformation* 4 4)))
	(loop for area in positive-areas
	      do (let* ((transformed-area (transform-region transformation area))
			(polygon (polygonalize transformed-area)))
		   (render-polygon result polygon 0 min-x min-y)))
	(loop for area in negative-areas
	      do (let* ((transformed-area (transform-region transformation area))
			(polygon (polygonalize transformed-area)))
		   (render-polygon result polygon 1 min-x min-y)))
	result))))

(defparameter *pixmaps* (make-hash-table :test #'equal))

(defun resolve-ink (medium)
  (if (eq (medium-ink medium) +foreground-ink+)
      (medium-foreground medium)
      (medium-ink medium)))

(defun make-ink (medium transparency)
  (let* ((a (/ transparency 16.0))
	 (1-a (- 1.0 a)))
    (multiple-value-bind (r g b) (color-rgb (resolve-ink medium))
      (make-rgb-color (+ (* a 1.0) (* 1-a r))
		      (+ (* a 1.0) (* 1-a g))
		      (+ (* a 1.0) (* 1-a b))))))

(defgeneric ensure-pixmap (medium design))

(defmethod ensure-pixmap (medium rdesign)
  (let* ((design (or (original-region rdesign) rdesign))
         (pixmap (gethash (list (medium-sheet medium) (resolve-ink medium) design)
			 *pixmaps*)))
    (when (null pixmap)
      (let* ((picture (render-to-array design))
	     (height (array-dimension picture 0))
	     (width (array-dimension picture 1))
	     (reduced-picture (make-array (list (/ height 4) (/ width 4)) :initial-element 16)))
	(loop for l from 0 below height
	      do (loop for c from 0 below width
		       do (when (zerop (aref picture l c))
			    (decf (aref reduced-picture (floor l 4) (floor c 4))))))
	(setf pixmap
	      (with-output-to-pixmap (pixmap-medium
				      (medium-sheet medium)
				      :width (/ width 4) :height (/ height 4))
		(loop for l from 0 below (/ height 4)
		      do (loop for c from 0 below (/ width 4)
			       do (draw-point*
				   pixmap-medium c l
				   :ink (make-ink
					 medium
					 (aref reduced-picture l c)))))))
	(setf (gethash (list (medium-sheet medium) (resolve-ink medium) design)
		       *pixmaps*)
	      pixmap)))
    pixmap))

(defun render-through-pixmap (design medium)
  (multiple-value-bind (min-x min-y)
      (bounding-rectangle* design)
    ;; the design we've got has already been transformed by the
    ;; medium/user transformation, and COPY-FROM-PIXMAP is in user
    ;; coordinates.  So we need to transform back (or set the medium's
    ;; transformation to be +IDENTITY-TRANSFORMATION+ temporarily, but
    ;; that's even uglier)
    (multiple-value-bind (utmin-x utmin-y)
        (untransform-position (medium-transformation medium) min-x min-y)
      (setf min-x (floor utmin-x)
            min-y (floor utmin-y))
      (let ((pixmap (ensure-pixmap medium design)))
        (copy-from-pixmap pixmap 0 0 (pixmap-width pixmap) (pixmap-height pixmap)
                          (medium-sheet medium) min-x min-y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic drawing

(defun draw-bezier-design* (sheet design &rest options)
  (climi::with-medium-options (sheet options)
    (medium-draw-bezier-design* sheet design)))

(defmethod draw-design (medium (design bezier-design) &rest options &key &allow-other-keys)
  (apply #'draw-bezier-design* medium design options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing bezier designs to screen


;;; Fallback method (suitable for CLX)

(defmethod medium-draw-bezier-design* (medium design)
  (render-through-pixmap design medium))

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test cases

(defparameter *r1* (make-bezier-area* '(10 10 20 20 30 20 40 10 30 5 20 5 10 10)))

(defparameter *r2* (make-bezier-area* '(15 10 20 12 30 15 35 10 30 8 20 8 15 10)))

(defparameter *r3* (region-difference *r1* *r2*))

(defparameter *r4* (make-bezier-curve* '(100 100 120 150 160 160 170 160)))

(defparameter *r5* (convolute-regions *r2* *r4*))
|#
