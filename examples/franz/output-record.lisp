;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIM; Base: 10; Lowercase: Yes -*-

;;; Hacks for efficiently drawing really big scientific graphs.
;;; Needs to be clim2.0 ified.
;;; $fiHeader: output-record.lisp,v 1.2 92/10/07 14:43:41 cer Exp $

(in-package :clim)
;;; Vendors have started locking this package, so we shouldn't be using it.

;;;**********
;;; Utilities
;;;**********

(defun bsearch (number array &optional
		       (key #'identity)
		       (start 0)
		       (end (1- (length array))))
  "Binary search to find a number in a sorted array."
  ;; If a number is inbetween two values, return the first of the two.
  (declare (values index element))
  (if (< end start)
      (values nil 0)
      (let ((array array) mid Nmid found)
	#+genera (declare (sys:array-register array))
	(loop
	  (if (or found (< end start)) (return))
	  (setq mid (truncate (+ start end) 2))
	  (setq Nmid (funcall key (aref array mid)))
	  (cond ((< number Nmid)
		 (if (= start mid)
		     (setq found (max 0 (1- mid)))
		     (setq end (1- mid))))
		((> number Nmid)
		 (if (= end mid)
		     (setq found mid)
		     (setq start (1+ mid))))
		(t ;; (= number Nmid)
		 (setq found mid))))  ; found it!
	(when found (values mid (aref array found))))))

;;; A hack to provide default method, simplifies protocol
;;; that user is required to provide.
(defmethod region-contains-point*-p ((region t) x y)
  (with-bounding-rectangle* (left top right bottom) region
    (ltrb-contains-point*-p left top right bottom x y)))

;;; A hack to provide default method, simplifies protocol
;;; that user is required to provide.
(defmethod region-intersects-region-p ((region1 t) (region2 t))
  (with-bounding-rectangle* (left1 top1 right1 bottom1) region1
    (with-bounding-rectangle* (left2 top2 right2 bottom2) region2
      (ltrb-overlaps-ltrb-p left1 top1 right1 bottom1
			    left2 top2 right2 bottom2))))

;;;*******************************
;;; The basic unoptimized protocol
;;;*******************************

(defclass simple-datum ()
    ((u :initarg :u)
     (v :initarg :v)
     (size :initform 2)
     ))

(defmethod uv-position ((self simple-datum))
  (with-slots (u v) self
    (values u v)))

(defmethod draw-datum ((self simple-datum) x y stream)
  (draw-point* stream x y))

(defmethod display-datum ((self simple-datum) x y stream)
  (with-output-as-presentation (:stream stream
				:object self
				:type 'expression
				:single-box t)
    (draw-datum self x y stream)))

(defun make-simple-datum (u v) (make-instance 'simple-datum :u u :v v))

(defclass dataset ()
    ((data :initform nil :initarg :data :accessor data)
     (x)
     (y)
     (width :initform 500 :initarg :width :accessor width)
     (height :initform 60 :initarg :height :accessor height)
     (ux-scale :initform 20)
     (vy-scale :initform 20)))

(defmethod xy-position ((dataset dataset))
  ;; stream coordinates of upper left corner.
  (values (slot-value dataset 'x) (slot-value dataset 'y)))

(defmethod set-xy-position ((dataset dataset) x y)
  (setf (slot-value dataset 'x) x
	(slot-value dataset 'y) y))

(defmethod size ((dataset dataset))
  (values (width dataset) (height dataset)))

(defmethod set-size ((dataset dataset) width height)
  (multiple-value-bind (w h) (size dataset)
    (when (or (/= w width) (/= h height))
      (setf (width dataset) width
	    (height dataset) height)
      ;; Now recompute scale.
      (let (minu maxu minv maxv)
	(map-data-uv dataset
		     #'(lambda (u v datum)
			 (declare (ignore datum))
			 (minf-or minu u)
			 (minf-or minv v)
			 (maxf-or maxu u)
			 (maxf-or maxv v)))
	(with-slots (ux-scale vy-scale) dataset
	  (setq ux-scale (/ (float width) (- maxu minu)))
	  (setq vy-scale (/ (float height) (- maxv minv))))))))

(defmethod map-data ((dataset dataset) function &key start end)
  (let ((data (data dataset)))
    (or start (setq start 0))
    (or end (setq end (length data)))
    (dovector (element data :start start :end end)
      (funcall function element))))

(defmethod map-data-uv ((dataset dataset) function &key start end)
  (map-data dataset
	    #'(lambda (datum)
		(declare (sys:downward-function))
		(multiple-value-bind (u v) (uv-position datum)
		  (funcall function u v datum)))
	    :start start
	    :end end))

(defmethod map-data-xy ((dataset dataset) function &key start end)
  ;; This translates from UV to XY coordinates.
  ;; This simple transformation simply provides offsets.
  ;; In a full implementation, need scaling as well.
  (with-slots (x y ux-scale vy-scale) dataset
    (let ((xoff x)
	  (yoff (+ y (truncate (height dataset) 2))))
      (map-data-uv dataset
		   #'(lambda (u v datum)
		       #+genera (declare (sys:downward-function))
		       (funcall function
				(+ (truncate (* u ux-scale)) xoff)
				(+ (truncate (* v vy-scale)) yoff)
				datum))
		   :start start
		   :end end))))

(defmethod display-data ((dataset dataset) stream &key (width 500) (height 60))
  (set-size dataset width height)
  (multiple-value-bind (x y) (stream-cursor-position* stream)
    (set-xy-position dataset x y))
  (let ((record
	  (with-output-as-presentation (:stream stream
					:object dataset
					:type 'expression
					:single-box t)
	    (map-data-xy dataset
			 #'(lambda (x y datum)
			     #+genera (declare (sys:downward-function))
			     (display-datum datum x y stream))))))
    (move-cursor-beyond-output-record stream record)
    record))

(defun make-sample-data (number-of-samples u0 u1 function noise-level maker)
  (let ((data (make-array number-of-samples))
	(dx (/ (- u1 u0) number-of-samples)))
    (dotimes (i number-of-samples)
      (let* ((x (+ u0 (* i dx)))
	     (y (+ (funcall function x)
		   (* noise-level
		      (- (random 1.0) 0.5)))))
	(setf (aref data i)
	      (funcall maker x y))))
    data))

(defvar *dataset1*
	(make-instance
	  'dataset
	  :data (make-sample-data 1000 0.0 25.0 #'sin 0.75 #'make-simple-datum)))

(defmacro elapsed-time (&body body)
  `(without-interrupts
     (let ((.start. (get-internal-run-time)))
       (values
	 (progn ,@body)
	 (/ (float (- (get-internal-run-time) .start.)) internal-time-units-per-second)))))

(defun graph1 (&optional (stream *standard-output*))
  (terpri stream)
  (elapsed-time (display-data *dataset1* stream)))

;;;*************************************************************
;;; Heavyweight datums implemented as lightweight output records
;;;*************************************************************

;;; This optimization relieves the implementation from consing an output
;;; record for each datum.

(defclass datum (presentation displayed-output-record-element simple-datum)
    (;; XY -- stream coordinates (relative to output-record-parent)
     (x :initarg :x)
     (y :initarg :y)     
     (parent :initform nil :accessor output-record-parent)
     (type :initform 'expression :reader presentation-type)))

(defmethod initialize-instance :after ((datum datum) &key object type single-box)
  ;; This just eats init keywords that records are supposed to handle.
  (declare (ignore object type single-box)))

(defmethod xy-position ((datum datum))
  (with-slots (x y) datum (values x y)))

(defmethod presentation-object ((datum datum))
  datum)

(defmethod presentation-single-box ((datum datum)) t)

(defmethod bounding-rectangle* ((datum datum))
  (with-slots (x y size) datum
    (values (- x size) (- y size) (+ x size) (+ y size))))

(defmethod output-record-start-position* ((datum datum))
  ;; Doesn't matter what is returned unless this guy has children.
  (with-slots (x y size) datum (values (- x size) (- y size))))

(defmethod output-record-set-position* ((datum datum) new-x new-y)
  ;; NEW-X and NEW-Y had better be fixnums
  (with-slots (x y) datum (setf x new-x y new-y)))

(defmethod map-over-output-record-elements-overlapping-region
	   ((datum datum) region continuation
	    &optional (x-offset 0) (y-offset 0) &rest continuation-args)
  (declare (ignore region continuation x-offset y-offset continuation-args))
  (declare (dynamic-extent continuation-args))
  nil)

(defmethod map-over-output-record-elements-containing-point*
	   ((datum datum) x y continuation
	    &optional (x-offset 0) (y-offset 0) &rest continuation-args)
  (declare (ignore x y continuation x-offset y-offset continuation-args))
  (declare (dynamic-extent continuation-args))
  nil)

(defmethod tree-recompute-extent ((datum datum))
  nil)

(defmethod replay-1 ((datum datum) stream
		     &optional region (x-offset 0) (y-offset 0))
  (declare (ignore region))
  ;; Region is the region being replayed (might want to clip to it).
  (with-slots (x y size) datum
    #+ignore
    (draw-datum datum (+ x x-offset) (+ y y-offset) stream)
    ;; We use draw-point-internal here because it bypasses all of
    ;; the higher level functions, which is OK during REPLAY.
    ;; This is what all the REPLAY methods do for standard output records.
    #-ignore
    (draw-point-internal stream x-offset y-offset x y 
			 (medium-ink stream) (medium-line-style stream))))

(defmethod output-record-refined-sensitivity-test ((datum datum) x y)
  (declare (ignore x y))
  t)

(defmethod highlight-output-record-1 ((datum datum) stream state)
  (declare (ignore state))
  (let ((x (slot-value datum 'x))
	(y (slot-value datum 'y))
	(size (slot-value datum 'size)))
    (multiple-value-bind (xoff yoff)
	(convert-from-relative-to-absolute-coordinates stream
						       (output-record-parent datum))
      (incf x xoff)
      (incf y yoff)
      (draw-rectangle* stream (- x size 1) (- y size 1) (+ x size 1) (+ y size 1)
		       :filled nil
		       :ink +flipping-ink+))))

(defmethod display-datum ((self datum) x y stream)
  (when (stream-record-p stream)
    (multiple-value-bind (abs-x abs-y)
	(point-position* (output-recording-stream-output-record-absolute-position stream))
      (output-record-set-position* self (- x abs-x) (- y abs-y)))
    (add-output-record stream self))		; sets parent
  ;; We're going to call replay at a high level
  (when (stream-draw-p stream)
    (replay self stream)))

(defun make-heavy-datum (u v) (make-instance 'datum :u u :v v))

(defvar *dataset2*
	(make-instance
	  'dataset
	  :data (make-sample-data 1000 0.0 25.0 #'sin 0.75 #'make-heavy-datum)))

(defun graph2 (&optional (stream *standard-output*))
  ;; About 2.5x faster than GRAPH1
  (terpri stream)
  (elapsed-time (display-data *dataset2* stream)))

;;;**********************
;;; dataset-output-record
;;;**********************

;;; This optimization provides a new type of output record that is
;;; very similar to linear-output-record.  

(defclass dataset-output-record (output-record-mixin output-record-element-mixin dataset)
    ())

(define-output-record-constructor dataset-output-record (&key x-position y-position (size 5))
  :x-position x-position :y-position y-position :size size)

;;; For debugging.
(defmethod output-record-elements ((record dataset-output-record))
  (with-slots (data) record
    (let ((result (make-list (length data))))
      (replace result data)
      result)))

(defmethod output-record-element ((record dataset-output-record) index)
  (with-slots (data) record
    (aref data index)))

(defmethod output-record-element-count ((record dataset-output-record))
  (with-slots (data) record
    (length data)))

(defmethod clear-output-record ((record dataset-output-record))
  (error "Can't do that"))

(defmethod add-output-record-element ((record dataset-output-record) element)
  (declare (ignore element))
  ;; this isn't an error because DISPLAY-DATUM want's to do it.
  nil)

(defmethod delete-output-record-element
	   ((record dataset-output-record) element &optional (errorp t))
  (declare (ignore element))
  (when errorp (error "Can't do that")))

(defmethod map-over-output-record-elements-overlapping-region
	   ((record dataset-output-record) region continuation 
	    &optional (x-offset 0) (y-offset 0) &rest continuation-args)
  (declare (dynamic-extent continuation continuation-args))
  (declare (fixnum x-offset y-offset))
  ;;(declare (optimize (safety 0)))
  (with-slots (data) record
    (if (or (null region) (eql region +everywhere+))
	(map-data record
		  #'(lambda (element)
		      (apply continuation element continuation-args)))
	(with-bounding-rectangle* (left1 top1 right1 bottom1) region
	  (translate-fixnum-positions x-offset y-offset left1 top1 right1 bottom1)
	  ;; Subtract out the record offset from the region, to make comparison fair
	  (multiple-value-bind (xoff yoff)
	      (output-record-position* record)
	    (translate-fixnum-positions (- xoff) (- yoff) left1 top1 right1 bottom1))
	  (map-data-xy record
		       #'(lambda (ex ey element)
			   (if (> ex right1)
			       (return-from
				 map-over-output-record-elements-overlapping-region
				 nil))
			   (when (and (>= ex left1)
				      #+ignore (<= top1 ey bottom1))
			     (apply continuation element continuation-args)))
		       :start (bsearch left1 (data record) #'xy-position)
		       ))))
  nil)

(defmethod map-over-output-record-elements-containing-point*
	   ((record dataset-output-record) x y continuation 
	    &optional (x-offset 0) (y-offset 0) &rest continuation-args)
  (declare (dynamic-extent continuation continuation-args))
  (declare (fixnum x y x-offset y-offset))
  (declare (optimize (safety 0)))
  (translate-fixnum-positions x-offset y-offset x y)
  (with-slots (data) record
    (multiple-value-bind (xoff yoff)
	(output-record-position* record)
      (translate-fixnum-positions (- xoff) (- yoff) x y))
    (map-data record
	      #'(lambda (element)
		  (with-bounding-rectangle* (left top right bottom) element
		    (if (> x right)
			(return-from
			  map-over-output-record-elements-containing-point*
			  nil))
		    (when (ltrb-contains-point*-p left top right bottom x y)
		      (apply continuation element continuation-args))))
	      :start (bsearch x (data record) #'xy-position)
	      ))
  nil)

(defmethod display-data ((dataset dataset-output-record) stream &key (width 500) (height 60))
  (set-size dataset width height)
  (multiple-value-bind (x y) (stream-cursor-position* stream)
    (set-xy-position dataset x y))
  (let ((record
	  (with-output-as-presentation (:stream stream
					:object dataset
					:type 'expression
					:single-box t)
	    (multiple-value-bind (abs-x abs-y)
		(point-position*
		  (output-recording-stream-output-record-absolute-position stream))
	      (multiple-value-bind (x y) (stream-cursor-position* stream)
		(output-record-set-position* dataset (- x abs-x) (- y abs-y))))
	    (add-output-record stream dataset)
	    (replay dataset stream))))
    (move-cursor-beyond-output-record stream record)
    record))

(defmethod cache-datum-positions ((dataset dataset-output-record))
  ;; This needs to run every time the translation from UV to XY
  ;; coordinates changes.  It does not need to run when the position
  ;; of the output record is changed, because XY coordinates of datum
  ;; is relative to XY position of parent.
  (set-xy-position dataset 0 0)
  (let (left top right bottom)
    (map-data-xy dataset
		 #'(lambda (x y datum)
		     (output-record-set-position* datum x y)
		     (with-bounding-rectangle* (ll tt rr bb) datum
		       (minf-or left ll)
		       (minf-or top  tt)
		       (maxf-or right  rr)
		       (maxf-or bottom bb)
		       (setf (output-record-parent datum) dataset))))
    (bounding-rectangle-set-edges dataset left top right bottom)))

(defmethod initialize-instance :after ((dataset dataset-output-record) &key)
  (cache-datum-positions dataset))

(defmethod set-size :around ((dataset dataset-output-record) new-width new-height)
  (multiple-value-bind (w h) (size dataset)
    (call-next-method dataset new-width new-height)
    (when (or (/= w new-width) (/= h new-height))
      (cache-datum-positions dataset))))

(defvar *dataset3*
	(make-instance
	  'dataset-output-record
	  :data (make-sample-data 1000 0.0 25.0 #'sin 0.75 #'make-heavy-datum)))

(defun graph3 (&optional (stream *standard-output*))
  ;; About 2x faster than GRAPH2.
  (terpri stream)
  (elapsed-time (display-data *dataset3* stream)))

