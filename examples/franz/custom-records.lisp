;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: custom-records.lisp,v 1.4 1993/07/22 15:38:27 cer Exp $

(in-package :clim-user)


;;; The basic, unoptimized protocol

;; Single data point
(defclass simple-data-point ()
    ((u :initarg :u)			;actual data position,
     (v :initarg :v)			; not the position in the display
     (size :initform 2)))

(declaim (inline make-simple-data-point))
(defun make-simple-data-point (u v)
  (make-instance 'simple-data-point :u u :v v))

(defmethod data-uv-position ((datum simple-data-point))
  (with-slots (u v) datum
    (values u v)))

(defmethod draw-data-point ((datum simple-data-point) x y stream)
  (draw-point* stream x y))

(defmethod display-data-point ((datum simple-data-point) x y stream)
  (with-output-as-presentation (stream datum 'simple-data-point
				:single-box t)
    (draw-data-point datum x y stream)))


;; A set of data points
(defclass dataset ()
    ((data :initform nil :initarg :data :accessor dataset-data)
     (x)
     (y)
     (width :initform 500 :initarg :width :accessor dataset-width)
     (height :initform 60 :initarg :height :accessor dataset-height)
     (ux-scale :initform 20)
     (vy-scale :initform 20)))

(defmethod data-xy-position ((dataset dataset))
  (with-slots (x y) dataset
    (values x y)))

(defmethod set-data-xy-position ((dataset dataset) x y)
  (setf (slot-value dataset 'x) x
	(slot-value dataset 'y) y))

(defmethod dataset-size ((dataset dataset))
  (with-slots (width height) dataset
    (values width height)))

(defmethod set-dataset-size ((dataset dataset) width height)
  (multiple-value-bind (w h) (dataset-size dataset)
    (when (or (/= w width) (/= h height))
      (setf (slot-value dataset 'width) width
	    (slot-value dataset 'height) height)
      ;; Now recompute scale
      (let (minu maxu minv maxv)
	(map-over-data-point-uv-positions
	  #'(lambda (u v datum)
	      (declare (ignore datum))
	      (clim-utils:minf-or minu u)
	      (clim-utils:minf-or minv v)
	      (clim-utils:maxf-or maxu u)
	      (clim-utils:maxf-or maxv v))
	  dataset)
	(with-slots (ux-scale vy-scale) dataset
	  (setq ux-scale (/ (float width) (- maxu minu)))
	  (setq vy-scale (/ (float height) (- maxv minv))))))))

(defgeneric map-over-data-points (function dataset &key start end)
  (declare (dynamic-extent function)))

(defmethod map-over-data-points (function (dataset dataset) &key start end)
  (let ((data (dataset-data dataset)))
    (clim-utils:dovector (element data :start (or start 0) :end (or end (length data)))
      (funcall function element))))

(defgeneric map-over-data-point-uv-positions (function dataset &key start end)
  (declare (dynamic-extent function)))

(defmethod map-over-data-point-uv-positions (function (dataset dataset) &key start end)
  (map-over-data-points
    #'(lambda (datum)
	(multiple-value-bind (u v) (data-uv-position datum)
	  (funcall function u v datum)))
    dataset
    :start start :end end))

(defgeneric map-over-data-point-xy-positions (function dataset &key start end)
  (declare (dynamic-extent function)))

(defmethod map-over-data-point-xy-positions (function (dataset dataset) &key start end)
  ;; This translates from UV to XY coordinates.  This transformation simply
  ;; provides offsets.  In a full implementation, need scaling as well.
  (with-slots (x y ux-scale vy-scale) dataset
    (let ((xoff x)
	  (yoff (+ y (truncate (dataset-height dataset) 2))))
      (map-over-data-point-uv-positions
	#'(lambda (u v datum)
	    (funcall function
		     (+ (truncate (* u ux-scale)) xoff)
		     (+ (truncate (* v vy-scale)) yoff)
		     datum))
	dataset
	:start start :end end))))

(defmethod display-data ((dataset dataset) stream &key (width 500) (height 60))
  (set-dataset-size dataset width height)
  (multiple-value-bind (x y) (stream-cursor-position stream)
    (set-data-xy-position dataset x y))
  (let ((record
	  (with-output-as-presentation (stream dataset 'dataset
					:single-box t)
	    (map-over-data-point-xy-positions
	      #'(lambda (x y datum)
		  (display-data-point datum x y stream))
	      dataset))))
    (clim-internals::move-cursor-beyond-output-record stream record)
    record))


;;; Heavyweight data points implemented as lightweight output records

;; This optimization relieves the implementation from consing an output
;; record for each data point
(defclass data-point (presentation displayed-output-record simple-data-point)
    ((x :initarg :x)			;(X,Y) position relative to parent
     (y :initarg :y)     
     (parent :initform nil :accessor output-record-parent)
     (type :initform 'data-point :reader presentation-type)))

(defmethod initialize-instance :after ((datum data-point) &key object type single-box)
  ;; Eat up the initargs that records are supposed to handle
  (declare (ignore object type single-box)))

(proclaim '(inline make-data-point))
(defun make-data-point (u v)
  (make-instance 'data-point :u u :v v))

(defmethod data-xy-position ((datum data-point))
  (with-slots (x y) datum
    (values x y)))

(defmethod presentation-object ((datum data-point))
  datum)

(defmethod presentation-single-box ((datum data-point)) t)

(defmethod bounding-rectangle* ((datum data-point))
  (with-slots (x y size) datum
    (values (- x size) (- y size)
	    (+ x size) (+ y size))))

(defmethod output-record-start-cursor-position ((datum data-point))
  (with-slots (x y size) datum
    (values (- x size) (- y size))))

(defmethod output-record-set-position ((datum data-point) new-x new-y)
  (with-slots (x y) datum
    (setf x new-x
	  y new-y)))

(defmethod map-over-output-records-overlapping-region
	   (function (datum data-point) region
	    &optional x-offset y-offset &rest continuation-args)
  (declare (ignore function region x-offset y-offset continuation-args))
  nil)

(defmethod map-over-output-records-containing-position
	   (function (datum data-point) x y
	    &optional x-offset y-offset &rest continuation-args)
  (declare (ignore function x y x-offset y-offset continuation-args))
  nil)

(defmethod tree-recompute-extent ((datum data-point))
  nil)

(defmethod replay-output-record ((datum data-point) stream
				 &optional region (x-offset 0) (y-offset 0))
  ;; Region is the region being replayed (might want to clip to it)
  (declare (ignore region))
  (with-slots (x y size) datum
    ;; We use MEDIUM-DRAW-POINT* here because it bypasses all of the
    ;; higher level functions, which is OK during REPLAY
    (let ((medium (sheet-medium stream)))
      (clim-utils:letf-globally (((medium-transformation medium) +identity-transformation+))
	(medium-draw-point* (sheet-medium stream) (+ x x-offset) (+ y y-offset))))))

(defmethod output-record-refined-position-test ((datum data-point) x y)
  (declare (ignore x y))
  t)

(defmethod highlight-output-record ((datum data-point) stream state)
  (declare (ignore state))
  (let ((x (slot-value datum 'x))
	(y (slot-value datum 'y))
	(size (slot-value datum 'size)))
    (multiple-value-bind (xoff yoff)
	(convert-from-relative-to-absolute-coordinates 
	  stream (output-record-parent datum))
      (incf x xoff)
      (incf y yoff))
    (draw-rectangle* stream 
		     (- x size 1) (- y size 1) (+ x size 1) (+ y size 1)
		     :filled nil :ink +flipping-ink+)))

(defmethod display-data-point ((datum data-point) x y stream)
  (when (stream-recording-p stream)
    (multiple-value-bind (abs-x abs-y)
	(point-position (clim-internals::stream-output-history-position stream))
      (output-record-set-position datum (- x abs-x) (- y abs-y)))
    (stream-add-output-record stream datum))	;sets parent
  ;; We're going to call replay at a high level
  (when (stream-drawing-p stream)
    (replay datum stream)))


;;; Output records that hold an entire dataset

;; This optimization provides a new type of output record that is very
;; similar to STANDARD-SEQUENCE-OUTPUT-RECORD
(defclass dataset-output-record 
	  (clim-internals::output-record-mixin 
	   clim-internals::output-record-element-mixin
	   dataset 
	   output-record)
    ())

(defmethod initialize-instance :after ((dataset dataset-output-record) &key)
  (cache-data-point-positions dataset))

(clim-internals::define-output-record-constructor dataset-output-record
						  (&key x-position y-position (size 5))
  :x-position x-position :y-position y-position :size size)

(defmethod output-record-children ((record dataset-output-record))
  (let* ((data (dataset-data record))
	 (result (make-list (length data))))
    (replace result data)
    result))

(defmethod output-record-element ((record dataset-output-record) index)
  (let ((data (dataset-data record)))
    (aref data index)))

(defmethod output-record-count ((record dataset-output-record) &key fastp)
  (declare (ignore fastp))
  (let ((data (dataset-data record)))
    (length data)))

(defmethod clear-output-record ((record dataset-output-record))
  (error "Cannot clear a ~A this way" 'dataset-output-record))

(defmethod add-output-record (child (record dataset-output-record))
  (declare (ignore child))
  ;; This isn't an error because DISPLAY-DATA-POINT wants to do it
  nil)

(defmethod delete-output-record (child (record dataset-output-record) &optional (errorp t))
  (declare (ignore child))
  (when errorp
    (error "Cannot delete a child from a ~A this way" 'dataset-output-record)))

(defmethod map-over-output-records-overlapping-region
	   (function (record dataset-output-record) region 
	    &optional (x-offset 0) (y-offset 0) &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (let ((data (dataset-data record)))
    (if (or (null region) (eq region +everywhere+))
	(map-over-data-points
	  #'(lambda (element)
	      (apply function element continuation-args))
	  record)
	(with-bounding-rectangle* (left top right bottom) region
	  (translate-positions x-offset y-offset left top right bottom)
	  ;; Subtract out the record offset from the region, to make comparison fair
	  (multiple-value-bind (xoff yoff)
	      (output-record-position record)
	    (translate-positions (- xoff) (- yoff) left top right bottom))
	  (map-over-data-point-xy-positions
	    #'(lambda (ex ey element)
		(when (> ex right)
		  (return-from map-over-output-records-overlapping-region nil))
		(when (and (>= ex left)
			   #+++ignore (<= top ey bottom))
		  (apply function element continuation-args)))
	    record
	    :start (bsearch left data :key #'data-xy-position)))))
  nil)

(defmethod map-over-output-records-containing-position
	   (function (record dataset-output-record) x y 
	    &optional (x-offset 0) (y-offset 0) &rest continuation-args)
  (declare (dynamic-extent function continuation-args))
  (let ((data (dataset-data record)))
    (translate-positions x-offset y-offset x y)
    (multiple-value-bind (xoff yoff)
	(output-record-position record)
      (translate-positions (- xoff) (- yoff) x y))
    (map-over-data-points 
      #'(lambda (element)
	  (with-bounding-rectangle* (left top right bottom) element
	    (when (> x right)
	      (return-from map-over-output-records-containing-position nil))
	    (when (clim-utils:ltrb-contains-position-p left top right bottom x y)
	      (apply function element continuation-args))))
      record
      :start (bsearch x data :key #'data-xy-position)))
  nil)

(defun bsearch (number vector 
		&key (key #'identity) (start 0) (end (1- (length vector))))
  ;; If a number is in between two values, return the first of the two
  (declare (values index element))
  (if (< end start)
      (values nil 0)
      (let (#+Genera (vector vector) 
	    (found nil)
	    mid Nmid)
	(declare (type vector vector))
	(loop
	  (when (or found (< end start)) 
	    (return))
	  (setq mid (truncate (+ start end) 2))
	  (setq Nmid (funcall key (aref vector mid)))
	  (cond ((< number Nmid)
		 (if (= start mid)
		     (setq found (max 0 (1- mid)))
		     (setq end (1- mid))))
		((> number Nmid)
		 (if (= end mid)
		     (setq found mid)
		     (setq start (1+ mid))))
		(t				;(= number Nmid)
		 (setq found mid))))		;found it!
	(when found (values mid (aref vector found))))))

(defmethod display-data ((dataset dataset-output-record) stream &key (width 500) (height 60))
  (set-dataset-size dataset width height)
  (multiple-value-bind (x y) (stream-cursor-position stream)
    (set-data-xy-position dataset x y))
  (let ((record 
	  (with-output-as-presentation (stream dataset 'dataset
					:single-box t)
	    (multiple-value-bind (abs-x abs-y)
		(point-position
		  (clim-internals::stream-output-history-position stream))
	      (multiple-value-bind (x y) (stream-cursor-position stream)
		(output-record-set-position dataset (- x abs-x) (- y abs-y))))
	    (stream-add-output-record stream dataset))))
    (replay dataset stream)
    (clim-internals::move-cursor-beyond-output-record stream record)
    record))

(defmethod cache-data-point-positions ((dataset dataset-output-record))
  ;; This needs to run every time the translation from UV to XY
  ;; coordinates changes.  It does not need to run when the position of
  ;; the output record is changed, because XY coordinates of datum is
  ;; relative to XY position of parent.
  (set-data-xy-position dataset 0 0)
  (let (left top right bottom)
    (map-over-data-point-xy-positions 
      #'(lambda (x y datum)
	  (output-record-set-position datum x y)
	  (with-bounding-rectangle* (ll tt rr bb) datum
	    (clim-utils:minf-or left ll)
	    (clim-utils:minf-or top  tt)
	    (clim-utils:maxf-or right  rr)
	    (clim-utils:maxf-or bottom bb)
	    (setf (output-record-parent datum) dataset)))
      dataset)
    (clim-utils:bounding-rectangle-set-edges dataset left top right bottom)))

(defmethod set-dataset-size :around ((dataset dataset-output-record) new-width new-height)
  (multiple-value-bind (w h) (dataset-size dataset)
    (call-next-method dataset new-width new-height)
    (when (or (/= w new-width) (/= h new-height))
      (cache-data-point-positions dataset))))


;;; Now the application

(defmacro elapsed-time (&body body)
  (let ((start '#:start))
    `(clim-sys:without-scheduling
       (let ((,start (get-internal-run-time)))
	 (values
	   (progn ,@body)
	   (/ (float (- (get-internal-run-time) ,start))
	      internal-time-units-per-second))))))

(defun display-sample-dataset (dataset &optional (stream *standard-output*))
  (declare (values output-record elapsed-time))
  (terpri stream)
  (elapsed-time (display-data dataset stream)))

(defun make-sample-data (number-of-samples u0 u1 generator noise-level constructor)
  (let ((data (make-array number-of-samples))
	(dx (/ (- u1 u0) number-of-samples)))
    (dotimes (i number-of-samples)
      (let* ((x (+ u0 (* i dx)))
	     (y (+ (funcall generator x)
		   (* noise-level (- (random 1.0) 0.5)))))
	(setf (aref data i) (funcall constructor x y))))
    data))

(defvar *sine-wave-dataset-1*
	(make-instance
	  'dataset
	  :data (make-sample-data 1000 0.0 25.0 #'sin 0.75 #'make-simple-data-point)))

(defvar *sine-wave-dataset-2*
	(make-instance
	  'dataset
	  :data (make-sample-data 1000 0.0 25.0 #'sin 0.75 #'make-data-point)))

(defvar *sine-wave-dataset-3*
	(make-instance
	  'dataset-output-record
	  :data (make-sample-data 1000 0.0 25.0 #'sin 0.75 #'make-data-point)))


(define-command-table scigraph-examples)

(define-application-frame scigraph () ()
  (:command-table (scigraph :inherit-from (scigraph-examples)
			    :menu (("Examples" :menu scigraph-examples))))
  (:panes
    (caption :application
	     :text-style '(:sans-serif :roman :normal)
	     :scroll-bars nil)
    (display :application
	     :scroll-bars :vertical
	     :text-cursor nil))
  (:layouts
    (default
      (vertically () (1/5 caption) (4/5 display)))))

(defmethod frame-standard-output ((frame scigraph))
  (get-frame-pane *application-frame* 'display))

(define-scigraph-command (com-exit-scigraph :menu "Exit") ()
  (frame-exit *application-frame*))

(define-scigraph-command (com-clear-display :menu "Clear") ()
  (window-clear (get-frame-pane *application-frame* 'caption))
  (window-clear (get-frame-pane *application-frame* 'display)))

(defun display-caption (&rest strings)
  (declare (dynamic-extent strings))
  (let* ((stream (get-frame-pane *application-frame* 'caption))
	 (width (min (floor (window-inside-width stream) 
			    (stream-character-width stream #\0))
		     72)))
    (window-clear stream)
    (filling-output (stream :fill-width `(,width :character))
      (dolist (string strings)
	(write-string string stream)))))

(define-command (com-example-1 :command-table scigraph-examples
			       :menu "Example 1") ()
  (display-caption 
    "The default way of doing output."
    "Each data point in the output has a corresponding CLIM point output record.")
  (let ((stream *standard-output*))
    (with-text-face (stream :italic)
      (format stream "~&Normal way of doing output:~%"))
    (multiple-value-bind (record time)
	(display-sample-dataset *sine-wave-dataset-1* stream)
      (format (get-frame-pane *application-frame* 'caption)
	  "~%Elapsed time was ~2$ seconds." time)
      (clim-internals::move-cursor-beyond-output-record stream record)
      (terpri stream))))

(define-command (com-example-2 :command-table scigraph-examples
			       :menu "Example 2") ()
  (display-caption 
    "A faster way of doing output. "
    "Each data point in the output serves as the displayed output record as well.")
  (let ((stream *standard-output*))
    (with-text-face (stream :italic)
      (format stream "~&Faster way of doing output:~%"))
    (multiple-value-bind (record time)
	(display-sample-dataset *sine-wave-dataset-2* stream)
      (format (get-frame-pane *application-frame* 'caption)
	  "~%Elapsed time was ~2$ seconds." time)
      (clim-internals::move-cursor-beyond-output-record stream record)
      (terpri stream))))

(define-command (com-example-3 :command-table scigraph-examples
			       :menu "Example 3") ()
  (display-caption 
    "The fastest way of doing output. "
    "Each data point in the output serves as the displayed output record,"
    "and there is a custom output record representing the entire data set as well.")
  (let ((stream *standard-output*))
    (with-text-face (stream :italic)
      (format stream "~&Fastest way of doing output:~%"))
    (multiple-value-bind (record time)
	(display-sample-dataset *sine-wave-dataset-3* stream)
      (format (get-frame-pane *application-frame* 'caption)
	  "~%Elapsed time was ~2$ seconds." time)
      (clim-internals::move-cursor-beyond-output-record stream record)
      (terpri stream))))



(clim-demo:define-demo "Custom Output Records" scigraph
  :left 200 :top 100 :width 600 :height 500)
