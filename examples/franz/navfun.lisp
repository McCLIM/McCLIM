;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-

;; $fiHeader: navfun.lisp,v 1.27 1993/09/17 00:20:42 colin Exp $

(in-package :clim-demo)

"Copyright (c) 1989, 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;;; Database support

(defvar *print-object-readably* nil)

(defvar *position-list*      nil "The position database")
(defvar *route-list*         nil "The route database")
(defvar *victor-airway-list* nil "The victor-airway database")
(defvar *aircraft-list*      nil "The aircraft database")

(defvar *max-latitude*  44)
(defvar *min-latitude*  41)
(defvar *max-longitude* 74)
(defvar *min-longitude* 70)

(defvar *magnifier* #-Cloe-Runtime (float 150)
		    #+Cloe-Runtime (float 100))

;; Given longitude,latitude return X,Y
(defun scale-coordinates (longitude latitude)
  (values (* *magnifier* (- *max-longitude* longitude))
	  (* *magnifier* (- *max-latitude* latitude))))

;; Given X,Y return longitude,latitude
(defun unscale-coordinates (x y window)
  (declare (ignore window))
  (values (- *max-longitude* (/ x *magnifier*))
	  (- *max-latitude* (/ y *magnifier*))))

(defmacro rounding-coordinates ((&rest coordinates) &body body)
  (let ((coords nil))
    (dolist (coord coordinates)
      (push `(,coord (round ,coord)) coords))
    `(let (,@(nreverse coords)) ,@body)))

(defvar *label-text-style* #+Genera '(:fix :roman :small)
			   #-Genera '(:fix :roman :normal))


;;; Basic data structures - points and positions

#-allegro
(defclass fp-point ()
    ((latitude :initarg :latitude
	       :accessor point-latitude)
     (longitude :initarg :longitude
		:accessor point-longitude)))

#-allegro
(defclass ground-position (fp-point)
    ((altitude :initarg :altitude
	       :accessor position-altitude)
     (deviation :initarg :deviation
		:accessor position-deviation)))

#+allegro
(eval-when (compile load eval)
  (defclass fp-point ()
      ((latitude :initarg :latitude
		 :accessor point-latitude)
       (longitude :initarg :longitude
		  :accessor point-longitude)))
  (defclass ground-position (fp-point)
      ((altitude :initarg :altitude
		 :accessor position-altitude)
       (deviation :initarg :deviation
		  :accessor position-deviation))))

(defmethod draw-position ((position ground-position) stream &optional label)
  (with-slots (longitude latitude) position
    (multiple-value-bind (x y) (scale-coordinates longitude latitude)
      (let* ((xx (+ x (/ 3 (tan (radian 30))))))
	(rounding-coordinates (xx y)
	  (draw-circle* stream xx y 2)
	  (draw-label label stream (+ xx 5) y))))))

;; Assumes X and Y are already rounded...
(defun draw-label (label stream x y)
  (when label
    (draw-text* stream label (+ x 5) y
		:text-style *label-text-style*)))

(defun distance (from-position to-position)
  (values (geodesic (point-latitude from-position) (point-longitude from-position)
		    (point-latitude to-position) (point-longitude to-position))))

(defun azimuth (from-position to-position)
  (multiple-value-bind (dist azim) 
      (geodesic (point-latitude from-position) (point-longitude from-position) 
                (point-latitude to-position) (point-longitude to-position))
    (declare (ignore dist))
    azim))

#-allegro
(defclass named-position (ground-position)
    ((name :initarg :name
	   :accessor position-name)
     (longname :initarg :longname
	       :accessor position-longname)))

#+allegro
(eval-when (compile load eval)
  (defclass named-position (ground-position)
      ((name :initarg :name
	     :accessor position-name)
       (longname :initarg :longname
		 :accessor position-longname))))

(defmethod describe-position-object ((position named-position) stream)
  (with-slots (name longname) position
    (format stream "~:(~A~) ~A ~A"
      (class-name (class-of position)) name longname)))

(defmethod print-object ((position named-position) stream)
  (if (or *print-object-readably* (not *print-escape*))
      (write (position-name position) :stream stream :escape nil)
    (print-unreadable-object (position stream :type t :identity t)
      (write (position-name position) :stream stream :escape nil))))


(eval-when (compile load eval)

(defun degminsec (degrees &optional (minutes 0) (seconds 0))
  (float (/ (+ (* degrees 3600) (* minutes 60) seconds) 3600)))

(defun getdegminsec (angle-in-degrees)
  (let* ((angle (* 3600 angle-in-degrees))
	 (seconds (rem angle 60))
	 (minutes (rem (/ angle 60) 60))
	 (degrees (/ angle 3600)))
    (values (floor degrees) (floor minutes) (floor seconds))))

(defun coast-segment (latitudes longitudes)
  (let* ((result (make-list (* 2 (length latitudes))))
	 (rpos result))
    (loop
      (when (null rpos) (return result))
      (setf (car rpos) (pop longitudes))
      (setf rpos (cdr rpos))
      (setf (car rpos) (pop latitudes))
      (setf rpos (cdr rpos)))))

)	;eval-when

(defvar *coastline* 
      (list
	(coast-segment				; Boston coastline
	  (list (degminsec 40 48) (degminsec 41 10) (degminsec 41 15) (degminsec 41 20)
		(degminsec 41 25) 
		(degminsec 41 30) (degminsec 41 30) (degminsec 41 35) (degminsec 41 40) 
		(degminsec 41 45) (degminsec 41 45) (degminsec 41 40) (degminsec 41 35) 
		(degminsec 41 30) (degminsec 41 35) (degminsec 41 40) (degminsec 41 45) 
		(degminsec 41 50) (degminsec 41 55) (degminsec 42 00) (degminsec 42 05) 
		(degminsec 42 05) (degminsec 42 00) (degminsec 41 55) (degminsec 41 50)
		(degminsec 41 45) (degminsec 41 45) (degminsec 41 50) (degminsec 41 55)
		(degminsec 42 00) (degminsec 42 05) (degminsec 42 10) (degminsec 42 15) 
		(degminsec 42 20) (degminsec 42 25) (degminsec 42 30) (degminsec 42 35) 
		(degminsec 42 40) (degminsec 42 45) (degminsec 42 50) (degminsec 42 55) 
		(degminsec 43 00) (degminsec 43 05) (degminsec 43 10) (degminsec 43 15) 
		(degminsec 43 20) (degminsec 43 25) (degminsec 43 30) (degminsec 43 35) 
		(degminsec 43 40) (degminsec 43 45) (degminsec 43 50) (degminsec 43 55) 
		(degminsec 43 60))
	  (list (degminsec 73 47) (degminsec 73 07) (degminsec 72 58) (degminsec 71 45)
		(degminsec 71 30)
		(degminsec 71 25) (degminsec 71 02) (degminsec 70 57) (degminsec 70 46)
		(degminsec 70 42) (degminsec 70 39) (degminsec 70 39) (degminsec 70 39)
		(degminsec 70 40) (degminsec 70 28) (degminsec 69 57) (degminsec 69 56)
		(degminsec 69 56) (degminsec 69 58) (degminsec 70 01) (degminsec 70 13)
		(degminsec 70 14) (degminsec 70 05) (degminsec 70 05) (degminsec 70 00)
		(degminsec 70 10) (degminsec 70 24) (degminsec 70 32) (degminsec 70 33)
		(degminsec 70 42) (degminsec 70 39) (degminsec 70 42) (degminsec 70 46)
		(degminsec 71 00) (degminsec 71 00) (degminsec 70 50) (degminsec 70 41)
		(degminsec 70 37) (degminsec 70 48) (degminsec 70 49) (degminsec 70 48)
		(degminsec 70 45) (degminsec 70 40) (degminsec 70 47) (degminsec 70 46)
		(degminsec 70 42) (degminsec 70 23) (degminsec 70 23) (degminsec 70 13)
		(degminsec 70 14) (degminsec 70 12) (degminsec 70 00) (degminsec 69 27)
		(degminsec 69 08)))
	(coast-segment				; Martha's vinyard
	  (list 
	    (degminsec 41 29) (degminsec 41 28) (degminsec 41 25) (degminsec 41 24) 
	    (degminsec 41 23) (degminsec 41 24) (degminsec 41 21) (degminsec 41 21) 
	    (degminsec 41 20) (degminsec 41 18) (degminsec 41 22) (degminsec 41 22) 
	    (degminsec 41 25) (degminsec 41 27) (degminsec 41 29))
	  (list
	    (degminsec 70 36) (degminsec 70 34) (degminsec 70 33) (degminsec 70 31)
	    (degminsec 70 30) (degminsec 70 28) (degminsec 70 27) (degminsec 70 44)
	    (degminsec 70 45) (degminsec 70 46) (degminsec 70 50) (degminsec 70 45)
	    (degminsec 70 44) (degminsec 70 41) (degminsec 70 36)))
	(coast-segment				; Nantucket
	  (list
	    (degminsec 41 24) (degminsec 41 20) (degminsec 41 15) (degminsec 41 14) 
	    (degminsec 41 14) (degminsec 41 16) (degminsec 41 18) (degminsec 41 18) 
	    (degminsec 41 19) (degminsec 41 24))
	  (list
	    (degminsec 70 03) (degminsec 70 00) (degminsec 69 57) (degminsec 70 00)
	    (degminsec 70 07) (degminsec 70 12) (degminsec 70 11) (degminsec 70 03)
	    (degminsec 70 01) (degminsec 70 03)))
	(coast-segment				; Block Island
	  (list
	    (degminsec 41 14) (degminsec 41 13) (degminsec 41 11) (degminsec 41 09) 
	    (degminsec 41 08) (degminsec 41 09) (degminsec 41 12) (degminsec 41 14))
	  (list
	    (degminsec 71 34) (degminsec 71 33) (degminsec 71 34) (degminsec 71 32)
	    (degminsec 71 36) (degminsec 71 37) (degminsec 71 35) (degminsec 71 34)))))

(defun draw-coastline (coastline &optional (stream *standard-output*))
  (with-scaling (stream (- *magnifier*))
    (with-translation (stream (- *max-longitude*) (- *max-latitude*))
      (dolist (coast coastline)
	(draw-polygon* stream coast :filled nil :closed nil :line-thickness 2)))))

(defun redraw-display ()
  (draw-coastline *coastline*)
  (flet ((present-position (object)
	   (present object (class-name (class-of object))
		    :view +iconic-view+ :single-box t)))
    (mapc #'present-position *position-list*))
  (flet ((present-route (object)
	   (present object (class-name (class-of object))
		    :view +iconic-view+ :single-box nil)))
    (mapc #'present-route *route-list*)
    (mapc #'present-route *victor-airway-list*)))


;;; Concrete position objects

#-allegro
(defclass airport (named-position) ())

#+allegro
(eval-when (compile load eval)
  (defclass airport (named-position) ()))

(defmethod draw-position ((airport airport) stream &optional label)
  (with-slots (longitude latitude) airport
    (multiple-value-bind (x y) (scale-coordinates longitude latitude)
      (rounding-coordinates (x y)
	(let ((color-args (and (color-stream-p stream)
			       (list :ink +green+))))
	  (apply #'draw-circle* stream x y 5 color-args))
	(draw-line* stream x (- y 2) x (+ y 2) :ink +background-ink+ :line-thickness 2)
	(draw-label label stream (+ x 5) y)))))

#-allegro
(defclass waypoint (named-position) ())

#-allegro
(defclass vor (named-position) ())

#+allegro
(eval-when (compile load eval)
  (defclass waypoint (named-position) ())
  (defclass vor (named-position) ()))

(defmethod describe-position-object ((vor vor) stream)
  (with-slots (name longname) vor
    (format stream "VOR ~A ~A" name longname)))

(defmethod draw-position ((vor vor) stream &optional label)
  (with-slots (longitude latitude) vor
    (multiple-value-bind (x y) (scale-coordinates longitude latitude)
      (let ((xx (+ x (/ 3 (tan (radian 30)))))
	    (color-args (and (color-stream-p stream)
			     (list :ink +cyan+))))
	(apply #'draw-hexagon (+ xx 5) (- y 3) (+ xx 5) (+ y 3) stream color-args)
	(rounding-coordinates (xx y)
	  (apply #'draw-circle* stream xx y 2  color-args)
	  (draw-label label stream (+ xx 5) y))))))

(defun draw-hexagon (x1 y1 x2 y2 stream &rest color-args)
  (declare (dynamic-extent color-args))
  (let* ((n 6)
	 (theta (* pi (1- (/ 2.0 n))))
	 (sin-theta (sin theta))
	 (cos-theta (cos theta)))
    (do ((i 1 (1+ i))
	 (x3) (y3))
	((not (<= i n)))
      (setq x3 (+ (- (- (* x1 cos-theta)
			(* y1 sin-theta))
		     (* x2 (1- cos-theta)))
		  (* y2 sin-theta))
	    y3 (- (- (+ (* x1 sin-theta)
			(* y1 cos-theta))
		     (* x2 sin-theta))
		  (* y2 (1- cos-theta))))
      (rounding-coordinates (x1 y1 x2 y2)
	(apply #'draw-line* stream x1 y1 x2 y2 color-args))
      (setq x1 x2 y1 y2 x2 x3 y2 y3))))

#-allegro
(defclass ndb (named-position) ())

#+allegro
(eval-when (compile load eval)
  (defclass ndb (named-position) ()))

(defmethod describe-position-object ((ndb ndb) stream)
  (with-slots (name longname) ndb
    (format stream "NDB ~A ~A" name longname)))

#-allegro
(defclass named-intersection (named-position) ())

#+allegro
(eval-when (compile load eval)
  (defclass named-intersection (named-position) ()))

(defmethod draw-position ((intersection named-intersection) stream &optional label)
  (with-slots (longitude latitude) intersection
    (multiple-value-bind (x y) (scale-coordinates longitude latitude)
      (rounding-coordinates (x y)
	(let ((color-args (and (color-stream-p stream)
			       (list :ink +magenta+))))
	  (apply #'draw-triangle* stream x (- y 3) (- x 3) (+ y 2) (+ x 3) (+ y 2)
		 color-args))
	(draw-label label stream (+ x 5) y)))))

#-allegro
(defclass visual-checkpoint (named-position) ())

#+allegro
(eval-when (compile load eval)
  (defclass visual-checkpoint (named-position) ()))


;; User interfaces to concrete position objects

(defun concrete-position-parser (type stream)
  (let ((object (completing-from-suggestions (stream)
		  (dolist (position *position-list*)
		    (when (typep position type)
		      (suggest (position-name position) position))))))
    object))


(define-presentation-type ground-position ())

(define-presentation-method present (object (type ground-position) stream (view iconic-view) &key)
  (draw-position object stream))


(define-presentation-type named-position ())

(define-presentation-method present (object (type named-position) stream view &key acceptably)
  (declare (ignore view))
  (let ((*print-object-readably* acceptably))
    (format stream "~A" (position-name object))))

(define-presentation-method present (object (type named-position) stream (view iconic-view)
				     &key)
  (draw-position object stream
		 (and (typep object 'named-position)
		      (position-name object))))

(define-presentation-method accept ((type named-position) stream view &key)
  (declare (ignore view))
  (with-presentation-type-decoded (name) type
    (concrete-position-parser name stream)))


(define-presentation-type airport ())

(define-presentation-type vor ())

(define-presentation-type ndb ())

(define-presentation-type named-intersection ())

(define-presentation-type visual-checkpoint ())


;;; Route objects

#-allegro
(defclass basic-route-segment ()
    ((at :initarg :at
	 :accessor route-segment-at)))

#-allegro
(defclass route-segment (basic-route-segment)
    ((altitude :initarg :altitude :accessor route-segment-altitude)
     (wind-info :initarg :wind-info :accessor route-segment-wind-info)))

#-allegro
(defclass basic-route ()
    ((name :initarg :name :accessor route-name)
     (legs :initarg :legs :accessor route-legs)))

#+allegro
(eval-when (compile load eval)
  (defclass basic-route-segment ()
      ((at :initarg :at
	   :accessor route-segment-at)))
  (defclass route-segment (basic-route-segment)
      ((altitude :initarg :altitude :accessor route-segment-altitude)
       (wind-info :initarg :wind-info :accessor route-segment-wind-info)))
  (defclass basic-route ()
      ((name :initarg :name :accessor route-name)
       (legs :initarg :legs :accessor route-legs))))

(defmethod print-object ((route basic-route) stream)
  (if (or *print-object-readably* (not *print-escape*))
      (write (route-name route) :stream stream :escape t)
    (print-unreadable-object (route stream :type t :identity t)
      (write (route-name route) :stream stream :escape nil))))

(defmethod describe-position-object ((route basic-route) stream)
  (with-slots (name legs) route
    (format stream "Route ~A ~A" name legs)))

(defmethod draw-route ((route basic-route) stream &rest drawing-args)
  (declare (dynamic-extent drawing-args))
  (with-slots (legs) route
    (let* ((start-pos (first legs))
	   (start-lat (route-segment-latitude start-pos))
	   (start-lon (route-segment-longitude start-pos)))
      (do* ((next-legs (cdr legs) (cdr next-legs))
	    next-pos next-lat next-lon)
	   ((null next-legs) nil)
	(setq next-pos (car next-legs)
	      next-lat (route-segment-latitude next-pos)
	      next-lon (route-segment-longitude next-pos))
	(multiple-value-bind (xfrom yfrom) (scale-coordinates start-lon start-lat)
	  (multiple-value-bind (xto yto) (scale-coordinates next-lon next-lat)
	    (rounding-coordinates (xfrom yfrom xto yto)
	      (apply #'draw-line* stream xfrom yfrom xto yto drawing-args))))
	(setq start-lat next-lat start-lon next-lon)))))

#-allegro
(defclass route (basic-route) ())

#+allegro
(eval-when (compile load eval)
  (defclass route (basic-route) ()))

(defun route-segment-position-name (route-segment)
  (position-name (route-segment-at route-segment)))

(defun route-segment-position-longname (route-segment)
  (position-longname (route-segment-at route-segment)))

(defun route-segment-leg-name (route-segment)
  (concatenate 'string "-" (route-segment-position-name route-segment)))

(defun generate-route-name-from-legs (leg-list)
  (apply #'concatenate 'string 
	 (route-segment-position-name (car leg-list))
	 (mapcar #'route-segment-leg-name (cdr leg-list))))

(defun route-segment-latitude (route-segment)
  (point-latitude (route-segment-at route-segment)))

(defun route-segment-longitude (route-segment)
  (point-longitude (route-segment-at route-segment)))


;;; Victor Airways

#-allegro
(defclass victor-airway-segment (basic-route-segment)
    ((properties :accessor victor-airway-segment-properties)
     (next-leg :accessor victor-airway-segment-next-leg)))

#-allegro
(defclass victor-airway (basic-route) ())

#+allegro
(eval-when (compile load eval)
  (defclass victor-airway-segment (basic-route-segment)
      ((properties :accessor victor-airway-segment-properties)
       (next-leg :accessor victor-airway-segment-next-leg)))
  (defclass victor-airway (basic-route) ()))

(defun route-parser (type list stream)
  (let ((object (completing-from-suggestions (stream)
		  (dolist (aroute list)
		    (when (typep aroute type)
		      (suggest (route-name aroute) aroute))))))
    object))


(define-presentation-type route ())

(define-presentation-method present (object (type route) stream view &key acceptably)
  (declare (ignore view))
  (let ((*print-object-readably* acceptably))
    (format stream "~A" (route-name object))))

(define-presentation-method present (object (type route) stream (view iconic-view) &key)
  (let ((drawing-args (if (color-stream-p stream)
			  (list :ink +red+)
			  '(:line-dashes t))))
    (apply #'draw-route object stream drawing-args)))

(define-presentation-method accept ((type route) stream view &key)
  (declare (ignore view))
  (route-parser 'route *route-list* stream))

(define-presentation-method highlight-presentation ((type route) record stream state)
  (highlight-route (presentation-object record) stream state))

(defvar *route-highlight-style* (make-line-style :thickness 2))

(defun highlight-route (route stream state)
  (declare (ignore state))
  (with-drawing-options (stream :line-style *route-highlight-style*)
    (draw-route route stream :ink +flipping-ink+)))


(define-presentation-type victor-airway ())

(define-presentation-method present (object (type victor-airway) stream view &key acceptably)
  (declare (ignore view))
  (let ((*print-object-readably* acceptably))
    (format stream "~A" (route-name object))))

(define-presentation-method present (object (type victor-airway) stream (view iconic-view)
				     &key)
  (let ((drawing-args (if (color-stream-p stream)
			  (list :ink +blue+)
			  '(:line-dashes nil))))
    (apply #'draw-route object stream drawing-args)))

(define-presentation-method accept ((type victor-airway) stream view &key)
  (declare (ignore view))
  (route-parser 'route *victor-airway-list* stream))

(define-presentation-method highlight-presentation ((type victor-airway) record stream state)
  (highlight-route (presentation-object record) stream state))


;;; Preferred Routes

;;; ADIZ

;;; Airspace

;;; TCA
;;; ARSA
;;; Warning
;;; Restricted
;;; Prohibited
;;; MOA


;;; Aircraft description

#-allegro
(defclass aircraft ()
    ((identification :initarg :identification	; Aircraft tail number
		     :accessor aircraft-identification)
     (type :initarg :type			; eg C-172
	   :accessor aircraft-type)
     (taxi-fuel :initarg :taxi-fuel		; fuel used for taxi&runup (estimate)
		:accessor aircraft-taxi-fuel)
     (preferred-cruising-altitude :initarg :preferred-cruising-altitude
				  :accessor aircraft-preferred-cruising-altitude)
     (normal-cruise-speed :initarg :normal-cruise-speed
			  :accessor aircraft-normal-cruise-speed)
     (fuel-consumption-at-normal-cruise :initarg :fuel-consumption-at-normal-cruise
					:accessor aircraft-fuel-consumption-at-normal-cruise)
     (maximum-usable-fuel :initarg :maximum-usable-fuel
			  :accessor aircraft-maximum-usable-fuel)
     (cost-per-hour :initarg :cost-per-hour
		    :accessor aircraft-cost-per-hour)
     (hobs-or-tach :initarg :hobs-or-tach
		   :accessor aircraft-hobs-or-tach)))

#+allegro
(eval-when (compile load eval)
  (defclass aircraft ()
      ((identification :initarg :identification	; Aircraft tail number
		       :accessor aircraft-identification)
       (type :initarg :type :accessor aircraft-type)
       (taxi-fuel :initarg :taxi-fuel :accessor aircraft-taxi-fuel)
       (preferred-cruising-altitude :initarg :preferred-cruising-altitude
				    :accessor aircraft-preferred-cruising-altitude)
       (normal-cruise-speed :initarg :normal-cruise-speed
			    :accessor aircraft-normal-cruise-speed)
       (fuel-consumption-at-normal-cruise :initarg :fuel-consumption-at-normal-cruise
					  :accessor aircraft-fuel-consumption-at-normal-cruise)
       (maximum-usable-fuel :initarg :maximum-usable-fuel
			    :accessor aircraft-maximum-usable-fuel)
       (cost-per-hour :initarg :cost-per-hour
		      :accessor aircraft-cost-per-hour)
       (hobs-or-tach :initarg :hobs-or-tach
		     :accessor aircraft-hobs-or-tach))))

(define-presentation-type aircraft ())

(define-presentation-method present (object (type aircraft) stream view &key acceptably)
  (declare (ignore view))
  (let ((*print-object-readably* acceptably))
    (format stream "~A" (aircraft-identification object))))

(define-presentation-method accept ((type aircraft) stream view &key)
  (declare (ignore view))
  (let ((ac (completing-from-suggestions (stream)
	      (dolist (aircraft *aircraft-list*)
		(suggest 
		  (aircraft-identification aircraft) aircraft)))))
    ac))

(defvar *last-plane* nil "The last plane referred to")

(defun edit-aircraft (aircraft)
  (let ((stream (frame-standard-output *application-frame*))
	(identification (aircraft-identification aircraft))
	(type (aircraft-type aircraft))
	(preferred-altitude (aircraft-preferred-cruising-altitude aircraft))
	(cruise-speed (aircraft-normal-cruise-speed aircraft))
	(fuel-consumption (aircraft-fuel-consumption-at-normal-cruise aircraft))
	(maximum-usable-fuel (aircraft-maximum-usable-fuel aircraft))
	(cost-per-hour (aircraft-cost-per-hour aircraft)))
    (accepting-values (stream :own-window t)
      (setq identification (accept 'string :prompt "Identification" 
				   :default identification :stream stream))
      (terpri stream)
      (setq type (accept 'string :prompt "Type" 
			 :default type :stream stream))
      (terpri stream)
      (setq preferred-altitude (accept 'integer :prompt "Preferred cruising altitude" 
				       :default preferred-altitude :stream stream))
      (terpri stream)
      (setq cruise-speed (accept 'integer :prompt "Normal cruise speed" 
				 :default cruise-speed :stream stream))
      (terpri stream)
      (setq fuel-consumption (accept 'float :prompt "Fuel consumption at normal cruise" 
				     :default fuel-consumption :stream stream))
      (terpri stream)
      (setq maximum-usable-fuel (accept 'float :prompt "Maximum usable fuel" 
					:default maximum-usable-fuel :stream stream))
      (terpri stream)
      (accept 'float :prompt "Cost per hour" 
	      :default cost-per-hour :stream stream)
      (terpri stream))
    (setf (aircraft-identification aircraft) identification)
    (setf (aircraft-type aircraft) type)
    (setf (aircraft-taxi-fuel aircraft) 0)
    (setf (aircraft-preferred-cruising-altitude aircraft) preferred-altitude)
    (setf (aircraft-normal-cruise-speed aircraft) cruise-speed)
    (setf (aircraft-fuel-consumption-at-normal-cruise aircraft) fuel-consumption)
    (setf (aircraft-maximum-usable-fuel aircraft) maximum-usable-fuel)
    (setf (aircraft-cost-per-hour aircraft) cost-per-hour)))


;;; Flight plans

#-allegro
(defclass flight-plan ()
    ((type :initarg :type
	   :accessor flight-plan-type)
     (aircraft-id :initarg :aircraft-id
		  :accessor flight-plan-aircraft-id)
     (aircraft-type :initarg :aircraft-type
		    :accessor flight-plan-aircraft-type)
     (true-speed :initarg :true-speed
		 :accessor flight-plan-true-speed)
     (departure-point :initarg :departure-point
		      :accessor flight-plan-departure-point)
     (departure-time :initarg :departure-time
		     :accessor flight-plan-departure-time)
     (cruising-alt :initarg :cruising-alt
		   :accessor flight-plan-cruising-alt)
     (route :initarg :route
	    :accessor flight-plan-route)
     (destination :initarg :destination
		  :accessor flight-plan-destination)
     (ete :initarg :ete
	  :accessor flight-plan-ete)
     (remarks :initarg :remarks
	      :accessor flight-plan-remarks)
     (fuel-on-board :initarg :fuel-on-board
		    :accessor flight-plan-fuel-on-board)
     (alternate :initarg :alternate
		:accessor flight-plan-alternate)
     (pilot :initarg :pilot
	    :accessor flight-plan-pilot)
     (souls :initarg :souls
	    :accessor flight-plan-souls)
     (color :initarg :color
	    :accessor flight-plan-color)))
#+allegro
(eval-when (compile load eval)
  (defclass flight-plan ()
      ((type :initarg :type :accessor flight-plan-type)
       (aircraft-id :initarg :aircraft-id :accessor flight-plan-aircraft-id)
       (aircraft-type :initarg :aircraft-type :accessor flight-plan-aircraft-type)
       (true-speed :initarg :true-speed :accessor flight-plan-true-speed)
       (departure-point :initarg :departure-point :accessor flight-plan-departure-point)
       (departure-time :initarg :departure-time :accessor flight-plan-departure-time)
       (cruising-alt :initarg :cruising-alt :accessor flight-plan-cruising-alt)
       (route :initarg :route :accessor flight-plan-route)
       (destination :initarg :destination :accessor flight-plan-destination)
       (ete :initarg :ete :accessor flight-plan-ete)
       (remarks :initarg :remarks :accessor flight-plan-remarks)
       (fuel-on-board :initarg :fuel-on-board :accessor flight-plan-fuel-on-board)
       (alternate :initarg :alternate :accessor flight-plan-alternate)
       (pilot :initarg :pilot :accessor flight-plan-pilot)
       (souls :initarg :souls :accessor flight-plan-souls)
       (color :initarg :color :accessor flight-plan-color))))

(defun compute-flight-plan (fp-stream plan)
  (let* ((route (flight-plan-route plan))
	 (plane (flight-plan-aircraft-id plan))
	 (leg-list (route-legs route))
	 (start (first leg-list))
	 (end   (car (last leg-list))))
    (progn		;surrounding-output-with-border (fp-stream)
      (multiple-value-bind (distance true-course)
	  (geodesic (route-segment-latitude start)
		    (route-segment-longitude start)
		    (route-segment-latitude end)
		    (route-segment-longitude end))
	(declare (ignore true-course))
	(format fp-stream 
	    "~&Flight Plan from ~A and ~A:~%The great circle distance is ~3,1F NM.~%"
	  (route-segment-position-name start) (route-segment-position-name end)
	  distance))
      (format fp-stream  "~&Route: [ ")
      (dolist (waypoint leg-list)
	(format fp-stream "~A " (route-segment-position-name waypoint)))
      (format fp-stream "].~%")
      (format fp-stream  "~&Plane: ~A ~A.~%"
	(aircraft-identification plane) (aircraft-type plane))
      (let ((total-distance 0)
	    (total-time-enroute 0))
	(do* ((currently-at start)
	      (route-to (cdr leg-list) (cdr route-to)))
	     ((null route-to) nil)
	  (multiple-value-bind (leg-distance leg-true-course)
	      (geodesic (route-segment-latitude currently-at)
			(route-segment-longitude currently-at)
			(route-segment-latitude (car route-to))
			(route-segment-longitude (car route-to)))
	    (declare (ignore leg-true-course))
	    (setq total-distance (+ total-distance leg-distance))
	    (setq currently-at  (car route-to))))
	(format fp-stream "~%")
	(formatting-table (fp-stream)
	  (surrounding-output-with-border (fp-stream :shape :underline)
	    (formatting-row (fp-stream)
	      (with-text-face (fp-stream :italic)
		(formatting-cell (fp-stream) (write-string "CHECKPOINT" fp-stream))
		(formatting-cell (fp-stream :align-x :right) (write-string "ID" fp-stream))
		(formatting-cell (fp-stream :align-x :right) (write-string "TC" fp-stream))
		(formatting-cell (fp-stream :align-x :right) (write-string "Leg" fp-stream))
		(formatting-cell (fp-stream :align-x :right) (write-string "Rem" fp-stream))
		(formatting-cell (fp-stream :align-x :right) (write-string "MC" fp-stream))
		(formatting-cell (fp-stream :align-x :right) (write-string "MH" fp-stream))
		(formatting-cell (fp-stream :align-x :right) (write-string "GS" fp-stream))
		(formatting-cell (fp-stream :align-x :right) (write-string "ETE" fp-stream))
		(formatting-cell (fp-stream :align-x :right) (write-string "ETA" fp-stream))
		(formatting-cell (fp-stream :align-x :right) (write-string "FUEL" fp-stream)))))
	  (setq total-time-enroute 0)
	  (do* ((currently-at start)
		(rem total-distance)
		(route-to (cdr leg-list) (cdr route-to)))	       
	       ((null route-to) nil)
	    (multiple-value-bind (leg-distance leg-true-course)
		(geodesic (route-segment-latitude currently-at) 
			  (route-segment-longitude currently-at) 
			  (route-segment-latitude (car route-to)) 
			  (route-segment-longitude (car route-to)))
	      (let* ((altitude (flight-plan-cruising-alt plan))
		     (estimated-wind (estimate-wind-at currently-at altitude))
		     (cruising-speed (aircraft-normal-cruise-speed plane))
		     (fuel-rate (aircraft-fuel-consumption-at-normal-cruise plane))
		     (deviation (position-deviation (route-segment-at currently-at)))
		     (mc (+ leg-true-course deviation)))
		(multiple-value-bind (th gs)
		    (true-heading-and-groundspeed leg-true-course cruising-speed 
						  (car estimated-wind) (cadr estimated-wind))
		  (let* ((mh (+ th deviation))
			 (leg-time (/ leg-distance gs))
			 (eta 
			   (+ total-time-enroute leg-time (flight-plan-departure-time plan)))
			 (fuel (* leg-time fuel-rate)))
		    (setq total-time-enroute (+ total-time-enroute leg-time))
		    (formatting-row (fp-stream)
		      (formatting-cell (fp-stream) 
			(format fp-stream "~A"
			  (route-segment-position-longname currently-at)))
		      (formatting-cell (fp-stream :align-x :right)	;ID
			(format fp-stream "~A"
			  (route-segment-position-name currently-at)))
		      (formatting-cell (fp-stream :align-x :right)	;TC
			(format fp-stream "~D" (floor leg-true-course))	)
		      (formatting-cell (fp-stream :align-x :right)	;Leg
			(format fp-stream "~1,1F" leg-distance))
		      (formatting-cell (fp-stream :align-x :right)	;Rem
			(format fp-stream "~1,1F" rem))
		      (formatting-cell (fp-stream :align-x :right)	;MC
			(format fp-stream "~D" (floor mc)))
		      (formatting-cell (fp-stream :align-x :right)	;MH
			(format fp-stream "~D" (floor mh)))
		      (formatting-cell (fp-stream :align-x :right)	;GS
			(format fp-stream "~D" (floor gs)))
		      (formatting-cell (fp-stream :align-x :right)	;ETE
			(format fp-stream "~A" (time-hhmm leg-time)))
		      (formatting-cell (fp-stream :align-x :right)	;ETA
			(format fp-stream "~A" (time-hhmm eta)))
		      (formatting-cell (fp-stream :align-x :right)	;Fuel
			(format fp-stream "~1,1F" fuel))))))
	      (setq currently-at  (car route-to) rem (setq rem (- rem leg-distance))))))
	(format fp-stream "~%")
	(formatting-table (fp-stream)
	  (formatting-row (fp-stream)
	    (formatting-cell (fp-stream) 
	      (format fp-stream "~A" (route-segment-position-longname end)))
	    (formatting-cell (fp-stream :align-x :right)	;ID
	      (format fp-stream "~A" (route-segment-position-name end)))))
	(format fp-stream "~%")
	(let* ((departure-time (flight-plan-departure-time plan))
	       (final-eta (+ total-time-enroute (flight-plan-departure-time plan)))
	       (fuel-on-board (flight-plan-fuel-on-board plan))
	       (fuel-consumption-at-cruise (aircraft-fuel-consumption-at-normal-cruise plane))
	       (total-fuel-used
		 (+ (aircraft-taxi-fuel plane)
		    (* total-time-enroute fuel-consumption-at-cruise)))
	       (average-fuel-usage 
		 (/ (* total-time-enroute fuel-consumption-at-cruise) total-time-enroute))
	       (cruising-altitude (flight-plan-cruising-alt plan))
	       (reserve-fuel (- fuel-on-board total-fuel-used))
	       (reserve-time (/ reserve-fuel fuel-consumption-at-cruise))
	       (true-airspeed (flight-plan-true-speed plan))
	       (reserve-distance (* reserve-time true-airspeed)))
	  (format fp-stream "~%")
	  (formatting-table (fp-stream)
	    (formatting-row (fp-stream)
	      (with-text-face (fp-stream :italic)
		(formatting-cell (fp-stream) (format fp-stream ""))
		(formatting-cell (fp-stream :align-x :center)
		  (write-string "A N A L Y S I S" fp-stream))))
	    (formatting-row (fp-stream)
	      (formatting-cell (fp-stream) 
		(format fp-stream "Depart at ~A" (time-hhmm departure-time)))
	      (formatting-cell (fp-stream) 
		(format fp-stream "Total Time ~A" (time-hhmm total-time-enroute)))
	      (setf (flight-plan-ete plan) total-time-enroute)
	      (formatting-cell (fp-stream) 
		(format fp-stream "Final ETA ~A" (time-hhmm final-eta))))
	    (formatting-row (fp-stream)
	      (formatting-cell (fp-stream) 
		(format fp-stream "Fuel on board ~1,1F" fuel-on-board))
	      (formatting-cell (fp-stream)
		(format fp-stream "Total Fuel ~1,1F gallons" total-fuel-used))
	      (formatting-cell (fp-stream) 
		(format fp-stream "Average fuel usage ~1,1F/hr" average-fuel-usage)))
	    (formatting-row (fp-stream)
	      (formatting-cell (fp-stream) 
		(format fp-stream "Total Distance ~1,1F nm" total-distance))
	      (formatting-cell (fp-stream) 
		(format fp-stream "Total Time ~A" (time-hhmm total-time-enroute)))	;+++
	      (formatting-cell (fp-stream) 
		(format fp-stream "Total Fuel ~1,1F gallons" total-fuel-used)))	;+++
	    (formatting-row (fp-stream)
	      (formatting-cell (fp-stream) 
		(format fp-stream "Cruise altitude ~A" cruising-altitude))    
	      (formatting-cell (fp-stream) 
		(format fp-stream "TAS ~A" true-airspeed))
	      #+ignore
	      (formatting-cell (fp-stream) 
		(format fp-stream "" #+ignore "CAS ~A" #+ignore 0)))	;unf +++
	    (formatting-row (fp-stream)
	      (formatting-cell (fp-stream) 
		(format fp-stream "Reserve Time ~A" (time-hhmm reserve-time)))
	      (formatting-cell (fp-stream) 
		(format fp-stream "Reserve Dist ~1,1F" reserve-distance))
	      (formatting-cell (fp-stream) 
		(format fp-stream "Reserve Fuel ~1,1F" reserve-fuel)))
	    ;+++ unf
	    (formatting-row (fp-stream)
	      (formatting-cell (fp-stream) 
		(format fp-stream "Cost @ $~1,2F/hr = $~1,2F"
		  (aircraft-cost-per-hour plane)
		  (* total-time-enroute (aircraft-cost-per-hour plane))))))
	  ;; Output any known wind info.
	  (format fp-stream "~%Winds Aloft~%")
	  (dolist (waypoint leg-list)
	    (let ((waypoint-printed nil))
	      (dolist (alt '(sfc 3000 6000 9000 12000))
		(let ((awind (cdr (assoc alt (route-segment-wind-info waypoint)))))
		  (when awind
		    (unless waypoint-printed
		      (format fp-stream "~%~A~%" (route-segment-position-longname waypoint)))
		    (setq waypoint-printed t)
		    (format fp-stream "~7A: ~A@~A~%" alt (car awind) (cadr awind))))))))))
    (format fp-stream "~%")))

;;; This needs to be a lot smarter!
(defun estimate-wind-at (waypoint altitude)
  (or (cdr (assoc altitude (route-segment-wind-info waypoint))) (list 0 0)))


(defun location-parser (stream compass-points)
  (let ((location
	  (accept `((sequence-enumerated
		      (member ,@compass-points)		;N/S or E/W
		      (integer 0 90)			;hours
		      (float 0.0 60.0)			;minutes
		      (null-or-type (integer 0 60)))	;seconds
		    :separator #\space)
		  :stream stream :prompt nil)))
    (let* ((compass-point (pop location))
	   (hours (pop location))
	   (minutes (pop location))
	   (seconds (or (pop location) 0)))
      (/ (float (+ seconds (* minutes 60) (* hours 3600)))
	 (if (eq compass-point (first compass-points)) 3600.0 -3600.00)))))

(defun location-printer (object stream compass-points &key acceptably)
  (if acceptably
      (format stream "~A" object)
    (format stream "~A ~3,'0D ~2,2F" 
      (if (< object 0) (second compass-points) (first compass-points))
      (floor (abs object))
      (- (* (abs object) 60) (* (floor (abs object)) 60)))))

(define-presentation-type longitude ())

(define-presentation-method present (object (type longitude) stream view &key acceptably)
  (declare (ignore view))
  (location-printer object stream '(W E) :acceptably acceptably))

(define-presentation-method accept ((type longitude) stream view &key)
  (declare (ignore view))
  (location-parser stream '(W E)))

(define-presentation-type latitude ())

(define-presentation-method present (object (type latitude) stream view &key acceptably)
  (declare (ignore view))
  (location-printer object stream '(N S) :acceptably acceptably))

(define-presentation-method accept ((type latitude) stream view &key)
  (declare (ignore view))
  (location-parser stream '(N S)))


(defun time-hhmmss (time-in-hours)
  (let* ((time-in-seconds (floor (* time-in-hours 3600)))
	 (hours (floor time-in-hours))
	 (minutes (- (floor time-in-seconds 60) (* hours 60)))
	 (seconds (- time-in-seconds (* hours 3600) (* minutes 60))))
    (if (zerop hours)
	(format nil "~1D:~2,'0D" minutes seconds)
      (format nil "~1D:~2,'0D:~2,'0D" hours minutes seconds))))

(defun time-hhmm (time-in-hours)
  (let* ((time-in-seconds (floor (* time-in-hours 3600)))
	 (hours (floor time-in-hours))
	 (minutes (- (floor time-in-seconds 60) (* hours 60))))
    (if (zerop hours)
	(format nil ":~2,'0D" minutes)
      (format nil "~1D:~2,'0D" hours minutes))))

(define-presentation-type time ())

(define-presentation-method present (object (type time) stream view &key acceptably)
  (declare (ignore view))
  (if acceptably
      (format stream "~A" object)
      (format stream "~A" (time-hhmm object))))

(define-presentation-method accept ((type time) stream view &key)
  (declare (ignore view))
  (let ((hhmm (accept '((sequence-enumerated
			  (integer 0 24)
			  (integer 0 60))
			:separator #\: :echo-space nil)
		      :stream stream :prompt nil)))
    (let* ((hours (pop hhmm))
	   (minutes (pop hhmm)))
      (/ (float (+ (* hours 60) minutes)) 60.0))))

(define-presentation-type wind ())

(define-presentation-method present (object (type wind) stream view &key acceptably)
  (declare (ignore view))
  (if acceptably
      (format stream "~A" object)
      (format stream "~A@~A" (first object) (second object))))

(define-presentation-method accept ((type wind) stream view &key)
  (declare (ignore view))
  (values (accept '((sequence-enumerated
		      (integer 0 360)		;direction
		      (integer 0 300))		;speed
		    :separator #\@ :echo-space nil)
		  :stream stream :prompt nil)))


;;; Viewport scaling

(defun get-display-center ()
  (let ((window *standard-output*))
    (with-bounding-rectangle* (left top right bottom) (window-viewport window)
      (unscale-coordinates (/ (+ left right) 2.0) (/ (+ top bottom) 2.0) window))))

(defun set-display-center (longitude latitude)
  (let ((window *standard-output*))
    (multiple-value-bind (x y)
	(scale-coordinates longitude latitude)
    (with-bounding-rectangle* (left top right bottom) (window-viewport window)
      (window-set-viewport-position window
				    (max 0 (- (floor x) (floor (- right left) 2)))
				    (max 0 (- (floor y) (floor (- bottom top) 2))))))))


;;; Flight-Planner user interface

(define-application-frame flight-planner ()
    ()
  (:panes 
    (display :application)
    (interactor :interactor :height '(5 :line)))
  (:layouts 
    (default
      (vertically () (3/4 display) (:fill interactor)))))

(define-flight-planner-command (com-zoom-in :name t :menu t) ()
  (multiple-value-bind (longitude latitude)
      (get-display-center)
    (setf *magnifier* (* *magnifier* 1.5))
    (window-clear *standard-output*)
    (set-display-center longitude latitude)
    (redraw-display)))

(define-flight-planner-command (com-zoom-out :name t :menu t) ()
  (multiple-value-bind (longitude latitude)
      (get-display-center)
    (setf *magnifier* (/ *magnifier* 1.5))
    (window-clear *standard-output*)
    (set-display-center longitude latitude)
    (redraw-display)))

(define-flight-planner-command (com-show-map :name t :menu t)
    ()
  (window-clear *standard-output*)
  (redraw-display))

(defmethod initialize-instance :after ((fp flight-planner) &key)
  (unless *position-list*
    (set-up)))

(defmethod enable-frame :after ((fp flight-planner))
  (let ((*application-frame* fp)
	(*standard-output* (frame-standard-output fp)))
    (redraw-display)))

(defmethod frame-standard-output ((p flight-planner))
  (get-frame-pane p 'display))

(defmethod frame-standard-input ((p flight-planner))
  (get-frame-pane p 'interactor))

(define-flight-planner-command (com-exit-flight-planner :name t :menu "Exit")
    ()
  ;; assume called via run-flight-planner
  (frame-exit *application-frame*))


;;; Database commands and support

(define-presentation-type latitude-and-longitude ())

(define-presentation-method present (object (type latitude-and-longitude) stream view &key)
  (declare (ignore view))
  (present (first object) 'latitude :stream stream)
  (write-char #\, stream)
  (present (second object) 'longitude :stream stream))

(define-presentation-method accept ((type latitude-and-longitude) stream view &key)
  (declare (ignore view))
  (values (accept '((sequence-enumerated latitude longitude))
		  :stream stream :prompt nil)))

;; Allows you to click anywhere when reading an X-and-Y to indicate that spot
(define-presentation-translator t-to-latitude-and-longitude
    ((or t blank-area) latitude-and-longitude flight-planner
     :gesture :select)
    (x y window)
  (multiple-value-bind (longitude latitude)
      (unscale-coordinates x y window)		;--- Why does this get bad value for Y ?
    (list latitude longitude)))

(defun route-start-object-p (thing)		;--- kludge!
  (or (typep thing 'airport)
      (typep thing 'named-intersection)
      (typep thing 'vor)))

(define-presentation-type-abbreviation route-start-object ()
  '(or airport named-intersection vor))

;;; Add <kind>
(define-flight-planner-command (com-add-object :name t :menu "Add")
    ((object '(member ground-position route victor-airway aircraft) ;; :confirm t
	     :prompt "Object")
     ;;--- what about keywords?
     (route-start '(null-or-type route-start-object)
		  :default nil
		  #+ignore :when #+ignore (eq object 'route)))
  (ecase object
    (ground-position
      (let ((new-position (query-new-position)))
	(when new-position
	  (present new-position 'ground-position :view +iconic-view+)
	  (push new-position *position-list*))))
    (route
      (let* ((new-route (query-new-route :route-start route-start)))
	(when new-route
	  (present new-route 'route :view +iconic-view+)
	  (push new-route *route-list*))))
    (victor-airway
      (let ((new-victor-airway (query-new-victor-airway)))
	(when new-victor-airway
	  (present new-victor-airway 'victor-airway :view +iconic-view+)
	  (push new-victor-airway *victor-airway-list*))))
    (aircraft
      (let ((new-aircraft (query-new-aircraft)))
	(when new-aircraft
	  (push new-aircraft *aircraft-list*))))))

(define-presentation-to-command-translator add-route
    (named-position com-add-object flight-planner
     :tester ((object)
	      (route-start-object-p object))
     :gesture :select)
    (object)
  (list 'route object))

(defun query-new-position ()
  (let ((stream (frame-standard-output *application-frame*))
	name
	long-name
	kind
	lat-and-long
	(altitude 0))
    (accepting-values (stream :own-window t)
      (setq name (accept 'string 
			 :prompt "Name" :stream stream))
      (terpri stream)
      (setq long-name (accept 'string 
			      :prompt "Long name" :stream stream))
      (terpri stream)
      (setq kind (accept '(member airport vor named-intersection visual-checkpoint)
			 :prompt "Kind of position" :stream stream))
      (terpri stream)
      (setq lat-and-long (accept 'latitude-and-longitude
				 :prompt "Latitude, Longitude" :stream stream))
      (terpri stream)
      (setq altitude (accept '(integer 0 60000) :prompt "Altitude" 
			     :default altitude :stream stream))
      (terpri stream))
    (make-instance kind
		   :name name
		   :longname long-name
		   :latitude (first lat-and-long)
		   :longitude (second lat-and-long)
		   :altitude altitude)))

(defun waypoint-object-p (thing)		;--- kludge!
  (or (null thing)
      (typep thing 'airport)
      (typep thing 'named-intersection)
      (typep thing 'vor)))

(define-presentation-type-abbreviation waypoint-object ()
  '(or null airport named-intersection vor))

(defun query-new-route (&key (name nil) (route-start nil))
  (do* ((overfly ())
	(point (or route-start
		   (accept 'route-start-object :prompt "Start"))
	       (accept 'waypoint-object :prompt "Waypoint"
		       :default nil :display-default nil)))
       ((null point)
	(setq overfly (nreverse overfly))
	(make-instance 'route 
		       :name (or name (generate-route-name-from-legs overfly))
		       :legs overfly))
    (push (make-instance 'route-segment :at point :wind-info nil) overfly)))

(defun query-new-victor-airway (&optional (name (accept 'string
							:prompt "Name of this Victor Airway")))
  (do* ((overfly ())
	(point (accept 'route-start-object  :prompt "Start")
	       (accept 'waypoint-object :prompt "Waypoint"
		       :default nil)))
       ((null point)
	(make-instance 'victor-airway :name name :legs (nreverse overfly)))
    (push (make-instance 'victor-airway-segment :at point) overfly)))

(defun query-new-aircraft ()
  (let ((stream (frame-standard-output *application-frame*))
	identification
	type
	(preferred-altitude 3500)
	(cruise-speed 110)
	(fuel-consumption 6)
	(maximum-usable-fuel 0)
	(cost-per-hour 50))
    (accepting-values (stream :own-window t)
      (setq identification (accept 'string 
				   :prompt "Identification" :stream stream))
      (terpri stream)
      (setq type (accept 'string 
			 :prompt "Type" :stream stream))
      (terpri stream)
      (setq preferred-altitude (accept 'integer 
				       :prompt "Preferred cruising altitude"
				       :default preferred-altitude :stream stream))
      (terpri stream)
      (setq cruise-speed (accept 'integer 
				 :prompt "Normal cruise speed"
				 :default cruise-speed :stream stream))
      (terpri stream)
      (setq fuel-consumption (accept 'float 
				     :prompt "Fuel consumption at normal cruise"
				     :default fuel-consumption :stream stream))
      (terpri stream)
      (setq maximum-usable-fuel (accept 'float 
					:prompt "Maximum usable fuel"
					:default maximum-usable-fuel :stream stream))
      (terpri stream)
      (setq cost-per-hour (accept 'float
				  :prompt "Cost per hour"
				  :default cost-per-hour :stream stream))
      (terpri stream))
    (make-instance 'aircraft
		   :identification identification
		   :type type
		   :taxi-fuel 0
		   :preferred-cruising-altitude preferred-altitude
		   :normal-cruise-speed cruise-speed
		   :fuel-consumption-at-normal-cruise fuel-consumption
		   :maximum-usable-fuel maximum-usable-fuel
		   :cost-per-hour cost-per-hour)))

(defun concrete-object-p (object)		;--- kludge!
  (or (typep object 'aircraft)
      (typep object 'victor-airway)
      (typep object 'route)
      (typep object 'ground-position)))

(define-presentation-type-abbreviation concrete-object ()
  '(or aircraft victor-airway route named-position ground-position))

;;; Delete <object>
(define-flight-planner-command com-delete-object
    ((object 'concrete-object :prompt "Object")
     (presentation 't)
     (window 't))
  (let ((stream (frame-standard-input *application-frame*)))
    (etypecase object
      (ground-position
	(format stream "~&Deleting position ~a.~%" object)
	(setq *position-list* (delete object *position-list*)))
      (route
	(format stream "~&Deleting route ~a.~%" object)
	(setq *route-list* (delete object *route-list*)))
      (victor-airway
	(format stream "~&Deleting victor-airway ~a.~%" object)
	(setq *victor-airway-list* (delete object *victor-airway-list*)))
      (aircraft
	(format stream "~&Deleting aircraft ~a.~%" object)
	(setq *aircraft-list* (delete object *aircraft-list*)))))
  (when presentation
    (clim:erase-output-record presentation window)))

(define-presentation-to-command-translator delete-object
    (t com-delete-object flight-planner
     :tester ((object)
	      (concrete-object-p object))
     :gesture :delete
     :documentation ((object stream)
		     ;; So that we don't see the keyword arguments...
		     (format stream "Delete Object ~A" object)))
    (object presentation window)
  (list object presentation window))

;;; Describe <object>
(define-flight-planner-command (com-describe-object :name t :menu "Describe")
    ((object 'concrete-object :prompt "Object"))
  (let ((stream (frame-standard-input *application-frame*)))
    (fresh-line stream)
    (describe-position-object object stream)))

(define-presentation-to-command-translator describe-object
    (t com-describe-object flight-planner
     :tester ((object)
	      (concrete-object-p object))
     :gesture :describe)
    (object)
  (list object))

;;; Edit <object>
(define-flight-planner-command (com-edit-object :name t :menu "Edit")
    ((argument 'aircraft ;; :confirm t
	       :prompt "Object"))
  (edit-aircraft argument))

(define-presentation-to-command-translator edit-object
    (aircraft com-edit-object flight-planner
     :gesture :edit)
    (object)
  (list object))

(define-flight-planner-command (com-flight-plan :name t :menu "Plan Flight")
    ((route 'route :prompt "Route"))
  (let* ((stream (frame-standard-output *application-frame*))
	 plan
	 (plane (or *last-plane* (first *aircraft-list*)))
	 (type 'vfr)
	 (equip (or (and plane (aircraft-type plane)) "C172/U"))
	 (airsp (or (and plane (aircraft-normal-cruise-speed plane)) 110))
	 (orig (route-segment-at (first (route-legs route))))
	 (dest (route-segment-at (car (last (route-legs route)))))
	 (deptm (/ (+ (* 12 60) 00) 60))
	 (alt (or (and plane (aircraft-preferred-cruising-altitude plane)) 3000))
	 remks
	 (fuel (or (and plane (aircraft-maximum-usable-fuel plane)) 0))
	 (alts nil)
	 pilot
	 (souls 1)
	 color)
    (accepting-values (stream :own-window t)
      (setq type (accept '(member vfr ifr dvfr) :prompt "Type"
			 :default type :stream stream))
      (terpri stream)
      (multiple-value-bind (new-plane ptype changed)
	  (accept 'aircraft :prompt "Aircraft Identification"
		  :default plane :stream stream)
	(declare (ignore ptype))
	(setq plane new-plane)
	(when (and changed plane)
	  ;; We would need to resynchronize the dialog, too, except that
	  ;; all the fields that depend on PLANE are after this one.
	  (setq equip (or (aircraft-type plane) "C172/U")
		airsp (or (aircraft-normal-cruise-speed plane) 110)
		alt   (or (aircraft-preferred-cruising-altitude plane) 3000)
		fuel  (or (aircraft-maximum-usable-fuel plane) 0))))
      (terpri stream)
      (setq equip (accept 'string :prompt "Aircraft Type/Special Equipment" 
			  :default equip :stream stream))
      (terpri stream)
      (setq airsp (accept 'integer :prompt "True Airspeed (kts)" 
			  :default airsp :stream stream))
      (terpri stream)
      (setq deptm (accept 'time :prompt "Proposed Departure Time"
			  :default deptm :stream stream))
      (terpri stream)
      (setq alt (accept 'integer :prompt "Cruising Altitude" 
			:default alt :stream stream))
      (terpri stream)
      (setq remks (accept '(null-or-type string) :prompt "Remarks" :stream stream))
      (terpri stream)
      (setq fuel (accept 'integer :prompt "Fuel on Board" 
			 :default fuel :stream stream))
      (terpri stream)
      (setq alts  (accept '(null-or-type airport) :prompt "Alternate Airport"
			  :default alts :stream stream))
      (terpri stream)
      (setq pilot (accept '(null-or-type string)
			  :prompt "Pilot's Name, Address, Phone number & Home Base"
			   :stream stream))
      (terpri stream)
      (setq souls (accept '(integer 1 500) :prompt "Number Aboard"
			  :default souls :stream stream))
      (terpri stream)
      (accept '(null-or-type string) :prompt "Color of Aircraft"
	      :default color :stream stream)
      (terpri stream))
    (setq *last-plane* plane)
    (setq plan (make-instance 'flight-plan
			      :type type
			      :aircraft-id plane
			      :aircraft-type equip
			      :true-speed airsp
			      :departure-point orig
			      :departure-time deptm
			      :cruising-alt alt
			      :route route
			      :destination dest
			      :remarks remks
			      :fuel-on-board fuel
			      :alternate alts
			      :pilot pilot
			      :souls souls
			      :color color))
    (accepting-values (stream
			:own-window t
			:exit-boxes '((:exit "Click here to remove this display")))
      (let ((fp-window stream))
 	(window-clear fp-window)
 	(compute-flight-plan fp-window plan)))))

(define-presentation-to-command-translator flight-plan
    (route com-flight-plan flight-planner
     :gesture :edit
     :priority +1)
    (object)
  (list object))



(define-flight-planner-command (com-show-distance :name t :menu t)
    ((start 'route-start-object)
     (end 'route-start-object))
  (multiple-value-bind (distance tc)
      (geodesic (point-latitude start) 
		(point-longitude start) 
		(point-latitude end) 
		(point-longitude end))
    (format *query-io* 
       "~&The distance between ~a and ~a is ~1,2F NM, and the true course is ~1,2F.~%"
      (position-name start) (position-name end) distance tc)))


;;; Misc. functions and constants

(defun square (n) (* n n))

(defun radian (degrees) (* degrees (/ pi 180)))
(defun degree (radians) (* radians (/ 180 pi)))

(defun geodesic (k m l n)			;arguments are in degrees
  (let* ((cc 0.0033901)
	 (o 3443.95594)				;semi major axis of Earth
	 (a (atan (* (- 1 cc) (tan (radian k)))))	;radians
	 (cosa (cos a))
	 (sina (sin a))
	 (b (atan (* (- 1 cc) (tan (radian l)))))	;radians
	 (cosb (cos b))
	 (sinb (sin b))
	 (d (* sina sinb))
	 (e (radian (- m n)))			;radians
	 (abse (abs e))
	 (cose (cos e))
	 (sinabse (sin abse))
	 (ff (+ (* sina sinb) (* cosa cosb cose)))
	 (s (* (square sinabse) (square cosb)))
	 (tt (square (- (* sinb cosa) (* sina cosb cose))))
	 (h (sqrt (+ s tt)))
	 (i (/ (- (square h)
		  (* (square sinabse) (square cosa) (square cosb)))
	       (square h)))
	 (j (* (atan (/ h ff))))		;radian
	 (g (+ j (* (/ (+ (square cc) cc) 2) (+ (* j (- 2 i)) (* h (- (* 2 d) (* i cc)))))))
	 (v (+ d (* h (/ 1 (tan (/ (* 180 j) pi)))) 
	       (* (square h) ff (+ (* 8 d (- (* i ff) d)) (- 1 (* 2 (square (abs ff))))))))
	 (p (+ g (* (/ (square cc) (* 16 h)) (+ (* 8 (square j) (- i 1) v) (* h j)))))
	 (r (* (- 1 cc) o p))			;R is distance

	 (a1 (* j (+ cc (square cc))))
	 (a3 (+ (* (/ (* d cc) (* 2 h)) (square (abs h))) (* 2 (square (abs j)))))
	 (a2 (* (/ (* i (square cc)) 4) (+ (* h ff) (* -5 j) 
				       (* 4 (square (abs j)) (/ 1 (tan j))))))
	 (q (+ a1 (- a2) a3))
	 (u (+ (* (/ (* (sin e) cosa cosb) h) q) e))
	 (w (- (* sinb cosa) (* (cos u) sina cosb)))
	 (x (* (sin u) cosb))
	 (a4 (atan (/ x w)))
	 (y (if (< a4 0) (+ a4 pi) a4))
	 (z (if (< e 0) (+ y pi) y)))
    (if (and (zerop z) (< l k)) (setq z pi))
    (values r (degree z))))
;    (list a b d e ff s tt h i j g v p 'dist r a1 a3 a2 q u w x a4 y 'dir z)))

;;; A position on the Geodesic globe.
;;; Miscellaneous functions

;;; Wind correction

;; tc=true course
;; th=true heading
;; v =true airspeed
;; gs=ground speed
(defun wind-speed-and-direction (tc th v gs)
  (let* ((w (- th tc))
	 (ht (- (* v (cos (radian w))) gs))
	 (cx (* v (sin (radian w))))
	 (ws (square (+ (abs (square ht)) (abs (square cx)))))
	 (w2 (* (degree (asin (/ cx ws))) (if (minusp (- v gs)) -1 1)))
	 (w1 (+ tc w2 (if (> gs v) 180 0)))
	 (wd (- w1 (* (floor (/ w1 360)) 360))))
    (setq wd (floor (+ (* 100 wd) 0.5) 100))
    (setq ws (floor (+ (* 100 ws) 0.5) 100))
    (values wd ws)))

;; tc=true course
;; v =true airspeed
;; wd=wind direction
;; b =wind speed
(defun true-heading-and-groundspeed (tc v wd b)
  (let* ((a (+ 180 wd (- 360 tc)))
	 (ht (* b (cos (radian a))))
	 (w (degree (asin (* b (/ (sin (radian a)) v)))))
	 (gs (+ (* v (cos (radian w))) ht))
	 (tt (- tc w))
	 (th (- tt (* (floor tt 360) 360))))
;    (setq th (floor (+ (* 100 wd) 0.5) 100))
;    (setq gs (floor (+ (* 100 wd) 0.5) 100))
    (values th gs)))

;(true-heading-and-groundspeed 229 125 270 14)
;(true-heading-and-groundspeed 229 125 0 0)

;;; Crosswind components (unused)

#+++ignore
(progn

(defun wind-components (wind-angle wind-speed)
  (let ((angle (radian wind-angle)))
    (values (* (cos angle) wind-speed)		;The headwind component
	    (* (sin angle) wind-speed))))	;The crosswind component

(defun density-altitude (pressure-altitude temperature)
  (* 145426 (- 1 (expt (/ (expt (/ (- 288.16 (* pressure-altitude 0.001981)) 288.16) 5.2563)
			  (/ (+ 273.16 temperature) 288.16))
		       0.235))))

(defun feet-per-minute (feet-per-mile ground-speed)
  (* feet-per-mile (/ ground-speed 60.0)))

(defun true-airspeed (indicated-airspeed altitude temperature)
  (let ((d (/ altitude (- 63691.776 (* 0.2190731712 altitude)))))
    (* indicated-airspeed (sqrt (/ (+ 273.16 temperature) (/ 288 (expt 10 d)))))))

;; mach = mach number, temperature is true OAT Celcius
(defun mach-to-true-airspeed (mach temperature)
  (* 38.967 mach (sqrt (+ 273.16 temperature))))

(defun leg-time (leg-distance leg-speed)
  (/ leg-distance leg-speed))

(defun leg-speed (leg-distance leg-time)
  (/ leg-distance leg-time))

(defun leg-distance (leg-speed leg-time)
  (* leg-speed leg-time))

(defun bank-angle-for-standard-rate-turn (speed)
  (degree (atan (/ (* speed 9.2177478 1.15) 3860))))

(defun g-force-in-bank (bank-angle)
  (/ 1 (cos (radian bank-angle))))

(defun diameter-of-turn (tas bank-angle)
  (/ (square tas) (* 34208 (tan (radian bank-angle)))))

(defun wind-correction-angle (wa ws tas)
  (degree (asin (/ (* ws (sin (radian wa))) tas))))

(defun speed-loss-due-to-crabbing (tas wca tas)
  (- tas (* tas (cos (radian wca)))))

)	;#+++ignore


;;; A simple cheat setup database

(defun add-position (name kind latitude longitude altitude deviation long-name)
  (when (or (> latitude *max-latitude*)
	    (< latitude *min-latitude*)
	    (> longitude *max-longitude*)
	    (< longitude *min-longitude*))
    (return-from add-position nil))
  (push (make-instance kind 
		       :name name 
		       :longname long-name
		       :latitude latitude 
		       :longitude longitude 
		       :altitude altitude
		       :deviation deviation) 
	*position-list*))

(defun add-aircraft (identification type altitude speed fuel-consumption max-fuel cost)
  (let ((aircraft (make-instance 'aircraft
				 :identification identification
				 :type type
				 :taxi-fuel 0
				 :preferred-cruising-altitude altitude
				 :normal-cruise-speed speed
				 :fuel-consumption-at-normal-cruise fuel-consumption
				 :maximum-usable-fuel max-fuel
				 :cost-per-hour cost)))
    (push aircraft *aircraft-list*)))

(defun customize-database ()
  ;; Airports
  (add-position "HFD" 'airport
		(degminsec 41 44) (degminsec 72 39) 19  15 "Hartford-Brainard")
 
  ;; Intersections
  (add-position "DREEM" 'named-intersection
		(degminsec 42 21.6) (degminsec 71 44.3) 0 15 "DREEM")
  (add-position "GRAYM" 'named-intersection
		(degminsec 42 06.1) (degminsec 72 01.9) 0 15 "GRAYM")
  (add-position "WITNY" 'named-intersection
		(degminsec 42 03) (degminsec 72 14.2) 0 15 "WITNY")
  (add-position "EAGRE" 'named-intersection
		(degminsec 41 45) (degminsec 72 20.6) 0 15 "EAGRE")

  ;; VOR's

  ;; Aircraft
  ;;            ident      type       alt   sp     fuel cost
  (add-aircraft "NCC-1701" "Starship" 35000 550 25 1000 10000)
  (add-aircraft "xyzzy"    "C172"     3500  110  6   50    50)
  
  )

(defun set-up ()
  (setq *position-list* nil
	*route-list* nil
	*victor-airway-list* nil
	*aircraft-list* nil)
  (dolist (bits *default-nav-data*)
    (apply #'(lambda (num name type freq longname lat1 lat2 lon1 lon2 dev ew elev)
	       (declare (ignore num ew freq))
	       (add-position 
		 name 
		 (case type
		   (a 'airport)
		   (v 'vor)
		   ((va av) 'airport)
		   ((an na) 'airport)
		   (c 'visual-checkpoint)	;actually visual cp
		   (n 'ndb))			;actually ndb
		 (degminsec lat1 lat2)
		 (degminsec lon1 lon2)
		 elev
		 dev
		 longname))
	   bits))
  (customize-database)
  t)


(define-demo "Flight Planner" flight-planner :width 1000 :height 800)

