;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-

;; $fiHeader: cad-demo.lisp,v 1.23 1993/07/27 01:45:14 colin Exp $

(in-package :clim-demo)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
 Portions copyright (c) 1989, 1990 International Lisp Associates."

;;; A simple gate-level CAD program.

;;; First we define the application-specific data structures, the components and their
;;; connection terminals.  The only part of the user interface specified at this
;;; level is the displayed representation (appearance) of the entities when they
;;; are drawn on the screen.

(defclass basic-thing (presentation displayed-output-record)
     ((x :initarg :x :accessor thing-x)
      (y :initarg :y :accessor thing-y)
      (size :initarg :size :accessor thing-size)))

(defmethod bounding-rectangle* ((thing basic-thing))
  (with-slots (x y size) thing
    (declare (type coordinate x y size))
    (values x y (+ x size) (+ y size))))

(defmethod region-contains-position-p ((thing basic-thing) x y)
  (with-bounding-rectangle* (left top right bottom) thing
    (and (<= left x)
	 (<= top y)
	 (>= right x)
	 (>= bottom y))))

(defmethod output-record-start-cursor-position ((thing basic-thing))
  (values 0 0))

(defmethod output-record-set-position ((thing basic-thing) new-x new-y)
  (with-slots (x y) thing
    (setf x (coordinate new-x)
	  y (coordinate new-y))))

(defmethod map-over-output-records-overlapping-region
	   (function (thing basic-thing) region
	    &optional (x-offset 0) (y-offset 0) &rest continuation-args)
  (declare (ignore region function x-offset y-offset continuation-args))
  (declare (dynamic-extent continuation-args))
  nil)

(defmethod map-over-output-records-containing-position
	   (function (thing basic-thing) x y
	    &optional (x-offset 0) (y-offset 0) &rest continuation-args)
  (declare (ignore x y function x-offset y-offset continuation-args))
  (declare (dynamic-extent continuation-args))
  nil)

(defmethod tree-recompute-extent ((thing basic-thing))
  nil)

(defvar *draw-connections* t)
(defmethod replay-output-record ((thing basic-thing) stream
				 &optional region (x-offset 0) (y-offset 0))
  (declare (ignore region x-offset y-offset))
  (if *draw-connections*
      (draw-self thing stream)
      (draw-body thing stream)))

(defmethod output-record-parent ((thing basic-thing))
  *application-frame*)

(defmethod output-record-refined-position-test ((comp basic-thing) x y)
  (declare (ignore x y))
  t)

(defmethod presentation-single-box ((thing basic-thing)) nil)

(defmethod presentation-object ((thing basic-thing))
  thing)

(defmethod thing-position ((thing basic-thing))
  (with-slots (x y) thing
    (values x y)))

(defmethod move ((thing basic-thing) new-x new-y)
  (with-slots (x y) thing
    (setf x (coordinate new-x)
	  y (coordinate new-y))))


;;;****************************************************************

;;; This is +red+ for color systems, otherwise +flipping-ink+.
(defvar *highlight-ink* +flipping-ink+)

(defvar *component-size* 
	#+Imach (sys:system-case
		  (:macivory 18)
		  (otherwise 25))
	#-Imach 25
 "Default display size of a component.")

;;; A connection belongs to a component.  The component may have any number of
;;; input and output connections, although currently only one output is supported.
(defclass connection
	  (basic-thing)
     ((component :initform nil :initarg :component :accessor connection-component)
      (other-connections :initform nil :accessor connection-other-connections)
      ;; Give wire router some hints
      (early :initarg :early :reader connection-early-p)
      (wire-offset :initarg :wire-offset :reader connection-wire-offset))
  (:default-initargs :size 5 :early nil :wire-offset 20))

(defmethod draw-body ((connection connection) stream &key (ink +foreground-ink+))
  (declare (ignore stream ink))
  ;; Don't do a thing
  )

(defmethod draw-self ((connection connection) stream &key (ink +foreground-ink+))
  (with-slots (x y size) connection
    (draw-circle* stream x y size
		  ;; compute filled from the value,
		  ;; white for on, black for off
		  :filled (not (connection-value connection))	;required method
		  :ink ink)))

(defmethod highlight-output-record ((connection connection) stream state)
  (if (eq *highlight-ink* +flipping-ink+)
      (with-slots (x y size) connection
	(draw-circle* stream x y (1+ size)
		      :filled t :ink +flipping-ink+))
      (ecase state
	(:highlight (draw-self connection stream :ink *highlight-ink*))
	(:unhighlight (draw-self connection stream :ink +foreground-ink+)))))

(defmethod bounding-rectangle* ((conn connection))
  (let ((fudge 2))
    (with-slots (x y size) conn
      (declare (type coordinate x y size))
      ;; size is a radius, but make the box larger so that connections
      ;; are easier to point to
      (values (- x size fudge) (- y size fudge)
	      (+ x size fudge) (+ y size fudge)))))

(defun connect (output input)
  ;; Inputs can have only one incoming connection, so remove this input
  ;; from its former incoming connection's outputs list.
  (with-slots (other-connections) input
    (when other-connections
      (setf (connection-other-connections (first other-connections))
	    (remove input (connection-other-connections (first other-connections)))))
    (setf other-connections (list output)))
  ;; Add this input to the list of other-connections of the output.
  (push input (connection-other-connections output)))

;;; Sort of hairy, but it always computes a connection's position relative to
;;; the current position of its owning component.  So, when the component is moved
;;; the new connection position is reflected immediately.
(defun compute-connection-position (connection)
  (with-slots (component) connection
    ;;--- CLOS bug, can't use COMPONENT in subsequent WITH-SLOTS
    (let ((foo component))
      (with-slots (x y inputs outputs) foo
	;; We don't deal with multiple outputs
	(assert (<= (length outputs) 1) nil
		"Don't know how to handle multiple outputs")
	(cond ((member connection outputs)
	       ;; The output has a constant location
	       (return-from compute-connection-position
		 (values (+ x *component-size*) y)))
	      ((member connection inputs)
	       ;; Divide up the available space (the height of the
	       ;; component) among the inputs, then figure out which input we are
	       ;; interested in and therefore how far down it is.
	       (let ((spacing (floor (* *component-size* 2) (1+ (length inputs))))
		     ;; Start at the top of the component
		     (y-pos (- y *component-size*)))
		 (let ((index (position connection inputs)))
		   (return-from compute-connection-position
		     (values x (+ y-pos (* spacing (1+ index))))))))
	      (t (error "Connection ~S is not among the connections of its component ~S"
			connection component)))))))

#-allegro
(defclass input (connection) ())

#+allegro
(eval-when (compile load eval)
  (defclass input (connection) ()))

(defmethod presentation-type ((input input))
  'input)

;;; An input connection's logic value is determined by the value of the output
;;; connection that is feeding it.
(defmethod connection-value ((conn input))
  (with-slots (other-connections) conn
    (assert (<= (length other-connections) 1)
	    nil
	    "Don't know how to handle multiple inputs to one connection.")
    ;; Floating inputs default to "off"
    (when other-connections
      (connection-value (first other-connections)))))

#-allegro
(defclass output (connection) ())

#+allegro
(eval-when (compile load eval)
  (defclass output (connection) ()))

(defmethod presentation-type ((output output))
  'output)

;;; An output connection's logic value is computed from the inputs by the
;;; component.
(defmethod connection-value ((conn output))
  (connection-value (slot-value conn 'component)))

;;;****************************************************************


#-allegro
(defclass component
	  (basic-thing)
     ((inputs :initform nil)
      (outputs :initform nil))
  (:default-initargs :size *component-size*))

#+allegro
(eval-when (compile load eval)
  (defclass component
	    (basic-thing)
      ((inputs :initform nil)
       (outputs :initform nil))
    (:default-initargs :size *component-size*)))

;;; Fill in the inputs and outputs from the init args.
(defmethod initialize-instance :after ((component component) &key (n-inputs 1) (n-outputs 1))
  ;; This early-p stuff is all a big kludge to get slightly better wire routing
  (let ((early-p nil)
	(offset-1 20)
	(offset-2 30))
    (flet ((make-one (type component)
	     (prog1 (make-instance type :component component
				   :early early-p
				   :wire-offset offset-1)
		    (rotatef offset-1 offset-2)
		    (setq early-p (not early-p)))))
      (dotimes (n n-inputs)
	(push (make-one 'input component) (slot-value component 'inputs)))
      (setq early-p nil)
      (setq offset-1 20 offset-2 30)
      (dotimes (n n-outputs)
	(push (make-one 'output component) (slot-value component 'outputs)))))
  ;; Place the newly-created connections relative to the component
  (move component (thing-x component) (thing-y component)))

(defun all-connections (component)
  (with-slots (inputs outputs) component
    (append inputs outputs)))

;;; When a component is added to the database, add its connections
(defmethod add-new-object :after (cd (new-object component))
  (dolist (conn (all-connections new-object))
    (add-new-object cd conn)))

(defmethod move :after ((component component) new-x new-y)
  (declare (ignore new-x new-y))
  (dolist (conn (all-connections component))
    (multiple-value-bind (x y)
	;; place the connections relative to their component
	(compute-connection-position conn)
      (move conn x y))))

;;; Call this on a component to display the whole thing
(defmethod draw-self ((component component) stream &key (ink +foreground-ink+))
  (draw-body component stream :ink ink)
  (draw-connections component stream :ink ink)
  (draw-wires component stream :ink ink))

;; Elegant, ain't we?
;; Why the hell can't Genera draw half circles in :ALU :FLIP?
;; Note the superb attention to detail in the selection of the
;; *ONLY* magic numbers that appear to work.
(defvar *component-start-angle* (+ #+Genera .00001 (* pi 3/2)))
(defvar *component-end-angle* (+ #+Genera .000001 (* pi 1/2)))

;;; Default body is a half-circle
(defmethod draw-body ((comp component) stream &key (ink +foreground-ink+))
  (with-slots (x y size) comp
    (draw-circle* stream x y size
		  :start-angle *component-start-angle*
		  :end-angle *component-end-angle*
		  :ink ink)))

;;; make a component behave as an output record
(defmethod bounding-rectangle* ((comp component))
  (with-slots (x y size) comp
    ;; Not (- X SIZE) because the component is a half-circle
    (values x (- y size) (+ x size) (+ y size))))

(defmethod output-record-set-position ((thing component) new-x new-y)
  (with-slots (x y size) thing
    (setf x (coordinate new-x)
	  y (coordinate (+ new-y size)))))


(defmethod presentation-type ((comp component))
  'component)

(defmethod highlight-output-record ((comp component) stream state)
  (if (eq *highlight-ink* +flipping-ink+)
      (with-slots (x y size) comp
	(draw-circle* stream x y (1+ size)
		      :start-angle *component-start-angle*
		      :end-angle *component-end-angle*
		      :ink +flipping-ink+))
      (ecase state
	(:highlight (draw-body comp stream :ink *highlight-ink*))
	(:unhighlight (draw-body comp stream :ink +foreground-ink+)))))

(defmethod draw-connections ((comp component) stream &key (ink +foreground-ink+))
  (dolist (conn (all-connections comp))
    (draw-self conn stream :ink ink)))

(defvar *draw-junctions* t)

;;; This guy is responsible for wire layout.  It doesn't have any global
;;; knowledge, so it can't avoid running over things or draw little humps
;;; where unconnected wires cross.
(defmethod draw-wires ((comp component) stream &key (ink +foreground-ink+))
  (with-slots (inputs outputs) comp
    (labels ((round-val (val &optional (direction :down))
	       (let ((chunk-size 20))
		 (ecase direction
		   (:up (incf val chunk-size))
		   (:down (decf val chunk-size) ))
		 (* chunk-size (round val chunk-size))))
	     (draw-junction (x y)
	       (when *draw-junctions*
		 (draw-rectangle* stream (- x 2) (- y 2) (+ x 3) (+ y 3) :ink ink)))
	     ;; Various routing helper functions.
	     ;; This one forks near X2, rounding down to the next CHUNK-SIZE
	     ;; X coord.
	     (draw-path-fork-late (x1 y1 x2 y2)
	       (draw-line* stream x1 y1 (round-val x2) y1 :ink ink)
	       (draw-junction (round-val x2) y1)
	       (draw-line* stream (round-val x2) y1 (round-val x2) y2 :ink ink)
	       (draw-junction (round-val x2) y2)
	       (draw-line* stream (round-val x2) y2 x2 y2 :ink ink))
	     ;; This one forks near X1, rouding up to the next CHUNK-SIZE
	     ;; X coord.
	     (draw-path-fork-early (x1 y1 x2 y2)
	       (draw-line* stream x1 y1 (round-val x1 :up) y1 :ink ink)
	       (draw-junction (round-val x1 :up) y1)
	       (draw-line* stream (round-val x1 :up) y1 (round-val x1 :up) y2 :ink ink)
	       (draw-junction (round-val x1 :up) y2)
	       (draw-line* stream (round-val x1 :up) y2 x2 y2 :ink ink))
	     ;; This one forks near X2, splitting off OFFSET units away.
	     #+ignore
	     (draw-path-fork-late-offset (x1 y1 x2 y2 offset)
	       (let ((x-mid (- x2 offset)))
		 (draw-line* stream x1 y1 x-mid y1 :ink ink)
		 (draw-line* stream x-mid y1 x-mid y2 :ink ink)
		 (draw-line* stream x-mid y2 x2 y2 :ink ink)))
	     ;; Path policy functions.  The one currently named DRAW-WIRE wins.
	     ;; This one forks late, extracting the offset from the connection.
	     ;; (see the code that creates connections)
	     #+ignore
	     (draw-wire-conn-offset (connection direction)
	       (dolist (other-conn (connection-other-connections connection))
		 (let ((conn connection))
		   ;; Always draw line from :FROM to :TO
		   (ecase direction
		     (:to (rotatef conn other-conn))
		     (:from ))
		   (multiple-value-bind (x y) (thing-position conn)
		     (multiple-value-bind (ox oy) (thing-position other-conn)
		       (draw-path-fork-late-offset
			 x y ox oy (connection-wire-offset other-conn)))))))
	     ;; This one forks early or late depending on a value stored in the connection
	     ;; at make-instance time.
	     (draw-wire #+ignore -early-late (connection direction)
	       (dolist (other-conn (connection-other-connections connection))
		 (let ((conn connection))
		   ;; Always draw line from :FROM to :TO
		   (ecase direction
		     (:to (rotatef conn other-conn))
		     (:from ))
		   (multiple-value-bind (x y) (thing-position conn)
		     (multiple-value-bind (ox oy) (thing-position other-conn)
		       (if (connection-early-p other-conn)
			   (draw-path-fork-early x y ox oy)
			   (draw-path-fork-late x y ox oy)))))))
	     ;; This one simply forks early for all connections.
	     #+ignore
	     (draw-wire-early (connection direction)
	       (multiple-value-bind (x y) (thing-position connection)
		 (dolist (oc (connection-other-connections connection))
		   (multiple-value-bind (ox oy)
		       (thing-position oc)
		     ;;(draw-line stream x y ox oy :ink ink)
		     ;; The draw-path guys need to know left-to-right ordering
		     ;; to do their jobs.
		     (ecase direction
		       (:to (draw-path-fork-early ox oy x y))
		       (:from (draw-path-fork-early x y ox oy)))))))
	     ;; This one simply forks late for all connections.
	     #+ignore
	     (draw-wire-late (connection direction)
	       (multiple-value-bind (x y) (thing-position connection)
		 (dolist (oc (connection-other-connections connection))
		   (multiple-value-bind (ox oy)
		       (thing-position oc)
		     ;;(draw-line stream x y ox oy :ink ink)
		     ;; The draw-path guys need to know left-to-right ordering
		     ;; to do their jobs.
		     (ecase direction
		       (:to (draw-path-fork-late ox oy x y))
		       (:from (draw-path-fork-late x y ox oy))))))))
      (dolist (i inputs)
	(draw-wire i :to))
      (dolist (o outputs)
	(draw-wire o :from)))))

;;; Various components

(defclass and-gate
	  (component)
     ()
  (:default-initargs :n-inputs 2))

(defmethod connection-value ((ag and-gate))
  (every #'connection-value (slot-value ag 'inputs)))

(defmethod equation-part ((ag and-gate))
  (let ((equation nil))
    (dolist (in (slot-value ag 'inputs))
      (let ((out (first (connection-other-connections in))))
	(when out
	  (unless (null equation)
	    (push "&" equation))
	  (push (equation-part (connection-component out))
		equation))))
    equation))

(defclass or-gate
	  (component)
     ()
  (:default-initargs :n-inputs 2))

(defmethod connection-value ((og or-gate))
  (some #'connection-value (slot-value og 'inputs)))

(defmethod equation-part ((og or-gate))
  (let ((equation nil))
    (dolist (in (slot-value og 'inputs))
      (let ((out (first (connection-other-connections in))))
	(when out
	  (unless (null equation)
	    (push "|" equation))
	  (push (equation-part (connection-component out))
		equation))))
    equation))

;; Elegant, ain't we?
;; Why the hell can't Genera draw half circles in :ALU :FLIP?
;; Note the superb attention to detail in the selection of the
;; *ONLY* magic numbers that appear to work.
(defvar *or-gate-start-angle* (+ #+Genera .00001 (* pi 3/2) -0.3))
(defvar *or-gate-end-angle* (+ #+Genera .000001 (* pi 1/2) 0.3))

;;; Default body is an almost-half-circle, so we get a different look
;;; for OR gates.  Looks marginal and XOR's funny under Genera.
(defmethod draw-body ((comp or-gate) stream &key (ink +foreground-ink+))
  (with-slots (x y size) comp
    (draw-circle* stream x y size
		  :start-angle *or-gate-start-angle*
		  :end-angle *or-gate-end-angle*
		  :ink ink)))

(defclass logic-constant
	  (component)
     ((name :initform nil)
      (value :initarg :value))
  (:default-initargs :n-inputs 0))

(defvar *name-code* (1- (char-code #\A)))

(defmethod initialize-instance :after ((lc logic-constant) &key)
	   (when *name-code*
	     (setf (slot-value lc 'name)
		   (string (code-char (incf *name-code*))))))

(defmethod connection-value ((component logic-constant))
  (slot-value component 'value))

(defmethod equation-part ((lc logic-constant))
  (slot-value lc 'name))

;;; Draw the logic "variable" name next to the component, or erase it.
;;; ---kludge since we have no draw-glyphs yet
(defmethod draw-body :after ((lc logic-constant) stream &key (ink +foreground-ink+))
  (with-slots (x y name) lc
    (when name
      (cond ((eq ink +background-ink+)
	     ;;--- gee, am I getting carried away?
	     (multiple-value-bind (nx ny)
 		 (values x (- y *component-size*))
	       (with-sheet-medium (medium stream)
		 (draw-rectangle* medium
				  (- nx 10) (- ny 10) nx (+ ny 20)
				  :ink +background-ink+))))
	    (t
	     (draw-text* stream name (- x 10) (- y 10)))))))

(defclass logic-one
	  (logic-constant)
     ()
  (:default-initargs :value t))

(defclass logic-zero
	  (logic-constant)
     ()
  (:default-initargs :value nil))

(defvar *component-types* '(("And Gate" :value and-gate)
			    ("Or Gate" :value or-gate)
			    ("Logic One" :value logic-one)
			    ("Logic Zero" :value logic-zero)))

;
;;; ****************************************************************

;;; The User Interface

;;; First define a "application" that manages the application's state variables
;;; and defines a high-level division of screen real estate.
(define-application-frame cad-demo
			  (standard-application-frame output-record)
    ((object-list :initform nil))
  (:panes
    (design-area :application))
  (:pointer-documentation t)
  (:layouts
    (default design-area)))

(defmethod initialize-instance :around ((cd cad-demo) &key)
  (call-next-method)
  (let ((dp (get-frame-pane cd 'design-area)))
    ;;--- kludge this one pane
    (setf (stream-output-history dp) cd)
    (setf (stream-recording-p dp) nil)))

(defmethod bounding-rectangle* ((cd cad-demo))
  (let ((left 0)
	(top 0)
	(right 0)
	(bottom 0))
    (flet ((compute-edges (element)
	     (with-bounding-rectangle* (le to ri bo) element
	       (clim-utils:minf left le)
	       (clim-utils:minf top to)
	       (clim-utils:maxf right ri)
	       (clim-utils:maxf bottom bo))))
      (declare (dynamic-extent #'compute-edges))
      (map-over-output-records-overlapping-region #'compute-edges cd nil))
    (values left top right bottom)))

(defmethod add-new-object ((cd cad-demo) new-object)
  (push new-object (slot-value cd 'object-list)))

;;; Make the cad demo application act as an output history
(defmethod map-over-output-records-overlapping-region
	   (function (cd cad-demo) region
	    &optional (x-offset 0) (y-offset 0) &rest continuation-args)
  (declare (dynamic-extent continuation-args))
  (dolist (object (slot-value cd 'object-list))
    (when (or (null region) (eq region +everywhere+)
	      (clim-internals::region-intersects-offset-region-p 
		object region x-offset y-offset))
      (apply function object continuation-args))))

(defmethod map-over-output-records-containing-position
	   (function (cd cad-demo) x y
	    &optional (x-offset 0) (y-offset 0) &rest continuation-args)
  (declare (dynamic-extent continuation-args))
  (translate-positions x-offset y-offset x y)
  (dolist (object (slot-value cd 'object-list))
    (when (region-contains-position-p object x y)
      (apply function object continuation-args))))

(defmethod output-record-start-cursor-position ((record cad-demo))
  (values 0 0))

(defmethod add-output-record (element (record cad-demo))
  (add-new-object record element))

(defmethod clear-output-record ((record cad-demo))
  (setf (slot-value record 'object-list) nil))

(defmethod output-record-parent ((record cad-demo))
  nil)

(defmethod replay-output-record ((record cad-demo) stream
				 &optional region (x-offset 0) (y-offset 0))
  (when (eq region +everywhere+)
    (setq region nil))
  (multiple-value-bind (rl rt rr rb)
      (and region (bounding-rectangle* region))
    (multiple-value-bind (xoff yoff) (output-record-position record)
      (map-over-output-records-overlapping-region
	#'(lambda (element)
	    (with-bounding-rectangle* (left top right bottom) element
	      (when (or (null region)
			(clim-utils:ltrb-overlaps-ltrb-p left top right bottom
							 rl rt rr rb))
		(replay-output-record element stream region
				      (+ x-offset xoff) (+ y-offset yoff)))))
	record nil x-offset y-offset))))


;;; The display function for the application-controlled output pane.  The
;;; application substrate automatically runs this.
(defmethod display-stuff ((frame cad-demo) stream)
  (dolist (object (slot-value frame 'object-list))
    (draw-self object stream)))

;;; Utility routines

;;; Should already exist on the POINT datatype
(define-presentation-type cad-position () )

(define-presentation-method present (object (type cad-position) stream (view textual-view)
				     &key)
  (format stream "~D, ~D" (car object) (cdr object)))

(define-presentation-method accept ((type cad-position) stream (view textual-view) &key)
  (values (accept '(sequence-enumerated integer integer)
		  :prompt nil :view view :stream stream)))

(define-presentation-method presentation-typep (object (type cad-position))
  (and (consp object)
       (integerp (car object))
       (integerp (cdr object))))

;;; Only over blank areas.
(define-presentation-translator select-position
    (blank-area cad-position cad-demo)
    (x y)
  (values (cons x y) nil '(:echo nil)))

;;; Now define the commands or commands of the application.  They will automatically
;;; show up in the :command-menu pane specified in the define-application form.

(defvar *component-prototypes* nil)

(defun make-component-prototypes ()
  (setq *component-prototypes* nil)
  ;; inhibit giving names to logic vars
  (let ((*name-code* nil))
    (dolist (ct (map 'list 'third *component-types*))
      (push (make-instance ct :x 0 :y 0) *component-prototypes*))))

;(make-component-prototypes)

;;; Return the class name of the selected component
(defun select-component (parent)
  (labels ((draw-icon-menu (menu presentation-type)
	     (formatting-table (menu :x-spacing 5)
	       (dolist (icon *component-prototypes*)
		 (with-output-as-presentation (menu icon presentation-type)
		   (formatting-row (menu)
		     (formatting-cell (menu)
		       (progn ;; (with-user-coordinates (menu) ...)
			 (draw-self icon menu)
			 (multiple-value-bind (x y)
			     (stream-cursor-position menu)
			   (stream-set-cursor-position
			     menu
			     ;; fudge for the fact that the presentation encloses the
			     ;; half of the circle that's invisible
			     (- x 20) (+ y *component-size*)))
			 (write-string (string (class-name (class-of icon))) menu)))))))))
    (with-menu (menu parent)
      (let ((component (menu-choose-from-drawer
			 menu 'menu-item #'draw-icon-menu)))
	(class-name (class-of component))))))



(defvar *component-menu* nil)

;;; This version caches the component menu.  Unfortunately this has problems
;;; when running on multiple roots!  Good for demos.
#+ignore
;;; Return the class name of the selected component
(defun select-component (parent)
  (labels ((draw-icon-menu (menu presentation-type)
	     (formatting-table (menu :x-spacing 5)
	       (dolist (icon *component-prototypes*)
		 (with-output-as-presentation (menu icon presentation-type)
		   (formatting-row (menu)
		     (formatting-cell (menu)
		       (with-user-coordinates (menu)
			 (draw-self icon menu)
			 (multiple-value-bind (x y)
			     (stream-cursor-position menu)
			   (stream-set-cursor-position
			     menu
			     ;; fudge for the fact that the presentation encloses the
			     ;; half of the circle that's invisible
			     (- x 20) (+ y *component-size*)))
			 (write-string (string (class-name (class-of icon))) menu)
			 ))))))
	     nil))
    (cond ((null *component-menu*)
	   (setq *component-menu* (allocate-resource 'clim-internals::menu parent))
	   (let ((component (menu-choose-from-drawer
			      *component-menu* 'menu-item #'draw-icon-menu)))
	     (class-name (class-of component))))
	  ;; Cached menu, so simulate the rest of the menu-choose mechanism here.
	  (t (let ((menu *component-menu*))
	       (multiple-value-bind (x y)
		   (stream-pointer-position-in-window-coordinates
		     (window-parent menu))
		 (position-sheet-carefully
		   (frame-top-level-sheet (pane-frame menu)) X y))
	       (window-expose menu)
	       (unwind-protect
		   (with-input-context ('menu-item)
				       (object presentation-type gesture)
			(loop (read-gesture :stream menu)
			      (beep))
		      (t (values (class-name (class-of object))
				 gesture)))
		 (window-set-visibility menu nil)))))))

;;; Try to start with a reasonable drawing for the demo.
(defun make-drawing (cd)
  (setq *name-code* (1- (char-code #\A)))
  (setf (slot-value cd 'object-list) nil)
  (flet ((mi (type x y)
	   (let ((obj (make-instance type
				     :x (floor (* x (/ *component-size* 25)))
				     :y (floor (* y (/ *component-size* 25))))))
	     (add-new-object cd obj)
	     obj))
	 (splice (out-comp in-comp conn-number)
	   (let ((out-conn (first (slot-value out-comp 'outputs)))
		 (in-conn (elt (slot-value in-comp 'inputs) conn-number)))
	     (connect out-conn in-conn))))
  (let (;; column 1
	(one1 (mi 'logic-one 100 100))
	(zero1 (mi 'logic-zero 100 200))
	(one2 (mi 'logic-one 100 300))
	(zero2 (mi 'logic-zero 100 450))
	;; column two
	(and1 (mi 'and-gate 200 150))
	(or1 (mi 'or-gate 200 350))
	;; colum three
	(or2 (mi 'or-gate 300 108))
	(and2 (mi 'and-gate 300 300))
	(or3 (mi 'or-gate 300 420))
	;; column four
	(or4 (mi 'or-gate 400 150))
	(and3 (mi 'and-gate 400 350))
	;; column five
	(or5 (mi 'or-gate 500 250))
	)
    (splice one1 and1 0)
    (splice zero1 and1 1)
    (splice one2 or1 0)
    (splice zero2 or1 1)
    (splice and1 and2 0)
    (splice or1 and2 1)
    (splice one1 or2 0)
    (splice and1 or2 1)
    (splice and1 and2 0)
    (splice or1 and2 1)
    (splice or1 or3 0)
    (splice zero2 or3 1)
    (splice or2 or4 0)
    (splice zero1 or4 1)
    (splice and2 and3 0)
    (splice or3 and3 1)
    (splice or4 or5 0)
    (splice and3 or5 1)
    )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-cad-demo-command (com-create-component :menu "Create" :keystroke #\C) ()
  (let* ((window (get-frame-pane *application-frame* 'design-area))
	 (type (menu-choose *component-types*
			    :associated-window (window-root window)
			    :cache t :unique-id 'component-types)))
    (when type
      (let* ((position (accept 'cad-position :stream window :prompt nil))
	     (object (make-instance type :x (car position) :y (cdr position))))
	(add-new-object *application-frame* object)
	(draw-self object window)))))

;;; Takes two operands, an input terminal and an output terminal
;;; --- This needs to propagate value changes down the line, or
;;; rather redraw those components whose values have changed.
(define-cad-demo-command (com-connect-gates :menu "Connect")
    ((output 'output :gesture :select)
     (input 'input :gesture :select))
  (let ((win (get-frame-pane *application-frame* 'design-area)))
    (draw-self (connection-component input) win :ink +background-ink+)
    (draw-self (connection-component output) win :ink +background-ink+)
    (connect output input)
    (draw-self (connection-component input) win)
    (draw-self (connection-component output) win)))

;;; Moves a component.
(define-cad-demo-command (com-move-component :menu "Move")
    ((component 'component :gesture :select))
  (let ((stream (get-frame-pane *application-frame* 'design-area)))
    (draw-self component stream :ink +background-ink+)
    (multiple-value-bind (x y delta-x delta-y)
	(let ((*draw-connections* nil))
	  (drag-output-record stream component
			      :repaint t
			      :erase #'(lambda (c s)
					 (draw-body c s :ink +background-ink+))
			      :finish-on-release t))
      (move component (- x delta-x) (+ *component-size* (- y delta-y))))
    (draw-self component stream)))

(define-cad-demo-command (com-clear :menu "Clear" :keystroke #\L)
    ()
  (with-slots (object-list) *application-frame*
    (setf object-list nil)
    (window-clear (get-frame-pane *application-frame* 'design-area))))

(define-cad-demo-command (com-refresh :menu "Refresh" :keystroke #\R)
    ()
  (window-erase-viewport (get-frame-pane *application-frame* 'design-area))
  (redisplay-frame-pane *application-frame* 'design-area :force-p t))

(define-cad-demo-command (com-show :menu "Show")
    ((output 'output :gesture :describe))
  (let ((comp (connection-component output))
	(win (get-frame-pane *application-frame* 'design-area)))
    (stream-set-cursor-position win 0 0)
    (draw-rectangle* win 0 0 800 20 :ink +background-ink+)
    (with-text-style (win '(:sans-serif :bold :very-large))
      (format win "~A" (equation-part comp)))))

(define-cad-demo-command (com-setup :menu "Setup" :keystroke #\S)
    ()
  (make-drawing *application-frame*)
  (com-refresh))

(define-cad-demo-command (com-exit-cad-demo :menu "Exit" :keystroke #\X)
    ()
  (frame-exit *application-frame*))

#+++ignore	;--- right now there is only one layout
(define-cad-demo-command (com-swap-layouts :menu "Swap Layouts")
    ()
  (let ((current-layout (frame-current-layout *application-frame*)))
    (setf (frame-current-layout *application-frame*)
	  (case current-layout
	    (main 'other)
	    (other 'main)))))

#||

Things to do


add commands to scale up and down
but first get better menu formatting!

||#



(define-demo "CAD Demo" cad-demo :width 700 :height 600)
