;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000,2001 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)
;;;  (c) copyright 2001 by
;;;           Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;           Lionel Salabartan (salabart@emi.u-bordeaux.fr)
;;;  (c) copyright 2001, 2002 by Alexey Dejneka (adejneka@comail.ru)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; TODO:
;;;
;;; - Scrolling does not work correctly. Region is given in "window"
;;; coordinates, without bounding-rectangle-position transformation.
;;; (Is it still valid?)
;;;
;;; - Redo setf*-output-record-position, extent recomputation for
;;; compound records
;;;
;;; - When DRAWING-P is NIL, should stream cursor move?
;;;
;;; - :{X,Y}-OFFSET.
;;;
;;; - (SETF OUTPUT-RECORD-START-CURSOR-POSITION) does not affect the
;;; bounding rectangle. What does it affect?
;;;
;;; - How should (SETF OUTPUT-RECORD-POSITION) affect the bounding
;;; rectangle of the parent? Now its bounding rectangle is accurately
;;; recomputed, but it is very inefficient for table formatting. It
;;; seems that CLIM is supposed to keep a "large enougn" rectangle and
;;; to shrink it to the correct size only when the layout is complete
;;; by calling TREE-RECOMPUTE-EXTENT.
;;;
;;; - Computation of the bounding rectangle of lines/polygons ignores
;;; LINE-STYLE-CAP-SHAPE.
;;;
;;; - Rounding of coordinates.
;;;
;;; - Document carefully the interface of
;;; STANDARD-OUTPUT-RECORDING-STREAM.
;;;
;;; - COORD-SEQ is a sequence, not a list.

;;; Troubles

;;; DC
;;;
;;; Some GFs are defined to have "a default method on CLIM's standard
;;; output record class". What does it mean? What is "CLIM's standard
;;; output record class"? Is it OUTPUT-RECORD or BASIC-OUTPUT-RECORD?
;;; Now they are defined on OUTPUT-RECORD.

;;; TDO
;;;
;;; Text output record must save ink and clipping region. But its
;;; protocol does not give any way to do it! And a user can put in a
;;; history a record of any class :(. Now we are using
;;; *DRAWING-OPTIONS* to put the necessary information and make sure
;;; that only instances of STANDARD-TEXT-OUTPUT-RECORD are used for
;;; recording. -- APD, 2002-06-15.

(in-package :CLIM-INTERNALS)

(define-protocol-class output-record (bounding-rectangle)
  ())

(define-protocol-class displayed-output-record (output-record)
  ())

;;; 16.2.1. The Basic Output Record Protocol
#+:cmu(declaim (ftype (function (output-record) (values rational rational))
		      output-record-position))
(defgeneric output-record-position (record)
  (:documentation
   "Returns the x and y position of RECORD. The position is the
position of the upper-left corner of its bounding rectangle. The
position is relative to the stream, where (0,0) is (initially) the
upper-left corner of the stream."))

(defgeneric* (setf output-record-position) (x y record)
  (:documentation
   "Changes the x and y position of the RECORD to be X and Y, and
updates the bounding rectangle to reflect the new position (and saved
cursor positions, if the output record stores it). If RECORD has any
children, all of the children (and their descendants as well) will be
moved by the same amount as RECORD was moved. The bounding rectangles
of all of RECORD's ancestors will also be updated to be large enough
to contain RECORD."))

#+:cmu(declaim (ftype (function (output-record) (values integer integer))
		      output-record-start-cursor-position))
(defgeneric output-record-start-cursor-position (record)
  (:documentation
   "Returns the x and y starting cursor position of RECORD. The
positions are relative to the stream, where (0,0) is (initially) the
upper-left corner of the stream."))

(defgeneric* (setf output-record-start-cursor-position) (x y record))

#+:cmu(declaim (ftype (function (output-record) (values integer integer))
		      output-record-end-cursor-position))
(defgeneric output-record-end-cursor-position (record)
  (:documentation
   "Returns the x and y ending cursor position of RECORD. The
positions are relative to the stream, where (0,0) is (initially) the
upper-left corner of the stream."))

(defgeneric* (setf output-record-end-cursor-position) (x y record))

(defgeneric output-record-parent (record)
  (:documentation
   "Returns the output record that is the parent of RECORD, or NIL if
RECORD has no parent."))

(defgeneric (setf output-record-parent) (parent record)
  (:documentation "Non-standard function."))

(defgeneric replay-output-record (record stream
                                  &optional region x-offset y-offset)
  (:documentation "Displays the output captured by RECORD on the
STREAM, exactly as it was originally captured. The current user
transformation, line style, text style, ink and clipping region of
STREAM are all ignored. Instead, these are gotten from the output
record.

Only those records that overlap REGION are displayed."))

(defgeneric output-record-hit-detection-rectangle* (record))

(defgeneric output-record-refined-position-test (record x y))

(defgeneric highlight-output-record (record stream state))

(defgeneric displayed-output-record-ink (displayed-output-record))

;;; 16.2.2. Output Record "Database" Protocol

(defgeneric output-record-children (record))

(defgeneric add-output-record (child record))

(defgeneric delete-output-record (child record &optional errorp))

(defgeneric clear-output-record (record))

(defgeneric output-record-count (record))

(defgeneric map-over-output-records-containing-position
  (function record x y &optional x-offset y-offset &rest function-args)
  (:documentation "Maps over all of the children of RECORD that
contain the point at (X,Y), calling FUNCTION on each one. FUNCTION is
a function of one or more arguments, the first argument being the
record containing the point. FUNCTION is also called with all of
FUNCTION-ARGS as APPLY arguments.

If there are multiple records that contain the point,
MAP-OVER-OUTPUT-RECORDS-CONTAINING-POSITION hits the most recently
inserted record first and the least recently inserted record
last. Otherwise, the order in which the records are traversed is
unspecified."))

(defgeneric map-over-output-records-overlapping-region
  (function record region &optional x-offset y-offset &rest function-args)
  (:documentation "Maps over all of the children of the RECORD that
overlap the REGION, calling FUNCTION on each one. FUNCTION is a
function of one or more arguments, the first argument being the record
overlapping the region. FUNCTION is also called with all of
FUNCTION-ARGS as APPLY arguments.

If there are multiple records that overlap the region and that overlap
each other, MAP-OVER-OUTPUT-RECORDS-OVERLAPPING-REGION hits the least
recently inserted record first and the most recently inserted record
last. Otherwise, the order in which the records are traversed is
unspecified. "))

;;; From the Franz CLIM user's guide but not in the spec... clearly necessary.
;;; What is its status? -- APD, 2002-06-14.
(defgeneric map-over-output-records
    (continuation record &optional x-offset y-offset &rest continuation-args))

;;; 16.2.3. Output Record Change Notification Protocol

(defgeneric recompute-extent-for-new-child (record child))

(defgeneric recompute-extent-for-changed-child
  (record child old-min-x old-min-y old-max-x old-max-y))

(defgeneric tree-recompute-extent (record))

;;; 16.3. Types of Output Records
(define-protocol-class graphics-displayed-output-record
    (displayed-output-record)
  ())

(define-protocol-class text-displayed-output-record (displayed-output-record)
  ())

;;; 16.3.3. Text Displayed Output Record
(defgeneric add-character-output-to-text-record
  (text-record character text-style width height baseline))

(defgeneric add-string-output-to-text-record
  (text-record string start end text-style width height baseline))

(defgeneric text-displayed-output-record-string (text-record))

;;; 16.4. Output Recording Streams
(define-protocol-class output-recording-stream ()
  ())

;;; 16.4.1. The Output Recording Stream Protocol
(defgeneric stream-recording-p (stream))

(defgeneric (setf stream-recording-p) (recording-p stream))

(defgeneric stream-drawing-p (stream))

(defgeneric (setf stream-drawing-p) (drawing-p stream))

(defgeneric stream-output-history (stream))

(defgeneric stream-current-output-record (stream))

(defgeneric (setf stream-current-output-record) (record stream))

(defgeneric stream-add-output-record (stream record))

(defgeneric stream-replay (stream &optional region))

(defgeneric erase-output-record (record stream &optional errorp))

;;; 16.4.3. Text Output Recording
(defgeneric stream-text-output-record (stream text-style))

(defgeneric stream-close-text-output-record (stream))

(defgeneric stream-add-character-output
  (stream character text-style width height baseline))

(defgeneric stream-add-string-output
  (stream string start end text-style width height baseline))

;;; 16.4.4. Output Recording Utilities
(defgeneric invoke-with-output-recording-options
    (stream continuation record draw))

(defgeneric invoke-with-new-output-record (stream continuation record-type
                                           &rest initargs
                                           &key
                                           &allow-other-keys))

(defgeneric invoke-with-output-to-output-record
    (stream continuation record-type
     &rest initargs
     &key
     &allow-other-keys))

(defgeneric make-design-from-output-record (record))

;;; 21.3 Incremental Redisplay Protocol.  These generic functions need
;;; to be implemented for all the basic displayed-output-records, so they are
;;; defined in this file.

(defgeneric match-output-records (record &rest args))

(defgeneric match-output-records-1 (record &key)
  (:method-combination and :most-specific-last))

;;; Macros
(defmacro with-output-recording-options ((stream
					  &key (record nil record-supplied-p)
					       (draw nil draw-supplied-p))
					 &body body)
  (when (eq stream 't) (setq stream '*standard-output*))
  (check-type stream symbol)
  (with-gensyms (continuation)
    `(flet ((,continuation  (,stream)
	      (declare (ignorable ,stream))
	      ,@body))
       (declare (dynamic-extent #',continuation))
       (invoke-with-output-recording-options
        ,stream #',continuation
        ,(if record-supplied-p record `(stream-recording-p ,stream))
        ,(if draw-supplied-p draw `(stream-drawing-p ,stream))))))

(defmacro with-new-output-record ((stream
                                   &optional
                                   (record-type ''standard-sequence-output-record)
                                   (record nil record-supplied-p)
                                   &rest initargs)
                                  &body body)
  "Creates a new output record of type RECORD-TYPE and then captures
the output of BODY into the new output record, and inserts the new
record into the current \"open\" output record assotiated with STREAM.
    If RECORD is supplied, it is the name of a variable that will be
lexically bound to the new output record inside the body. INITARGS are
CLOS initargs that are passed to MAKE-INSTANCE when the new output
record is created.
    It returns the created output record.
    The STREAM argument is a symbol that is bound to an output
recording stream. If it is T, *STANDARD-OUTPUT* is used."
  (when (eq stream 't) (setq stream '*standard-output*))
  (check-type stream symbol)
  (unless record-supplied-p (setq record (gensym)))
  `(invoke-with-new-output-record ,stream
                                  #'(lambda (,stream ,record)
                                      (declare (ignorable ,stream ,record))
                                      ,@body)
                                  ,record-type
                                  ,@initargs))

(defmacro with-output-to-output-record
    ((stream
      &optional (record-type ''standard-sequence-output-record)
                (record nil record-supplied-p)
      &rest initargs)
     &body body)
  "Creates a new output record of type RECORD-TYPE and then captures
the output of BODY into the new output record. The cursor position of
STREAM is initially bound to (0,0)
    If RECORD is supplied, it is the name of a variable that will be
lexically bound to the new output record inside the body. INITARGS are
CLOS initargs that are passed to MAKE-INSTANCE when the new output
record is created.
    It returns the created output record.
    The STREAM argument is a symbol that is bound to an output
recording stream. If it is T, *STANDARD-OUTPUT* is used."
  (when (eq stream 't) (setq stream '*standard-output*))
  (check-type stream symbol)
  (unless record-supplied-p (setq record (gensym "RECORD")))
  `(invoke-with-output-to-output-record
    ,stream
    #'(lambda (,stream ,record)
        (declare (ignorable ,stream ,record))
        ,@body)
    ,record-type
    ,@initargs))


;;;; Implementation

(defclass basic-output-record (standard-bounding-rectangle output-record)
  ((parent :initarg :parent ; XXX
	   :initform nil
           :accessor output-record-parent)) ; XXX
  (:documentation "Implementation class for the Basic Output Record Protocol."))

(defmethod initialize-instance :after ((record basic-output-record)
				       &rest args
                                       &key (x-position 0) (y-position 0))
  (declare (ignore args))
  (with-slots (x1 y1 x2 y2) record
    (setq x1 x-position
	  y1 y-position
	  x2 x-position
	  y2 y-position)))

(defclass compound-output-record (basic-output-record)
  ((x :initarg :x-position
      :initform 0
      :documentation "X-position of the empty record.")
   (y :initarg :y-position
      :initform 0
      :documentation "Y-position of the empty record.")
   (in-moving-p :initform nil
                :documentation "Is set while changing the position."))
  (:documentation "Implementation class for output records with children."))

;;; 16.2.1. The Basic Output Record Protocol
(defmethod output-record-position ((record basic-output-record))
  (bounding-rectangle-position record))

(defmethod* (setf output-record-position) (nx ny (record basic-output-record))
  (with-slots (x1 y1 x2 y2) record
    (let ((dx (- nx x1))
          (dy (- ny y1)))
      (setf x1 nx  y1 ny
            x2 (+ x2 dx)  y2 (+ y2 dy))))
  (values nx ny))

(defmethod* (setf output-record-position) :around
    (nx ny (record basic-output-record))
  (with-bounding-rectangle* (min-x min-y max-x max-y) record
    (call-next-method)
    (let ((parent (output-record-parent record)))
      (when parent
        (recompute-extent-for-changed-child parent record
                                            min-x min-y max-x max-y))))
  (values nx ny))

(defmethod* (setf output-record-position) :before
    (nx ny (record compound-output-record))
  (with-slots (x1 y1 in-moving-p) record
    (letf ((in-moving-p t))
      (let ((dx (- nx x1))
            (dy (- ny y1)))
        (map-over-output-records
         (lambda (child)
           (multiple-value-bind (x y) (output-record-position child)
             (setf (output-record-position child)
                   (values (+ x dx) (+ y dy)))))
         record)))))

(defmethod output-record-start-cursor-position ((record basic-output-record))
  (values nil nil))

(defmethod* (setf output-record-start-cursor-position)
    (x y (record basic-output-record))
  (values x y))

(defmethod output-record-end-cursor-position ((record basic-output-record))
  (values nil nil))

(defmethod* (setf output-record-end-cursor-position)
    (x y (record basic-output-record))
  (values x y))

#+cmu
(progn
  ;; Sometimes CMU's PCL fails with forward reference classes, so this
  ;; is a kludge to keep it happy.
  ;;
  ;; This was reported as a bug to cmucl-imp [<E18vKN1-0004DQ-00@saphir.local>]
  ;;
  ;; In short it exposes itself when you compile and load into a
  ;; _virgin_ lisp the following:
  ;;
  ;;   (defclass foo (bar) ())
  ;;   (defun barz () (make-instance 'foo))
  ;;   (defclass bar () ())
  ;;
  ;; --GB 2003-03-18
  ;;
  (defclass gs-ink-mixin () ())
  (defclass gs-clip-mixin () ())
  (defclass gs-line-style-mixin () ())
  (defclass gs-text-style-mixin () ()))

;;; Humph. It'd be nice to tie this to the actual definition of a
;;; medium. -- moore
(defclass complete-medium-state
    (gs-ink-mixin gs-clip-mixin gs-line-style-mixin gs-text-style-mixin)
  ())

(defun replay (record stream &optional region)
  (stream-close-text-output-record stream)
  (when (stream-drawing-p stream)
    (with-cursor-off stream
      (letf (((stream-cursor-position stream) (values 0 0))
             ((stream-recording-p stream) nil)
	     ;; Is there a better value to bind to baseline?
             ((slot-value stream 'baseline) (slot-value stream 'baseline)))
	(with-sheet-medium (medium stream)
	  (let ((medium-state (make-instance 'complete-medium-state
					     :medium medium)))
	    (unwind-protect
		 (progn
		   (setf (medium-transformation medium)
			 +identity-transformation+)
		   (replay-output-record record stream region))
	      (set-medium-graphics-state medium-state medium))))))))


(defmethod replay-output-record ((record compound-output-record) stream
				 &optional region (x-offset 0) (y-offset 0))
  (when (null region)
    (let ((viewport (pane-viewport stream)))
      (cond ((not (null viewport))
             (setf region (untransform-region (sheet-delta-transformation stream viewport)
                                            (pane-viewport-region stream))))
            (t
             (setq region +everywhere+)))))
  (with-drawing-options (stream :clipping-region region)
    (map-over-output-records-overlapping-region
     #'replay-output-record record region x-offset y-offset
     stream region x-offset y-offset)))

(defmethod output-record-hit-detection-rectangle* ((record output-record))
  ;; XXX DC
  (bounding-rectangle* record))

(defmethod output-record-refined-position-test ((record basic-output-record)
						x y)
  (declare (ignore x y))
  t)

;;; XXX Should this only be defined on recording streams?
(defmethod highlight-output-record ((record output-record)
				    stream state)
  ;; XXX DC
  ;; XXX Disable recording?
  (letf (((medium-transformation stream) +identity-transformation+))
    (multiple-value-bind (x1 y1 x2 y2)
        (output-record-hit-detection-rectangle* record)
      (ecase state
        (:highlight
         (draw-rectangle* (sheet-medium stream) x1 y1 x2 y2
                          :filled nil :ink +foreground-ink+)) ; XXX +FLIPPING-INK+?
        (:unhighlight
         (draw-rectangle* (sheet-medium stream) x1 y1 x2 y2
                          :filled nil :ink +background-ink+)))))) ; XXX +FLIPPING-INK+?

;;; 16.2.2. The Output Record "Database" Protocol
(defmethod output-record-children ((record basic-output-record))
  nil)

(defmethod add-output-record (child (record basic-output-record))
  (declare (ignore child))
  (error "Cannot add a child to ~S." record))

(defmethod add-output-record :before (child (record compound-output-record))
  (let ((parent (output-record-parent child)))
    (when parent
      (restart-case
          (error "~S already has a parent ~S." child parent)
        (delete ()
          :report "Delete from the old parent."
          (delete-output-record child parent))))))

(defmethod add-output-record :after (child (record compound-output-record))
  (recompute-extent-for-new-child record child))

(defmethod delete-output-record (child (record basic-output-record)
                                 &optional (errorp t))
  (declare (ignore child))
  (when errorp (error "Cannot delete a child from ~S." record)))

(defmethod delete-output-record :after (child (record compound-output-record)
                                              &optional (errorp t))
  (declare (ignore errorp))
  (with-bounding-rectangle* (x1 y1 x2 y2) child
    (recompute-extent-for-changed-child record child x1 y1 x2 y2)))

(defmethod clear-output-record ((record basic-output-record))
  (error "Cannot clear ~S." record))

(defmethod clear-output-record :after ((record compound-output-record))
  (with-slots (x y x1 y1 x2 y2) record
    (setf x1 x  y1 y
          x2 x  y2 y)))

(defmethod output-record-count ((record basic-output-record))
  0)

(defmethod map-over-output-records
    (function (record basic-output-record)
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  (declare (ignore function x-offset y-offset function-args))
  nil)

;;; This needs to work in "most recently added last" order. Is this
;;; implementation right? -- APD, 2002-06-13
#+nil
(defmethod map-over-output-records
    (function (record compound-output-record)
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  (declare (ignore x-offset y-offset))
  (map nil (lambda (child) (apply function child function-args))
       (output-record-children record)))

(defmethod map-over-output-records-containing-position
    (function (record basic-output-record) x y
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  (declare (ignore function x y x-offset y-offset function-args))
  nil)

;;; This needs to work in "most recently added first" order. Is this
;;; implementation right? -- APD, 2002-06-13
#+nil
(defmethod map-over-output-records-containing-position
    (function (record compound-output-record) x y
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  (declare (ignore x-offset y-offset))
  (map nil
       (lambda (child)
         (when (and (multiple-value-bind (min-x min-y max-x max-y)
			(output-record-hit-detection-rectangle* child)
		      (and (<= min-x x max-x) (<= min-y y max-y)))
		    (output-record-refined-position-test child x y))
           (apply function child function-args)))
       (output-record-children record)))

(defmethod map-over-output-records-overlapping-region
    (function (record basic-output-record) region
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  (declare (ignore function region x-offset y-offset function-args))
  nil)

;;; This needs to work in "most recently added last" order. Is this
;;; implementation right? -- APD, 2002-06-13
#+nil
(defmethod map-over-output-records-overlapping-region
    (function (record compound-output-record) region
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  (declare (ignore x-offset y-offset))
  (map nil
       (lambda (child) (when (region-intersects-region-p region child)
                         (apply function child function-args)))
       (output-record-children record)))

;;; 16.2.3. Output Record Change Notification Protocol
(defmethod recompute-extent-for-new-child
    ((record compound-output-record) child)
  (with-bounding-rectangle* (old-x1 old-y1 old-x2 old-y2) record
    (with-slots (parent x1 y1 x2 y2) record
      (if (= 1 (length (output-record-children record)))
	  (setf (values x1 y1 x2 y2) (bounding-rectangle* child))
	  (with-bounding-rectangle* (x1-child y1-child x2-child y2-child) child
	    (minf x1 x1-child)
	    (minf y1 y1-child)
	    (maxf x2 x2-child)
	    (maxf y2 y2-child)))
      (when parent
        (recompute-extent-for-changed-child parent record
					    old-x1 old-y1 old-x2 old-y2))))
  record)

(defmethod %tree-recompute-extent* ((record compound-output-record))
  ;; Internal helper function
  (let ((new-x1 0)
	(new-y1 0)
	(new-x2 0)
	(new-y2 0)
	(first-time t))
    (map-over-output-records
     (lambda (child)
       (if first-time
           (progn
             (multiple-value-setq (new-x1 new-y1 new-x2 new-y2)
               (bounding-rectangle* child))
             (setq first-time nil))
           (with-bounding-rectangle* (cx1 cy1 cx2 cy2) child
             (minf new-x1 cx1)
             (minf new-y1 cy1)
             (maxf new-x2 cx2)
             (maxf new-y2 cy2))))
     record)
    (if first-time
	(with-slots (x y) record
	  (values x y x y))
	(values new-x1 new-y1 new-x2 new-y2))))

(defmethod recompute-extent-for-changed-child
    ((record compound-output-record) changed-child
     old-min-x old-min-y old-max-x old-max-y)
  ;; If the child's old and new bbox lies entirely within the record's bbox
  ;; then no change need be made to the record's bbox.  Otherwise, if some part
  ;; of the child's bbox was on the record's bbox and is now inside, examine
  ;; all the children to determine the correct new bbox.
  (with-slots (x1 y1 x2 y2) record
    (with-bounding-rectangle* (child-x1 child-y1 child-x2 child-y2)
	changed-child
      (unless (and (> x1 old-min-x) (> x1 child-x1)
		   (> y1 old-min-y) (> y1 child-y1)
		   (< x2 old-max-x) (< x2 child-x2)
		   (< y2 old-max-y) (< y2 child-y2))
	;; Don't know if changed-child has been deleted or what, so go through
	;; all the children and construct the updated bbox.
	(setf (values x1 y1 x2 y2) (%tree-recompute-extent* record)))))
  record)

(defmethod recompute-extent-for-changed-child :around
    ((record compound-output-record) child
     old-min-x old-min-y old-max-x old-max-y)
  (declare (ignore child old-min-x old-min-y old-max-x old-max-y))
  (unless (slot-value record 'in-moving-p)
    (let ((old-rectangle (multiple-value-call #'make-bounding-rectangle
                           (bounding-rectangle* record))))
      (call-next-method)
      (with-slots (parent x1 y1 x2 y2) record
        (when (and parent (not (region-equal old-rectangle record)))
          (multiple-value-call #'recompute-extent-for-changed-child
            (values parent record)
            (bounding-rectangle* old-rectangle))))))
  record)

(defmethod tree-recompute-extent ((record compound-output-record))
  (with-slots (x1 y1 x2 y2) record
    (setf (values x1 y1 x2 y2) (%tree-recompute-extent* record)))
  record)

(defmethod tree-recompute-extent :around ((record compound-output-record))
  (let ((old-rectangle (multiple-value-call #'make-bounding-rectangle
                         (bounding-rectangle* record))))
    (call-next-method)
    (with-slots (parent x1 y1 x2 y2) record
      (when (and parent (not (region-equal old-rectangle record)))
        (recompute-extent-for-changed-child parent record x1 y1 x2 y2))))
  record)

;;; 16.3.1. Standard output record classes

(defclass standard-sequence-output-record (compound-output-record)
  ((children :initform (make-array 8 :adjustable t :fill-pointer 0)
	     :reader output-record-children)))

(defmethod add-output-record (child (record standard-sequence-output-record))
  (vector-push-extend child (output-record-children record))
  (setf (output-record-parent child) record))

(defmethod delete-output-record (child (record standard-sequence-output-record)
				 &optional (errorp t))
  (with-slots (children) record
    (let ((pos (position child children :test #'eq)))
      (if (null pos)
	  (when errorp
	    (error "~S is not a child of ~S" child record))
	  (progn
	    (setq children (replace children children
				    :start1 pos
				    :start2 (1+ pos)))
	    (decf (fill-pointer children))
            (setf (output-record-parent child) nil))))))

(defmethod clear-output-record ((record standard-sequence-output-record))
  (let ((children (output-record-children record)))
    (map 'nil (lambda (child) (setf (output-record-parent child) nil))
         children)
    (fill children nil)
    (setf (fill-pointer children) 0)))

(defmethod output-record-count ((record standard-sequence-output-record))
  (length (output-record-children record)))

(defmethod map-over-output-records
    (function (record standard-sequence-output-record)
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  "Applies FUNCTION to all children in the order they were added."
  (declare (ignore x-offset y-offset))
  (loop with children = (output-record-children record)
     for child across children
     do (apply function child function-args)))

(defmethod map-over-output-records-containing-position
    (function (record standard-sequence-output-record) x y
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  "Applies FUNCTION to children, containing (X,Y), in the reversed
order they were added."
  (declare (ignore x-offset y-offset))
  (loop with children = (output-record-children record)
     for i from (1- (length children)) downto 0
     for child = (aref children i)
     when (and (multiple-value-bind (min-x min-y max-x max-y)
                   (output-record-hit-detection-rectangle* child)
                 (and (<= min-x x max-x) (<= min-y y max-y)))
               (output-record-refined-position-test child x y))
     do (apply function child function-args)))

(defmethod map-over-output-records-overlapping-region
    (function (record standard-sequence-output-record) region
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  "Applies FUNCTION to children, overlapping REGION, in the order they
were added."
  (declare (ignore x-offset y-offset))
  (loop with children = (output-record-children record)
     for child across children
     when (region-intersects-region-p region child)
     do (apply function child function-args)))

;;; XXX bogus for now.
(defclass standard-tree-output-record (standard-sequence-output-record)
  (
   ))

(defmethod match-output-records ((record t) &rest args)
  (apply #'match-output-records-1 record args))

;;; Factor out the graphics state portions of the output records so
;;; they can be manipulated seperately e.g., by incremental
;;; display. The individual slots of a graphics state are factored into mixin
;;; classes so that each output record can capture only the state that it needs.
;;; -- moore

;;; It would be appealing to define a setf method, e.g. (setf
;;; medium-graphics-state), for setting a medium's state from a graphics state
;;; object, but that would require us to define a medium-graphics-state reader
;;; that would cons a state object.  I don't want to do that.

(defclass graphics-state ()
  ()
  (:documentation "Stores those parts of the medium/stream graphics state
  that need to be restored when drawing an output record"))

(defgeneric set-medium-graphics-state (state medium)
  (:documentation "Sets the MEDIUM graphics state from STATE"))

(defmethod set-medium-graphics-state (state medium)
  (declare (ignore medium))
  state)

(defmethod set-medium-graphics-state (state (stream output-recording-stream))
  (with-sheet-medium (medium stream)
    (set-medium-graphics-state state medium)))

(defclass gs-ink-mixin (graphics-state)
  ((ink :initarg :ink :accessor graphics-state-ink)))

(defmethod initialize-instance :after ((obj gs-ink-mixin)
				       &key (stream nil)
				       (medium (when stream
						 (sheet-medium stream))))
  (when (and medium (not (slot-boundp obj 'ink)))
    (setf (slot-value obj 'ink) (medium-ink medium))))

(defmethod set-medium-graphics-state :after ((state gs-ink-mixin) medium)
  (setf (medium-ink medium) (graphics-state-ink state)))

(defmethod match-output-records-1 and ((record gs-ink-mixin)
				       &key (ink nil inkp))
  (or (null inkp)
      (design-equalp (graphics-state-ink record) ink)))

(defclass gs-clip-mixin (graphics-state)
  ((clip :initarg :clipping-region :accessor graphics-state-clip
         :documentation "Clipping region in stream coordinates.")))


(defmethod initialize-instance :after ((obj gs-clip-mixin)
				       &key (stream nil)
				       (medium (when stream
						 (sheet-medium stream))))
  (when medium
    (with-slots (clip)
	obj
      (let ((clip-region (if (slot-boundp obj 'clip)
			     (region-intersection (medium-clipping-region
						   medium)
						  clip)
			     (medium-clipping-region medium))))
	(setq clip (transform-region (medium-transformation medium)
				     clip-region))))))

(defmethod set-medium-graphics-state :after ((state gs-clip-mixin) medium)
  ;;
  ;; This definition is kind of wrong. When output records are about to
  ;; be replayed only a certain region of the stream should be affected.[1]
  ;; Therefore I disabled this code, since this way only breaks the
  ;; [not very frequent case] that the output record actually contains
  ;; a clipping region different from +everywhere+, while having it in
  ;; breaks redisplay of streams in just about every case.
  ;;
  ;; Most notably Closure is affected by this, as it does the equivalent of
  ;; (draw-rectangle* medium 0 0 800 200 :ink +white+ :filled t)
  ;; (draw-text* medium "Hello" 100 100)
  ;;
  ;; Having this code in a redisplay on the region
  ;; (make-rectangle* 0 0 50 50) fills the drawing pane with a white
  ;; rectangle obscuring the text.
  ;;
  ;; [1] it is of course debatable where this extra clipping because
  ;; of redisplay should come from. Should replay-output-record set it
  ;; up? Should handle-repaint do so?
  ;;
  ;; --GB 2003-03-14
  #+nil
  (setf (medium-clipping-region medium) (graphics-state-clip state)))

(defmethod match-output-records-1 and ((record gs-clip-mixin)
				       &key (clip nil clipp))
  (or clipp
      (region-equal (graphics-state-clip record) clip)))

;;; 16.3.2. Graphics Displayed Output Records
(defclass standard-displayed-output-record (gs-clip-mixin gs-ink-mixin
					    basic-output-record
                                            displayed-output-record)
  ((ink :reader displayed-output-record-ink))
  (:documentation "Implementation class for DISPLAYED-OUTPUT-RECORD."))

(defclass gs-line-style-mixin (graphics-state)
  ((line-style :initarg :line-style :accessor graphics-state-line-style)))

(defmethod initialize-instance :after ((obj gs-line-style-mixin)
				       &key (stream nil)
				       (medium (when stream
						 (sheet-medium stream))))
  (when medium
    (unless (slot-boundp obj 'line-style)
      (setf (slot-value obj 'line-style) (medium-line-style medium)))))

(defmethod set-medium-graphics-state :after ((state gs-line-style-mixin) medium)
  (setf (medium-line-style medium) (graphics-state-line-style state)))

(defmethod match-output-records-1 and ((record gs-line-style-mixin)
				       &key (line-style nil line-style-p))
  (or (null line-style-p)
      (line-style-equalp (graphics-state-line-style record) line-style)))

(defgeneric graphics-state-line-style-border (record medium)
  (:method ((record gs-line-style-mixin) medium)
    (/ (line-style-effective-thickness (graphics-state-line-style record)
				        medium)
       2)))

(defclass gs-text-style-mixin (graphics-state)
  ((text-style :initarg :text-style :accessor graphics-state-text-style)))

(defmethod initialize-instance :after ((obj gs-text-style-mixin)
				       &key (stream nil)
				       (medium (when stream
						 (sheet-medium stream))))
  (when medium
    (unless (slot-boundp obj 'text-style)
      (setf (slot-value obj 'text-style) (medium-text-style medium)))))

(defmethod set-medium-graphics-state :after ((state gs-text-style-mixin) medium)
  (setf (medium-text-style medium) (graphics-state-text-style state)))

(defmethod match-output-records-1 and ((record gs-text-style-mixin)
				       &key (text-style nil text-style-p))
  (or (null text-style-p)
      (text-style-equalp (graphics-state-text-style record) text-style)))

(defclass standard-graphics-displayed-output-record
    (standard-displayed-output-record
     graphics-displayed-output-record)
  ())

(defmethod match-output-records-1 and
  ((record standard-displayed-output-record)
   &key (x1 nil x1-p) (y1 nil y1-p)
   (x2 nil x2-p) (y2 nil y2-p)
   (bounding-rectangle nil bounding-rectangle-p))
  (if bounding-rectangle-p
      (region-equal record bounding-rectangle)
      (multiple-value-bind (my-x1 my-y1 my-x2 my-y2)
	  (bounding-rectangle* record)
	(and (or (null x1-p) (coordinate= my-x1 x1))
	     (or (null y1-p) (coordinate= my-y1 y1))
	     (or (null x2-p) (coordinate= my-x2 x2))
	     (or (null y2-p) (coordinate= my-y2 y2))))))

;;; This is an around method so that more specific before methods can be
;;; defined for the various mixin classes, that modify the state after it has
;;; been set in the graphics state.

(defmethod replay-output-record :around
    ((record standard-displayed-output-record) stream
     &optional region x-offset y-offset)
  (declare (ignore region x-offset y-offset))
  (set-medium-graphics-state record stream)
  (call-next-method))

(defclass coord-seq-mixin ()
  ((coord-seq :accessor coord-seq :initarg :coord-seq))
  (:documentation "Mixin class that implements methods for records that contain
   sequences of coordinates."))

(defun coord-seq-bounds (coord-seq border)
  (let* ((min-x (elt coord-seq 0))
	 (min-y (elt coord-seq 1))
	 (max-x min-x)
	 (max-y min-y))
    (do-sequence ((x y) coord-seq)
      (minf min-x x)
      (minf min-y y)
      (maxf max-x x)
      (maxf max-y y))
    (values (- min-x border) (- min-y border)
	    (+ max-x border) (+ max-y border))))

(defmethod initialize-instance :after ((record coord-seq-mixin) &key)
  (let ((medium (sheet-medium (slot-value record 'stream))))
    (with-slots (coord-seq)
	record
      (setf coord-seq
	    (transform-position-sequence 'vector
					 (medium-transformation medium)
					 coord-seq)))))

;;; x1, y1 slots must exist in class...

(defmethod* (setf output-record-position) :around
    (nx ny (record coord-seq-mixin))
  (with-slots (x1 y1)
      record
    (let ((dx (- nx x1))
	  (dy (- ny y1))
	  (coords (slot-value record 'coord-seq)))
      (multiple-value-prog1
	  (call-next-method)
	(loop for i from 0 below (length coords) by 2
	      do (progn
		   (incf (aref coords i) dx)
		   (incf (aref coords (1+ i)) dy)))))))

(defmethod match-output-records-1 and ((record coord-seq-mixin)
				       &key (coord-seq nil coord-seq-p))
  (or (null coord-seq-p)
      (let* ((my-coord-seq (slot-value record 'coord-seq))
	     (len (length my-coord-seq)))
	(and (eql len (length coord-seq))
	     (loop for elt1 across my-coord-seq
		   for elt2 across coord-seq
		   always (coordinate= elt1 elt2))))))

;;; Do we need to save/restore graphics state in each call to
;;; replay-output-record, or could we do it only in replay?  I'd like to save
;;; state in a graphics state object, but I'm not going to allocate one in each
;;; recursive invocation of replay-output-record :P -- moore

(defmacro def-grecording (name ((&rest mixins) &rest args) &body body)
  (let ((method-name (symbol-concat '#:medium- name '*))
	(class-name (symbol-concat name '#:-OUTPUT-RECORD))
	(medium (gensym "MEDIUM"))
        (class-vars `((stream :initarg :stream)
                      ,@(loop for arg in args
                           collect `(,arg
                                     :initarg ,(intern (symbol-name arg)
                                                       :keyword)))))
        (arg-list (loop for arg in args
                     nconc `(,(intern (symbol-name arg) :keyword) ,arg))))
    `(progn
       (defclass ,class-name (,@mixins standard-graphics-displayed-output-record)
	 ,class-vars)
       (defmethod initialize-instance :after ((graphic ,class-name) &rest args)
	 (declare (ignore args))
	 (with-slots (x1 y1 x2 y2
		      stream ink clipping-region
		      line-style text-style ,@args)
	     graphic
           (let* ((medium (sheet-medium stream)))
             (multiple-value-setq (x1 y1 x2 y2) (progn ,@body)))))
       (defmethod ,method-name :around ((stream output-recording-stream) ,@args)
         ;; XXX STANDARD-OUTPUT-RECORDING-STREAM ^?
	 (with-sheet-medium (medium stream)
	   (when (stream-recording-p stream)
	     (let ((record (make-instance ',class-name
			     :stream stream
			     ,@arg-list)))
	       (stream-add-output-record stream record)))
	   (when (stream-drawing-p stream)
	     (call-next-method))))
       (defmethod replay-output-record ((record ,class-name) stream
					&optional (region +everywhere+)
                                        (x-offset 0) (y-offset 0))
         (declare (ignore x-offset y-offset region))
	 (with-slots (,@args) record
           (let ((,medium (sheet-medium stream))
                 ;; is sheet a sheet-with-medium-mixin? --GB
                 )
	     ;; Graphics state is set up in :around method.
	     (,method-name ,medium ,@args)))))))

(def-grecording draw-point ((gs-line-style-mixin) point-x point-y)
  (let ((border (graphics-state-line-style-border graphic medium)))
    (with-transformed-position ((medium-transformation medium) point-x point-y)
      (setf (slot-value graphic 'point-x) point-x
	    (slot-value graphic 'point-y) point-y)
      (values (- point-x border)
	      (- point-y border)
	      (+ point-x border)
	      (+ point-y border)))))

(defmethod* (setf output-record-position) :around
    (nx ny (record draw-point-output-record))
  (with-slots (x1 y1 point-x point-y)
      record
    (let ((dx (- nx x1))
	  (dy (- ny y1)))
      (multiple-value-prog1
	  (call-next-method)
	(incf point-x dx)
	(incf point-y dy)))))

(defmethod match-output-records-1 and ((record draw-point-output-record)
				       &key (point-x nil point-x-p)
				       (point-y nil point-y-p))
  (and (or (null point-x-p)
	   (coordinate= (slot-value record 'point-x) point-x))
       (or (null point-y-p)
	   (coordinate= (slot-value record 'point-y) point-y))))


(def-grecording draw-points ((coord-seq-mixin gs-line-style-mixin) coord-seq)
  ;; coord-seq has already been transformed
  (let ((border (graphics-state-line-style-border graphic medium)))
    (coord-seq-bounds coord-seq border)))

(def-grecording draw-line ((gs-line-style-mixin)
			   point-x1 point-y1 point-x2 point-y2)
  (let ((transform (medium-transformation medium))
	(border (graphics-state-line-style-border graphic medium)))
    (with-transformed-position (transform point-x1 point-y1)
      (with-transformed-position (transform point-x2 point-y2)
	(setf (slot-value graphic 'point-x1) point-x1
	      (slot-value graphic 'point-y1) point-y1
	      (slot-value graphic 'point-x2) point-x2
	      (slot-value graphic 'point-y2) point-y2)
	(values (- (min point-x1 point-x2) border)
		(- (min point-y1 point-y2) border)
		(+ (max point-x1 point-x2) border)
		(+ (max point-y1 point-y2) border))))))

(defmethod* (setf output-record-position) :around
    (nx ny (record draw-line-output-record))
  (with-slots (x1 y1
	       point-x1 point-y1 point-x2 point-y2)
      record
    (let ((dx (- nx x1))
	  (dy (- ny y1)))
      (multiple-value-prog1
	  (call-next-method)
	(incf point-x1 dx)
	(incf point-y1 dy)
	(incf point-x2 dx)
	(incf point-y2 dy)))))

(defmethod match-output-records-1 and ((record draw-line-output-record)
				       &key (point-x1 nil point-x1-p)
				       (point-y1 nil point-y1-p)
				       (point-x2 nil point-x2-p)
				       (point-y2 nil point-y2-p))
  (and (or (null point-x1-p)
	   (coordinate= (slot-value record 'point-x1) point-x1))
       (or (null point-y1-p)
	   (coordinate= (slot-value record 'point-y1) point-y1))
       (or (null point-x2-p)
	   (coordinate= (slot-value record 'point-x2) point-x2))
       (or (null point-y2-p)
	   (coordinate= (slot-value record 'point-y2) point-y2))))


(def-grecording draw-lines ((coord-seq-mixin gs-line-style-mixin) coord-seq)
  (let ((border (graphics-state-line-style-border graphic medium)))
    (coord-seq-bounds coord-seq border)))

;;; Helper function
(defun normalize-coords (dx dy &optional unit)
  (let ((norm (sqrt (+ (* dx dx) (* dy dy)))))
    (if unit
	(let ((scale (/ unit norm)))
	  (values (* dx scale) (* dy scale)))
	(values (/ dx norm) (/ dy norm)))))

(defun polygon-record-bounding-rectangle
    (coord-seq closed filled line-style border miter-limit)
  (cond (filled
	 (coord-seq-bounds coord-seq 0))
	((eq (line-style-joint-shape line-style) :round)
	 (coord-seq-bounds coord-seq border))
	(t (let* ((x1 (svref coord-seq 0))
		  (y1 (svref coord-seq 1))
		  (min-x x1)
		  (min-y y1)
		  (max-x x1)
		  (max-y y1)
		  (len (length coord-seq)))
	     (unless closed
	       (setq min-x (- x1 border)  min-y (- y1 border)
		     max-x (+ x1 border)  max-y (+ y1 border)))
	     ;; Setup for iterating over the coordinate vector.  If the polygon
	     ;; is closed deal with the extra segment.
	     (multiple-value-bind (initial-xp initial-yp
				   final-xn final-yn
				   initial-index final-index)
		 (if closed
		     (values (svref coord-seq (- len 2))
			     (svref coord-seq (- len 1))
			     x1 y1
			     0 (- len 2))
		     (values x1 y1
			     (svref coord-seq (- len 2))
			     (svref coord-seq (- len 1))
			     2 (- len 4)))
	       (ecase (line-style-joint-shape line-style)
		 (:miter
		  ;;FIXME: Remove successive positively proportional segments
		  (loop with sin-limit = (sin (* 0.5 miter-limit))
			and xn and yn
			for i from initial-index to final-index by 2
			for xp = initial-xp then x
			for yp = initial-yp then y
			for x = (svref coord-seq i)
			for y = (svref coord-seq (1+ i))
			do (setf (values xn yn)
				 (if (eql i final-index)
				     (values final-xn final-yn)
				     (values (svref coord-seq (+ i 2))
					     (svref coord-seq (+ i
								 3)))))
			   (multiple-value-bind (ex1 ey1)
			       (normalize-coords (- x xp) (- y yp))
			     (multiple-value-bind (ex2 ey2)
				 (normalize-coords (- x xn) (- y yn))
			       (let* ((cos-a (+ (* ex1 ex2) (* ey1 ey2)))
				      (sin-a/2 (sqrt (* 0.5 (- 1.0 cos-a)))))
				 (if (< sin-a/2 sin-limit)
				     (let ((nx (* border
						  (max (abs ey1) (abs ey2))))
					   (ny (* border
						  (max (abs ex1) (abs ex2)))))
				       (minf min-x (- x nx))
				       (minf min-y (- y ny))
				       (maxf max-x (+ x nx))
				       (maxf max-y (+ y ny)))
				     (let ((length (/ border sin-a/2)))
				       (multiple-value-bind (dx dy)
					   (normalize-coords (+ ex1 ex2)
							     (+ ey1 ey2)
							     length)
					 (minf min-x (+ x dx))
					 (minf min-y (+ y dy))
					 (maxf max-x (+ x dx))
					 (maxf max-y (+ y dy))))))))))
		 ((:bevel :none)
		  (loop with xn and yn
			for i from initial-index to final-index by 2
			for xp = initial-xp then x
			for yp = initial-yp then y
			for x = (svref coord-seq i)
			for y = (svref coord-seq (1+ i))
			do (setf (values xn yn)
				 (if (eql i final-index)
				     (values final-xn final-yn)
				     (values (svref coord-seq (+ i 2))
					     (svref coord-seq (+ i
								 3)))))
			   (multiple-value-bind (ex1 ey1)
			       (normalize-coords (- x xp) (- y yp))
			     (multiple-value-bind (ex2 ey2)
				 (normalize-coords (- x xn) (- y yn))
			       (let ((nx (* border (max (abs ey1) (abs ey2))))
				     (ny (* border (max (abs ex1) (abs ex2)))))
				 (minf min-x (- x nx))
				 (minf min-y (- y ny))
				 (maxf max-x (+ x nx))
				 (maxf max-y (+ y ny))))))))
	       (unless closed
		 (multiple-value-bind (x y)
		     (values (svref coord-seq final-index)
			     (svref coord-seq (1+ final-index)))
		   (minf min-x (- x border))
		   (minf min-y (- y border))
		   (maxf max-x (+ x border))
		   (maxf max-y (+ y border)))))
	     (values min-x min-y max-x max-y)))))

(def-grecording draw-polygon ((coord-seq-mixin gs-line-style-mixin)
			      coord-seq closed filled)
  (let ((border (graphics-state-line-style-border graphic medium)))
    (polygon-record-bounding-rectangle
     coord-seq closed filled line-style border (medium-miter-limit medium))))

(defmethod match-output-records-1 and ((record draw-polygon-output-record)
				       &key (closed nil closedp)
				       (filled nil filledp))
  (and (or (null closedp)
	   (eql (slot-value record 'closed) closed))
       (or (null filledp)
	   (eql (slot-value record 'filled) filled))))

(def-grecording draw-rectangle ((gs-line-style-mixin)
				left top right bottom filled)
  (let ((border (graphics-state-line-style-border graphic medium)))
    (polygon-record-bounding-rectangle
     (vector left top left bottom right bottom right top)
     t filled line-style border
     (medium-miter-limit medium))))

(defmethod* (setf output-record-position) :around
    (nx ny (record draw-rectangle-output-record))
  (with-slots (x1 y1
	       left top right bottom)
      record
    (let ((dx (- nx x1))
	  (dy (- ny y1)))
      (multiple-value-prog1
	  (call-next-method)
	(incf left dx)
	(incf top dy)
	(incf right dx)
	(incf bottom dy)))))

(defmethod match-output-records-1 and ((record draw-rectangle-output-record)
				       &key (left nil leftp)
				       (top nil topp)
				       (right nil rightp)
				       (bottom nil bottomp)
				       (filled nil filledp))
  (and (or (null leftp)
	   (coordinate= (slot-value record 'left) left))
       (or (null topp)
	   (coordinate= (slot-value record 'top) top))
       (or (null rightp)
	   (coordinate= (slot-value record 'right) right))
       (or (null bottomp)
	   (coordinate= (slot-value record 'bottom) bottom))
       (or (null filledp)
	   (eql (slot-value record 'filled) filled))))

(def-grecording draw-ellipse ((gs-line-style-mixin)
			      center-x center-y
			      radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			      start-angle end-angle filled)
  (multiple-value-bind (min-x min-y max-x max-y)
      (bounding-rectangle* (make-ellipse* center-x center-y
					  radius-1-dx radius-1-dy
					  radius-2-dx radius-2-dy
					  :start-angle start-angle
					  :end-angle end-angle))
    (if filled
        (values min-x min-y max-x max-y)
	(let ((border (graphics-state-line-style-border graphic medium)))
	  (values (- min-x border)
		  (- min-y border)
		  (+ max-x border)
		  (+ max-y border))))))

(defmethod* (setf output-record-position) :around
    (nx ny (record draw-ellipse-output-record))
  (with-slots (x1 y1 center-x center-y)
      record
    (let ((dx (- nx x1))
	  (dy (- ny y1)))
      (multiple-value-prog1
	  (call-next-method)
	(incf center-x dx)
	(incf center-y dy)))))

(defmethod match-output-records-1 and ((record draw-ellipse-output-record)
				       &key (center-x nil center-x-p)
				       (center-y nil center-y-p))
  (and (or (null center-x-p)
	   (coordinate= (slot-value record 'center-x) center-x))
       (or (null center-y-p)
	   (coordinate= (slot-value record 'center-y) center-y))))

(def-grecording draw-text ((gs-text-style-mixin) string point-x point-y start end
			   align-x align-y toward-x toward-y transform-glyphs)
  ;; FIXME!!! Text direction.
  ;; Multiple lines?
 (let* ((text-style (graphics-state-text-style graphic))
	(width (stream-string-width stream string
                                    :start start :end end
                                    :text-style text-style))
        (ascent (text-style-ascent text-style (sheet-medium stream)))
        (descent (text-style-descent text-style (sheet-medium stream)))
        (height (+ ascent descent))
        left top right bottom)
   (ecase align-x
     (:left (setq left point-x
                  right (+ point-x width)))
     (:right (setq left (- point-x width)
                   right point-x))
     (:center (setq left (- point-x (round width 2))
                    right (+ point-x (round width 2)))))
   (ecase align-y
     (:baseline (setq top (- point-y ascent)
                      bottom (+ point-y descent)))
     (:top (setq top point-y
                 bottom (+ point-y height)))
     (:bottom (setq top (- point-y height)
                    bottom point-y))
     (:center (setq top (- point-y (floor height 2))
                    bottom (+ point-y (ceiling height 2)))))
   (values left top right bottom)))

(defmethod* (setf output-record-position) :around
    (nx ny (record draw-text-output-record))
  (with-slots (x1 y1 point-x point-y toward-x toward-y)
      record
    (let ((dx (- nx x1))
	  (dy (- ny y1)))
      (multiple-value-prog1
	  (call-next-method)
	(incf point-x dx)
	(incf point-y dy)
	(incf toward-x dx)
	(incf toward-y dy)))))

(defmethod match-output-records-1 and ((record draw-text-output-record)
				       &key (string nil stringp)
				       (start nil startp)
				       (end nil endp)
				       (point-x nil point-x-p)
				       (point-y nil point-y-p)
				       (align-x nil align-x-p)
				       (align-y nil align-y-p)
				       (toward-x nil toward-x-p)
				       (toward-y nil toward-y-p)
				       (transform-glyphs nil
							 transform-glyphs-p))
   (and (or (null stringp)
	    (string= (slot-value record 'string) string))
	(or (null startp)
	    (eql (slot-value record 'start) start))
	(or (null endp)
	    (eql (slot-value record 'end) end))
	(or (null point-x-p)
	    (coordinate= (slot-value record 'point-x) point-x))
	(or (null point-y-p)
	    (coordinate= (slot-value record 'point-y) point-y))
	(or (null align-x-p)
	    (eq (slot-value record 'align-x) align-x))
	(or (null align-y-p)
	    (eq (slot-value record 'align-y) align-y))
	(or (null toward-x-p)
	    (coordinate= (slot-value record 'toward-x) toward-x))
	(or (null toward-y-p)
	    (coordinate= (slot-value record 'toward-y) toward-y))
	(or (null transform-glyphs-p)
	    (eq (slot-value record 'transform-glyphs) transform-glyphs))))

;;; 16.3.3. Text Displayed Output Record
(defvar *drawing-options* (list +foreground-ink+ +everywhere+)
  "The ink and the clipping region of the current stream.") ; XXX TDO

(defclass styled-string (gs-text-style-mixin gs-clip-mixin gs-ink-mixin)
  ((start-x :initarg :start-x)
   (string :initarg :string :reader styled-string-string)))

(defclass standard-text-displayed-output-record
    (text-displayed-output-record standard-displayed-output-record)
  ((initial-x1 :initarg :start-x)
   (initial-y1 :initarg :start-y)
   (strings :initform nil)
   (baseline :initform 0)
   (width :initform 0)
   (max-height :initform 0)
   (start-x :initarg :start-x)
   (start-y :initarg :start-y)
   (end-x :initarg :start-x)
   (end-y :initarg :start-y)
   (wrapped :initform nil
	    :accessor text-record-wrapped)
   (medium :initarg :medium :initform nil)))

(defmethod initialize-instance :after
    ((obj standard-text-displayed-output-record) &key stream)
  (when stream
    (setf (slot-value obj 'medium) (sheet-medium stream))))

(defmethod print-object ((self standard-text-displayed-output-record) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (with-slots (start-x start-y strings) self
      (format stream "~D,~D ~S"
              start-x start-y
              (mapcar #'styled-string-string strings)))))

(defmethod* (setf output-record-position) :before
    (nx ny (record standard-text-displayed-output-record))
  (with-slots (x1 y1 start-x start-y end-x end-y strings) record
    (let ((dx (- nx x1))
          (dy (- ny y1)))
      (incf start-x dx)
      (incf start-y dy)
      (incf end-x dx)
      (incf end-y dy)
      (loop for s in strings
	    do (incf (slot-value s 'start-x) dx)))))

(defmethod replay-output-record ((record standard-text-displayed-output-record)
				 stream
				 &optional region (x-offset 0) (y-offset 0))
  (declare (ignore region x-offset y-offset))
  (with-slots (strings baseline max-height start-y wrapped x1 y1)
      record
    (with-sheet-medium (medium stream) ;is sheet a sheet-with-medium-mixin? --GB
      ;; FIXME:
      ;; 1. SLOT-VALUE...
      ;; 2. It should also save a "current line".
      (setf (slot-value stream 'baseline) baseline)
      (loop for substring in strings
	    do (with-slots (start-x string)
		   substring
		 (setf (stream-cursor-position stream)
		       (values start-x start-y))
		 (set-medium-graphics-state substring medium)
		 (stream-write-line stream string)))
      (when wrapped			; FIXME
	(draw-rectangle* medium
			 (+ wrapped 0) start-y
			 (+ wrapped 4) (+ start-y max-height)
			 :ink +foreground-ink+
			 :filled t)))))

(defmethod output-record-start-cursor-position
    ((record standard-text-displayed-output-record))
  (with-slots (start-x start-y) record
    (values start-x start-y)))

(defmethod output-record-end-cursor-position
    ((record standard-text-displayed-output-record))
  (with-slots (end-x end-y) record
    (values end-x end-y)))

(defmethod tree-recompute-extent
    ((text-record standard-text-displayed-output-record))
  (with-slots (parent x1 y1 x2 y2 width max-height) text-record
              (setq x2 (coordinate (+ x1 width))
                    y2 (coordinate (+ y1 max-height))))
  text-record)

(defmethod add-character-output-to-text-record ; XXX OAOO with ADD-STRING-...
    ((text-record standard-text-displayed-output-record)
     character text-style char-width height new-baseline)
  (with-slots (strings baseline width max-height start-y end-x end-y medium)
      text-record
    (if (and strings
	     (let ((string (last1 strings)))
	       (match-output-records string
				     :text-style text-style
				     :ink (medium-ink medium)
				     :clipping-region (medium-clipping-region
						       medium))))
	(vector-push-extend character (slot-value (last1 strings) 'string))
	(nconcf strings
		(list (make-instance
		       'styled-string
		       :start-x end-x
		       :text-style text-style
		       :medium medium	; pick up ink and clipping region
		       :string (make-array 1 :initial-element character
					   :element-type 'character
					   :adjustable t
					   :fill-pointer t)))))
    (setq baseline (max baseline new-baseline)
	  end-x (+ end-x char-width)
	  max-height (max max-height height)
	  end-y (max end-y (+ start-y max-height))
	  width (+ width char-width)))
  (tree-recompute-extent text-record))

(defmethod add-string-output-to-text-record
    ((text-record standard-text-displayed-output-record)
     string start end text-style string-width height new-baseline)
  (if end
      (setq end (min end (length string)))
      (setq end (length string)))
  (let ((length (max 0 (- end start))))
    (cond
      ((eql length 1)
       (add-character-output-to-text-record text-record
                                            (aref string start)
                                            text-style
                                            string-width height new-baseline))
      (t (with-slots (strings baseline width max-height start-y end-x end-y
		      medium)
	     text-record
	   (let ((styled-string (make-instance
				 'styled-string
				 :start-x end-x
				 :text-style text-style
				 :medium medium
				 :string (make-array (length string)
						     :element-type 'character
						     :adjustable t
						     :fill-pointer t))))
	     (nconcf strings (list styled-string))
	     (replace (styled-string-string styled-string) string
		      :start2 start :end2 end))
	   (setq baseline (max baseline new-baseline)
		 end-x (+ end-x string-width)
		 max-height (max max-height height)
		 end-y (max end-y (+ start-y max-height))
		 width (+ width string-width)))
       (tree-recompute-extent text-record)))))

(defmethod text-displayed-output-record-string
    ((record standard-text-displayed-output-record))
  (with-output-to-string (result)
    (with-slots (strings) record
      (loop for (nil nil substring) in strings
         do (write-string substring result)))))

;;; 16.3.4. Top-Level Output Records
(defclass stream-output-history-mixin ()
  ())

(defclass standard-sequence-output-history
    (standard-sequence-output-record stream-output-history-mixin)
  ())

(defclass standard-tree-output-history
    (standard-tree-output-record stream-output-history-mixin)
  ())

;;; 16.4. Output Recording Streams
(defclass standard-output-recording-stream (output-recording-stream)
  ((recording-p :initform t :reader stream-recording-p)
   (drawing-p :initform t :accessor stream-drawing-p)
   (output-history :initform (make-instance 'standard-tree-output-history)
                   :reader stream-output-history)
   (current-output-record :accessor stream-current-output-record)
   (current-text-output-record :initform nil
                               :accessor stream-current-text-output-record)
   (local-record-p :initform t
                   :documentation "This flag is used for dealing with streams outputting strings char-by-char.")))

(defmethod initialize-instance :after
    ((stream standard-output-recording-stream) &rest args)
  (declare (ignore args))
  (setf (stream-current-output-record stream) (stream-output-history stream)))

;;; Used in initializing clim-stream-pane

(defmethod reset-output-history ((stream
				  standard-output-recording-stream))
  (setf (slot-value stream 'output-history)
	(make-instance 'standard-tree-output-history))
  (setf (stream-current-output-record stream) (stream-output-history stream)))

;;; 16.4.1 The Output Recording Stream Protocol
(defmethod (setf stream-recording-p)
    (recording-p (stream standard-output-recording-stream))
  (let ((old-val (slot-value stream 'recording-p)))
    (setf (slot-value stream 'recording-p) recording-p)
    (when (not (eq old-val recording-p))
      (stream-close-text-output-record stream))
    recording-p))

(defmethod stream-add-output-record
    ((stream standard-output-recording-stream) record)
  (add-output-record record (stream-current-output-record stream)))

(defmethod stream-replay
    ((stream standard-output-recording-stream) &optional region)
  (replay (stream-output-history stream) stream region))

(defun output-record-ancestor-p (ancestor child)
  (loop for record = child then parent
     for parent = (output-record-parent record)
     when (eq parent nil) do (return nil)
     when (eq parent ancestor) do (return t)))

(defmethod erase-output-record (record (stream standard-output-recording-stream)
                                &optional (errorp t))
  (letf (((stream-recording-p stream)  nil))
    (let ((region (bounding-rectangle record)))
      (with-bounding-rectangle* (x1 y1 x2 y2) region
        (if (output-record-ancestor-p (stream-output-history stream) record)
            (progn
              (delete-output-record record (output-record-parent record))
              (with-output-recording-options (stream :record nil)
                (draw-rectangle* stream x1 y1 x2 y2 :ink +background-ink+))
              (stream-replay stream region))
            (when errorp
              (error "~S is not contained in ~S." record stream)))))))

(defun copy-textual-output-history (window stream &optional region record)
  ;; FIXME
  (declare (ignore window stream region record))
  (error "Not implemented."))

;;; 16.4.3. Text Output Recording
(defmethod stream-text-output-record
    ((stream standard-output-recording-stream) text-style)
  (declare (ignore text-style))
  (let ((record (stream-current-text-output-record stream)))
    (unless (and record (typep record 'standard-text-displayed-output-record))
      (multiple-value-bind (cx cy) (stream-cursor-position stream)
        (setf record (make-instance 'standard-text-displayed-output-record
                                    :x-position cx :y-position cy
                                    :start-x cx :start-y cy
				    :stream stream)
              (stream-current-text-output-record stream) record)))
    record))

(defmethod stream-close-text-output-record
    ((stream standard-output-recording-stream))
  (let ((record (stream-current-text-output-record stream)))
    (when record
      (setf (stream-current-text-output-record stream) nil)
      #|record stream-current-cursor-position to (end-x record) - already done|#
      (stream-add-output-record stream record))))

(defmethod stream-add-character-output
    ((stream standard-output-recording-stream)
     character text-style width height baseline)
  (add-character-output-to-text-record
   (stream-text-output-record stream text-style)
   character text-style width height baseline))

(defmethod stream-add-string-output ((stream standard-output-recording-stream)
                                     string start end text-style
                                     width height baseline)
  (add-string-output-to-text-record (stream-text-output-record stream
                                                               text-style)
                                    string start end text-style
                                    width height baseline))

;;; Text output catching methods
(defmacro without-local-recording (stream &body body)
  `(letf (((slot-value ,stream 'local-record-p) nil))
     ,@body))

(defmethod stream-write-line :around
    ((stream standard-output-recording-stream) line)
  (when (and (stream-recording-p stream)
             (slot-value stream 'local-record-p))
    (let* ((medium (sheet-medium stream))
           (text-style (medium-text-style medium))
           (*drawing-options* (list (medium-ink medium) ; XXX TDO
                                    (medium-clipping-region medium))))
      (stream-add-string-output stream line 0 nil text-style
                                (stream-string-width stream line
                                                     :text-style text-style)
                                (text-style-height text-style medium)
                                (text-style-ascent text-style medium))))
  (when (stream-drawing-p stream)
    (without-local-recording stream
                             (call-next-method))))

#+nil
(defmethod stream-write-char :around ((stream standard-output-recording-stream) char)
  (when (and (stream-recording-p stream)
             (slot-value stream 'local-record-p))
    (if (or (eql char #\return)

        (stream-close-text-output-record stream)
      (let* ((medium (sheet-medium stream))
             (text-style (medium-text-style medium)))
        (stream-add-character-output stream char text-style
                                     (stream-character-width stream char :text-style text-style)
                                     (text-style-height text-style medium)
                                     (text-style-ascent text-style medium)))))
  (without-local-recording stream
                           (call-next-method))))

#+nil
(defmethod stream-write-string :around ((stream standard-output-recording-stream) string
                                        &optional (start 0) end)
  ;; Problem: it is necessary to check for line wrapping. Now the
  ;; default method for STREAM-WRITE-STRING do char-by-char output,
  ;; therefore STREAM-WRITE-CHAR can do the right thing.
  (when (and (stream-recording-p stream)
             (slot-value stream 'local-record-p))
    (let* ((medium (sheet-medium stream))
           (text-style (medium-text-style medium)))
      (stream-add-string-output stream string start end text-style
                                (stream-string-width stream string
                                                     :start start :end end
                                                     :text-style text-style)
                                (text-style-height text-style medium)
                                (text-style-ascent text-style medium))))
  (without-local-recording stream
                           (call-next-method)))


(defmethod stream-finish-output :after ((stream standard-output-recording-stream))
  (stream-close-text-output-record stream))

(defmethod stream-force-output :after ((stream standard-output-recording-stream))
  (stream-close-text-output-record stream))

(defmethod stream-terpri :after ((stream standard-output-recording-stream))
  (stream-close-text-output-record stream))

(defmethod* (setf stream-cursor-position) :after (x y (stream standard-output-recording-stream))
	    (declare (ignore x y))
  (stream-close-text-output-record stream))

;(defmethod stream-set-cursor-position :after ((stream standard-output-recording-stream))
;  (stream-close-text-output-record stream))

(defmethod stream-wrap-line :before ((stream standard-output-recording-stream))
  (when (stream-recording-p stream)
    (setf (text-record-wrapped (stream-text-output-record stream nil)) ; FIXME!
          (stream-text-margin stream))))

;;; 16.4.4. Output Recording Utilities

(defmethod invoke-with-output-recording-options
  ((stream output-recording-stream) continuation record draw)
  "Calls CONTINUATION on STREAM enabling or disabling recording and drawing
according to the flags RECORD and DRAW."
  (letf (((stream-recording-p stream) record)
	 ((stream-drawing-p stream) draw))
    (funcall continuation stream)))

(defmethod invoke-with-new-output-record ((stream output-recording-stream)
                                          continuation record-type
                                          &rest initargs
                                          &key
					  &allow-other-keys)
  (stream-close-text-output-record stream)
  (let ((new-record (apply #'make-instance record-type initargs)))
    (letf (((stream-current-output-record stream) new-record))
      ;; Should we switch on recording? -- APD
      (funcall continuation stream new-record)
      (finish-output stream))
    (stream-add-output-record stream new-record)
    new-record))

(defmethod invoke-with-output-to-output-record
    ((stream output-recording-stream) continuation record-type
     &rest initargs
     &key
     &allow-other-keys)
  (stream-close-text-output-record stream)
  (let ((new-record (apply #'make-instance record-type initargs)))
    (with-output-recording-options (stream :record t :draw nil)
      (letf (((stream-current-output-record stream) new-record)
             ((stream-cursor-position stream) (values 0 0)))
        (funcall continuation stream new-record)
        (finish-output stream)))
    new-record))

(defmethod make-design-from-output-record (record)
  ;; FIXME
  (declare (ignore record))
  (error "Not implemented."))


;;; Additional methods
(defmethod scroll-vertical :around ((stream output-recording-stream) dy)
  (declare (ignore dy))
  (with-output-recording-options (stream :record nil)
    (call-next-method)))

(defmethod scroll-horizontal :around ((stream output-recording-stream) dx)
  (declare (ignore dx))
  (with-output-recording-options (stream :record nil)
    (call-next-method)))

(defmethod handle-repaint ((stream output-recording-stream) region)
  (stream-replay stream region))

