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
;;; - Scrolling does not work correctly. Region is given in "window" coordinates,
;;;   without bounding-rectangle-position transformation.
;;; - Redo setf*-output-record-position, extent recomputation for
;;;   compound records
;;; - How to deal with mixing of positioning/modifying?
;;; - When DRAWING-P is NIL, should stream cursor move?
;;; - OUTPUT-RECORD is a protocol class, it should not have any slots/methods.

(in-package :CLIM-INTERNALS)

;;; Should we blow off standard-bounding-rectangle and implement the
;;; bounding rectangle protocol ourselves?  Or use x1,y1 from
;;; standard-bounding-rectangle as our position?

(defclass basic-output-record (bounding-rectangle)
  ()
  (:documentation "Internal protocol class for common elements of output-record
 and displayed-output-record"))

(defclass basic-output-record-mixin (standard-bounding-rectangle
				     basic-output-record)
  ((x :initarg :x-position
      :initform 0
      :type rational)
   (y :initarg :y-position
      :initform 0
      :type rational)
   (parent :initarg :parent
	   :initform nil
           :reader output-record-parent))
  (:documentation "Implementation class for the Basic Output Record Protocol"))

(defmethod initialize-instance :after ((record basic-output-record-mixin)
				       &rest args)
  (declare (ignore args))
  (with-slots (x y x1 y1 x2 y2) record
    (setq x1 x
	  y1 y
	  x2 x
	  y2 y)))

(defclass output-record (basic-output-record)
  ())

(defun output-record-p (x)
  (typep x 'output-record))

(defclass output-record-mixin (basic-output-record-mixin output-record)
  ()
  (:documentation "Implementation class for output records i.e., those records
 that have children."))

(defclass displayed-output-record (basic-output-record)
  ())

(defclass displayed-output-record-mixin (basic-output-record-mixin
					 displayed-output-record)
  ((ink :initarg :ink :reader displayed-output-record-ink)
   (initial-x1 :initarg :initial-x1)
   (initial-y1 :initarg :initial-y1))
  (:documentation "Implementation class for displayed-output-record."))

(defun displayed-output-record-p (x)
  (typep x 'displayed-output-record))

; 16.2.1. The Basic Output Record Protocol
#+:cmu(declaim (ftype (function (output-record) (values rational rational))
		      output-record-position))
(defgeneric output-record-position (record)
  (:documentation
   "Returns the x and y position of RECORD. The position is the
position of the upper-left corner of its bounding rectangle. The
position is relative to the stream, where (0,0) is (initially) the
upper-left corner of the stream."))

(defgeneric* (setf output-record-position) (x y record))

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
   "Returns the output record that is the parent of RECORD, or nil if
RECORD has no parent."))

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

; 16.2.2. Output Record "Database" Protocol

(defgeneric output-record-children (record))

(defgeneric add-output-record (child record))

(defgeneric delete-output-record (child record &optional (errorp t)))

(defgeneric clear-output-record (record))

(defgeneric output-record-count (record))

(defgeneric map-over-output-records-containing-position
  (function record x y &optional x-offset y-offset &rest function-args))

(defgeneric map-over-output-records-overlapping-region
  (function record region &optional x-offset y-offset &rest function-args))

;;; From the Franz CLIM user's guide but not in the spec... clearly necessary.
(defgeneric map-over-output-records
    (continuation record &optional x-offset y-offset &rest continuation-args))

; 16.2.3. Output Record Change Notification Protocol

(defgeneric recompute-extent-for-new-child (record child))

(defgeneric recompute-extent-for-changed-child
  (record child old-min-x old-min-y old-max-x old-max-y))

(defgeneric tree-recompute-extent (record))

;;; Methods

(defmethod output-record-position ((record basic-output-record-mixin))
  (with-slots (x y) record
    (values x y)))

(defvar *suppress-notify-parent* nil
  "When t, don't notify the parent of a change in an output record's
   bounding rectangle.")

(defmethod* (setf output-record-position)
    (nx ny (record basic-output-record-mixin))
  (with-slots (x y x1 y1 x2 y2) record
    (let ((dx (- nx x))
          (dy (- ny y)))
      (incf x1 dx) (incf y1 dy)
      (incf x2 dx) (incf y2 dy))
    (setq x nx
          y ny)))

(defmethod* (setf output-record-position) :before
    (nx ny (record output-record))
  (let ((*suppress-notify-parent* t))
    (multiple-value-bind (old-x old-y) (output-record-position record)
      (let ((dx (- nx old-x))
	    (dy (- ny old-y)))
	(map-over-output-records
	 #'(lambda (child)
	     (multiple-value-bind (x y) (output-record-position child)
	       (setf (output-record-position child)
		     (values (+ x dx) (+ y dy)))))
	 record)))))

(defmethod* (setf output-record-position) :around
    (nx ny (record basic-output-record))
  (declare (ignore nx ny))
  (if *suppress-notify-parent*
      (call-next-method)
      (with-bounding-rectangle* (min-x min-y max-x max-y) record
	(call-next-method)
	(let ((parent (output-record-parent record)))
	  (when parent
	    (recompute-extent-for-changed-child parent record
						min-x min-y max-x max-y))))))


(defmethod output-record-start-cursor-position ((record basic-output-record))
  (values nil nil))

(defmethod* (setf output-record-start-cursor-position)
    (x y (record basic-output-record))
  (declare (ignore x y))
  nil)

(defmethod output-record-end-cursor-position ((record basic-output-record))
  (values nil nil))

(defmethod* (setf output-record-end-cursor-position)
    (x y (record basic-output-record))
  (declare (ignore x y))
  nil)

(defun replay (record stream &optional region)
  (stream-close-text-output-record stream)
  (when (stream-drawing-p stream)
    (with-cursor-off stream
      (multiple-value-bind (cx cy) (stream-cursor-position stream)
        (unwind-protect
             (letf (((stream-recording-p stream) nil))
               (replay-output-record record stream region))
          (setf (stream-cursor-position stream) (values cx cy)))))))

(defmethod replay-output-record ((record output-record) stream
				 &optional region (x-offset 0) (y-offset 0))
  (when (null region)
    (setq region +everywhere+))
  (map-over-output-records-overlapping-region
   #'replay-output-record record region x-offset y-offset
   stream region x-offset y-offset))

;;; XXX ? should this be defined on output-record at all?
;;; Probably not -- moore
(defmethod erase-output-record ((record output-record) stream
                                &optional (errorp t))
  (declare (ignore stream errorp))
  nil)

(defmethod output-record-hit-detection-rectangle*
    ((record basic-output-record))
  (bounding-rectangle* record))

(defmethod output-record-refined-position-test ((record basic-output-record)
						x y)
  (declare (ignore x y))
  t)

;;; XXX Should this only be defined on recording streams?
(defmethod highlight-output-record ((record basic-output-record-mixin)
				    stream state)
  (multiple-value-bind (x1 y1 x2 y2)
      (output-record-hit-detection-rectangle* record)
    (ecase state
      (:highlight
       (draw-rectangle* (sheet-medium stream) x1 y1 x2 y2
                        :filled nil :ink +foreground-ink+))
      (:unhighlight
       (draw-rectangle* (sheet-medium stream) x1 y1 x2 y2
                        :filled nil :ink +background-ink+)))))

(defclass standard-sequence-output-record (output-record-mixin)
  ((children :initform (make-array 8 :adjustable t :fill-pointer 0)
	     :reader output-record-children)))

;;; XXX bogus for now.
(defclass standard-tree-output-record (standard-sequence-output-record)
  (
   ))

#+nil
(defmethod* (setf output-record-position)
    (nx ny (record standard-sequence-output-record))
  (with-slots (x y) record
    (setq x nx
          y ny)))

(defmethod add-output-record (child (record standard-sequence-output-record))
  (with-slots (children) record
    (vector-push-extend child children))
  (with-slots (parent) child
    (setf parent record)))

(defmethod add-output-record :before (child (record output-record-mixin))
  (when (zerop (output-record-count record)) 
    (with-slots (x1 y1 x2 y2) record
      (setf (values x1 y1 x2 y2) (bounding-rectangle* child)))))

(defmethod add-output-record :after (child (record output-record))
  (recompute-extent-for-new-child record child))

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
	    (decf (fill-pointer children)))))))

(defmethod delete-output-record :after (child (record output-record-mixin)
					&optional (errorp t))
  (declare (ignore errorp))
  (with-bounding-rectangle* (x1 y1 x2 y2) child
    (recompute-extent-for-changed-child record child x1 y1 x2 y2)))

(defmethod clear-output-record ((record standard-sequence-output-record))
  (with-slots (children x1 y1 x2 y2) record
    (fill children nil)
    (setf (fill-pointer children) 0)
    (setq x2 x1
	  y2 y1)))

(defmethod output-record-count ((record standard-sequence-output-record))
  (length (output-record-children record)))

(defmethod map-over-output-records
    (function (record standard-sequence-output-record)
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  (declare (ignore x-offset y-offset))
  (loop for child across (output-record-children record)
	do (apply function child function-args)))

;;; This needs to work in "most recently added first" order, which I
;;; didn't know until recently :) -- moore
(defmethod map-over-output-records-containing-position
    (function (record standard-sequence-output-record) x y
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  (declare (ignore x-offset y-offset))
  (with-slots (children) record
    (loop for i from (1- (length children)) downto 0
	  for child = (aref children i)
	  when (and (multiple-value-bind (min-x min-y max-x max-y)
			(output-record-hit-detection-rectangle* child)
		      (and (<= min-x x max-x) (<= min-y y max-y)))
		    (output-record-refined-position-test child x y))
	  do (apply function child function-args))))

(defmethod map-over-output-records-overlapping-region
    (function (record standard-sequence-output-record) region
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  (declare (ignore x-offset y-offset))
  (loop for child across (output-record-children record)
        do (when (region-intersects-region-p region child)
             (apply function child function-args))))

;;; If the child is the only child of record, the record's bounding rectangle
;;; is set to the child's.
(defmethod recompute-extent-for-new-child
    ((record standard-sequence-output-record) child)
  (with-bounding-rectangle* (old-x1 old-y1 old-x2 old-y2) record
    (with-slots (parent children x1 y1 x2 y2) record
      (if (eql 1 (length children))
	  (setf (values x1 y1 x2 y2) (bounding-rectangle* child))
	  (with-bounding-rectangle* (x1-child y1-child x2-child y2-child) child
	    (minf x1 x1-child)
	    (minf y1 y1-child)
	    (maxf x2 x2-child)
	    (maxf y2 y2-child)))
      (when parent
        (recompute-extent-for-changed-child parent record
					    old-x1 old-y1 old-x2 old-y2)))))

(defmethod recompute-extent-for-changed-child :around
    ((record basic-output-record-mixin) child
     old-min-x old-min-y old-max-x old-max-y)
  (declare (ignore child old-min-x old-min-y old-max-x old-max-y))
  (let ((old-rectangle (multiple-value-call #'make-bounding-rectangle
                         (bounding-rectangle* record))))
    (call-next-method)
    (with-slots (parent x1 y1 x2 y2) record
      (when (and parent (not (region-equal old-rectangle record)))
      	(recompute-extent-for-changed-child parent record x1 y1 x2 y2)))))

;; Internal helper function
(defmethod %tree-recompute-extent* ((record output-record))
  (let ((new-x1 0)
	(new-y1 0)
	(new-x2 0)
	(new-y2 0)
	(first-time t))
    (map-over-output-records
     #'(lambda (child)
	 (if first-time
	     (progn
	       (setf (values new-x1 new-y1 new-x2 new-y2)
		     (bounding-rectangle* child))
	       (setq first-time nil))
	     (with-bounding-rectangle* (cx1 cy1 cx2 cy2) child
	       (minf new-x1 cx1)
	       (minf new-y1 cy1)
	       (maxf new-x2 cx2)
	       (maxf new-y2 cy2))))
     record)
    ;; If we don't have any children, collapse the bbox to the min point.
    (if first-time
	(multiple-value-bind (x1 y1)
	    (output-record-position record)
	  (values x1 y1 x1 y1))
	(values new-x1 new-y1 new-x2 new-y2))))

(defmethod recompute-extent-for-changed-child
    ((record output-record-mixin) changed-child
     old-min-x old-min-y old-max-x old-max-y)
  ;; If the child's old and new bbox lies entirely within the record's bbox
  ;; then no change need be made to the record's bbox.  Otherwise, if some part
  ;; of the child's bbox was on the record's bbox and is now inside, examine
  ;; all the children to determine the correct new bbox.
  (with-slots (x1 y1 x2 y2) record
    (with-bounding-rectangle* (child-x1 child-y1 child-x2 child-y2)
	changed-child
      (unless (and (> x1 old-min-x)
		   (> y1 old-min-y)
		   (< x2 old-max-x)
		   (< y2 old-max-y)
		   (> x1 child-x1)
		   (> y1 child-y1)
		   (< x2 child-x2)
		   (< y2 child-y2))
	;; Don't know if changed-child has been deleted or what, so go through
	;; all the children and construct the updated bbox.
	(setf (values x1 y1 x2 y2) (%tree-recompute-extent* record))
	nil))))

(defmethod tree-recompute-extent ((record output-record-mixin))
  (with-slots (x1 y1 x2 y2) record
    (setf (values x1 y1 x2 y2) (%tree-recompute-extent* record))
    nil))


(defmethod tree-recompute-extent :around ((record output-record))
  (let ((old-rectangle (multiple-value-call #'make-bounding-rectangle
                         (bounding-rectangle* record))))
    (call-next-method)
    (with-slots (parent x1 y1 x2 y2) record
      (when (and parent (not (region-equal old-rectangle record)))
        (recompute-extent-for-changed-child parent record x1 y1 x2 y2)))))


;;; Graphics recording classes

(defclass graphics-displayed-output-record (displayed-output-record)
  ())

(defclass graphics-displayed-output-record-mixin
    (displayed-output-record-mixin graphics-displayed-output-record)
  ((clip :initarg :clipping-region
         :documentation "Clipping region in user coordinates.")
   (transform :initarg :transformation)
   (line-style :initarg :line-style)
   (text-style :initarg :text-style)
   ))

(defun graphics-displayed-output-record-p (x)
  (typep x 'graphics-displayed-output-record))


;;; stream-output-history-mixin class

(defclass stream-output-history-mixin ()
  ())

(defclass standard-sequence-output-history
    (standard-sequence-output-record stream-output-history-mixin)
  ())

(defclass standard-tree-output-history
    (standard-tree-output-record stream-output-history-mixin)
  ())


;;; Output-Recording-Stream class

(defclass output-recording-stream ()
  ())

(defun output-recording-stream-p (x)
  (typep x 'output-recording-stream))

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

;;; 16.4.1 The Output Recording Stream Protocol
(defgeneric stream-recording-p (stream))

(defgeneric (setf stream-recording-p) (recording-p stream))

(defgeneric stream-drawing-p (stream))

(defgeneric (setf stream-drawing-p) (drawing-p stream))

(defgeneric stream-output-history (stream))

(defgeneric stream-current-output-record (stream))

(defgeneric (setf stream-current-output-record) (record stream))

(defgeneric stream-add-output-record (stream record))

(defgeneric stream-replay (stream &optional region))

(defgeneric erase-output-record (record stream &optional (errorp t)))

(defgeneric copy-textual-output-history (window stream &optional region record))

(defmethod (setf stream-recording-p)
    (recording-p (stream standard-output-recording-stream))
  (let ((old-val (slot-value stream 'recording-p)))
    (setf (slot-value stream 'recording-p) recording-p)
    (when (not (eql old-val recording-p))
      (stream-close-text-output-record stream))
    recording-p))

;;; 16.4.3 Text Output Recording

(defgeneric stream-text-output-record (stream text-style))

(defgeneric stream-close-text-output-record (stream))

(defgeneric stream-add-character-output
  (stream character text-style width height baseline))

(defgeneric stream-add-string-output
  (stream string start end text-style width height baseline))

;;; Methods
(defmethod initialize-instance :after ((stream output-recording-stream) &rest args)
  (declare (ignore args))
  (setf (stream-current-output-record stream) (stream-output-history stream)))

(defmethod stream-add-output-record ((stream output-recording-stream) record)
  (add-output-record record (stream-current-output-record stream)))

(defmethod stream-replay ((stream output-recording-stream) &optional region)
  (replay (stream-output-history stream) stream region))

(defmacro with-output-recording-options ((stream 
					  &key (record nil record-supplied-p)
					       (draw nil draw-supplied-p))
					 &body body)
  (declare (type symbol stream))
  (when (eq stream 't)
    (setq stream '*standard-output*))
  (let ((continuation-name (gensym "WITH-OUTPUT-RECORDING-OPTIONS")))
    `(flet ((,continuation-name  (,stream) ,@body))
       (declare (dynamic-extent #',continuation-name))
       (invoke-with-output-recording-options ,stream
                                             #',continuation-name
                                             ,(if record-supplied-p
						  record
						  `(stream-recording-p
						    ,stream))
					     ,(if draw-supplied-p
						  draw
						  `(stream-drawing-p
						    ,stream))))))


(defmethod invoke-with-output-recording-options
  ((stream output-recording-stream) continuation record draw)
  "Calls CONTINUATION on STREAM enabling or disabling recording and drawing
according to the flags RECORD and DRAW."
  (letf (((stream-recording-p stream) record)
	 ((stream-drawing-p stream) draw))
    (funcall continuation stream)))

(defmacro with-new-output-record ((stream
                                   &optional
                                   (record-type 'standard-sequence-output-record)
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
  (declare (type symbol stream record))
  (when (eq stream 't)
    (setq stream '*standard-output*))
  (unless record-supplied-p
    (setq record (gensym)))
  `(invoke-with-new-output-record
    ,stream
    #'(lambda (,stream ,record)
        (declare (ignorable ,stream ,record))
        ,@body)
    ',record-type
    ,@initargs))

(defmethod invoke-with-new-output-record ((stream output-recording-stream)
                                          continuation record-type
                                          &rest initargs
                                          &key parent
					  &allow-other-keys)
  (stream-close-text-output-record stream)
  (unless parent
    (setq parent (stream-current-output-record stream)))
  (let ((new-record (apply #'make-instance record-type :parent parent initargs)))
    (letf (((stream-current-output-record stream) new-record))
      ;; Should we switch on recording? -- APD
      (funcall continuation stream new-record)
      (finish-output stream))
    (stream-add-output-record stream new-record)
    new-record))

(defmacro with-output-to-output-record
    ((stream
      &optional
      (record-type 'standard-sequence-output-record)
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
  (when (eq stream 't)
    (setq stream '*standard-output*))
  (check-type stream symbol)
  (unless record-supplied-p (setq record (gensym "RECORD")))
  `(invoke-with-output-to-output-record
    ,stream
    #'(lambda (,stream ,record)
        (declare (ignorable ,stream ,record))
        ,@body)
    ',record-type
    ,@initargs))

(defmethod invoke-with-output-to-output-record
    ((stream output-recording-stream)
     continuation record-type
     &rest initargs
     &key parent
     &allow-other-keys)
  (stream-close-text-output-record stream)
  (unless parent (setq parent (stream-current-output-record stream)))
  (let ((new-record (apply #'make-instance record-type
                           :parent parent initargs)))
    (with-output-recording-options (stream :record t :draw nil)
      (multiple-value-bind (cx cy)
          (stream-cursor-position stream)
        (unwind-protect
             (letf (((stream-current-output-record stream) new-record))
               (setf (stream-cursor-position stream) (values 0 0))
               (funcall continuation stream new-record)
               (finish-output stream))
          (setf (stream-cursor-position stream) (values cx cy)))))
    new-record))

(defmethod scroll-vertical :around ((stream output-recording-stream) dy)
  (declare (ignore dy))
  (with-output-recording-options (stream :record nil)
    (declare (ignore stream))
    (call-next-method)))

(defmethod scroll-horizontal :around ((stream output-recording-stream) dx)
  (declare (ignore dx))
  (with-output-recording-options (stream :record nil)
    (declare (ignore stream))
    (call-next-method)))

(defmethod handle-repaint ((stream output-recording-stream) region)
  (stream-replay stream region))

#|
(defmethod handle-event :after ((stream output-recording-stream) (event pointer-button-press-event))
  (highlight-output-record (stream-current-output-record stream) stream :highlight))

(defmethod handle-event :before ((stream output-recording-stream) (event pointer-button-release-event))
  (highlight-output-record (stream-current-output-record stream) stream :unhighlight))
|#


;;; Graphics and text recording classes

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun compute-class-vars (names)
    (cons (list 'stream :initarg :stream)
	  (loop for name in names
		collecting (list name :initarg (intern (symbol-name name)
                                                       :keyword)))))

  (defun compute-arg-list (names)
    (loop for name in names
	  nconcing (list (intern (symbol-name name) :keyword) name)))
  )

(defmacro def-grecording (name (&rest args) &body body)
  (let ((method-name (intern (format nil "MEDIUM-~A*" name)))
	(class-name (intern (format nil "~A-OUTPUT-RECORD" name)))
	(medium (gensym "MEDIUM"))
	(border (gensym "BORDER")))
    `(progn
       (defclass ,class-name (graphics-displayed-output-record-mixin)
	 ,(compute-class-vars args))
       (defmethod initialize-instance :after ((graphic ,class-name) &rest args)
	 (declare (ignore args))
	 (with-slots (x y x1 y1 x2 y2 initial-x1 initial-y1
		      stream ink clipping-region transform
		      line-style text-style
		      ,@args) graphic
           (let ((,border (1+ (/ (line-style-thickness line-style) 2))))
             (multiple-value-bind (lf tp rt bt) (progn ,@body)
               (setq x1 (- lf ,border)
                     y1 (- tp ,border)
                     x2 (+ rt ,border)
                     y2 (+ bt ,border))))
           (setf x x1
                 y y1
                 initial-x1 x1
                 initial-y1 y1)))
       (defmethod ,method-name :around ((stream output-recording-stream) ,@args)
	 (with-sheet-medium (medium stream)
	   (when (stream-recording-p stream)
	     (let ((record (make-instance ',class-name
			     :stream stream
			     :ink (medium-ink medium)
			     :clipping-region (medium-clipping-region medium)
			     :transformation (medium-transformation medium)
			     :line-style (medium-line-style medium)
			     :text-style (medium-text-style medium)
			     ,@(compute-arg-list args))))
	       (stream-add-output-record stream record)))
	   (when (stream-drawing-p stream)
	     (call-next-method))))
       (defmethod replay-output-record ((record ,class-name) stream
					&optional (region +everywhere+)
                                        (x-offset 0) (y-offset 0))
	 (with-slots (x y initial-x1 initial-y1
                      ink clip transform line-style text-style ,@args) record
           (let ((transformation (compose-translation-with-transformation
                                  transform
                                  (+ (- x initial-x1) x-offset)
                                  (+ (- y initial-y1) y-offset)))
                 (,medium (sheet-medium stream))
                 ;; is sheet a sheet-with-medium-mixin? --GB
                 )
             (letf (((medium-ink ,medium) ink)
                    ((medium-transformation ,medium) transformation)
                    ((medium-clipping-region ,medium)
                     (region-intersection clip
                                          (untransform-region transformation
                                                              region)))
                    ((medium-line-style ,medium) line-style)
                    ((medium-text-style ,medium) text-style))
               (,method-name ,medium ,@args))))))))

(def-grecording draw-point (point-x point-y)
  (with-transformed-position (transform point-x point-y)
     (values point-x point-y point-x point-y)))

(def-grecording draw-points (coord-seq)
  (with-transformed-positions (transform coord-seq)
     (loop for (x y) on coord-seq by #'cddr
           minimize x into min-x
           minimize y into min-y
           maximize x into max-x
           maximize y into max-y
           finally (return (values min-x min-y max-x max-y)))))

(def-grecording draw-line (point-x1 point-y1 point-x2 point-y2)
  (with-transformed-position (transform point-x1 point-y1)
    (with-transformed-position (transform point-x2 point-y2)
      (values (min point-x1 point-x2) (min point-y1 point-y2)
              (max point-x1 point-x2) (max point-y1 point-y2)))))

(def-grecording draw-lines (coord-seq)
  (with-transformed-positions (transform coord-seq)
     (loop for (x y) on coord-seq by #'cddr
           minimize x into min-x
           minimize y into min-y
           maximize x into max-x
           maximize y into max-y
           finally (return (values min-x min-y max-x max-y)))))

(def-grecording draw-polygon (coord-seq closed filled)
  ;; FIXME !!!
  ;; If LINE-STYLE-JOINT-SHAPE is :MITTER, then the bb is larger than
  ;; these numbers by (LINE-THICKNESS / (sin (angle / 2))),
  ;; which is larger than LINE-THICKNESS
  (with-transformed-positions (transform coord-seq)
     (loop for (x y) on coord-seq by #'cddr
           minimize x into min-x
           minimize y into min-y
           maximize x into max-x
           maximize y into max-y
           finally (return (values min-x min-y max-x max-y)))))

(def-grecording draw-rectangle (left top right bottom filled)
  ;; FIXME!!! If the rectangle is a line/point, MAKE-RECTANGLE* gives +NOWHERE+,
  ;; and BOUNDING-RECTANGLE* signals an error.
  (bounding-rectangle* (transform-region transform
                                         (make-rectangle* left top right bottom))))

(def-grecording draw-ellipse (center-x center-y
			      radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			      start-angle end-angle filled)
  (bounding-rectangle* (transform-region transform
                                         (make-ellipse* center-x center-y
                                                        radius-1-dx radius-1-dy radius-2-dx radius-2-dy
                                                        :start-angle start-angle
                                                        :end-angle end-angle))))

(def-grecording draw-text (string point-x point-y start end
			   align-x align-y toward-x toward-y transform-glyphs)
  ;; FIXME!!! transformation
  ;; Multiple lines?
 (let* ((width (stream-string-width stream string
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


;;; Text recording class

(defclass text-displayed-output-record (displayed-output-record)
  ())

(defclass text-displayed-output-record-mixin
    (text-displayed-output-record displayed-output-record-mixin)
  ((strings :initform nil)
   (baseline :initform 0)
   (width :initform 0)
   (max-height :initform 0)
   (start-x :initarg :start-x)
   (start-y :initarg :start-y)
   (end-x)
   (end-y)
   (wrapped :initform nil
	    :accessor text-record-wrapped)))

(defun text-displayed-output-record-p (x)
  (typep x 'text-displayed-output-record))

(defmethod print-object ((self text-displayed-output-record-mixin) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (if (slot-boundp self 'start-x)
	(with-slots (start-x start-y strings) self
	  (format stream "~D,~D ~S" start-x start-y (mapcar #'third strings)))
      (format stream "empty"))))

(defgeneric add-character-output-to-text-record
  (text-record character text-style width height baseline))
(defgeneric add-string-output-to-text-record
  (text-record string start end text-style width height baseline))
(defgeneric text-displayed-output-record-string (text-record))

;;; Methods
(defmethod tree-recompute-extent
    ((text-record text-displayed-output-record-mixin))
  (with-slots (parent x y
                      x1 y1 x2 y2 width max-height) text-record
              (setq x1 (coordinate x)
                    y1 (coordinate y)
                    x2 (coordinate (+ x width))
                    y2 (coordinate (+ y max-height)))))

(defmethod* (setf output-record-position) :before
    (nx ny (record text-displayed-output-record-mixin))
  (with-slots (x1 y1 x2 y2 x y start-x start-y end-x end-y) record
    (let ((dx (- nx x))
          (dy (- ny y)))
      (incf start-x dx)
      (incf start-y dy)
      (incf end-x dx)
      (incf end-y dy))))

(defmethod add-character-output-to-text-record ((text-record text-displayed-output-record-mixin)
						character text-style char-width height
						new-baseline)
  (with-slots (strings baseline width max-height start-y end-x end-y) text-record
    (if (and strings (eq (second (first (last strings))) text-style))
	(vector-push-extend character (third (first (last strings))))
      (setq strings (nconc strings (list (list end-x text-style (make-array 1 :initial-element character :element-type 'character :adjustable t :fill-pointer t))))))
    (setq baseline (max baseline new-baseline)
	  end-x (+ end-x char-width)
	  max-height (max max-height height)
	  end-y (max end-y (+ start-y max-height))
          width (+ width char-width)))
  (tree-recompute-extent text-record))

(defmethod add-string-output-to-text-record ((text-record text-displayed-output-record-mixin)
					     string start end text-style string-width height
					     new-baseline)
  (if end
      (setq end (min end (1- (length string))))
      (setq end (1- (length string))))
  (let ((length (max 0 (- (1+ end) start))))
    (cond
     ((= length 1)
      (add-character-output-to-text-record text-record (aref string start) text-style string-width height new-baseline))
     (t
      (setq string (make-array length :displaced-to string
			       :displaced-index-offset start
			       :element-type (array-element-type string)))
      (with-slots (strings baseline width max-height start-y end-x end-y) text-record
	(setq baseline (max baseline new-baseline)
	      strings (nconc strings
			     (list (list end-x text-style
					 (make-array (length string)
						     :initial-contents string
						     :element-type 'character
						     :adjustable t
						     :fill-pointer t))))
	      end-x (+ end-x string-width)
	      max-height (max max-height height)
	      end-y (max end-y (+ start-y max-height))
	      width (+ width string-width)))
      (tree-recompute-extent text-record)))))

(defmethod replay-output-record ((record text-displayed-output-record-mixin)
				 stream
				 &optional region (x-offset 0) (y-offset 0))
  (declare (ignore region))
  (with-slots (strings baseline max-height start-x start-y wrapped
               x y x1 y1 initial-x1 initial-y1) record
    (let ((old-medium (sheet-medium stream))
	  (new-medium (make-medium (port stream) stream)))
      (unwind-protect
           (progn
             (setf (%sheet-medium stream) new-medium) ;is sheet a sheet-with-medium-mixin? --GB
             (setf (%medium-sheet new-medium) stream) ;is medium a basic medium?
             (setf (medium-transformation new-medium)
                   (make-translation-transformation
                    x-offset
                    y-offset))

             (setf (stream-cursor-position stream) (values start-x start-y))
             (letf (((slot-value stream 'baseline) baseline))
               (loop for (x text-style string) in strings
                     do (setf (medium-text-style new-medium) text-style)
			(setf (stream-cursor-position stream)
			  (values (+ x (- x1 initial-x1)) start-y))
			(stream-write-line stream string)))
             ;; clipping region
             #|restore cursor position? set to (end-x,end-y)?|#
             #+nil(loop for y = (+ start-y baseline)
                        for (x text-style string) in strings
                        do (setf (medium-text-style new-medium) text-style)
                        (draw-text* (sheet-medium stream) string x y
                                    :text-style text-style :clipping-region (untransform-region (medium-transformation new-medium) region)))
             (if wrapped
                 (draw-rectangle* (sheet-medium stream)
                                  (+ wrapped 0) start-y (+ wrapped 4) (+ start-y max-height)
                                  :ink +foreground-ink+
                                  :filled t)))
	(setf (%sheet-medium stream) old-medium))))) ;is sheet a sheet-with-medium-mixin? --GB

(defmethod output-record-start-cursor-position
    ((record text-displayed-output-record-mixin))
  (with-slots (start-x start-y) record
    (values start-x start-y)))

(defmethod output-record-end-cursor-position
    ((record text-displayed-output-record-mixin))
  (with-slots (end-x end-y) record
    (values end-x end-y)))

(defmethod text-displayed-output-record-string
    ((record text-displayed-output-record-mixin))
  (with-slots (strings) record
    (loop for result = ""
	  for s in strings
	  do (setq result (concatenate 'string result (third s)))
	     finally (return result))))


(defclass stream-text-record (text-displayed-output-record-mixin)
  ())

;;; Methods for text output to output recording streams
(defmethod stream-text-output-record ((stream standard-output-recording-stream) text-style)
  (declare (ignore text-style))
  (let ((record (stream-current-text-output-record stream)))
    (unless record
      (setf (stream-current-text-output-record stream)
            (setq record (make-instance 'stream-text-record)))
      (with-slots (start-x start-y end-x end-y x1 y1 x2 y2 x y
                   initial-x1 initial-y1) record
	  (multiple-value-bind (cx cy) (stream-cursor-position stream)
	    (setq start-x cx
		  start-y cy
		  end-x start-x
		  end-y start-y
		  x1 (coordinate start-x)
		  x2 (coordinate end-x)
		  y1 (coordinate start-y)
		  y2 (coordinate end-y)
                  initial-x1 x1
                  initial-y1 y1
                  x start-x
                  y start-y))))
    record))

(defmethod stream-close-text-output-record ((stream standard-output-recording-stream))
  (let ((record (stream-current-text-output-record stream)))
    (when record
      (setf (stream-current-text-output-record stream) nil)
      #|record stream-current-cursor-position to (end-x record) - already done|#
      (stream-add-output-record stream record))))

(defmethod stream-add-character-output ((stream standard-output-recording-stream)
                                        character text-style
                                        width height baseline)
  (add-character-output-to-text-record (stream-text-output-record stream text-style)
                                       character text-style width height baseline))

(defmethod stream-add-string-output ((stream standard-output-recording-stream)
                                     string start end text-style
                                     width height baseline)
  (add-string-output-to-text-record (stream-text-output-record stream text-style)
                                    string start end text-style
                                    width height baseline))

(defmacro without-local-recording (stream &body body)
  `(letf (((slot-value ,stream 'local-record-p) nil))
    ,@body))

(defmethod stream-write-line :around ((stream standard-output-recording-stream) line)
  (when (and (stream-recording-p stream)
             (slot-value stream 'local-record-p))
    (let* ((medium (sheet-medium stream))
           (text-style (medium-text-style medium)))
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
            (eql char #\newline))
        (stream-close-text-output-record stream)
      (let* ((medium (sheet-medium stream))
             (text-style (medium-text-style medium)))
        (stream-add-character-output stream char text-style
                                     (stream-character-width stream char :text-style text-style)
                                     (text-style-height text-style medium)
                                     (text-style-ascent text-style medium)))))
  (without-local-recording stream
                           (call-next-method)))

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
