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
;;;
;;; - Redo setf*-output-record-position, extent recomputation for
;;; compound records
;;;
;;; - When DRAWING-P is NIL, should stream cursor move?
;;;
;;; - :{X,Y}-OFFSET.

;;; Bug: (SETF OUTPUT-RECORD-POSITION) returns the record instead of
;;; the position. It is useful for debugging, but it is wrong.

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
(define-protocol-class graphics-displayed-output-record (output-record)
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
                                           &allow-other-keys))

(defgeneric invoke-with-output-to-output-record
    (stream continuation record-type
     &rest initargs
     &allow-other-keys))

(defgeneric make-design-from-output-record (record))


;;;; Implementation

(defclass basic-output-record (standard-bounding-rectangle output-record)
  ((parent :initarg :parent ; XXX
	   :initform nil
           :accessor output-record-parent)) ; XXX
  (:documentation "Implementation class for the Basic Output Record Protocol."))

(defmethod initialize-instance :after ((record basic-output-record)
                                       &key (x-position 0) (y-position 0)
				       &rest args)
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
  record)

(defmethod* (setf output-record-position) :around
    (nx ny (record basic-output-record))
  (declare (ignore nx ny))
  (with-bounding-rectangle* (min-x min-y max-x max-y) record
    (call-next-method)
    (let ((parent (output-record-parent record)))
      (when parent
        (recompute-extent-for-changed-child parent record
                                            min-x min-y max-x max-y))))
  record)

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

(defmethod replay-output-record ((record compound-output-record) stream
				 &optional region (x-offset 0) (y-offset 0))
  (when (null region)
    (setq region +everywhere+))
  (map-over-output-records-overlapping-region
   #'replay-output-record record region x-offset y-offset
   stream region x-offset y-offset))

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
                          :filled nil :ink +foreground-ink+))
        (:unhighlight
         (draw-rectangle* (sheet-medium stream) x1 y1 x2 y2
                          :filled nil :ink +background-ink+))))))

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

;;; 16.3.2. Graphics Displayed Output Records
(defclass standard-displayed-output-record (basic-output-record
                                            displayed-output-record)
  ((ink :initarg :ink :reader displayed-output-record-ink)
   (initial-x1 :initarg :initial-x1)
   (initial-y1 :initarg :initial-y1))
  (:documentation "Implementation class for DISPLAYED-OUTPUT-RECORD."))

(defclass standard-graphics-displayed-output-record
    (standard-displayed-output-record graphics-displayed-output-record)
  ((clip :initarg :clipping-region
         :documentation "Clipping region in user coordinates.")
   (transform :initarg :transformation)
   (line-style :initarg :line-style)
   (text-style :initarg :text-style)))

(defmacro def-grecording (name (&rest args) &body body)
  (let ((method-name (intern (format nil "MEDIUM-~A*" name)))
	(class-name (intern (format nil "~A-OUTPUT-RECORD" name)))
	(medium (gensym "MEDIUM"))
        (border 'border)
        (class-vars `((stream :initarg :stream)
                      ,@(loop for arg in args
                           collect `(,arg
                                     :initarg ,(intern (symbol-name arg)
                                                       :keyword)))))
        (arg-list (loop for arg in args
                     nconc `(,(intern (symbol-name arg) :keyword) ,arg))))
    `(progn
       (defclass ,class-name (standard-graphics-displayed-output-record)
	 ,class-vars)
       (defmethod initialize-instance :after ((graphic ,class-name) &rest args)
	 (declare (ignore args))
	 (with-slots (x1 y1 x2 y2 initial-x1 initial-y1
		      stream ink clipping-region transform
		      line-style text-style
		      ,@args) graphic
           (let ((,border (/ (line-style-effective-thickness
                              line-style (sheet-medium stream))
                             2)))
             (declare (ignorable ,border))
             (multiple-value-setq (x1 y1 x2 y2) (progn ,@body)))
           (setf initial-x1 x1
                 initial-y1 y1)))
       (defmethod ,method-name :around ((stream output-recording-stream) ,@args)
         ;; XXX STANDARD-OUTPUT-RECORDING-STREAM ^?
	 (with-sheet-medium (medium stream)
	   (when (stream-recording-p stream)
	     (let ((record (make-instance ',class-name
			     :stream stream
			     :ink (medium-ink medium)
			     :clipping-region (medium-clipping-region medium)
			     :transformation (medium-transformation medium)
			     :line-style (medium-line-style medium)
			     :text-style (medium-text-style medium)
			     ,@arg-list)))
	       (stream-add-output-record stream record)))
	   (when (stream-drawing-p stream)
	     (call-next-method))))
       (defmethod replay-output-record ((record ,class-name) stream
					&optional (region +everywhere+)
                                        (x-offset 0) (y-offset 0))
         (declare (ignore x-offset y-offset))
	 (with-slots (x1 y1 initial-x1 initial-y1
                      ink clip transform line-style text-style ,@args) record
           (let ((transformation (compose-translation-with-transformation
                                  transform
                                  (- x1 initial-x1)
                                  (- y1 initial-y1)))
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
     (values (- point-x border)
             (- point-y border)
             (+ point-x border)
             (+ point-y border))))

(def-grecording draw-points (coord-seq)
  (with-transformed-positions (transform coord-seq)
     (loop for (x y) on coord-seq by #'cddr
           minimize x into min-x
           minimize y into min-y
           maximize x into max-x
           maximize y into max-y
           finally (return (values (- min-x border)
                                   (- min-y border)
                                   (+ max-x border)
                                   (+ max-y border))))))

(def-grecording draw-line (point-x1 point-y1 point-x2 point-y2)
  (with-transformed-position (transform point-x1 point-y1)
    (with-transformed-position (transform point-x2 point-y2)
      (values (- (min point-x1 point-x2) border)
              (- (min point-y1 point-y2) border)
              (+ (max point-x1 point-x2) border)
              (+ (max point-y1 point-y2) border)))))

(def-grecording draw-lines (coord-seq)
  (with-transformed-positions (transform coord-seq)
     (loop for (x y) on coord-seq by #'cddr
           minimize x into min-x
           minimize y into min-y
           maximize x into max-x
           maximize y into max-y
           finally (return (values (- min-x border)
                                   (- min-y border)
                                   (+ max-x border)
                                   (+ max-y border))))))

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
           finally (return (if filled
                               (values min-x min-y max-x max-y)
                               (values (- min-x border)
                                       (- min-y border)
                                       (+ max-x border)
                                       (+ max-y border)))))))

(def-grecording draw-rectangle (left top right bottom filled)
  ;; FIXME!!! If the rectangle is a line/point, MAKE-RECTANGLE* gives +NOWHERE+,
  ;; and BOUNDING-RECTANGLE* signals an error.
  (multiple-value-bind (min-x min-y max-x max-y)
      (bounding-rectangle* (transform-region transform
                                             (make-rectangle*
                                              left top right bottom)))
    (if filled
        (values min-x min-y max-x max-y)
        (values (- min-x border)
                (- min-y border)
                (+ max-x border)
                (+ max-y border)))))

(def-grecording draw-ellipse (center-x center-y
			      radius-1-dx radius-1-dy radius-2-dx radius-2-dy
			      start-angle end-angle filled)
  (multiple-value-bind (min-x min-y max-x max-y)
      (bounding-rectangle*
       (transform-region transform
                         (make-ellipse* center-x center-y
                                        radius-1-dx radius-1-dy
                                        radius-2-dx radius-2-dy
                                        :start-angle start-angle
                                        :end-angle end-angle)))
    (if filled
        (values min-x min-y max-x max-y)
        (values (- min-x border)
                (- min-y border)
                (+ max-x border)
                (+ max-y border)))))

(def-grecording draw-text (string point-x point-y start end
			   align-x align-y toward-x toward-y transform-glyphs)
  ;; FIXME!!! Text direction.
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

;;; 16.3.3. Text Displayed Output Record
(defvar *drawing-options* (list +foreground-ink+ +everywhere+)
  "The ink and the clipping region of the current stream.") ; XXX TDO

(defclass styled-string ()
  ((start-x :initarg :start-x)
   (text-style :initarg :text-style)
   (ink :initarg :ink)
   (clipping-region :initarg :clipping-region)
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
	    :accessor text-record-wrapped)))

(defmethod print-object ((self standard-text-displayed-output-record) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (with-slots (start-x start-y strings) self
      (format stream "~D,~D ~S"
              start-x start-y
              (mapcar #'styled-string-string strings)))))

(defmethod* (setf output-record-position) :before
    (nx ny (record standard-text-displayed-output-record))
  (with-slots (x1 y1 start-x start-y end-x end-y) record
    (let ((dx (- nx x1))
          (dy (- ny y1)))
      (incf start-x dx)
      (incf start-y dy)
      (incf end-x dx)
      (incf end-y dy))))

(defmethod replay-output-record ((record standard-text-displayed-output-record)
				 stream
				 &optional region (x-offset 0) (y-offset 0))
  (declare (ignore region x-offset y-offset))
  (with-slots (strings baseline max-height start-y wrapped
               x1 y1 initial-x1 initial-y1) record
    (with-sheet-medium (medium stream) ;is sheet a sheet-with-medium-mixin? --GB
      ;; XXX Disable recording?
      (multiple-value-bind (cx cy) (stream-cursor-position stream)
        (letf (((medium-text-style medium) (make-text-style nil nil nil))
               ((medium-ink medium) +foreground-ink+)
               ((medium-clipping-region medium) +everywhere+)
               ((medium-transformation medium) +identity-transformation+))
          (unwind-protect ; XXX Redo when LETF will work with VALUES.
               (progn
                 (letf (((slot-value stream 'baseline) baseline)) ; FIXME
                   (loop with offset =  (- x1 initial-x1)
                      for substring in strings
                      do (with-slots (start-x text-style ink clipping-region
                                              string)
                             substring
                           (setf (stream-cursor-position stream)
                                 (values (+ start-x offset) start-y))
                           (setf (medium-text-style medium) text-style
                                 (medium-ink medium) ink
                                 (medium-clipping-region medium) clipping-region)
                           (stream-write-line stream string))))
                 (when wrapped ; FIXME
                   (draw-rectangle* medium
                                    (+ wrapped 0) start-y
                                    (+ wrapped 4) (+ start-y max-height)
                                    :ink +foreground-ink+
                                    :filled t)))
            (setf (stream-cursor-position stream) (values cx cy))))))))

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
  (destructuring-bind (ink clipping-region) *drawing-options* ; XXX TDO
    (with-slots (strings baseline width max-height start-y end-x end-y)
        text-record
      (if (and strings
               (let ((string (last1 strings)))
                 (and (eq text-style (slot-value string 'text-style))
                      (eq ink (slot-value string 'ink))
                      (eq clipping-region
                          (slot-value string 'clipping-region)))))
          (vector-push-extend character (slot-value (last1 strings) 'string))
          (nconcf strings
                  (list (make-instance
                         'styled-string
                         :start-x end-x
                         :text-style text-style
                         :ink (first *drawing-options*) ; XXX TDO
                         :clipping-region (second *drawing-options*)
                         :string (make-array 1 :initial-element character
                                             :element-type 'character
                                             :adjustable t
                                             :fill-pointer t)))))
      (setq baseline (max baseline new-baseline)
            end-x (+ end-x char-width)
            max-height (max max-height height)
            end-y (max end-y (+ start-y max-height))
            width (+ width char-width))))
  (tree-recompute-extent text-record))

(defmethod add-string-output-to-text-record
    ((text-record standard-text-displayed-output-record)
     string start end text-style string-width height new-baseline)
  (if end
      (setq end (min end (length string)))
      (setq end (length string)))
  (let ((length (max 0 (- end start))))
    (cond
      ((= length 1)
       (add-character-output-to-text-record text-record
                                            (aref string start)
                                            text-style
                                            string-width height new-baseline))
      (t
       (setq string (make-array length :displaced-to string ; XXX
                                :displaced-index-offset start
                                :element-type (array-element-type string)))
       (with-slots (strings baseline width max-height start-y end-x end-y)
           text-record
         (nconcf strings
                 (list (make-instance
                        'styled-string
                        :start-x end-x
                        :text-style text-style
                        :ink (first *drawing-options*) ; XXX TDO
                        :clipping-region (second *drawing-options*)
                        :string (make-array (length string)
                                            :initial-contents string
                                            :element-type 'character
                                            :adjustable t
                                            :fill-pointer t))))
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
              (draw-rectangle* stream x1 y1 x2 y2 :ink +background-ink+)
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
                                    :start-x cx :start-y cy)
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

;;; 16.4.4. Output Recording Utilities

(defmacro with-output-recording-options ((stream
					  &key (record nil record-supplied-p)
					       (draw nil draw-supplied-p))
					 &body body)
  (when (eq stream 't) (setq stream '*standard-output*))
  (check-type stream symbol)
  (with-gensyms (continuation)
    `(flet ((,continuation  (,stream) ,@body))
       (declare (dynamic-extent #',continuation))
       (invoke-with-output-recording-options
        ,stream #',continuation
        ,(if record-supplied-p record `(stream-recording-p ,stream))
        ,(if draw-supplied-p draw `(stream-drawing-p ,stream))))))

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
  (when (eq stream 't) (setq stream '*standard-output*))
  (check-type stream symbol)
  (unless record-supplied-p (setq record (gensym)))
  `(invoke-with-new-output-record ,stream
                                  #'(lambda (,stream ,record)
                                      (declare (ignorable ,stream ,record))
                                      ,@body)
                                  ',record-type
                                  ,@initargs))

(defmethod invoke-with-new-output-record ((stream output-recording-stream)
                                          continuation record-type
                                          &rest initargs
					  &allow-other-keys)
  (stream-close-text-output-record stream)
  (let ((new-record (apply #'make-instance record-type initargs)))
    (letf (((stream-current-output-record stream) new-record))
      ;; Should we switch on recording? -- APD
      (funcall continuation stream new-record)
      (finish-output stream))
    (stream-add-output-record stream new-record)
    new-record))

(defmacro with-output-to-output-record
    ((stream
      &optional (record-type 'standard-sequence-output-record)
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
    ',record-type
    ,@initargs))

(defmethod invoke-with-output-to-output-record
    ((stream output-recording-stream) continuation record-type
     &rest initargs
     &allow-other-keys)
  (stream-close-text-output-record stream)
  (let ((new-record (apply #'make-instance record-type initargs)))
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

#|
(defmethod handle-event :after ((stream output-recording-stream) (event pointer-button-press-event))
  (highlight-output-record (stream-current-output-record stream) stream :highlight))

(defmethod handle-event :before ((stream output-recording-stream) (event pointer-button-release-event))
  (highlight-output-record (stream-current-output-record stream) stream :unhighlight))
|#
