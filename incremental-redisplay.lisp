;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2002 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2002 by Tim Moore (moore@bricoworks.com)

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

(in-package :clim-internals)

#|
Incremental Redisplay Theory of Operation

In McCLIM I'm abandoning the compute-difference-set protocol from the spec.
It's not clear how record movement interacts with layers, antialiasing, etc.,
and with our current lame compound output record representation it's probably
not a good idea.  Instead I've created compute-affected-region, which just
returns the regions that need to be erased and redrawn.  Perhaps this can be
couched in terms of compute-difference-set later...

Because records may overlap, it's essential to preserve the drawing order
encoded in the record tree.  Therefore, we can't just erase and redraw
individual records; we need to collect regions that need to be erased and
drawn, then test records in the tree against those regions.  Regions that are
redrawn also need to be erased, of course.

At first, updating-output creates a compound output record similar to a normal
compound-output-record.  redisplay is called on the top level updating-output-
record.  Each recursive invocation calls compute-new-output-records-1.  If the
record passes the cache test, nothing further is done.  Otherwise, the node is
marked dirty, the child output records are snapshotted , graphics state for that
node is restored, and the  displayer continuation is called.  That leaves new
records as the children of the node.

Next, compute-affected-region is called on the top level node.  Nodes are
examined in the old and current sets.  The different cases are:

* Record in old set is not an updating-output-record.  Its region is
erased.

* Record in new set is not an updating-output-record.  Its region is drawn

* Record is in old set, but not in the current set.  All the
displayed-output-records under it must be erased.

* Record is in current set, but not old set.  All its displayed-output-records
must be drawn.

* Record is in both old and new set.  If it's not dirty, nothing is done.
Otherwise recurse and collect the regions to be erased and drawn.

The code can be simplified by treating all non updating-output-records as
dirty.

I thought of this process originally in terms of an "clear region" -- to be
erased -- and an "draw region" which needs to be drawn.  In practice, we can't
distinguish between the two because any region we erase needs to be redrawn and
vice-versa.  So, it simplifys things to only keep track of a single dirty
region in compute-affected-region.

|#

;;; Should this have a more complete CPL, to pull in the fact that it needs a
;;; medium for graphics state?
(defclass updating-output-stream-mixin (extended-output-stream)
  ((redisplaying-p :reader stream-redisplaying-p :initform nil)
   (id-map :accessor id-map :initform nil)))

(defclass updating-stream-state (complete-medium-state)
  ((cursor-x :accessor cursor-x :initarg :cursor-x :initform 0)
   (cursor-y :accessor cursor-y :initarg :cursor-y :initform 0)))

(defmethod initialize-instance :after ((obj updating-stream-state)
				       &key (stream nil))
  (when stream
    (setf (values (slot-value obj 'cursor-x) (slot-value obj 'cursor-y))
	  (stream-cursor-position stream))))

(defmethod set-medium-graphics-state :after
    ((state updating-stream-state) (stream updating-output-stream-mixin))
  (setf (stream-cursor-position stream)
	(values (cursor-x state) (cursor-y state))))

(defmethod medium-graphics-state ((stream updating-output-stream-mixin)
				  &optional state)
  (if (and state (subtypep state 'updating-stream-state))
      (reinitialize-instance state :stream stream)
      (make-instance 'updating-stream-state :stream stream)))

(define-protocol-class updating-output-record (output-record))

(defclass updating-output-children-record (standard-sequence-output-record)
  ((last-bounding-box :accessor last-bounding-box
		      :documentation "When traversing children, holds the
   bounding box of the last updating-output-record visited.")))

(defmethod initialize-instance :after ((obj updating-output-children-record)
				       &key)
  (setf (last-bounding-box obj) (make-instance 'standard-rectangle
					       :x1 0 :x2 0 :y1 0 :y2 0)))

(defgeneric output-record-dirty (record))

(defmethod output-record-dirty ((record output-record))
  t)

(defclass updating-output-record-mixin (compound-output-record
					updating-output-record)
  ((unique-id :reader output-record-unique-id :initarg :unique-id)
   (id-test :reader output-record-id-test :initarg :id-test
	    :initform #'eql)
   (cache-value :reader output-record-cache-value :initarg :cache-value)
   (cache-test :reader output-record-cache-test :initarg :cache-test
	       :initform #'eql)
   (fixed-position :reader output-record-fixed-position
		   :initarg :fixed-position :initform nil)
   (displayer :reader output-record-displayer :initarg :displayer)
   (sub-record :accessor sub-record
	       :documentation "The actual contents of this record.  All output
record operations are forwarded to this record.")
   ;; Start and end cursor
   (start-graphics-state :accessor start-graphics-state
			 :initarg :start-graphics-state
			 :documentation "Graphics state needed to
   render record")
   (end-graphics-state :accessor end-graphics-state
		       :initarg :end-graphics-state
		       :documentation "Graphics state after rendering
   record; used to render non updating-output-records that follow")
   ;; Old record position
   (old-x :accessor old-x)
   (old-y :accessor old-y)
   (old-children :accessor old-children
		 :documentation "Contains the output record tree for the
  current display.")
   (id-map :accessor id-map :initform nil)
   (output-record-dirty :accessor output-record-dirty :initform :new
	  :documentation 
	  "If T, record was visited by compute-new-output-records-1.")
   ;; on-screen state?
   ))

(defmethod initialize-instance :after ((obj updating-output-record-mixin)
				       &key)
  (multiple-value-bind (x y)
      (output-record-position obj)
    (setf (sub-record obj) (make-instance 'updating-output-children-record
					  :x-position x :y-position y
					  :parent obj))))


(defmethod output-record-start-cursor-position
    ((record updating-output-record-mixin))
  (let ((state (start-graphics-state record)))
    (values (cursor-x state) (cursor-y state))))

(defmethod* (setf output-record-start-cursor-position)
    (x y (record updating-output-record-mixin))
  (let ((state (start-graphics-state record)))
    (setf (values (cursor-x state) (cursor-y state)) (values x y))))

(defmethod output-record-end-cursor-position
    ((record updating-output-record-mixin))
  (let ((state (end-graphics-state record)))
    (values (cursor-x state) (cursor-y state))))

(defmethod* (setf output-record-end-cursor-position)
    (x y (record basic-output-record))
  (let ((state (end-graphics-state record)))
    (setf (values (cursor-x state) (cursor-y state)) (values x y))))


(defmethod output-record-children ((record updating-output-record-mixin))
  (output-record-children (sub-record record)))

(defmethod output-record-count ((record updating-output-record-mixin))
  (output-record-count (sub-record record)))

(defmethod map-over-output-records
    (function (record updating-output-record-mixin)
     &optional (x-offset 0) (y-offset 0)
     &rest other-args)
  (apply #'map-over-output-records
	 function (sub-record record) x-offset y-offset
	 other-args)
  nil)

(defmethod map-over-output-records-containing-position
    (function (record updating-output-record-mixin) x y
     &optional (x-offset 0) (y-offset 0)
     &rest other-args)
  (apply #'map-over-output-records-containing-position
	 function (sub-record record) x y x-offset y-offset
	 other-args)
  nil)

(defmethod map-over-output-records-overlapping-region
    (function (record updating-output-record-mixin) region
     &optional (x-offset 0) (y-offset 0)
     &rest other-args)
  (apply #'map-over-output-records-overlapping-region
	 function (sub-record record) region x-offset y-offset
	 other-args)
  nil)

(defmethod add-output-record (child (record updating-output-record-mixin))
  (add-output-record child (sub-record record)))

(defmethod delete-output-record (child (record updating-output-record-mixin)
				 &optional (errorp t))
  (delete-output-record child (sub-record record) errorp))

(defmethod clear-output-record ((record updating-output-record-mixin))
  (clear-output-record (sub-record record)))

(defclass standard-updating-output-record (updating-output-record-mixin)
  ())

;;; 
(defvar *current-updating-output* nil)

(defgeneric compute-new-output-records (record stream))

(defgeneric compute-new-output-records-1 (record stream displayer)
  (:documentation "Like compute-new-output-records with an explicit
  displayer function."))

(defmethod compute-new-output-records ((record standard-updating-output-record)
				       stream)
  (with-output-recording-options (stream :record t :draw nil)
    (compute-new-output-records-1 record 
				  stream
				  (output-record-displayer record))))

(defmethod compute-new-output-records-1
    ((record standard-updating-output-record) stream displayer)
  (setf (old-children record) (sub-record record))
  (multiple-value-bind (x y)
      (output-record-position record)
    (setf (values (old-x record) (old-y record)) (values x y))
    #+nil
    (setf (values (old-min-x record) (old-min-y record)
		  (old-max-x record) (old-max-y record))
	  (bounding-rectangle* record))
    (setf (output-record-parent record) nil)
    (add-output-record record (stream-current-output-record stream))
    (setf (sub-record record) (make-instance 'standard-sequence-output-record
					     :x-position x :y-position y
					     :parent record)))
  (letf (((stream-current-output-record stream) record))
    (set-medium-graphics-state (start-graphics-state record) stream)
    (funcall displayer stream))
  (setf (output-record-dirty record) :updated))

(defun displayed-records-region (record)
  "Returns the region of all the displayed records in the leaves of record"
  (let ((region +nowhere+))
    (labels ((do-record (r)
	       (cond ((typep r 'compound-output-record)
		      (map-over-output-records #'do-record r))
		     (t (setq region (region-union region r))))))
      (do-record record)
      region)))

(defgeneric compute-delete-region (record))

(defmethod compute-delete-region ((record standard-displayed-output-record))
  record)

(defmethod compute-delete-region ((record compound-output-record))
  (let ((dirty-region +nowhere+))
    (map-over-output-records
     #'(lambda (r)
	 (setf dirty-region
	       (region-union dirty-region (compute-delete-region r))))
     record)
    dirty-region))

(defmethod compute-delete-region ((record standard-updating-output-record))
  (let ((dirty-region +nowhere+)
	(dirty (output-record-dirty record)))
    ;; If it's not new or unchanged...
    (when (or (null dirty)
	      (eq dirty :updated))
      (map-over-output-records
       #'(lambda (r)
	   (setf dirty-region
		 (region-union dirty-region (compute-delete-region r))))
       (old-children record)))
    ;; Release garbage
    (setf (old-children record) nil)
    dirty-region))

(defgeneric compute-draw-region (record))

(defmethod compute-draw-region ((record standard-displayed-output-record))
  record)

(defmethod compute-draw-region ((record compound-output-record))
  (let ((dirty-region +nowhere+))
    (map-over-output-records
     #'(lambda (r)
	 (setf dirty-region
	       (region-union dirty-region (compute-draw-region r))))
     record)
    dirty-region))

;;; Can the same (eq) output-record appear in both old and new children if it's
;;; not an updating-output-record?  Hmm, I suppose...

(defmethod compute-draw-region ((record standard-updating-output-record))
  (let ((dirty (output-record-dirty record)))
    (format *debug-io* "compute-draw-region ~S dirty: ~S~%" record dirty)
    (prog1
	(if (or (eq dirty :updated)
		(eq dirty :new))
	    (call-next-method)
	    +nowhere+)
      (setf (output-record-dirty record) nil))))

;;; Work in progress

(defvar *enable-updating-output* nil
  "Switch to turn on incremental redisplay")

(defmethod invoke-updating-output ((stream updating-output-stream-mixin)
				   continuation
				   (record-type
				    (eql 'standard-updating-output-record))
				   unique-id id-test cache-value cache-test
				   &key (fixed-position nil) (all-new nil)
				   (parent-cache nil))
  (unless *enable-updating-output*
    (return-from invoke-updating-output (funcall continuation stream)))
  (with-accessors ((id-map id-map))
      (or parent-cache *current-updating-output* stream)
    (let* ((record-cons (assoc unique-id id-map :test id-test))
	   (record (cdr record-cons)))
      (cond ((or all-new (null record))
	     ;; This case covers the outermost updating-output too.
	     (with-new-output-record (stream
				      'standard-updating-output-record
				      *current-updating-output*
				      :unique-id unique-id
				      :id-test id-test
				      :cache-value cache-value
				      :cache-test cache-test
				      :fixed-position fixed-position
				      :displayer continuation)
	       (setq record *current-updating-output*)
	       (push record id-map)
	       (setf (start-graphics-state record)
		     (medium-graphics-state stream))
	       (funcall continuation stream)
	       (setf (end-graphics-state record)
		     (medium-graphics-state stream))
	       (if record-cons
		   (setf (cdr record-cons) record)
		   (setf id-map (acons unique-id record id-map)))))
	    ((not (funcall cache-test
			   cache-value
			   (output-record-cache-value record)))
	     (compute-new-output-records-1 record stream continuation))
	    (t
	     ;; It doesn't need to be updated, but it does go into the
	     ;; parent's sequence of records
	     (setf (output-record-dirty record) :clean)
	     (setf (output-record-parent record) nil)
	     (add-output-record record (stream-current-output-record stream))
	     (set-medium-graphics-state(end-graphics-state record) stream)))
      record)))

#+nil
(defmethod invoke-updating-output (stream
				   continuation
				   (record-type
				    (eql 'standard-updating-output-record))
				   unique-id id-test cache-value cache-test
				   &key (fixed-position nil) (all-new nil)
				   (parent-cache nil))
  (funcall continuation stream))

; &key (unique-id (gensym)) was used earlier,
; changed to (unique-id `',(gensym)) as per gilham's request
; please CHECKME and delete this comment :]

(defmacro updating-output
    ((stream
      &key (unique-id `',(gensym)) (id-test '#'eql) cache-value (cache-test '#'eql)
      (fixed-position nil fixed-position-p)
      (all-new nil all-new-p)
      (parent-cache nil parent-cache-p)
      (record-type ''standard-updating-output-record))
     &body body)
  (when (eq stream t)
    (setq stream '*standard-output*))
  (let ((func (gensym "UPDATING-OUTPUT-CONTINUATION")))
    `(flet ((,func (,stream)
	      ,@body))
       (invoke-updating-output ,stream #',func ,record-type ,unique-id
			       ,id-test ,cache-value ,cache-test
	                       ,@ (and fixed-position-p
				       `(:fixed-position ,fixed-position))
			       ,@(and all-new-p `(:all-new ,all-new))
			       ,@(and parent-cache-p
				      `(:parent-cache ,parent-cache))))))

(defun redisplay (record stream &key (check-overlapping t))
  (redisplay-output-record record stream check-overlapping))

;;; Take the spec at its word that the x/y and parent-x/parent-y arguments are
;;; "entirely bogus."

(defgeneric redisplay-output-record (record stream
				     &optional check-overlapping))

(defmethod redisplay-output-record ((record updating-output-record)
				    (stream updating-output-stream-mixin)
				    &optional (check-overlapping t))
  (declare (ignore check-overlapping))
  (letf (((slot-value stream 'redisplaying-p) t))
    (compute-new-output-records record stream)
    (let ((delete-region (compute-delete-region record))
	  (draw-region (compute-draw-region record)))
      (format *debug-io* "delete: ~S~%draw: ~S~%" delete-region draw-region)
      (with-output-recording-options (stream :record nil :draw t)
	(with-drawing-options (stream :clipping-region delete-region)
	  (with-bounding-rectangle* (x1 y1 x2 y2)
	      record
	    (draw-rectangle* stream x1 y1 x2 y2 :ink +background-ink+)))
	(with-drawing-options (stream :clipping-region draw-region)
	  (with-bounding-rectangle* (x1 y1 x2 y2)
	      record
	    (draw-rectangle* stream x1 y1 x2 y2 :ink +background-ink+))))
      (replay record stream delete-region)
      (replay record stream draw-region))))

(defun convert-from-relative-to-absolute-coordinates (stream record)
  (let ((scy (if stream (bounding-rectangle-height stream) 0.0d0))
        (scx (if stream (bounding-rectangle-width stream) 0.0d0))
        (ory 0.0d0)
        (orx 0.0d0))
    (if record (multiple-value-setq (ory orx) (output-record-position record)))
    (values (+ scy ory) (+ scx orx))))
