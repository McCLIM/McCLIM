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

(in-package :CLIM-INTERNALS)

(define-protocol-class updating-output-record (output-record))

(defclass updating-output-children-record (standard-sequence-output-record)
  ((last-bounding-box :accessor last-bounding-box
		      :documentation "When traversing children, holds the
   bounding box of the last updating-output-record visited.")))

(defmethod initialize-instance :after ((obj updating-output-children-record)
				       &key)
  (setf (last-bounding-box obj) (make-instance 'standard-rectangle
					       :x1 0 :x2 0 :y1 0 :y2 0)))

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
   (start-x :accessor start-x)
   (start-y :accessor start-y)
   (end-x :accessor end-x)
   (end-y :accessor end-y)
   ;; Old record position
   (old-x :accessor old-x)
   (old-y :accessor old-y)
   (old-start-x :accessor old-start-x)
   (old-start-y :accessor old-start-y)
   ;; XXX Need to capture the "user" transformation, I think; deal with that
   ;; later.
   (old-children :accessor old-children
		 :documentation "Contains the output record tree for the
  current display.")
   (id-map :accessor id-map :initform nil)
   (dirty :accessor dirty :initform nil
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
					  :parent obj)))
  )

(defmethod output-record-start-cursor-position
    ((record updating-output-record-mixin))
  (with-slots (start-x start-y) record
    (values start-x start-y)))

(defmethod* (setf output-record-start-cursor-position)
    (x y (record basic-output-record))
  (with-slots (start-x start-y) record
    (setf (values start-x start-y) (values x y))))

(defmethod output-record-end-cursor-position
    ((record updating-output-record-mixin))
  (with-slots (end-x end-y) record
    (values end-x end-y)))

(defmethod* (setf output-record-end-cursor-position)
    (x y (record updating-output-record-mixin))
  (with-slots (end-x end-y) record
    (setf (values end-x end-y) (values x y))))

(defmethod output-record-children ((record updating-output-record-mixin))
  (list (sub-record record)))

(defmethod output-record-count ((record updating-output-record-mixin))
  1)

(defmethod map-over-output-records
    (function (record updating-output-record-mixin)
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  (declare (ignore x-offset y-offset))
  (apply function (sub-record record) function-args)
  nil)

(defmethod map-over-output-records-containing-position
    (function (record updating-output-record-mixin) x y
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  (declare (ignore x-offset y-offset))
  (let ((child (sub-record record)))
    (when (and (multiple-value-bind (min-x min-y max-x max-y)
		   (output-record-hit-detection-rectangle* child)
		 (and (<= min-x x max-x) (<= min-y y max-y)))
	       (output-record-refined-position-test child x y))
      (apply function child function-args))
    nil))

(defmethod map-over-output-records-overlapping-region
    (function (record updating-output-record-mixin) region
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  (declare (ignore x-offset y-offset))
  (let ((child (sub-record record)))
    (when (region-intersects-region-p region child)
      (apply function child function-args))
    nil))

(defmethod add-output-record (child (record updating-output-record-mixin))
  (add-output-record child (sub-record record)))

(defmethod delete-output-record (child (record updating-output-record-mixin)
				 &optional (errorp t))
  (delete-output-record child (sub-record record) errorp))

(defmethod clear-output-record ((record updating-output-record-mixin))
  (clear-output-record (sub-record record)))

(defclass standard-updating-output-record (updating-output-record-mixin)
  ())

(defclass updating-output-stream-mixin ()
  ((redisplaying-p :reader stream-redisplaying-p :initform nil)
   (id-map :accessor id-map :initform nil)))

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
    ((record standard-updating-output-record)
     stream
     displayer)
  (setf (old-children record) (sub-record record))
  (multiple-value-bind (x y)
      (output-record-position record)
    (setf (values (old-x record) (old-y record)) (values x y))
    (setf (old-start-x record) (start-x record))
    (setf (old-start-y record) (start-y record))
    (setf (values (old-min-x record) (old-min-y record)
		  (old-max-x record) (old-max-y record))
	  (bounding-rectangle* record))
    (setf (output-record-parent record) nil)
    (add-output-record record (stream-current-output-record stream))
    (setf (sub-record record) (make-instance 'standard-sequence-output-record
					     :x-position x :y-position y
					     :parent record)))
  (letf (((stream-current-output-record stream) record))
    (restore-graphics-start-state record stream)
    (funcall displayer stream)))

(defgeneric compute-affected-region (record forcep))

(defmethod compute-affected-region ((record compound-output-record) forcep)
  (declare (ignore forcep))
  (let ((clear-region +nowhere+)
	(draw-region +nowhere+))
    (map-over-output-records
     #'(lambda (r)
	 (multiple-value-bind (sub-clear-region sub-draw-region)
	     (compute-affected-region r)
	   (setf clear-region (region-union clear-region sub-clear-region))
	   (setf draw-region (region-union draw-region sub-draw-region))))
     record)
    (vaules clear-region draw-region)))

(defmethod compute-affected-region ((record standard-updating-output-record)
				    forcep)
  (let ((child-updating-records nil)
	(clear-region +nowhere+)
	(draw-region +nowhere+))
    (when (dirty record)
      (map-over-output-records
       #'(lambda (r)
	   (multiple-value-bind (sub-clear-region sub-draw-region)
	       (compute-affected-region r forcep)
	     (setf clear-region (region-union clear-region sub-clear-region))
	     (when (typep r 'updating-output-record)
	       (push (cons r sub-draw-region) child-updating-records))))
       (old-children record)))
    (when (or (dirty record) forcep)
      (map-over-output-records
       #'(lambda (r)
	   (let ((cached-region (and (typep r 'updating-output-record)
				     (cdr (assoc r child-updating-records
						 :test #'eq)))))
	     (if cached-region
		 (setf draw-region (region-union draw-region cached-region))
		 (multiple-value-bind (sub-clear-region sub-draw-region)
		     (compute-affected-region r forcep)
		   (declare (ignore sub-clear-region))
		   (setf draw-region (region-union draw-region
						   sub-draw-region))))))
       (sub-record record)))
    (values clear-region draw-region)))

;;; Work in progress
#+nil
(defmethod invoke-updating-output ((stream updating-output-stream-mixin)
				   continuation
				   (record-type
				    (eql 'standard-updating-output-record))
				   unique-id id-test cache-value cache-test
				   &key (fixed-position nil) (all-new nil)
				   (parent-cache nil))
  (with-accessors ((id-map id-map))
      (or parent-cache *current-updating-output* stream)
    (let ((record (find unique-id id-map :test id-test)))
      (cond ((or all-new (null record))
	     (when record
	       (setf id-map (delete record id-map :test #'eq)))
	     ()
	     (with-output-to-output-record (stream
					    'standard-updating-output-record
					    *current-updating-output*
					    :unique-id unique-id
					    :id-test id-test
 					    :cache-value cache-value
					    :cache-test cache-test
					    :fixed-position fixed-position
					    :displayer continuation)
	       (setq record *current-updating-output*)
	       (push *current-updating-output* id-map)
	       (record-graphics-start-state *current-updating-output* stream)
	       (funcall continuation stream)
	       (record-graphics-end-state *current-updating-output* stream)))
	    ((null record)
	     (error "No output record for updating output!"))
	    ((not (funcall cache-test
			   cache-value
			   (output-record-cache-value record)))
	     (compute-new-output-records-1 record stream continuation))
	    (t (maybe-move-output-record record stream)))
      (update-parent )
      )))


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
    ((stream &rest args
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
  (letf (((stream-redisplaying-p stream) t))
    (compute-new-output-records record stream)
    (multiple-value-bind (clear-region draw-region)
	(compute-affected-region record nil)
      (with-output-recording-options (stream :record nil :draw t)
	(with-drawing-options (stream :clipping-region clear-region)
	  (with-bounding-rectangle* (x1 y1 x2 y2)
	      record
	    (draw-rectangle* stream x1 y2 x2 y2 :ink +background-ink+))))
      (replay record stream (region-union clear-region draw-region)))))

