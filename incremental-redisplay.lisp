;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2002 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2002,2003 by Tim Moore (moore@bricoworks.com)

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
    (x y (record updating-output-record-mixin))
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

(defmethod print-object ((obj standard-updating-output-record) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (when (slot-boundp obj 'x1)
      (with-slots (x1 y1 x2 y2) obj
	(format stream "X ~S:~S Y ~S:~S " x1 x2 y1 y2))
      (format stream "~S" (output-record-dirty obj)))))
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

(defgeneric find-child-output-record (record use-old-elements record-type
				      &rest initargs
				      &key unique-id unique-id-test))

(defgeneric find-equal-display-record (root use-old-elements record))

(defmethod find-equal-display-record ((root standard-updating-output-record)
				      use-old-elements
				      record)
  (cond ((eq (output-record-dirty root) :clean)
	 nil)
	(use-old-elements
	 (when (slot-boundp root 'old-children)
	   (find-equal-display-record (old-children root)
				      use-old-elements
				      record)))
	(t (find-equal-display-record (sub-record root)
				      use-old-elements
				      record))))

(defmethod find-equal-display-record ((root compound-output-record)
				      use-old-elements
				      record)
  (map-over-output-records-overlapping-region
   #'(lambda (r)
       (let ((result (find-equal-display-record r use-old-elements record)))
	 (when result
	   (return-from find-equal-display-record result))))
   root
   record)
  nil)

(defmethod find-equal-display-record ((root displayed-output-record)
				      use-old-elements
				      record)
  (declare (ignore use-old-elements))
  (if (output-record-equal root record)
      root
      nil))

(defgeneric map-over-displayed-output-records
    (function root use-old-elements clean)
  (:documentation "Call function on all displayed-output-records in ROOT's
 tree, respecting use-old-elements."))

(defmethod map-over-displayed-output-records (function
					      (root standard-updating-output-record)
					      use-old-elements
					      clean)
  (cond ((and (not clean) (eq (output-record-dirty root) :clean))
	 nil)
	((and use-old-elements (slot-boundp root 'old-children))
	 (map-over-displayed-output-records function
					    (old-children root)
					    use-old-elements
					    clean))
	((not use-old-elements)
	 (map-over-displayed-output-records function
					    (sub-record root)
					    use-old-elements
					    clean))
	(t nil)))

(defmethod map-over-displayed-output-records (function
					      (root compound-output-record)
					      use-old-elements
					      clean)
  (flet ((mapper (record)
	   (map-over-displayed-output-records function
					      record
					      use-old-elements
					      clean)))
    (declare (dynamic-extent #'mapper))
    (map-over-output-records #'mapper root)))

(defmethod map-over-displayed-output-records (function
					      (root displayed-output-record)
					      use-old-elements
					      clean)
  (declare (ignore clean))
  (declare (ignore use-old-elements))
  (funcall function root))

(defgeneric compute-difference-set (record &optional check-overlapping
					   offset-x offset-y
					   old-offset-x old-offset-y))

(defmethod compute-difference-set ((record standard-updating-output-record)
				   &optional check-overlapping
				   offset-x offset-y
				   old-offset-x old-offset-y)
  (declare (ignore check-overlapping offset-x offset-y
		   old-offset-x old-offset-y))
  (let ((existing-output-records (make-hash-table :test #'eq))
	(draws nil)
	(erases nil))
    ;; Find which new output records are already on screen
    (map-over-displayed-output-records
     #'(lambda (r)
	 (let ((old (find-equal-display-record record t r)))
	   (if old
	       (setf (gethash old existing-output-records) r)
	       (push r draws))))
     record
     nil
     nil)
    ;; Find old records that should be erased
    (map-over-displayed-output-records
     #'(lambda (r)
	 (unless (gethash r existing-output-records)
	   (push r erases)))
     record
     t
     nil)
    (values erases nil draws nil nil)))

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
	     (compute-new-output-records-1 record stream continuation)
	     (setf (slot-value record 'cache-value) cache-value))
	    (t
	     ;; It doesn't need to be updated, but it does go into the
	     ;; parent's sequence of records
	     (setf (output-record-dirty record) :clean)
	     (setf (output-record-parent record) nil)
	     (add-output-record record (stream-current-output-record stream))
	     (set-medium-graphics-state(end-graphics-state record) stream)))
      record)))

; &key (unique-id (gensym)) was used earlier,
; changed to (unique-id `',(gensym)) as per gilham's request
; please CHECKME and delete this comment :]

(defun force-update-cache-test (a b)
  (declare (ignore a b))
  nil)

(defmacro updating-output
    ((stream
      &key (unique-id `',(gensym)) (id-test '#'eql)
      (cache-value ''no-cache-value cache-value-supplied-p)
      (cache-test '#'eql)
      (fixed-position nil fixed-position-p)
      (all-new nil all-new-p)
      (parent-cache nil parent-cache-p)
      (record-type ''standard-updating-output-record))
     &body body)
  (when (eq stream t)
    (setq stream '*standard-output*))
  (unless cache-value-supplied-p
    (setq cache-test '#'force-update-cache-test))
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
    (let ((*current-updating-output* record))
      (compute-new-output-records record stream)
      (multiple-value-bind (erases moves draws)
	  (compute-difference-set record)
	(declare (ignore moves))
	(with-output-recording-options (stream :record nil :draw t)
	  (loop for r in erases
		do (with-bounding-rectangle* (x1 y1 x2 y2)
		       r
		     (draw-rectangle* stream x1 y1 x2 y2
				      :ink +background-ink+)))
	  (loop for r in draws
		do (with-bounding-rectangle* (x1 y1 x2 y2)
		       r
		     (draw-rectangle* stream x1 y1 x2 y2
				      :ink +background-ink+))))
	;; Redraw all the regions that have been erased.  This takes care of all
	;; random records that might overlap.
	(loop for r in erases
	      do (replay record stream r))
	(loop for r in draws
	      do (replay record stream r))))))

(defun convert-from-relative-to-absolute-coordinates (stream record)
  (let ((scy (if stream (bounding-rectangle-height stream) 0.0d0))
        (scx (if stream (bounding-rectangle-width stream) 0.0d0))
        (ory 0.0d0)
        (orx 0.0d0))
    (if record (multiple-value-setq (ory orx) (output-record-position record)))
    (values (+ scy ory) (+ scx orx))))
