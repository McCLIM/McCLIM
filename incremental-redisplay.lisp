;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2002 by Michael McDonald (mikemac@mikemac.com)

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

(defclass updating-output-record (output-record)
  ())

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
   (start-x)
   (start-y)
   (end-x)
   (end-y)
   ;; XXX Need to capture the "user" transformation, I think; deal with that
   ;; later.
   (old-subrecord :accessor old-children
		 :documentation "Contains the output record tree for the
  current display.")
   (id-map :accessor id-map)))

(defmethod initialize-instance :after ((obj updating-output-record-mixin)
				       &key)
  (with-slots (id-test)
      obj
    (if (or (eq id-test #'eq)
	    (eq id-test #'eql)
	    (eq id-test #'equal)
	    (eq id-test #'equalp))
	(setf (id-map obj) (make-hash-table :test id-test))
	(setf (id-map obj) nil)))
  (multiple-value-bind (x y)
      (output-record-position obj)
    (setf (sub-record obj) (make-instance 'standard-sequence-output-record
					  :x-position x :y-position y
					  :parent obj))))

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

(defclass standard-updating-output-record (updating-output-record-mixin)
  ())

(defclass updating-output-stream-mixin ()
  ((redisplaying-p :reader stream-redisplaying-p :initform nil)))

(defmethod invoke-updating-output
    (stream continuation record-type unique-id id-test cache-value cache-test
     &key all-new parent-cache)
  (declare (ignore record-type unique-id id-test cache-value cache-test all-new parent-cache))
  (funcall continuation stream))

(defmacro updating-output
    ((stream &rest args
      &key unique-id (id-test '#'eql) cache-value (cache-test '#'eql)
      fixed-position all-new parent-cache
      (record-type ''standard-updating-output-record))
     &body body)
  (declare (ignore fixed-position))
  (when (eq stream t)
    (setq stream '*standard-output*))
  (let ((func (gensym "UPDATING-OUTPUT-CONTINUATION")))
    `(flet ((,func (,stream)
	      ,@body))
       (invoke-updating-output ,stream #',func ,record-type ,unique-id
			       ,id-test ,cache-value ,cache-test
			       :all-new ,all-new
			       :parent-cache ,parent-cache))))
