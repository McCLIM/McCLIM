;;; -*- Mode: Lisp; Package: GOATEE -*-

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

(in-package :goatee)

;;; Interface between input-editing-streams and Goatee areas.

(defclass editing-stream-snapshot ()
  ((buffer :reader stream-input-buffer
	   :initform (make-array 16 :adjustable t :fill-pointer 0))
   (insertion-pointer :accessor stream-insertion-pointer :initform 0)
   (scan-pointer :accessor stream-scan-pointer :initform 0)))
  
(defclass goatee-input-editing-mixin ()
  ((area :accessor area :initarg :area)
   (snapshot :accessor snapshot :initarg :snapshot
	     :initform (make-instance 'editing-stream-snapshot))))

;;; Stream is the encapsulated stream
(defmethod initialize-instance :after ((obj goatee-input-editing-mixin)
					&key stream (initial-contents ""))
  (multiple-value-bind (cx cy)
      (stream-cursor-position stream)
    (setf (cursor-visibility (stream-text-cursor stream)) nil)
    (setf (area obj)
	  (make-instance
	   'simple-screen-area
	   :stream stream
	   :buffer (make-instance
		    'editable-buffer
		    :initial-contents initial-contents)
	   :x-position cx
	   :y-position cy))
    (stream-add-output-record stream (area obj))
    ;; initialize input-editing-stream state to conform to our reality
    (make-input-editing-stream-snapshot obj (area obj))))
  

(defun make-input-editing-stream-snapshot (snapshot area)
  (let ((buffer (buffer area)))
    (buffer-string buffer :result (stream-input-buffer snapshot))
    (multiple-value-bind (point-line pos)
	(point* buffer)
      (loop for line = (dbl-head (lines buffer)) then (next line)
	    until (eq line point-line)
	    sum (size line) into ip
	    finally (setf (stream-insertion-pointer snapshot) (+ ip pos))))))

(defmethod stream-process-gesture ((stream goatee-input-editing-mixin)
				   gesture
				   type)
  (let ((area (area stream))
	(snapshot (snapshot stream)))
    (execute-gesture-command gesture area *simple-area-gesture-table*)
  (make-input-editing-stream-snapshot snapshot area)
  (let ((first-mismatch (mismatch (stream-input-buffer snapshot)
				  (stream-input-buffer stream))))
    (unwind-protect
	 (cond ((null first-mismatch)
		;; No change actually took place, event though IP may have
		;; moved. 
		nil)
	       ((< first-mismatch (stream-scan-pointer stream))
		(immediate-rescan stream))
	       ((and (eql first-mismatch
			  (1- (stream-insertion-pointer snapshot)))
		     (eql (aref (stream-input-buffer snapshot) first-mismatch)
			  gesture))
		;; As best we can tell an insertion happened: one gesture was
		;; entered it was inserted in the buffer.  There may be other
		;; changes above IP, but we don't care.
		gesture)
	       (t
		;; Other random changes, but we want to allow more editing
		;; before scanning them.
		nil))
      (let ((snapshot-buffer (stream-input-buffer snapshot))
	    (stream-buffer (stream-input-buffer stream)))
	(setf (stream-insertion-pointer stream)
	      (stream-insertion-pointer snapshot))
	(when (< (car (array-dimensions stream-buffer))
		 (fill-pointer snapshot-buffer))
	  (adjust-array stream-buffer (fill-pointer snapshot-buffer)))
	(setf (fill-pointer stream-buffer) (fill-pointer snapshot-buffer))
	(when (and first-mismatch
		   (>= (fill-pointer snapshot-buffer) first-mismatch))
	  (replace stream-buffer snapshot-buffer
		   :start1 first-mismatch
		   :start2 first-mismatch)))))))
