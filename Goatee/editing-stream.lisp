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

(defclass noise-extent (extent)
  ()
  (:documentation "Characters within the extent are input editor noise
  strings.  Eventually these should be read-only and atomic."))

;;; Stream is the encapsulated stream
(defmethod initialize-instance :after ((obj goatee-input-editing-mixin)
					&key stream (initial-contents ""))
  (multiple-value-bind (cx cy)
      (stream-cursor-position stream)
    (setf (cursor-visibility (stream-text-cursor stream)) nil)
    (setf (area obj)
	  (make-instance
	   'simple-screen-area
	   :area-stream stream
	   :buffer (make-instance
		    'editable-buffer
		    :initial-contents initial-contents)
	   :x-position cx
	   :y-position cy))
    (stream-add-output-record stream (area obj))
    ;; initialize input-editing-stream state to conform to our reality
    (make-input-editing-stream-snapshot obj (area obj))))
  
(defun make-input-editing-stream-snapshot (snapshot area)
  (let ((buffer (buffer area))
	(input-buffer (stream-input-buffer snapshot)))
    (multiple-value-bind (point-line point-pos)
	(point* buffer)
      (setf (fill-pointer input-buffer) 0)
      (map-over-region #'(lambda (line pos)
			   (let ((noise nil))
			     (map-over-extents-at-location*
			      #'(lambda (extent line pos)
				  (when (typep extent 'noise-extent)
				    (if (and (eq line (line (bp-start extent)))
					     (eql pos (pos (bp-start extent))))
					(setq noise
					      climi::*noise-string-start*)
					(setq noise climi::*noise-string*))))
			      line
			      pos
			      :start-state :closed
			      :end-state :open)
			     (vector-push-extend (or noise (char-ref line pos))
						 input-buffer)))
		       buffer
		       (buffer-start buffer)
		       (buffer-end buffer))
      (setf (stream-insertion-pointer snapshot)
	    (offset-location* buffer point-line point-pos)))))

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

(defmethod climi::finalize ((stream goatee-input-editing-mixin)
			    input-sensitizer)
  (setf (cursor-visibility (cursor (area stream))) nil)
  (when input-sensitizer
    (let ((real-stream (encapsulating-stream-stream stream)))
      (funcall input-sensitizer
	       real-stream
	       #'(lambda ()
		   (let ((record (area stream)))
		     (delete-output-record record
					   (output-record-parent record))
		     (stream-add-output-record real-stream record)))))))

;;; Hopefully only used on small buffers.

(defun location*-offset (buffer offset)
  (loop for line = (dbl-head (lines buffer)) then (next line)
	for size = (and line (size line))
	while line
	summing size into total-offset
	do (when (>= total-offset offset)
	     (let ((pos (- size (- total-offset offset))))
	       (if (> pos (line-last-point line))
		   (return (values (next line) 0))
		   (return (values line pos)))))
	finally (error 'goatee-error
		       :format-control "Offset ~S is greater than the ~
  size of buffer ~S"
		       :format-arguments (list offset buffer))))

(defun offset-location* (buffer line pos)
  (loop with end-line = (location* (buffer-end buffer))
	for buf-line = (location* (buffer-start buffer)) then (next buf-line)
	until (or (eq buf-line line) (eq buf-line end-line))
	summing (size buf-line) into total-offset
	finally (progn
		  (unless (eq buf-line line)
		    (error 'goatee-error
			   :format "Location line ~S pos ~S isn't in buffer ~S"
			   :format-arguments (list line pos buffer)))
		  (return (+ total-offset pos)))))


(defmethod replace-input ((stream goatee-input-editing-mixin) new-input
			  &key
			  (start 0)
			  (end (length new-input))
			  (buffer-start nil buffer-start-supplied-p)
			  (rescan nil rescan-supplied-p))
  (unless buffer-start-supplied-p
    (if (eq stream climi::*current-input-stream*)
	(setq buffer-start climi::*current-input-position*)
	(setq buffer-start 0)))
  (let* ((scan-pointer (stream-scan-pointer stream))
	 (area (area stream))
	 (buf (buffer area))
	 (del-chars (- scan-pointer buffer-start)))
    (if (<= 0 del-chars)
	(progn
	  (with-point (buf)
	    (if (mismatch (stream-input-buffer stream) new-input
			  :start1 buffer-start :end1 scan-pointer
			  :start2 start :end2 end)
		(multiple-value-bind (line pos)
		    (location*-offset buf buffer-start)
		  (when (> del-chars 0)
		    (delete-char buf del-chars :line line :pos pos))
		  ;; location should be preserved across the delete-char, but
		  ;; it would be safest to use a buffer pointer or something...
		  (insert buf new-input
			  :line line :pos pos :start start :end end)
		  (make-input-editing-stream-snapshot stream area)
		  ;; If not rescanning, adjust scan pointer to point after new
		  ;; input
		  (if (and rescan-supplied-p (null rescan))
		      (setf (stream-scan-pointer stream)
			    (offset-location* buf
					      (line (point buf))
					      (pos (point buf))))
		      (queue-rescan stream)))
		(when rescan
		  (queue-rescan stream))))
	  ;; XXX Redundant with make-input-editing-stream-snapshot?
	  (setf (stream-insertion-pointer stream)
		(offset-location* buf (line (point buf)) (pos (point buf))))
	  (redisplay-area area))
	(warn "replace-input stream ~S: buffer-start ~S is greater than ~
               scan-pointer ~S.  Don't know how to deal with that."
	      stream
	      buffer-start
	      scan-pointer))))

;;; There used to be complicated logic here to support output when
;;; rescanning, but it seems to be very hairy to get right in
;;; combination with read-gesture's behavior upon seeing noise
;;; strings, especially with respect to peek and unread-gesture.  So, just
;;; suppress printing the noise string unless we're at the end of the
;;; buffer and can't screw anything up.

(defmethod input-editor-format ((stream goatee-input-editing-mixin)
				format-string
				&rest format-args)
  (let* ((scan-pointer (stream-scan-pointer stream))
	 (area (area stream))
	 (buf (buffer area))
	 (output (apply #'format nil format-string format-args)))
    (when (< scan-pointer (fill-pointer (stream-input-buffer stream)))
      (return-from input-editor-format nil))
    (multiple-value-bind (line pos)
	(location*-offset buf scan-pointer)
      (let ((extent (make-instance 'noise-extent
		      :start-line line :start-pos pos)))
	(with-point (buf)
	  (insert buf output :line line :pos pos))
	(setf (start-state extent) :open)
	(setf (end-state extent) :open)
	(setf (stream-scan-pointer stream)
	      (offset-location* buf
				(line (bp-end extent))
				(pos (bp-end extent))))
	(make-input-editing-stream-snapshot stream area)
	(redisplay-area area))))
  nil)
