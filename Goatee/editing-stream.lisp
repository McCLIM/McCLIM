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

(defmethod cursor-visibility ((stream goatee-input-editing-mixin))
  (cursor-visibility (area stream)))

(defmethod (setf cursor-visibility) (visibility
				     (stream goatee-input-editing-mixin))
  (setf (cursor-visibility (area stream)) visibility))

(defclass noise-extent (extent)
  ()
  (:documentation "Characters within the extent are input editor noise
  strings.  Eventually these should be read-only and atomic."))

(defclass accept-result-extent (extent)
  ((object :accessor object :initarg :object)
   (result-type :accessor result-type :initarg :result-type))
  (:documentation "The extent is read with a single read-gesture;
  result is returned."))

;;; Stream is the encapsulated stream
(defmethod initialize-instance :after ((obj goatee-input-editing-mixin)
				       &rest args
				       &key stream (initial-contents "")
				       (cursor-visibility t)
				       (background-ink
					(medium-background stream))
				       single-line)
  (multiple-value-bind (cx cy)
      (stream-cursor-position stream)
    (let ((max-width (- (stream-text-margin stream) cx)))
      ;; XXX hack to give area a fixed size rectangle that can be highlighted
      (with-output-recording-options (stream :record t)
	(draw-rectangle* stream cx cy
			 (+ cx max-width) (+ cy (stream-line-height stream))
			 :ink background-ink
			 :filled t))
      (climi::with-keywords-removed (args (:initial-contents :single-line))
	(setf (area obj)
	      (apply #'make-instance
		     'simple-screen-area
		     :area-stream stream
		     :buffer (make-instance 'editable-buffer
					    :initial-contents
					    initial-contents
					    :newline-character (if single-line
								   nil
								   #\Newline))
		     :x-position cx
		     :y-position cy
		     :cursor-visibility cursor-visibility
		     :max-width max-width
		     :allow-other-keys t
		     args)))
      ;; XXX Really add it here?
      (stream-add-output-record stream (area obj))
      #+nil (redisplay-area (area obj))
      ;; initialize input-editing-stream state to conform to our reality
      (make-input-editing-stream-snapshot obj (area obj)))))

(defvar climi::*noise-string-start*)
(defvar climi::*noise-string*)

(defun make-input-editing-stream-snapshot (snapshot area)
  (let ((buffer (buffer area))
	(input-buffer (stream-input-buffer snapshot)))
    (multiple-value-bind (point-line point-pos)
	(point* buffer)
      (setf (fill-pointer input-buffer) 0)
      (map-over-region
       #'(lambda (line pos)
	   (let ((noise nil))
	     (map-over-extents-at-location*
	      #'(lambda (extent line pos)
		  (cond ((typep extent 'noise-extent)
			 (if (and (eq line (line (bp-start extent)))
				  (eql pos (pos (bp-start extent))))
			     (setq noise climi::*noise-string-start*)
			     (setq noise climi::*noise-string*)))
			((typep extent 'accept-result-extent)
			 (if (and (eq line (line (bp-start extent)))
				  (eql pos (pos (bp-start extent))))
			     (setq noise extent)
			     (setq noise climi::*noise-string*)))))
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

(defmethod update-input-editing-stream ((stream goatee-input-editing-mixin))
  (let ((area (area stream))
	(snapshot (snapshot stream)))
    (make-input-editing-stream-snapshot snapshot area)
    (let ((first-mismatch (mismatch (stream-input-buffer snapshot)
				    (stream-input-buffer stream)))
	  (snapshot-buffer (stream-input-buffer snapshot))
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
		 :start2 first-mismatch))
      first-mismatch)))

(defmethod stream-process-gesture ((stream goatee-input-editing-mixin)
				   gesture
				   type)
  (declare (ignore type))
  (when (activation-gesture-p gesture)
    (setf (stream-insertion-pointer stream)
	  (fill-pointer (stream-input-buffer stream)))
    (set-editing-stream-insertion-pointer stream
					  (stream-insertion-pointer stream))
    (setf (climi::activation-gesture stream) gesture)
    (rescan-if-necessary stream)
    (return-from stream-process-gesture gesture))
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
		  ;; Throw out. Buffer is still updated by protect forms
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
		  (queue-rescan stream)
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

(defun reposition-stream-cursor (stream)
  "Moves the cursor somewhere clear of Goatee's editing area."
  (let ((max-y 0))
    (map-over-output-records #'(lambda (r)
                                 (setf max-y (max max-y (bounding-rectangle-max-y r))))
                             (stream-output-history stream))
    (setf (stream-cursor-position stream)
          (values 0 max-y))))
                                                

(defmethod climi::finalize ((stream goatee-input-editing-mixin)
			    input-sensitizer)
  (call-next-method)
  (setf (cursor-visibility (cursor (area stream))) nil)
  (let ((real-stream (encapsulating-stream-stream stream))
	(record (area stream)))  
    (when input-sensitizer
      (erase-output-record record real-stream)
      (funcall input-sensitizer
	       real-stream
	       #'(lambda ()
		   (stream-add-output-record real-stream record)
		   (when (stream-drawing-p real-stream)
		     #+nil (format *trace-output* "Redisplaying ~S~&" record)
		     (replay record real-stream)))))
    (reposition-stream-cursor real-stream)))

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

(defgeneric set-editing-stream-insertion-pointer (stream pointer))

(defmethod set-editing-stream-insertion-pointer
    ((stream goatee-input-editing-mixin) pointer)
  (let* ((area (area stream))
	 (buffer (buffer area)))
    (setf (point* buffer) (location*-offset buffer pointer))
    (redisplay-area area)))

(defmethod (setf stream-insertion-pointer) :after
    ((new-value integer) (stream goatee-input-editing-mixin))
  (set-editing-stream-insertion-pointer stream new-value))

(defun %replace-input (stream new-input start end buffer-start
		       rescan rescan-supplied-p
		       extent-class &rest extent-args)
  (let* ((scan-pointer (stream-scan-pointer stream))
	 (area (area stream))
	 (buf (buffer area))
	 (del-chars (- scan-pointer buffer-start)))
    (when (stream-rescanning-p stream)
      (return-from %replace-input nil))
    (if (<= 0 del-chars)
	(progn
	  (with-point (buf)
	    (multiple-value-bind (line pos)
		(location*-offset buf buffer-start)
	      (when (> del-chars 0)
		(delete-char buf del-chars :line line :pos pos))
	      ;; location should be preserved across the delete-char, but
	      ;; it would be safest to use a buffer pointer or something...
	      (let ((extent (and extent-class
				 (apply #'make-instance extent-class
					:start-line line :start-pos pos
					extent-args))))
		(insert buf new-input
			:line line :pos pos :start start :end end)
		(when extent
		  (setf (start-state extent) :open)
		  (setf (end-state extent) :open))
		(make-input-editing-stream-snapshot stream area)
		;; If not rescanning, adjust scan pointer to point after new
		;; input
		(if (and rescan-supplied-p (null rescan))
		    (setf (stream-scan-pointer stream)
			  (offset-location* buf
					    (line (point buf))
					    (pos (point buf))))
		    (queue-rescan stream)))))
	  ;; XXX Redundant with make-input-editing-stream-snapshot?
	  (setf (stream-insertion-pointer stream)
		(offset-location* buf (line (point buf)) (pos (point buf))))
	  (redisplay-area area))
	(warn "replace-input stream ~S: buffer-start ~S is greater than ~
               scan-pointer ~S.  Don't know how to deal with that."
	      stream
	      buffer-start
	      scan-pointer))))

(defvar climi::*current-input-stream*)
(defvar climi::*current-input-position*)

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
  (%replace-input stream new-input
		  start end buffer-start rescan rescan-supplied-p nil))

(defun present-acceptably-to-string
    (object type view for-context-type)
  (flet ((present-it (acceptably)
	   (present-to-string object type
			      :view view
			      :acceptably acceptably
			      :for-context-type for-context-type)))
    (let* ((acceptably t)
	   (printed-rep nil))
      (handler-case
	  (setq printed-rep (present-it t))
	(error ()
	  (setq acceptably nil)
	  (setq printed-rep (present-it nil))))
      (values printed-rep (if acceptably
			      nil
			      object)))))

(defmethod presentation-replace-input
    ((stream goatee-input-editing-mixin)
     object type view
     &key
     (buffer-start nil buffer-start-supplied-p)
     (rescan nil rescan-supplied-p)
     query-identifier
     (for-context-type type))
  (declare (ignore query-identifier))
  (multiple-value-bind (printed-rep accept-object)
      (present-acceptably-to-string object type view
				    for-context-type)
    (unless buffer-start-supplied-p
      (if (eq stream climi::*current-input-stream*)
	  (setq buffer-start climi::*current-input-position*)
	  (setq buffer-start 0)))
      (apply #'%replace-input stream printed-rep
	     0 (length printed-rep) buffer-start rescan rescan-supplied-p 
	     (if accept-object
		 `(accept-result-extent :object ,accept-object
		                        :result-type ,type)
		 '(nil)))))


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
    (when (stream-rescanning-p stream)
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

(defmethod redraw-input-buffer ((stream goatee-input-editing-mixin) &optional (start-position 0))
  (declare (ignore start-position))
  (redisplay-area (area stream)))

(defmethod erase-input-buffer ((stream goatee-input-editing-mixin)
                               &optional (start-position 0))
  (declare (ignore start-position))
  (clear-output-record (area stream)))
