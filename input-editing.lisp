;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2001 by 
;;;           Tim Moore (moore@bricoworks.com)

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

;;; Stub of input editing so we can see what we're doing and make
;;; progress on ACCEPT

(defvar *activation-gestures* nil)
(defvar *standard-activation-gestures* '(newline))

(defvar *delimiter-gestures* nil)

(defclass input-editing-stream ()
  ())

(defgeneric input-editing-stream-p (stream)
  (:method ((stream input-editing-stream))
    t)
  (:method ((stream t))
    nil))

(defclass standard-input-editing-stream (input-editing-stream
					 standard-encapsulating-stream)
  ((buffer :reader stream-input-buffer
	   :initform (make-array 16 :adjustable t :fill-pointer 0))
   (insertion-pointer :accessor stream-insertion-pointer :initform 0)
   (scan-pointer :accessor stream-scan-pointer :initform 0)
   (rescan-queued :accessor rescan-queued :initform nil)
   (rescanning-p :reader stream-rescanning-p :initform nil)))

(defclass input-editing-noise-string ()
  ((noise-string :accessor noise-string :initarg :noise-string)))

;;; Have to reexamine how many of the keyword arguments to stream-read-gesture
;;; should really be passed to the encapsulated stream.
(defmethod stream-read-gesture ((stream standard-input-editing-stream)
				&rest rest-args &key peek-p
				&allow-other-keys)
  (remf rest-args :peek-p)
  (rescan-if-necessary stream)
  (with-slots (buffer insertion-pointer scan-pointer)
      stream
    (loop
     (if (< scan-pointer insertion-pointer)
	 (return-from stream-read-gesture
	   (prog1
	       (aref buffer scan-pointer)
	     (unless peek-p
	       (incf scan-pointer))))
	 (multiple-value-bind (gesture type)
	     (apply #'stream-read-gesture
		    (encapsulating-stream-stream stream)
		    rest-args)
	   (let ((result (stream-process-gesture stream gesture type)))
	     (when result
	       	(vector-push-extend result (stream-input-buffer stream))
		(incf insertion-pointer))))))))

(defmethod stream-unread-gesture ((stream standard-input-editing-stream)
				  gesture)
  (declare (ignore gesture))
  (when (> (stream-scan-pointer stream) 0)
    (decf (stream-scan-pointer stream))))

(defgeneric stream-process-gesture (stream gesture type))

;;; This will get beefed up, obviously

(defmethod stream-process-gesture ((stream standard-input-editing-stream)
				   gesture type)
  (if (characterp gesture)
      (progn
	(when (encapsulating-stream-stream stream)
	  (write-char gesture stream))
	gesture)))

(defmacro with-activation-gestures ((gestures &key override) &body body)
  ;; XXX Guess this implies that gestures need to be defined at compile time.
  ;; Sigh.
  (let ((gesture-form (if (and (symbolp gestures)
			       (gethash gestures *gesture-names*))
			  `(list ',gestures)
			  gestures))
	(gestures (gensym)))
    `(let* ((,gestures ,gesture-form)
	    (*activation-gestures* (if ,override
				       ,gestures
				       (append ,gestures
					       *activation-gestures*))))
       ,@body)))


(defun activation-gesture-p (gesture)
  (loop for gesture-name in *activation-gestures*
	when (event-matches-gesture-name-p gesture gesture-name)
	do (return t)
	finally (return nil)))

(defun delimiter-gesture-p (gesture)
  (loop for gesture-name in *delimiter-gestures*
	when (event-matches-gesture-name-p gesture gesture-name)
	do (return t)
	finally (return nil)))

(defmacro with-input-editing ((&optional (stream t)
			       &key input-sensitizer initial-contents
			       (class ''standard-input-editing-stream))
			      &body body)
  (if (eq stream t)
      (setq stream '*standard-input*))
  `(invoke-with-input-editing ,stream
			      #'(lambda (,stream) ,@body)
			      ,input-sensitizer ,initial-contents ,class))

(define-condition rescan-condition (condition)
  ())

(defun invoke-with-input-editing (stream
				  continuation
				  input-sensitizer
				  initial-contents
				  class)
  (declare (ignore input-sensitizer initial-contents))
  (with-activation-gestures (*standard-activation-gestures*)
    (if (typep stream 'extended-input-stream)
	(let ((editing-stream (make-instance class :stream stream))
	      (first-time t))
	  (loop
	   (if first-time
	       (setq first-time nil)
	       (reset-scan-pointer stream))
	   (block rescan
	     (handler-bind ((rescan-condition #'(lambda (c)
						  (declare (ignore c))
						  (return-from rescan nil))))
	       (return-from invoke-with-input-editing
		 (multiple-value-prog1
		     (funcall continuation editing-stream)
		   ;; The user can still do editing, which will blow out of here
		   ;; and repeat the main loop again.
		   (loop for gesture = (stream-read-gesture editing-stream)
			 until (activation-gesture-p gesture))))))))
	(funcall continuation stream))))

(defgeneric reset-scan-pointer (stream &optional scan-pointer))

(defmethod reset-scan-pointer (stream &optional (scan-pointer 0))
  (setf (stream-scan-pointer stream) scan-pointer)
  (setf (slot-value stream 'rescanning-p) t))

(defgeneric immediate-rescan (stream))

(defmethod immediate-rescan ((stream standard-input-editing-stream))
  (signal 'rescan-condition))

(defgeneric queue-rescan (stream))

(defmethod queue-rescan ((stream standard-input-editing-stream))
  (setf (rescan-queued stream) t))

(defgeneric rescan-if-necessary (stream &optional inhibit-activation))

(defmethod rescan-if-necessary ((stream standard-input-editing-stream)
				&optional inhibit-activation)
  (declare (ignore inhibit-activation))
  (when (rescan-queued stream)
    (setf (rescan-queued stream) nil)
    (immediate-rescan stream)))

(defgeneric input-editor-format (stream format-string &rest format-args))

(defmethod input-editor-format ((stream t) format-string &rest format-args)
  (apply #'format stream format-string format-args))

(defun make-room (buffer pos n)
  (let ((fill (fill-pointer buffer)))
    (when (> (+ fill n)
	     (array-dimension buffer 0))
      (adjust-array buffer (list (+ fill n))))
    (incf (fill-pointer buffer) n)
    (replace buffer buffer :start1 (+ pos n) :start2 pos :end2 fill)))


(defmethod input-editor-format ((stream standard-input-editing-stream)
				format-string
				&rest format-args)
  (let ((noise (make-instance 'input-editing-noise-string
			      :noise-string (apply #'format
						   nil
						   format-string
						   format-args))))
    (with-slots (buffer insertion-pointer scan-pointer)
	stream
      (if (< scan-pointer (fill-pointer buffer))
	  (progn
	    (make-room buffer scan-pointer 1)
	    (setf (aref buffer scan-pointer) noise))
	  (vector-push-extend noise buffer))
      (when (>= insertion-pointer scan-pointer)
	(incf insertion-pointer))
      (incf scan-pointer))
    (when (encapsulating-stream-stream stream)
      (write-string (noise-string noise)   stream))))


(defun read-token (stream &key
		   (input-wait-handler *input-wait-handler*)
		   (pointer-button-press-handler
		    *pointer-button-press-handler*)
		   click-only)
  (declare (ignore click-only))		;XXX For now
  (let ((result (make-array 1
			    :adjustable t
			    :fill-pointer 0
			    :element-type 'character))
	(in-quotes nil))
    ;; The spec says that read-token ignores delimiter gestures if the
    ;; first character is #\", until it sees another.  OK... what about
    ;; other occurences of #\"?  Guess we'll just accumulate them.
    (loop for first-char = t then nil
	  for gesture = (read-gesture
			 :stream stream
			 :input-wait-handler input-wait-handler
			 :pointer-button-press-handler
			 pointer-button-press-handler)
	  do (cond ((or (activation-gesture-p gesture)
			(typep gesture 'pointer-button-event)
			(and (not in-quotes)
			     (delimiter-gesture-p gesture)))
		    (loop-finish))
		   ((characterp gesture)
		    (if (eql gesture #\")
			(cond (first-char
			       (setq in-quotes t))
			      (in-quotes
			       (setq in-quotes nil))
			      (t (vector-push-extend gesture result)))
			(vector-push-extend gesture result)))
		   (t nil))
	  finally (progn
		    (unread-gesture gesture :stream stream)
		    (return (subseq result 0))))))
