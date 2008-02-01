;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2001 by 
;;;           Tim Moore (moore@bricoworks.com)
;;;  (c) copyright 2006 by
;;           Troels Henriksen (athas@sigkill.dk)

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

;;; Finalization of the implementation of the Goatee-based
;;; input-editing-stream. This probably doesn't work perfectly.

(in-package :clim-internals)

(defclass goatee-input-editing-stream (empty-input-mixin
                                       goatee:goatee-input-editing-mixin
                                       standard-input-editing-mixin
                                       input-editing-stream
                                       standard-encapsulating-stream)
  ((buffer :reader stream-input-buffer
	   :initform (make-array 16 :adjustable t :fill-pointer 0))
   (insertion-pointer :accessor stream-insertion-pointer :initform 0)
   (scan-pointer :accessor stream-scan-pointer :initform 0)
   (rescan-queued :accessor rescan-queued :initform nil)
   (rescanning-p :reader stream-rescanning-p :initform nil)
   (activation-gesture :accessor activation-gesture :initform nil)))

(defmethod interactive-stream-p ((stream goatee-input-editing-stream))
  t)

(defmethod stream-accept ((stream goatee-input-editing-stream) type
			  &rest args
			  &key (view (stream-default-view stream))
			  &allow-other-keys)
  (apply #'prompt-for-accept stream type view args)
  (apply #'accept-1 stream type args))

;;; Have to reexamine how many of the keyword arguments to
;;; stream-read-gesture should really be passed to the encapsulated
;;; stream.
;;;
;;; OK, now I know :) They should all be passed, except for peek-p.
;;; However, the loop that calls stream-read-gesture on the
;;; encapsulated stream needs to return null if we see a :timeout or
;;; :eof.
;;;
;;; Activation gesture handling has been moved out of
;;; stream-process-gesture to stream-read-gesture and
;;; stream-unread-gesture. This allows a gesture to be read in while
;;; it is not an activation gesture, unread, and then read again as an
;;; activation gesture. This kind of game seems to be needed for
;;; reading forms properly. -- moore
(defmethod stream-read-gesture ((stream goatee-input-editing-stream)
				&rest rest-args &key peek-p
				&allow-other-keys)
  (with-keywords-removed (rest-args (:peek-p))
    (rescan-if-necessary stream)
    (with-slots (buffer insertion-pointer scan-pointer activation-gesture)
	stream
      (loop
	 (loop
	    while (< scan-pointer insertion-pointer)
	    do (let ((gesture (aref buffer scan-pointer)))
		 ;; Skip noise strings.
		 ;; XXX We should skip accept results too; I think that they
		 ;; should be consumed by ACCEPT-1. That's not happening yet.
		 (cond ((characterp gesture)
			(unless peek-p
                          (incf scan-pointer))
			(return-from stream-read-gesture gesture))
		       ((and (not peek-p)
			     (typep gesture 'goatee::accept-result-extent))
			(incf scan-pointer)
			(throw-object-ptype (goatee::object gesture)
					    (goatee::result-type gesture)))
		       (t (incf scan-pointer)))))
	 ;; The scan pointer should not be greater than the insertion pointer
	 ;; because the code that set the insertion pointer should have queued
	 ;; a rescan.
	 (when (> scan-pointer insertion-pointer)
	   (warn "scan-pointer ~S > insertion-pointer ~S; shouldn't happen"
		 scan-pointer insertion-pointer)
	   (immediate-rescan stream))
	 (when activation-gesture
	   (return-from stream-read-gesture
	     (prog1
		 activation-gesture
	       (unless peek-p
		 (setf activation-gesture nil)))))
	 (setf (slot-value stream 'rescanning-p) nil)
	 ;; In McCLIM stream-process-gesture is responsible for inserting
	 ;; characters into the buffer, changing the insertion pointer and
	 ;; possibly setting up the activation-gesture slot.
	 (loop
	    with gesture and type 
	    do (setf (values gesture type)
		     (apply #'stream-read-gesture
			    (encapsulating-stream-stream stream) rest-args))
	    when (null gesture)
            do (return-from stream-read-gesture (values gesture type))
	    when (stream-process-gesture stream gesture type)
            do (loop-finish))))))

(defmethod stream-unread-gesture ((stream goatee-input-editing-stream)
				  gesture)
  (with-slots (buffer scan-pointer activation-gesture)
      stream
    (when (> scan-pointer 0)
      (if (and (eql scan-pointer (fill-pointer buffer))
	       (activation-gesture-p gesture))
	  (setf activation-gesture gesture)
	  (decf scan-pointer)))))

(defmethod activate-stream ((stream goatee-input-editing-stream) gesture)
  (setf (activation-gesture stream) gesture)
  (setf (stream-insertion-pointer stream)
	(fill-pointer (stream-input-buffer stream)))
  (goatee::set-editing-stream-insertion-pointer
   stream
   (stream-insertion-pointer stream)))

(defmethod reset-scan-pointer ((stream goatee-input-editing-stream)
			       &optional (scan-pointer 0))
  (setf (stream-scan-pointer stream) scan-pointer)
  (setf (slot-value stream 'rescanning-p) t))

(defmethod immediate-rescan ((stream goatee-input-editing-stream))
  (signal 'rescan-condition))

(defmethod queue-rescan ((stream goatee-input-editing-stream))
  (setf (rescan-queued stream) t))

(defmethod rescan-if-necessary ((stream goatee-input-editing-stream)
				&optional inhibit-activation)
  (declare (ignore inhibit-activation))
  (when (rescan-queued stream)
    (setf (rescan-queued stream) nil)
    (immediate-rescan stream)))

(defmethod input-editing-stream-output-record ((stream goatee-input-editing-stream))
  (area stream))
