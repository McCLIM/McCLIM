;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by 
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)
;;;  (c) copyright 2001 by Tim Moore (moore@bricoworks.com)

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

(defclass standard-input-stream (fundamental-character-input-stream)
  ((unread-chars :initform nil
		 :accessor stream-unread-chars)
   )
  )

(defmethod stream-read-char ((pane standard-input-stream))
  (if (stream-unread-chars pane)
      (pop (stream-unread-chars pane))
    (loop for event = (event-read pane)
	if (and (typep event 'key-press-event)
		(characterp (keyboard-event-key-name event)))
	return (let ((char (keyboard-event-key-name event)))
		 (case char
		   (#\Return
		    (setq char #\Newline))
		   (#\Backspace
		    (setq char #\Delete)))
		 (stream-write-char pane char)
		 char)
	  else do (handle-event pane event))))

(defmethod stream-unread-char ((pane standard-input-stream) char)
  (push char (stream-unread-chars pane)))

(defmethod stream-read-char-no-hang ((pane standard-input-stream))
  (if (stream-unread-chars pane)
      (pop (stream-unread-chars pane))
    (loop for event = (event-read-no-hang pane)
	if (null event)
	   return nil
	if (and (typep event 'key-release-event)
		(characterp (keyboard-event-key-name event)))
	return (keyboard-event-key-name event))))

(defclass extended-input-stream (fundamental-character-input-stream)
  ((pointer)
   (cursor :initarg :text-cursor)))

(defgeneric extended-input-stream-p (object)
  (:method ((object extended-input-stream))
    t)
  (:method ((object t))
    nil))

(defclass standard-extended-input-stream (extended-input-stream)
  ())

(defvar *input-wait-test* nil)
(defvar *input-wait-handler* nil)
(defvar *pointer-button-press-handler* nil)

;;; XXX Do there need to be locks around access to the buffer?

(defmethod handle-event ((stream standard-extended-input-stream)
			 (event key-press-event))
  (let ((char (keyboard-event-key-name event))
	(buffer (stream-input-buffer stream)))
    (if (characterp char)
	(progn
	  (case char
	    (#\Return
	     (setq char #\Newline))
	    (#\Backspace
	     (setq char #\Delete)))
	  (event-queue-append buffer char))
	(event-queue-append buffer event))))


(defmethod handle-event ((stream standard-extended-input-stream)
			 (event pointer-button-press-event))
  (vector-push-extend event (stream-input-buffer stream)))

(defun read-gesture (&key
		     (stream *standard-input*)
		     timeout
		     peek-p
		     (input-wait-test *input-wait-test*)
		     (input-wait-handler *input-wait-handler*)
		     (pointer-button-press-handler
		      *pointer-button-press-handler*))
  (stream-read-gesture stream
		       :timeout timeout
		       :peek-p peek-p
		       :input-wait-test input-wait-test
		       :input-wait-handler input-wait-handler
		       :pointer-button-press-handler
		       pointer-button-press-handler))

(defgeneric stream-read-gesture (stream
				 &key timeout peek-p
				 input-wait-test
				 input-wait-handler
				 pointer-button-press-handler))

(defun pop-gesture (buffer peek-p)
  (if peek-p
      (event-queue-peek buffer)
    (event-queue-read buffer)))

(defun repush-gesture (gesture buffer)
  (event-queue-prepend buffer gesture))

(defmethod convert-to-gesture ((ev event))
  nil)

(defmethod convert-to-gesture ((ev character))
  ev)

(defmethod convert-to-gesture ((ev symbol))
  ev)

(defmethod convert-to-gesture ((ev key-press-event))
  (keyboard-event-key-name ev))

(defmethod stream-read-gesture ((stream standard-extended-input-stream)
				&key timeout peek-p
				(input-wait-test *input-wait-test*)
				(input-wait-handler *input-wait-handler*)
				(pointer-button-press-handler
				 *pointer-button-press-handler*))
  (with-encapsulating-stream (estream stream)
    (let ((*input-wait-test* input-wait-test)
	  (*input-wait-handler* input-wait-handler)
	  (*pointer-button-press-handler* pointer-button-press-handler)
	  (buffer (stream-input-buffer stream)))
      (loop
	(if (event-queue-listen buffer)
	    (let ((gesture (pop-gesture buffer peek-p)))
	      (when (and pointer-button-press-handler
			 (typep gesture 'pointer-button-press-event))
		(funcall pointer-button-press-handler stream gesture))
	      (setq gesture (convert-to-gesture gesture))
	      (if gesture
		  (return-from stream-read-gesture gesture)))
	  ;; Wait for input... or not
	  (multiple-value-bind (available reason)
	      (stream-input-wait estream
				 :timeout timeout
				 :input-wait-test input-wait-test)
	    (unless available
	      (case reason
		(:timeout
		 (return-from stream-read-gesture (values nil
							  :timeout)))
		(:input-wait-test
		 nil)
		(t (funcall input-wait-handler stream))))))))))


(defgeneric stream-input-wait (stream &key timeout input-wait-test))

(defmethod stream-input-wait ((stream standard-extended-input-stream)
			      &key timeout input-wait-test)
  (let ((buffer (stream-input-buffer stream)))
    (loop
     (if (event-queue-listen buffer)
	 (progn
	   (return-from stream-input-wait t))
	 (progn
	   (when (and input-wait-test (funcall input-wait-test stream))
	     (return-from stream-input-wait (values nil :input-wait-test)))
	   (multiple-value-bind (result reason)
	       ;; XXX need to decay timeout on multiple trips through the loop
	       (process-next-event (port stream) :timeout timeout)
	     (when (and (not result) (eq reason :timeout))
	       (return-from stream-input-wait (values nil :timeout)))))))))


(defun unread-gesture (gesture &key (stream *standard-input*))
  (stream-unread-gesture stream gesture))

(defgeneric stream-unread-gesture (stream gesture))

(defmethod stream-unread-gesture ((stream standard-extended-input-stream)
				  gesture)
  (with-encapsulating-stream (estream stream)
    (repush-gesture gesture (stream-input-buffer estream))))

;;; Standard stream methods on standard-extended-input-stream.  Ignore any
;;; pointer gestures in the input buffer.

(defmethod stream-read-char ((stream standard-extended-input-stream))
  (with-encapsulating-stream (estream stream)
    (loop for char = (stream-read-gesture estream)
	  until (characterp char)
	  finally (return char))))

(defmethod stream-read-char-no-hang ((stream standard-extended-input-stream))
  (with-encapsulating-stream (estream stream)
    (loop for char = (stream-read-gesture estream :timeout 0)
	  do (when (or (null char) (characterp char))
	       (loop-finish))
	  finally (return char))))

(defmethod stream-unread-char ((stream standard-extended-input-stream)
			       char)
  (with-encapsulating-stream (estream stream)
    (stream-unread-gesture estream char)))

(defmethod stream-peek-char ((stream standard-extended-input-stream))
  (with-encapsulating-stream (estream stream)
    (loop for char = (stream-read-gesture estream :peek-p t)
	  do (if (characterp char)
		 (loop-finish)
		 (stream-read-gesture estream)) ; consume pointer gesture
	  finally (return char))))

(defmethod stream-listen ((stream standard-extended-input-stream))
  (with-encapsulating-stream (estream stream)
    (loop
     (if (stream-input-wait estream :timeout 0)
	 (let ((gesture (stream-read-gesture estream :peek-p t)))
	   (if (characterp gesture)
	       (return-from stream-listen t)
	       (stream-read-gesture estream))) ; consume pointer gesture
	 (return-from stream-listen nil)))))

(defmethod stream-read-line ((stream standard-extended-input-stream))
  (with-encapsulating-stream (estream stream)
    (let ((result (make-array 1
			      :element-type character
			      :adjustable t
			      :fill-pointer 0)))
      (loop for char = (stream-read-char estream)
	    while (not (char= char #\Newline))
	    do (vector-push-extend char result)
	    finally (return (subseq result 0))))))

;;; Gestures

(defparameter *gesture-names* (make-hash-table))

(defmacro define-gesture-name (name type gesture-spec &key (unique t))
  `(add-gesture-name ',name ',type ',gesture-spec ,@(and unique
							 `(:unique ',unique))))

;;; XXX perhaps this should be in the backend somewhere?
(defconstant +name-to-char+ '((:newline . #\newline)
			      (:linefeed . #\linefeed)
			      (:return . #\return)
			      (:tab . #\tab)
			      (:backspace . #\backspace)
			      (:page . #\page)
			      (:rubout . #\rubout)))

(defun add-gesture-name (name type gesture-spec &key unique)
  (destructuring-bind (device-name . modifiers)
      gesture-spec
    (let* ((modifier-state (apply #'make-modifier-state modifiers)))
      (cond ((and (eq type :keyboard)
		  (symbolp device-name))
	     (let ((real-device-name (cdr (assoc device-name +name-to-char+))))
	       (unless real-device-name
		 (error "~S is not a known key name" device-name))
	       (setq device-name real-device-name)))
	    ((and (member type '(:pointer-button
				 :pointer-button-press
				 :pointer-button-release)
			  :test #'eq))
	     (let ((real-device-name
		    (case device-name
		      (:left +pointer-left-button+)
		      (:middle +pointer-middle-button+)
		      (:right +pointer-right-button+)
		      (t (error "~S is not a known button")))))
	       (setq device-name real-device-name))))
      (let ((gesture-entry (list type device-name modifier-state)))
	(if unique
	    (setf (gethash name *gesture-names*) (list gesture-entry))
	    (push gesture-entry (gethash name *gesture-names*)))))))


(defgeneric %event-matches-gesture (event type device-name modifier-state))

(defmethod %event-matches-gesture (event type device-name modifier-state)
  nil)

(defmethod %event-matches-gesture ((event key-press-event)
				   (type (eql :keyboard))
				   device-name
				   modifer-state)
  (and (eql (keyboard-event-key-name event) device-name)
       (eql (event-modifier-state event) modifer-state)))

(defmethod %event-matches-gesture ((event pointer-button-press-event)
				   type
				   device-name
				   modifer-state)
  (and (or (eql type :pointer-button-press)
	   (eql type :pointer-button))
       (eql (pointer-event-button event) device-name)
       (eql (event-modifier-state event) modifier-state)))

(defmethod %event-matches-gesture ((event pointer-button-release-event)
				   type
				   device-name
				   modifer-state)
  (and (or (eql type :pointer-button-release)
	   (eql type :pointer-button))
       (eql (pointer-event-button event) device-name)
       (eql (event-modifier-state event) modifier-state)))

(defmethod %event-matches-gesture ((event pointer-button-event)
				   type
				   device-name
				   modifer-state)
  (and (or (eql type :pointer-button-press)
	   (eql type :pointer-button-release)
	   (eql type :pointer-button))
       (eql (pointer-event-button event) device-name)
       (eql (event-modifier-state event) modifier-state)))

;;; Because gesture objects are either characters or event objects, support
;;; characters here too.

(defmethod %event-matches-gesture ((event character)
				   (type (eql :keyboard))
				   device-name
				   modifier-state)
  (and (eql event device-name)
       (eql modifier-state 0)))

(defun event-matches-gesture-name-p (event gesture-name)
  (let ((gesture-entry (gethash gesture-name *gesture-names*)))
    (loop for (type device-name modifier-state) in gesture-entry
	  do (when (%event-matches-gesture event
					   type
					   device-name
					   modifier-state)
	       (return-from event-matches-gesture-name-p t))
	  finally (return nil))))

(defun modifier-state-matches-gesture-name-p (modifier-state gesture-name)
  (loop for (type device-name gesture-state) in (gethash gesture-name
							 *gesture-names*)
	do (when (eql gesture-state modifer-state)
	     (return-from modifier-state-matches-gesture-name-p t))
	finally (return nil)))


(defun make-modifier-state (&rest modifiers)
  (loop for result = 0 then (logior (case modifier
				      (:shift +shift-key+)
				      (:control +control-key+)
				      (:meta +meta-key+)
				      (:super +super-key+)
				      (:hyper +hyper-key+)
				      (t (error "~S is not a known modifier")))
				    result)
	for modifier in modifiers
	finally (return result)))

;;; Standard gesture names

(define-gesture-name :abort :keyboard (#\c :control))
(define-gesture-name :clear-input :keyboard (#\u :control))
(define-gesture-name :complete :keyboard (:tab))
(define-gesture-name :help :keyboard (#\/ :control))

(define-gesture-name :select :pointer-button-press (:left))
(define-gesture-name :describe :pointer-button-press (:middle))
(define-gesture-name :menu :pointer-button-press (:right))
(define-gesture-name :edit :pointer-button-press (:left :meta))
(define-gesture-name :delete :pointer-button-press (:middle :shift))

;;; Define so we have a gesture for #\newline that we can use in
;;; *standard-activation-gestures*

(define-gesture-name :newline :keyboard (#\newline))
(define-gesture-name :return :keyboard (#\return))
