;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by 
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)
;;;  (c) copyright 2001,2002 by Tim Moore (moore@bricoworks.com)

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

;;; X returns #\Return and #\Backspace where we want to see #\Newline
;;; and #\Delete at the stream-read-char level.  Dunno if this is the
;;; right place to do the transformation...

(defconstant +read-char-map+ '((#\Return . #\Newline) (#\Backspace . #\Delete)))

(defun char-for-read (char)
  (let ((new-char (cdr (assoc char +read-char-map+))))
    (or new-char char)))

(defun unmap-char-for-read (char)
  (let ((new-char (car (rassoc char +read-char-map+))))
    (or new-char char)))

;;; Streams are subclasses of standard-sheet-input-mixin regardless of
;;; whether or not we are multiprocessing.  In single-process mode the
;;; blocking calls to stream-read-char, stream-read-gesture are what
;;; cause process-next-event to be called.  It's most convenient to
;;; let process-next-event queue up events for the stream and then see
;;; what we've got after it returns.

(defclass standard-input-stream (fundamental-character-input-stream
				 standard-sheet-input-mixin)
  ((unread-chars :initform nil
		 :accessor stream-unread-chars)))

(defmethod stream-read-char ((pane standard-input-stream))
  (if (stream-unread-chars pane)
      (pop (stream-unread-chars pane))
      ;XXX
      (flet ((do-one-event (event)
	       (if (and (typep event 'key-press-event)
			(characterp (keyboard-event-key-name event)))
		   (let ((char (char-for-read (keyboard-event-key-name event))))
		     (stream-write-char pane char)
		     (return-from stream-read-char char))
		   (handle-event pane event))))
	(let* ((port (port pane))
	       (queue (frame-event-queue *application-frame*))
	       (old-count (slot-value port 'event-count)))
	  (loop
	   (let ((event (event-queue-read-no-hang queue)))
	     (if event
		 (do-one-event event)
		 (setq old-count
		       (port-wait-on-event-processing
			port
			:event-count old-count)))))))))


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
	return (char-for-read (keyboard-event-key-name event)))))

(defclass extended-input-stream (fundamental-character-input-stream
				 standard-sheet-input-mixin)
  ())

(defgeneric extended-input-stream-p (object)
  (:method ((object extended-input-stream))
    t)
  (:method ((object t))
    nil))

(defclass standard-extended-input-stream (extended-input-stream)
  ((pointer)
   (cursor :initarg :text-cursor)
   (input-buffer :accessor stream-input-buffer :initarg :input-buffer
		 :initform (make-array 1 :adjustable t :fill-pointer 0))))

(defvar *input-wait-test* nil)
(defvar *input-wait-handler* nil)
(defvar *pointer-button-press-handler* nil)

(defgeneric stream-set-input-focus (stream))

;;; XXX deal with body declarations
(defmacro with-input-focus ((stream) &body body)
  (when (eq stream t)
    (setq stream '*standard-input*))
  (let ((old-stream (gensym "OLD-STREAM")))
    `(let ((,old-stream (stream-set-input-focus ,stream)))
       (unwind-protect (progn
			 ,@body)
	 (if ,old-stream
	     (stream-set-input-focus ,old-stream)
	     (setf (port-keyboard-input-focus (port ,stream)) nil))))))


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
  (prog1
      (aref buffer 0)
    (unless peek-p
      (when (> (length buffer) 1)
	(replace buffer buffer :start2 1))
      (decf (fill-pointer buffer)))))


(defun repush-gesture (gesture buffer)
  (let ((old-fill (fill-pointer buffer)))
    (incf (fill-pointer buffer))
    (when (> old-fill 0)
      (replace buffer buffer :start1 1 :start2 0 :end2 old-fill))
    (setf (aref buffer 0) gesture)))


(defmethod convert-to-gesture ((ev event))
  nil)

(defmethod convert-to-gesture ((ev character))
  ev)

(defmethod convert-to-gesture ((ev symbol))
  ev)

(defmethod convert-to-gesture ((ev window-manager-delete-event))
  (frame-exit (pane-frame (event-sheet ev))))

(defmethod convert-to-gesture ((ev key-press-event))
  (let ((modifiers (event-modifier-state ev)))
    (if (or (zerop modifiers)
	    (eql modifiers +shift-key+))
	(keyboard-event-key-name ev)
	ev)))

(defmethod convert-to-gesture ((ev pointer-button-press-event))
  ev)

(defmethod handle-event ((stream standard-extended-input-stream) event)
  (let ((buffer (stream-input-buffer stream))
	(gesture (convert-to-gesture event)))
    (cond ((characterp gesture)
	   (case gesture
	     (#\Return
	      (setq gesture #\Newline))
	     (#\Backspace
	      (setq gesture #\Delete)))
	   (vector-push-extend gesture buffer))
	  (gesture
	   (vector-push-extend gesture buffer))
	  (t nil))))

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
	  (buffer (stream-input-buffer stream))
	  (queue (frame-event-queue *application-frame*)))
      (loop
       ;; Wait for input... or not
       ;; XXX decay timeout.
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
	      ;; input-wait-handler might leave the event for us.
	      (let ((event (event-queue-peek queue)))
		(and input-wait-handler
		     (funcall input-wait-handler stream))
		(let ((current-event (event-queue-peek queue)))
		  (when (and event (eq event current-event))
		    (event-queue-read queue)
		    (handle-event (event-sheet event) event)))))
	     (t nil))))
       ;; Something might be in the stream buffer.
       (when (> (length buffer) 0)
	 (let ((gesture (pop-gesture buffer peek-p)))
	   (if (and pointer-button-press-handler
		    (typep gesture 'pointer-button-press-event))
	       (funcall pointer-button-press-handler stream gesture)
	       (return-from stream-read-gesture gesture))))))))


(defgeneric stream-input-wait (stream &key timeout input-wait-test))

(defmethod stream-input-wait ((stream standard-extended-input-stream)
			      &key timeout input-wait-test)
  (block exit
    (let* ((buffer (stream-input-buffer stream))
	   (port (port stream))
	   (queue (frame-event-queue *application-frame*))
	   (old-count (slot-value port 'event-count))
	   reason)
      ;; Loop if not multiprocessing or if input-wait-test returns nil
      ;; XXX need to decay timeout on multiple trips through the loop
      (tagbody
       check-buffer
	 (when (> (length buffer) 0)
	   (return-from exit t))
	 (let ((event (event-queue-peek queue)))
	   (if (and input-wait-test (funcall input-wait-test stream))
	       (return-from exit (values nil :input-wait-test))
	       (when event
		 (event-queue-read queue) ;event better still be there...
		 (handle-event (event-sheet event) event)
		 (go check-buffer))))
	 ;; Event queue has been drained, time to block waiting for new events.
	 ;; If new events have appeared on the queue since our last time
	 ;; through the loop, our stale value of old-count causes
	 ;; port-wait-on-event-processing to return immediately.
	 (setf (values old-count reason)
	       (port-wait-on-event-processing port
					      :event-count old-count
					      :timeout timeout))
	 (if (not old-count)
	     (return-from exit (values nil reason))
	     (go check-buffer))))))


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
	  finally (return (char-for-read char)))))

(defmethod stream-read-char-no-hang ((stream standard-extended-input-stream))
  (with-encapsulating-stream (estream stream)
    (loop for char = (stream-read-gesture estream :timeout 0)
	  do (when (or (null char) (characterp char))
	       (loop-finish))
	  finally (return (char-for-read char)))))

(defmethod stream-unread-char ((stream standard-extended-input-stream)
			       char)
  (with-encapsulating-stream (estream stream)
    (stream-unread-gesture estream (unmap-char-for-read char))))

(defmethod stream-peek-char ((stream standard-extended-input-stream))
  (with-encapsulating-stream (estream stream)
    (loop for char = (stream-read-gesture estream :peek-p t)
	  do (if (characterp char)
		 (loop-finish)
		 (stream-read-gesture estream)) ; consume pointer gesture
	  finally (return (char-for-read char)))))

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
			      :element-type 'character
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
				   modifier-state)
  (and (eql (keyboard-event-key-name event) device-name)
       (eql (event-modifier-state event) modifer-state)))

(defmethod %event-matches-gesture ((event pointer-button-press-event)
				   type
				   device-name
				   modifier-state)
  (and (or (eql type :pointer-button-press)
	   (eql type :pointer-button))
       (eql (pointer-event-button event) device-name)
       (eql (event-modifier-state event) modifier-state)))

(defmethod %event-matches-gesture ((event pointer-button-release-event)
				   type
				   device-name
				   modifier-state)
  (and (or (eql type :pointer-button-release)
	   (eql type :pointer-button))
       (eql (pointer-event-button event) device-name)
       (eql (event-modifier-state event) modifier-state)))

(defmethod %event-matches-gesture ((event pointer-button-event)
				   type
				   device-name
				   modifier-state)
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
	do (when (eql gesture-state modifier-state)
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
