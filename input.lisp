;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)

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

;;; Input Protocol Classes

(defclass standard-sheet-input-mixin ()
  ((queue :initform nil)
   (port :initform nil
	 :initarg :port
	 :reader port)
   ))

(defmethod dispatch-event ((sheet standard-sheet-input-mixin) event)
  (if (typep event 'device-event)
      (queue-event sheet event)
    (handle-event sheet event)))

(defmethod queue-event ((sheet standard-sheet-input-mixin) event)
  (with-slots (queue) sheet
    (setq queue (nconc queue (list event)))))
  
(defmethod handle-event ((sheet standard-sheet-input-mixin) event)
  ;; Standard practice is too ignore events
  (declare (ignore event))
  nil)

(defmethod event-read ((sheet standard-sheet-input-mixin))
  (with-slots (queue) sheet
    (loop while (null queue)
	  do (process-next-event (port sheet)))
    (pop queue)))

(defmethod event-read-with-timeout ((sheet standard-sheet-input-mixin)
				    &key (timeout nil) (wait-function nil))
  (with-slots (queue) sheet
    (loop while (null queue)
	  do (process-next-event (port sheet) :timeout timeout :wait-function wait-function))
    (pop queue)))

(defmethod event-read-no-hang ((sheet standard-sheet-input-mixin))
  (with-slots (queue) sheet
    (if (null queue)
	   nil
      (pop queue))))

(defmethod event-peek ((sheet standard-sheet-input-mixin) &optional event-type)
  (with-slots (queue) sheet
    (if event-type
	(loop while (and queue (not (typep (first queue) event-type)))
	    do (pop queue)))
    (if (null queue)
	nil
      (first queue))))

(defmethod event-unread ((sheet standard-sheet-input-mixin) event)
  (with-slots (queue) sheet
    (push event queue)))

(defmethod event-listen ((sheet standard-sheet-input-mixin))
  (with-slots (queue) sheet
    (not (null queue))))

(defclass immediate-sheet-input-mixin (standard-sheet-input-mixin)
  (
   ))

(defmethod dispatch-event ((sheet immediate-sheet-input-mixin) event)
  (handle-event sheet event))

(define-condition sheet-is-mute-for-input (error)
    (
     ))

(defclass mute-sheet-input-mixin ()
  (
   ))

(defmethod dispatch-event ((sheet mute-sheet-input-mixin) event)
  (declare (ignore event))
  (error 'sheet-is-mute-for-input))

(defmethod queue-event ((sheet mute-sheet-input-mixin) event)
  (declare (ignore event))
  (error 'sheet-is-mute-for-input))

(defmethod handle-event ((sheet mute-sheet-input-mixin) event)
  (declare (ignore event))
  (error 'sheet-is-mute-for-input))

(defmethod event-read ((sheet mute-sheet-input-mixin))
  (error 'sheet-is-mute-for-input))

(defmethod event-read-with-timeout ((sheet mute-sheet-input-mixin)
				    &key (timeout nil) (wait-function nil))
  (declare (ignore timeout wait-function))
  (error 'sheet-is-mute-for-input))

(defmethod event-read-no-hang ((sheet mute-sheet-input-mixin))
  (error 'sheet-is-mute-for-input))

(defmethod event-peek ((sheet mute-sheet-input-mixin) &optional event-type)
  (declare (ignore event-type))
  (error 'sheet-is-mute-for-input))

(defmethod event-unread ((sheet mute-sheet-input-mixin) event)
  (declare (ignore event))
  (error 'sheet-is-mute-for-input))

(defmethod event-listen ((sheet mute-sheet-input-mixin))
  (error 'sheet-is-mute-for-input))

(defclass delegate-sheet-input-mixin ()
  ((delegate :initform nil
	     :initarg :delegate
	     :accessor delegate-sheet-delegate)
   ))

(defmethod dispatch-event ((sheet delegate-sheet-input-mixin) event)
  (dispatch-event (delegate-sheet-delegate sheet) event))

(defmethod queue-event ((sheet delegate-sheet-input-mixin) event)
  (queue-event (delegate-sheet-delegate sheet) event))

(defmethod handle-event ((sheet delegate-sheet-input-mixin) event)
  (handle-event (delegate-sheet-delegate sheet) event))

(defmethod event-read ((sheet delegate-sheet-input-mixin))
  (event-read (delegate-sheet-delegate sheet)))

(defmethod event-read-with-timeout ((sheet delegate-sheet-input-mixin)
				    &key (timeout nil) (wait-function nil))
  (event-read-with-timeout (delegate-sheet-delegate sheet)
			   :timeout timeout :wait-function wait-function))

(defmethod event-read-no-hang ((sheet delegate-sheet-input-mixin))
  (event-read-no-hang (delegate-sheet-delegate sheet)))

(defmethod event-peek ((sheet delegate-sheet-input-mixin) &optional event-type)
  (event-peek (delegate-sheet-delegate sheet) event-type))

(defmethod event-unread ((sheet delegate-sheet-input-mixin) event)
  (event-unread (delegate-sheet-delegate sheet) event))

(defmethod event-listen ((sheet delegate-sheet-input-mixin))
  (event-listen (delegate-sheet-delegate sheet)))

