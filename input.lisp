;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2002 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>

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

;; Event queues

(defclass standard-event-queue ()
  ((lock :initform (make-lock "event queue")
         :reader event-queue-lock)
   (head :initform nil
         :accessor event-queue-head
         :documentation "Head pointer of event queue.")
   (tail :initform nil
         :accessor event-queue-tail
         :documentation "Tail pointer of event queue.") ))

(defmethod event-queue-read-no-hang ((eq standard-event-queue))
  "Reads one event from the queue, if there is no event just return NIL."
  (with-lock-held ((event-queue-lock eq))
    (let ((res (pop (event-queue-head eq))))
      (when (null (event-queue-head eq))
        (setf (event-queue-tail eq) nil))
      res)))

(defmethod event-queue-read ((eq standard-event-queue))
  "Reads one event from the queue, if there is no event, hang until here is one."
  (loop
      (let ((res (event-queue-read-no-hang eq)))
        (when res
          (return res))
        (process-wait  "Waiting for event"
                       (lambda ()
                         (not (null (event-queue-head eq))))))))

(defmethod event-queue-append ((eq standard-event-queue) item)
  "Append the item at the end of the queue."
  (with-lock-held ((event-queue-lock eq))
    (cond ((null (event-queue-tail eq))
           (setf (event-queue-head eq) (cons item nil)
                 (event-queue-tail eq) (event-queue-head eq)))
          (t
           (setf (event-queue-tail eq)
                 (setf (cdr (event-queue-tail eq)) (cons item nil)))))))

(defmethod event-queue-prepend ((eq standard-event-queue) item)
  "Prepend the item to the beginning of the queue."
  (with-lock-held ((event-queue-lock eq))
    (cond ((null (event-queue-tail eq))
           (setf (event-queue-head eq) (cons item nil)
                 (event-queue-tail eq) (event-queue-head eq)))
          (t
           (push item (event-queue-head eq))))))

(defmethod event-queue-peek ((eq standard-event-queue))
  (with-lock-held ((event-queue-lock eq))
    (first (event-queue-head eq))))

(defmethod event-queue-peek-if (predicate (eq standard-event-queue))
  "Goes thru the whole event queue an returns the first event, which
   satisfies 'predicate' and leaves the event in the queue.
   Returns NIL, if there is no such event."
  (with-lock-held ((event-queue-lock eq))
    (find-if predicate (event-queue-head eq))))

(defmethod event-queue-listen ((eq standard-event-queue))
  (not (null (event-queue-head eq))))

;; STANDARD-SHEET-INPUT-MIXIN

(defclass standard-sheet-input-mixin ()
  ((queue :initform (make-instance 'standard-event-queue)
	  :reader sheet-event-queue)
   (port :initform nil
	 :initarg :port
	 :reader port)
   ))

(defmethod stream-input-buffer ((stream standard-sheet-input-mixin))
  (sheet-event-queue stream))

;(defmethod dispatch-event ((sheet standard-sheet-input-mixin) event)
;  (if (typep event 'device-event)
;      (queue-event sheet event)
;    (handle-event sheet event)))

(defmethod dispatch-event ((sheet standard-sheet-input-mixin) event)
  (handle-event sheet event))

(defmethod dispatch-event ((sheet standard-sheet-input-mixin) (event device-event))
  (queue-event sheet event))

(defmethod queue-event ((sheet standard-sheet-input-mixin) event)
  (with-slots (queue) sheet
    (event-queue-append queue event)))
  
(defmethod handle-event ((sheet standard-sheet-input-mixin) event)
  ;; Standard practice is too ignore events
  (declare (ignore event))
  nil)

(defmethod event-read ((sheet standard-sheet-input-mixin))
  (with-slots (queue) sheet
    (event-queue-read queue)))

(defmethod event-read-with-timeout ((sheet standard-sheet-input-mixin)
				    &key (timeout nil) (wait-function nil))
  ;; This one is not in the spec ;-( --GB
  (with-slots (queue) sheet
    (event-queue-read queue)))

(defmethod event-read-no-hang ((sheet standard-sheet-input-mixin))
  (with-slots (queue) sheet
    (event-queue-read-no-hang queue)))

(defmethod event-peek ((sheet standard-sheet-input-mixin) &optional event-type)
  (with-slots (queue) sheet
    (if event-type
        (event-queue-peek-if (lambda (x)
                               (typep x event-type))
                             queue)
        (event-queue-peek-if (lambda (x) (declare (ignore x)) t)
                             queue))))

(defmethod event-unread ((sheet standard-sheet-input-mixin) event)
  (with-slots (queue) sheet
    (event-queue-prepend queue event)))

(defmethod event-listen ((sheet standard-sheet-input-mixin))
  (with-slots (queue) sheet
    (event-queue-listen queue)))

;;;;

(defclass immediate-sheet-input-mixin (standard-sheet-input-mixin)
  (
   ))

(defmethod dispatch-event ((sheet immediate-sheet-input-mixin) event)
  (handle-event sheet event))

(define-condition sheet-is-mute-for-input (error)
    (
     ))

(defclass sheet-mute-input-mixin ()
  (
   ))

(defmethod dispatch-event ((sheet sheet-mute-input-mixin) event)
  (declare (ignore event))
  (error 'sheet-is-mute-for-input))

(defmethod queue-event ((sheet sheet-mute-input-mixin) event)
  (declare (ignore event))
  (error 'sheet-is-mute-for-input))

(defmethod handle-event ((sheet sheet-mute-input-mixin) event)
  (declare (ignore event))
  (error 'sheet-is-mute-for-input))

(defmethod event-read ((sheet sheet-mute-input-mixin))
  (error 'sheet-is-mute-for-input))

(defmethod event-read-with-timeout ((sheet sheet-mute-input-mixin)
				    &key (timeout nil) (wait-function nil))
  (declare (ignore timeout wait-function))
  (error 'sheet-is-mute-for-input))

(defmethod event-read-no-hang ((sheet sheet-mute-input-mixin))
  (error 'sheet-is-mute-for-input))

(defmethod event-peek ((sheet sheet-mute-input-mixin) &optional event-type)
  (declare (ignore event-type))
  (error 'sheet-is-mute-for-input))

(defmethod event-unread ((sheet sheet-mute-input-mixin) event)
  (declare (ignore event))
  (error 'sheet-is-mute-for-input))

(defmethod event-listen ((sheet sheet-mute-input-mixin))
  (error 'sheet-is-mute-for-input))

;;;;

(defclass delegate-sheet-input-mixin ()
  ((delegate :initform nil
	     :initarg :delegate
	     :accessor delegate-sheet-delegate) ))

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

