;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2002 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 2014 by Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2018 by Daniel Kochmanski (daniel@turtleware.eu)

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

(in-package :clim-internals)

;;; Input Protocol Classes

;; Event queues

(defstruct schedule-entry
  time event)

(defclass schedule-mixin ()
  ((schedule-time
    :initform nil
    :accessor event-schedule-time
    :documentation "The next time an event should be scheduled.")
   (schedule
    :initform nil
    :accessor event-queue-schedule
    :documentation "Time ordered queue of events to schedule."))
  (:documentation "Experimental timer event extension."))

(defun now ()
  (/ (get-internal-real-time)
     1.0
     internal-time-units-per-second))

;; Given two alarm times compute remaining time (in seconds). If both times are
;; NIL returns NIL.
(defun compute-decay (time-1 time-2)
  (cond ((and time-1 time-2) (- (min time-1 time-2) (now)))
        (time-1 (- time-1 (now)))
        (time-2 (- time-2 (now)))
        (T NIL)))

(declaim (inline now compute-decay))

;; See if it's time to inject a scheduled event into the queue.
(defgeneric check-schedule (queue)
  (:method ((queue schedule-mixin))
    (alexandria:when-let* ((schedule-time (event-schedule-time queue))
                           (execute-p (>= (now) schedule-time))
                           (entry (pop (event-queue-schedule queue))))
      (if-let ((next-entry (first (event-queue-schedule queue))))
        (setf (event-schedule-time queue) (schedule-entry-time next-entry))
        (setf (event-schedule-time queue) nil))
      (event-queue-append queue (schedule-entry-event entry)))))

(defgeneric schedule-event-queue (queue event delay)
  (:method ((queue schedule-mixin) event delay)
    (with-slots (schedule) queue
      (let ((alarm (+ (now) delay)))
        (cond
          ((null schedule)
           (push (make-schedule-entry :time alarm :event event) schedule)
           (setf (event-schedule-time queue) alarm))
          ((< alarm (event-schedule-time queue))
           (push (make-schedule-entry :time alarm :event event) schedule)
           (setf (event-schedule-time queue) alarm))
          (t
           (do* ((previous schedule (rest previous))
                 (current  (rest schedule) (rest current))
                 (entry #1=(first current) #1#))
                ((or (null current)
                     (< alarm (schedule-entry-time entry)))
                 (setf (cdr previous)
                       (list* (make-schedule-entry :time alarm :event event) (cdr previous)))))))))))

;;; EVENT-QUEUE protocol (not exported)

(define-protocol-class event-queue () ())

(defgeneric event-queue-read (event-queue)
  (:documentation "Reads one event from the queue, if there is no event, hang
until here is one."))

(defgeneric event-queue-read-no-hang (event-queue)
  (:documentation "Reads one event from the queue, if there is no event just
return NIL."))

(defgeneric event-queue-read-with-timeout (event-queue timeout wait-function)
  (:documentation "Reads one event from the queue, if there is no event before
timeout returns NIL."))

(defgeneric event-queue-append (event-queue item)
  (:documentation "Append the item at the end of the queue. Does event compression."))

(defgeneric event-queue-prepend (event-queue item)
  (:documentation "Prepend the item to the beginning of the queue."))

(defgeneric event-queue-peek (event-queue)
  (:documentation "Peeks the first event in a queue. Queue is left unchanged.
If queue is empty returns NIL."))

(defgeneric event-queue-peek-if (predicate event-queue)
  (:documentation "Goes through the whole event queue and returns the first
event, which satisfies PREDICATE. Queue is left unchanged. Returns NIL if there
is no such event."))

(defgeneric event-queue-listen (event-queue)
  (:documentation "Returns true if there are any events in the queue. Otherwise
returns NIL."))

(defgeneric event-queue-listen-or-wait (event-queue &key timeout)
  (:documentation "Returns true if there are any events in the queue. Otherwise
blocks until event arives or timeout is reached. If there is no event before
timeout returns NIL."))


(defclass simple-event-queue (event-queue schedule-mixin)
  ((head :initform nil
         :accessor event-queue-head
         :documentation "Head pointer of event queue.")
   (tail :initform nil
         :accessor event-queue-tail
         :documentation "Tail pointer of event queue.")
   (port :initform nil
         :initarg  :port
         :accessor event-queue-port
         :documentation "The port which will be generating events for the queue."))
  (:documentation "Event queue which works under assumption that there are no
concurrent threads. Most notably it calls process-next-event by itself. Doesn't
use condition-variables nor locks."))

;;; We want to force port output just in case we wait for an event yet to be
;;; performed by a user (which may be prompted on a screen). -- jd 2018-12-26
(defun do-port-force-output (port-event-queue)
  (when-let ((port (event-queue-port port-event-queue)))
    (port-force-output port)))

(defun %event-queue-read (queue)
  (check-schedule queue)
  (let ((res (pop (event-queue-head queue))))
    (when (null (event-queue-head queue))
      (setf (event-queue-tail queue) nil))
    res))

(defmethod event-queue-read ((queue simple-event-queue))
  (do-port-force-output queue)
  (let ((decay (compute-decay nil (event-schedule-time queue)))
        (port (event-queue-port queue)))
    (when decay (maxf decay 0))
    (loop
       as result = (%event-queue-read queue)
       if result do (return result)
       else do (process-next-event port :timeout decay))))

(defmethod event-queue-read-no-hang ((queue simple-event-queue))
  (do-port-force-output queue)
  (%event-queue-read queue))

(defmethod event-queue-read-with-timeout ((queue simple-event-queue) timeout wait-function)
  (do-port-force-output queue)
  (when wait-function
    (warn "EVENT-QUEUE-READ-WITH-TIMEOUT: ignoring WAIT-FUNCTION."))
  (loop
     with timeout-time = (and timeout (+ timeout (now)))
     with port = (event-queue-port queue)
     as decay = (compute-decay timeout-time (event-schedule-time queue))
     do
       (when decay (maxf decay 0))
       (if-let ((event (%event-queue-read queue)))
         (return event)
         (if (and timeout-time (> (now) timeout-time))
             (return nil)
             (process-next-event port :wait-function wait-function :timeout decay)))))

(defmethod event-queue-append ((queue simple-event-queue) item)
  (labels ((append-event ()
             (cond ((null (event-queue-tail queue))
                    (setf (event-queue-head queue) (cons item nil)
                          (event-queue-tail queue) (event-queue-head queue)))
                   (t
                    (setf (event-queue-tail queue)
                          (setf (cdr (event-queue-tail queue)) (cons item nil))))))
           (event-delete-if (predicate)
             (when (not (null (event-queue-head queue)))
               (setf (event-queue-head queue)
                     (delete-if predicate (event-queue-head queue))
                     (event-queue-tail queue)
                     (last (event-queue-head queue))))))
    (typecase item
      ;;
      ;; Motion Event Compression
      ;;
      ;; . find the (at most one) motion event
      ;; . delete it
      ;; . append item to queue
      ;;
      ;; But leave enter/exit events.
      ;;
      ((and pointer-motion-event (not pointer-boundary-event))
       (let ((sheet (event-sheet item)))
         (event-delete-if
          #'(lambda (x)
              (and (typep x 'pointer-motion-event)
                   (not (typep x 'pointer-boundary-event))
                   (eq (event-sheet x) sheet))))
         (append-event)))
      ;;
      ;; Resize event compression
      ;;
      (window-configuration-event
       (when (typep (event-sheet item) 'top-level-sheet-pane)
         (let ((sheet (event-sheet item)))
           (event-delete-if
            #'(lambda (ev)
                (and (typep ev 'window-configuration-event)
                     (eq (event-sheet ev) sheet)))))
         (append-event)))
      ;;
      ;; Repaint event compression
      ;;
      (window-repaint-event
       (let ((region (window-event-native-region item))
             (sheet  (event-sheet item))
             (did-something-p nil))
         (labels ((fun (xs)
                    (cond ((null xs)
                           ;; We reached the queue's tail: Append the
                           ;; new event, construct a new one if
                           ;; necessary.
                           (when did-something-p
                             (setf item
                                   (make-instance 'window-repaint-event
                                                  :timestamp (event-timestamp item)
                                                  :sheet     (event-sheet item)
                                                  :region    region)))
                           (setf (event-queue-tail queue) (cons item nil)) )
                          ((and (typep (car xs) 'window-repaint-event)
                                (eq (event-sheet (car xs)) sheet))
                           ;; This is a repaint event for the same
                           ;; sheet, delete it and combine its region
                           ;; into the new event.
                           (setf region
                                 (region-union region
                                               (window-event-native-region (car xs))))
                           ;; Here is an alternative, which just takes
                           ;; the bounding rectangle.
                           ;; NOTE: When doing this also take care that
                           ;; the new region really is cleared.
                           ;; (setf region
                           ;;   (let ((old-region (window-event-native-region (car xs))))
                           ;;     (make-rectangle*
                           ;;      (min (bounding-rectangle-min-x region)
                           ;;           (bounding-rectangle-min-x old-region))
                           ;;      (min (bounding-rectangle-min-y region)
                           ;;           (bounding-rectangle-min-y old-region))
                           ;;      (max (bounding-rectangle-max-x region)
                           ;;           (bounding-rectangle-max-x old-region))
                           ;;      (max (bounding-rectangle-max-y region)
                           ;;           (bounding-rectangle-max-y old-region)))))
                           (setf did-something-p t)
                           (fun (cdr xs)))
                          ;;
                          (t
                           (setf (cdr xs) (fun (cdr xs)))
                           xs))))
           (setf (event-queue-head queue) (fun (event-queue-head queue))))))
      ;; Regular events are just appended:
      (otherwise
       (append-event)))))

(defmethod event-queue-prepend ((queue simple-event-queue) item)
  (if (null (event-queue-tail queue))
      (setf (event-queue-head queue) (cons item nil)
            (event-queue-tail queue) (event-queue-head queue))
      (push item (event-queue-head queue))))

(defmethod event-queue-peek ((queue simple-event-queue))
  (do-port-force-output queue)
  (check-schedule queue)
  (or (first (event-queue-head queue))
      (when (process-next-event (event-queue-port queue) :timeout 0)
        (first (event-queue-head queue)))))

(defmethod event-queue-peek-if (predicate (queue simple-event-queue))
  (do-port-force-output queue)
  ;; Slurp as many elements as available.
  (loop until (null (process-next-event (event-queue-port queue) :timeout 0)))
  ;; Check-schedule may append scheduled event in a queue.
  (check-schedule queue)
  (find-if predicate (event-queue-head queue)))

(defmethod event-queue-listen-or-wait ((queue simple-event-queue) &key timeout)
  (do-port-force-output queue)
  (loop
     with timeout-time = (and timeout (+ timeout (now)))
     with port = (event-queue-port queue)
     as decay = (compute-decay timeout-time (event-schedule-time queue))
     do
       (when decay (maxf decay 0))
       (check-schedule queue)
       (cond ((event-queue-head queue)
              (return t))
             ((and timeout-time (> (now) timeout-time))
              (return nil))
             (T
              (process-next-event port :timeout decay)))))

(defmethod event-queue-listen ((queue simple-event-queue))
  (event-queue-listen-or-wait queue :timeout 0))


(defclass concurrent-event-queue (simple-event-queue)
  ((lock :initform (make-recursive-lock "event queue")
         :reader event-queue-lock)
   (processes
         :initform (make-condition-variable)
         :accessor event-queue-processes
         :documentation "Condition variable for waiting processes")))

(defmethod check-schedule :around ((queue concurrent-event-queue))
  (with-recursive-lock-held ((event-queue-lock queue))
    (call-next-method)))

(defmethod schedule-event-queue :around ((queue concurrent-event-queue) event delay)
  (with-recursive-lock-held ((event-queue-lock queue))
    (call-next-method)))

(defmethod event-queue-read ((queue concurrent-event-queue))
  (do-port-force-output queue)
  (let ((lock (event-queue-lock queue))
        (cv (event-queue-processes queue)))
    (loop
       as decay = (compute-decay nil (event-schedule-time queue))
       do (with-recursive-lock-held (lock)
            (if-let ((result (%event-queue-read queue)))
              (return result)
              (condition-wait cv lock decay))))))

(defmethod event-queue-read-no-hang ((queue concurrent-event-queue))
  (do-port-force-output queue)
  (with-recursive-lock-held ((event-queue-lock queue))
    (%event-queue-read queue)))

;;; XXX Should we do something with the wait function? I suspect that
;;; it's not compatible with the brave new world of native threads.

(defmethod event-queue-read-with-timeout ((queue concurrent-event-queue)
                                          timeout wait-function)
  (do-port-force-output queue)
  (when wait-function
    (warn "EVENT-QUEUE-READ-WITH-TIMEOUT: ignoring WAIT-FUNCTION."))
  ;; We need to LOOP because of possible spurious wakeup (sbcl/bt quirk).
  (loop
     with lock = (event-queue-lock queue)
     with cv = (event-queue-processes queue)
     with timeout-time = (and timeout (+ timeout (now)))
     as decay = (compute-decay timeout-time (event-schedule-time queue))
     do
       (when decay (maxf decay 0))
       (with-recursive-lock-held (lock)
         (if-let ((event (%event-queue-read queue)))
           (return event)
           (if (and timeout-time (> (now) timeout-time))
               (return nil)
               (condition-wait cv lock decay))))))

(defmethod event-queue-append ((queue concurrent-event-queue) item)
  (with-recursive-lock-held ((event-queue-lock queue))
    (call-next-method)
    (condition-notify (event-queue-processes queue))))

(defmethod event-queue-prepend ((queue concurrent-event-queue) item)
  (with-recursive-lock-held ((event-queue-lock queue))
    (call-next-method)
    (condition-notify (event-queue-processes queue))))

(defmethod event-queue-peek ((queue concurrent-event-queue))
  (do-port-force-output queue)
  (with-recursive-lock-held ((event-queue-lock queue))
    (check-schedule queue)
    (first (event-queue-head queue))))

(defmethod event-queue-peek-if (predicate (queue concurrent-event-queue))
  (do-port-force-output queue)
  (with-recursive-lock-held ((event-queue-lock queue))
    (find-if predicate (event-queue-head queue))))

(defmethod event-queue-listen ((queue concurrent-event-queue))
  (do-port-force-output queue)
  (check-schedule queue)
  (not (null (event-queue-head queue))))

(defmethod event-queue-listen-or-wait ((queue concurrent-event-queue) &key timeout)
  (do-port-force-output queue)
  ;; We need to LOOP because of possible spurious wakeup (sbcl/bt quirk).
  (loop
     with lock = (event-queue-lock queue)
     with cv = (event-queue-processes queue)
     with timeout-time = (and timeout (+ timeout (now)))
     as decay = (compute-decay timeout-time (event-schedule-time queue))
     do
       (when decay (maxf decay 0))
       (with-recursive-lock-held (lock)
         (check-schedule queue)
         (cond ((event-queue-head queue)
                (return t))
               ((and timeout-time (> (now) timeout-time))
                (return nil))
               (T
                (condition-wait cv lock decay))))))


;;; STANDARD-SHEET-INPUT-MIXIN

(defclass standard-sheet-input-mixin ()
  ((queue :initform (if *multiprocessing-p*
                        (make-instance 'concurrent-event-queue)
                        (make-instance 'simple-event-queue))
          :reader sheet-event-queue
          :initarg :input-buffer)
   (port :initform nil
         :initarg :port
         :reader port)))

(defmethod stream-input-buffer ((stream standard-sheet-input-mixin))
  (sheet-event-queue stream))

(defmethod (setf stream-input-buffer) (new-val
                                       (stream standard-sheet-input-mixin))
  (setf (slot-value stream 'queue) new-val))

(defmethod dispatch-event ((sheet standard-sheet-input-mixin) event)
  (queue-event sheet event))

(defmethod queue-event ((sheet standard-sheet-input-mixin) event)
  (with-slots (queue) sheet
    (event-queue-append queue event)))

(defmethod schedule-event ((sheet standard-sheet-input-mixin) event delay)
  (with-slots (queue) sheet
    (schedule-event-queue queue event delay)))

(defmethod handle-event ((sheet standard-sheet-input-mixin) event)
  ;; Standard practice is to ignore events
  (declare (ignore event))
  nil)

(defmethod event-read ((sheet standard-sheet-input-mixin))
  (with-slots (queue) sheet
    (event-queue-read queue)))

(defgeneric clim-extensions:event-read-with-timeout (sheet &key timeout wait-function)
  (:method ((sheet standard-sheet-input-mixin)
            &key (timeout nil) (wait-function nil))
    ;; This one is not in the spec ;-( --GB
    (with-slots (queue) sheet
      (event-queue-read-with-timeout queue timeout wait-function))))

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

;;; Support for callers that want to set an event queue for every pane.

(defclass no-event-queue-mixin ()
  ())

(defmethod initialize-instance :after ((obj no-event-queue-mixin)
                                       &key input-buffer)
  (declare (ignore input-buffer))
  nil)

(defmethod (setf stream-input-buffer) (new-val (stream no-event-queue-mixin))
  new-val)

(defclass immediate-sheet-input-mixin (no-event-queue-mixin)
  ())

(defmethod dispatch-event ((sheet immediate-sheet-input-mixin) event)
  (handle-event sheet event))

(defmethod handle-event ((sheet immediate-sheet-input-mixin) event)
  (declare (ignore event))
  nil)

(define-condition sheet-is-mute-for-input (error)
    ())

(defclass sheet-mute-input-mixin (no-event-queue-mixin)
  ())

(defmethod dispatch-event ((sheet sheet-mute-input-mixin) event)
  (declare (ignore event))
  (error 'sheet-is-mute-for-input))

(defmethod queue-event ((sheet sheet-mute-input-mixin) event)
  (declare (ignore event))
  (error 'sheet-is-mute-for-input))

(defmethod schedule-event ((sheet sheet-mute-input-mixin) event delay)
  (declare (ignore event delay))
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

;;; Don't know if this event queue stuff is completely right, or if it
;;; matters much...

(defmethod initialize-instance :after ((obj delegate-sheet-input-mixin)
                                       &key input-buffer)
  (declare (ignore input-buffer)))

(defmethod stream-input-buffer ((stream delegate-sheet-input-mixin))
  (sheet-event-queue (delegate-sheet-delegate stream)))

(defmethod (setf stream-input-buffer) (new-val
                                       (stream delegate-sheet-input-mixin))
  (setf (stream-input-buffer (delegate-sheet-delegate stream)) new-val))

(defmethod dispatch-event ((sheet delegate-sheet-input-mixin) event)
  (dispatch-event (delegate-sheet-delegate sheet) event))

(defmethod queue-event ((sheet delegate-sheet-input-mixin) event)
  (queue-event (delegate-sheet-delegate sheet) event))

(defmethod schedule-event ((sheet delegate-sheet-input-mixin) event delay)
  (schedule-event (delegate-sheet-delegate sheet) event delay))

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

;;; Class actually used by panes.

(defclass clim-sheet-input-mixin (standard-sheet-input-mixin)
  ())
