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

(in-package :clim-internals)

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
         :documentation "Tail pointer of event queue.")
   (processes
         :initform (make-condition-variable)
	 :accessor event-queue-processes
	 :documentation "Condition variable for waiting processes")
   ;; experimental extension for scheduled event insersion
   (schedule-time
         :initform nil
         :accessor event-schedule-time
         :documentation "The next time an event should be scheduled.")
   (schedule
         :initform nil
         ;; :accessor event-queue-schedule
         ;; this accessor conflicts with the method below.
         ;; noted by mikemac. I recommend renaming the slot.
         ;; --GB 2002-11-10
         :documentation "Time ordered queue of events to schedule.")))

(defclass port-event-queue (standard-event-queue)
  ((port :initform nil
         :initarg  :port
         :accessor event-queue-port
         :documentation "The port which will be generating events for the queue.")))

(defmethod event-queue-read-no-hang/locked ((eq standard-event-queue))
  (check-schedule eq)
  (let ((res (pop (event-queue-head eq))))
    (when (null (event-queue-head eq))
      (setf (event-queue-tail eq) nil))
    res))

(defmethod event-queue-read-no-hang ((eq standard-event-queue))
  "Reads one event from the queue, if there is no event just return NIL."
  (with-lock-held ((event-queue-lock eq))
    (event-queue-read-no-hang/locked eq)))

(defmethod event-queue-read ((eq standard-event-queue))
  "Reads one event from the queue, if there is no event, hang until here is one."
  (let ((lock (event-queue-lock eq)))
    (with-lock-held (lock)
      (loop
	 (check-schedule eq)
	 (let ((res (event-queue-read-no-hang/locked eq)))
	   (when res
	     (return res))
	   ;; is there an event waiting to be scheduled?
	   (with-slots (schedule-time) eq
	     (let* ((now    (now))
		    (timeout (when schedule-time (- schedule-time now))))
	       (condition-wait (event-queue-processes eq) lock timeout))))))))

;;; XXX Should we do something with the wait function? I suspect that
;;; it's not compatible with the brave new world of native threads.

(defmethod event-queue-read-with-timeout ((eq standard-event-queue)
                                          timeout wait-function)
  (let ((lock (event-queue-lock eq)))
    (with-lock-held (lock)
      (loop
	 (check-schedule eq)
	 (let ((res (event-queue-read-no-hang/locked eq)))
	   (when res
	     (return res))
	   (when wait-function
	     (warn "event-queue-read-with-timeout ignoring predicate"))
	   (unless (condition-wait (event-queue-processes eq) lock timeout)
	     (return)))))))

(defmethod event-queue-append ((eq standard-event-queue) item)
  "Append the item at the end of the queue. Does event compression."
  (with-lock-held ((event-queue-lock eq))
    (labels ((append-event ()
	       (cond ((null (event-queue-tail eq))
		      (setf (event-queue-head eq) (cons item nil)
			    (event-queue-tail eq) (event-queue-head eq)))
		     (t
		      (setf (event-queue-tail eq)
			    (setf (cdr (event-queue-tail eq)) (cons item nil))))))
	     (event-delete-if (predicate)
	       (when (not (null (event-queue-head eq)))
		 (setf (event-queue-head eq)
		       (delete-if predicate (event-queue-head eq))
		       (event-queue-tail eq)
		       (last (event-queue-head eq))))))
      (cond 
       ;; Motion Event Compression
       ;; 
       ;; . find the (at most one) motion event
       ;; . delete it
       ;; . append item to queue
       ;;
       ;; But leave enter/exit events.
       ;;
       ((and (typep item 'pointer-motion-event)
	     (not (typep item 'pointer-boundary-event)))
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
       ((typep item 'window-configuration-event)
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
       ((typep item 'window-repaint-event)
        (let ((region (window-event-native-region item))
              (sheet  (event-sheet item))
              (did-something-p nil))
          (labels ((fun (xs)
		     (cond ((null xs)
                          ;; We reached the queue's tail: Append the new event, construct a new
                          ;; one if necessary.
                          (when did-something-p
                            (setf item
                              (make-instance 'window-repaint-event
                                :timestamp (event-timestamp item)
                                :sheet     (event-sheet item)
                                :region    region)))
                          (setf (event-queue-tail eq) (cons item nil)) )
                         ;;
                         ((and (typep (car xs) 'window-repaint-event)
                               (eq (event-sheet (car xs)) sheet))
                          ;; This is a repaint event for the same sheet, delete it and combine
                          ;; its region into the new event.
                          (setf region
                            (region-union region (window-event-native-region (car xs))))
                          ;; Here is an alternative, which just takes the bounding rectangle. 
                          ;; NOTE: When doing this also take care that the new region really
                          ;; is cleared.
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
	    (setf (event-queue-head eq) (fun (event-queue-head eq))))))
     ;; Regular events are just appended:
       (t (append-event))))
    (condition-notify (event-queue-processes eq))))

(defmethod event-queue-prepend ((eq standard-event-queue) item)
  "Prepend the item to the beginning of the queue."
  (with-lock-held ((event-queue-lock eq))
    (cond ((null (event-queue-tail eq))
           (setf (event-queue-head eq) (cons item nil)
                 (event-queue-tail eq) (event-queue-head eq)))
          (t
           (push item (event-queue-head eq))))
    (condition-notify (event-queue-processes eq))))

(defmethod event-queue-peek ((eq standard-event-queue))
  (with-lock-held ((event-queue-lock eq))
    (check-schedule eq)
    (first (event-queue-head eq))))

(defmethod event-queue-peek-if (predicate (eq standard-event-queue))
  "Goes thru the whole event queue and returns the first event, which
   satisfies 'predicate' and leaves the event in the queue.
   Returns NIL, if there is no such event."
  (with-lock-held ((event-queue-lock eq))
    (find-if predicate (event-queue-head eq))))

(defmethod event-queue-listen ((eq standard-event-queue))
  (check-schedule eq)
  (not (null (event-queue-head eq))))

(defun now ()
  (/ (get-internal-real-time)
     internal-time-units-per-second))

(defmethod event-queue-listen-or-wait ((eq standard-event-queue) &key timeout)
  (check-schedule eq)
  (let ((lock (event-queue-lock eq)))
    (with-lock-held (lock)
      (with-slots (schedule-time) eq
	(flet ((pred ()
		 (not (null (event-queue-head eq)))))
	  (cond
	    (timeout
	     (loop as    timeout-time = (+ now timeout)
		with  now = (now)
		do    (when (pred)
			(return t))
		do    (when (>= now timeout-time)
			(return nil))
		do    (let ((timeout (if schedule-time
					 (min (- schedule-time now)
					      (- timeout-time now))
					 (- timeout-time now))))
			(condition-wait (event-queue-processes eq)
					lock timeout))
		do    (check-schedule eq)))
	    (schedule-time
	     (loop do (when (pred)
			(return t))
		do (condition-wait
		    (event-queue-processes eq) lock (- schedule-time (now)))
		do (check-schedule eq)))
	    (t
	     (or (pred)
		 (progn
		   (condition-wait (event-queue-processes eq) lock)
		   t)))))))))

(defmethod check-schedule ((eq standard-event-queue))
  ; see if it's time to inject a scheduled event into the queue.
  (with-slots (schedule-time schedule) eq
    (when (and schedule-time
               (> (now) schedule-time))
      (let* ((event (pop schedule))
             (sheet (pop schedule)))
        (setf schedule-time (pop schedule))
        (dispatch-event sheet event))
      t)))

; ugh. FIXME when I work - build a priority queue or something
(defmethod schedule-event-queue ((eq standard-event-queue) sheet event delay)
  (with-slots (schedule-time schedule) eq
    (let ((when (+ (now) delay)))
      (if schedule
          (cond
            ((< when schedule-time)
             (push schedule-time schedule)
             (push sheet schedule)
             (push event schedule)
             (setf schedule-time when))
            (t
; (format *trace-output* "queue = ~A~%" schedule)
             (do* ((prev  (cdr schedule)  (cdddr prev))
                   (point (cddr schedule) (cdddr point))
                   (time  (car point)))
                  ((or (null point)
                       (< when time))
                   (setf (cdr prev)
                         (cons when (cons event (cons sheet (cdr prev)))))))))
          (progn
            (setf schedule-time when)
            (push sheet schedule)
            (push event schedule))))))

;; PORT-EVENT-QUEUE methods

(defun do-port-force-output (port-event-queue)
  (let ((port (event-queue-port port-event-queue)))
    (when port (port-force-output port))))

(defmethod event-queue-read :before ((eq port-event-queue))
  (do-port-force-output eq))

(defmethod event-queue-read-no-hang :before ((eq port-event-queue))
  (do-port-force-output eq))

(defmethod event-queue-read-with-timeout :before ((eq port-event-queue)
                                                  timeout wait-function)
  (declare (ignore timeout wait-function))
  (do-port-force-output eq))

(defmethod event-queue-listen :before ((eq port-event-queue))
  (do-port-force-output eq))

(defmethod event-queue-listen-or-wait :before ((eq standard-event-queue)
                                               &key timeout)
  (declare (ignore timeout))
  (do-port-force-output eq))

(defmethod event-queue-peek :before ((eq port-event-queue))
  (do-port-force-output eq))  

(defmethod event-queue-peek-if :before (predicate (eq port-event-queue))
  (declare (ignore predicate))
  (do-port-force-output eq))

;; STANDARD-SHEET-INPUT-MIXIN

(defclass standard-sheet-input-mixin ()
  ((queue :initform (make-instance 'port-event-queue)
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

;(defmethod dispatch-event ((sheet standard-sheet-input-mixin) event)
;  (if (typep event 'device-event)
;      (queue-event sheet event)
;    (handle-event sheet event)))

(defmethod dispatch-event ((sheet standard-sheet-input-mixin) event)
  (queue-event sheet event))

(defmethod queue-event ((sheet standard-sheet-input-mixin) event)
  (with-slots (queue) sheet
    (event-queue-append queue event)))

(defmethod schedule-event ((sheet standard-sheet-input-mixin) event delay)
  (with-slots (queue) sheet
    (schedule-event-queue queue sheet event delay)))
  
(defmethod handle-event ((sheet standard-sheet-input-mixin) event)
  ;; Standard practice is to ignore events
  (declare (ignore event))
  nil)

(defmethod event-read ((sheet standard-sheet-input-mixin))  
  (with-slots (queue) sheet
    (event-queue-read queue)))

(defmethod event-read-with-timeout ((sheet standard-sheet-input-mixin)
				    &key (timeout nil) (wait-function nil))
  ;; This one is not in the spec ;-( --GB
  (with-slots (queue) sheet
    (event-queue-read-with-timeout queue timeout wait-function)))

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

;;; Don't know if this event queue stuff is completely right, or if it matters
;;; much...

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
