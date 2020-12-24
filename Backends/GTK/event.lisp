(in-package :clim-gtk)

(defun push-event-to-queue (port event)
  (bordeaux-threads:with-lock-held ((gtk-port/event-queue-lock port))
    (push event (gtk-port/event-queue port))
    (bordeaux-threads:condition-notify (gtk-port/event-queue-condvar port))))

(defun read-event-from-queue-or-wait (port &key timeout)
  (with-accessors ((q gtk-port/event-queue)
                   (lock gtk-port/event-queue-lock)
                   (condvar gtk-port/event-queue-condvar))
      port
    (bordeaux-threads:with-lock-held (lock)
      (loop
        until q
        do (bordeaux-threads:condition-wait condvar lock :timeout timeout))
      ;; It would be more efficient to use an array here so we don't have to loop through
      ;; the event queue every time an event is read. For now this should be acceptable
      ;; since the event queue should never be very long.
      (when q
        (let ((event (if (cdr q)
                         ;; The event queue contains at least 2 elements. We need to remove the last
                         ;; element by setting the previous one's rest slot to nil.
                         (loop
                           with prev = q
                           with current = (cdr q)
                           while (cdr current)
                           do (progn
                                (setq prev current)
                                (setq current (cdr current)))
                           finally (return (progn
                                             (setf (cdr prev) nil)
                                             (car current))))
                         ;; ELSE: Only one element. Remove it by setting the event queue pointer to nil.
                         (let ((event (car q)))
                           (setf q nil)
                           event))))
          (bordeaux-threads:condition-notify condvar)
          event)))))

(defmethod process-next-event ((port gtk-port) &key wait-function (timeout nil))
  (when (maybe-funcall wait-function)
    (return-from process-next-event (values nil :wait-function)))
  (let ((event (read-event-from-queue-or-wait port :timeout timeout)))
    (if event
        (progn
          (distribute-event port event)
          t)
        (if (maybe-funcall wait-function)
            (values nil :wait-function)
            (values nil :timeout)))))

(defvar *event-ts* 0)

(defun process-configure-event (port widget event sheet)
  (declare (ignore widget))
  (push-event-to-queue port
                       (make-instance 'window-configuration-event
                                      :timestamp (incf *event-ts*)
                                      :sheet sheet
                                      :x (gdk:gdk-event-configure-x event)
                                      :y (gdk:gdk-event-configure-y event)
                                      :width (gdk:gdk-event-configure-width event)
                                      :height (gdk:gdk-event-configure-height event)))
  (push-event-to-queue port
                       (make-instance 'window-repaint-event
                                      :timestamp (incf *event-ts*)
                                      :sheet sheet
                                      :region clim:+everywhere+)))

(defun process-drawing-area-configure (event mirror)
  (let ((width (gdk:gdk-event-configure-width event))
        (height (gdk:gdk-event-configure-height event))
        (image (gtk-mirror/image mirror)))
    (declare (ignore image))
    #+nil
    (log:info "drawing area configure: (~s,~s) size: (~s,~s)" width height
              (gdk:gdk-event-configure-x event) (gdk:gdk-event-configure-y event))))

(defun start-port-event-thread (port)
  (bordeaux-threads:make-thread (lambda ()
                                  (loop
                                    until (bordeaux-threads:with-lock-held ((gtk-port/stopped-lock port))
                                            (gtk-port/stop-request port))
                                    do (alexandria:when-let ((event (process-next-event port)))
                                         (distribute-event port event))))
                                :name "GTK Port Event Thread"))
