(in-package :clim-gtk)

(defun push-event-to-queue (port event)
  (bordeaux-threads:with-lock-held ((gtk-port/lock port))
    (push event (gtk-port/event-queue port))
    (bordeaux-threads:condition-notify (gtk-port/condvar port))))

(defun read-event-from-queue-or-wait (port &key timeout)
  (with-accessors ((q gtk-port/event-queue)
                   (lock gtk-port/lock)
                   (condvar gtk-port/condvar))
      port
    (bordeaux-threads:with-lock-held (lock)
      (loop
        until (or q (gtk-port/stop-request port))
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

(defun create-event-listeners (window port sheet mirror)
  (gobject:g-signal-connect window "configure-event"
                            (lambda (widget event)
                              (process-configure-event port widget event sheet mirror)
                              gdk:+gdk-event-propagate+))
  #+nil
  (gobject:g-signal-connect window "motion-notify-event"
                            (lambda (widget event)
                              (declare (ignore widget))
                              (process-mouse-motion-event port event sheet)
                              gdk:+gdk-event-propagate+))
  (gtk:gtk-widget-add-events window '(:all-events-mask)))

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

(defun process-configure-event (port widget event sheet mirror)
  (declare (ignore widget))
  (let ((width (gdk:gdk-event-configure-width event))
        (height (gdk:gdk-event-configure-height event))
        (image (gtk-mirror/image mirror)))
    (bordeaux-threads:with-lock-held ((gtk-mirror/lock mirror))
      (when (or (/= width (cairo:cairo-image-surface-get-width image))
                (/= height (cairo:cairo-image-surface-get-height image)))
        (setf (gtk-mirror/requested-image-width mirror) width)
        (setf (gtk-mirror/requested-image-height mirror) height)))
    (push-event-to-queue port
                         (make-instance 'window-configuration-event
                                        :timestamp (incf *event-ts*)
                                        :sheet sheet
                                        :x (gdk:gdk-event-configure-x event)
                                        :y (gdk:gdk-event-configure-y event)
                                        :width width
                                        :height height)))
  (push-event-to-queue port
                       (make-instance 'window-repaint-event
                                      :timestamp (incf *event-ts*)
                                      :sheet sheet
                                      :region clim:+everywhere+)))

(defun process-mouse-motion-event (port event sheet)
  (let ((x (gdk:gdk-event-motion-x event))
        (y (gdk:gdk-event-motion-y event))
        (root-x (gdk:gdk-event-motion-x-root event))
        (root-y (gdk:gdk-event-motion-y-root event)))
    (push-event-to-queue port
                         (make-instance 'pointer-motion-event
                                        :pointer 0
                                        :button 0
                                        :x x
                                        :y y
                                        :graft-x root-x
                                        :graft-y root-y
                                        :sheet sheet
                                        :modifier-state 0
                                        :timestamp (incf *event-ts*)))))

(defun start-port-event-thread (port)
  (bordeaux-threads:make-thread (lambda ()
                                  (with-simple-restart (restart-event-loop "Restart the GTK port event loop")
                                    (loop
                                      for event = (read-event-from-queue-or-wait port)
                                      until (bordeaux-threads:with-lock-held ((gtk-port/lock port))
                                              (gtk-port/stop-request port))
                                      when event
                                        do (distribute-event port event))))
                                :name "GTK Port Event Thread"))
