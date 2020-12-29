(in-package :clim-gtk)

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

(defun create-event-listeners (window port sheet mirror input-context)
  (gobject:g-signal-connect window "configure-event"
                            (lambda (widget event)
                              (process-configure-event port widget event sheet mirror)
                              gdk:+gdk-event-propagate+))
  (gobject:g-signal-connect window "map"
                            (lambda (widget)
                              (declare (ignore widget))
                              (process-map-event port sheet)
                              gdk:+gdk-event-propagate+))
  (gobject:g-signal-connect window "destroy"
                            (lambda (widget)
                              (declare (ignore widget))
                              (process-destroy-event port sheet)
                              gdk:+gdk-event-propagate+))
  (gobject:g-signal-connect window "enter-notify-event"
                            (lambda (widget event)
                              (declare (ignore widget))
                              (process-enter-notify-event port event sheet)
                              gdk:+gdk-event-propagate+))
  (gobject:g-signal-connect window "leave-notify-event"
                            (lambda (widget event)
                              (declare (ignore widget))
                              (process-leave-notify-event port event sheet)
                              gdk:+gdk-event-propagate+))
  (gobject:g-signal-connect window "key-press-event"
                            (lambda (widget event)
                              (declare (ignore widget))
                              (process-key-press-event port event sheet mirror)
                              gdk:+gdk-event-propagate+))
  (gobject:g-signal-connect window "key-release-event"
                            (lambda (widget event)
                              (declare (ignore widget))
                              (process-key-release-event port event sheet mirror)
                              gdk:+gdk-event-propagate+))
  (gobject:g-signal-connect window "motion-notify-event"
                            (lambda (widget event)
                              (declare (ignore widget))
                              (process-mouse-motion-event port event sheet)
                              gdk:+gdk-event-propagate+))
  (gobject:g-signal-connect window "button-press-event"
                            (lambda (widget event)
                              (declare (ignore widget))
                              (process-button-press-event port event sheet)
                              gdk:+gdk-event-propagate+))
  (gobject:g-signal-connect window "button-release-event"
                            (lambda (widget event)
                              (declare (ignore widget))
                              (process-button-release-event port event sheet)
                              gdk:+gdk-event-propagate+))

  (gobject:g-signal-connect input-context "commit"
                            (lambda (context string)
                              (log:info "Got input: ~s ~s" context string)))
  (gobject:g-signal-connect input-context "preedit-start"
                            (lambda (context user-data)
                              (log:info "Got preedit: ~s ~s" context user-data)))

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
                                        :height height
                                        :timestamp (incf *event-ts*)))))

(defun process-map-event (port sheet)
  (push-event-to-queue port
                       (make-instance 'window-repaint-event
                                      :timestamp (incf *event-ts*)
                                      :sheet sheet
                                      :region clim:+everywhere+
                                      :timestamp (incf *event-ts*))))

(defun process-destroy-event (port sheet)
  (push-event-to-queue port (make-instance 'climi::window-destroy-event :sheet sheet)))

(defun process-enter-notify-event (port event sheet)
  (process-crossing-event (ecase (gdk:gdk-event-crossing-mode event)
                            ((:normal :ungrab :gtk-ungrab) 'climi::pointer-ungrab-enter-event)
                            ((:grab :gtk-grab) 'climi::pointer-grab-enter-event))
                          port event sheet))

(defun process-leave-notify-event (port event sheet)
  (process-crossing-event (ecase (gdk:gdk-event-crossing-mode event)
                            ((:normal :ungrab :gtk-ungrab) 'climi::pointer-ungrab-leave-event)
                            ((:grab :gtk-grab) 'climi::pointer-grab-leave-event))
                          port event sheet))

(defun process-crossing-event (name port event sheet)
  (push-event-to-queue port
                       (make-instance name
                                      :pointer 0
                                      :button 0
                                      :x (gdk:gdk-event-crossing-x event)
                                      :y (gdk:gdk-event-crossing-y event)
                                      :graft-x (gdk:gdk-event-crossing-x-root event)
                                      :graft-y (gdk:gdk-event-crossing-y-root event)
                                      :sheet sheet
                                      :modifier-state 0
                                      :timestamp (incf *event-ts*))))

(defun keyboard-event-to-sym (event)
  (let ((keyval (gdk:gdk-event-key-keyval event)))
    (clim-xcommon:keysym-to-keysym-name keyval)))

(defun process-generic-key-event (name port event sheet)
  (log:info "key event: ~s: string=~s code=~s" name (gdk:gdk-event-key-string event) (gdk:gdk-event-key-hardware-keycode event))
  (let* ((key-string (gdk:gdk-event-key-string event))
         (keysym-name (keyboard-event-to-sym event))
         (length (length key-string))
         (character (cond
                      ((zerop length) nil)
                      ((= length 1) (aref key-string 0))
                      (t key-string)))
         (clim-event (make-instance name
                                    :key-name keysym-name ;;keysym-name
                                    :key-character character
                                    :x 0
                                    :y 0
                                    :graft-x 0
                                    :graft-y 0
                                    :sheet sheet
                                    :modifier-state 0
                                    :timestamp (incf *event-ts*))))
    (push-event-to-queue port clim-event)))

(defun process-key-press-event (port event sheet mirror)
  (declare (ignore mirror))
  (progn ;;unless (gtk:gtk-im-context-filter-keypress (gtk-mirror/input-context mirror) event)
    (process-generic-key-event 'key-press-event port event sheet)))

(defun process-key-release-event (port event sheet mirror)
  (declare (ignore mirror))
  (progn ;;unless (gtk:gtk-im-context-filter-keypress (gtk-mirror/input-context mirror) event)
    (process-generic-key-event 'key-release-event port event sheet)))

(defun convert-button-name (gdk-button-id)
  (case gdk-button-id
    (1 clim:+pointer-left-button+)
    (2 clim:+pointer-middle-button+)
    (3 clim:+pointer-right-button+)
    ((t) clim:+pointer-left-button+)))

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

(defun process-button-press-event (port event sheet)
  (process-button-event 'pointer-button-press-event port event sheet))

(defun process-button-release-event (port event sheet)
    (process-button-event 'pointer-button-release-event port event sheet))

(defun process-button-event (name port event sheet)
  (push-event-to-queue port
                       (make-instance name
                                      :pointer 0
                                      :button (convert-button-name (gdk:gdk-event-button-button event))
                                      :x (gdk:gdk-event-button-x event)
                                      :y (gdk:gdk-event-button-y event)
                                      :graft-x (gdk:gdk-event-button-x-root event)
                                      :graft-y (gdk:gdk-event-button-y-root event)
                                      :sheet sheet
                                      :modifier-state 0
                                      :timestamp (incf *event-ts*))))
