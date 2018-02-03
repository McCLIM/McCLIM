(in-package :clim-mezzano)

(defvar *last-mouse-x* 0)
(defvar *last-mouse-y* 0)
(defvar *last-modifier-state* 0)

(defvar *char->name* (make-hash-table :test #'eql))

;;;======================================================================
;;;
;;; mez-event->mcclim-event - converts mezzano events to mcclim events
;;;
;;;======================================================================

(defgeneric mez-event->mcclim-event (event))

(defmethod mez-event->mcclim-event (event)
  ;; Default cause - log event and ignore
  (debug-format "mcclim backend unexpected event")
  (debug-format "    ~S" event)
  (values nil nil))

;;;======================================================================
;;; Keyboard Events
;;;======================================================================

(defun get-name (char)
  (let ((name (gethash char *char->name*)))
    (if name
        name
        (setf (gethash char *char->name*) (intern (string char) :keyword)))))

(defparameter +modifier-to-clim-alist+
  `((:shift    . ,+shift-key+)
    (:control  . ,+control-key+)
    (:meta     . ,+meta-key+)
    (:super    . ,+super-key+)))

(defun compute-modifier-state (modifier-keys)
  (let ((modifier 0))
    (dolist (key modifier-keys)
      (let ((modifier-info (assoc key +modifier-to-clim-alist+)))
        (if modifier-info
            (setf modifier (logior modifier (cdr modifier-info)))
            (debug-format "Unknown modifier key ~S" key))))
    (setf *last-modifier-state* modifier)))

(defmethod mez-event->mcclim-event ((event key-event))
  ;; (debug-format "key-event")
  ;; (debug-format "    ~S" (mezzano.gui.compositor::key-scancode event))
  ;; (debug-format "    ~S" (mezzano.gui.compositor::key-releasep event))
  ;; (debug-format "    ~S" (mezzano.gui.compositor::key-key event))
  ;; (debug-format "    ~S"
  ;;               (mezzano.gui.compositor::key-modifier-state event))
  (let* ((releasep (mezzano.gui.compositor::key-releasep event))
         (char (mezzano.gui.compositor::key-key event))
         (name (get-name char))
         (modifier-state (compute-modifier-state (mezzano.gui.compositor::key-modifier-state event))))
    (values
     (make-instance (if releasep 'key-release-event 'key-press-event)
                    :key-name name
                    :key-character char
                    :x *last-mouse-x*
                    :y *last-mouse-y*
                    :graft-x 0
                    :graft-y 0
                    :sheet *current-focus*
                    :modifier-state modifier-state)
     T)))

;;;======================================================================
;;; Pointer Events
;;;======================================================================

(defun compute-mouse-buttons (buttons)
  (let ((result 0))
    ;; bit 0 -> bit 0
    (setf (ldb (byte 1 0) result) (ldb (byte 1 0) buttons))
    ;; bit 2 -> bit 1
    (setf (ldb (byte 1 1) result) (ldb (byte 1 2) buttons))
    ;; bit 1 -> bit 2
    (setf (ldb (byte 1 2) result) (ldb (byte 1 1) buttons))
    result))

(defun pointer-motion-event (sheet event)
  (if *current-focus*
      (values
       (let ((buttons (compute-mouse-buttons (mezzano.gui.compositor::mouse-button-state event)))
             (time 0))
       (make-instance 'pointer-motion-event
                      :pointer 0
                      :button buttons
                      :x *last-mouse-x*
                      :y *last-mouse-y*
                      :graft-x 0
                      :graft-y 0
                      :sheet sheet
                      :modifier-state *last-modifier-state*
                      :timestamp time)
       T)))
    (values nil nil))

(defun pointer-button-event (sheet event)
  (let* ((buttons (compute-mouse-buttons
                   (mezzano.gui.compositor::mouse-button-state event)))
         (change (compute-mouse-buttons
                  (mezzano.gui.compositor::mouse-button-change event)))
         (time 0))
    (values
     (make-instance (if (= (logand buttons change) 0)
                        'pointer-button-release-event
                        'pointer-button-press-event)
                    :pointer 0
                    :button buttons
                    :x *last-mouse-x*
                    :y *last-mouse-y*
                    :graft-x 0
                    :graft-y 0
                    :sheet sheet
                    :modifier-state *last-modifier-state*
                    :timestamp time)
     T)))

(defun frame-mouse-event (sheet mez-frame event)
  (handler-case
      (progn
        (mezzano.gui.widgets:frame-mouse-event mez-frame event)
        (values nil nil))
    (mezzano.gui.widgets:close-button-clicked ()
      (values
       (make-instance 'window-manager-delete-event :sheet sheet)
       T))))

(defmethod mez-event->mcclim-event ((event mouse-event))
  ;; (debug-format "mouse-event")
  ;; (debug-format "    ~S" (mezzano.gui.compositor::mouse-button-state event))
  ;; (debug-format "    ~S" (mezzano.gui.compositor::mouse-button-change event))
  ;; (debug-format "    ~S" (mezzano.gui.compositor::mouse-x-position event))
  ;; (debug-format "    ~S" (mezzano.gui.compositor::mouse-y-position event))
  ;; (debug-format "    ~S" (mezzano.gui.compositor::mouse-x-motion event))
  ;; (debug-format "    ~S" (mezzano.gui.compositor::mouse-y-motion event))

  (let* ((mez-window (mezzano.gui.compositor::window event))
         (mouse-x    (mezzano.gui.compositor::mouse-x-position event))
         (mouse-y    (mezzano.gui.compositor::mouse-y-position event))
         (mez-mirror (port-lookup-mirror *port* mez-window))
         (sheet      (port-lookup-sheet *port* mez-window)))
    (with-slots (mez-frame dx dy width height) mez-mirror
      (cond ((or (null mez-frame)
                 (<= mouse-x dx) (>= mouse-x width)
                 (<= mouse-y dy) (>= mouse-y height))
             (setf *last-mouse-x* mouse-x
                   *last-mouse-y* mouse-y)
             (frame-mouse-event sheet mez-frame event))

            ((= (mezzano.gui.compositor::mouse-button-change event) 0)
             (setf *last-mouse-x* (- mouse-x dx)
                   *last-mouse-y* (- mouse-y dy))
             (pointer-motion-event sheet event))

            (T
             (setf *last-mouse-x* (- mouse-x dx)
                   *last-mouse-y* (- mouse-y dy))
             (pointer-button-event sheet event))))))

;;;======================================================================
;;; Activation Events
;;;======================================================================

(defmethod mez-event->mcclim-event ((event window-activation-event))
  (let* ((sheet (mez-window->sheet (mezzano.gui.compositor::window event)))
         (focus (frame-query-io (pane-frame sheet))))
  ;; (debug-format "window-activation-event - not implemented")
  ;; (debug-format "    ~S" mez-window)
  ;; (debug-format "    ~S" (mezzano.gui.compositor::state event))
  ;; (debug-format "    ~S" sheet)
  (setf *current-focus* focus)
  (values nil nil)))

(defmethod mez-event->mcclim-event ((event quit-event))
  ;; (debug-format "quit-event")
  (values
   (make-instance 'window-destroy-event
                  :sheet *current-focus*
                  :region nil)
   T))

(defmethod mez-event->mcclim-event ((event window-close-event))
  (debug-format "window-close-event - ignored (?)")
  (values nil nil))
