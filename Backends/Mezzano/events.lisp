(in-package :clim-mezzano)

;; minimum mezzano frame size for resize events
(defparameter *minimum-width* 100)
(defparameter *minimum-height* 100)

(defvar *last-mouse-x* 0)
(defvar *last-mouse-y* 0)
(defvar *last-mouse-sheet* nil)
(defvar *last-modifier-state* 0)

(defvar *char->name* (make-hash-table :test #'eql))

;;;======================================================================
;;;
;;; mez-event->mcclim-event - converts mezzano events to mcclim events
;;;
;;;======================================================================

(defgeneric mez-event->mcclim-event (mcclim-fifo event))

(defmethod mez-event->mcclim-event (mcclim-fifo event)
  ;; Default case - log event and ignore
  (debug-format "mcclim backend unexpected event")
  (debug-format "    ~S" event))

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

(defmethod mez-event->mcclim-event (mcclim-fifo (event key-event))
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
    (mezzano.supervisor:fifo-push
     (make-instance (if releasep 'key-release-event 'key-press-event)
                    :key-name name
                    :key-character char
                    :x *last-mouse-x*
                    :y *last-mouse-y*
                    :graft-x 0
                    :graft-y 0
                    :sheet *current-focus*
                    :modifier-state modifier-state)
     mcclim-fifo
     nil)))

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

(defun pointer-motion-event (mcclim-fifo sheet event)
  (let ((time 0))
    (mezzano.supervisor:fifo-push
     (make-instance 'pointer-motion-event
                    :pointer 0
                    :x *last-mouse-x*
                    :y *last-mouse-y*
                    :graft-x 0
                    :graft-y 0
                    :sheet sheet
                    :modifier-state *last-modifier-state*
                    :timestamp time)
     mcclim-fifo
     nil)))

(defun pointer-button-event (mcclim-fifo sheet event)
  (let* ((buttons (compute-mouse-buttons
                   (mezzano.gui.compositor::mouse-button-state event)))
         (change (compute-mouse-buttons
                  (mezzano.gui.compositor::mouse-button-change event)))
         (time 0))
    (mezzano.supervisor:fifo-push
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
     mcclim-fifo
     nil)))

(defun mouse-exit-event (mcclim-fifo sheet event)
  ;; (debug-format "mouse-exit-event")
  ;; (debug-format "    ~S" sheet)
  (let ((time 0))
    (mezzano.supervisor:fifo-push
    (make-instance 'pointer-exit-event
                   :pointer 0
                   :x *last-mouse-x*
                   :y *last-mouse-y*
                   :graft-x 0
                   :graft-y 0
                   :sheet sheet
                   :modifier-state *last-modifier-state*
                   :timestamp time)
    mcclim-fifo
    nil)))

(defun mouse-enter-event (mcclim-fifo sheet event)
  ;; (debug-format "mouse-enter-event")
  ;; (debug-format "    ~S" sheet)
  (let ((time 0))
    (mezzano.supervisor:fifo-push
     (make-instance 'pointer-enter-event
                    :pointer 0
                    :x *last-mouse-x*
                    :y *last-mouse-y*
                    :graft-x 0
                    :graft-y 0
                    :sheet sheet
                    :modifier-state *last-modifier-state*
                    :timestamp time)
     mcclim-fifo
     nil)))

(defun frame-mouse-event (mcclim-fifo sheet mez-frame event)
  (handler-case
      (mezzano.gui.widgets:frame-mouse-event mez-frame event)
    (mezzano.gui.widgets:close-button-clicked ()
      (debug-format "close button clicked ~S" sheet)
      (mezzano.supervisor:fifo-push
       (make-instance 'window-manager-delete-event :sheet sheet)
       mcclim-fifo
       nil))))

(defmethod mez-event->mcclim-event (mcclim-fifo (event mouse-event))
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
    (when mez-mirror
      (with-slots (mez-frame dx dy width height) mez-mirror
        (cond ((or (null mez-frame)
                   (mezzano.gui.widgets::in-frame-header-p mez-frame mouse-x mouse-y)
                   (mezzano.gui.widgets::in-frame-border-p mez-frame mouse-x mouse-y))
               ;; (<= mouse-x dx) (>= mouse-x width)
               ;; (<= mouse-y dy) (>= mouse-y height))
               (setf *last-mouse-x* mouse-x
                     *last-mouse-y* mouse-y)
               (when *last-mouse-sheet*
                 (mouse-exit-event mcclim-fifo *last-mouse-sheet* event)
                 (setf *last-mouse-sheet* nil))
               (frame-mouse-event mcclim-fifo sheet mez-frame event))

              ((= (mezzano.gui.compositor::mouse-button-change event) 0)
               (setf *last-mouse-x* (- mouse-x dx)
                     *last-mouse-y* (- mouse-y dy))
               (funcall (mezzano.gui.widgets::set-cursor-function mez-frame)
                        :default)
               (cond ((eq sheet *last-mouse-sheet*)
                      (pointer-motion-event mcclim-fifo sheet event))
                     (T
                      (when *last-mouse-sheet*
                        (mouse-exit-event mcclim-fifo *last-mouse-sheet* event))
                      (mouse-enter-event mcclim-fifo sheet event)
                      (setf *last-mouse-sheet* sheet))))
              (T
               (setf *last-mouse-x* (- mouse-x dx)
                     *last-mouse-y* (- mouse-y dy))
               (unless (eq sheet *last-mouse-sheet*)
                 (when *last-mouse-sheet*
                   (mouse-exit-event mcclim-fifo *last-mouse-sheet* event))
                 (mouse-enter-event mcclim-fifo sheet event)
                 (setf *last-mouse-sheet* sheet))
               (pointer-button-event mcclim-fifo sheet event)))))))

;;;======================================================================
;;; Activation Events
;;;======================================================================

(defmethod mez-event->mcclim-event (mcclim-fifo (event window-activation-event))
  (let* ((mez-window (mezzano.gui.compositor::window event))
         (mez-mirror (port-lookup-mirror *port* mez-window))
         (mez-frame (and mez-mirror (slot-value mez-mirror 'mez-frame)))
         (sheet (mez-window->sheet mez-window))
         (focus (and sheet (frame-query-io (pane-frame sheet)))))
    ;; (debug-format "window-activation-event - not implemented")
    ;; (debug-format "    ~S" mez-window)
    ;; (debug-format "    ~S" (mezzano.gui.compositor::state event))
    ;; (debug-format "    ~S" sheet)

    (when mez-frame
      (setf (mezzano.gui.widgets:activep mez-frame)
            (mezzano.gui.compositor::state event))
      (mezzano.gui.widgets:draw-frame mez-frame))
    (setf *current-focus* focus)))

(defmethod mez-event->mcclim-event (mcclim-fifo (event quit-event))
  ;; (debug-format "quit-event")
  (mezzano.supervisor:fifo-push
   (make-instance 'window-destroy-event
                  :sheet *current-focus*
                  :region nil)
   mcclim-fifo
   nil))

(defmethod mez-event->mcclim-event (mcclim-fifo (event window-close-event))
  ;;; TODO - what needs to happen here anything?
  )

;;;======================================================================
;;; Resize events
;;;======================================================================

(defmethod mez-event->mcclim-event (mcclim-fifo (event resize-request-event))
  (let* ((mez-window (mezzano.gui.compositor::window event))
         (mez-mirror (port-lookup-mirror *port* mez-window))
         (mez-frame (and mez-mirror (slot-value mez-mirror 'mez-frame)))
         (sheet (mez-window->sheet mez-window))
         (old-width (mezzano.gui.compositor:width mez-window))
         (old-height (mezzano.gui.compositor:height mez-window))
         (new-width (max *minimum-width* (mezzano.gui.compositor:width event)))
         (new-height (max *minimum-height* (mezzano.gui.compositor:height event))))
    (when (and mez-frame
               (or (not (eql old-width new-width))
                   (not (eql old-height new-height))))
      (let* ((surface (mezzano.gui:make-surface new-width new-height))
             (pixels (mezzano.gui::surface-pixels surface)))
        (mezzano.gui.widgets:resize-frame mez-frame surface)
        (mezzano.gui.compositor:resize-window
         mez-window surface
         :origin (mezzano.gui.compositor:resize-origin event))

        (setf (slot-value mez-mirror 'mez-pixels) pixels
              (slot-value mez-mirror 'fwidth) new-width
              (slot-value mez-mirror 'fheight) new-height
              (slot-value mez-mirror 'width) (- new-width 2)
              (slot-value mez-mirror 'height) (- new-height 20))

        (mezzano.supervisor:fifo-push
         (make-instance 'window-configuration-event
                        :sheet sheet
                        :region nil
                        :width new-width
                        :height new-height
                        :x (mezzano.gui.compositor::window-x mez-window)
                        :y (mezzano.gui.compositor::window-y mez-window))
         mcclim-fifo
         nil)
        ))))

(defmethod mez-event->mcclim-event (mcclim-fifo (event resize-event))
  ;;; TODO - what needs to happen here anything?
  )
