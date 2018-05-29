(in-package :clim-mezzano)

;; minimum mezzano frame size for resize events
(defparameter *minimum-width* 100)
(defparameter *minimum-height* 100)

;; These x y variables are always in mcclim "units", that is they
;; always apply to the *last-mouse-sheet*, not the mezzano frame
(defvar *last-mouse-x* 0)
(defvar *last-mouse-y* 0)
(defvar *last-graft-x* 0)
(defvar *last-graft-y* 0)
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

(defmethod mez-event->mcclim-event (mcclim-fifo (event mos:key-event))
  (let* ((releasep (mos:key-releasep event))
         (char (mos:key-key event))
         (name (get-name char))
         (modifier-state (compute-modifier-state (mos:key-modifier-state event)))
         (mez-window (mos:window event))
         (sheet (port-lookup-sheet *port* mez-window)))
    (when sheet
      (mos:fifo-push
       (make-instance (if releasep 'key-release-event 'key-press-event)
                      :key-name name
                      :key-character char
                      :x *last-mouse-x*
                      :y *last-mouse-y*
                      :graft-x *last-graft-x*
                      :graft-y *last-graft-y*
                      :sheet (or (frame-properties (pane-frame sheet) 'focus)
                                 sheet)
                      :modifier-state modifier-state)
       mcclim-fifo
       nil))))

;;;======================================================================
;;; Pointer Events
;;;======================================================================

(defparameter +button-to-clim-alist+
  `((,(byte 1 0) . ,+pointer-left-button+)
    (,(byte 1 1) . ,+pointer-right-button+)
    (,(byte 1 2) . ,+pointer-middle-button+)
    (,(byte 1 3) . ,+pointer-wheel-up+)
    (,(byte 1 4) . ,+pointer-wheel-down+)
    ;; (,(byte 1 ???) . ,+pointer-wheel-left+)
    ;; (,(byte 1 ???) . , +pointer-wheel-right+)
    ))

(defun compute-mouse-buttons (buttons)
  (let ((result 0))
    (dolist (tr +button-to-clim-alist+)
      (when (ldb-test (car tr) buttons)
        (setf result (logior result (cdr tr)))))
    result))

(defun pointer-motion-event (mcclim-fifo sheet event)
  (let ((time 0))
    (mos:fifo-push
     (make-instance 'pointer-motion-event
                    :pointer 0
                    :x *last-mouse-x*
                    :y *last-mouse-y*
                    :graft-x *last-graft-x*
                    :graft-y *last-graft-y*
                    :sheet sheet
                    :modifier-state *last-modifier-state*
                    :timestamp time)
     mcclim-fifo
     nil)))

(defun pointer-button-event (mcclim-fifo sheet event)
  (let* ((buttons (compute-mouse-buttons (mos:mouse-button-state event)))
         (change (compute-mouse-buttons (mos:mouse-button-change event)))
         (time 0))
    (mos:fifo-push
     (make-instance (if (= (logand buttons change) 0)
                        'pointer-button-release-event
                        'pointer-button-press-event)
                    :pointer 0
                    :button change
                    :x *last-mouse-x*
                    :y *last-mouse-y*
                    :graft-x *last-graft-x*
                    :graft-y *last-graft-y*
                    :sheet sheet
                    :modifier-state *last-modifier-state*
                    :timestamp time)
     mcclim-fifo
     nil)))

(defun mouse-exit-event (mcclim-fifo sheet event)
  (let ((time 0))
    (mos:fifo-push
    (make-instance 'pointer-exit-event
                   :pointer 0
                   :x *last-mouse-x*
                   :y *last-mouse-y*
                   :graft-x *last-graft-x*
                   :graft-y *last-graft-y*
                   :sheet sheet
                   :modifier-state *last-modifier-state*
                   :timestamp time)
    mcclim-fifo
    nil)))

(defun mouse-enter-event (mcclim-fifo sheet event)
  (let ((time 0))
    (mos:fifo-push
     (make-instance 'pointer-enter-event
                    :pointer 0
                    :x *last-mouse-x*
                    :y *last-mouse-y*
                    :graft-x *last-graft-x*
                    :graft-y *last-graft-y*
                    :sheet sheet
                    :modifier-state *last-modifier-state*
                    :timestamp time)
     mcclim-fifo
     nil)))

(defun frame-mouse-event (mcclim-fifo sheet mez-frame event)
  (handler-case
      (mos:frame-mouse-event mez-frame event)
    (mos:close-button-clicked ()
      (mos:fifo-push
       (make-instance 'window-manager-delete-event :sheet sheet)
       mcclim-fifo
       nil))))

(defmethod mez-event->mcclim-event (mcclim-fifo (event mos:mouse-event))
  (let* ((mez-window (mos:window event))
         (mouse-x    (mos:mouse-x-position event))
         (mouse-y    (mos:mouse-y-position event))
         (mez-mirror (port-lookup-mirror *port* mez-window))
         (sheet      (port-lookup-sheet *port* mez-window)))
    (when mez-mirror
      (with-slots (mez-frame dx dy width height) mez-mirror
        (setf *last-mouse-x* (- mouse-x dx)
              *last-mouse-y* (- mouse-y dy)
              *last-graft-x* (+ mouse-x (mos:window-x mez-window))
              *last-graft-y* (+ mouse-y (mos:window-y mez-window)))
        (cond ((and mez-frame
                    (or (mos:in-frame-header-p mez-frame mouse-x mouse-y)
                        (mos:in-frame-border-p mez-frame mouse-x mouse-y)))
               (when *last-mouse-sheet*
                 (mouse-exit-event mcclim-fifo *last-mouse-sheet* event)
                 (setf *last-mouse-sheet* nil))
               (frame-mouse-event mcclim-fifo sheet mez-frame event))

              ((= (mos:mouse-button-change event) 0)
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
               (unless (eq sheet *last-mouse-sheet*)
                 (when *last-mouse-sheet*
                   (mouse-exit-event mcclim-fifo *last-mouse-sheet* event))
                 (mouse-enter-event mcclim-fifo sheet event)
                 (setf *last-mouse-sheet* sheet))
               (pointer-button-event mcclim-fifo sheet event)))))))

;;;======================================================================
;;; Activation Events
;;;======================================================================

(defmethod mez-event->mcclim-event (mcclim-fifo (event mos:window-activation-event))
  (let* ((mez-window (mos:window event))
         (mez-mirror (port-lookup-mirror *port* mez-window))
         (mez-frame (and mez-mirror (slot-value mez-mirror 'mez-frame)))
         (sheet (port-lookup-sheet *port* mez-window))
         (focus (and sheet (frame-query-io (pane-frame sheet)))))
    (when mez-frame
      (setf (mos:activep mez-frame)
            (mos:state event))
      (mos:draw-frame mez-frame)
      (mos:fifo-push
       (with-slots (width height) mez-mirror
         (make-instance 'window-repaint-event
                        :timestamp 0
                        :sheet sheet
                        :region (make-rectangle* 0 0 width height)))
       mcclim-fifo))))

(defmethod mez-event->mcclim-event (mcclim-fifo (event mos:quit-event))
  (let* ((mez-window (mos:window event))
         (sheet (port-lookup-sheet *port* mez-window)))
    (when sheet
      (mos:fifo-push
       (make-instance 'window-destroy-event
                      :sheet sheet
                      :region nil)
       mcclim-fifo
       nil))))

(defmethod mez-event->mcclim-event (mcclim-fifo (event mos:window-close-event))
  ;;; TODO - what needs to happen here anything?
  )

;;;======================================================================
;;; Resize events
;;;======================================================================

(defmethod mez-event->mcclim-event (mcclim-fifo (event mos:resize-request-event))
  (let* ((mez-window (mos:window event))
         (mez-mirror (port-lookup-mirror *port* mez-window))
         (mez-frame (and mez-mirror (slot-value mez-mirror 'mez-frame)))
         (sheet (port-lookup-sheet *port* mez-window))
         (fwidth (max *minimum-width* (mos:width event)))
         (fheight (max *minimum-height* (mos:height event))))
    (multiple-value-bind (dw dh) (size-deltas mez-mirror)
      (when (and mez-frame
                 (or (/= fwidth (mos:width mez-window))
                     (/= fheight (mos:height mez-window))))
        (let* ((surface (mos:make-surface fwidth fheight))
               (pixels (mos:surface-pixels surface))
               (width (- fwidth dw))
               (height(- fheight dh)))
          (mos:resize-frame mez-frame surface)
          (mos:resize-window
           mez-window surface
           :origin (mos:resize-origin event))

          (setf (slot-value mez-mirror 'mez-pixels) pixels
                (slot-value mez-mirror 'fwidth) fwidth
                (slot-value mez-mirror 'fheight) fheight
                (slot-value mez-mirror 'width) width
                (slot-value mez-mirror 'height) height)

          (mos:fifo-push
           (make-instance 'window-configuration-event
                          :sheet sheet
                          :region nil
                          :width width
                          :height height
                          :x (mos:window-x mez-window)
                          :y (mos:window-y mez-window))
           mcclim-fifo
           nil)
          )))))

(defmethod mez-event->mcclim-event (mcclim-fifo (event mos:resize-event))
  ;;; TODO - what needs to happen here anything?
  )
