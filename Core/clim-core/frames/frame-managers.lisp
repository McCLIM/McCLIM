;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2021 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Frame managers.
;;;

(in-package #:clim-internals)

(defvar *default-frame-manager* nil)
(defvar *pane-realizer* nil)

;; FIXME: The spec says the port must "conform to options".  I've added a check
;; that the ports match, but we've no protocol for testing the other
;; options. -Hefner
(defun find-frame-manager (&rest options &key port &allow-other-keys)
  (declare (special *frame-manager*))
  (if (and (boundp '*frame-manager*)
           (or (null port) (eql port (port *frame-manager*))))
      *frame-manager*
      (if (and *default-frame-manager*
               (frame-manager-p *default-frame-manager*)
               (or (null port) (eql port (port *default-frame-manager*))))
          *default-frame-manager*
          (first (frame-managers (or port (apply #'find-port options)))))))

(defmacro with-frame-manager ((frame-manager) &body body)
  `(let ((*frame-manager* ,frame-manager))
     (declare (special *frame-manager*))
     (locally ,@body)))

(defmacro with-look-and-feel-realization ((frame-manager frame) &body body)
  `(let ((*pane-realizer* ,frame-manager)
         (*application-frame* ,frame))
     (locally ,@body)))

(defun map-over-frames (function &key port frame-manager)
  (cond (frame-manager
         (mapc function (frame-manager-frames frame-manager)))
        (port
         (loop for manager in (frame-managers port)
               do (map-over-frames function :frame-manager manager)))
        (t (loop for p in *all-ports*
                 do (map-over-frames function :port p)))))

(defmethod (setf frame-manager)
    ((new-manager frame-manager) (frame application-frame))
  (let ((old-manager (frame-manager frame)))
    (unless (eq new-manager old-manager)
      (when old-manager
        (disown-frame old-manager frame))
      (when new-manager
        (adopt-frame new-manager frame)))))

;;; STANDARD-FRAME-MANAGER class

(defclass standard-frame-manager (frame-manager)
  ((port
    :initarg :port
    :reader port)
   (frames
    :initform nil
    :reader frame-manager-frames)))

(defmethod adopt-frame
    ((fm standard-frame-manager) (frame application-frame))
  (push frame (slot-value fm 'frames)))

(defmethod disown-frame
    ((fm standard-frame-manager) (frame application-frame))
  (alexandria:removef (slot-value fm 'frames) frame))

(defmethod note-frame-enabled
    ((fm standard-frame-manager) (frame application-frame))
  (declare (ignore fm frame)))

(defmethod note-frame-disabled
    ((fm standard-frame-manager) (frame application-frame))
  (declare (ignore fm frame)))

(defmethod note-frame-iconified
    ((fm standard-frame-manager) (frame application-frame))
  (declare (ignore fm frame)))

(defmethod note-frame-deiconified
    ((fm standard-frame-manager) (frame application-frame))
  (declare (ignore fm frame)))

(defmethod note-command-enabled
    ((fm standard-frame-manager) (frame application-frame) command-name)
  (declare (ignore fm frame command-name)))

(defmethod note-command-disabled
    ((fm standard-frame-manager) (frame application-frame) command-name)
  (declare (ignore fm frame command-name)))

(defmethod note-frame-pretty-name-changed
    ((fm standard-frame-manager) (frame application-frame) new-value)
  (declare (ignore fm frame new-value)))

(defmethod note-frame-icon-changed
    ((fm standard-frame-manager) (frame application-frame) new-value)
  (declare (ignore fm frame new-value)))


;;; Standard application frame methods

(defmethod make-pane-1 :around
    ((fm standard-frame-manager) (frame standard-application-frame)
     type &rest args &key (event-queue nil evq-p) &allow-other-keys)
  ;; Default event-queue to the frame event queue.
  (declare (ignore event-queue))
  (if (null evq-p)
      (let ((evq (frame-event-queue frame))
            (*input-buffer* (frame-input-buffer frame)))
        (apply #'call-next-method fm frame type :event-queue evq args))
      (call-next-method)))

(defmethod find-pane-for-frame
    ((fm standard-frame-manager) (frame standard-application-frame))
  (make-pane-1 fm frame 'top-level-sheet-pane
               :name (frame-name frame)
               :pretty-name (frame-pretty-name frame)
               :icon (frame-icon frame)
               ;; sheet is enabled from enable-frame
               :enabled-p nil))

(defmethod adopt-frame
    ((fm standard-frame-manager) (frame standard-application-frame))
  (call-next-method)
  (setf (%frame-manager frame) fm)
  (setf (port frame) (port fm))
  (setf (graft frame) (find-graft :port (port frame)))
  (let ((*application-frame* frame)
        (event-queue (frame-event-queue frame)))
    (setf (slot-value frame 'top-level-sheet)
          (find-pane-for-frame fm frame))
    (generate-panes fm frame)
    (setf (slot-value frame 'state) :disabled)
    (when (typep event-queue 'event-queue)
      (setf (event-queue-port event-queue) (port fm)))
    frame))

(defmethod disown-frame
    ((fm standard-frame-manager) (frame standard-application-frame))
  (call-next-method)
  (when-let* ((event-queue (frame-event-queue frame))
              (calling-frame (frame-calling-frame frame))
              (calling-queue (frame-event-queue calling-frame))
              (another-queue-p (not (eql calling-queue event-queue))))
    (setf (event-queue-port event-queue) nil))
  (sheet-disown-child (graft frame) (frame-top-level-sheet frame))
  (setf (frame-panes frame) nil)
  (setf (slot-value frame 'layouts) nil)
  (setf (%frame-manager frame) nil)
  (setf (slot-value frame 'state) :disowned)
  frame)

;;; Menu frame methods

(defmethod adopt-frame
    ((fm standard-frame-manager) (frame menu-frame))
  (call-next-method)
  (setf (%frame-manager frame) fm)
  (let* ((t-l-s (make-pane-1 fm *application-frame*
                             'menu-unmanaged-top-level-sheet-pane
                             :name 'top-level-sheet
                             ;; enabling should be left to enable-frame
                             :enabled-p nil)))
    (setf (slot-value frame 'top-level-sheet) t-l-s)
    (sheet-adopt-child t-l-s (frame-panes frame))
    (let ((graft (find-graft :port (port fm))))
      (sheet-adopt-child graft t-l-s)
      (setf (graft frame) graft))
    (let ((pre-space (compose-space t-l-s))
          (frame-min-width (slot-value frame 'min-width)))
      (multiple-value-bind (width min-width max-width height min-height max-height)
          (space-requirement-components pre-space)
        (flet ((foomax (x y) (max (or x 1) (or y 1))))
          (let ((space (make-space-requirement
                        :min-width  (foomax frame-min-width min-width)
                        :width      (foomax frame-min-width width)
                        :max-width  (foomax frame-min-width max-width)
                        :min-height min-height
                        :height     height
                        :max-height max-height)))
            (allocate-space (frame-panes frame)
                            (space-requirement-width space)
                            (space-requirement-height space))
            (setf (sheet-region t-l-s)
                  (make-bounding-rectangle 0 0
                                           (space-requirement-width space)
                                           (space-requirement-height space))))
          (setf (sheet-transformation t-l-s)
                (make-translation-transformation (slot-value frame 'left)
                                                 (slot-value frame 'top))))))))

(defmethod disown-frame
    ((fm standard-frame-manager) (frame menu-frame))
  (call-next-method)
  (let ((tps (frame-top-level-sheet frame)))
    (sheet-disown-child tps (frame-panes frame))
    (sheet-disown-child (graft frame) tps))
  (setf (%frame-manager frame) nil))
