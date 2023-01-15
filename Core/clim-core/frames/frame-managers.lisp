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
          (let ((port (or port (apply #'find-port options))))
            (first (or (frame-managers port)
                       (push (make-instance 'standard-frame-manager :port port)
                             (frame-managers port))))))))

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
         (map nil function (frame-manager-frames frame-manager)))
        (port
         (loop for manager in (frame-managers port)
               do (map-over-frames function :frame-manager manager)))
        (t (loop for p in *all-ports*
                 do (map-over-frames function :port p)))))


;;; HEADLESS-FRAME-MANAGER class

(defmethod note-frame-enabled (fm frame)
  (declare (ignore fm frame)))

(defmethod note-frame-disabled (fm frame)
  (declare (ignore fm frame)))

(defmethod note-frame-iconified (fm frame)
  (declare (ignore fm frame)))

(defmethod note-frame-deiconified (fm frame)
  (declare (ignore fm frame)))

(defmethod note-command-enabled (fm frame new-value)
  (declare (ignore fm frame new-value)))

(defmethod note-command-disabled (fm frame new-value)
  (declare (ignore fm frame new-value)))

(defmethod note-frame-pretty-name-changed (fm frame new-value)
  (declare (ignore fm frame new-value)))

(defmethod note-frame-icon-changed (fm frame new-value)
  (declare (ignore fm frame new-value)))

(defmethod note-frame-command-table-changed (fm frame new-value)
  (declare (ignore fm frame new-value)))

(defclass headless-frame-manager (frame-manager)
  ((frames
    :initform nil
    :reader frame-manager-frames)))

(defmethod adopt-frame :after
    ((fm headless-frame-manager) (frame application-frame))
  (push frame (slot-value fm 'frames)))

(defmethod disown-frame :before
    ((fm headless-frame-manager) (frame application-frame))
  (alexandria:removef (slot-value fm 'frames) frame))

(defmethod adopt-frame
    ((fm headless-frame-manager) (frame application-frame))
  (declare (ignore fm frame)))

(defmethod disown-frame
    ((fm headless-frame-manager) (frame application-frame))
  (declare (ignore fm frame)))

(defmethod find-pane-for-frame
    ((fm headless-frame-manager) (frame application-frame))
  (declare (ignore fm frame)))

(defmethod make-pane-1
    ((fm headless-frame-manager) (frame application-frame) type &rest args)
  (declare (ignore fm frame type args)))

(defmethod generate-panes
    ((fm headless-frame-manager) (frame application-frame))
  (declare (ignore fm frame)))

;;; STANDARD-FRAME-MANAGER class

(defclass standard-frame-manager (headless-frame-manager)
  ((port
    :initarg :port
    :reader port)))

(defmethod adopt-frame
    ((fm standard-frame-manager) (frame standard-application-frame))
  (call-next-method)
  (setf (%frame-manager frame) fm)
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
  (let ((tpl-sheet (frame-top-level-sheet frame)))
    (sheet-disown-child (sheet-parent tpl-sheet) tpl-sheet))
  (setf (frame-panes frame) nil)
  (setf (frame-panes-for-layout frame) nil)
  (setf (slot-value frame 'layouts) nil)
  (setf (%frame-manager frame) nil)
  (setf (slot-value frame 'state) :disowned)
  frame)

(defmethod make-pane-1
    ((fm standard-frame-manager) (frame standard-application-frame) (type class) &rest args)
  (apply #'make-instance type :frame frame :manager fm :port (port fm) args))

(defmethod make-pane-1
    ((fm standard-frame-manager) (frame standard-application-frame) type &rest args)
  (apply #'make-pane-1 fm frame (find-concrete-pane-class fm type) args))

(defmethod make-pane-1 :around
    ((fm standard-frame-manager) (frame standard-application-frame)
     (type class) &rest args &key (event-queue nil evq-p) &allow-other-keys)
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

(defun disown-frame-panes (fm frame)
  (declare (ignore fm))
  (when-let ((panes (frame-panes frame)))
    (labels ((disown-direct-child (ancestor sheet)
               (when-let ((parent (sheet-parent sheet)))
                 (if (eq ancestor parent)
                     (sheet-disown-child ancestor sheet)
                     (disown-direct-child ancestor parent)))))
      (disown-direct-child (frame-top-level-sheet frame) panes)))
  (loop for (nil . pane) in (frame-panes-for-layout frame)
        for parent = (sheet-parent pane)
        when parent
          do (sheet-disown-child parent pane)))

(defun adopt-frame-panes (fm frame layout)
  (declare (ignore layout))
  (flet ((maybe-add-auxiliary-panes (frame)
           (let ((root (frame-panes frame))
                 (menu (slot-value frame 'menu-bar))
                 (pdoc (slot-value frame 'pdoc-bar)))
             (when menu
               (setf (frame-menu-bar-pane frame)
                     (cond ((eq menu t)
                            (make-menu-bar (frame-command-table frame)
                                           frame 'hmenu-pane))
                           ((consp menu)
                            (let ((ct (make-command-table nil :menu menu)))
                              (setf (slot-value frame 'menu-bar) ct)
                              (make-menu-bar ct frame 'hmenu-pane)))
                           (menu
                            (make-menu-bar menu frame 'hmenu-pane))))
               (setf menu (frame-menu-bar-pane frame)))
             (when pdoc
               (multiple-value-bind (pane stream)
                   (make-clim-pointer-documentation-pane)
                 (setf pdoc pane
                       (frame-pointer-documentation-output frame) stream)))
             (if (or menu pdoc)
                 (make-pane-1 fm frame 'vrack-pane
                                :contents (remove nil (list menu root pdoc))
                                :port (port frame))
                 root))))
    (let ((tpl-sheet (frame-top-level-sheet frame))
          (new-root (maybe-add-auxiliary-panes frame)))
      (setf (frame-panes frame) new-root)
      (sheet-adopt-child tpl-sheet new-root)
      (unless (sheet-parent tpl-sheet)
        (sheet-adopt-child (find-graft :port (port fm)) tpl-sheet))
      ;; Find the size of the new frame.
      (multiple-value-bind (w h) (frame-geometry* frame)
        (layout-frame frame w h)))))

(defmethod generate-panes
    ((fm standard-frame-manager) (frame standard-application-frame))
  (funcall (frame-panes-constructor frame) fm frame)
  (funcall (frame-layout-constructor frame) fm frame))

(defmethod note-frame-enabled
    ((fm standard-frame-manager) (frame standard-application-frame))
  (declare (ignore fm))
  (setf (sheet-enabled-p (frame-top-level-sheet frame)) t))

(defmethod note-frame-disabled
    ((fm standard-frame-manager) (frame standard-application-frame))
  (declare (ignore fm))
  (let ((top-level-sheet (frame-top-level-sheet frame)))
    (setf (sheet-enabled-p top-level-sheet) nil)))

(defmethod note-frame-iconified
    ((fm standard-frame-manager) (frame standard-application-frame))
  (declare (ignore fm))
  (shrink-sheet (frame-top-level-sheet frame)))

(defmethod note-frame-deiconified
    ((fm standard-frame-manager) (frame standard-application-frame))
  (declare (ignore fm))
  (setf (sheet-enabled-p (frame-top-level-sheet frame)) t))

(defmethod note-command-enabled
    ((fm standard-frame-manager) (frame standard-application-frame) command-name)
  (declare (ignore fm))
  (menu-bar-refresh-command frame command-name t))

(defmethod note-command-disabled
    ((fm standard-frame-manager) (frame standard-application-frame) command-name)
  (declare (ignore fm))
  (menu-bar-refresh-command frame command-name nil))

(defmethod note-frame-pretty-name-changed
    ((fm standard-frame-manager) (frame standard-application-frame) new-value)
  (declare (ignore fm))
  ;; If there is a top-level sheet, set its pretty name. The port can reflect
  ;; this change in the window title.
  (when-let ((top-level-sheet (frame-top-level-sheet frame)))
    (setf (sheet-pretty-name top-level-sheet) new-value)))

(defmethod note-frame-icon-changed
    ((fm standard-frame-manager) (frame standard-application-frame) new-value)
  (declare (ignore fm))
  ;; If there is a top-level sheet, set its icon. The port can reflect
  ;; this change by telling the window manager which might display the
  ;; new icon somewhere.
  (when-let ((top-level-sheet (frame-top-level-sheet frame)))
    (setf (sheet-icon top-level-sheet) new-value)))

(defmethod note-frame-command-table-changed
    ((fm standard-frame-manager) (frame standard-application-frame) new-command-table)
  ;; Update the menu-bar even if its command-table doesn't change to ensure
  ;; that disabled commands are not active (and vice versa). -- jd 2020-12-12
  (when-let* ((menu-bar (frame-menu-bar-pane frame))
              (bar-command-table (slot-value frame 'menu-bar)))
    (if (eq bar-command-table t)
        (update-menu-bar (frame-menu-bar-pane frame) frame new-command-table)
        (update-menu-bar (frame-menu-bar-pane frame) frame bar-command-table))))

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
      (sheet-adopt-child graft t-l-s))
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
    (sheet-disown-child (sheet-parent tps) tps))
  (setf (%frame-manager frame) nil))
