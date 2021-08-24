;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2002 Gilbert Baumann <gbaumann@common-lisp.net>
;;;  (c) copyright 2002 Alexey Dejneka <adejneka>
;;;  (c) copyright 2003 Andy Hefner <ahefner@common-lisp.net>
;;;  (c) copyright 2017 Daniel Kochmanski <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; 29.4.5 Creating a Standalone CLIM Window
;;;

(in-package #:clim-internals)

(defclass window-stream (clim-stream-pane)
  ())

(define-application-frame a-window-stream (standard-application-frame
                                           standard-encapsulating-stream
                                           standard-extended-input-stream
                                           fundamental-character-output-stream)
  ((scroll-bars :initform :vertical
                :initarg :scroll-bars)
   stream
   pane)
  (:pane
   (with-slots (stream pane scroll-bars) *application-frame*
     (multiple-value-setq (pane stream)
       (make-clim-stream-pane :name 'a-window-stream-pane
                              :display-time nil
                              :type 'window-stream
                              :scroll-bars scroll-bars
                              :height 400 :width 700))
     pane)))

(defmethod close ((stream window-stream) &key abort)
  (declare (ignore abort))
  (when-let* ((frame (pane-frame stream))
              (fm (frame-manager frame)))
    (disown-frame fm frame))
  (when (next-method-p)
    (call-next-method)))

(defun open-window-stream (&key port
                                left top right bottom width height
                                foreground background
                                text-style
                                (vertical-spacing 2)
                                end-of-line-action
                                end-of-page-action
                                output-record
                                (draw t)
                                (record t)
                                (initial-cursor-visibility :off)
                                text-margin
                                save-under
                                input-buffer
                                (scroll-bars :vertical)
                                borders
                                label)
  (declare (ignorable foreground background
                      text-style
                      vertical-spacing
                      end-of-line-action
                      end-of-page-action
                      output-record
                      draw
                      record
                      initial-cursor-visibility
                      text-margin
                      save-under
                      borders
                      label))
  (setf port (or port (find-port)))
  ;; Input buffers in the spec are not well defined for panes but at least we
  ;; know that they are vectors while event queues are deliberately
  ;; unspecified. OPEN-WINDOW-STREAM description is fudged in this regard by
  ;; allowing to specify input-buffer as either. -- jd 2019-06-21
  (let* ((fm (find-frame-manager :port port))
         (frame (apply #'make-application-frame 'a-window-stream
                       :frame-manager fm
                       :pretty-name (or label "")
                       :left left
                       :top top
                       :right right
                       :bottom bottom
                       :width width
                       :height height
                       :scroll-bars scroll-bars
                       (typecase input-buffer
                         (event-queue (list :event-queue input-buffer))
                         (vector      (list :input-buffer input-buffer))
                         (otherwise   nil)))))
    ;; Adopt and enable the pane
    (when (eq (frame-state frame) :disowned)
      (adopt-frame fm frame))
    (unless (or (eq (frame-state frame) :enabled)
                (eq (frame-state frame) :shrunk))
      (enable-frame frame))
    ;; Start a new thread to run the event loop, if necessary.
    (let ((*application-frame* frame))
      (stream-set-input-focus (encapsulating-stream-stream frame)))
    #+clim-mp
    (unless input-buffer
      (redisplay-frame-panes frame :force-p t)
      (clim-sys:make-process (lambda () (let ((*application-frame* frame))
                                          (standalone-event-loop)))))
    (encapsulating-stream-stream frame)))

(defun standalone-event-loop ()
  "An simple event loop for applications that want all events to be handled by
 handle-event methods, which also handles FRAME-EXIT."
  (let ((frame *application-frame*))
    (handler-case
        (let ((queue (frame-event-queue frame)))
          (loop for event = (event-queue-read queue)
                ;; EVENT-QUEUE-READ in single-process mode calls
                ;; PROCESS-NEXT-EVENT itself.
                do (handle-event (event-sheet event) event)))
      (frame-exit ()
        (disown-frame (frame-manager frame) frame)))))
