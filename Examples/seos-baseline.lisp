
(in-package #:clim-demo)

(define-application-frame seos-baseline ()
  ()
  (:pane :application
         :display-function #'display
         :end-of-line-action :allow
         :scroll-bars nil))

(defun show-line (stream &rest args)
  (loop for (size text) on args by #'cddr do
       (with-drawing-options (stream :text-size size)
         (format stream text)))
  (terpri stream))

(defmethod display ((frame seos-baseline) pane)
  (declare (ignore frame))
  (show-line pane :normal "Hello " :huge "world!")
  (show-line pane :normal "Second " :huge "line " :tiny "hello " :normal "world!")
  (show-line pane :huge "Third " :normal "line " :tiny "hello " :huge "world!")
  (show-line pane :normal "Last " :huge "line " :normal "bam bam")

  (terpri pane)
  (with-room-for-graphics (pane)
    (draw-line* pane 0 0 530 0))
  (terpri pane)
  (format pane "All lines should have text aligned on the same baseline. Likely failures:

1. Parts of the text with different size aligned to the top (not baseline).

2. Pressing space cause redisplay and schedules repaint after 1s. This may
exhibit different outlook of displayed and repainted output.

See the introduction in \"15.3 The Text Cursor\"."))

(define-seos-baseline-command (com-redisplay :keystroke #\space) ()
  (schedule-event *standard-output*
                  (make-instance 'window-repaint-event
                                 :region +everywhere+
                                 :sheet *standard-output*)
                  1))
