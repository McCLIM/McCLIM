(in-package :clim-user)

(define-application-frame simple ()
  ()
  (:menu-bar nil)
  (:panes
   (app :application
        :display-function #'draw-simple
        :draw :command-loop :record nil :scroll-bars nil
        :height 50 :width 100))
  (:layouts (:default app)))

(let ((counter 0))
  (defmethod draw-simple ((pane simple) stream)
    (draw-text* stream "Hello" 20 20)
    (draw-text* stream "X" (+ (mod counter 40) 20) 40)
    (incf counter)
    (clim-internals::schedule-timer-event stream 'update 1.0)))

(defmethod handle-event ((pane application-pane) (event timer-event))
  (redisplay-frame-panes *application-frame* :force-p t))

(defmethod handle-repaint  ((pane application-pane) region)
  (declare (ignore region))
  (redisplay-frame-panes *application-frame* :force-p t))

(defun simple-main ()
  (run-frame-top-level (make-application-frame 'simple))) 