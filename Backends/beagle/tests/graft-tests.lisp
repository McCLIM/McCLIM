
(in-package :clim-user)

(define-application-frame arrow-test ()
  ()
  (:panes
   (app :application
	:display-time t
	:display-function #'display-fn
	:scroll-bars nil
	:height 400 :width 400))
  (:layouts
   (default app)))

(defun display-fn (frame pane)
  (declare (ignore frame))
  (draw-arrow* pane 50 50 350 350))

;;; -> (clim:run-frame-top-level (clim:make-application-frame 'clim-user::arrow-test))

;;; Currently (01.MAY.2005) draws:

;;;  +---------+
;;;  |         |
;;;  | \       |
;;;  |  \      |
;;;  |   \     |
;;;  |    \    |
;;;  |    _\|  |
;;;  |         |
;;;  +---------+

;;; This is ONLY because we use a flipped mirror (Cocoa internal).

;;; Changing this to NOT use a flipped mirror, but still have a
;;; graft with orientation :default.

