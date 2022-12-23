;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2001 by Tim Moore <moore@bricoworks.com>
;;;  (c) Copyright 2022 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-demo)

(defclass echo-interactor-pane (interactor-pane)
  ())

(defmethod stream-read-gesture :around ((stream echo-interactor-pane)
                                       &key &allow-other-keys)
  (let* ((results (multiple-value-list (call-next-method)))
         (gesture (car results)))
    (when gesture
      (with-drawing-options (t :ink +dark-green+)
        (print gesture)))
    (values-list results)))

(defmethod handle-event :around ((stream echo-interactor-pane) event)
  (with-drawing-options (t :ink +dark-red+)
    (print event))
  (call-next-method))

(define-application-frame stream-test ()
  ()
  (:menu-bar nil)
  (:panes
   (tester (make-clim-stream-pane :type 'echo-interactor-pane))
   (shower :application))
  (:layouts
   (default (vertically ()
              tester
              shower))))
