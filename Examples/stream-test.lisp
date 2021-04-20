;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2001 by Tim Moore <moore@bricoworks.com>
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
      (print gesture *trace-output*))
    (values-list results)))

(define-application-frame stream-test ()
  ()
  (:menu-bar nil)
  (:panes
   (tester (make-clim-stream-pane :type 'echo-interactor-pane)))
  (:layouts
   (default (vertically () tester))))
