;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 1998-2000 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) Copyright 2000 by Robert Strandh <strandh@labri.u-bordeaux.fr>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-demo)

(defun address-book ()
  (declare (special frame fm port pane medium graft))
  (loop for port in climi::*all-ports*
      do (destroy-port port))
  (setq climi::*all-ports* nil)
  (setq frame (make-application-frame 'address-book))
;  (setq fm (frame-manager frame))
;  (setq port (port fm))
;  (setq pane (frame-standard-output frame))
;  (setq medium (sheet-medium pane))
;  (setq graft (graft frame))
  (run-frame-top-level frame))

(defun test-define-application-frame ()
  (macroexpand '(define-application-frame address-book ()
    ;; This application has two state variables, the currently displayed
    ;; address and the window from which user queries should be read.
    ((current-address :initform nil)
     (interaction-pane )
     (name-pane))
  (:panes
    (interactor :interactor)
    (address :application
             :incremental-redisplay t
             :display-function 'display-current-address)
    (names :application
           :incremental-redisplay t
           :display-function 'display-names))
  (:layouts
    (default
      (vertically ()
        (horizontally ()
          address names)
        interactor))))))
