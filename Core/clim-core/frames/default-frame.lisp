;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2021 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; The default application frame. It is used as a global value for the variable
;;; *application-frame* to ensure that the variable is always bound to a frame.
;;;

(in-package #:climi)

(define-application-frame default-application () ()
  (:command-definer nil)
  (:menu-bar nil)
  (:top-level ((lambda (frame) (declare (ignore frame))))))

(defvar *default-application-frame*
  (make-application-frame 'default-application))

(defmethod command-enabled (command-name (frame default-application))
  (declare (ignore command-name frame))
  t)

(setf *application-frame* *default-application-frame*)
