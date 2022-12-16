(in-package #:climi)

;;; 22.4 The Pointer Protocol
;;;
;;; Methods pointer-button-state, pointer-position and (setf pointer-position)
;;; are implemented by the backend that query the display server for the
;;; current pointer state. In the future we may want to cache the current
;;; pointer state and position based on pointer-events.

(defclass standard-pointer (pointer)
  ((port :reader port :initarg :port)
   (sheet :initform nil :initarg :sheet :accessor pointer-sheet)
   (cursor :initform nil :initarg :cursor :accessor pointer-cursor)))

(defmethod pointer-button-state ((pointer standard-pointer))
  +pointer-no-button+)

(defmethod pointer-position ((pointer standard-pointer))
  (values 0 0))

(defmethod* (setf pointer-position) (x y (pointer standard-pointer))
  (error "Don't know how to set the pointer position."))
