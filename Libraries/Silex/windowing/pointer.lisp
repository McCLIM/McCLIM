(in-package #:silex)

;;; 22.4 The Pointer Protocol
;;;
;;; Implemented by the back end.  Sort of.

(defgeneric pointer-sheet (pointer))

(defgeneric (setf pointer-sheet) (sheet pointer))

(defgeneric pointer-button-state (pointer))

(defgeneric pointer-position (pointer))

(defgeneric* (setf pointer-position) (x y pointer))

(defgeneric synthesize-pointer-motion-event (pointer)
  (:documentation "Create a CLIM pointer motion event based on the current pointer state."))

(defgeneric pointer-cursor (pointer))

(defgeneric (setf pointer-cursor) (cursor pointer))

;;; FIXME: I think the standard-pointer should absorb some of the common
;;; methods that are currently entirely provided by the backends.

(defclass standard-pointer (pointer)
  ((port :reader port :initarg :port)
   (state-lock :reader state-lock :initform (make-lock "pointer lock"))
   (button-state :initform 0 )
   (sheet :initform nil :initarg :sheet :accessor pointer-sheet)))

(defmethod pointer-button-state ((pointer standard-pointer))
  (with-lock-held ((state-lock pointer))
    (slot-value pointer 'button-state)))

(defmethod pointer-update-state
    ((pointer standard-pointer) (event pointer-button-press-event))
  (with-lock-held ((state-lock pointer))
    (setf (slot-value pointer 'button-state)
          (logior (slot-value pointer 'button-state)
                  (pointer-event-button event)))))

(defmethod pointer-update-state
    ((pointer standard-pointer) (event pointer-button-release-event))
  (with-lock-held ((state-lock pointer))
    (setf (slot-value pointer 'button-state)
          (logandc2 (slot-value pointer 'button-state)
                    (pointer-event-button event)))))
