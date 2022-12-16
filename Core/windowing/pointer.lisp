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

;;; Normally we'd use SHEET-DELTA-TRANSFORMATION from the sheet to the graft
;;; but it is more prone to rounding errors than transforming the position one
;;; transformation at a time (most notably for rotations). -- jd 2022-12-16
(defun sheet-position-from-screen-position (sheet screen-x screen-y)
  (labels ((untransform-delta (sheet root root-x root-y)
             (if (or (null sheet) (eq sheet root))
                 (values root-x root-y)
                 (let ((parent (sheet-parent sheet))
                       (transformation (sheet-transformation sheet)))
                   (multiple-value-bind (parent-x parent-y)
                       (untransform-delta parent root root-x root-y)
                     (untransform-position transformation parent-x parent-y))))))
    (let* ((graft (graft* sheet))
           (graft-transformation (sheet-native-transformation graft)))
      (multiple-value-bind (graft-x graft-y)
          (untransform-position graft-transformation screen-x screen-y)
        (multiple-value-bind (sheet-x sheet-y)
            (untransform-delta sheet graft graft-x graft-y)
          (values sheet-x sheet-y graft-x graft-y screen-x screen-y))))))

(defun screen-position-from-sheet-position (sheet sheet-x sheet-y)
  (labels ((transform-delta (sheet root sheet-x sheet-y)
             (if (or (null sheet) (eq sheet root))
                 (values sheet-x sheet-y)
                 (let ((parent (sheet-parent sheet))
                       (transformation (sheet-transformation sheet)))
                   (multiple-value-call #'transform-delta parent root
                     (transform-position transformation sheet-x sheet-y))))))
    (let* ((graft (graft* sheet))
           (graft-transformation (sheet-native-transformation graft)))
      (multiple-value-bind (graft-x graft-y)
          (transform-delta sheet graft sheet-x sheet-y)
        (multiple-value-bind (screen-x screen-y)
            (transform-position graft-transformation graft-x graft-y)
          (values screen-x screen-y graft-x graft-y sheet-x sheet-y))))))

(defun sheet-pointer-position (sheet pointer)
  (multiple-value-bind (screen-x screen-y) (pointer-position pointer)
    (multiple-value-bind (sheet-x sheet-y)
        (sheet-position-from-screen-position sheet screen-x screen-y)
      (values sheet-x sheet-y))))

(defun set-sheet-pointer-position (sheet pointer sheet-x sheet-y)
  (multiple-value-bind (screen-x screen-y)
      (screen-position-from-sheet-position sheet sheet-x sheet-y)
    (setf (pointer-position pointer) (values screen-x screen-y))))
