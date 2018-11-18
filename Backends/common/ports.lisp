(in-package #:climi)

(defclass standard-port (clim:basic-port)
  ((mirrored-sheet->current-pointer-cursor :initform (make-hash-table :test #'eq))
   (selection-owner :initform nil :accessor port-selection-owner)
   (selection-requester :initform nil :accessor port-selection-requester)))

(defmethod port-lookup-current-pointer-cursor ((port standard-port) sheet)
  (gethash sheet (slot-value port 'mirrored-sheet->current-pointer-cursor)))

(defmethod climb:set-sheet-pointer-cursor :before ((port standard-port) sheet cursor)
  (setf (gethash sheet (slot-value port 'mirrored-sheet->current-pointer-cursor)) cursor))


(defmethod climb:bind-selection :around ((port standard-port) (sheet basic-sheet) &optional time)
  (setf (selection-owner port) sheet)
  (call-next-method port (sheet-mirrored-ancestor sheet) time))

(defmethod climb:release-selection :around ((port standard-port) &optional time)
  (declare (ignore time))
  (setf (selection-owner port) nil)
  (call-next-method))

(defmethod climb:request-selection :around ((port standard-port) requestor time)
  (setf (port-selection-requester port) requestor)
  (call-next-method port (sheet-mirror requestor) time))
