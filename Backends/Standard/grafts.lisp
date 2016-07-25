(in-package :clim-standard)

(defclass standard-graft (graft)
  ())

(defmethod sheet-direct-mirror ((sheet standard-graft))
  (port-lookup-mirror (port sheet) sheet))

(defmethod (setf sheet-direct-mirror) (mirror (sheet standard-graft))
  (port-register-mirror (port sheet) sheet mirror))
