(in-package :clim-standard)

(defclass standard-graft (graft)
  ())

(defmethod sheet-direct-mirror ((sheet standard-graft))
  (port-lookup-mirror (port sheet) sheet))
