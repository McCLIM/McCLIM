(in-package :climi)

(defclass standard-graft (graft)
  ())

(defmethod clim:sheet-direct-mirror ((sheet standard-graft))
  (port-lookup-mirror (port sheet) sheet))
