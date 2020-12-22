(in-package :clim-gtk)

(defclass gtk-graft (graft)
  ())

(defmethod graft-width ((graft gtk-graft) &key (units :device))
  (declare (ignore units))
  nil)

(defmethod graft-height ((graft gtk-graft) &key (units :device))
  (declare (ignore units))
  nil)
