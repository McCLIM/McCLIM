;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2005 by Christophe Rhodes <c.rhodes@gold.ac.uk>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-null)

(defclass null-graft (graft)
  ())

(defmethod graft-width ((graft null-graft) &key (units :device))
  (declare (ignore units))
  nil)

(defmethod graft-height ((graft null-graft) &key (units :device))
  (declare (ignore units))
  nil)
