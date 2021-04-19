;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998-2000 Michael McDonald <mikemac@mikemac.com>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-clx)

;;; CLX-GRAFT class

(defclass clx-graft (graft) ())

(defmethod graft-width ((graft clx-graft) &key (units :device))
  (let ((screen (clx-port-screen (port graft))))
    (ecase units
      (:device (xlib:screen-width screen))
      (:inches (/ (xlib:screen-width-in-millimeters screen) 25.4s0))
      (:millimeters (xlib:screen-width-in-millimeters screen))
      (:screen-sized 1))))

(defmethod graft-height ((graft clx-graft) &key (units :device))
  (let ((screen (clx-port-screen (port graft))))
    (ecase units
      (:device (xlib:screen-height screen))
      (:inches (/ (xlib:screen-height-in-millimeters screen) 25.4s0))
      (:millimeters (xlib:screen-height-in-millimeters screen))
      (:screen-sized 1))))


