;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2001 by Julien Boninfante (boninfan@emi.u-bordeaux.fr)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

(in-package :CLIM-INTERNALS)

;;; OPENGL-GRAFT class

(defclass opengl-graft (graft)
  ()
  )

(defmethod graft-width ((graft opengl-graft) &key (units :device))
  (let ((screen (opengl-port-screen (port graft))))
    (ecase units
      (:device (xlib-gl:screen-width screen))
      (:inches (/ (xlib-gl:screen-mwidth screen) 25.4s0))
      (:millimeters (xlib-gl:screen-mwidth screen))
      (:screen-sized 1))))

(defmethod graft-height ((graft opengl-graft) &key (units :device))
  (let ((screen (opengl-port-screen (port graft))))
    (ecase units
      (:device (xlib-gl:screen-height screen))
      (:inches (/ (xlib-gl:screen-mheight screen) 25.4s0))
      (:millimeters (xlib-gl:screen-mheight screen))
      (:screen-sized 1))))

#+nil
(defmethod graft-pixels-per-millimeter ((graft opengl-graft))
  (let ((screen (opengl-port-screen (port graft))))
    (/ (xlib-gl:screen-width screen)
       (xlib-gl:screen-mwidth screen))))

#+nil
(defmethod graft-pixels-per-inch ((graft opengl-graft))
  (* (graft-pixels-per-millimeter graft) 25.4s0))

