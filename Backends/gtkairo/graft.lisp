;;; -*- Mode: Lisp; Package: CLIM-CLX -*-

;;;  (c) 2006 David Lichteblau (david@lichteblau.com)
;;; based on the null backend by:
;;;  (c) 2005 Christophe Rhodes (c.rhodes@gold.ac.uk)

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

(in-package :clim-gtkairo)

(defclass gtkairo-graft (graft)
  ())

(defmethod graft-width ((graft gtkairo-graft) &key (units :device))
  (with-gtk ()
    (let ((screen (gdk_screen_get_default)))
      (ecase units
	(:device (gdk_screen_get_width screen))
	(:millimeters (gdk_screen_get_width_mm screen))
	(:inches (/ (gdk_screen_get_width_mm screen) 25.4))
	(:screen-sized 1)))))

(defmethod graft-height ((graft gtkairo-graft) &key (units :device))
  (with-gtk ()
    (let ((screen (gdk_screen_get_default)))
      (ecase units
	(:device (gdk_screen_get_height screen))
	(:millimeters (gdk_screen_get_height_mm screen))
	(:inches (/ (gdk_screen_get_height_mm screen) 25.4))
	(:screen-sized 1)))))
