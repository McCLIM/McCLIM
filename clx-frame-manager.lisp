;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)

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

;;; CLX-FRAME-MANAGER class

(defclass clx-frame-manager (frame-manager)
  ()
  )

(defmethod make-pane-1 ((fm clx-frame-manager) (frame application-frame) type &rest args)
  (apply #'make-instance type ; (intern (concatenate 'string "X11-" (symbol-name type)) :clim)
	 :frame frame
	 :manager fm
	 :port (port frame)
	 args))


;;; include later for menus
;(defmethod adopt-frame :after ((fm clx-frame-manager) (frame menu-frame))
;  (xlib:map-window (sheet-direct-mirror (slot-value frame 'top-level-sheet))))

(defmethod adopt-frame :after ((fm clx-frame-manager) (frame application-frame))
  (xlib:map-window (sheet-direct-mirror (slot-value frame 'top-level-sheet))))
