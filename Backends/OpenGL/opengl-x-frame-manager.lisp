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

;;; OPENGL-X-FRAME-MANAGER-MIXIN class

(defclass opengl-graphical-system-frame-manager-mixin ()
  ()
  (:documentation "This frame-manager-mixin class store elements relative to the X-Windows system architecture"))

(defmethod adopt-frame :after ((fm opengl-graphical-system-frame-manager-mixin) (frame menu-frame))
  (let ((display (opengl-port-display (port frame))))
    (xlib-gl:XMapWindow display (sheet-direct-mirror (frame-top-level-sheet frame)))
    (xlib-gl:XFlush display)))

(defmethod adopt-frame :after ((fm opengl-graphical-system-frame-manager-mixin) (frame application-frame))
  (let ((display (opengl-port-display (port frame))))
    (xlib-gl:XMapWindow display (sheet-direct-mirror (frame-top-level-sheet frame)))
    (xlib-gl:XFlush display)))
