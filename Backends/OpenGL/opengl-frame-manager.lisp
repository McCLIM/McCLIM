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

;;; OPENGL-FRAME-MANAGER class

(defclass opengl-frame-manager (frame-manager opengl-graphical-system-frame-manager-mixin)
  ())

#+nil
(defmethod make-pane-1 ((fm opengl-frame-manager) (frame application-frame) type &rest args)
  (if (not (find-class type nil))
      (setq type (intern (format nil "~A-PANE" type):clim)))
  (let ((sheet (apply #'make-instance type
		      :frame frame
		      :manager fm
		      :port (port frame)
		      args)))
    sheet))

(defmethod make-pane-1 ((fm opengl-frame-manager) (frame application-frame) type &rest args)
  (apply #'make-instance
	 (or (find-symbol (concatenate 'string "OPENGL-" (symbol-name type)) :climi)
	     (find-symbol (concatenate 'string "OPENGL-" (symbol-name type) "-PANE") :climi)
	     (find-symbol (concatenate 'string (symbol-name type) "-PANE") :climi)
	     type)
	 :frame frame
	 :manager fm
	 :port (port frame)
	 args))

#|
(defmethod adopt-frame :after ((fm opengl-frame-manager) (frame menu-frame))
  (xlib-gl:xmapwindow (sheet-direct-mirror (slot-value frame 'top-level-sheet))))

(defmethod adopt-frame :after ((fm opengl-frame-manager) (frame application-frame))
  (xlib-gl:xmapwindow (sheet-direct-mirror (slot-value frame 'top-level-sheet))))
|#
