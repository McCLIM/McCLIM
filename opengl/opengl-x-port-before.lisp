;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2001 by  Julien Boninfante (boninfan@emi.u-bordeaux.fr)

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

;; OpenGL with X mixin class

(defclass opengl-graphical-system-port-mixin ()
  ((display :initform nil
	    :reader opengl-port-display)
   (screen :initform nil
	   :reader opengl-port-screen)
   (root :initform nil
	 :reader opengl-port-root)
   (font-table :initform (make-hash-table :test #'eq))
   (top-level :type top-level-sheet-pane
	      :initform nil
	      :accessor opengl-port-top-level))
  (:documentation "This port-mixin class store elements relative to the X-Windows system architecture"))


(defmacro flush (port sheet)
  `(gl:glXSwapBuffers (opengl-port-display ,port) (sheet-direct-mirror ,sheet)))

(defun find-array-address (array)
  (alien::sap-int (cmucl-interface:array-data-address array)))

(defparameter viewport-infos (make-array 4 :element-type '(unsigned-byte 32)))

(defun update-viewport-infos (x y width height)
  (declare (type fixnum x y width height))
  (setf (aref viewport-infos 0) x
	(aref viewport-infos 1) y
	(aref viewport-infos 2) width
	(aref viewport-infos 3) height))
	

;; This function must be defined relativy to a graphical system
;; because the coordinate system defines the orientation of
;; the frustrum.
(defun opengl-reshape (width height)
  (declare (type fixnum width height))
  (gl:glViewport 0 0 width height)
  (update-viewport-infos 0 0 width height)
  (gl:glMatrixMode gl:GL_PROJECTION)
  (gl:glLoadIdentity)
  ; using glOrtho with near=-1d0 et far=1d0 is equivalent to use glOrtho2D which is not implemented in the bindings
  (gl:glOrtho 0d0 (coerce width 'double-float) (coerce height 'double-float) 0d0 -1d0 1d0)
  (gl:glMatrixMode gl:GL_MODELVIEW)
  (gl:glLoadIdentity))

