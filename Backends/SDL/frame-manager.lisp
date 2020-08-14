;;; -*- Mode: Lisp; Package: CLIM-SDL -*-

;;; (c) 2005 Christophe Rhodes (c.rhodes@gold.ac.uk)

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

(in-package :clim-sdl)

(defclass sdl-frame-manager (frame-manager)
  ())

(defmethod adopt-frame :after ((fm sdl-frame-manager) (frame application-frame))
  (log:info "fm=~s frame=~s" fm frame)
  (let ((mirror (sheet-direct-mirror (frame-top-level-sheet frame))))
    nil))

(defmethod note-space-requirements-changed :after ((graft sdl-graft) pane)
  ())

#+nil
(defmethod sheet-direct-mirror ((sheet sdl-top-level-sheet-pane))
  ())

(defmethod find-concrete-pane-class ((fm sdl-frame-manager) pane-type &optional errorp)
  (if (eq pane-type 'climi::top-level-sheet-pane)
      (find-class 'sdl-top-level-sheet-pane)
      (call-next-method)))
