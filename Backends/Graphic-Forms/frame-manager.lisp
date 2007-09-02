;;; -*- Mode: Lisp; Package: CLIM-GRAPHIC-FORMS -*-

;;; (c) 2006 Jack D. Unrue (jdunrue (at) gmail (dot) com)
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

(in-package :clim-graphic-forms)

(defclass graphic-forms-frame-manager (frame-manager)
  ())

(defmethod make-pane-1 ((fmgr graphic-forms-frame-manager) (frame application-frame) type &rest initargs)
  #+nil (gfs::debug-format "make-pane-1 type: ~a initargs: ~a~%" type initargs)
  (apply #'make-pane-2 type :manager fmgr :frame frame :port (port frame) initargs))

(defmethod adopt-frame :after ((fmgr graphic-forms-frame-manager) (frame application-frame))
  ())

(defmethod note-space-requirements-changed :after ((graft graphic-forms-graft) pane)
  #+nil (gfs::debug-format "space requirements changed: ~a~%" pane))
