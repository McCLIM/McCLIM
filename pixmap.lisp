;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2001 by Iban HATCHONDO (hatchond@mei.u-bordeaux.fr)

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

(defclass pixmap ()
  ((sheet :initarg :sheet :reader pixmap-sheet)
   (width :initarg :width :reader pixmap-width)
   (height :initarg :height :reader pixmap-height)
   ))

(defgeneric pixmap-mirror (mirrored-pixmap))
(defgeneric allocate-pixmap (sheet width height))
(defgeneric deallocate-pixmap (pixmap))
(defgeneric copy-to-pixmap (sheet sheet-x sheet-y width height 
			    &optional pixmap (pixmap-x 0) (pixmap-y 0)))
(defgeneric copy-from-pixmap (pixmap from-x from-y width height sheet to-x to-y))
(defgeneric copy-area (sheet from-x from-y width height to-x to-y))

(defclass mirrored-pixmap (pixmap)
  ((port :initform nil :initarg :port :accessor port)
   (medium :initform nil :accessor pixmap-medium)
   (region :initform nil :accessor sheet-region)
   ))

(defmethod initialize-instance :after ((pixmap mirrored-pixmap) &rest args)
  (declare (ignore args))
  (with-slots (width height region) pixmap
     (setf region (make-bounding-rectangle 0 0 width height))))

(defmethod pixmap-mirror ((pixmap mirrored-pixmap))
  (port-lookup-mirror (port pixmap) pixmap))

(defmethod allocate-pixmap ((sheet sheet) width height)
  (port-allocate-pixmap (port sheet) sheet width height))

(defmethod deallocate-pixmap ((pixmap pixmap))
  (port-deallocate-pixmap (port (pixmap-sheet pixmap)) pixmap))







