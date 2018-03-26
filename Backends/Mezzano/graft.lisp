;;; -*- Mode: Lisp; Package: CLIM-MEZZANO -*-

;;;  (c) copyright 2005 Christophe Rhodes (c.rhodes@gold.ac.uk)

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

(in-package :clim-mezzano)

(defparameter +width-dots-per-inch+ 96)
(defparameter +height-dots-per-inch+ 96)

(defclass mezzano-graft (graft)
  (width
   height))

(defmethod sheet-direct-mirror ((sheet mezzano-graft))
  (port-lookup-mirror (port sheet) sheet))

(defmethod (setf sheet-direct-mirror) (mirror (sheet mezzano-graft))
  (port-register-mirror (port sheet) sheet mirror))

(defmethod initialize-instance :after ((graft mezzano-graft) &rest args)
  (declare (ignore args))
  (debug-format "make-instance mezzano-graft")
  (let ((framebuffer (mos:current-framebuffer)))
    (setf (slot-value graft 'width) (mos:framebuffer-width framebuffer)
          (slot-value graft 'height) (mos:framebuffer-height framebuffer))))

(defmethod graft-width ((graft mezzano-graft) &key (units :device))
  (ecase units
    (:device (slot-value graft 'width))
    (:inches (/ (slot-value graft 'width) +width-dots-per-inch+))
    (:millimeters (/ (slot-value graft 'width) +width-dots-per-inch+ 2.54))
    (:screen-sized 1)))

(defmethod graft-height ((graft mezzano-graft) &key (units :device))
  (ecase units
    (:device (slot-value graft 'height))
    (:inches (/ (slot-value graft 'height) +width-dots-per-inch+))
    (:millimeters (/ (slot-value graft 'height) +width-dots-per-inch+ 2.54))
    (:screen-sized 1)))
