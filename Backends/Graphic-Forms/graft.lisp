;;; -*- Mode: Lisp; Package: CLIM-GRAPHIC-FORMS -*-

;;; (c) 2006-2007 Jack D. Unrue (jdunrue (at) gmail (dot) com)
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

(defclass graphic-forms-graft (graft)
  ())

(defmethod graft-width ((graft graphic-forms-graft) &key (units :device))
  (gfw:with-root-window (window)
    (let ((size (gfw:size window)))
      (gfw:with-graphics-context (gc window)
        (ecase units
          (:device       (gfs:size-width size))
          (:millimeters  (gfs::get-device-caps (gfs:handle gc) gfs::+horzsize+))
          (:inches       (floor (gfs:size-width size)
                                (gfs::get-device-caps (gfs:handle gc) gfs::+logpixelsx+)))
          (:screen-sized 1))))))

(defmethod graft-height ((graft graphic-forms-graft) &key (units :device))
  (gfw:with-root-window (window)
    (let ((size (gfw:size window)))
      (gfw:with-graphics-context (gc window)
        (ecase units
          (:device       (gfs:size-height size))
          (:millimeters  (gfs::get-device-caps (gfs:handle gc) gfs::+vertsize+))
          (:inches       (floor (gfs:size-height size)
                                (gfs::get-device-caps (gfs:handle gc) gfs::+logpixelsy+)))
          (:screen-sized 1))))))
