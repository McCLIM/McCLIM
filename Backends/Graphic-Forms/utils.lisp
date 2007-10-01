;;; -*- Mode: Lisp; Package: CLIM-GRAPHIC-FORMS; -*-

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

(defun requirement->size (req)
  (gfs:make-size :width (floor (space-requirement-width req))
                 :height (floor (space-requirement-height req))))

(defun translate-rectangle (gfw-rect)
  (let ((pnt (gfs:location gfw-rect))
        (size (gfs:size gfw-rect)))
    (make-rectangle* (gfs:point-x pnt)
                     (gfs:point-y pnt)
                     (+ (gfs:point-x pnt) (gfs:size-width size))
                     (+ (gfs:point-y pnt) (gfs:size-height size)))))

(declaim (inline coordinates->rectangle))
(defun coordinates->rectangle (left top right bottom)
  (gfs:create-rectangle :x (floor left)
                        :y (floor top)
                        :width (floor (- right left))
                        :height (floor (- bottom top))))

(defun coordinates->points (seq)
  (loop for i from 0 below (length seq) by 2
        collect (gfs:make-point :x (floor (elt seq i))
                                :y (floor (elt seq (+ i 1))))))

(declaim (inline radians->degrees))
(defun radians->degrees (rads)
  (floor (* rads 180) pi))
