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

(declaim (inline round-coordinate))
(defun round-coordinate (x)
  (floor (+ x .5)))

(defun requirement->size (req)
  (gfs:make-size :width (round-coordinate (space-requirement-width req))
                 :height (round-coordinate (space-requirement-height req))))

(defun translate-rectangle (gfw-rect)
  (let ((pnt (gfs:location gfw-rect))
        (size (gfs:size gfw-rect)))
    (make-rectangle* (gfs:point-x pnt)
                     (gfs:point-y pnt)
                     (+ (gfs:point-x pnt) (gfs:size-width size))
                     (+ (gfs:point-y pnt) (gfs:size-height size)))))

(declaim (inline coordinates->rectangle))
(defun coordinates->rectangle (left top right bottom)
  (gfs:create-rectangle :x (round-coordinate left)
                        :y (round-coordinate top)
                        :width (round-coordinate (- right left))
                        :height (round-coordinate (- bottom top))))

(defun coordinates->points (list)
  (cond
    ((null list) (values))
    ((and (car list) (cdr list))
      (concatenate 'list (list (gfs:make-point :x (round-coordinate (car list)) 
                                               :y (round-coordinate (car (cdr list)))))
                         (coordinates->points (cdr (cdr list)))))))
