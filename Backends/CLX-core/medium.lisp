;;;  (c) copyright 2016 by 
;;;           Robert Strandh (robert.strandh@gmail.com)

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

(cl:in-package #:mcclim-clx-core)

;;; A point is said to be PAINTABLE if its coordinates are valid for
;;; X11.  Recall that coordinates in X11 are signed 16-bit integers.
(defun point-paintable-p (x y)
  (and (<= #x-8000 x #x7fff)
       (<= #x-8000 y #x7fff)))

;;; A line segment is said to be paintable if both its endpoints are
;;; paintable.
(defun line-segment-paintable-p (x1 y1 x2 y2)
  (and (point-paintable-p x1 y1)
       (point-paintable-p x2 y2)))
