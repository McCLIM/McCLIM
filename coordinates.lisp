;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-INTERNALS; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: The coordinate Datatype
;;;   Created: 1998-12-05 18:06
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; --------------------------------------------------------------------------------------
;;;  (c) copyright 1998,1999 by Gilbert Baumann

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

;;!EXPORT coordinate

(in-package :CLIM-INTERNALS)

(deftype coordinate () 'double-float)

(defun coordinate (n)
  "Coerces N to be a coordinate."
  (declare (type number n))
  (coerce n 'coordinate))

(defun coordinate-epsilon ()
  ;; tweak if you like
  (* (expt 2 10) double-float-epsilon))

(defun coordinate= (x y)
  (< (abs (- x y)) (coordinate-epsilon)))

(defun coordinate<= (x y)
  (<= (- x y) (coordinate-epsilon)))

(defun coordinate/= (x y)
  (not (coordinate= x y)))
