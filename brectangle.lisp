;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)

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

;;; BOUNDING-RECTANGLE class - abstract class

(defclass bounding-rectangle ()
  (
   ))

(defun bounding-rectangle-p (x)
  (typep x 'bounding-rectangle))

(defmacro with-bounding-rectangle* ((x-min y-min x-max y-max) region &body body)
  `(multiple-value-bind (,x-min ,y-min ,x-max ,y-max) (bounding-rectangle* ,region)
     ,@body))

(defmethod bounding-rectangle* ((bounding-rectangle bounding-rectangle))
  (error "Bounding-Rectangle* is not defined for the protocol class BOUNDING-RECTANGLE"))

(defmethod bounding-rectangle-min-x ((bounding-rectangle bounding-rectangle))
  (with-bounding-rectangle* (x-min y-min x-max y-max) bounding-rectangle
    (declare (ignore y-min x-max y-max))
    x-min))

(defmethod bounding-rectangle-min-y ((bounding-rectangle bounding-rectangle))
  (with-bounding-rectangle* (x-min y-min x-max y-max) bounding-rectangle
    (declare (ignore x-min x-max y-max))
    y-min))

(defmethod bounding-rectangle-max-x ((bounding-rectangle bounding-rectangle))
  (with-bounding-rectangle* (x-min y-min x-max y-max) bounding-rectangle
    (declare (ignore x-min y-min y-max))
    x-max))

(defmethod bounding-rectangle-max-y ((bounding-rectangle bounding-rectangle))
  (with-bounding-rectangle* (x-min y-min x-max y-max) bounding-rectangle
    (declare (ignore x-min y-min x-max))
    y-max))

(defmethod bounding-rectangle-position ((bounding-rectangle bounding-rectangle))
  (with-bounding-rectangle* (x-min y-min x-max y-max) bounding-rectangle
    (declare (ignore x-max y-max))
    (make-point x-min y-min)))

(defmethod bounding-rectangle-width ((bounding-rectangle bounding-rectangle))
  (with-bounding-rectangle* (x-min y-min x-max y-max) bounding-rectangle
    (declare (ignore y-min y-max))
    (- x-max x-min)))

(defmethod bounding-rectangle-height ((bounding-rectangle bounding-rectangle))
  (with-bounding-rectangle* (x-min y-min x-max y-max) bounding-rectangle
    (declare (ignore x-min x-max))
    (- y-max y-min)))

(defmethod bounding-rectangle-size ((bounding-rectangle bounding-rectangle))
  (with-bounding-rectangle* (x-min y-min x-max y-max) bounding-rectangle
    (values (- x-max x-min) (- y-max y-min))))

;;; STANDARD-BOUNDING-RECTANGLE class - instantiatable class

(defclass standard-bounding-rectangle (bounding-rectangle)
  ((x-min :initarg :min-x)
   (y-min :initarg :min-y)
   (x-max :initarg :max-x)
   (y-max :initarg :max-y)
   ))

(defmethod print-object ((box standard-bounding-rectangle) stream)
  (with-slots (x-min y-min x-max y-max) box
    (print-unreadable-object (box stream :type t :identity t)
      (format stream "X ~D:~D Y ~D:~D" x-min x-max y-min y-max))))

(defun make-bounding-rectangle (x1 y1 x2 y2)
  (check-type x1 real)
  (check-type y1 real)
  (check-type x2 real)
  (check-type y2 real)
  (make-instance 'standard-bounding-rectangle
    :min-x (min x1 x2)
    :min-y (min y1 y2)
    :max-x (max x1 x2)
    :max-y (max y1 y2)))

(defmethod bounding-rectangle* ((bounding-rectangle standard-bounding-rectangle))
  (with-slots (x-min y-min x-max y-max) bounding-rectangle
    (values x-min y-min x-max y-max)))

