;;; -*- Mode: Lisp; Package: GOATEE -*-

;;;  (c) copyright 2002 by Tim Moore (moore@bricoworks.com)
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

(in-package :goatee)

;;; Mostly implements dbl-list-head
(defclass dbl-super ()
  ((next :accessor next :initarg :next :initform nil)))

(defgeneric prev (dbl-list))

(defmethod prev ((dbl-list dbl-super))
  nil)

(defclass dbl-list (dbl-super)
  ((prev :accessor prev :initarg :prev :initform nil)))

(defgeneric dbl-insert-after (new-element dbl-list))

(defmethod dbl-insert-after ((new-element dbl-list) (dbl-list dbl-super))
  (setf (prev new-element) dbl-list)
  (setf (next new-element) (next dbl-list))
  (when (next dbl-list)
    (setf (prev (next dbl-list)) new-element))
  (setf (next dbl-list) new-element)
  new-element)

(defgeneric dbl-insert-before (new-element dbl-list))

(defmethod dbl-insert-before ((new-element dbl-list) (dbl-list dbl-list))
  (setf (next new-element) dbl-list)
  (setf (prev new-element) (prev dbl-list))
  (when (prev dbl-list)
    (setf (next (prev dbl-list)) new-element))
  (setf (prev dbl-list) new-element)
  new-element)

(defgeneric dbl-remove (element))

(defmethod dbl-remove ((element dbl-list))
  (when (prev element)
    (setf (next (prev element)) (next element)))
  (when (next element)
    (setf (prev (next element)) (prev element)))
  nil)

(defgeneric dbl-kill-after (element)
  (:documentation "Remove all elements after element."))

(defmethod dbl-kill-after ((element dbl-super))
  (let ((next (next element)))
    (when next
      (setf (prev next) nil))
    (setf (next element) nil)
    element))

(defclass dbl-list-head (dbl-super)
  ())

(defmethod dbl-head ((dbl-list dbl-list-head))
  (next dbl-list))

(defmethod (setf dbl-head) (val (dbl-list dbl-list-head))
  (setf (next dbl-list) val))

(defclass dbl-list-cell (dbl-list)
  ((contents :accessor contents :initarg :contents :initform nil)))


(defun make-dbl-list ()
  (make-instance 'dbl-list-head))

(defun insert-obj-before (obj dbl-list)
  (let ((cell (make-instance 'dbl-list-cell :contents obj)))
    (dbl-insert-before cell dbl-list)))

(defun insert-obj-after (obj dbl-list)
  (let ((cell (make-instance 'dbl-list-cell :contents obj)))
    (dbl-insert-after cell dbl-list)))

(defun dbl-list-elements (dbl-list)
  (loop for dbl = (dbl-head dbl-list) then (next dbl)
	while dbl
	collect (contents dbl)))
