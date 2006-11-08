;;; -*- mode: lisp -*-
;;; 
;;; (c) copyright 2005 by Aleksandar Bakic (a_bakic@yahoo.com)
;;; 

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

;;; Buffer class that allows for specifying buffer implementation at run time.

(in-package :drei-buffer)

(defclass delegating-buffer (buffer)
  ((implementation :initarg :implementation :reader implementation))
  (:documentation "Buffer class that delegates the buffer protocol
functionality to a buffer implementation object stored in the
IMPLEMENTATION slot. Do not bind it directly to a mark. Instead, bind
the buffer from the IMPLEMENTATION slot."))

(defmethod low-mark ((buffer delegating-buffer))
  (low-mark (implementation buffer)))

(defmethod high-mark ((buffer delegating-buffer))
  (high-mark (implementation buffer)))

(defmethod modified-p ((buffer delegating-buffer))
  (modified-p (implementation buffer)))

(defmethod clear-modify ((buffer delegating-buffer))
  (clear-modify (implementation buffer)))

(defmethod size ((buffer delegating-buffer))
  (size (implementation buffer)))

(defmethod number-of-lines ((buffer delegating-buffer))
  (number-of-lines (implementation buffer)))

(defmethod insert-buffer-object ((buffer delegating-buffer) offset object)
  (insert-buffer-object (implementation buffer) offset object))

(defmethod insert-buffer-sequence ((buffer delegating-buffer) offset sequence)
  (insert-buffer-sequence (implementation buffer) offset sequence))

(defmethod delete-buffer-range ((buffer delegating-buffer) offset n)
  (delete-buffer-range (implementation buffer) offset n))

(defmethod buffer-object ((buffer delegating-buffer) offset)
  (buffer-object (implementation buffer) offset))

(defmethod (setf buffer-object) (object (buffer delegating-buffer) offset)
  (setf (buffer-object (implementation buffer) offset) object))

(defmethod buffer-sequence ((buffer delegating-buffer) offset1 offset2)
  (buffer-sequence (implementation buffer) offset1 offset2))

(defmethod buffer-line-number ((buffer delegating-buffer) offset)
  (buffer-line-number (implementation buffer) offset))

(defmethod buffer-column-number ((buffer delegating-buffer) offset)
  (buffer-column-number (implementation buffer) offset))
