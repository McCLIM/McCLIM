;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2005 by Tim Moore (moore@bricoworks.com)
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

(in-package :clim-internals)

;;; Classes for the gadget dialog views. Eventually.

;;; A gadget that's not in the spec but which would  be useful.
(defclass pop-up-menu-view (gadget-dialog-view)
  ()
  (:documentation "A dialog view that presents the elements of a
COMPLETION presentation type as a pop-up menu."))

(defparameter +pop-up-menu-view+ (make-instance 'pop-up-menu-view))

;;; By storing these parameters and options from the COMPLETION
;;; presentation type  in this object, we avoid having to dig them
;;; out of the presentation type on each call to select-query. That
;;; would not be possible if we are accepting a subtype of COMPLETION.
(defclass av-pop-up-menu-record (standard-updating-output-record)
  ((pop-up-sequence :accessor pop-up-sequence :initform nil)
   (pop-up-test :accessor pop-up-test :initform nil)
   (pop-up-value-key :accessor pop-up-value-key :initform nil)
   (pop-up-name-key :accessor pop-up-name-key :initform nil)))

(define-presentation-method accept-present-default
    ((type completion) stream (view pop-up-menu-view)
     default default-supplied-p present-p query-identifier)
  (declare (ignore present-p))
  (unless default-supplied-p
    (setq default (funcall value-key (elt sequence 0))))
  (let ((record (updating-output (stream :unique-id query-identifier
				  :cache-value default
				  :record-type 'av-pop-up-menu-record)
		  (with-output-as-presentation
		      (stream query-identifier 'selectable-query)
		    (surrounding-output-with-border
			(stream :shape :inset :move-cursor t)
		      (write-string (funcall name-key default) stream))))))
    (setf (pop-up-sequence record) sequence)
    (setf (pop-up-test record) test)
    (setf (pop-up-value-key record) value-key)
    (setf (pop-up-name-key record) name-key)
    record))

(defmethod select-query (stream query (record av-pop-up-menu-record))
  (declare (ignore stream))
  (let* ((value-key (pop-up-value-key record))
	 (name-key (pop-up-name-key record)))
    (multiple-value-bind (new-value item event)
	(menu-choose (map 'list
			  #'(lambda (item)
			      `(,(funcall name-key item)
				 :value ,(funcall value-key item)))
			  (pop-up-sequence record)))
      (declare (ignore item))
      (when event
	(setf (value query) new-value)
	(setf (changedp query) t)))))

(defmethod deselect-query (stream query (record av-pop-up-menu-record))
  (declare (ignore stream query))
  nil)

(defmethod finalize-query-record (query (record av-pop-up-menu-record))
  (declare (ignore query))
  nil)

