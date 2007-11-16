;;; -*- Mode: Lisp; Package: DREI-CORE -*-

;;;  (c) copyright 2007 by
;;;           Troels Henriksen (athas@sigkill.dk)

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

;;; Facilities and protocols for iterating through buffer objects, the
;;; point being that the buffer may be magically exchanged for some
;;; other buffer, permitting easy iteration through multiple buffers
;;; as a single sequence. This is meant to support Climacs'
;;; Group-facility, I'm not sure what else it could be used for.

(in-package :drei-core)

(defclass target-specification ()
  ((%drei :reader drei-instance
          :initarg :drei-instance
          :initform (error "A Drei instance must be provided for a target specification")))
  (:documentation "The base class for target specifications,
objects that permit browsing through targets for various
operations. `Target-specification' instances start off
deactivated."))

(defgeneric activate-target-specification (target-specification)
  (:documentation "Cause the Drei instance associated with
`target-specification' to switch to the \"current\" target of
`target-specification', whatever that is. It is illegal to call
any other target function on a `target-specification' object
until it has been activated by this function, and it is illegal
to call this function on an already activated
`target-specification' instance."))

(defgeneric deactivate-target-specification (target-specification)
  (:documentation "Deactivate the `target-specification'
instance, restoring whatever state the call to
`activate-target-specification' modified. It is illegal to call
`deactivate-target-specification' on a deactivated
`target-specification' instance."))

(defgeneric subsequent-targets-p (target-specification)
  (:documentation "Return true if there are more targets to act
on, that is, if the `next-target' function would not signal an
error."))

(defgeneric preceding-targets-p (target-specification)
  (:documentation "Return true if there are targets to act on in
sequence before the current target, that is, if the
`previous-target' function would not signal an error."))

(defgeneric next-target (target-specification)
  (:documentation "Change to the next target specified by the
target specification. Signals an error of type `no-more-targets'
if `subsequent-targets-p' is false."))

(defgeneric previous-target (target-specification)
  (:documentation "Change to the previous target specified by the
target specification. Signals an error of type `no-more-targets'
if `preceding-targets-p' is false."))

(define-condition no-more-targets (simple-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
	     (format stream "No more targets available for iteration")))
  (:documentation "Signal that there are no more targets
available for iteration, either forward or backwards in the
sequence of targets."))

(defclass current-buffer-target (target-specification)
  ((%buffer :accessor buffer))
  (:documentation "A target specification class specifying just
one buffer, the current buffer of the Drei instance at the time
of object creation. This is mostly used as a dummy target
specification to make target-aware commands behave \"normally\"
when no particular targets are specified."))

(defmethod initialize-instance :after ((obj current-buffer-target) &rest initargs)
  (declare (ignore initargs))
  (setf (buffer obj) (buffer (drei-instance obj))))

(defmethod activate-target-specification ((spec current-buffer-target))
  ;; Noop.
  )

(defmethod deactivate-target-specification ((spec current-buffer-target))
  ;; Noop.
  )

(defmethod subsequent-targets-p ((spec current-buffer-target))
  nil)

(defmethod preceding-targets-p ((spec current-buffer-target))
  nil)

(defmethod next-target ((spec current-buffer-target))
  (error 'no-more-targets))

(defmethod previous-target ((spec current-buffer-target))
  (error 'no-more-targets))

(defvar *default-target-creator* #'(lambda (drei)
                                     (make-instance 'current-buffer-target :drei-instance drei))
  "A function of a single argument, the Drei instance, that
creates a target specification object (or subtype thereof) that
should be used for aquiring targets.")

(defclass buffer-list-target-specification (target-specification)
  ((%buffers :initarg :buffers
             :initform '()
             :accessor buffers)
   (%buffer-count :accessor buffer-count)
   (%current-buffer-index :initform 0
                          :accessor current-buffer-index))
  (:documentation "A target specification that has a provided
list of existing buffers as its target."))

(defmethod initialize-instance :after ((obj buffer-list-target-specification)
                                       &rest initargs)
  (declare (ignore initargs))
  (setf (buffer-count obj) (length (buffers obj)))
  ;; If the current buffer is in the list of buffers, we move it to
  ;; the head of the list, since it makes sense to make it the
  ;; starting point.
  (when (/= (length (setf (buffers obj)
                          (remove (buffer (drei-instance obj)) (buffers obj))))
            (buffer-count obj))
    (push (buffer (drei-instance obj)) (buffers obj))))

(defmethod activate-target-specification ((spec buffer-list-target-specification))
  (unless (or (null (buffers spec))
              (eq (buffer (drei-instance spec)) (first (buffers spec))))
    (setf (buffer (drei-instance spec)) (first (buffers spec)))
    (beginning-of-buffer (point (drei-instance spec)))))

(defmethod deactivate-target-specification ((spec buffer-list-target-specification)))

(defmethod subsequent-targets-p ((spec buffer-list-target-specification))
  (/= (1+ (current-buffer-index spec)) (buffer-count spec)))

(defmethod preceding-targets-p ((spec buffer-list-target-specification))
  (plusp (current-buffer-index spec)))

(defmethod next-target ((spec buffer-list-target-specification))
  (if (subsequent-targets-p spec)
      (progn
        (setf (buffer (drei-instance spec))
              (elt (buffers spec) (incf (current-buffer-index spec))))
        (beginning-of-buffer (point (drei-instance spec))))
      (error 'no-more-targets)))

(defmethod previous-target ((spec buffer-list-target-specification))
  (if (preceding-targets-p spec)
      (progn
        (setf (buffer (drei-instance spec))
              (elt (buffers spec) (decf (current-buffer-index spec))))
        (end-of-buffer (point (drei-instance spec))))
      (error 'no-more-targets)))
