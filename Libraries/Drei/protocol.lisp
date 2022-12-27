;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2007 Troels Henriksen <thenriksen@common-lisp.net>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Protocol generic function provided by the Drei library.

(in-package #:drei)

;;; Convenience stuff

(defgeneric drei-instance-of (object)
  (:documentation "Return the Drei instance of `object'. For an
editor frame, this would be the active editor instance. If
`object' itself is a Drei instance, this function should just
return `object'."))

(defun drei-instance (&optional (object *esa-instance*))
  "Return the Drei instance of `object'. If `object' is not
provided, the currently running Drei instance will be returned."
  (and object (drei-instance-of object)))

(defun (setf drei-instance) (new-instance &optional (object *esa-instance*))
  (setf (drei-instance-of object) new-instance))

(defun current-view (&optional (object (drei-instance)))
  "Return the view of the provided object. If no object is
provided, the currently running Drei instance will be used."
  (and object (view object)))

(defun (setf current-view) (new-view &optional (object (drei-instance)))
  (setf (view object) new-view))

(defgeneric point-of (object)
  (:documentation "Return the mark object that is the point of
`object'. Some objects have their own points, for example Drei
buffer-views and buffers."))

(defun point (&optional (object (current-view)))
  "Return the point of the provided object. If no object is
provided, the current view will be used."
  (point-of object))

(defun (setf point) (new-point object)
  (setf (point-of object) new-point))

(defgeneric mark-of (object)
  (:documentation "Return the mark object that is the mark of
`object'. Some objects have their own points, for example Drei
instances."))

(defun mark (&optional (object (current-view)))
  "Return the mark of the provided object. If no object is
provided, the current view will be used."
  (mark-of object))

(defun (setf mark) (new-mark object)
  (setf (mark-of object) new-mark))

(defun current-syntax ()
  "Return the syntax of the current buffer."
  (syntax (current-view)))

;;; Activity protocol

(defgeneric active (object)
  (:documentation "Whether OBJECT (a Drei instance, view or cursor) is
active or not.

An active cursor is drawn using the active ink, and an inactive is
drawn using the inactive ink. Typically, a cursor will be active when
the associated Drei view has focus."))

;;; Redisplay engine protocol

(defgeneric display-drei-view-contents (stream view)
  (:documentation "The purpose of this function is to display the
contents of a Drei view to some output surface.

STREAM is the CLIM output stream that redisplay should be performed
on, VIEW is the Drei view instance that is being displayed.

Methods defined for this generic function can draw whatever they want,
but they should not assume that they are the only user of STREAM,
unless the STREAM argument has been specialized to some
application-specific pane class that can guarantee this. For example,
when accepting multiple values using the `accepting-values' macro,
several Drei instances will be displayed simultaneously on the same
stream. It is permitted to only specialise STREAM on
`clim-stream-pane' and not `extended-output-stream'. When writing
methods for this function, be aware that you cannot assume that the
buffer will contain only characters, and that any subsequence of the
buffer is coercable to a string. Drei buffers can contain arbitrary
objects, and redisplay methods are required to handle this (though
they are not required to handle it nicely, they can just ignore the
object, or display the `princ'ed representation.)"))

(defgeneric display-drei-view-cursor (stream view cursor)
  (:documentation "The purpose of this function is to display a
visible indication of a cursor of a Drei view to some output surface.

STREAM is the CLIM output stream that drawing should be performed on,
VIEW is the Drei view object that is being redisplayed, CURSOR is the
cursor object to be displayed (a subclass of `drei-cursor').

Methods on this generic function can draw whatever they want, but they
should not assume that they are the only user of STREAM, unless the
STREAM argument has been specialized to some application-specific pane
class that can guarantee this. It is permitted to only specialise
STREAM on `clim-stream-pane' and not `extended-output-stream'. It is
recommended to use the function `offset-to-screen-position' to
determine where to draw the visual representation for the cursor. It
is also recommended to use the ink specified by CURSOR to perform the
drawing, if applicable. This method will only be called by the Drei
redisplay engine when the cursor is active and the buffer position it
refers to is on display - therefore, `offset-to-screen-position' is
*guaranteed* to not return NIL or T."))
