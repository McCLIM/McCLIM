;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2001 by Tim Moore <moore@bricoworks.com>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-internals)

;;; Views are defined here and not in presentations.lisp so that they
;;; can be referenced in the streams code.

(defclass textual-view (view)
  ())

(defclass textual-menu-view (textual-view)
  ())

(defclass textual-dialog-view (textual-view)
  ())

(defclass gadget-view (view)
  ((foreground :initarg :foreground :reader gadget-view-foreground)
   (background :initarg :background :reader gadget-view-background)
   (text-style :initarg :text-style :reader gadget-view-text-style)))

(defclass gadget-menu-view (gadget-view)
  ())

(defclass gadget-dialog-view (gadget-view)
  ())

(defclass pointer-documentation-view (textual-view)
  ())


(defparameter +textual-view+ (make-instance 'textual-view))

(defparameter +textual-menu-view+ (make-instance 'textual-menu-view))

(defparameter +textual-dialog-view+ (make-instance 'textual-dialog-view))

(defparameter +gadget-view+ (make-instance 'gadget-view))

(defparameter +gadget-menu-view+ (make-instance 'gadget-menu-view))

(defparameter +gadget-dialog-view+ (make-instance 'gadget-dialog-view))

(defparameter +pointer-documentation-view+ (make-instance 'pointer-documentation-view))


(defmethod stream-default-view (stream)
  (declare (ignore stream))
  +textual-view+)
