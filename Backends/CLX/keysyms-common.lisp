;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-XCOMMON; -*-
;;; --------------------------------------------------------------------------------------
;;;     Title: X11 keysym handling
;;;   Created: 2002-02-11
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; --------------------------------------------------------------------------------------
;;;  (c) copyright 2002 by Gilbert Baumann

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

;;; Support and port mixin for X based backends, handling keycode to
;;; keysym and character mapping, and handling of modifiers.

(in-package :clim-xcommon)

(defvar *keysym-hash-table*
    (make-hash-table :test #'eql))

(defvar *reverse-keysym-hash-table*
    (make-hash-table :test #'eq))

(defun define-keysym (name value)
  (pushnew name (gethash value *keysym-hash-table* nil))
  (setf (gethash name *reverse-keysym-hash-table*) value))

(defun lookup-keysym (value)
  (car (last (gethash value *keysym-hash-table*))))

(defun reverse-lookup-keysym (value)
  (gethash value *reverse-keysym-hash-table*))

(defclass keysym-port-mixin ()
  ((modifier-cache :accessor modifier-cache :initform nil)))

(defgeneric modifier-mapping (port)
  (:documentation "Returns an array of lists of keysym keywords for
  each of the X modifiers shift lock control mod1 mod2 mod3 mod4 mod5"))

(defmethod modifier-mapping (port)
  (error "Define me!"))

;;; The X state is the state before the current event, so key events
;;; for the modifier keys don't reflect the state that results from
;;; pressing or releasing those keys.  We want the CLIM modifiers to
;;; reflect the post event state.

(defun x-keysym-to-clim-modifiers (port event-key keychar keysym-keyword state)
  "event-key is :key-press or :key-release"
  (multiple-value-bind (clim-modifiers shift-lock? caps-lock? mode-switch?)
      (x-event-state-modifiers port state)
    (declare (ignore shift-lock? caps-lock? mode-switch?))
    (if (characterp keychar)
	clim-modifiers	;; ?? true?
	(modify-modifiers event-key
			  keysym-keyword
			  clim-modifiers))))

;;; Modifier cache
;;;
;;; Cache word is cons of two integers, CLIM modifier word and other bits for
;;; shift-lock, etc.  The defconstants below are for the other word.

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant +shift-lock+ 1)
(defconstant +caps-lock+ 2)
(defconstant +mode-switch+ 4)
)

(defconstant +clim-modifiers+ '(((:meta-left :meta-right) #.+meta-key+)
				((:hyper-left :hyper-right) #.+hyper-key+)
				((:super-left :super-right) #.+super-key+)
				((:shift-left :shift-right) #.+shift-key+)
				((:control-left :control-right)
				 #.+control-key+)))

(defconstant +other-modifiers+ '((:shift-lock #.+shift-lock+)
				 (:caps-lock #.+caps-lock+)
				 (:mode-switch #.+mode-switch+)))


(defun make-modifier-cache (port)
  (let* ((modifiers (modifier-mapping port))
	 (modifier-byte-size (length modifiers))
	 (num-modifiers (ash 1 modifier-byte-size))
	 (cache (make-array num-modifiers)))
    (loop for x-modifier from 0 below num-modifiers
	  for clim-modifier = 0
	  for other-modifier = 0
	  do (loop for bit from 0 below modifier-byte-size
		   for bit-modifiers = (aref modifiers bit)
		   when (logbitp bit x-modifier)
		   do (progn
			(loop for (syms val) in +clim-modifiers+
			      when (intersection syms bit-modifiers)
			      do (setf clim-modifier
				       (logior clim-modifier val)))
			(loop for (sym val) in +other-modifiers+
			      when (member sym bit-modifiers)
			      do (setf other-modifier
				       (logior other-modifier val))))
		   finally (setf (aref cache x-modifier)
				 (cons clim-modifier other-modifier))))
    (setf (modifier-cache port) cache)))

(defgeneric x-event-state-modifiers (port state)
  (:documentation "For the X STATE, returns as multiple values, the
  corresponding set of CLIM modifiers and flags for shift lock, caps lock, and
  mode switch."))

(defmethod x-event-state-modifiers ((port keysym-port-mixin) state)
  (with-accessors ((modifier-cache modifier-cache))
      port
    (unless modifier-cache
      (setf modifier-cache (make-modifier-cache port)))
    (destructuring-bind (clim-modifiers . other-modifiers)
	;; Mask off the button state bits.
	(aref modifier-cache
	      (mod state (length modifier-cache)))
      (values clim-modifiers
	      (logtest +shift-lock+ other-modifiers)
	      (logtest +caps-lock+ other-modifiers)
	      (logtest +mode-switch+ other-modifiers)))))

(defun modify-modifiers (event-key keysym-keyword modifiers)
  (let ((keysym-modifier (loop for (keysyms modifier) in +clim-modifiers+
			       if (member keysym-keyword keysyms)
			       return modifier)))
    (cond ((and keysym-modifier (eq event-key :key-press))
	   (logior modifiers keysym-modifier))
	  ((and keysym-modifier (eq event-key :key-release))
	   (logandc2 modifiers keysym-modifier))
	  (t modifiers))))
