;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-INTERNALS; -*-
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

(in-package :CLIM-INTERNALS)

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

(defun x-event-to-key-name-and-modifiers (display keycode state)
  (let* ((modifiers (mapcar (lambda (keycodes)
                              (mapcar (lambda (keycode)
                                        (lookup-keysym
                                         (xlib:keycode->keysym display keycode 0)))
                                      keycodes))
                            (multiple-value-list (xlib:modifier-mapping display))))
         (active-modifiers
          (loop for i from 0 below 8 
              when (logbitp i state)
              append (elt modifiers i))))
    (let* ((shift?  (or (member :shift-left active-modifiers)
                        (member :shift-right active-modifiers)))
           (shift-lock? (member :shift-lock active-modifiers))
           (caps-lock?  (member :caps-lock active-modifiers))
           (keysym (xlib:keycode->keysym display keycode 
                                         (+ (if (if shift-lock? 
                                                    (not shift?) 
                                                    (if caps-lock? t shift?))
                                                1 0)
                                            (if (member :mode-switch active-modifiers)
                                                2 0))))
	   (char (xlib:keysym->character display keysym
					 (+ (if (if shift-lock? 
                                                    (not shift?) 
						  (if caps-lock? t shift?))
                                                1 0)
                                            (if (member :mode-switch active-modifiers)
                                                2 0)))))
                                                   
      (values (or char (lookup-keysym keysym))
              (+ (if (or (member :meta-left active-modifiers)
                         (member :meta-right active-modifiers))
                     +meta-key+
                   0)
                 (if (or (member :hyper-left active-modifiers)
                         (member :hyper-right active-modifiers))
                     +hyper-key+
                   0)
                 (if (or (member :super-left active-modifiers)
                         (member :super-right active-modifiers))
                     +super-key+
                   0)
                 (if (or (member :shift-left active-modifiers)
                         (member :shift-right active-modifiers))
                     +shift-key+
                   0)
                 (if (or (member :control-left active-modifiers)
                         (member :control-right active-modifiers))
                     +control-key+
                   0)
                 (if (or (member :alt-left active-modifiers)
                         (member :alt-right active-modifiers))
                     +alt-key+
                   0))) )))

;;;;

(defun numeric-keysym-to-character (keysym)
  (and (<= 0 keysym 255)
       (code-char keysym)))

(defun keysym-to-character (keysym)
  (numeric-keysym-to-character (reverse-lookup-keysym keysym)))

(defmethod keyboard-event-character ((keyboard-event keyboard-event))
  (and (zerop (logand (event-modifier-state keyboard-event)
                      (logior +meta-key+ +hyper-key+ +super-key+ +control-key+ +alt-key+)))
       (keysym-to-character (keyboard-event-key-name keyboard-event))))
