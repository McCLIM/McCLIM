;;;; ----------------------------------------------------------------------
;;;;     Title: X11 standard keysym interpretation rules
;;;;   Created: 2016-01-25
;;;;    Author: Robert Strandh <robert.strandh@gmail.com>
;;;;   License: LGPL (See file COPYING for details).
;;;; ----------------------------------------------------------------------
;;;;  (c) copyright 2016 by Robert Strandh

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the 
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;;; Boston, MA  02111-1307  USA.

(cl:in-package #:clim-clx)

;;;; The purpose of the code in this file is to apply the standard X11
;;;; rules for translating a keycode to a keysym.
;;;;
;;;; Recall that the standard rules for interpreting keycodes involve
;;;; (up to) four different keysyms, indexed from 0 to 3.  Keysyms
;;;; with indices 0 and 1 are said to be in group 1 and keysyms with
;;;; indices 2 and 3 are said to be in group 2.
;;;;
;;;; MODE SWITCH: Whether keysyms in group 1 or keysyms in group 2
;;;; should be used is controlled by a bit position in the modifier
;;;; mask in effect when a key-press event occurs.  It can be one or
;;;; more of the bits corresponding to Mod1 through Mod5, or none of
;;;; those bits.  Whether one of those bits (say M) indicates that
;;;; group 2 should be used is controlled by a two things: an
;;;; assignment of some keycode K to the keysym named :MODE-SWITCH,
;;;; and the attachment of that same keycode K to the modifier M.  We
;;;; can determine whether this is the case by looking at the return
;;;; values of a call to XLIB:MODIFIER-MAPPING corresponding to Mod1
;;;; through Mod5, and then call XLIB:KEYCODE->KEYSYM with each of the
;;;; keycodes in those return values to check whether it is assigned
;;;; to the keysym named :MODE-SWITCH.  This information needs to be
;;;; determined only at start-up and when the keyboard mapping
;;;; changes.  We summarize this information as a mask that has a 1 in
;;;; the position corresponding to M if and only if M should be
;;;; interpreted as a mode switch modifier.
;;;;
;;;; Which keysym to use within a group is controlled by rules
;;;; described below.  To understand those rules, we need to
;;;; understand how to determine from the modifier mask whether
;;;; any of caps-lock, shift-lock, or num-lock is in effect.
;;;;
;;;; NUM-LOCK: Whether num-lock is in effect is controlled by a bit
;;;; position in the modifier mask in effect when a key-press event
;;;; occurs in a way similar to the way mode switch is determined.  It
;;;; can be one or more of the bits corresponding to Mod1 through
;;;; Mod5, or none of those bits.  Whether one of those bits (say M)
;;;; indicates that group 2 should be used is controlled by a two
;;;; things: an assignment of some keycode K to the keysym named
;;;; :NUM-LOCK, and the attachment of that same keycode K to the
;;;; modifier M.  Again, We can determine whether this is the case by
;;;; looking at the return values of a call to XLIB:MODIFIER-MAPPING
;;;; corresponding to Mod1 through Mod5, and then call
;;;; XLIB:KEYCODE->KEYSYM with each of the keycodes in those return
;;;; values to check whether it is assigned to the keysym named
;;;; :NUM-LOCK.  This information needs to be determined only at
;;;; start-up and when the keyboard mapping changes.  We summarize
;;;; this information as a mask that has a 1 in the position
;;;; corresponding to M if and only if M should be interpreted as a
;;;; num-lock modifier.

;;;; There are two steps involved in this process.
;;;;
;;;; The first step is executed when a CLX port is created, and when
;;;; the keyboard mapping of a port is altered.  In this step, we
;;;; create a KEYSYM-INTERPRETATION instance that determines how a
;;;; modifier mask should be interpreted.  The interpretation of the
;;;; modifier mask depends on the assignment of keycodes to certain
;;;; keysyms, and of the attachment of those keycodes to modifiers.

(defclass keysym-interpretation ()
  (;; This slot contains a mask to be applied to an X11 modifier mask
   ;; to determine whether the "mode switch" modifier is in effect.
   (%mode-switch-mask :initarg :mode-switch-mask
		      :initform #b00000000
		      :accessor mode-switch-mask)
   (%num-lock-mask :initarg :num-lock-mask
		   :initform #b00000000
		   :accessor num-lock-mask)))

;;; Return true if and only if the mode switch is in effect.  This is
;;; the case when the modifier mask contains a 1 in a position that
;;; has been assigned to the mode switch modifier.
(defun mode-switch-in-effect-p (keysym-interpretation modifier-mask)
  (plusp (logand (mode-switch-mask keysym-interpretation) modifier-mask)))

;;; Return true if and only if numeric lock is in effect.  This is the
;;; case when the modifier mask contains a 1 in a position that has
;;; been assigned to the num-lock modifier.
(defun num-lock-in-effect-p (keysym-interpretation modifier-mask)
  (plusp (logand (num-lock-mask keysym-interpretation) modifier-mask)))
