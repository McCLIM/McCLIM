;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-CLX; -*-
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

(in-package :clim-clx)

(defun modifier-keycode->keysyms (display keycode)
  (let ((first-x-keysym (xlib:keycode->keysym display keycode 0)))
    (when (zerop first-x-keysym)
      (return-from modifier-keycode->keysyms nil))
    (let ((second-x-keysym (xlib:keycode->keysym display keycode 1)))
      (cons (clim-xcommon:lookup-keysym first-x-keysym)
	    (if (eql first-x-keysym second-x-keysym)
		nil
		(list (clim-xcommon:lookup-keysym second-x-keysym)))))))

;;; The X state is the state before the current event, so key events
;;; for the modifier keys don't reflect the state that results from
;;; pressing or releasing those keys.  We want the CLIM modifiers to
;;; reflect the post event state.

(defun x-event-to-key-name-and-modifiers (port event-key keycode state)
  (multiple-value-bind (clim-modifiers shift-lock? caps-lock? mode-switch?)
      (clim-xcommon:x-event-state-modifiers port state)
    ;; We filter away the shift state if there is a difference between
    ;; the shifted and unshifted keysym. This is so eg. #\A will not
    ;; look like "#\A with a Shift modifier", as this makes gesture
    ;; processing more difficult.
    (let* ((display (clx-port-display port))
	   (shift? (logtest +shift-key+ clim-modifiers))
           (shift-modifier? (if shift-lock?
                                (not shift?)
                                (if caps-lock? t shift?)))
	   (shifted-keysym (xlib:keycode->keysym display keycode 
                                                 (+ 1 (if mode-switch?
                                                          2 0))))
           (unshifted-keysym (xlib:keycode->keysym display keycode 
                                                   (if mode-switch?
                                                       2 0)))
           (keysym (if shift-modifier?
                       shifted-keysym
                       unshifted-keysym)))
      (let* ((keysym-keyword (clim-xcommon:lookup-keysym keysym))
             (char (xlib:keysym->character display keysym
                                           (+ (if shift-modifier?
                                                  1 0)
                                              (if mode-switch?
                                                  2 0))))
             (modifiers (clim-xcommon:x-keysym-to-clim-modifiers
                         port event-key char (clim-xcommon:lookup-keysym keysym)
                         state)))
        (values char
                (if (= shifted-keysym unshifted-keysym)
                    modifiers
                    (logandc2 modifiers +shift-key+))
                keysym-keyword)))))

;;;;

(defun numeric-keysym-to-character (keysym)
  (and (<= 0 keysym 255)
       (code-char keysym)))

(defun keysym-to-character (keysym)
  (numeric-keysym-to-character (clim-xcommon:reverse-lookup-keysym keysym)))

