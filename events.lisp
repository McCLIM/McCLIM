;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)

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

(defclass event ()
  ((timestamp :initarg :timestamp
	      :reader event-timestamp)
   ))

(defun eventp (x)
  (typep x 'event))

(defmethod event-type ((event event))
  (let* ((type (string (type-of event)))
	 (position (search "-EVENT" type)))
    (if (null position)
	:event
      (intern (subseq type 0 position) :keyword))))

(defclass device-event (event)
  ((sheet :initarg :sheet
	  :reader event-sheet)
   (modifier-state :initarg :modifier-state
		   :reader event-modifier-state)
   ))

(defclass keyboard-event (device-event)
  ((key-name :initarg :key-name
	     :reader keyboard-event-key-name)
   ))

(defmethod keyboard-event-character ((keyboard-event keyboard-event))
  nil)

(defclass key-press-event (keyboard-event)
  (
   ))

(defclass key-release-event (keyboard-event)
  (
   ))

(defclass pointer-event (device-event)
  ((pointer :initarg :pointer
	    :reader pointer-event-pointer)
   (button :initarg :button
	   :reader pointer-event-button)
   (x :initarg :x
      :reader pointer-event-x)
   (y :initarg :y
      :reader pointer-event-y)
   ))

(defclass pointer-button-event (pointer-event)
  (
   ))

(defclass pointer-button-click-event (pointer-button-event)
  (
   ))

(defclass pointer-button-double-click-event (pointer-button-event)
  (
   ))

(defclass pointer-button-click-and-hold-event (pointer-button-event)
  (
   ))

(defclass pointer-motion-event (pointer-event)
  (
   ))

(defclass pointer-enter-event (pointer-motion-event)
  (
   ))

(defclass pointer-exit-event (pointer-motion-event)
  (
   ))

(defclass window-event (device-event)
  ((region :initarg :region
	   :reader window-event-region)
   ))

(defmethod window-event-native-region ((window-event window-event))
  (window-event-region window-event))

(defmethod window-event-mirrored-sheet ((window-event window-event))
  (sheet-mirror (event-sheet window-event)))

(defclass window-configuration-event (window-event)
  (
   ))

(defclass window-repaint-event (window-event)
  (
   ))

(defclass timer-event (event)
  (
   ))

;;; Constants dealing with events

(defconstant +pointer-left-button+ 1)
(defconstant +pointer-middle-button+ 2)
(defconstant +pointer-right-button+ 3)

(defconstant +shift-key+ 1)
(defconstant +control-key+ 2)
(defconstant +meta-key+ 4)
(defconstant +super-key+ 8)
(defconstant +hyper-key+ 16)

(defmacro key-modifier-state-match-p (button modifier-state &body clauses)
  (let ((button-names '((:left . +pointer-left-button+)
			(:middle . +pointer-middle-button+)
			(:right . +pointer-right-button+)))
	(modifier-names '((:shift . +shift-key+)
			  (:control . +control-key+)
			  (:meta . +meta-key+)
			  (:super . +super-key+)
			  (:hyper . +hyper-key+)))
	(b (gensym))
	(m (gensym)))
    (labels ((do-substitutes (c)
	     (cond
	      ((null c)
	       nil)
	      ((consp c)
	       (cons (do-substitutes (car c)) (do-substitutes (cdr c))))
	      ((assoc c button-names)
	       (list 'check-button (cdr (assoc c button-names))))
	      ((assoc c modifier-names)
	       (list 'check-modifier (cdr (assoc c modifier-names))))
	      (t
	       c))))
      `(flet ((check-button (,b) (= ,button ,b))
	      (check-modifier (,m) (not (zerop (logand ,m ,modifier-state)))))
	 (and ,@(do-substitutes clauses))))))
