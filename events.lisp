;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by 
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)

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

;;; ------------------------------------------------------------------------------------------
;;;  Events
;;;

;; The event objects are defined similar to the CLIM event hierarchy.
;;
;; Class hierarchy as in CLIM:
;; 
;;   event
;;     device-event
;;       keyboard-event
;;         key-press-event
;;         key-release-event
;;       pointer-event
;;         pointer-button-event
;;           pointer-button-press-event
;;           pointer-button-release-event
;;           pointer-button-hold-event
;;         pointer-motion-event
;;           pointer-boundary-event
;;             pointer-enter-event
;;             pointer-exit-event
;;     window-event
;;       window-configuration-event
;;       window-repaint-event
;;     window-manager-event
;;       window-manager-delete-event
;;     timer-event
;;


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
      :reader pointer-event-native-x)
   (y :initarg :y
      :reader pointer-event-native-y)
   ))

(defmacro get-pointer-position ((sheet event) &body body)
  `(multiple-value-bind (x y)
       (untransform-position (sheet-native-transformation ,sheet)
			     (pointer-event-native-x ,event)
			     (pointer-event-native-y ,event))
     (declare (ignorable x y))
     ,@body))
  
(defmethod pointer-event-x ((event pointer-event))
  (get-pointer-position ((event-sheet event) event) x))

(defmethod pointer-event-y ((event pointer-event))
  (get-pointer-position ((event-sheet event) event) y))


(defclass pointer-button-event (pointer-event)
  (
   ))


(defclass pointer-button-press-event (pointer-button-event) ())

(defclass pointer-button-release-event (pointer-button-event) ())

(defclass pointer-button-hold-event (pointer-button-event) ())


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

(defclass pointer-boundary-event (pointer-motion-event)
  (
   ))

(defclass pointer-enter-event (pointer-boundary-event)
  (
   ))

(defclass pointer-exit-event (pointer-boundary-event)
  (
   ))


(defclass pointer-ungrab-event (pointer-exit-event)
  ())

(defclass window-event (event)
  ((sheet :initarg :sheet
	  :reader event-sheet)
   (region :initarg :region
	   :reader window-event-native-region)
   ))

(defmethod window-event-region ((event window-event))
  (transform-region (sheet-native-transformation (event-sheet event))
		    (window-event-native-region event)))

(defmethod window-event-mirrored-sheet ((event window-event))
  (sheet-mirror (event-sheet event)))

(defclass window-configuration-event (window-event)
  ((x :initarg :x :reader window-configuration-event-native-x)
   (y :initarg :y :reader window-configuration-event-native-y)
   (width :initarg :width :reader window-configuration-event-width)
   (height :initarg :height :reader window-configuration-event-height)))

(defmacro get-window-position ((sheet event) &body body)
  `(multiple-value-bind (x y)
       (transform-position (sheet-native-transformation ,sheet)
			   (window-configuration-event-native-x ,event)
			   (window-configuration-event-native-y ,event))
     (declare (ignorable x y))
     ,@body))

(defmethod window-configuration-event-x ((event window-configuration-event))
  (get-window-position ((event-sheet event) event) x))

(defmethod window-configuration-event-y ((event window-configuration-event))
  (get-window-position ((event-sheet event) event) y))

(defclass window-unmap-event (window-event)
  ())

(defclass window-destroy-event (window-event)
  ())

(defclass window-repaint-event (window-event)
  (
   ))

(defclass window-manager-event (event) ())

(defclass window-manager-delete-event (window-manager-event) ())

(defclass timer-event (event)
  (
   ))

(defmethod event-instance-slots ((self event))
  '(timestamp))

(defmethod event-instance-slots ((self device-event))
  '(timestamp modifier-state sheet))

(defmethod event-instance-slots ((self keyboard-event))
   '(timestamp modifier-state sheet key-name))

(defmethod event-instance-slots ((self pointer-event))
  '(timestamp modifier-state sheet pointer button x y root-x root-y))

(defmethod event-instance-slots ((self window-event))
  '(timestamp region))

;(defmethod print-object ((self event) sink)
; (print-object-with-slots self (event-instance-slots self) sink))

;(defmethod translate-event ((self pointer-event) dx dy)
;  (apply #'make-instance (class-of self)
;         :x (+ dx (pointer-event-x self))
;         :y (+ dy (pointer-event-y self))
;         (fetch-slots-as-kwlist self (event-instance-slots self))))

;(defmethod translate-event ((self window-event) dx dy)
;  (apply #'make-instance (class-of self)
;         :region (translate-region (window-event-region self) dx dy)
;         (fetch-slots-as-kwlist self (event-instance-slots self))))

;(defmethod translate-event ((self event) dx dy)
;  (declare (ignore dx dy))
;  self)

;;; Constants dealing with events

(defconstant +pointer-left-button+   #x01)
(defconstant +pointer-middle-button+ #x02)
(defconstant +pointer-right-button+  #x04)

(defconstant +shift-key+             #x0100)
(defconstant +control-key+           #x0200)
(defconstant +meta-key+              #x0400)
(defconstant +super-key+             #x0800)
(defconstant +hyper-key+             #x1000)
(defconstant +alt-key+               #x2000)

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

(defmethod event-type ((event device-event)) :device)
(defmethod event-type ((event keyboard-event)) :keyboard)
(defmethod event-type ((event key-press-event)) :key-press)
(defmethod event-type ((event key-release-event)) :key-release)
(defmethod event-type ((event pointer-event)) :pointer)
(defmethod event-type ((event pointer-button-event)) :pointer-button)
(defmethod event-type ((event pointer-button-press-event)) :pointer-button-press)
(defmethod event-type ((event pointer-button-release-event)) :pointer-button-release)
(defmethod event-type ((event pointer-button-hold-event)) :pointer-button-hold)
(defmethod event-type ((event pointer-motion-event)) :pointer-motion)
(defmethod event-type ((event pointer-boundary-event)) :pointer-boundary)
(defmethod event-type ((event pointer-enter-event)) :pointer-enter)
(defmethod event-type ((event pointer-exit-event)) :pointer-exit)
(defmethod event-type ((event window-event)) :window)
(defmethod event-type ((event window-configuration-event)) :window-configuration)
(defmethod event-type ((event window-repaint-event)) :window-repaint)
(defmethod event-type ((event window-manager-event)) :window-manager)
(defmethod event-type ((event window-manager-delete-event)) :window-manager-delete)
(defmethod event-type ((event timer-event)) :timer)

;; keyboard-event-character keyboard-event 
;; pointer-event-native-x pointer-event
;; pointer-event-native-y pointer-event
;; window-event-native-region window-event
;; window-event-mirrored-sheet window-event

;; Key names are a symbol whose value is port-specific. Key names
;; corresponding to the set of standard characters (such as the
;; alphanumerics) will be a symbol in the keyword package.
;; ???!

