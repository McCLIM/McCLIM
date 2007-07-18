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

(in-package :clim-internals)

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

(defvar *last-timestamp* 0)
(defvar *last-timestamp-lock* (make-lock))

(defclass standard-event (event)
  ((timestamp :initarg :timestamp
              :initform nil
	      :reader event-timestamp)
   ;; This slot is pretty much required in order to call handle-event. Some
   ;; events have something other than a sheet in this slot, which is gross.
   (sheet :initarg :sheet
	  :reader event-sheet)))

(defmethod initialize-instance :after ((event standard-event) &rest initargs)
  (declare (ignore initargs))
  (let ((timestamp (event-timestamp event)))
    (with-lock-held (*last-timestamp-lock*)
      (if timestamp
          (maxf *last-timestamp* timestamp)
          (setf (slot-value event 'timestamp)
                (incf *last-timestamp*))))))

;; ### method deleted, since it is defined below in a less obfuscated
;;     way.
;; --GB 2002-11-20
;(defmethod event-type ((event event))
;  (let* ((type (string (type-of event)))
;	 (position (search "-EVENT" type)))
;    (if (null position)
;	:event
;      (intern (subseq type 0 position) :keyword))))
;;; Reintroduce something like that definition, with defmethod goodness.
;;; -- moore

(defmacro define-event-class (name supers slots &rest options)
  (let* ((event-tag (string '#:-event))
	 (name-string (string name))
	 (pos (search event-tag name-string :from-end t)))
    (when (or (null pos)
	      (not (eql (+ pos (length event-tag)) (length name-string))))
      (error "~S does not end in ~A and is not a valid event name for ~
  define-event-class."
	     name event-tag))
    (let ((type (intern (subseq name-string 0 pos) :keyword)))
      `(progn
	 (defclass ,name ,supers
	   ,slots
	   ,@options)
	 (defmethod event-type ((event ,name))
	   ',type)))))

(define-event-class device-event (standard-event)
  ((modifier-state :initarg :modifier-state
		   :reader event-modifier-state)
   (x :initarg :x
      :reader device-event-native-x)
   (y :initarg :y
      :reader device-event-native-y)
   (graft-x :initarg :graft-x
            :reader device-event-native-graft-x)
   (graft-y :initarg :graft-y
            :reader device-event-native-graft-y)))

(define-event-class keyboard-event (device-event)
  ((key-name :initarg :key-name
	     :reader keyboard-event-key-name)
   (key-character :initarg :key-character :reader keyboard-event-character
		  :initform nil)))

(define-event-class key-press-event (keyboard-event)
  ())

(define-event-class key-release-event (keyboard-event)
  ())

(define-event-class pointer-event (device-event)
  ((pointer :initarg :pointer
	    :reader pointer-event-pointer)
   (button :initarg :button
	   :reader pointer-event-button)
   (x :reader pointer-event-native-x)
   (y :reader pointer-event-native-y)
   (graft-x :reader pointer-event-native-graft-x)
   (graft-y :reader pointer-event-native-graft-y) ))

(defmacro get-pointer-position ((sheet event) &body body)
   (with-gensyms (event-var sheet-var x-var y-var)
     `(let* ((,sheet-var ,sheet)
	     (,event-var ,event)
	     (,x-var (device-event-native-x ,event-var))
	     (,y-var (device-event-native-y ,event-var)))
	(multiple-value-bind (x y)
	    (if ,sheet-var
		(untransform-position (sheet-native-transformation ,sheet-var)
				      ,x-var
				      ,y-var)
		(values ,x-var ,y-var))
	  (declare (ignorable x y))
	  ,@body))))
  
(defmethod pointer-event-x ((event pointer-event))
  (get-pointer-position ((event-sheet event) event) x))

(defmethod pointer-event-y ((event pointer-event))
  (get-pointer-position ((event-sheet event) event) y))

(defgeneric pointer-event-position* (pointer-event))

(defmethod pointer-event-position* ((event pointer-event))
  (get-pointer-position ((event-sheet event) event)
    (values x y)))

(defgeneric device-event-x (device-event))

(defmethod device-event-x ((event device-event))
  (get-pointer-position ((event-sheet event) event) x))

(defgeneric device-event-y (device-event))

(defmethod device-event-y ((event device-event))
  (get-pointer-position ((event-sheet event) event) y))

(define-event-class pointer-button-event (pointer-event)
  ())


(define-event-class pointer-button-press-event (pointer-button-event) ())

(define-event-class pointer-button-release-event (pointer-button-event) ())

(define-event-class pointer-button-hold-event (pointer-button-event) ())


(define-event-class pointer-button-click-event (pointer-button-event)
  ())

(define-event-class pointer-button-double-click-event (pointer-button-event)
  ())

(define-event-class pointer-button-click-and-hold-event (pointer-button-event)
  ())

(define-event-class pointer-motion-event (pointer-event)
  ())

(defclass motion-hint-mixin ()
  ()
  (:documentation "A mixin class for events that are a motion hint;
    pointer location coordinates need to be fetched explicitly."))

(defclass pointer-motion-hint-event (pointer-motion-event motion-hint-mixin)
  ())

(define-event-class pointer-boundary-event (pointer-motion-event)
  ())

(define-event-class pointer-enter-event (pointer-boundary-event)
  ())

(define-event-class pointer-exit-event (pointer-boundary-event)
  ())


(define-event-class pointer-ungrab-event (pointer-exit-event)
  ())

(define-event-class window-event (standard-event)
  ((region :initarg :region
	   :reader window-event-native-region)))

(defmethod window-event-region ((event window-event))
  (untransform-region (sheet-native-transformation (event-sheet event))
                      (window-event-native-region event)))

(defmethod window-event-mirrored-sheet ((event window-event))
  (sheet-mirror (event-sheet event)))

(define-event-class window-configuration-event (window-event)
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

(defgeneric window-configuration-event-x (window-configuration-event))

(defmethod window-configuration-event-x ((event window-configuration-event))
  (get-window-position ((event-sheet event) event) x))

(defgeneric window-configuration-event-y (window-configuration-event))

(defmethod window-configuration-event-y ((event window-configuration-event))
  (get-window-position ((event-sheet event) event) y))

(define-event-class window-unmap-event (window-event)
  ())

(define-event-class window-destroy-event (window-event)
  ())

(define-event-class window-repaint-event (window-event)
  ())

(define-event-class window-manager-event (standard-event) ())

(define-event-class window-manager-delete-event (window-manager-event)
  ;; sheet (inherited from standard-event) is not required by the spec but we
  ;; need to know which window to delete - mikemac
  ())     

(define-event-class timer-event (standard-event)
  ((token
     :initarg :token
     :reader  event-token)))

;;; Constants dealing with events

(defconstant +pointer-left-button+   #x01)
(defconstant +pointer-middle-button+ #x02)
(defconstant +pointer-right-button+  #x04)
(defconstant +pointer-wheel-up+      #x08)
(defconstant +pointer-wheel-down+    #x10)
(defconstant +pointer-wheel-left+    #x20)
(defconstant +pointer-wheel-right+   #x40)

(defconstant +shift-key+             #x0100)
(defconstant +control-key+           #x0200)
(defconstant +meta-key+              #x0400)
(defconstant +super-key+             #x0800)
(defconstant +hyper-key+             #x1000)
(defconstant +alt-key+               #x2000)

(defmacro key-modifier-state-match-p (button modifier-state &body clauses)
  (let ((button-names '((:left       . +pointer-left-button+)
                        (:middle     . +pointer-middle-button+)
                        (:right      . +pointer-right-button+)
                        (:wheel-up   . +pointer-wheel-up+)
                        (:wheel-down . +pointer-wheel-down+)))
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

;; Key names are a symbol whose value is port-specific. Key names
;; corresponding to the set of standard characters (such as the
;; alphanumerics) will be a symbol in the keyword package.
;; ???!

