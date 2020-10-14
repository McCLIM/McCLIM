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
;;           pointer-scroll-event
;;         pointer-motion-event
;;           pointer-boundary-event
;;             pointer-enter-event
;;               pointer-grab-enter-event
;;               pointer-ungrab-enter-event
;;             pointer-exit-event
;;               pointer-grab-leave-event
;;               pointer-ungrab-leave-event
;;     window-event
;;       window-configuration-event
;;       window-repaint-event
;;     window-manager-event
;;       window-manager-delete-event
;;       window-manager-focus-event
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
          :reader event-sheet))
  (:default-initargs :sheet (alexandria:required-argument :sheet)))

(defmethod initialize-instance :after ((event standard-event) &rest initargs)
  (declare (ignore initargs))
  (let ((timestamp (event-timestamp event)))
    (with-lock-held (*last-timestamp-lock*)
      (if timestamp
          (maxf *last-timestamp* timestamp)
          (setf (slot-value event 'timestamp)
                (incf *last-timestamp*))))))

;;; This macro automates the definition of a method on the EVENT-TYPE
;;; generic function.  Methods on that function should return a
;;; keyword with the same name as the event class name, except with
;;; the "-event" suffix stripped off.
(defmacro define-event-class (name superclasses slots &rest options)
  (let* ((event-tag (string '#:-event))
         (name-string (string name))
         (pos (search event-tag name-string :from-end t)))
    ;; Check that the name of the class ends with "-EVENT".
    (when (or (null pos)
              (not (eql (+ pos (length event-tag)) (length name-string))))
      (error "~S does not end in ~A and is not a valid event name for ~
  define-event-class."
             name event-tag))
    (let ((type (intern (subseq name-string 0 pos) :keyword)))
      `(progn
         (defclass ,name ,superclasses
           ,slots
           ,@options)
         (defmethod event-type ((event ,name))
           ',type)))))

;;; We have three pairs of the pointer event coordinates in different
;;; coordinate systems:
;;;
;;; * (X Y) - native coordinates (the mirror)
;;; * (SHEET-X SHEET-Y) - sheet coordinates
;;; * (GRAFT-X GRAFT-Y) - graft coordinates (the screen)
(define-event-class device-event (standard-event)
  ((modifier-state :initarg :modifier-state :reader event-modifier-state)
   (x :initarg :x :reader device-event-native-x)
   (y :initarg :y :reader device-event-native-y)
   (sheet-x :reader device-event-x)
   (sheet-y :reader device-event-y)
   (graft-x :initarg :graft-x :reader device-event-native-graft-x)
   (graft-y :initarg :graft-y :reader device-event-native-graft-y)))

;;; This macro is responsible for translating the pointer coordinates into
;;; sheet coordinates. That make sense i.e when we try to synthesize event for
;;; a different sheet (i.e when it doesn't have a mirror), or to "detect" the
;;; innermost sheet under the pointer (ditto).
(defmacro get-pointer-position ((sheet event) &body body)
  (alexandria:once-only (sheet event)
    `(multiple-value-bind (x y)
         (if ,sheet
             (untransform-position (sheet-delta-transformation ,sheet (graft ,sheet))
                                   (device-event-native-graft-x ,event)
                                   (device-event-native-graft-y ,event))
             (values (device-event-native-x ,event)
                     (device-event-native-y ,event)))
       (declare (ignorable x y))
       ,@body)))

(defmethod initialize-instance :after ((event device-event) &key x y)
  (if-let ((sheet (event-sheet event)))
    (multiple-value-bind (sheet-x sheet-y)
        (untransform-position (sheet-native-transformation sheet) x y)
      (setf (slot-value event 'sheet-x) sheet-x
            (slot-value event 'sheet-y) sheet-y))
    (setf (slot-value event 'sheet-x) x
          (slot-value event 'sheet-y) y)))

(define-event-class keyboard-event (device-event)
  ((key-name :initarg :key-name
             :reader keyboard-event-key-name)
   (key-character :initarg :key-character :reader keyboard-event-character
                  :initform nil)))

(defmethod print-object ((event keyboard-event) stream)
  (print-unreadable-object (event stream :type t :identity nil)
    (format stream "NAME ~s CHARACTER ~s"
            (keyboard-event-key-name event)
            (keyboard-event-character event))))

(define-event-class key-press-event (keyboard-event)
  ())

(define-event-class key-release-event (keyboard-event)
  ())

(define-event-class pointer-event (device-event)
  ((pointer :initarg :pointer
            :reader pointer-event-pointer)
   (sheet-x :reader pointer-event-x)
   (sheet-y :reader pointer-event-y)
   (x :reader pointer-event-native-x)
   (y :reader pointer-event-native-y)
   (graft-x :reader pointer-event-native-graft-x)
   (graft-y :reader pointer-event-native-graft-y) ))

(defmethod print-object ((event pointer-event) stream)
  (print-unreadable-object (event stream :type t :identity nil)
    (format stream "~S ~S"
            (pointer-event-x event)
            (pointer-event-y event))))

(define-event-class pointer-button-event (pointer-event)
  ((button :initarg :button
           :reader pointer-event-button)))

(define-event-class pointer-button-press-event          (pointer-button-event) ())
(define-event-class pointer-button-release-event        (pointer-button-event) ())
(define-event-class pointer-button-hold-event           (pointer-button-event) ())
(define-event-class pointer-button-click-event          (pointer-button-event) ())
(define-event-class pointer-button-double-click-event   (pointer-button-event) ())
(define-event-class pointer-button-click-and-hold-event (pointer-button-event) ())

(define-event-class pointer-scroll-event (pointer-button-event)
  ((delta-x :initform 0 :initarg :delta-x
            :reader pointer-event-delta-x)
   (delta-y :initform 0 :initarg :delta-y
            :reader pointer-event-delta-y)))

(define-event-class pointer-motion-event   (pointer-button-event)   ())
(define-event-class pointer-boundary-event (pointer-motion-event)   ())
(define-event-class pointer-enter-event    (pointer-boundary-event) ())
(define-event-class pointer-exit-event     (pointer-boundary-event) ())

(define-event-class pointer-grab-enter-event   (pointer-enter-event) ())
(define-event-class pointer-grab-leave-event   (pointer-exit-event)  ())
(define-event-class pointer-ungrab-enter-event (pointer-enter-event) ())
(define-event-class pointer-ungrab-leave-event (pointer-exit-event)  ())

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
   (width :initarg :width :reader window-configuration-event-native-width)
   (height :initarg :height :reader window-configuration-event-native-height)))

(macrolet ((get-window-property (kind which event)
             (multiple-value-bind (transform x y)
                 (ecase kind
                   (:position (values 'untransform-position
                                      'window-configuration-event-native-x
                                      'window-configuration-event-native-y))
                   (:size (values 'untransform-distance
                                  'window-configuration-event-native-width
                                  'window-configuration-event-native-height)))
               `(nth-value
                 ,which (,transform (sheet-native-transformation
                                     (let ((sheet (event-sheet ,event)))
                                       (or (sheet-parent sheet) sheet)))
                                    (,x ,event) (,y ,event))))))

  (defgeneric window-configuration-event-x (event)
    (:method ((event window-configuration-event))
      (get-window-property :position 0 event)))

  (defgeneric window-configuration-event-y (event)
    (:method ((event window-configuration-event))
      (get-window-property :position 1 event)))

  (defgeneric window-configuration-event-width (event)
    (:method ((event window-configuration-event))
      (get-window-property :size 0 event)))

  (defgeneric window-configuration-event-height (event)
    (:method ((event window-configuration-event))
      (get-window-property :size 1 event))))

(define-event-class window-unmap-event   (window-event) ())
(define-event-class window-destroy-event (window-event) ())
(define-event-class window-repaint-event (window-event) ())

(define-event-class window-manager-event        (standard-event)       ())
(define-event-class window-manager-delete-event (window-manager-event) ())
(define-event-class window-manager-focus-event  (window-manager-event) ())

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

;; Key names are a symbol whose value is port-specific. Key names
;; corresponding to the set of standard characters (such as the
;; alphanumerics) will be a symbol in the keyword package.
;; ???!
