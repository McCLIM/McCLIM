;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2000 by Arthur Lemmens (lemmens@simplex.nl)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GADGETS
;;;
;;; This code corresponds to section 30.3 (Basic Gadget Classes) of the 
;;; CLIM 2.0 spec and to 10.5.2 of Lispworks' CLIM 2.0 User's Guide.
;;;
;;; Lispworks' User's Guide differs from the spec on the following points:
;;; - Lispworks' name for STANDARD-GADGET is BASIC-GADGET
;;; - Lispworks' name for ORIENTED-GADGET is ORIENTED-GADGET-MIXIN
;;; - Lispworks' name for LABELLED-GADGET is LABELLED-GADGET-MIXIN
;;; - Lispworks' name for RANGE-GADGET is RANGE-GADGET-MIXIN
;;; - Lispworks specifies the generic functions GADGET-RANGE and GADGET-RANGE*.
;;; - Lispworks specifies the generic function GADGET-ACTIVE-P.
;;; I've tried to make this code compatible with both the spec and Lispworks.
;;; 
;;; I wonder what the differences are with ACL and MCL.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; History:
;;; - 2000-06-18, Arthur Lemmens
;;;     Created this file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Gadget
;;;

(defclass gadget (pane)
  ((id :initarg :id
       :initform (gensym "GADGET")
       :accessor gadget-id)
   (client :initarg :client
           :initform nil
           :accessor gadget-client)
   (armed-callback :initarg :armed-callback
                   :initform nil
                   :reader gadget-armed-callback)
   (disarmed-callback :initarg :disarmed-callback
                      :initform nil
                      :reader gadget-disarmed-callback)
   (active-p  ;; [Arthur] I'm not so sure about the value for :initform.
              ;; Maybe T is better? Or maybe we should call
              ;; ACTIVATE-GADGET after creating a gadget?
              :initform nil 
              :reader gadget-active-p)
   (armed :initform nil) ))

(defun gadgetp (object)
  (typep object 'gadget))


(defclass basic-gadget (gadget)
  ;; Half-baked attempt to be compatible with Lispworks.
  ())

(defclass standard-gadget (basic-gadget)
  ())

(defgeneric armed-callback (gadget client gadget-id))
(defgeneric disarmed-callback (gadget client gadget-id))

;; "The default methods (on standard-gadget) call the function stored
;; in gadget-armed-callback or gadget-disarmed-callback with one argument,
;; the gadget."

(defmethod armed-callback ((gadget standard-gadget) client gadget-id)
  (let ((callback (gadget-armed-callback gadget)))
    (when callback
      (funcall callback gadget))))

(defmethod disarmed-callback ((gadget standard-gadget) client gadget-id)
  (let ((callback (gadget-disarmed-callback gadget)))
    (when callback
      (funcall callback gadget))))


;;;
;;; Activation
;;;

(defgeneric activate-gadget (gadget))
(defgeneric deactivate-gadget (gadget))
(defgeneric note-gadget-activated (client gadget))
(defgeneric note-gadget-deactivated (client gadget))

(defmethod activate-gadget ((gadget gadget))
  (with-slots (active-p) gadget
    (unless active-p
      (setf active-p t)
      (note-gadget-activated (gadget-client gadget) gadget))))

(defmethod deactivate-gadget ((gadget gadget))
  (with-slots (active-p) gadget
    (when active-p
      (setf active-p nil)
      (note-gadget-deactivated (gadget-client gadget) gadget))))

(defmethod note-gadget-activated (client (gadget gadget))
  (declare (ignore client gadget))
  ;; Default: do nothing  
  )

(defmethod note-gadget-deactivated (client (gadget gadget))
  (declare (ignore client gadget))
  ;; Default: do nothing
  )

;;;
;;; Value-gadget
;;;

(defclass value-gadget (standard-gadget)
  ((value :initarg :value
          :reader gadget-value)
   (value-changed-callback :initarg :value-changed-callback
                           :initform nil
                           :reader gadget-value-changed-callback)))

(defgeneric (setf gadget-value) (value gadget &key invoke-callback))

(defmethod (setf gadget-value) 
           (value (gadget value-gadget) &key invoke-callback)
  (setf (slot-value gadget 'value)
        value)
  (when invoke-callback
    (value-changed-callback gadget 
                            (gadget-client gadget) 
                            (gadget-id gadget)
                            value)))

(defgeneric value-changed-callback (value-gadget client gadget-id value))

(defmethod value-changed-callback ((gadget value-gadget) client gadget-id value)
  (declare (ignore client gadget-id))
  (let ((callback (gadget-value-changed-callback gadget)))
    (when callback
      (funcall callback gadget value))))

  
;;;
;;; Action-gadget
;;;

(defclass action-gadget (standard-gadget)
  ((activate-callback :initarg :activate-callback
                      :initform nil
                      :reader gadget-activate-callback)))

(defgeneric activate-callback (action-gadget client gadget-id))

(defmethod activate-callback ((gadget action-gadget) client gadget-id)
  (let ((callback (gadget-activate-callback gadget)))
    (when callback
      (funcall callback gadget))))

;;;
;;; Oriented-gadget, labelled-gadget, range-gadget
;;;

(defclass oriented-gadget ()
  ((orientation :initarg :orientation
                :reader gadget-orientation)))

(defclass oriented-gadget-mixin (oriented-gadget)
  ;; Try to be compatible with Lispworks' CLIM.
  ())

(defclass labelled-gadget ()
  ((label :initarg :label
          :accessor gadget-label)
   (align-x :initarg :align-x
            :accessor gadget-label-align-x)
   (align-y :initarg :align-y
            :accessor gadget-label-align-y)
   (label-text-style :initarg :label-text-style
                     :accessor gadget-label-text-style)))

(defclass labelled-gadget-mixin (labelled-gadget)
  ;; Try to be compatible with Lispworks' CLIM.
  ())

;; LATER: Implement the following: "Changing the label of a gadget
;; may result in invoking the layout protocol on the gadget and its
;; ancestor sheets." (And similarly for changing the alignment or
;; the label text style of a gadget.)

(defclass range-gadget ()
  ((min-value :initarg :min-value
              :accessor gadget-min-value)
   (max-value :initarg :max-value
              :accessor gadget-max-value)))

(defclass range-gadget-mixin (range-gadget)
  ;; Try to be compatible with Lispworks' CLIM.
  ())

;; GADGET-RANGE and GADGET-RANGE* are from Lispworks' CLIM 2.0 User Guide. 
;; They're not defined in the spec.

(defgeneric gadget-range (range-gadget)
  (:documentation
   "Returns the difference of the maximum and minimum value of RANGE-GADGET."))

(defmethod gadget-range ((gadget range-gadget))
  (- (gadget-max-value gadget)
     (gadget-min-value gadget)))

(defgeneric gadget-range* (range-gadget)
  (:documentation 
   "Returns the minimum and maximum value of RANGE-GADGET as two values."))

(defmethod gadget-range* ((gadget range-gadget))
  (values (gadget-min-value gadget)
          (gadget-max-value gadget)))

