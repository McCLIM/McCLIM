;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2000 by 
;;; Arthur Lemmens (lemmens@simplex.nl),
;;; Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;; and Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;  (c) copyright 2001 by
;;; Lionel Salabartan (salabart@emi.u-bordeaux.fr)
;;;  (c) copyright 2001 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2001 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>

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

;;;; Notes

;; There is STANDARD-GADGET in this file but not in the spec, where
;; from? Lispworks?

;; The spec says ORIENTED-GADGET-MIXIN, we call it ORIENTED-GADGET and
;; later define ORIENTED-GADGET-MIXIN with the remark "Try to be
;; compatible with Lispworks' CLIM."
;;
;; This makes me suspect, that either "ORIENTED-GADGET-MIXIN" in the
;; spec is a typo, or all other classes like e.g. ACTION-GADGET should
;; really be named e.g. ACTION-GADGET-MIXIN. Also that would make more
;; sense to me. --GB

;; We have: LABELLED-GADGET, the spec has LABELLED-GADGET-MIXIN. Typo? 
;; Compatibility?

;; Why is there GADGET-LABEL-TEXT-STYLE? The spec says, that just the
;; pane's text-style should be borrowed.

;; RANGE-GADGET / RANGE-GADGET-MIXIN: same thing as with
;; ORIENTED-GADGET-MIXIN.

;; Why is there no (SETF GADGET-RANGE*) in the spec? Omission?

;; I would like to make COMPOSE-LABEL-SPACE and DRAW-LABEL* into some
;; sort of label protocol, so that application programmers can
;; programm their own sort of labels alleviateing the need for
;; something like a drawn button gadget.
;;
;; Q: Can we make it so that a mixin class can override another mixin
;;    class?
;;
;;    All the programmer should need to do is e.g.
;;
;;    (defclass pattern-label-mixin ()
;;      (pattern :initarg :pattern))
;;
;;    (defmethod compose-label-space ((me pattern-label-mixin))
;;      (with-slots (pattern) me
;;        (make-space-requirement :width (pattern-width pattern)
;;                                :height (pattern-height pattern))))
;;
;;    (defmethod draw-label ((me pattern-label-mixin) x1 y1 x2 y2)
;;      (with-slots (pattern) me
;;        (draw-design me (transform-region (make-translation-transformation x1 y1)
;;                                          pattern))))
;;
;;    (defclass patterned-button (pattern-label-mixin push-button-pane)
;;      ())
;;
;; But then this probably is backwards. Specifing that :LABEL can be
;; another pane probably is much easier and would still allow for the
;; backend to choose the concrete widget class for us.
;;
;; --GB

;; - Should RADIO-BOX-PANE and CHECK-BOX-PANE use rack or box layout?

;; - :CHOICES initarg to RADIO-BOX and CHECK-BOX is from Franz' user
;;   guide.

;;;; TODO

;; - the scroll-bar needs more work:
;;    . dragging should not change the value, the value should only
;;      be changed after releasing the mouse.
;;    . it should arm/disarm
;;    . it should be deactivatable

;; - the slider needs a total overhaul

;; - TEXT-FILED, TEXT-AREA dito

;; - GADGET-COLOR-MIXIN is currently kind of dangling, we should reuse
;;   it for effective-gadget-foreground et al.

;; - The color of a 3Dish border should be derived from a gadget's
;;   background.

;; - It seems that 3D-BORDER-MIXIN is only used for the scroll-bar, so
;;   remove it

;; - Somehow engrafting the push button's medium does not work. The
;;   text-style initarg does not make it to the sheets medium.

;; - make NIL a valid label, and take it into account when applying
;;   spacing.

;;;; --------------------------------------------------------------------------
;;;;
;;;;  30.3 Basic Gadget Classes
;;;;

;;; XXX I'm not sure that *application-frame* should be rebound like this. What
;;; about gadgets in accepting-values windows? An accepting-values window
;;; shouldn't be bound to *application-frame*. -- moore
(defun invoke-callback (pane callback &rest more-arguments)
  (when callback
    (let ((*application-frame* (pane-frame pane)))
      (apply callback pane more-arguments))))

;;
;; gadget sub-classes
;;

;;
;; gadget's colors
;;

(defclass gadget-color-mixin ()
  ((normal :type color
	   :initform +gray80+
	   :initarg :normal
	   :accessor gadget-normal-color)
   (highlighted :type color
		:initform +gray85+
		:initarg :highlighted
		:accessor gadget-highlighted-color)
   (pushed-and-highlighted :type color
			   :initform +gray75+
			   :initarg :pushed-and-highlighted
			   :accessor gadget-pushed-and-highlighted-color)
   (current-color :type color
		  :accessor gadget-current-color))
  (:documentation "This class define the gadgets colors."))

(defmethod initialize-instance :after ((gadget gadget-color-mixin) &rest args)
  (declare (ignore args))
  (setf (slot-value gadget 'current-color) (gadget-normal-color gadget)))

(defmethod (setf gadget-current-color) :after (color (gadget gadget-color-mixin))
  (declare (ignore color))
  (dispatch-repaint gadget (sheet-region gadget)))

#||
;; Labelled-gadget

(defgeneric draw-label (gadget label x y))

(defmethod compose-space ((pane labelled-gadget) &key width height)
  (declare (ignore width height))
  (compose-space-aux pane (gadget-label pane)))

(defmethod compose-space-aux ((pane labelled-gadget) (label string))
  (with-sheet-medium (medium pane)
    (let ((as (text-style-ascent (gadget-label-text-style pane) pane))
          (ds (text-style-descent (gadget-label-text-style pane) pane)))
      (multiple-value-bind (width height)
          (text-size medium (gadget-label pane)
                     :text-style (gadget-label-text-style pane))
        (setf height (+ as ds))
        ;; FIXME remove explicit values
        ;; instead use spacer pane in derived classes
        (let ((tw (* 1.3 width))
              (th (* 2.5 height)))
          (setf th (+ 6 height))
          (make-space-requirement :width tw :height th
                                  :max-width 400 :max-height 400
                                  :min-width tw :min-height th))))))

(defmethod draw-label ((pane labelled-gadget) (label string) x y)
  (draw-text* pane label
	      x y
	      :align-x (gadget-label-align-x pane)
	      :align-y (gadget-label-align-y pane)
	      :text-style (gadget-label-text-style pane)))
||#

(defclass basic-gadget (permanent-medium-sheet-output-mixin
                        ;; sheet-leaf-mixin ; <- this cannot go here...
                        gadget-color-mixin
			;; These are inherited from pane, via
			;; clim-sheet-input-mixin and clim-repainting-mixin 
                        ;; immediate-sheet-input-mixin
                        ;; immediate-repainting-mixin
			basic-pane
                        gadget)
  ())
                        


;; Where is this standard-gadget from? --GB
(defclass standard-gadget (basic-gadget)
  ())

(defgeneric armed-callback (gadget client gadget-id)
  (:argument-precedence-order client gadget-id gadget))

(defgeneric disarmed-callback (gadget client gadget-id)
  (:argument-precedence-order client gadget-id gadget))

;; "The default methods (on standard-gadget) call the function stored
;; in gadget-armed-callback or gadget-disarmed-callback with one argument,
;; the gadget."

(defmethod armed-callback ((gadget basic-gadget) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback gadget (gadget-armed-callback gadget)))

(defmethod disarmed-callback ((gadget basic-gadget) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback gadget (gadget-disarmed-callback gadget)))

;;
;; arming and disarming gadgets
;;

;; Redrawing is supposed to be handled on an :AFTER method on arm- and
;; disarm-callback.

(defmethod arm-gadget ((gadget basic-gadget) &optional (value t))
  (with-slots (armed) gadget
    (unless (eql armed value)
      (setf armed value)
      (if value
          (armed-callback gadget (gadget-client gadget) (gadget-id gadget))
          (disarmed-callback gadget (gadget-client gadget) (gadget-id gadget))))))

(defmethod disarm-gadget ((gadget basic-gadget))
  (arm-gadget gadget nil))

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
  (declare (ignore client))
  ;; Default: do nothing  
  )

(defmethod note-gadget-deactivated (client (gadget gadget))
  (declare (ignore client))
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

(defmethod (setf gadget-value) (value (gadget value-gadget) &key invoke-callback)
  (setf (slot-value gadget 'value) value)
  (when invoke-callback
    (value-changed-callback gadget 
                            (gadget-client gadget) 
                            (gadget-id gadget)
                            value)))

(defgeneric value-changed-callback (gadget client gadget-id value)
  (:argument-precedence-order client gadget-id value gadget))

(defmethod value-changed-callback ((gadget value-gadget) client gadget-id value)
  (declare (ignore client gadget-id))
  (invoke-callback gadget (gadget-value-changed-callback gadget) value))

;;;
;;; Action-gadget
;;;

(defclass action-gadget (standard-gadget)
  ((activate-callback :initarg :activate-callback
                      :initform nil
                      :reader gadget-activate-callback)))

(defgeneric activate-callback (action-gadget client gadget-id)
  (:argument-precedence-order client gadget-id action-gadget))

(defmethod activate-callback ((gadget action-gadget) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback gadget (gadget-activate-callback gadget)))

;;;
;;; Oriented-gadget
;;;

(defclass oriented-gadget ()
  ((orientation :type    (member :vertical :horizontal)
		:initarg :orientation
                :reader  gadget-orientation)))

(defclass oriented-gadget-mixin (oriented-gadget)
  ;; Try to be compatible with Lispworks' CLIM.
  ())

;;;;
;;;; labelled-gadget
;;;;

(defclass labelled-gadget ()
  ((label       :initarg :label
                :initform ""
                :accessor gadget-label)
   #+nil
   (align-x     :initarg :align-x
                :accessor gadget-label-align-x)
   #+nil
   (align-y     :initarg :align-y
                :accessor gadget-label-align-y)
   #+nil
   (text-style  :initform *default-text-style*
		:initarg :text-style
                :accessor gadget-text-style)))

(defclass labelled-gadget-mixin (labelled-gadget)
  ;; Try to be compatible with Lispworks' CLIM.
  ())

;;;;
;;;; Range-gadget
;;;;

(defclass range-gadget ()
  ((min-value :initarg :min-value
              :accessor gadget-min-value)
   (max-value :initarg :max-value
              :accessor gadget-max-value)))

(defclass range-gadget-mixin (range-gadget)
  ;; Try to be compatible with Lispworks' CLIM.
  ())

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


;;;; ------------------------------------------------------------------------------------------
;;;;
;;;;  30.4 Abstract Gadget Classes
;;;;

;;; 30.4.1 The abstract push-button Gadget

(defclass push-button (labelled-gadget-mixin action-gadget)
  ())

;;; 30.4.2 The abstract toggle-button Gadget

(defclass toggle-button (labelled-gadget-mixin value-gadget)
  ()
  (:documentation "The value is either t either nil"))

;;; 30.4.3 The abstract menu-button Gadget

(defclass menu-button (labelled-gadget-mixin value-gadget)
  ()
  (:documentation "The value is a button"))

;;; 30.4.4 The abstract scroll-bar Gadget

(defgeneric drag-callback (pane client gadget-id value)
  (:argument-precedence-order client gadget-id value pane))

(defgeneric scroll-to-top-callback (scroll-bar client gadget-id)
  (:argument-precedence-order client gadget-id scroll-bar))

(defgeneric scroll-to-bottom-callback (scroll-bar client gadget-id)
  (:argument-precedence-order client gadget-id scroll-bar))

(defgeneric scroll-up-line-callback (scroll-bar client gadget-id)
  (:argument-precedence-order client gadget-id scroll-bar))

(defgeneric scroll-up-page-callback (scroll-bar client gadget-id)
  (:argument-precedence-order client gadget-id scroll-bar))

(defgeneric scroll-down-line-callback (scroll-bar client gadget-id)
  (:argument-precedence-order client gadget-id scroll-bar))

(defgeneric scroll-down-page-callback (scroll-bar client gadget-id)
  (:argument-precedence-order client gadget-id scroll-bar))

(defclass scroll-bar (value-gadget oriented-gadget-mixin range-gadget-mixin)
  ((drag-callback :initarg :drag-callback
		  :initform nil
		  :reader scroll-bar-drag-callback)
   (scroll-to-bottom-callback :initarg :scroll-to-bottom-callback
			      :initform nil
			      :reader scroll-bar-scroll-to-bottom-callback)
   (scroll-to-top-callback :initarg :scroll-to-top-callback
			   :initform nil
			   :reader scroll-bar-scroll-to-top-callback)
   (scroll-down-line-callback :initarg :scroll-down-line-callback
                              :initform nil
                              :reader scroll-bar-scroll-down-line-callback)
   (scroll-up-line-callback :initarg :scroll-up-line-callback
                            :initform nil
                            :reader scroll-bar-scroll-up-line-callback)
   (scroll-down-page-callback :initarg :scroll-down-page-callback
                              :initform nil
                              :reader scroll-bar-scroll-down-page-callback)
   (scroll-up-page-callback :initarg :scroll-up-page-callback
                            :initform nil
                            :reader scroll-bar-scroll-up-page-callback)
   (thumb-size :initarg :thumb-size :initform 1/4
               :accessor scroll-bar-thumb-size
	       :documentation "The size of the scroll bar thumb (slug) in the
  units of the gadget value. When the scroll bar is drawn the empty region of
  the scroll bar and the thumb are drawn in proportion  to the values of the
  gadget range and thumb size."))
  (:default-initargs :value 0
                     :min-value 0
                     :max-value 1
                     :orientation :vertical))

(defmethod drag-callback ((pane scroll-bar) client gadget-id value)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-drag-callback pane) value))

(defmethod scroll-to-top-callback ((pane scroll-bar) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-to-top-callback pane)))

(defmethod scroll-to-bottom-callback ((pane scroll-bar) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-to-bottom-callback pane)))

(defmethod scroll-up-line-callback ((pane scroll-bar) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-up-line-callback pane)))

(defmethod scroll-up-page-callback ((pane scroll-bar) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-up-page-callback pane)))

(defmethod scroll-down-line-callback ((pane scroll-bar) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-down-line-callback pane)))

(defmethod scroll-down-page-callback ((pane scroll-bar) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callback pane (scroll-bar-scroll-down-page-callback pane)))

;;; 30.4.5 The abstract slider Gadget

(defclass slider-gadget (labelled-gadget-mixin
			 value-gadget
			 oriented-gadget-mixin
			 range-gadget-mixin
			 gadget-color-mixin
                         ;;
                         value-changed-repaint-mixin
                         )
  ()
  (:documentation "The value is a real number, and default value for orientation is :vertical,
and must never be nil."))

;;; 30.4.6 The abstract radio-box and check-box Gadgets

;; The only real different between a RADIO-BOX and a CHECK-BOX is the
;; number of allowed selections.

(defclass radio-box (value-gadget oriented-gadget-mixin) 
  ()
  (:documentation "The value is a button")
  (:default-initargs
    :value nil))

;; RADIO-BOX-CURRENT-SELECTION is just a synonym for GADGET-VALUE:

(defmethod radio-box-current-selection ((radio-box radio-box))
  (gadget-value radio-box))

(defmethod (setf radio-box-current-selection) (new-value (radio-box radio-box))
  (setf (gadget-value radio-box) new-value))

(defmethod radio-box-selections ((pane radio-box))
  (let ((v (radio-box-current-selection pane)))
    (and v (list v))))

(defmethod value-changed-callback :before (value-gadget (client radio-box) gadget-id value)
  (declare (ignorable value-gadget gadget-id value))
  ;; Note that we ignore 'value', this is because if value is non-NIL,
  ;; then the toggle button was turned off, which would make no
  ;; toggle-button turned on => constraint "always exactly one
  ;; selected" missed. So simply turning this toggle button on again
  ;; fixes it.
  (unless (or (and (not value)
                   (not (eq (gadget-value client) value-gadget)))
              (and value
                   (eq (gadget-value client) value-gadget)))
    (setf (gadget-value client :invoke-callback t) value-gadget)))

;;;; CHECK-BOX

(defclass check-box (value-gadget oriented-gadget-mixin) 
  ()
  (:documentation "The value is a list of buttons")
  (:default-initargs
   :value nil
   :orientation :vertical))

;; CHECK-BOX-CURRENT-SELECTION is just a synonym for GADGET-VALUE:

(defmethod check-box-current-selection ((check-box check-box))
  (gadget-value check-box))

(defmethod (setf check-box-current-selection) (new-value (check-box check-box))
  (setf (gadget-value check-box) new-value))

(defmethod value-changed-callback :before (value-gadget (client check-box) gadget-id value)
  (declare (ignorable gadget-id))
  (if value
      (setf (gadget-value client :invoke-callback t)
            (adjoin value-gadget (gadget-value client)))
      (setf (gadget-value client :invoke-callback t)
            (remove value-gadget (gadget-value client)))))

(defmethod (setf gadget-value) :after (buttons (check-box check-box) &key invoke-callback)
  ;; this is silly, but works ...
  (dolist (c (sheet-children check-box))
    (unless (eq (not (null (member c buttons)))
                (not (null (gadget-value c))))
      (setf (gadget-value c :invoke-callback invoke-callback) (member c buttons)) )))

(defmacro with-radio-box ((&rest options
                           &key (type :one-of) (orientation :vertical) &allow-other-keys)
                          &body body)
  (let ((contents (gensym "CONTENTS-"))
        (selected-p (gensym "SELECTED-P-"))
        (initial-selection (gensym "INITIAL-SELECTION-")))
    `(let ((,contents nil)
           (,selected-p nil)
           (,initial-selection nil))
       (declare (special ,selected-p))
       (flet ((make-pane (type &rest options)
                (cond ((eq type 'toggle-button)
                       (let ((pane (apply #'make-pane type 
                                          :value ,selected-p 
                                          :indicator-type ',type
                                          options)))
                         (push pane ,contents)
                         (when ,selected-p
                           (push pane ,initial-selection))))
                      (t
                       (error "oops")))))
         (macrolet ((radio-box-current-selection (subform)
                      `(let ((,',selected-p t))
                         (declare (special ,',selected-p))
                         ,(cond ((stringp subform)
                                 `(make-pane 'toggle-button :label ,subform))
                                (t
                                 subform)))))
           ,@(mapcar (lambda (form)
                       (cond ((stringp form)
                              `(make-pane 'toggle-button :label ,form))
                             (t
                              form)))
                     body)))
       (make-pane ',(if (eq type :one-of) 
                            'radio-box
                            'check-box)
                  :orientation ',orientation
                  :current-selection ,(if (eq type :one-of)
                                          `(or (first ,initial-selection)
                                               (first ,contents))
                                        `,initial-selection)
                  :choices (reverse ,contents)
                  ,@options))))

;;; 30.4.7 The abstract list-pane and option-pane Gadgets

(defclass list-pane (value-gadget)
  ()
  (:documentation 
   "The instantiable class that implements an abstract list pane, that is, a gadget
    whose semantics are similar to a radio box or check box, but whose visual
    appearance is a list of buttons.")
  (:default-initargs :value nil))

(defclass option-pane (value-gadget)
  ()
  (:documentation
   "The instantiable class that implements an abstract option pane, that is, a
    gadget whose semantics are identical to a list pane, but whose visual
    appearance is a single push button which, when pressed, pops up a menu of
    selections."))

;;; 30.4.8 The abstract text-field Gadget

(defclass text-field (value-gadget action-gadget)
  ((editable-p :accessor editable-p :initarg editable-p :initform t))
  (:documentation "The value is a string")
  (:default-initargs :value ""))

(defmethod initialize-instance :after ((gadget text-field) &rest rest)
  (unless (getf rest :normal)
    (setf (slot-value gadget 'current-color) +white+
          (slot-value gadget 'normal) +white+)))

;;; 30.4.9 The abstract text-editor Gadget

(defclass text-editor (text-field)
  ()
  (:documentation "The value is a string"))

;;;; ------------------------------------------------------------------------------------------
;;;;
;;;;  Mixin Classes for Concrete Gadgets
;;;;

(defclass standard-gadget-pane (;;permanent-medium-sheet-output-mixin
                                ;;immediate-sheet-input-mixin
                                ;;immediate-repainting-mixin
                                sheet-leaf-mixin
                                standard-gadget)
  ()
  (:documentation
   "PANE class to include in gadget pane classes."))

;;;; Redrawing mixins

(defclass arm/disarm-repaint-mixin ()
  ()
  (:documentation
   "Mixin class for gadgets, whose appearence depends on its armed state."))

(defmethod armed-callback :after ((gadget arm/disarm-repaint-mixin) client id)
  (declare (ignore client id))
  (dispatch-repaint gadget (or (pane-viewport-region gadget)
                               (sheet-region gadget))))

(defmethod disarmed-callback :after ((gadget arm/disarm-repaint-mixin) client id)
  (declare (ignore client id))
  (dispatch-repaint gadget (or (pane-viewport-region gadget)
                               (sheet-region gadget))))

(defclass value-changed-repaint-mixin ()
  ()
  (:documentation
   "Mixin class for gadgets, whose appearence depends on its value."))

(defmethod (setf gadget-value) :after (new-value (gadget value-changed-repaint-mixin)
                                       &key &allow-other-keys)
  (declare (ignore new-value))
  (dispatch-repaint gadget (or (pane-viewport-region gadget)
                               (sheet-region gadget))))

;;;; Event handling mixins

(defclass enter/exit-arms/disarms-mixin ()
  ()
  (:documentation
   "Mixin class for gadgets which are armed when the mouse enters and 
    disarmed when the mouse leaves."))

(defmethod handle-event :before ((pane enter/exit-arms/disarms-mixin) (event pointer-enter-event))
  (declare (ignorable event))
  (arm-gadget pane))

(defmethod handle-event :after ((pane enter/exit-arms/disarms-mixin) (event pointer-exit-event))
  (declare (ignorable event))
  (disarm-gadget pane))

;;;; changing-label-invokes-layout-protocol-mixin

(defclass changing-label-invokes-layout-protocol-mixin ()
  ()
  (:documentation
   "Mixin class for gadgets, which want invoke the layout protocol, if the label changes."))

;;;; Common behavior on STANDARD-GADGET-PANE and BASIC-GADGET

;;
;; When a gadget is not activated, it receives no device events.
;;
(defmethod handle-event :around ((pane standard-gadget) (event device-event))
  (when (gadget-active-p pane)
    (call-next-method)))

;; When a gadget is deactivated, it cannot be armed.

;; Glitch: upon re-activation the mouse might happen to be in the
;; gadget and thus re-arm it immediately, that is not implemented.

(defmethod note-gadget-deactivated :after (client (gadget standard-gadget))
  (declare (ignorable client))
  (disarm-gadget gadget))

;;;; ------------------------------------------------------------------------------------------
;;;;
;;;;  Drawing Utilities for Concrete Gadgets
;;;;

;;; Labels

(defmethod compose-label-space ((gadget labelled-gadget-mixin) &key (wider 0) (higher 0))
  (with-slots (label align-x align-y) gadget
    (let* ((as (text-style-ascent (pane-text-style gadget) gadget))
           (ds (text-style-descent (pane-text-style gadget) gadget))
           (w  (+ (text-size gadget label :text-style (pane-text-style gadget)) wider))
           (h  (+ as ds higher)))
      (make-space-requirement :width w  :min-width w  :max-width  w
                              :height h :min-height h :max-height h))))

(defmethod draw-label* ((pane labelled-gadget-mixin) x1 y1 x2 y2
                        &key (ink +foreground-ink+))
  (with-slots (align-x align-y label) pane
    (let ((as (text-style-ascent (pane-text-style pane) pane))
          (ds (text-style-descent (pane-text-style pane) pane))
          (w  (text-size pane label :text-style (pane-text-style pane))))
      (draw-text* pane label
                  (case align-x
                    ((:left) x1)
                    ((:right) (- x2 w))
                    ((:center) (/ (+ x1 x2 (- w)) 2))
                    (otherwise x1))     ;defensive programming
                  (case align-y
                    ((:top) (+ y1 as))
                    ((:center) (/ (+ y1 y2 (- as ds)) 2))
                    ((:bottom) (- y2 ds))
                    (otherwise (/ (+ y1 y2 (- as ds)) 2))) ;defensive programming
                  ;; Giving the text-style here shouldn't be neccessary --GB
                  :text-style (pane-text-style pane)
                  :ink ink))))

;;; 3D-ish Look

;; DRAW-BORDERED-POLYGON medium point-seq &key border-width style
;;
;; -GB

(labels ((line-hnf (x1 y1 x2 y2)
           (values (- y2 y1) (- x1 x2) (- (* x1 y2) (* y1 x2))))
         
         (line-line-intersection (x1 y1 x2 y2 x3 y3 x4 y4)
           (multiple-value-bind (a1 b1 c1) (line-hnf x1 y1 x2 y2)
             (multiple-value-bind (a2 b2 c2) (line-hnf x3 y3 x4 y4)
               (let ((d (- (* a1 b2) (* b1 a2))))
                 (cond ((< (abs d) 1e-6)
                        nil)
                       (t
                        (values (/ (- (* b2 c1) (* b1 c2)) d)
                                (/ (- (* a1 c2) (* a2 c1)) d))))))))
         
         (polygon-orientation (point-seq)
           "Determines the polygon's orientation.
            Returns:  +1 = counter-clock-wise 
                      -1 = clock-wise

            The polygon should be clean from duplicate points or co-linear points.
            If the polygon self intersects, the orientation may not be defined, this
            function does not try to detect this situation and happily returns some
            value."
           ;;
           (let ((n (length point-seq)))
             (let* ((min-i 0)
                    (min-val (point-x (elt point-seq min-i))))
               ;;
               (loop for i from 1 below n do
                     (when (< (point-x (elt point-seq i)) min-val)
                       (setf min-val (point-x (elt point-seq i))
                             min-i i)))
               ;;
               (let ((p0 (elt point-seq (mod (+ min-i -1) n)))
                     (p1 (elt point-seq (mod (+ min-i 0) n)))
                     (p2 (elt point-seq (mod (+ min-i +1) n))))
                 (signum (- (* (- (point-x p2) (point-x p0)) (- (point-y p1) (point-y p0)))
                            (* (- (point-x p1) (point-x p0)) (- (point-y p2) (point-y p0)))))))))
         
         (clean-polygon (point-seq)
           "Cleans a polygon from duplicate points and co-linear points. Furthermore
            tries to bring it into counter-clock-wise orientation."
           ;; first step: remove duplicates
           (setf point-seq
                 (let ((n (length point-seq)))
                   (loop for i from 0 below n 
                         for p0 = (elt point-seq (mod (+ i -1) n))
                         for p1 = (elt point-seq (mod (+ i 0) n))
                         unless (and (< (abs (- (point-x p0) (point-x p1))) 10e-8)
                                     (< (abs (- (point-y p0) (point-y p1))) 10e-8))
                         collect p1)))
           ;; second step: remove colinear points
           (setf point-seq
                 (let ((n (length point-seq)))
                   (loop for i from 0 below n
                         for p0 = (elt point-seq (mod (+ i -1) n))
                         for p1 = (elt point-seq (mod (+ i 0) n))
                         for p2 = (elt point-seq (mod (+ i +1) n))
                         unless (< (abs (- (* (- (point-x p1) (point-x p0)) (- (point-y p2) (point-y p0)))
                                           (* (- (point-x p2) (point-x p0)) (- (point-y p1) (point-y p0)))))
                                   10e-8)
                         collect p1)))
           ;; third step: care for the orientation
           (if (and (not (null point-seq))
                    (minusp (polygon-orientation point-seq)))
               (reverse point-seq)
               point-seq) ))
  
  (defun shrink-polygon (point-seq width)
    (let ((point-seq (clean-polygon point-seq)))
      (let ((n (length point-seq)))
        (values
         point-seq
         (loop for i from 0 below n
               for p0 = (elt point-seq (mod (+ i -1) n))
               for p1 = (elt point-seq (mod (+ i  0) n))
               for p2 = (elt point-seq (mod (+ i +1) n))
               collect
               (let* ((dx1 (- (point-x p1) (point-x p0))) (dy1 (- (point-y p1) (point-y p0)))
                      (dx2 (- (point-x p2) (point-x p1))) (dy2 (- (point-y p2) (point-y p1)))
                      ;;
                      (m1  (/ width (sqrt (+ (* dx1 dx1) (* dy1 dy1)))))
                      (m2  (/ width (sqrt (+ (* dx2 dx2) (* dy2 dy2)))))
                      ;;
                      (q0  (make-point (+ (point-x p0) (* m1 dy1)) (- (point-y p0) (* m1 dx1))))
                      (q1  (make-point (+ (point-x p1) (* m1 dy1)) (- (point-y p1) (* m1 dx1))))
                      (q2  (make-point (+ (point-x p1) (* m2 dy2)) (- (point-y p1) (* m2 dx2))))
                      (q3  (make-point (+ (point-x p2) (* m2 dy2)) (- (point-y p2) (* m2 dx2)))) )
                 ;;
                 (multiple-value-bind (x y)
                     (multiple-value-call #'line-line-intersection
                       (point-position q0) (point-position q1)
                       (point-position q2) (point-position q3))
                   (if x
                       (make-point x y)
                       (make-point 0 0)))))))))

  (defun draw-bordered-polygon (medium point-seq
                                       &key (border-width 2)
                                            (style        :inset))
    (labels ((draw-pieces (outer-points inner-points dark light)
               (let ((n (length outer-points)))
                 (dotimes (i n)
                   (let* ((p1 (elt outer-points (mod (+ i  0) n)))
                          (p2 (elt outer-points (mod (+ i +1) n)))
                          (q1 (elt inner-points (mod (+ i  0) n)))
                          (q2 (elt inner-points (mod (+ i +1) n)))
                          (p1* (transform-region +identity-transformation+  p1))
                          (p2* (transform-region +identity-transformation+  p2))
                          (a (mod (atan (- (point-y p2*) (point-y p1*))
                                        (- (point-x p2*) (point-x p1*)))
                                  (* 2 pi))))
                     (draw-polygon medium (list p1 q1 q2 p2)
                                   :ink
                                   (if (<= (* 1/4 pi) a (* 5/4 pi))
                                       dark light)))))))
      (let ((light  *3d-light-color*)
            (dark   *3d-dark-color*))
      ;;
      (ecase style
        (:solid
         (multiple-value-call #'draw-pieces (shrink-polygon point-seq border-width)
                              +black+ +black+))
        (:inset
         (multiple-value-call #'draw-pieces (shrink-polygon point-seq border-width)
                              dark light))
        (:outset
         (multiple-value-call #'draw-pieces (shrink-polygon point-seq border-width)
                              light dark))
        ;;
        ;; Mickey Mouse is the trademark of the Walt Disney Company.
        ;;
        (:mickey-mouse-outset
         (multiple-value-bind (outer-points inner-points) (shrink-polygon point-seq border-width)
           (declare (ignore outer-points))
           (multiple-value-bind (outer-points middle-points) (shrink-polygon point-seq (/ border-width 2))
             (draw-pieces outer-points middle-points +white+ +black+)
             (draw-pieces middle-points inner-points light dark))))
        (:mickey-mouse-inset
         (multiple-value-bind (outer-points inner-points) (shrink-polygon point-seq border-width)
           (declare (ignore outer-points))
           (multiple-value-bind (outer-points middle-points) (shrink-polygon point-seq (/ border-width 2))
             (draw-pieces outer-points middle-points dark light)
             (draw-pieces middle-points inner-points +black+ +white+))))
        ;;
        (:ridge
         (multiple-value-bind (outer-points inner-points) (shrink-polygon point-seq border-width)
           (declare (ignore outer-points))
           (multiple-value-bind (outer-points middle-points) (shrink-polygon point-seq (/ border-width 2))
             (draw-pieces outer-points middle-points light dark)
             (draw-pieces middle-points inner-points dark light))))
        (:groove
         (multiple-value-bind (outer-points inner-points) (shrink-polygon point-seq border-width)
           (declare (ignore outer-points))
           (multiple-value-bind (outer-points middle-points) (shrink-polygon point-seq (/ border-width 2))
             (draw-pieces outer-points middle-points dark light)
             (draw-pieces middle-points inner-points light dark))))
        (:double
         (multiple-value-bind (outer-points inner-points) (shrink-polygon point-seq border-width)
           (declare (ignore outer-points))
           (multiple-value-bind (outer-points imiddle-points) (shrink-polygon point-seq (* 2/3 border-width))
             (declare (ignore outer-points))
             (multiple-value-bind (outer-points omiddle-points) (shrink-polygon point-seq (* 1/3 border-width))
               (draw-pieces outer-points omiddle-points +black+ +black+)
               (draw-pieces imiddle-points inner-points +black+ +black+))))))))) )

(defun draw-bordered-rectangle* (medium x1 y1 x2 y2 &rest options)
  (apply #'draw-bordered-polygon
         medium
         (polygon-points (make-rectangle* x1 y1 x2 y2))
         options))

(defun draw-engraved-label* (pane x1 y1 x2 y2)
  (draw-label* pane (1+ x1) (1+ y1) (1+ x2) (1+ y2) :ink *3d-light-color*)
  (draw-label* pane x1 y1 x2 y2 :ink *3d-dark-color*))  

;;;;
;;;; 3D-BORDER-MIXIN Class
;;;;

;; 3D-BORDER-MIXIN class can be used to add a 3D-ish border to
;; panes. There are three new options:
;;
;;  :border-width       The width of the border
;;  :border-style       The border's style one of :inset, :outset, :groove, :ridge, :solid,
;;                      :double, :dotted, :dashed
;;                      [:dotted and :dashed are not yet implemented]
;;
;;  :border-color       The border's color
;;                      [Not implemented yet]
;;
;; [These options are modelled after CSS].
;;
;; When using 3D-BORDER-MIXIN, one should query the pane's inner
;; region, where drawing should take place, by PANE-INNER-REGION.
;;
;; --GB

(defclass 3D-border-mixin ()
  ((border-width :initarg :border-width :initform 2)
   (border-style :initarg :border-style :initform :outset)
   (border-color :initarg :border-color :initform "???")))

(defmethod pane-inner-region ((pane 3D-border-mixin))
  (with-slots (border-width) pane
    (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
      (make-rectangle* (+ x1 border-width) (+ y1 border-width)
                       (- x2 border-width) (- y2 border-width)))))

(defmethod handle-repaint :after ((pane 3D-border-mixin) region)
  (declare (ignore region))
  (with-slots (border-width border-style) pane
    (draw-bordered-polygon pane (polygon-points (bounding-rectangle (sheet-region pane)))
                           :border-width border-width
                           :style border-style)))

;;;; ------------------------------------------------------------------------------------------
;;;;
;;;;  30.4a Concrete Gadget Classes
;;;;

;; xxx move these!

(defparameter *3d-border-thickness* 2)

;;; Common colors:

(defmethod gadget-highlight-background ((gadget basic-gadget))
  (compose-over (compose-in #|+paleturquoise+|# +white+ (make-opacity .5))
                (pane-background gadget)))

(defmethod effective-gadget-foreground ((gadget basic-gadget))
  (if (gadget-active-p gadget)
      +foreground-ink+
      (compose-over (compose-in (pane-foreground gadget)
                                (make-opacity .5))
                    (pane-background gadget))))

(defmethod effective-gadget-background ((gadget basic-gadget))
  (if (slot-value gadget 'armed)
      (gadget-highlight-background gadget)
      (pane-background gadget)))

(defmethod effective-gadget-input-area-color ((gadget basic-gadget))
  (if (gadget-active-p gadget)
      +lemonchiffon+
      (compose-over (compose-in +lemonchiffon+ (make-opacity .5))
                    (pane-background gadget))))

;;; ------------------------------------------------------------------------------------------
;;; 30.4.1 The concrete push-button Gadget

(defclass push-button-pane  (push-button
                             labelled-gadget-mixin
                             changing-label-invokes-layout-protocol-mixin
                             arm/disarm-repaint-mixin
                             enter/exit-arms/disarms-mixin
                             standard-gadget-pane)
  ((pressedp          :initform nil)
   (show-as-default-p :type boolean
		      :initform nil
		      :initarg :show-as-default-p
		      :accessor push-button-show-as-default-p))
  (:default-initargs
    :text-style (make-text-style :sans-serif nil nil)
    :background *3d-normal-color*
    :align-x :center
    :align-y :center
    :x-spacing 4
    :y-spacing 2))

(defmethod compose-space ((gadget push-button-pane) &key width height)
  (declare (ignore width height))
  (space-requirement+* (space-requirement+* (compose-label-space gadget)
                                            :min-width (* 2 (pane-x-spacing gadget))
                                            :width (* 2 (pane-x-spacing gadget))
                                            :max-width (* 2 (pane-x-spacing gadget))
                                            :min-height (* 2 (pane-y-spacing gadget))
                                            :height (* 2 (pane-y-spacing gadget))
                                            :max-height (* 2 (pane-y-spacing gadget)))
                       :min-width (* 2 *3d-border-thickness*)
                       :width (* 2 *3d-border-thickness*)
                       :max-width (* 2 *3d-border-thickness*)
                       :min-height (* 2 *3d-border-thickness*)
                       :height (* 2 *3d-border-thickness*)
                       :max-height (* 2 *3d-border-thickness*)))

(defmethod handle-event ((pane push-button-pane) (event pointer-button-press-event))
  (with-slots (pressedp) pane
    (setf pressedp t)
    (dispatch-repaint pane +everywhere+)))

(defmethod handle-event ((pane push-button-pane) (event pointer-button-release-event))
  (with-slots (armed pressedp) pane
    (setf pressedp nil)
    (when armed
      (activate-callback pane (gadget-client pane) (gadget-id pane))
      (setf pressedp nil)
      (dispatch-repaint pane +everywhere+))))

(defmethod handle-repaint ((pane push-button-pane) region)
  (declare (ignore region))
  (with-slots (armed pressedp) pane
    (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
      (draw-rectangle* pane x1 y1 x2 y2 :ink (effective-gadget-background pane))
      (draw-bordered-rectangle* pane x1 y1 x2 y2
                                :style (if (and pressedp armed) :inset :outset))
      (multiple-value-bind (x1 y1 x2 y2) (values (+ x1 *3d-border-thickness* (pane-x-spacing pane))
                                                 (+ y1 *3d-border-thickness* (pane-y-spacing pane))
                                                 (- x2 *3d-border-thickness* (pane-x-spacing pane))
                                                 (- y2 *3d-border-thickness* (pane-y-spacing pane)))
        (if (gadget-active-p pane)
            (draw-label* pane x1 y1 x2 y2 :ink (effective-gadget-foreground pane))
            (draw-engraved-label* pane x1 y1 x2 y2))))))

(defmethod deactivate-gadget :after ((gadget push-button-pane))
  (dispatch-repaint gadget +everywhere+))

(defmethod activate-gadget :after ((gadget push-button-pane))
  (dispatch-repaint gadget +everywhere+))


;;; ------------------------------------------------------------------------------------------
;;;  30.4.2 The concrete toggle-button Gadget

(defclass toggle-button-pane (toggle-button 
                              ;; repaint behavior:
                              arm/disarm-repaint-mixin
                              value-changed-repaint-mixin
                              ;; callback behavior:
                              changing-label-invokes-layout-protocol-mixin
                              ;; event handling:
                              enter/exit-arms/disarms-mixin
                              ;; other
                              standard-gadget-pane)
  ((indicator-type :type (member :one-of :some-of)
		   :initarg :indicator-type
		   :reader toggle-button-indicator-type
                   :initform :some-of) )
  (:default-initargs
    :value nil
    :text-style (make-text-style :sans-serif nil nil)    
    :align-x :left
    :align-y :center
    :x-spacing 2
    :y-spacing 2
    :background *3d-normal-color*))

(defmethod compose-space ((pane toggle-button-pane) &key width height)
  (declare (ignore width height))
  (let ((sr (compose-label-space pane)))
    (space-requirement+*
     (space-requirement+* sr
                          :min-width  (* 3 (pane-x-spacing pane))
                          :width      (* 3 (pane-x-spacing pane))
                          :max-width  (* 3 (pane-x-spacing pane))
                          :min-height (* 2 (pane-y-spacing pane))
                          :height     (* 2 (pane-y-spacing pane))
                          :max-height (* 2 (pane-y-spacing pane)))
     :min-width (space-requirement-height sr)
     :width     (space-requirement-height sr)
     :max-width (space-requirement-height sr)
     :min-height 0
     :max-height 0
     :height 0)))

(defmethod draw-toggle-button-indicator ((gadget standard-gadget-pane) (type (eql :one-of)) value x1 y1 x2 y2)
  (multiple-value-bind (cx cy) (values (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))
    (let ((radius (/ (- y2 y1) 2)))
      (draw-circle* gadget cx cy radius
                     :start-angle (* 1/4 pi)
                     :end-angle (* 5/4 pi)
                     :ink *3d-dark-color*)
      (draw-circle* gadget cx cy radius
                     :start-angle (* 5/4 pi)
                     :end-angle (* 9/4 pi)
                     :ink *3d-light-color*)
      (draw-circle* gadget cx cy (max 1 (- radius 2))
                     :ink (effective-gadget-input-area-color gadget))
      (when value
        (draw-circle* gadget cx cy (max 1 (- radius 4))
                      :ink (effective-gadget-foreground gadget))))))

(defmethod draw-toggle-button-indicator ((pane standard-gadget-pane) (type (eql :some-of)) value
                                         x1 y1 x2 y2)
  (draw-rectangle* pane x1 y1 x2 y2 :ink (effective-gadget-input-area-color pane))
  (draw-bordered-rectangle* pane x1 y1 x2 y2 :style :inset)
  (when value
    (multiple-value-bind (x1 y1 x2 y2) (values (+ x1 3) (+ y1 3)
                                               (- x2 3) (- y2 3))
      (draw-line* pane x1 y1 x2 y2 :ink (effective-gadget-foreground pane) :line-thickness 2)
      (draw-line* pane x2 y1 x1 y2 :ink (effective-gadget-foreground pane) :line-thickness 2))))

(defmethod handle-repaint ((pane toggle-button-pane) region)
  (declare (ignore region))
  (when (sheet-grafted-p pane)
    (with-special-choices (pane)
      (with-slots (armed) pane
        (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
          (draw-rectangle* pane x1 y1 x2 y2 :ink (effective-gadget-background pane))
          (let* ((as (text-style-ascent (pane-text-style pane) pane))
                 (ds (text-style-descent (pane-text-style pane) pane)) )
            (multiple-value-bind (tx1 ty1 tx2 ty2)
                (values (+ x1 (pane-x-spacing pane))
                        (- (/ (+ y1 y2) 2) (/ (+ as ds) 2))
                        (+ x1 (pane-x-spacing pane) (+ as ds))
                        (+ (/ (+ y1 y2) 2) (/ (+ as ds) 2)))
              (draw-toggle-button-indicator pane (toggle-button-indicator-type pane) (gadget-value pane)
                                            tx1 ty1 tx2 ty2)
              (draw-label* pane (+ tx2 (pane-x-spacing pane)) y1 x2 y2
                           :ink (effective-gadget-foreground pane)))))))))

(defmethod handle-event ((pane toggle-button-pane) (event pointer-button-release-event))
  (with-slots (armed) pane
    (when armed
      (setf (gadget-value pane :invoke-callback t) (not (gadget-value pane))))))

;;; ------------------------------------------------------------------------------------------
;;;  30.4.3 The concrete menu-button Gadget

(defclass menu-button-pane (menu-button
			    standard-gadget-pane)
  ()
  (:default-initargs
    :text-style (make-text-style :sans-serif nil nil)
    :background *3d-normal-color*
    :x-spacing 3
    :y-spacing 2
    :align-x :left
    :align-y :center))

(defmethod handle-repaint ((pane menu-button-pane) region)
  (declare (ignore region))
  (with-slots (x-spacing y-spacing) pane
    (with-special-choices (pane)
      (let ((region (sheet-region pane)))
        (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
          (draw-rectangle* pane x1 y1 x2 y2
                           :ink (effective-gadget-background pane)
                           :filled t)
          (cond ((slot-value pane 'armed)
                 (draw-bordered-rectangle* pane x1 y1 x2 y2 :style :outset :border-width *3d-border-thickness*))
                (t))
          (multiple-value-bind (x1 y1 x2 y2)
              (values (+ x1 x-spacing) (+ y1 y-spacing)
                      (- x2 x-spacing) (- y2 y-spacing))
            (if (gadget-active-p pane)
                (draw-label* pane x1 y1 x2 y2 :ink (effective-gadget-foreground pane))
                (draw-engraved-label* pane x1 y1 x2 y2))))))))

(defmethod compose-space ((gadget menu-button-pane) &key width height)
  (declare (ignore width height))
  (space-requirement+* (space-requirement+* (compose-label-space gadget)
                                            :min-width (* 2 (pane-x-spacing gadget))
                                            :width (* 2 (pane-x-spacing gadget))
                                            :max-width (* 2 (pane-x-spacing gadget))
                                            :min-height (* 2 (pane-y-spacing gadget))
                                            :height (* 2 (pane-y-spacing gadget))
                                            :max-height (* 2 (pane-y-spacing gadget)))
                       :min-width (* 2 *3d-border-thickness*)
                       :width (* 2 *3d-border-thickness*)
                       :max-width (* 2 *3d-border-thickness*)
                       :min-height (* 2 *3d-border-thickness*)
                       :height (* 2 *3d-border-thickness*)
                       :max-height (* 2 *3d-border-thickness*)))

;;; ------------------------------------------------------------------------------------------
;;;  30.4.4 The concrete scroll-bar Gadget

(defclass scroll-bar-pane (3D-border-mixin
                           scroll-bar)
  ((event-state :initform nil)
   (drag-dy :initform nil)
   ;;; poor man's incremental redisplay
   ;; drawn state
   (up-state :initform nil)
   (dn-state :initform nil)
   (tb-state :initform nil)
   (tb-y1    :initform nil)
   (tb-y2    :initform nil)
   ;; old drawn state
   (old-up-state :initform nil)
   (old-dn-state :initform nil)
   (old-tb-state :initform nil)
   (old-tb-y1    :initform nil)
   (old-tb-y2    :initform nil)
   ;;
   (all-new-p    :initform t) )
  (:default-initargs :border-width 2
                     :border-style :inset
                     :background *3d-inner-color*))

(defmethod compose-space ((sb scroll-bar-pane) &key width height)
  (declare (ignore width height))  
  (if (eq (gadget-orientation sb) :vertical)
      (make-space-requirement :min-width 1
			      :width *scrollbar-thickness*
			      :min-height (* 3 *scrollbar-thickness*)
			      :height (* 4 *scrollbar-thickness*))
      (make-space-requirement :min-height 1
			      :height *scrollbar-thickness*
			      :min-width (* 3 *scrollbar-thickness*)
			      :width (* 4 *scrollbar-thickness*))))

;;;; Redisplay

(defun scroll-bar/update-display (scroll-bar)
  (with-slots (up-state dn-state tb-state tb-y1 tb-y2
               old-up-state old-dn-state old-tb-state old-tb-y1 old-tb-y2
               all-new-p)
      scroll-bar
    ;;
    (scroll-bar/compute-display scroll-bar)
    ;; redraw up arrow
    (unless (and (not all-new-p) (eql up-state old-up-state))
      (with-drawing-options (scroll-bar :transformation (scroll-bar-transformation scroll-bar))
        (with-bounding-rectangle* (x1 y1 x2 y2) (scroll-bar-up-region scroll-bar)
          (draw-rectangle* scroll-bar x1 y1 x2 y2 :ink *3d-inner-color*)
          (let ((pg (list (make-point (/ (+ x1 x2) 2) y1)
                          (make-point x1 y2)
                          (make-point x2 y2))))
            (case up-state
              (:armed
               (draw-polygon scroll-bar pg :ink *3d-inner-color*)
               (draw-bordered-polygon scroll-bar pg :style :inset :border-width 2))
              (otherwise
               (draw-polygon scroll-bar pg :ink *3d-normal-color*)
               (draw-bordered-polygon scroll-bar pg :style :outset :border-width 2) ))))) )
    ;; redraw dn arrow
    (unless (and (not all-new-p) (eql dn-state old-dn-state))
      (with-drawing-options (scroll-bar :transformation (scroll-bar-transformation scroll-bar))
        (with-bounding-rectangle* (x1 y1 x2 y2) (scroll-bar-down-region scroll-bar)
          (draw-rectangle* scroll-bar x1 y1 x2 y2 :ink *3d-inner-color*)
          (let ((pg (list (make-point (/ (+ x1 x2) 2) y2)
                          (make-point x1 y1)
                          (make-point x2 y1))))
            (case dn-state
              (:armed
               (draw-polygon scroll-bar pg :ink *3d-inner-color*)
               (draw-bordered-polygon scroll-bar pg :style :inset :border-width 2))
              (otherwise
               (draw-polygon scroll-bar pg :ink *3d-normal-color*)
               (draw-bordered-polygon scroll-bar pg :style :outset :border-width 2)))))))
    ;; thumb
    (unless (and (not all-new-p)
                 (and (eql tb-state old-tb-state)
                      (eql tb-y1 old-tb-y1)
                      (eql tb-y2 old-tb-y2)))
      (cond ((and (not all-new-p)
                  (eql tb-state old-tb-state)
                  (numberp tb-y1) (numberp old-tb-y1)
                  (numberp tb-y2) (numberp old-tb-y2)
                  (= (- tb-y2 tb-y1) (- old-tb-y2 old-tb-y1)))
             ;; Thumb is just moving, compute old and new region
             (multiple-value-bind (x1 ignore.1 x2 ignore.2)
                 (bounding-rectangle* (scroll-bar-thumb-bed-region scroll-bar))
               (declare (ignore ignore.1 ignore.2))
               ;; compute new and old region
               (with-sheet-medium (medium scroll-bar)
                 (with-drawing-options (medium :transformation (scroll-bar-transformation scroll-bar))
                   (multiple-value-bind (ox1 oy1 ox2 oy2) (values x1 old-tb-y1 x2 old-tb-y2)
                     (multiple-value-bind (nx1 ny1 nx2 ny2) (values x1 tb-y1 x2 tb-y2)
                       (declare (ignore nx2))
                       (copy-area medium ox1 oy1 (- ox2 ox1) (- oy2 oy1) nx1 ny1)
                       ;; clear left-overs from the old region
                       (if (< oy1 ny1)
                           (draw-rectangle* medium ox1 oy1 ox2 ny1 :ink *3d-inner-color*)
                           (draw-rectangle* medium ox1 oy2 ox2 ny2 :ink *3d-inner-color*)))) ))))
             (t
              ;; redraw whole thumb bed and thumb all anew
              (with-drawing-options (scroll-bar :transformation (scroll-bar-transformation scroll-bar))
                  (with-bounding-rectangle* (bx1 by1 bx2 by2) (scroll-bar-thumb-bed-region scroll-bar)
                    (with-bounding-rectangle* (x1 y1 x2 y2) (scroll-bar-thumb-region scroll-bar)
                      (draw-rectangle* scroll-bar bx1 by1 bx2 y1 :ink *3d-inner-color*)
                      (draw-rectangle* scroll-bar bx1 y2 bx2 by2 :ink *3d-inner-color*)
                      (draw-rectangle* scroll-bar x1 y1 x2 y2 :ink *3d-normal-color*)
                      (draw-bordered-polygon scroll-bar
                                             (polygon-points (make-rectangle* x1 y1 x2 y2))
                                             :style :outset
                                             :border-width 2)
                    ;;;;;;
                      (let ((y (/ (+ y1 y2) 2)))
                        (draw-bordered-polygon scroll-bar
                                               (polygon-points (make-rectangle* (+ x1 3) (- y 1) (- x2 3) (+ y 1)))
                                               :style :inset
                                               :border-width 1)
                        (draw-bordered-polygon scroll-bar
                                               (polygon-points (make-rectangle* (+ x1 3) (- y 4) (- x2 3) (- y 2)))
                                               :style :inset
                                               :border-width 1)
                        (draw-bordered-polygon scroll-bar
                                               (polygon-points (make-rectangle* (+ x1 3) (+ y 4) (- x2 3) (+ y 2)))
                                               :style :inset
                                               :border-width 1))))))))
    (setf old-up-state up-state
          old-dn-state dn-state
          old-tb-state tb-state
          old-tb-y1 tb-y1
          old-tb-y2 tb-y2
          all-new-p nil) ))

(defun scroll-bar/compute-display (scroll-bar)
  (with-slots (up-state dn-state tb-state tb-y1 tb-y2
                        event-state) scroll-bar
    (setf up-state (if (eq event-state :up-armed) :armed nil))
    (setf dn-state (if (eq event-state :dn-armed) :armed nil))
    (setf tb-state nil)                 ;we have no armed display yet
    (with-bounding-rectangle* (x1 y1 x2 y2) (scroll-bar-thumb-region scroll-bar)
      (declare (ignore x1 x2))
      (setf tb-y1 y1
            tb-y2 y2))))

;;;; Utilities

;; We think all scroll bars as vertically oriented, therefore we have
;; SCROLL-BAR-TRANSFORMATION, which should make every scroll bar
;; look like being vertically oriented -- simplifies much code.

(defmethod scroll-bar-transformation ((sb scroll-bar))
  (ecase (gadget-orientation sb)
    (:vertical   +identity-transformation+)
    (:horizontal (make-transformation 0 1 1 0 0 0))))

(defun translate-range-value (a mina maxa mino maxo 
                              &optional (empty-result (/ (+ mino maxo) 2)))
  "When \arg{a} is some value in the range from \arg{mina} to \arg{maxa},
   proportionally translate the value into the range \arg{mino} to \arg{maxo}."
  (if (zerop (- maxa mina))
      empty-result
      (+ mino (* (/ (- a mina)
                    (- maxa mina))
                 (- maxo mino)))))

;;;; SETF :after methods

(defmethod (setf gadget-min-value) :after (new-value (pane scroll-bar-pane))
  (declare (ignore new-value))
  (scroll-bar/update-display pane))

(defmethod (setf gadget-max-value) :after (new-value (pane scroll-bar-pane))
  (declare (ignore new-value))
  (scroll-bar/update-display pane))

(defmethod (setf scroll-bar-thumb-size) :after (new-value (pane scroll-bar-pane))
  (declare (ignore new-value))
  (scroll-bar/update-display pane))

(defmethod (setf gadget-value) :after (new-value (pane scroll-bar-pane) &key invoke-callback)
  (declare (ignore new-value invoke-callback))
  (scroll-bar/update-display pane))

(defmethod* (setf scroll-bar-values)
    (min-value max-value thumb-size value (scroll-bar scroll-bar-pane))
  (setf (slot-value scroll-bar 'min-value) min-value
	(slot-value scroll-bar 'max-value) max-value
	(slot-value scroll-bar 'thumb-size) thumb-size
	(slot-value scroll-bar 'value) value)
  (scroll-bar/update-display scroll-bar))

;;;; geometry

(defparameter +minimum-thumb-size-in-pixels+ 30)

(defmethod scroll-bar-up-region ((sb scroll-bar-pane))
  (with-bounding-rectangle* (minx miny maxx maxy) (transform-region (scroll-bar-transformation sb)
                                                                    (pane-inner-region sb))
    (declare (ignore maxy))
    (make-rectangle* minx miny
                     maxx (+ miny (- maxx minx)))))

(defmethod scroll-bar-down-region ((sb scroll-bar-pane))
  (with-bounding-rectangle* (minx miny maxx maxy) (transform-region (scroll-bar-transformation sb)
                                                                    (pane-inner-region sb))
    (declare (ignore miny))
    (make-rectangle* minx (- maxy (- maxx minx))
                     maxx maxy)))

(defun scroll-bar/thumb-bed* (sb)
  ;; -> y1 y2 y3
  (with-bounding-rectangle* (minx miny maxx maxy) (transform-region (scroll-bar-transformation sb)
                                                                    (pane-inner-region sb))
    (let ((y1 (+ miny (- maxx minx) 1))
          (y3 (- maxy (- maxx minx) 1)))
      (let ((ts (scroll-bar-thumb-size sb)))
        ;; This is the right spot to handle ts = :none or perhaps NIL
        (multiple-value-bind (range) (gadget-range sb)
          (let ((ts-in-pixels (round (* (- y3 y1) (/ ts (max 1 (+ range ts))))))) ; handle range + ts = 0
            (setf ts-in-pixels (min (- y3 y1) ;thumb can't be larger than the thumb bed
                                    (max +minimum-thumb-size-in-pixels+ ;but shouldn't be smaller than this.
                                         ts-in-pixels)))
            (values
             y1
             (- y3 ts-in-pixels)
             y3)))))))

(defmethod scroll-bar-thumb-bed-region ((sb scroll-bar-pane))
  (with-bounding-rectangle* (minx miny maxx maxy) (transform-region (scroll-bar-transformation sb)
                                                                    (pane-inner-region sb))
    (declare (ignore miny maxy))
    (multiple-value-bind (y1 y2 y3) (scroll-bar/thumb-bed* sb)
      (declare (ignore y2))
      (make-rectangle* minx y1
                       maxx y3))))

(defun scroll-bar/map-coordinate-to-value (sb y)
  (multiple-value-bind (y1 y2 y3) (scroll-bar/thumb-bed* sb)
    (declare (ignore y3))
    (multiple-value-bind (minv maxv) (gadget-range* sb)
      (translate-range-value y y1 y2 minv maxv minv))))

(defun scroll-bar/map-value-to-coordinate (sb v)
  (multiple-value-bind (y1 y2 y3) (scroll-bar/thumb-bed* sb)
    (declare (ignore y3))
    (multiple-value-bind (minv maxv) (gadget-range* sb)
      (round (translate-range-value v minv maxv y1 y2 y1)))))

(defmethod scroll-bar-thumb-region ((sb scroll-bar-pane))
  (with-bounding-rectangle* (x1 y1 x2 y2) (scroll-bar-thumb-bed-region sb)
    (declare (ignore y1 y2))
    (multiple-value-bind (y1 y2 y3) (scroll-bar/thumb-bed* sb)
      (declare (ignore y1))
      (let ((y4 (scroll-bar/map-value-to-coordinate sb (gadget-value sb))))
        (make-rectangle* x1 y4 x2 (+ y4 (- y3 y2)))))))

;;;; event handler

(defmethod handle-event ((sb scroll-bar-pane) (event pointer-button-press-event))
  (multiple-value-bind (x y) (transform-position (scroll-bar-transformation sb)
                                                 (pointer-event-x event) (pointer-event-y event))
    (with-slots (event-state drag-dy) sb
      (cond ((region-contains-position-p (scroll-bar-up-region sb) x y)
             (scroll-up-line-callback sb (gadget-client sb) (gadget-id sb))
             (setf event-state :up-armed)
             (scroll-bar/update-display sb))
            ((region-contains-position-p (scroll-bar-down-region sb) x y)
             (scroll-down-line-callback sb (gadget-client sb) (gadget-id sb))
             (setf event-state :dn-armed)
             (scroll-bar/update-display sb))
            ;;
            ((region-contains-position-p (scroll-bar-thumb-region sb) x y)
             (setf event-state :dragging
                   drag-dy (- y (bounding-rectangle-min-y (scroll-bar-thumb-region sb)))))
            ;;
            ((region-contains-position-p (scroll-bar-thumb-bed-region sb) x y)
             (if (< y (bounding-rectangle-min-y (scroll-bar-thumb-region sb)))
                 (scroll-up-page-callback sb (gadget-client sb) (gadget-id sb))
                 (scroll-down-page-callback sb (gadget-client sb) (gadget-id sb))))
            (t
             nil)))))

(defmethod handle-event ((sb scroll-bar-pane) (event pointer-motion-event))
  (multiple-value-bind (x y) (transform-position (scroll-bar-transformation sb)
                                                 (pointer-event-x event) (pointer-event-y event))
    (declare (ignore x))
    (with-slots (event-state drag-dy) sb
      (case event-state
        (:dragging
         (let* ((y-new-thumb-top (- y drag-dy))
                (new-value
                 (min (gadget-max-value sb)
                      (max (gadget-min-value sb)
                           (scroll-bar/map-coordinate-to-value sb y-new-thumb-top)))) )
           ;; ### when dragging value shouldn't be immediately updated
           (setf (gadget-value sb #|:invoke-callback nil|#)
                 new-value)
           (drag-callback sb (gadget-client sb) (gadget-id sb) new-value)) )))))

(defmethod handle-event ((sb scroll-bar-pane) (event pointer-button-release-event))
  (with-slots (event-state) sb
    (case event-state
      (:up-armed (setf event-state nil))
      (:dn-armed (setf event-state nil))
      (otherwise
       (setf event-state nil) )))
  (scroll-bar/update-display sb) )

(defmethod handle-repaint ((pane scroll-bar-pane) region)
  (with-slots (all-new-p) pane
    (setf all-new-p t)
    (scroll-bar/update-display pane)))

;;; ------------------------------------------------------------------------------------------
;;;  30.4.5 The concrete slider Gadget

;; ----------------------------------------------------------
;; What should be done for having a better look for sliders
;;
;; We should find a way to draw the value, when show-value-p
;; is true, in a good position, or to dedicate a particular
;; sheet for this drawing (this sheet would be inside the
;; slider's sheet, probably his child).
;; ----------------------------------------------------------

;; This values should be changeable by user. That's
;; why they are parameters, and not constants.
(defparameter slider-button-long-dim 30)
(defparameter slider-button-short-dim 10)

(defclass slider-pane (slider-gadget basic-pane)
  ((drag-callback  :initform nil
		   :initarg :drag-callback
		   :reader slider-drag-callback)
   (show-value-p   :type boolean
		   :initform nil
		   :initarg :show-value-p
		   :accessor gadget-show-value-p)
   (decimal-places :initform 0
                   :initarg :decimal-places
                   :reader slider-decimal-places)
   (number-of-quanta :initform nil
                     :initarg :number-of-quanta
                     :reader slider-number-of-quanta)))

(defmethod compose-space ((pane slider-pane) &key width height)
  (declare (ignore width height))
  (let ((minor (+ 50 (if (gadget-show-value-p pane) 30 0)))
        (major 128))
  (if (eq (gadget-orientation pane) :vertical)
      (make-space-requirement :min-width  minor :width  minor
                              :min-height major :height major)
    (make-space-requirement :min-width  major :width  major
                            :min-height minor :height minor))))
                            


(defmethod initialize-instance :before ((pane slider-pane) &rest rest)
  (declare (ignore rest))
  (setf (slot-value pane 'orientation) :vertical))

(defmethod drag-callback ((pane slider-pane) client gadget-id value)
  (declare (ignore client gadget-id))
  (when (slider-drag-callback pane)
    (funcall (slider-drag-callback pane) pane value)))

(defmethod handle-event ((pane slider-pane) (event pointer-enter-event))
  (with-slots (armed) pane
    (unless armed
      (setf armed t))
    (armed-callback pane (gadget-client pane) (gadget-id pane))))

(defmethod handle-event ((pane slider-pane) (event pointer-exit-event))
  (with-slots (armed) pane
    (when armed
      (setf armed nil))
    (disarmed-callback pane (gadget-client pane) (gadget-id pane))))

(defmethod handle-event ((pane slider-pane) (event pointer-button-press-event))
  (with-slots (armed) pane
     (when armed
       (setf armed ':button-press))))

(defmethod handle-event ((pane slider-pane) (event pointer-motion-event))
  (with-slots (armed) pane
    (when (eq armed ':button-press)
      (let ((value (convert-position-to-value pane
					      (if (eq (gadget-orientation pane) :vertical)
						  (pointer-event-y event)
						  (pointer-event-x event)))))
	(setf (gadget-value pane :invoke-callback nil) value)
	(drag-callback pane (gadget-client pane) (gadget-id pane) value)
	(dispatch-repaint pane (sheet-region pane))))))

(defmethod handle-event ((pane slider-pane) (event pointer-button-release-event))
  (with-slots (armed) pane
    (when armed
      (setf armed t
	    (gadget-value pane :invoke-callback t)
	    (convert-position-to-value pane
				       (if (eq (gadget-orientation pane) :vertical)
					   (pointer-event-y event)
					   (pointer-event-x event))))
      (dispatch-repaint pane (sheet-region pane)))))


(defmethod convert-position-to-value ((pane slider-pane) dim)
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
    (multiple-value-bind (good-dim1 good-dim2)
	(if (eq (gadget-orientation pane) :vertical)
	    ;; vertical orientation
	    (values (+ y1 (ash slider-button-short-dim -1))
		    (- y2 (ash slider-button-short-dim -1)))
	    ;; horizontal orientation
	    (values (+ x1 (ash slider-button-short-dim -1))
		    (- x2 (ash slider-button-short-dim -1))))
      (let ((displacement
             (/ (- (max good-dim1 (min dim good-dim2)) good-dim1)
                (- good-dim2 good-dim1)))
            (quanta (slider-number-of-quanta pane)))
        (+ (gadget-min-value pane)
           (* (gadget-range pane)
              (if quanta
                  (/ (round (* displacement quanta)) quanta)
                  displacement)))))))

(defun format-value (value decimal-places)
  (if (<= decimal-places 0)
      (format nil "~D" (round value))
      (let ((control-string (format nil "~~,~DF" decimal-places)))
        (format nil control-string value))))

(defmethod handle-repaint ((pane slider-pane) region)
  (declare (ignore region))
  (with-special-choices (pane)
    (let ((position (convert-value-to-position pane))
	  (slider-button-half-short-dim (ash slider-button-short-dim -1))
	  (slider-button-half-long-dim  (ash slider-button-long-dim -1))
          (background-color (pane-background pane))          
          (inner-color (gadget-current-color pane)))      
      (flet ((draw-thingy (x y)
               (draw-circle* pane x y 8.0 :filled t :ink inner-color)
               (draw-circle* pane x y 8.0 :filled nil :ink +black+)
               (draw-circle* pane x y 7.0
                             :filled nil :ink +white+
                             :start-angle (* 0.25 pi)
                             :end-angle   (* 1.25 pi))
               (draw-circle* pane x y 7.0
                             :filled nil :ink +black+
                             :start-angle (* 1.25 pi)
                             :end-angle   (* 2.25 pi))))
        (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
          (display-gadget-background pane background-color 0 0 (- x2 x1) (- y2 y1))
          (case (gadget-orientation pane)
            ((:vertical)
             (let ((middle (round (- x2 x1) 2)))
               (draw-bordered-polygon pane
                                      (polygon-points
                                       (make-rectangle*
                                        (- middle 2) (+ y1 slider-button-half-short-dim)
                                        (+ middle 2) (- y2 slider-button-half-short-dim)))
                                      :style :inset
                                      :border-width 2)
               (draw-thingy middle (- position slider-button-half-short-dim))
               (when (gadget-show-value-p pane)
                 (draw-text* pane (format-value (gadget-value pane)
                                                (slider-decimal-places pane))
                             5 ;(- position slider-button-half-short-dim)
                             (- middle slider-button-half-long-dim)))))
            ((:horizontal)
             (let ((middle (round (- y2 y1) 2)))
               (draw-bordered-polygon pane
                                      (polygon-points
                                       (make-rectangle*
                                        (+ x1 slider-button-half-short-dim) (- middle 2)
                                        (- x2 slider-button-half-short-dim) (+ middle 2)))
                                      :style :inset
                                      :border-width 2)
               (draw-thingy (- position slider-button-half-short-dim) middle)
              (when (gadget-show-value-p pane)
	        (draw-text* pane (format-value (gadget-value pane)
                                               (slider-decimal-places pane))
			         5 ;(- position slider-button-half-short-dim)
			         (- middle slider-button-half-long-dim)))))))))))

#|
(defmethod handle-repaint ((pane slider-pane) region)
  (declare (ignore region))
  (with-special-choices (pane)
    (let ((position (convert-value-to-position pane))
	  (slider-button-half-short-dim (ash slider-button-short-dim -1))
	  (slider-button-half-long-dim (ash slider-button-long-dim -1)))
      (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
	(display-gadget-background pane (gadget-current-color pane) 0 0 (- x2 x1) (- y2 y1))
	(if (eq (gadget-orientation pane) :vertical)
	    ; vertical case
	    (let ((middle (round (- x2 x1) 2)))
	      (draw-line* pane
			  middle (+ y1 slider-button-half-short-dim)
			  middle (- y2 slider-button-half-short-dim)
			  :ink +black+
	      (draw-rectangle* pane
			       (- middle slider-button-half-long-dim) (- position slider-button-half-short-dim)
			       (+ middle slider-button-half-long-dim) (+ position slider-button-half-short-dim)
			       :ink +gray85+ :filled t)
	      (draw-edges-lines* pane
                                 +white+
				 (- middle slider-button-half-long-dim) (- position slider-button-half-short-dim)
                                 +black+
				 (+ middle slider-button-half-long-dim) (+ position slider-button-half-short-dim))
	      (when (gadget-show-value-p pane)
		(draw-text* pane (format-value (gadget-value pane)
                                               (slider-decimal-places pane))
			    5 ;(- middle slider-button-half-short-dim)
			    10))) ;(- position slider-button-half-long-dim)
	    ; horizontal case
	    (let ((middle (round (- y2 y1) 2)))
	      (draw-line* pane
			  (+ x1 slider-button-half-short-dim) middle
			  (- x2 slider-button-half-short-dim) middle
			  :ink +black+)
	      (draw-rectangle* pane
			       (- position slider-button-half-short-dim) (- middle slider-button-half-long-dim)
			       (+ position slider-button-half-short-dim) (+ middle slider-button-half-long-dim)
			       :ink +gray85+ :filled t)
	      (draw-edges-lines* pane
                                 +white+
				 (- position slider-button-half-short-dim) (- middle slider-button-half-long-dim)
                                 +black+
				 (+ position slider-button-half-short-dim) (+ middle slider-button-half-long-dim))
	      (when (gadget-show-value-p pane)
		(draw-text* pane (format-value (gadget-value pane)
                                               (slider-decimal-places pane))
			    5 ;(- position slider-button-half-short-dim)
			    (- middle slider-button-half-long-dim)))))))))
|#


(defmethod convert-value-to-position ((pane slider-pane))
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
    (let ((x1 (+ x1 8.0)) ; replace this with some rectangle-inset transform or something
          (y1 (+ y1 8.0)))
      (multiple-value-bind (good-dim1 good-dim2)
	  (if (eq (gadget-orientation pane) :vertical)
	      ; vertical orientation
	      (values (+ y1 (ash slider-button-short-dim -1))
		      (- y2 (ash slider-button-short-dim -1)))
	      ; horizontal orientation
	      (values (+ x1 (ash slider-button-short-dim -1))
		      (- x2 (ash slider-button-short-dim -1))))
        (+ good-dim1 (* (- good-dim2 good-dim1)
                        (if (zerop (gadget-range pane))
                            0.5
                          (/ (- (gadget-value pane) (gadget-min-value pane))
                             (gadget-range pane)))))))))
           
;;; ------------------------------------------------------------------------------------------
;;;  30.4.6 The concrete radio-box and check-box Gadgets

;; radio-box

(defclass radio-box-pane (radio-box rack-layout-mixin sheet-multiple-child-mixin basic-pane)
  ()
  (:default-initargs
   :background *3d-normal-color*))

(defmethod initialize-instance :after ((pane radio-box-pane)
                                       &key choices current-selection orientation &allow-other-keys)
  (setf (box-layout-orientation pane) orientation)
  (setf (gadget-value pane) current-selection)
  (let ((children
         (mapcar (lambda (c)
                   (let ((c (if (stringp c)
                                (make-pane 'toggle-button-pane :label c :value nil)
                                c)))
                     (setf (gadget-value c) (if (eq c (radio-box-current-selection pane)) t nil))
                     (setf (gadget-client c) pane)
                     c))
                 choices)))
    (mapc (curry #'sheet-adopt-child pane) children)))

(defmethod (setf gadget-value) :after (button (radio-box radio-box-pane) &key invoke-callback)
  ;; this is silly, but works ...
  (dolist (c (sheet-children radio-box))
    (unless (eq (not (null (eq c button)))
                (not (null (gadget-value c))))
      (setf (gadget-value c :invoke-callback invoke-callback) (eq c button)) )))

;; check-box

(defclass check-box-pane (check-box rack-layout-mixin sheet-multiple-child-mixin basic-pane)
  ()
  (:default-initargs
   :text-style (make-text-style :sans-serif nil nil)
   :background *3d-normal-color*))

(defmethod initialize-instance :after ((pane check-box-pane)
                                       &key choices current-selection orientation &allow-other-keys)
  (setf (box-layout-orientation pane) orientation)
  (setf (gadget-value pane) current-selection)
  (let ((children
         (mapcar (lambda (c)
                   (let ((c (if (stringp c)
                                (make-pane 'toggle-button-pane :label c :value nil)
                                c)))
                     (setf (gadget-value c) (if (member c current-selection) t nil))
                     (setf (gadget-client c) pane)
                     c))
                 choices)))
    (mapc (curry #'sheet-adopt-child pane) children) ))

;;; ------------------------------------------------------------------------------------------
;;;  30.4.7 The concrete list-pane and option-pane Gadgets


;;; LIST-PANE

;; Note: According to the LispWorks CLIM User's Guide, they do some peculiar
;; things in their list pane. Instead of :exclusive and :nonexclusive modes,
;; they call them :one-of and :some-of. I've supported these aliases for
;; compatibility. They also state the default mode is :some-of, which
;; contradicts the CLIM 2.0 Spec and doesn't make a lot of sense.
;; McCLIM defaults to :one-of.

;; TODO: Improve performance in order to scale to extremely large lists.
;;        * Computing text-size for a 100k list items is expensive
;;        * Need to share text size and cache of computed name-key/value-key
;;          results with LIST-PANE when instantiated in the popup for
;;          the OPTION-PANE.
;;        * Improve repaint logic when items are selected to reduce flicker.
;;       Currently the list and option panes are usable up to several thousand
;;       items on a reasonably fast P4.

;; TODO: Consider appearance of nonexclusive option-pane when multiple items are
;;       selected.

;; TODO: I think the list/option gadgets currently ignore enabled/disabled status.

;; Notes
;;   A some-of/nonexclusive list pane (or option-pane popup window) supports
;;   the following behaviors:
;;       single-click: toggle selected item
;;        shift-click: select/deselect multiple items. Selection or deselection
;;                     is chosen according to the result of your previous click.
;;  McCLIM adds an initarg :prefer-single-selection. If true, a nonexclusive pane
;;  will deselect other items selected when a new selection is made. Multiple
;;  items can be selected using control-click, or shift-click as before. This
;;  imitates the behvior of certain GUIs and may be useful in applications.

(define-abstract-pane-mapping 'list-pane 'generic-list-pane)

(defclass meta-list-pane ()
  ((mode        :initarg :mode
                :initform :exclusive
                :reader list-pane-mode
                :type (member :one-of :some-of :exclusive :nonexclusive))
   (items       :initarg :items
                :initform nil
                :reader list-pane-items
                :type sequence)
   (name-key    :initarg :name-key
                :initform #'princ-to-string
                :reader list-pane-name-key
                :documentation "A function to be applied to items to gain a printable representation")
   (value-key   :initarg :value-key
                :initform #'identity
                :reader list-pane-value-key
                :documentation "A function to be applied to items to gain its value
                                for the purpose of GADGET-VALUE.")
   (presentation-type-key :initarg :presentation-type-key
			  :initform (constantly nil)
			  :reader list-pane-presentation-type-key
			  :documentation "A function to be applied to items to find the presentation types for their values, or NIL.")
   (test        :initarg :test
                :initform #'eql
                :reader list-pane-test
                :documentation "A function to compare two items for equality.")))

(defclass generic-list-pane (list-pane meta-list-pane
                                       standard-sheet-input-mixin ;; Hmm..
                                       value-changed-repaint-mixin
                                       mouse-wheel-scroll-mixin)
  ((highlight-ink :initform +royalblue4+
                  :initarg :highlight-ink
                  :reader list-pane-highlight-ink)
   (item-strings :initform nil
     :documentation "Vector of item strings.")
   (item-values :initform nil
     :documentation "Vector of item values.")
   (items-width  :initform nil
     :documentation "Width sufficient to contain all items")
   (last-action  :initform nil
     :documentation "Last action performed on items in the pane, either
:select, :deselect, or NIL if none has been performed yet.")
   (last-index   :initform nil
     :documentation "Index of last item clicked, for extending selections.")
   (prefer-single-selection :initform nil :initarg :prefer-single-selection
     :documentation "For nonexclusive menus, emulate the common behavior of 
preferring selection of a single item, but allowing extension of the 
selection via the control modifier.")
   (items-length :initform nil :documentation "Number of items"))
  (:default-initargs :text-style (make-text-style :sans-serif :roman :normal)
                     :background +white+ :foreground +black+))

(defmethod initialize-instance :after ((gadget meta-list-pane) &rest rest)
  (declare (ignorable rest))
  ;; Initialize slot value if not specified
  #+NIL ;; XXX
  (when (slot-boundp gadget 'value)
    (setf (slot-value gadget 'value)
          (if (list-pane-exclusive-p gadget)
              (funcall (list-pane-value-key gadget) (first (list-pane-items gadget)))
              (mapcar #'list-pane-value-key (list (first (list-pane-items gadget)))))))
    
  (when (and (not (list-pane-exclusive-p gadget))
             (not (listp (gadget-value gadget))))
    (error "A :nonexclusive list-pane cannot be initialized with a value which is not a list."))
  (when (not (list-pane-exclusive-p gadget))
    (with-slots (value) gadget
      (setf value (copy-list value))))
  #+IGNORE
  (when (and (list-pane-exclusive-p gadget)
             (> (length (gadget-value gadget)) 1))
    (error "An 'exclusive' list-pane cannot be initialized with more than one item selected.")))

(defmethod value-changed-callback :before
    ((gadget generic-list-pane) client gadget-id value)
  (declare (ignore client gadget-id))
  ;; Maybe act as if a presentation was clicked on, but only if the
  ;; list pane only allows single-selection.
  (when (or (eq (list-pane-mode gadget) :one-of)
            (eq (list-pane-mode gadget) :exclusive))
    (let* ((i (position value (generic-list-pane-item-values gadget)))
           (item (elt (list-pane-items gadget) i))
           (ptype (funcall (list-pane-presentation-type-key gadget) item)))
      (when ptype
        (throw-object-ptype value ptype)))))

(defun list-pane-exclusive-p (pane)
  (or (eql (list-pane-mode pane) :exclusive)
      (eql (list-pane-mode pane) :one-of)))

(defmethod initialize-instance :after ((gadget generic-list-pane) &rest rest)
  (declare (ignorable rest))
  ;; For a nonexclusive list-pane, compute some reasonable default for the last
  ;; selected item to make shift-click do something useful.
  (when (not (list-pane-exclusive-p gadget))
    (with-slots (test last-action last-index) gadget
      (when (not (zerop (length (gadget-value gadget))))
        (setf last-action :select
              last-index
              (reduce #'max
                      (mapcar #'(lambda (item) (position item (generic-list-pane-item-values gadget) :test test))
                              (gadget-value gadget))))))))

(defmethod generic-list-pane-item-strings ((pane generic-list-pane))
  (with-slots (item-strings) pane
    (or item-strings
        (setf item-strings
          (map 'vector (lambda (item)
                         (let ((s (funcall (list-pane-name-key pane) item)))
                           (if (stringp s)
                               s
                             (princ-to-string s)))) ;defensive programming!
                (list-pane-items pane))))))

(defmethod generic-list-pane-item-values ((pane generic-list-pane))
  (with-slots (item-values) pane
    (or item-values
        (setf item-values
          (map 'vector (list-pane-value-key pane) (list-pane-items pane))))))

(defmethod generic-list-pane-items-width ((pane generic-list-pane))
  (with-slots (items-width) pane
    (or items-width
        (setf items-width
              (reduce #'max (map 'vector (lambda (item-string)
                                           (text-size pane item-string))
                                 (generic-list-pane-item-strings pane))
                      :initial-value 0)))))

(defmethod generic-list-pane-items-length ((pane generic-list-pane))
  (with-slots (items-length) pane
    (or items-length
        (setf items-length
              (length (generic-list-pane-item-strings pane))))))

(defmethod generic-list-pane-item-height ((pane generic-list-pane))
  (+ (text-style-ascent  (pane-text-style pane) pane)
     (text-style-descent (pane-text-style pane) pane)))

(defmethod compose-space ((pane generic-list-pane) &key width height)
  (declare (ignore width height))
  (let* ((n (generic-list-pane-items-length pane))
         (w (generic-list-pane-items-width pane))
         (h (* n (generic-list-pane-item-height pane))))
    (make-space-requirement :width w     :height h
                            :min-width w :min-height h
                            :max-width +fill+ :max-height +fill+)))

(defmethod allocate-space ((pane generic-list-pane) w h)
  (resize-sheet pane w h))

(defmethod scroll-quantum ((pane generic-list-pane))
  (generic-list-pane-item-height pane))

(defmethod handle-repaint ((pane generic-list-pane) region)
  (with-bounding-rectangle* (sx0 sy0 sx1 sy1) (sheet-region pane)
    (declare (ignore sx1 sy1))
    (with-bounding-rectangle* (rx0 ry0 rx1 ry1)
      (if (bounding-rectangle-p region)
          region
          (or (pane-viewport-region pane)   ; workaround for +everywhere+
              (sheet-region pane)))
      (let ((item-height (generic-list-pane-item-height pane))
            (highlight-ink (list-pane-highlight-ink pane)))
        (do ((index (floor (- ry0 sy0) item-height) (1+ index)))
            ((or (> (+ sy0 (* item-height index)) ry1)
                 (>= index (generic-list-pane-items-length pane))))
          (let ((y0 (+ sy0 (* index item-height)))
                (y1 (+ sy0 (* (1+ index) item-height))))
            (multiple-value-bind (background foreground)
                (cond ((not (slot-boundp pane 'value))
                       (values (pane-background pane) (pane-foreground pane)))
                      ((if (list-pane-exclusive-p pane)
                        (funcall (list-pane-test pane)
                                 (elt (generic-list-pane-item-values pane) index)
                                 (gadget-value pane))
                        (member (elt (generic-list-pane-item-values pane) index) (gadget-value pane)
                                :test (list-pane-test pane)))
                       (values highlight-ink (pane-background pane)))
                      (t (values (pane-background pane) (pane-foreground pane))))
              (draw-rectangle* pane rx0 y0 rx1 y1 :filled t :ink background)
              (draw-text* pane (elt (generic-list-pane-item-strings pane) index)
                          sx0
                          (+ y0 (text-style-ascent (pane-text-style pane) pane))
                          :ink foreground
                          :text-style (pane-text-style pane)))))))))

(defun generic-list-pane-select-item (pane item-value)
  "Toggle selection  of a single item in the generic-list-pane.
Returns :select or :deselect, depending on what action was performed."
  (if (list-pane-exclusive-p pane)
      (progn
        (setf (gadget-value pane :invoke-callback t) item-value)
        :select)
      (let ((member (member item-value (gadget-value pane) :test (list-pane-test pane))))
        (setf (gadget-value pane :invoke-callback t)
              (cond ((list-pane-exclusive-p pane)
                     (list item-value))
                    (member
                     (remove item-value (gadget-value pane)
                             :test (list-pane-test pane)))
                    ((not member) (cons item-value (gadget-value pane)))))
        (if member :deselect :select))))

(defun generic-list-pane-add-selected-items (pane item-values)
  "Add a set of items to the current selection"
  (when (not (list-pane-exclusive-p pane))
    (setf (gadget-value pane :invoke-callback t)
          (remove-duplicates (append item-values
                                     (gadget-value pane))
                             :test (list-pane-test pane)))))

(defun generic-list-pane-deselect-items (pane item-values)
  "Remove a set of items from the current selection"
  (when (not (list-pane-exclusive-p pane))
    (setf (gadget-value pane :invoke-calback t)
          (labels ((fun (item-values result)
                     (if (null item-values)
                         result
                         (fun (rest item-values)
                              (delete (first item-values) result
                                      :test (list-pane-test pane))))))
            (fun item-values (gadget-value pane))))))

(defun generic-list-pane-item-from-x-y (pane mx my)
  "Given a pointer event, determine what item in the pane it has fallen upon. 
Returns two values, the item itself, and the index within the item list."
  (declare (ignore mx))
  (with-bounding-rectangle* (sx0 sy0 sx1 sy1)  (sheet-region pane)
    (declare (ignorable sx0 sx1 sy1))
      (with-slots (items) pane
        (let* ((item-height (generic-list-pane-item-height pane))
               (number-of-items (generic-list-pane-items-length pane))
               (n (floor (- my sy0) item-height))
               (index (and (>= n 0)
                           (< n number-of-items)
                           n))
               (item-value (and index (elt (generic-list-pane-item-values pane) index))))
          (values item-value index)))))

(defun generic-list-pane-handle-click (pane x y modifier)
  (multiple-value-bind (item-value index)
      (generic-list-pane-item-from-x-y pane x y)
    (if (list-pane-exclusive-p pane)
        ;; Exclusive mode
        (when index
          (setf (slot-value pane 'last-action)
                (generic-list-pane-select-item pane item-value)))
        ;; Nonexclusive mode
        (when index
          (with-slots (last-index last-action items prefer-single-selection) pane
            (cond
              ;; Add single selection
              ((not (zerop (logand modifier +control-key+)))
               (setf last-action (generic-list-pane-select-item pane item-value)))
              ;; Maybe extend selection
              ((not (zerop (logand modifier +shift-key+)))
               (if (and (numberp last-index)
                        (not (null last-action)))
                   ;; Extend last selection
                   (funcall (if (eql last-action :select)
                                #'generic-list-pane-add-selected-items
                                #'generic-list-pane-deselect-items)
                            pane
                            (coerce (subseq (generic-list-pane-item-values pane)
                                            (min last-index index)
                                            (1+ (max last-index index))) 'list))
                   (setf last-action (generic-list-pane-select-item pane item-value))))
              ;; Toggle single item
              (t (if prefer-single-selection
                     (setf (gadget-value pane :invoke-callback t) (list item-value)
                           last-action :select)
                     (setf last-action (generic-list-pane-select-item pane item-value)))))
            (setf last-index index))))))

(defun generic-list-pane-handle-click-from-event (pane event)
  (multiple-value-bind (x y) (values (pointer-event-x event) (pointer-event-y event))
    (generic-list-pane-handle-click pane x y (event-modifier-state event))))

(defclass ad-hoc-presentation (standard-presentation) ())

(defmethod output-record-hit-detection-rectangle*
    ((presentation ad-hoc-presentation))
  (values most-negative-fixnum most-negative-fixnum
	  most-positive-fixnum most-positive-fixnum))

(defun generic-list-pane-handle-right-click (pane event)
  (multiple-value-bind (x y)
      (values (pointer-event-x event) (pointer-event-y event))
    (multiple-value-bind (item-value index)
	(generic-list-pane-item-from-x-y pane x y)
      (let* ((item (elt (list-pane-items pane) index)))
	(meta-list-pane-call-presentation-menu pane item)))))

(defun meta-list-pane-call-presentation-menu (pane item)
  (let ((ptype (funcall (list-pane-presentation-type-key pane) item)))
    (when ptype
      (let ((presentation
	     (make-instance 'ad-hoc-presentation
	       :object (funcall (list-pane-value-key pane) item)
	       :single-box t
	       :type ptype)))
	(call-presentation-menu
	 presentation
	 *input-context*
	 *application-frame*
	 pane
	 42 42
	 :for-menu t
	 :label (format nil "Operation on ~A" ptype))))))

(defmethod handle-event ((pane generic-list-pane) (event pointer-button-press-event))
  (case (pointer-event-button event)
    (#.+pointer-left-button+
      (generic-list-pane-handle-click-from-event pane event)
      (setf (slot-value pane 'armed) nil))      
    (#.+pointer-right-button+
      (generic-list-pane-handle-right-click pane event))
    (t
      (when (next-method-p) (call-next-method)))))

(defmethod handle-event ((pane generic-list-pane) (event pointer-button-release-event))
  (if (eql (pointer-event-button event) +pointer-left-button+)
      (and (slot-value pane 'armed)
           (generic-list-pane-handle-click-from-event pane event))
      (when (next-method-p) (call-next-method))))

(defgeneric (setf list-pane-items)
    (newval pane &key invoke-callback)
  (:documentation
   "Set the current list of items for this list pane.
The current GADGET-VALUE will be adjusted by removing values not
specified by the new items.  VALUE-CHANGED-CALLBACK will be called
if INVOKE-CALLBACK is given."))

(defmethod (setf list-pane-items)
    (newval (pane meta-list-pane) &key invoke-callback)
  (declare (ignore invoke-callback))
  (setf (slot-value pane 'items) newval))

(defmethod (setf list-pane-items)
    :after
    (newval (pane meta-list-pane) &key invoke-callback)
  (when (slot-boundp pane 'value)
    (let ((new-values
	   (coerce (climi::generic-list-pane-item-values pane) 'list))
	  (test (list-pane-test pane)))
      (setf (gadget-value pane :invoke-callback invoke-callback)
	    (if (list-pane-exclusive-p pane)
		(if (find (gadget-value pane) new-values :test test)
		    (gadget-value pane)
		    nil)
		(intersection (gadget-value pane) new-values :test test))))))

(defmethod (setf list-pane-items)
    (newval (pane generic-list-pane) &key invoke-callback)
  (call-next-method)
  (with-slots (items items-length item-strings item-values) pane
    (setf items-length (length newval))
    (setf item-strings nil)
    (setf item-values nil)))

(defmethod (setf list-pane-items) :after
    (newval (pane generic-list-pane) &key invoke-callback)
  (change-space-requirements
   pane
   :height (space-requirement-height (compose-space pane)))
  (handle-repaint pane +everywhere+))

;;; OPTION-PANE

(define-abstract-pane-mapping 'option-pane 'generic-option-pane)

(defclass generic-option-pane (option-pane
                               meta-list-pane
                               value-changed-repaint-mixin
                               3d-border-mixin
                               arm/disarm-repaint-mixin
                               enter/exit-arms/disarms-mixin)
  ((current-label :initform "" :accessor generic-option-pane-label))
  (:default-initargs :text-style (make-text-style :sans-serif :roman :normal)))

(defun option-pane-evil-backward-map (pane value)
  (let ((key-fn (list-pane-value-key pane)))
    (if (eql key-fn #'identity)            ;; SANE CASE
        value        
        (find value (list-pane-items pane) ;; INSANE CASE
              :key key-fn :test (list-pane-test pane)))))

(defun generic-option-pane-compute-label-from-value (pane value)
  (flet ((label (value) (funcall (list-pane-name-key pane) (option-pane-evil-backward-map pane value))))
    (if (list-pane-exclusive-p pane)
        (if (or value
              (member nil (list-pane-items pane) ;; Kludge in case NIL is part of the item set..
                      :key (list-pane-value-key pane)
                      :test (list-pane-test pane)))
            (label value)
            "")
        (cond ((= 0 (length value)) "")
              ((= 1 (length value)) (label (first value)))
              (t "...")))))
  
(defun generic-option-pane-compute-label-from-item (pane item)
  (funcall (list-pane-name-key pane) item))

(defun generic-option-pane-compute-label (pane)
  (generic-option-pane-compute-label-from-value pane (gadget-value pane)))

(defmethod initialize-instance :after ((object generic-option-pane) &rest rest)
  (declare (ignore rest))
  (setf (slot-value object 'current-label)
        (if (slot-boundp object 'value)
            (generic-option-pane-compute-label object)
            "")))

(defmethod (setf gadget-value) :after (new-value (gadget generic-option-pane) &key &allow-other-keys)
  (setf (slot-value gadget 'current-label)
        (generic-option-pane-compute-label-from-value gadget new-value)))

(defmethod generic-option-pane-widget-size (pane)
  ;; We now always make the widget occupying a square.
  (let ((h (bounding-rectangle-height pane)))
    (values h h)))

(defun draw-engraved-vertical-separator (pane x y0 y1 highlight-color shadow-color)
  (draw-line* pane (1+ x) (1+ y0) (1+ x) (1- y1) :ink highlight-color)
  (draw-line* pane x y1 (1+ x) y1 :ink highlight-color)
  (draw-line* pane x (1+ y0) x (1- y1) :ink shadow-color)
  (draw-line* pane x y0 (1+ x) y0 :ink shadow-color))

(defun generic-option-pane-text-size (pane)
  (text-size (sheet-medium pane) (slot-value pane 'current-label)
             :text-style (pane-text-style pane)))

(defun draw-vertical-arrow (sheet x0 y0 direction)
  (assert (or (eq direction :up)
              (eq direction :down)))
  (let* ((dx -4)
         (dy 4)
         (shape
          (if (eq direction :up)  ;; Hack-p?
              (list x0 y0
                    (+ x0 dx) (+ 1 y0 dy)
                    (- x0 dx) (+ 1 y0 dy))
              (list x0 y0
                    (+ 1 x0 dx) (+ y0 (- dy))
                    (- x0 dx)   (+ y0 (- dy))))))
    (draw-polygon* sheet shape :ink +black+)))

(defun generic-option-pane-compute-max-label-width (pane)
  (max
   (reduce #'max
           (mapcar #'(lambda (value)
                       (text-size (sheet-medium pane)
                                  (generic-option-pane-compute-label-from-item pane value)
                                  :text-style (pane-text-style pane)))
                   (list-pane-items pane)))
   (text-size (sheet-medium pane) "..." :text-style (pane-text-style pane))))

(defmethod compose-space ((pane generic-option-pane) &key width height)
  (declare (ignore width height))
  (let* ((horizontal-padding 8)         ;### 2px border + 2px padding each side
         (vertical-padding   8)         ;### this should perhaps be computed from
                                        ;### border-width and spacing.
         (l-width  (generic-option-pane-compute-max-label-width pane))
         (l-height (text-style-height (pane-text-style pane) (sheet-medium pane)))
         (total-width (+ horizontal-padding l-width
                         ;; widget width
                         l-height
                         8))
         (total-height (+ vertical-padding l-height)))
    (make-space-requirement :min-width total-width
                            :width total-width
                            :max-width +fill+
                            :min-height total-height
                            :height total-height
                            :max-height total-height)))

(defmethod generic-option-pane-draw-widget (pane)
  (with-bounding-rectangle* (x0 y0 x1 y1) pane
    (declare (ignore x0))                            
    (multiple-value-bind (widget-width widget-height)
        (generic-option-pane-widget-size pane)
      (let ((center (floor (/ (- y1 y0) 2)))
            (height/2 (/ widget-height 2))
            (highlight-color (compose-over (compose-in +white+ (make-opacity 0.85))
                                        (pane-background pane)))
            (shadow-color (compose-over (compose-in +black+ (make-opacity 0.3))
                                        (pane-background pane))))
        (draw-engraved-vertical-separator pane
                                          (- x1 widget-width -1)
                                          (- center height/2)
                                          (+ center height/2)
                                          highlight-color shadow-color)
        (let* ((x (+ (- x1 widget-width) (/ widget-width 2)))
               (frob-x (+ (floor x) 0)))
          (draw-vertical-arrow pane frob-x (- center 6) :up)
          (draw-vertical-arrow pane frob-x (+ center 6) :down))))))

(defun rewrite-event-for-grab (grabber event)
  (multiple-value-bind (nx ny)
      (multiple-value-call #'untransform-position
        (sheet-delta-transformation grabber nil) ;; assumes this is the graft's coordinate system..
        (values (pointer-event-native-graft-x event)
                (pointer-event-native-graft-y event)))
    (with-slots (sheet x y) event
      (setf sheet grabber
            x nx
            y ny)))
  event)

(defun popup-compute-spaces (pane graft)
  (with-bounding-rectangle* (x0 top x1 bottom) (sheet-region pane)
    (multiple-value-call #'(lambda (x0 top x1 bottom)
                             (declare (ignore x0 x1))
                             (values (max 0 (1- top))
                                     (max 0 (- (graft-height graft) bottom))
                                     top
                                     bottom))
      (transform-position (sheet-delta-transformation pane nil) x0 top)  ;; XXX (see above)
      (transform-position (sheet-delta-transformation pane nil) x1 bottom))))

(defun popup-compute-height (parent-pane child-pane)
  "Decides whether to place the child-pane above or below the parent-pane, and
 how to do so. Returns three values: First T if the pane is too large to fit on
 the screen, otherwise NIL. Second, whether to place the child-pane above or
 below parent-pane. Third, the height which the popup should be constrained to
 if the first value is true."
  (let* ((sr (compose-space child-pane))
         (height (space-requirement-min-height sr)))
    (multiple-value-bind (top-space bottom-space)
        (popup-compute-spaces parent-pane (graft parent-pane))
      (let ((polite-ts (* 0.8 top-space))
            (polite-bs (* 0.8 bottom-space)))
        (cond ((and (<= polite-ts height)
                    (<= polite-bs height))
               (multiple-value-call #'values t
                                    (if (> top-space bottom-space)
                                        (values :above (* 0.7 top-space))
                                        (values :below (* 0.7 bottom-space)))))
              ((> polite-bs height) (values nil :below height))
              (t (values nil :above height)))))))

(defun popup-init (parent manager frame)
  (let ((list-pane (apply #'make-pane-1 manager frame 'generic-list-pane
                                :items (list-pane-items parent)
                                :mode  (list-pane-mode parent)
                                :name-key (list-pane-name-key parent)
                                :value-key (list-pane-value-key parent)
                                :test (list-pane-test parent)
                                (and (slot-boundp parent 'value)
                                     (list :value (gadget-value parent))))))
    (multiple-value-bind (scroll-p position height)
        (popup-compute-height parent list-pane)
      (with-bounding-rectangle* (cx0 cy0 cx1 cy1) parent
        (multiple-value-bind (x0 y0 x1 y1)
            (multiple-value-call #'values
              (transform-position (sheet-delta-transformation parent nil) cx0 cy0)
              (transform-position (sheet-delta-transformation parent nil) cx1 cy1))
          ;; Note: This :suggested-width/height business is really a silly hack
          ;;       which I could have easily worked around without adding kludges
          ;;       to the scroller-pane..
          (let* ((topmost-pane (if scroll-p 
                                  ;list-pane
                                  (scrolling (:scroll-bar :vertical
                                              :suggest-height height   ;; Doesn't appear to be working..
                                              :suggest-width (if scroll-p (+ 30 (bounding-rectangle-width list-pane))))
                                     list-pane)
                                  list-pane))
                 (topmost-pane    (outlining (:thickness 1) topmost-pane))
                 (composed-height (space-requirement-height (compose-space topmost-pane :width (- x1 x0) :height height)))
                 (menu-frame      (make-menu-frame topmost-pane
                                                   :min-width (bounding-rectangle-width parent)
                                                   :left x0
                                                   :top (if (eq position :below)
                                                            y1
                                                            (- y0 composed-height 1)))))
            (values list-pane topmost-pane menu-frame)))))))

(defun popup-list-box (parent)
  (let* ((frame *application-frame*)
         (manager (frame-manager frame))
         ;; Popup state
         (final-change nil) ;; Menu should exit after next value change         
         (inner-grab nil)    ;; Gadget is grabbing the pointer, used to simulate
                             ;; X implicit pointer grabbing (for the scrollbar)
         (retain-value nil)
         (consume-and-exit nil) ;; If true, wait until a button release then exit
         (last-click-time nil)
         (last-item-index nil))
    (with-look-and-feel-realization (manager *application-frame*)
      (multiple-value-bind (list-pane topmost-pane menu-frame)
          (popup-init parent manager frame)
        (setf (slot-value list-pane 'armed) t)
        (adopt-frame manager menu-frame)
        
        (labels ((in-window (window child x y)
                   (and window
                        (sheet-ancestor-p child window)
                        (multiple-value-call #'region-contains-position-p
                          (sheet-region window)
                          (transform-position (sheet-delta-transformation child window) x y))))
                 (in-list (window x y)
                   (in-window list-pane window x y))
                 (in-menu (window x y)
                   (in-window topmost-pane window x y))
                 (compute-double-clicked ()
                   (let* ((now (get-internal-real-time)))
                     (prog1 (and last-click-time
                                 (< (/ (- now last-click-time) internal-time-units-per-second) *double-click-delay*))
                       (setf last-click-time now))))
                 (end-it ()
                   (throw 'popup-list-box-done nil)))
          
          (catch 'popup-list-box-done
            (setf (slot-value list-pane 'value-changed-callback)
                  (lambda (pane value)
                    (declare (ignore pane value))
                    (when (and final-change
                               (not consume-and-exit))
                      (end-it))))

            (tracking-pointer (list-pane :multiple-window t :highlight nil)
              (:pointer-motion (&key event window x y)
                 (cond (inner-grab (handle-event inner-grab (rewrite-event-for-grab inner-grab event)))
                       ((and (list-pane-exclusive-p parent)
                             (in-list window x y))
                        (generic-list-pane-handle-click list-pane x y 0))
                       ((in-menu window x y)
                        (handle-event window event))))
              
              (:pointer-button-press (&key event x y)
                 (if inner-grab
                     (handle-event inner-grab event)
                     (cond ((in-list (event-sheet event) x y)
                            (multiple-value-bind (item current-index)
                                (generic-list-pane-item-from-x-y list-pane x y)
                              (declare (ignore item))
                              (let ((double-clicked (and (compute-double-clicked)
                                                         (= (or last-item-index -1)
                                                            (or current-index -2))))
                                    (exclusive (list-pane-exclusive-p parent)))
                                (setf retain-value t
                                      final-change   (or exclusive double-clicked)
                                      last-item-index current-index
                                      consume-and-exit (or exclusive
                                                           (and (not exclusive)
                                                                double-clicked)))
                                (unless (and (not exclusive)
                                             double-clicked)
                                    (handle-event list-pane event)))))
                           ((in-menu (event-sheet event) x  y)
                            (handle-event (event-sheet event) event)
                            (setf inner-grab (event-sheet event)))
                           (t (setf consume-and-exit t)))))

              (:pointer-button-release (&key event x y)
                (when consume-and-exit (end-it))
                (cond (inner-grab
                       (handle-event inner-grab event)
                       (setf inner-grab nil))
                      ((in-list (event-sheet event) x y)
                       (when (list-pane-exclusive-p parent)
                         (setf retain-value t
                               final-change t)
                         (handle-event list-pane event)))
                      ((in-menu (event-sheet event) x y)
                       (handle-event (event-sheet event) event)))))))
        ;; Cleanup and exit
        (when retain-value
          (setf (gadget-value parent :invoke-callback t)
                (gadget-value list-pane)))
        (disown-frame manager menu-frame)))))

(defmethod handle-event ((pane generic-option-pane) (event pointer-button-press-event))
  (popup-list-box pane)
  (disarm-gadget pane))

(defmethod handle-repaint ((pane generic-option-pane) region)
  (with-bounding-rectangle* (x0 y0 x1 y1) (sheet-region pane)
    (multiple-value-bind (widget-width widget-height)
        (generic-option-pane-widget-size pane)
      (declare (ignore widget-height))
      (draw-rectangle* pane x0 y0 x1 y1 :ink (effective-gadget-background pane))
      (let* ((tx1 (- x1 widget-width)))
        (draw-text* pane (slot-value pane 'current-label)
                    (/ (- tx1 x0) 2)
                    (/ (+ (- y1 y0)
                           (- (text-style-ascent (pane-text-style pane) pane)
                              (text-style-descent (pane-text-style pane) pane)))
                        2)
                    :align-x :center
                    :align-y :baseline))
      (generic-option-pane-draw-widget pane))))
        

;;;; ------------------------------------------------------------------------------------------
;;;;
;;;;  30.5 Integrating Gadgets and Output Records
;;;;

;;
;; GADGET-OUTPUT-RECORD
;;

(defclass gadget-output-record (basic-output-record displayed-output-record)
  ((gadget :initarg :gadget :accessor gadget)))

(defmethod initialize-instance :after ((record gadget-output-record) &key x y)
  (setf (output-record-position record) (values x y)))

(defmethod note-output-record-got-sheet ((record gadget-output-record) sheet)
  (multiple-value-bind (x y)  (output-record-position record)
    (sheet-adopt-child sheet (gadget record))
    (allocate-space (gadget record)
		    (rectangle-width record)
		    (rectangle-height record))
    (move-sheet (gadget record) x y)))

(defmethod note-output-record-lost-sheet ((record gadget-output-record) sheet)
  (sheet-disown-child sheet (gadget record)))

;; This is as good a place as any other to handle moving the position of the
;; gadget if the output record has moved. This is consistent with other
;; operations on output records that force you to manage repainting manually.
(defmethod replay-output-record  ((record gadget-output-record) stream
                                  &optional region x-offset y-offset)
  (declare (ignorable record stream region x-offset y-offset))
  (multiple-value-bind (gx gy)
      (transform-position (sheet-transformation (gadget record)) 0 0)
    (multiple-value-bind (ox oy)
        (output-record-position record)
      (unless (and (= ox gx)
                   (= oy gy))
        (move-sheet (gadget record) ox oy)))))

(defun setup-gadget-record (sheet record)
  (let* ((child (gadget record))
	 (sr (compose-space child))
	 (width  (space-requirement-width sr))
	 (height (space-requirement-height sr)))
    (multiple-value-bind (x y)(output-record-position record)
      (setf (rectangle-edges* record) (values x y (+ x width) (+ y height)))
    (when t ; :move-cursor t
      ;; Almost like LWW, except baseline of text should align with bottom
      ;; of gadget? FIXME
      (setf (stream-cursor-position sheet)
	    (values (+ x (bounding-rectangle-width record))
		    (+ y (bounding-rectangle-height record))))))))

;; The CLIM 2.0 spec does not really say what this macro should return.
;; Existing code written for "Real CLIM" assumes it returns the gadget pane
;; object. I think returning the gadget-output-record would be more useful.
;; For compatibility I'm having it return (values GADGET GADGET-OUTPUT-RECORD)

(defmacro with-output-as-gadget ((stream &rest options) &body body)
  ;; NOTE - incremental-redisplay 12/28/05 will call this on redisplay
  ;; unless wrapped in (updating-output (stream :cache-value t) ...)
  ;; Otherwise, new gadget-output-records are generated but only the first
  ;; gadget is ever adopted, and an erase-output-record called on a newer
  ;; gadget-output-record will face a sheet-not-child error when trying
  ;; to disown the never adopted gadget.
  (setf stream (stream-designator-symbol stream '*standard-output*))
  (let ((gadget-output-record (gensym))
	(x (gensym))
	(y (gensym)))
    `(multiple-value-bind (,x ,y)(stream-cursor-position ,stream)
       (flet ((with-output-as-gadget-continuation (,stream record)
		(flet ((with-output-as-gadget-body (,stream)
			 (declare (ignorable ,stream))
			 (progn ,@body)))
		  (setf (gadget record)
			(with-output-as-gadget-body ,stream))))
	      (gadget-output-record-constructor ()
		(make-instance 'gadget-output-record
			       ,@options :x ,x :y ,y)))
	 (declare (dynamic-extent with-output-as-gadget-continuation
				  gadget-output-record-constructor))
	 (let ((,gadget-output-record
		(invoke-with-output-to-output-record
		 ,stream
		 #'with-output-as-gadget-continuation
		 nil
		 #'gadget-output-record-constructor)))
	   (setup-gadget-record ,stream ,gadget-output-record)
	   (stream-add-output-record ,stream ,gadget-output-record)
	   (values (gadget ,gadget-output-record) ,gadget-output-record))))))
;;; 

(defclass orientation-from-parent-mixin () ())

(defmethod orientation ((gadget orientation-from-parent-mixin))
  (etypecase (sheet-parent gadget)
    ((or hbox-pane hrack-pane) :vertical)
    ((or vbox-pane vrack-pane) :horizontal)))


(defclass clim-extensions::box-adjuster-gadget
               (standard-gadget 3d-border-mixin orientation-from-parent-mixin)
  ((drag-from :initform nil)
   (left-sr)
   (left-peer)
   (right-sr)
   (right-peer))
  (:default-initargs :background *3d-inner-color*)
  (:documentation "The box-adjuster-gadget allows users to resize the panes
in a layout by dragging the boundary between the panes.  To use it, insert
it in a layout between two panes that are to be resizeable.  E.g.:
 (vertically ()
    top-pane
    (make-pane 'clim-extensions:box-adjuster-gadget)
    bottom-pane)"))  

(defmethod compose-space ((gadget clim-extensions:box-adjuster-gadget)
                          &key width height)
  (declare (ignore width height))
  (if (eq (orientation gadget) :vertical)
      (make-space-requirement :min-width 6 :width 6 :max-width 6
                              :min-height 1 :height 1)
      (make-space-requirement :min-height 6 :height 6 :max-height 6
                              :min-width 1 :width 1)))

(defmethod handle-event ((gadget clim-extensions:box-adjuster-gadget)
                         (event pointer-button-press-event))
  (with-slots (drag-from left-peer right-peer left-sr right-sr) gadget
    (setf drag-from (if (eq (orientation gadget) :vertical)
                        (pointer-event-native-graft-x event)
                        (pointer-event-native-graft-y event))
          (values left-peer right-peer)
          (do ((current (box-layout-mixin-clients (sheet-parent gadget)) (rest current)))
              ((or (null (third current))
                   (eq gadget (box-client-pane (second current))))
               (values (box-client-pane (first current))
                       (box-client-pane (third current)))))
          left-sr  (compose-space left-peer)
          right-sr (compose-space right-peer))))

(defmethod handle-event ((gadget clim-extensions:box-adjuster-gadget)
                         (event pointer-button-release-event))
  (setf (slot-value gadget 'drag-from) nil))

(defun adjust-space-requirement (pane sr orientation delta)
  (multiple-value-bind (major major-max major-min)
      (if (eq orientation :vertical)
          (values (space-requirement-width sr)
                  (space-requirement-max-width sr)
                  (space-requirement-min-width sr))
          (values (space-requirement-height sr)
                  (space-requirement-max-height sr)
                  (space-requirement-min-height sr)))
    (let ((new-major (max major-min (min major-max (+ major delta)))))
      (if (eq orientation :vertical)
          (change-space-requirements pane :width new-major)
          (change-space-requirements pane :height new-major))
      (- new-major major))))

(defmethod handle-event ((gadget clim-extensions:box-adjuster-gadget)
                         (event pointer-motion-event)
                         &aux (orientation (orientation gadget)))
  (with-slots (drag-from left-peer left-sr right-peer right-sr) gadget
    (when (and drag-from (typep (sheet-parent gadget) 'box-layout-mixin))
      (let* ((major-pos (if (eq orientation :vertical)
                            (pointer-event-native-graft-x event)
                            (pointer-event-native-graft-y event)))
             (delta (- major-pos drag-from)))
        (changing-space-requirements (:resize-frame nil)
          (adjust-space-requirement left-peer  left-sr  orientation delta)
          (adjust-space-requirement right-peer right-sr orientation (- delta)))))))

(defmethod note-sheet-grafted ((sheet clim-extensions:box-adjuster-gadget))
  (setf (sheet-pointer-cursor sheet) :rotate))

;;; Support for definition of callbacks and associated callback events. A
;;; callback event is used by a backend when a high-level notification of a
;;; gadget state change is delivered in the CLIM event process -- by a native
;;; gadget, for example -- and must be delivered in the application process.

(define-event-class callback-event (standard-event)
  ((sheet :initarg :gadget :reader event-gadget
	  :documentation "An alias for sheet, for readability")
   (callback-function :initarg :callback-function :reader callback-function)
   (client :initarg :client :reader event-client)
   (client-id :initarg :client-id :reader event-client-id)
   (other-args :initarg :other-args :reader event-other-args :initform nil)))

(defun queue-callback (fn gadget client client-id &rest other-args)
  (queue-event gadget (make-instance 'callback-event
				     :callback-function fn
				     :gadget gadget
				     :client client
				     :client-id client-id
				     :other-args other-args)))

(defmethod handle-event ((gadget basic-gadget) (event callback-event))
  (apply (callback-function event)
	 (event-gadget event)
	 (event-client event)
	 (event-client-id event)
	 (event-other-args event)))

