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

(in-package :CLIM-INTERNALS)

; copied to here to avoid a stupid sbcl forward class problem
#+sbcl
(defclass value-changed-repaint-mixin ()
  ()
  (:documentation
   "Mixin class for gadgets, whose appearence depends on its value."))

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

;; Is "no label" as initarg to labelled gadget really such a good
;; idea? I would prefer "".

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

;; - I would like to have a :ACTIVE-P initarg

;; - :CHOICES initarg to RADIO-BOX and CHECK-BOX is from Franz' user
;;   guide.

;;;; TODO

;; - the scroll-bar needs more work:
;;    . dragging should not change the value, the value should only
;;      be changed after releasing the mouse.
;;    . it should arm/disarm
;;    . it should be deactivatable

;; - the slider needs a total overhaul

;; - LIST-PANE must move from panes.lisp here.

;; - OPTION-PANE needs an implmentation

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

;;;; ------------------------------------------------------------------------------------------
;;;;
;;;;  30.3 Basic Gadget Classes
;;;;

;;;
;;; Gadget
;;;

(define-protocol-class gadget (pane)
  ((id                :initarg :id
                      :initform (gensym "GADGET")
                      :accessor gadget-id)
   (client            :initarg :client
                      :initform *application-frame*
                      :accessor gadget-client)
   (armed-callback    :initarg :armed-callback
                      :initform nil
                      :reader gadget-armed-callback)
   (disarmed-callback :initarg :disarmed-callback
                      :initform nil
                      :reader gadget-disarmed-callback)
   ;; [Arthur] I'm not so sure about the value for :initform.
   ;; Maybe T is better? Or maybe we should call
   ;; ACTIVATE-GADGET after creating a gadget?
   ;;
   ;; I think, T is correct here --GB
   (active-p            :initform t
                        :reader gadget-active-p)
   ;;
   ;; I am not so lucky with the armed slot in GADGET --GB
   (armed               :initform nil)

   ;; These are directly borrowed from BASIC-PANE I am still not sure
   ;; about the exact class hierarchy to implement. --GB
#|
   (foreground       :initarg :foreground
                     :initform +black+
                     :reader pane-foreground)
   (background       :initarg :background
                     :initform +white+
                     :reader pane-background)
   (text-style       :initarg :text-style
                     :initform *default-text-style*
                     :reader pane-text-style)

   (align-x          :initarg :align-x
                     :type (member :left :center :right)
                     :initform :left    ;??
                     :reader pane-align-x)
   (align-y          :initarg :align-y
                     :type (member :top :center :bottom)
                     :initform :top     ;??
                     :reader pane-align-y)|# )

  )

;;;

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
          (ds (text-style-ascent (gadget-label-text-style pane) pane)))
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
                        gadget
                        )
  ;; Half-baked attempt to be compatible with Lispworks. ??? -moore
  ;; Inherited from basic-pane with different defaults.
  ((foreground  :initform +black+)
   (background  :initform +white+)
   ))


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

(defgeneric (setf gadget-value) (value gadget &key invoke-callback))

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
   #+NIL
   (align-x     :initarg :align-x
                :accessor gadget-label-align-x)
   #+NIL
   (align-y     :initarg :align-y
                :accessor gadget-label-align-y)
   #+NIL
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
               :accessor scroll-bar-thumb-size)
   ))

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
      :value nil))

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
    appearance is a list of buttons."))

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
  (dispatch-repaint gadget +everywhere+))

(defmethod disarmed-callback :after ((gadget arm/disarm-repaint-mixin) client id)
  (declare (ignore client id))
  (dispatch-repaint gadget +everywhere+))

(defclass value-changed-repaint-mixin ()
  ()
  (:documentation
   "Mixin class for gadgets, whose appearence depends on its value."))

(defmethod (setf gadget-value) :after (new-value (gadget value-changed-repaint-mixin) 
                                       &key &allow-other-keys)
  (declare (ignore new-value))
  (dispatch-repaint gadget +everywhere+))

;;;; Event handling mixins

(defclass enter/exit-arms/disarms-mixin ()
  ()
  (:documentation
   "Mixin class for gadgets, which will be armed, when the mouse enters and 
    disarmed, when the mouse leaves."))

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

;;

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

;;;; ------------------------------------------------------------------------------------------
;;;;
;;;;  Drawing Utilities for Concrete Gadgets
;;;;

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
  (compose-over (compose-in +paleturquoise+ (make-opacity .5))
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
      +LEMONCHIFFON+
      (compose-over (compose-in +LEMONCHIFFON+ (make-opacity .5))
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
    :y-spacing 4))

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
        (draw-label* pane x1 y1 x2 y2
                     :ink (effective-gadget-foreground pane))))))

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
  ((indicator-type :type (member '(:one-of :some-of))
		   :initarg :indicator-type
		   :reader toggle-button-indicator-type
                   :initform :some-of) )
  (:default-initargs
   :text-style (make-text-style :sans-serif nil nil)
    :align-x :left
    :align-y :center
    :x-spacing 3
    :y-spacing 3
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
          (draw-label* pane
                       (+ x1 x-spacing)
                       (+ y1 y-spacing)
                       (- x2 x-spacing)
                       (- y2 y-spacing)
                       :ink (effective-gadget-foreground pane)))))))

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

(defclass scroll-bar-pane (sheet-multiple-child-mixin
                           3D-border-mixin
                           scroll-bar
                           )
  ((event-state :initform nil)
   (drag-dy :initform nil)
   (inhibit-redraw-p
    :initform nil
    :documentation "Hack, when set to non-NIL changing something does not trigger redrawing.")
   (thumb :initform nil)
   )
  (:default-initargs :value 0
                     :min-value 0
                     :max-value 1
                     :orientation :vertical
                     :border-width 2
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

;;; The thumb of a scroll bar

;; work in progress --GB

#||
(defclass scroll-bar-thumb-pane (arm/disarm-repaint-mixin
                                 basic-gadget)
  ((tr :initform nil)
   (allowed-region :initarg :allowed-region))
  (:default-initargs
      :background *3d-normal-color*))

(defmethod handle-event ((pane scroll-bar-thumb-pane) (event pointer-enter-event))
  (declare (ignoreable event))
  (with-slots (armed) pane
    (arm-gadget pane (adjoin :have-mouse armed))))

(defmethod handle-event ((pane scroll-bar-thumb-pane) (event pointer-exit-event))
  (declare (ignoreable event))
  (with-slots (armed) pane
    (arm-gadget pane (remove :have-mouse armed))))

(defmethod handle-event ((pane scroll-bar-thumb-pane) (event pointer-button-press-event))
  (with-slots (tr armed) pane
    (arm-gadget pane (adjoin :dragging armed))
    (setf tr (compose-transformations
              (make-scaling-transformation 1 1)
              (compose-transformations
              (compose-transformations
               (make-translation-transformation (- (pointer-event-x event)) (- (pointer-event-y event)))
               (invert-transformation (sheet-delta-transformation (sheet-parent pane) (graft pane))))
              (invert-transformation (sheet-native-transformation (graft pane)))))) ))

(defmethod handle-event ((pane scroll-bar-thumb-pane) (event pointer-button-release-event))
  (with-slots (tr armed) pane
    (arm-gadget pane (remove :dragging armed))
    (setf tr nil)) )

(defmethod handle-event ((pane scroll-bar-thumb-pane) (event pointer-motion-event))
  (with-slots (tr allowed-region) pane
    (when tr
      (multiple-value-bind (nx ny) (transform-position tr
                                                       (pointer-event-native-graft-x event)
                                                       (pointer-event-native-graft-y event))
        (with-bounding-rectangle* (x1 y1 x2 y2) allowed-region
          (move-sheet pane
                      (clamp nx x1 x2)
                      (clamp ny y1 y2)))))))

(defmethod handle-repaint ((pane scroll-bar-thumb-pane) region)
  (with-bounding-rectangle* (x1 y1 x2 y2) pane
    (draw-rectangle* pane x1 y1 x2 y2 :ink (effective-gadget-background pane))
    (draw-bordered-polygon pane
                           (polygon-points (make-rectangle* x1 y1 x2 y2))
                           :style :outset
                           :border-width 2)
    (let ((y (/ (+ y1 y2) 2)))
      (draw-bordered-polygon pane
                             (polygon-points (make-rectangle* (+ x1 3) (- y 1) (- x2 3) (+ y 1)))
                             :style :inset
                             :border-width 1)
      (draw-bordered-polygon pane
                             (polygon-points (make-rectangle* (+ x1 3) (- y 4) (- x2 3) (- y 2)))
                             :style :inset
                             :border-width 1)
      (draw-bordered-polygon pane
                             (polygon-points (make-rectangle* (+ x1 3) (+ y 4) (- x2 3) (+ y 2)))
                             :style :inset
                             :border-width 1))))

;;;

(defmethod sheet-adopt-child :after (sheet (scroll-bar scroll-bar-pane))
  ;; create a sheet for the thumb
  '(with-slots (thumb) scroll-bar
    (setf thumb (make-pane 'scroll-bar-thumb-pane
                           :allowed-region (make-rectangle* 2 15 14 340)
                           ))
    (setf (sheet-region thumb)
          (make-rectangle* 0 0 12 50))
    (setf (sheet-transformation thumb)
          (compose-transformations
           (make-transformation 1 0 0 1 0 0)
           (make-translation-transformation 2 0)))
    (sheet-adopt-child scroll-bar thumb)))

||#

;;; Utilities

;; We think all scroll bars as vertically oriented, therefore we have
;; SCROLL-BAR-TRANSFORMATION, which should make every scroll bar
;; look like being vertically oriented -- simplifies much code.

(defmethod scroll-bar-transformation ((sb scroll-bar))
  (ecase (gadget-orientation sb)
    (:vertical   +identity-transformation+)
    (:horizontal (make-transformation 0 1 1 0 0 0))))

(defun translate-range-value (a mina maxa mino maxo)
  "When \arg{a} is some value in the range from \arg{mina} to \arg{maxa},
   proportionally translate the value into the range \arg{mino} to \arg{maxo}."
  (+ mino (* (/ (- a mina) (- maxa mina)) (- maxo mino))))

;;; Scroll-bar's sub-regions

(defmethod (setf scroll-bar-thumb-size) :after (new-value (sb scroll-bar-pane))
  (declare (ignore new-value))
  (with-slots (inhibit-redraw-p thumb) sb
    #||
    ;;work in progress
    (setf (sheet-region thumb)
          (with-bounding-rectangle* (x1 y1 x2 y2) (scroll-bar-thumb-bed-region sb)
            (multiple-value-bind (minv maxv) (gadget-range* sb)
              (multiple-value-bind (v) (gadget-value sb)
                (let ((ts (scroll-bar-thumb-size sb)))
                  (let ((ya (translate-range-value v minv (+ maxv ts) y1 y2))
                        (yb (translate-range-value (+ v ts) minv (+ maxv ts) y1 y2)))
                    (make-rectangle* 0 0 (- x2 x1) (- yb ya))))))))
    ||#
    (unless inhibit-redraw-p
      (dispatch-repaint sb +everywhere+)))) ;arg...

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

(defmethod scroll-bar-thumb-bed-region ((sb scroll-bar-pane))
  (with-bounding-rectangle* (minx miny maxx maxy) (transform-region (scroll-bar-transformation sb)
                                                                    (pane-inner-region sb))
    (make-rectangle* minx (+ miny (- maxx minx) 1)
                     maxx (- maxy (- maxx minx) 1))))

(defmethod scroll-bar-thumb-region ((sb scroll-bar-pane))
  (with-bounding-rectangle* (x1 y1 x2 y2) (scroll-bar-thumb-bed-region sb)
    (multiple-value-bind (minv maxv) (gadget-range* sb)
      (multiple-value-bind (v) (gadget-value sb)
        (let ((ts (scroll-bar-thumb-size sb)))
          (let ((ya (translate-range-value v minv (+ maxv ts) y1 y2))
                (yb (translate-range-value (+ v ts) minv (+ maxv ts) y1 y2)))
            (make-rectangle* x1 ya x2 yb)))))))

#||
;; alternative:

(defmethod scroll-bar-up-region ((sb scroll-bar-pane))
  (with-bounding-rectangle* (minx miny maxx maxy) (transform-region (scroll-bar-transformation sb)
                                                                    (sheet-region sb))
    (make-rectangle* (+ minx 2) (- (- maxy (* 2 (- maxx minx))) 2)
                     (- maxx 2) (- (- maxy (- maxx minx)) 2))))

(defmethod scroll-bar-down-region ((sb scroll-bar-pane))
  (with-bounding-rectangle* (minx miny maxx maxy) (transform-region (scroll-bar-transformation sb)
                                                                    (sheet-region sb))
    (make-rectangle* (+ minx 2) (+ (- maxy (- maxx minx)) 2)
                     (- maxx 2) (-  maxy 2))))

(defmethod scroll-bar-thumb-bed-region ((sb scroll-bar-pane))
  (with-bounding-rectangle* (minx miny maxx maxy) (transform-region (scroll-bar-transformation sb)
                                                                    (sheet-region sb))
    (make-rectangle* (+ minx 2) (+ miny 2 )
                     (- maxx 2) (- maxy 2 (* 2 (- maxx minx)) 2))))

(defmethod scroll-bar-thumb-region ((sb scroll-bar-pane))
  (with-bounding-rectangle* (x1 y1 x2 y2) (scroll-bar-thumb-bed-region sb)
    (multiple-value-bind (minv maxv) (gadget-range* sb)
      (multiple-value-bind (v) (gadget-value sb)
        (let ((ts (scroll-bar-thumb-size sb)))
          (let ((ya (translate-range-value v minv (+ maxv ts) y1 y2))
                (yb (translate-range-value (+ v ts) minv (+ maxv ts) y1 y2)))
            (make-rectangle* x1 ya x2 yb)))))))
||#


;;; Event handlers

#||
(defmethod handle-event ((sb scroll-bar-pane) (event pointer-enter-event))
  (declare (ignorable event))
  (with-slots (armed) sb
     (unless armed
       (setf armed t)
       (armed-callback sb (gadget-client sb) (gadget-id sb)))))

(defmethod handle-event ((sb scroll-bar-pane) (event pointer-exit-event))
  (declare (ignorable event))
  (with-slots (armed) sb
     (when armed
       (setf armed nil)
       (disarmed-callback sb (gadget-client sb) (gadget-id sb)))))
||#

(defmethod handle-event ((sb scroll-bar-pane) (event pointer-button-press-event))
  (multiple-value-bind (x y) (transform-position (scroll-bar-transformation sb)
                                                 (pointer-event-x event) (pointer-event-y event))
    (with-slots (event-state drag-dy) sb
      (cond ((region-contains-position-p (scroll-bar-up-region sb) x y)
             (scroll-up-line-callback sb (gadget-client sb) (gadget-id sb))
             (setf event-state :up-armed)
             (dispatch-repaint sb +everywhere+))
            ((region-contains-position-p (scroll-bar-down-region sb) x y)
             (scroll-down-line-callback sb (gadget-client sb) (gadget-id sb))
             (setf event-state :dn-armed)
             (dispatch-repaint sb +everywhere+))
            ((region-contains-position-p (scroll-bar-thumb-region sb) x y)
             (setf event-state :dragging
                   drag-dy (- y (bounding-rectangle-min-y (scroll-bar-thumb-region sb)))))
            ((region-contains-position-p (scroll-bar-thumb-bed-region sb) x y)
             (if (< y (bounding-rectangle-min-y (scroll-bar-thumb-region sb)))
                 (scroll-up-page-callback sb (gadget-client sb) (gadget-id sb))
                 (scroll-down-page-callback sb (gadget-client sb) (gadget-id sb))))
            (t
             nil)))))

(defmethod handle-event ((sb scroll-bar-pane) (event pointer-button-release-event))
  (with-slots (event-state) sb
    (case event-state
      (:up-armed (setf event-state nil))
      (:dn-armed (setf event-state nil))
      (otherwise
       (setf event-state nil) )))
  (dispatch-repaint sb +everywhere+) )

(defmethod handle-event ((sb scroll-bar-pane) (event pointer-motion-event))
  (multiple-value-bind (x y) (transform-position (scroll-bar-transformation sb)
                                                 (pointer-event-x event) (pointer-event-y event))
    (declare (ignore x))
    (with-slots (event-state drag-dy inhibit-redraw-p) sb
      (case event-state
        (:dragging
         (let* ((y-new-thumb-top (- y drag-dy))
                (ts (scroll-bar-thumb-size sb))
                (new-value (min (gadget-max-value sb)
                                (max (gadget-min-value sb)
                                     (translate-range-value y-new-thumb-top
                                                            (bounding-rectangle-min-y (scroll-bar-thumb-bed-region sb))
                                                            (bounding-rectangle-max-y (scroll-bar-thumb-bed-region sb))
                                                            (gadget-min-value sb)
                                                            (+ (gadget-max-value sb) ts))))))
           ;; Blitter hack:
           #-NIL
           (with-drawing-options (sb :transformation (scroll-bar-transformation sb))
             (with-bounding-rectangle* (ox1 oy1 ox2 oy2) (scroll-bar-thumb-region sb)
               (setf (gadget-value sb) new-value)
               (with-bounding-rectangle* (nx1 ny1 nx2 ny2) (scroll-bar-thumb-region sb)
		 (declare (ignore nx2))
                 (copy-area sb ox1 oy1 (- ox2 ox1) (- oy2 oy1) nx1 ny1)
                 (if (< oy1 ny1)
                     (draw-rectangle* sb ox1 oy1 ox2 ny1 :ink *3d-normal-color*)
                     (draw-rectangle* sb ox1 oy2 ox2 ny2 :ink *3d-normal-color*)))))
           #+NIL
           (dispatch-repaint sb +everywhere+)
           (unwind-protect
                (progn
                  (setf inhibit-redraw-p t)
                  (setf (gadget-value sb) new-value)
                  (drag-callback sb (gadget-client sb) (gadget-id sb)
                                 new-value))
             (setf inhibit-redraw-p nil))
           ))))))

;;; Repaint

(defmethod handle-repaint ((sb scroll-bar-pane) region)
  (declare (ignore region))
  (with-special-choices (sb)
    (let ((tr (scroll-bar-transformation sb)))
      (with-bounding-rectangle* (minx miny maxx maxy) (transform-region tr (sheet-region sb))
        (with-drawing-options (sb :transformation tr)
          (draw-rectangle* sb minx miny maxx maxy :filled t
                           :ink *3d-normal-color*)
          ;; draw up arrow
          (with-bounding-rectangle* (x1 y1 x2 y2) (scroll-bar-up-region sb)
            (let ((pg (list (make-point (/ (+ x1 x2) 2) y1)
                            (make-point x1 y2)
                            (make-point x2 y2))))
              (case (slot-value sb  'event-state)
                (:up-armed
                 (draw-polygon sb pg :ink *3d-inner-color*)
                 (draw-bordered-polygon sb pg :style :inset :border-width 2))
                (otherwise
                 (draw-polygon sb pg :ink *3d-normal-color*)
                 (draw-bordered-polygon sb pg :style :outset :border-width 2) ))))

          ;; draw down arrow
          (with-bounding-rectangle* (x1 y1 x2 y2) (scroll-bar-down-region sb)
            (let ((pg (list (make-point (/ (+ x1 x2) 2) y2)
                            (make-point x1 y1)
                            (make-point x2 y1))))
              (case (slot-value sb 'event-state)
                (:dn-armed
                 (draw-polygon sb pg :ink *3d-inner-color*)
                 (draw-bordered-polygon sb pg :style :inset :border-width 2))
                (otherwise
                 (draw-polygon sb pg :ink *3d-normal-color*)
                 (draw-bordered-polygon sb pg :style :outset :border-width 2)))))
          ;; draw thumb
          (with-bounding-rectangle* (x1 y1 x2 y2) (scroll-bar-thumb-region sb)
            (draw-rectangle* sb x1 y1 x2 y2 :ink *3d-normal-color*)
            (draw-bordered-polygon sb
                                   (polygon-points (make-rectangle* x1 y1 x2 y2))
                                   :style :outset
                                   :border-width 2)
            (let ((y (/ (+ y1 y2) 2)))
              (draw-bordered-polygon sb
                                     (polygon-points (make-rectangle* (+ x1 3) (- y 1) (- x2 3) (+ y 1)))
                                     :style :inset
                                     :border-width 1)
              (draw-bordered-polygon sb
                                     (polygon-points (make-rectangle* (+ x1 3) (- y 4) (- x2 3) (- y 2)))
                                     :style :inset
                                     :border-width 1)
              (draw-bordered-polygon sb
                                     (polygon-points (make-rectangle* (+ x1 3) (+ y 4) (- x2 3) (+ y 2)))
                                     :style :inset
                                     :border-width 1))) )))))


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
          (background-color (gadget-current-color pane)))
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
	      (draw-circle* pane middle (- position slider-button-half-short-dim) 8.0
                                   :filled t :ink background-color)
	      (draw-circle* pane middle (- position slider-button-half-short-dim) 8.0
                                   :filled nil :ink +black+)
	      (draw-circle* pane middle (- position slider-button-half-short-dim) 7.0
                                   :filled nil
                                   :start-angle (* 0.25 pi)
                                   :end-angle   (* 1.25 pi)
                                   :ink +white+)
	      (draw-circle* pane middle (- position slider-button-half-short-dim) 7.0
                                   :filled nil
                                   :start-angle (* 1.25 pi)
                                   :end-angle   (* 2.25 pi)
                                   :ink +black+)
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
	      (draw-circle* pane (- position slider-button-half-short-dim) middle 8.0
                                   :filled t :ink background-color)
	      (draw-circle* pane (- position slider-button-half-short-dim) middle 8.0
                                   :filled nil :ink +black+)
	      (draw-circle* pane (- position slider-button-half-short-dim) middle 7.0
                                   :filled nil
                                   :start-angle (* 0.25 pi)
                                   :end-angle   (* 1.25 pi)
                                   :ink +white+)
	      (draw-circle* pane (- position slider-button-half-short-dim) middle 7.0
                                   :filled nil
                                   :start-angle (* 1.25 pi)
                                   :end-angle   (* 2.25 pi)
                                   :ink +black+)
              (when (gadget-show-value-p pane)
	        (draw-text* pane (format-value (gadget-value pane)
                                               (slider-decimal-places pane))
			         5 ;(- position slider-button-half-short-dim)
			         (- middle slider-button-half-long-dim))))))))))

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
        (+ good-dim1 (/ (* (- (gadget-value pane) (gadget-min-value pane))
			   (- good-dim2 good-dim1))
		        (gadget-range pane)))))))

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

#||
(defclass list-pane (value-gadget)
  ((mode        :initarg :mode
                :initform :some-of
                :reader list-pane-mode
                :type (member :one-of :some-of))
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
   (test        :initarg :test
                :initform #'eql
                :reader list-pane-test
                :documentation "A function to compare two items for equality.") ))

(defclass generic-list-pane (basic-pane list-pane #|permanent-medium-sheet-output-mixin|# )
  ((item-strings :initform nil
                 :documentation "Vector of item strings.")
   (selected-items :initform nil
                   :documentation "List of indexes of selected items.")
   ))

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

(defmethod compose-space ((pane generic-list-pane) &key width height)
  (declare (ignore width height))
  (let* ((n (length (generic-list-pane-item-strings pane)))
         (w (reduce #'max (map 'vector (lambda (item-string)
                                         (text-size pane item-string))
                               (generic-list-pane-item-strings pane))
                    :initial-value 0))
         (h (* n (+ (text-style-ascent (pane-text-style pane) pane)
                    (text-style-descent (pane-text-style pane) pane)))))
    (make-space-requirement :width w :height h
                            :min-width w :min-height h
                            :max-width w :max-height h)))

#+NIL
(defmethod allocate-space ((pane generic-list-pane) width height)
  )

(defmethod handle-repaint ((pane generic-list-pane) region)
  (with-slots (selected-items) pane
    (let* ((a (text-style-ascent (pane-text-style pane) pane))
           (d (text-style-descent (pane-text-style pane) pane)))
      (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
        (loop
            for i from 0
            for x across (generic-list-pane-item-strings pane) do
              (let ((y (+ a (* i (+ a d)))))
                (cond ((member i selected-items)
                       (draw-rectangle* pane x1 (- y a) x2 (+ y d))
                       (draw-text* pane x 0 y :ink +background-ink+))
                      (t
                       (draw-rectangle* pane x1 (- y a) x2 (+ y d) :ink +background-ink+)
                       (draw-text* pane x 0 y)))))))))

(defmethod handle-event ((pane generic-list-pane) (event pointer-button-press-event))
  (multiple-value-bind (mx my) (values (pointer-event-x event) (pointer-event-y event))
    (let ((k (floor my (+ (text-style-ascent (pane-text-style pane) pane)
                          (text-style-descent (pane-text-style pane) pane))))
          (n (length (generic-list-pane-item-strings pane))))
      (if (member k (slot-value pane 'selected-items))
          (setf (slot-value pane 'selected-items) (delete k (slot-value pane 'selected-items)))
        (pushnew k (slot-value pane 'selected-items)))
      (dispatch-repaint pane +everywhere+)
      )))

||#

;;; ------------------------------------------------------------------------------------------
;;;  30.4.8 The concrete text-field Gadget


(defparameter *default-text-field-text-style*
    (make-text-style :fixed :roman :normal))

(defclass text-field-pane (text-field 
			   standard-extended-output-stream
			   standard-output-recording-stream
			   enter/exit-arms/disarms-mixin
			   basic-pane)
  ((area :accessor area :initform nil 
	 :documentation "The Goatee area used for text editing.")
   (previous-focus :accessor previous-focus :initform nil
		   :documentation
		   "The pane that previously had keyboard focus")
   (exposed :accessor exposed :initform nil))
  (:default-initargs
    :text-style *default-text-field-text-style*))

(defmethod initialize-instance :after ((gadget text-field) &rest rest)
  (unless (getf rest :normal)
    (setf (slot-value gadget 'current-color) +white+
	  (slot-value gadget 'normal) +white+)))

(defmethod initialize-instance :after ((pane text-field-pane) &rest rest)
  (declare (ignore rest))
  #-nil (setf (medium-text-style (sheet-medium pane))
	      (slot-value pane 'text-style)))

(defmethod handle-repaint :after ((pane text-field-pane) region)
  (declare (ignore region))
  (unless (exposed pane)
    (multiple-value-bind (cx cy)
	(stream-cursor-position pane)
      (setf (cursor-visibility (stream-text-cursor pane)) nil)
      (setf (area pane) (make-instance 'goatee:simple-screen-area
				       :area-stream pane
				       :x-position cx
				       :y-position cy
				       :initial-contents (slot-value pane
								     'value))))
    (stream-add-output-record pane (area pane))))

;;; Unilaterally declare a "focus follows mouse" policy.  I don't like this
;;; much; the whole issue of keyboard focus needs a lot more thought,
;;; especially when multiple application frames per port become possible.

(defmethod armed-callback :after ((gadget text-field-pane) client id)
  (declare (ignore client id))
  (let ((port (port gadget)))
    (setf (previous-focus gadget) (port-keyboard-input-focus port))
    (setf (port-keyboard-input-focus port) gadget)))

(defmethod disarmed-callback :after ((gadget text-field-pane) client id)
  (declare (ignore client id))
  (let ((port (port gadget)))
    (setf (port-keyboard-input-focus port) (previous-focus gadget))
    (setf (previous-focus gadget) nil)))

(defmethod handle-event ((gadget text-field-pane) (event key-press-event))
  (let ((gesture (convert-to-gesture event))
	(*activation-gestures* *standard-activation-gestures*))
    (when (activation-gesture-p gesture)
      (activate-callback gadget (gadget-client gadget) (gadget-id gadget))
      (return-from handle-event t))
    (goatee:execute-gesture-command gesture
				    (area gadget)
				    goatee::*simple-area-gesture-table*)
    (let ((new-value (goatee::buffer-string (goatee::buffer (area gadget)))))
      (unless (string= (gadget-value gadget) new-value)
	(setf (slot-value gadget 'value) new-value)
	(value-changed-callback gadget 
				(gadget-client gadget) 
				(gadget-id gadget)
				new-value)))))

(defmethod (setf gadget-value) :after (new-value (gadget text-field-pane)
				       &key invoke-callback)
  (declare (ignore invoke-callback))
  (let* ((area (area gadget))
	 (buffer (goatee::buffer area))
	 (start (goatee::buffer-start buffer))
	 (end (goatee::buffer-end buffer)))
    (goatee::delete-region buffer start end)
    (goatee::insert buffer new-value :position start)
    (goatee::redisplay-area area)))

#+nil
(defmethod handle-repaint ((pane text-field-pane) region)
  (declare (ignore region))
  (with-special-choices (pane)
    (with-sheet-medium (medium pane)
      (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
        (display-gadget-background pane (gadget-current-color pane) 0 0 (- x2 x1) (- y2 y1))
        (draw-text* pane (gadget-value pane)
                    x1
                    (+ y1 (text-style-ascent (medium-text-style medium) medium))
                    :align-x :left
                    :align-y :baseline)))))


(defmethod compose-space ((pane text-field-pane) &key width height)
  (declare (ignore width height))
  (with-sheet-medium (medium pane)
    (let ((as (text-style-ascent (medium-text-style medium) medium))
          (ds (text-style-descent (medium-text-style medium) medium))
          (w  (text-size medium (gadget-value pane))))
      (let ((width w)
            (height (+ as ds)))
        (make-space-requirement :width width :height height
                                :max-width width :max-height height
                                :min-width width :min-height height)))))

(defmethod allocate-space ((pane text-field-pane) w h)
  (resize-sheet pane w h))
  
;;; ------------------------------------------------------------------------------------------
;;;  30.4.9 The concrete text-editor Gadget

(defclass text-editor-pane (text-editor)
  ((width :type integer
	  :initarg :width
	  :initform 300
	  :reader text-editor-width)
   (height :type integer
	   :initarg :height
	   :initform 300
	   :reader text-editor-height)))

(defmethod compose-space ((pane text-editor-pane) &key width height)
  (declare (ignore width height))
  (let ((width (text-editor-width pane))
	(height (text-editor-height pane)))
  (make-space-requirement :width width
			  :min-width width
			  :max-width width
			  :height height
			  :min-height height
			  :max-height height)))

;;;; ------------------------------------------------------------------------------------------
;;;;
;;;;  30.5 Integrating Gadgets and Output Records
;;;;

;;
;; GADGET-OUTPUT-RECORD
;;

(defclass gadget-output-record (output-record) ())

(defmacro with-output-as-gadget (stream &body body)
  (declare (type symbol stream))
  (when (eq stream t)
    (setq stream '*standard-output*))
  (let ((gadget (gensym))
	(gadget-output-record (gensym)))
    `(let* ((,gadget (progn ,@body))
	    (,gadget-output-record (make-instance 'gadget-output-record 
				    :children (list ,gadget))))
       (stream-add-output-record ,stream ,gadget-output-record)
       ,gadget)))



