;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2000 by
;;; Arthur Lemmens (lemmens@simplex.nl),
;;; Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;; and Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;  (c) copyright 2001 by
;;; Lionel Salabartan (salabart@emi.u-bordeaux.fr)
;;;  (c) copyright 2001 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2001 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 2006 by Troels Henriksen (athas@sigkill.dk)

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

;;; This file contains the concrete implementation of the text-field
;;; and text-editor gadgets. It is loaded rather late, because it
;;; requires Drei. Half of the complexity here is about working around
;;; annoying Goatee quirks, generalising it to three editor substrates
;;; is nontrivial.

(in-package :clim-internals)

;;; The text editor gadget(s) is implemented as a class implementing
;;; the text editor gadget protocol, but containing an editor
;;; substrate object that takes care of the actual editing logic,
;;; redisplay, etc. The substrates need to be gadgets themselves and
;;; are defined here.

(defparameter *default-text-field-text-style*
  (make-text-style :fix :roman :normal))

(defclass editor-substrate-mixin (value-gadget)
  ((activation-gestures :reader activation-gestures
                        :initarg :activation-gestures)
   (user :reader user-gadget
         :initarg :user-gadget
         :documentation "The editor gadget using this editor substrate."
         :initform (error "Editor substrates must have a user.")))
  (:documentation "A mixin class for text editor gadget substrates.")
  (:default-initargs :activation-gestures '()))

(defmethod gadget-id ((gadget editor-substrate-mixin))
  (gadget-id (user-gadget gadget)))

(defmethod (setf gadget-id) (value (gadget editor-substrate-mixin))
  (setf (gadget-id (user-gadget gadget)) value))

(defmethod gadget-client ((gadget editor-substrate-mixin))
  (gadget-client (user-gadget gadget)))

(defmethod (setf gadget-client) (value (gadget editor-substrate-mixin))
  (setf (gadget-client (user-gadget gadget)) value))

(defmethod gadget-armed-callback ((gadget editor-substrate-mixin))
  (gadget-armed-callback (user-gadget gadget)))

(defmethod gadget-disarmed-callback ((gadget editor-substrate-mixin))
  (gadget-disarmed-callback (user-gadget gadget)))

(defclass text-field-substrate-mixin (editor-substrate-mixin)
  ()
  (:documentation "A mixin class for editor substrates used for text field gadgets."))

(defclass text-editor-substrate-mixin (editor-substrate-mixin)
  ((ncolumns :reader text-editor-ncolumns
             :initarg :ncolumns
             :initform nil
             :type (or null integer))
   (nlines :reader text-editor-nlines
           :initarg :nlines
           :initform nil
           :type (or null integer)))
  (:documentation "A mixin class for editor substrates used for text editor gadgets."))

;;; Now, define the Drei substrate.

(defclass drei-editor-substrate (drei:drei-gadget-pane
                                 editor-substrate-mixin)
  ()
  (:metaclass esa-utils:modual-class)
  (:documentation "A class for Drei-based editor substrates."))

(defmethod (setf gadget-value) :after (value (gadget drei-editor-substrate)
                                             &key invoke-callback)
  (declare (ignore invoke-callback))
  ;; Hm! I wonder if this can cause trouble.  I think not.
  (drei:display-drei gadget))

(defclass drei-text-field-substrate (text-field-substrate-mixin
                                     drei-editor-substrate)
  ()
  (:metaclass esa-utils:modual-class)
  (:documentation "The class for Drei-based text field substrates."))

(defmethod drei:handle-gesture ((drei drei-text-field-substrate) gesture)
  (if (with-activation-gestures ((activation-gestures drei))
        (activation-gesture-p gesture))
      (activate-callback drei (gadget-client drei) (gadget-id drei))
      (call-next-method)))

(defmethod compose-space ((pane drei-text-field-substrate) &key width height)
  (declare (ignore width height))
  (with-sheet-medium (medium pane)
    (let ((as (text-style-ascent (medium-text-style medium) medium))
          (ds (text-style-descent (medium-text-style medium) medium))
          (w  (text-size medium (gadget-value pane))))
      (let ((width w)
            (height (+ as ds)))
        (make-space-requirement :height height :max-height height :min-height height
                                                                  :min-width width :width width)))))

(defclass drei-text-editor-substrate (text-editor-substrate-mixin
                                      drei-editor-substrate)
  ()
  (:metaclass esa-utils:modual-class)
  (:documentation "The class for Drei-based text editor substrates."))

(defmethod compose-space ((pane drei-text-editor-substrate) &key width height)
  (with-sheet-medium (medium pane)
    (let* ((text-style (medium-text-style medium))
           (line-height (+ (text-style-height text-style medium)
                           (stream-vertical-spacing pane)))
           (column-width (text-style-width text-style medium)))
      (with-accessors ((ncolumns text-editor-ncolumns)
                       (nlines text-editor-nlines)) pane
        (apply #'space-requirement-combine* #'(lambda (req1 req2)
                                                (or req2 req1))
               (call-next-method)
               (let ((width (if ncolumns
                                (+ (* ncolumns column-width))
                                width))
                     (height (if nlines
                                 (+ (* nlines line-height))
                                 height)))
                 (list
                  :width width :max-width width :min-width width
                  :height height :max-height height :min-height height)))))))

(defmethod allocate-space ((pane drei-text-editor-substrate) w h)
  (resize-sheet pane w h))

;;; Now, define the Goatee substrate.

(defclass goatee-editor-substrate (editor-substrate-mixin
                                   text-field
                                   clim-stream-pane)
  ((area :accessor area
         :initform nil
         :documentation "The Goatee area used for text editing.")
   ;; This hack is necessary because the Goatee editing area is not
   ;; created until the first redisplay... yuck.
   (value :documentation "The initial value for the Goatee area."))
  (:default-initargs
   :text-style *default-text-field-text-style*))

(defmethod initialize-instance :after ((pane goatee-editor-substrate) &rest rest)
  (declare (ignore rest))
  (setf (medium-text-style (sheet-medium pane))
        (slot-value pane 'text-style)))

;; Is there really a benefit to waiting until the first painting to
;; create the goatee instance? Why not use INITIALIZE-INSTANCE?
(defmethod handle-repaint :before ((pane goatee-editor-substrate) region)
  (declare (ignore region))
  (unless (area pane)
    (multiple-value-bind (cx cy)
        (stream-cursor-position pane)
      (setf (cursor-visibility (stream-text-cursor pane)) nil)
      (setf (area pane) (make-instance 'goatee:simple-screen-area
                         :area-stream pane
                         :x-position cx
                         :y-position cy
                         :initial-contents (slot-value pane 'value))))
    (stream-add-output-record pane (area pane))))

;;; This implements click-to-focus-keyboard-and-pass-click-through
;;; behaviour.
(defmethod handle-event :before
    ((gadget goatee-editor-substrate) (event pointer-button-press-event))
  (let ((previous (stream-set-input-focus gadget)))
    (when (and previous (typep previous 'gadget))
      (disarmed-callback previous (gadget-client previous) (gadget-id previous)))
    (armed-callback gadget (gadget-client gadget) (gadget-id gadget))))

(defmethod armed-callback :after ((gadget goatee-editor-substrate) client id)
  (declare (ignore client id))
  (handle-repaint gadget +everywhere+)	;FIXME: trigger initialization
  (let ((cursor (cursor (area gadget))))
    (letf (((cursor-state cursor) nil))
      (setf (cursor-appearance cursor) :solid))))

(defmethod disarmed-callback :after ((gadget goatee-editor-substrate) client id)
  (declare (ignore client id))
  (handle-repaint gadget +everywhere+)	;FIXME: trigger initialization
  (let ((cursor (cursor (area gadget))))
    (letf (((cursor-state cursor) nil))
      (setf (cursor-appearance cursor) :hollow))))

(defmethod handle-event
    ((gadget goatee-editor-substrate) (event key-press-event))
  (let ((gesture (convert-to-gesture event))
	(*activation-gestures* (activation-gestures gadget)))
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

(defmethod (setf gadget-value) :after (new-value (gadget goatee-editor-substrate)
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
(defmethod handle-repaint ((pane goatee-editor-substrate) region)
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

(defclass goatee-text-field-substrate (text-field-substrate-mixin
                                       goatee-editor-substrate)
  ()
  (:documentation "The class for Goatee-based text field substrates."))

(defmethod compose-space ((pane goatee-text-field-substrate) &key width height)
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

(defclass goatee-text-editor-substrate (text-editor-substrate-mixin
                                       goatee-editor-substrate)
  ()
  (:documentation "The class for Goatee-based text field substrates."))

(defmethod compose-space ((pane goatee-text-editor-substrate) &key width height)
  (with-sheet-medium (medium pane)
    (let* ((text-style (medium-text-style medium))
           (line-height (+ (text-style-height text-style medium)
                           (stream-vertical-spacing pane)))
           (column-width (text-style-width text-style medium)))
      (with-accessors ((ncolumns text-editor-ncolumns)
                       (nlines text-editor-nlines)) pane
        (apply #'space-requirement-combine* #'(lambda (req1 req2)
                                                (or req2 req1))
               (call-next-method)
               (let ((width (if ncolumns
                                (+ (* ncolumns column-width))
                                width))
                     (height (if nlines
                                 (+ (* nlines line-height))
                                 height)))
                 (list :width width :max-width width :min-width width
                       :height height :max-height height :min-height height)))))))

(defmethod allocate-space ((pane goatee-text-editor-substrate) w h)
  (resize-sheet pane w h))

(defun make-text-field-substrate (user &rest args)
  "Create an appropriate text field gadget editing substrate object."
  (let* ((substrate (apply #'make-pane (if *use-goatee*
                                           'goatee-text-field-substrate
                                           'drei-text-field-substrate)
                           :user-gadget user args))
         (sheet substrate))
    (values substrate sheet)))

(defun make-text-editor-substrate (user &rest args &key scroll-bars value
                                   &allow-other-keys)
  "Create an appropriate text editor gadget editing substrate
object. Returns two values, the first is the substrate object,
the second is the sheet that should be adopted by the user
gadget."
  (let* ((minibuffer (when (and (not *use-goatee*) scroll-bars)
                       (make-pane 'drei::drei-minibuffer-pane)))
         (substrate (apply #'make-pane (if *use-goatee*
                                           'goatee-text-editor-substrate
                                           'drei-text-editor-substrate)
                     :user-gadget user
                     :minibuffer minibuffer args))
         (sheet (if scroll-bars
                    (scrolling (:scroll-bars scroll-bars)
                      substrate)
                    substrate)))
    (if *use-goatee*
        (setf (slot-value substrate 'value) value)
        (setf (gadget-value substrate) value))
    (values substrate (if minibuffer
                          (vertically ()
                            sheet
                            minibuffer)
                          sheet))))

;;; The class for using these substrates in the gadgets.

(defclass editor-substrate-user-mixin (value-gadget)
  ((substrate :accessor substrate
              :documentation "The editing substrate used for this
text field."))
  (:documentation "A mixin class for creating gadgets using
editor substrates."))

(defmethod gadget-value ((gadget editor-substrate-user-mixin))
  (gadget-value (substrate gadget)))

(defmethod (setf gadget-value) (value (gadget editor-substrate-user-mixin)
                                &key invoke-callback)
  (declare (ignore invoke-callback))
  (setf (gadget-value (substrate gadget)) value))

;;; ------------------------------------------------------------------------------------------
;;;  30.4.8 The concrete text-field Gadget

(defclass text-field-pane (text-field
                           vrack-pane editor-substrate-user-mixin)
  ((activation-gestures :accessor activation-gestures
			:initarg :activation-gestures
			:documentation "A list of gestures that
cause the activate callback to be called."))
  (:default-initargs
   :activation-gestures *standard-activation-gestures*))

(defmethod initialize-instance :after ((object text-field-pane)
                                       &key id client armed-callback
                                       disarmed-callback
                                       activation-gestures activate-callback
                                       value value-changed-callback)
  ;; Make an editor substrate object for the gadget.
  (let ((substrate (make-text-field-substrate
                    object :id id :client client :armed-callback armed-callback
                    :disarmed-callback disarmed-callback
                    :activation-gestures activation-gestures
                    :activate-callback activate-callback
                    :value value
                    :value-changed-callback value-changed-callback)))
    (setf (substrate object) substrate)
    (sheet-adopt-child object substrate)))

;;; ------------------------------------------------------------------------------------------
;;;  30.4.9 The concrete text-editor Gadget

(defclass text-editor-pane (text-editor
                            vrack-pane editor-substrate-user-mixin)
  ()
  (:default-initargs :activation-gestures '()))

(defmethod initialize-instance :after ((object text-editor-pane)
                                       &key id client armed-callback
                                       disarmed-callback
                                       activation-gestures scroll-bars
                                       ncolumns nlines value)
  ;; Make an editor substrate object for the gadget.
  (multiple-value-bind (substrate sheet)
      (make-text-editor-substrate object
       :id id :client client :armed-callback armed-callback
       :disarmed-callback disarmed-callback
       :activation-gestures activation-gestures
       :scroll-bars scroll-bars
       :ncolumns ncolumns :nlines nlines
       :value value)
    (setf (substrate object) substrate)
    (sheet-adopt-child object sheet)))
