;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2000 by 
;;; Arthur Lemmens (lemmens@simplex.nl),
;;; Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;; and Julien Boninfante (boninfan@emi.u-bordeaux.fr)

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


;;;
;;; Gadget
;;;

(defclass gadget (immediate-sheet-input-mixin pane immediate-repaint-mixin)
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
   (active-p;; [Arthur] I'm not so sure about the value for :initform.
    ;; Maybe T is better? Or maybe we should call
    ;; ACTIVATE-GADGET after creating a gadget?
    :initform t
    :reader gadget-active-p)
   (armed :initform nil) ))

(defun gadgetp (object)
  (typep object 'gadget))

;;
;; gadget's colors
;;

(defclass gadget-color-mixin ()
  ((normal :type color
	   :initform +gray50+
	   :initarg :normal
	   :accessor gadget-normal-color)
   (highlighted :type color
		:initform +gray75+
		:initarg :highlighted
		:accessor gadget-highlighted-color)
   (pushed-and-highlighted :type color
			   :initform +gray25+
			   :initarg :pushed-and-highlighted
			   :accessor gadget-pushed-and-highlighted-color)
   (current-color :type color
		  :accessor gadget-current-color))
  (:documentation "This class define the gadgets colors."))

(defmethod initialize-instance :after ((gadget gadget-color-mixin) &rest args)
  (declare (ignore args))
  (setf (gadget-current-color gadget) (gadget-normal-color gadget)))

(defmethod define-gadget-color ((gadget gadget-color-mixin) x1 y1 x2 y2)
  (draw-rectangle* gadget x1 y1 x2 y2 :ink (gadget-current-color gadget) :filled t))


;;
;; gadget sub-classes
;;

(defclass basic-gadget (gadget gadget-color-mixin)
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
  (declare (ignore client gadget-id))
  (let ((callback (gadget-armed-callback gadget)))
    (when callback
      (funcall callback gadget))))

(defmethod disarmed-callback ((gadget standard-gadget) client gadget-id)
  (declare (ignore client gadget-id))
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
  (declare (ignore client gadget-id))
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
	  :initform "No label"
          :accessor gadget-label)
   (align-x :initarg :align-x
            :accessor gadget-label-align-x)
   (align-y :initarg :align-y
            :accessor gadget-label-align-y)
   (label-text-style :initform nil
		     :initarg :label-text-style
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


;;
;; PUSH-BUTTON gadget
;;

(defclass push-button (action-gadget labelled-gadget-mixin) ())
  
(defclass push-button-pane  (push-button clim-stream-pane)
  ((show-as-default-p :type (member '(t nil))
		      :initform nil
		      :initarg :show-as-default-p
		      :accessor push-button-show-as-default-p)))

(defclass clx-push-button-pane (push-button-pane) ())

(defmethod handle-event ((pane push-button-pane) (event pointer-enter-event))
  (with-slots (armed) pane
    (unless armed
      (setf armed t)
      (setf (gadget-current-color pane) (gadget-highlighted-color pane))
      (dispatch-repaint pane (sheet-region pane))
      (armed-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event ((pane push-button-pane) (event pointer-exit-event))
  (with-slots (armed) pane
    (when armed
      (setf (push-button-show-as-default-p pane) nil)
      (setf armed nil)
      (setf (gadget-current-color pane) (gadget-normal-color pane))
      (dispatch-repaint pane (sheet-region pane))
      (disarmed-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event ((pane push-button-pane) (event pointer-button-press-event))
  (with-slots (armed) pane
    (unless armed
      (armed-callback pane (gadget-client pane) (gadget-id pane)))
    (setf armed ':button-press)
    (setf (push-button-show-as-default-p pane) t)
    (setf (gadget-current-color pane) (gadget-pushed-and-highlighted-color pane))
    (dispatch-repaint pane (sheet-region pane))))

(defmethod handle-event ((pane push-button-pane) (event pointer-button-release-event))
  (with-slots (armed) pane
    (when (eql armed ':button-press)
      (activate-callback pane (gadget-client pane) (gadget-id pane))
      (setf armed t)
      (setf (push-button-show-as-default-p pane) nil)
      (setf (gadget-current-color pane) (gadget-highlighted-color pane))
      (dispatch-repaint pane (sheet-region pane))
      (disarmed-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event ((pane push-button-pane) (event window-repaint-event))
  (dispatch-repaint pane (sheet-region pane)))

(defmethod repaint-sheet ((pane push-button-pane) region)
  (declare (ignore region))
  (let ((text (gadget-label pane))
	(region (sheet-region pane)))
    (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
      (setf x2 (- x2 x1 1)
	    x1 1
	    y2 (- y2 y1 1)
	    y1 1)
      (define-gadget-color pane x1 y1 x2 y2)
      (if (push-button-show-as-default-p pane)
	  (progn
	    (draw-line* pane x1 y1 x2 y1 :ink +black+)
	    (draw-line* pane x1 y1 x1 y2 :ink +black+)
	    (draw-line* pane x1 y2 x2 y2 :ink +white+)
	    (draw-line* pane x2 y1 x2 y2 :ink +white+))
	  (progn
	    (draw-line* pane x1 y1 x2 y1 :ink +white+)
	    (draw-line* pane x1 y1 x1 y2 :ink +white+)
	    (draw-line* pane x1 y2 x2 y2 :ink +black+)
	    (draw-line* pane x2 y1 x2 y2 :ink +black+)))
      (draw-text* pane text
		  (round (- x2 x1) 2)
		  (round (- y2 y1) 2)
		  :align-x :center
		  :align-y :center))))


;;
;; TOGGLE-BUTTON gadget
;;

(defclass toggle-button (value-gadget labelled-gadget-mixin) ()
  (:documentation "The value is either t either nil"))

(defclass toggle-button-pane (toggle-button clim-stream-pane)
  ((indicator-type :type (member '(:one-of :some-of))
		   :initarg :indicator-type
		   :reader toggle-button-indicator-type)))
; We don't have implemented the difference of appearence whether the 
; indicator-type is :one-of or :some-of

(defclass clx-toggle-button-pane (toggle-button-pane) ())

(defmethod handle-event ((pane toggle-button-pane) (event pointer-enter-event))
  (with-slots (armed) pane
    (unless armed
      (setf armed t)
      (unless (gadget-value pane)
	(setf (gadget-current-color pane) (gadget-highlighted-color pane))
	(dispatch-repaint pane (sheet-region pane)))
      (armed-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event ((pane toggle-button-pane) (event pointer-exit-event))
  (with-slots (armed) pane
    (when armed
      (setf armed nil)
      (if (gadget-value pane)
	  (setf (gadget-current-color pane) (gadget-pushed-and-highlighted-color pane))
	  (setf (gadget-current-color pane) (gadget-normal-color pane)))
      (dispatch-repaint pane (sheet-region pane))
      (disarmed-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event ((pane toggle-button-pane) (event pointer-button-press-event))
  (with-slots (armed) pane
    (unless armed
      (armed-callback pane (gadget-client pane) (gadget-id pane)))
    (setf armed ':button-press)
    (setf (gadget-current-color pane) (gadget-pushed-and-highlighted-color pane))
    (dispatch-repaint pane (sheet-region pane))))     
      
(defmethod handle-event ((pane toggle-button-pane) (event pointer-button-release-event))
  (with-slots (armed) pane
    (when (eql armed ':button-press)
      (setf (gadget-value pane :invoke-callback t) (not (gadget-value pane)))
      (setf armed t)
      (unless (gadget-value pane)
	(setf (gadget-current-color pane) (gadget-highlighted-color pane))
	(dispatch-repaint pane (sheet-region pane)))
      (disarmed-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event ((pane toggle-button-pane) (event window-repaint-event))
  (dispatch-repaint pane (sheet-region pane)))

(defmethod repaint-sheet ((pane toggle-button-pane) region)
  (declare (ignore region))
  (let ((text (gadget-label pane))
	(region (sheet-region pane))
	(armed (slot-value pane 'armed)))
    (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
      (setf x2 (- x2 x1 1)
	    x1 1
	    y2 (- y2 y1 1)
	    y1 1)
      (define-gadget-color pane x1 y1 x2 y2)
      (if (or (gadget-value pane) (eql armed ':button-press))
	  (progn
	    (draw-line* pane x1 y1 x2 y1 :ink +black+)
	    (draw-line* pane x1 y1 x1 y2 :ink +black+)
	    (draw-line* pane x1 y2 x2 y2 :ink +white+)
	       (draw-line* pane x2 y1 x2 y2 :ink +white+))
	  (progn
	    (draw-line* pane x1 y1 x2 y1 :ink +white+)
	    (draw-line* pane x1 y1 x1 y2 :ink +white+)
	    (draw-line* pane x1 y2 x2 y2 :ink +black+)
	    (draw-line* pane x2 y1 x2 y2 :ink +black+)))
      (draw-text* pane text
		     (round (- x2 x1) 2)
		     (round (- y2 y1) 2)
		     :align-x :center
		     :align-y :center))))


;;
;; MENU-BUTTON gadget
;;

(defclass menu-button (value-gadget labelled-gadget-mixin) ()
  (:documentation "The value is a button"))

(defclass menu-button-pane (menu-button clim-stream-pane) ())

(defclass clx-menu-button-pane (menu-button-pane) ())

(defmethod compute-space ((pane menu-button-pane))
  (compute-space (gadget-value pane)))

(defmethod handle-event ((pane menu-button-pane) (event pointer-enter-event))
  (with-slots (armed) pane
    (unless armed
      (setf armed t)
      (armed-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event ((pane menu-button-pane) (event pointer-exit-event))
  (with-slots (armed) pane
    (when armed
      (setf armed nil)
      (disarmed-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event ((pane menu-button-pane) (event pointer-button-press-event))
  (with-slots (armed) pane
    (unless armed
      (armed-callback pane (gadget-client pane) (gadget-id pane)))
    (setf armed ':button-press)
    (dispatch-repaint pane (sheet-region pane))))

(defmethod handle-event ((pane menu-button-pane) (event pointer-button-release-event))
  (with-slots (armed) pane
    (when (eql armed ':button-press)
      (value-changed-callback pane (gadget-client pane) (gadget-id pane) t) ;FALSE for the value
      (setf armed t)
      (dispatch-repaint pane (sheet-region pane))
      (disarmed-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event ((pane menu-button-pane) (event window-repaint-event))
  (dispatch-repaint pane (sheet-region pane)))

;;
;; SCROLL-BAR gadget
;;

(defgeneric drag-callback (scroll-bar client gadget-id value))
(defgeneric scroll-to-top-callback (scroll-bar client gadget-id))
(defgeneric scroll-to-bottom-callback (scroll-bar client gadget-id))
(defgeneric scroll-up-line-callback (scroll-bar client gadget-id))
(defgeneric scroll-up-page-callback (scroll-bar client gadget-id))
(defgeneric scroll-down-line-callback (scroll-bar client gadget-id))
(defgeneric scroll-down-page-callback (scroll-bar client gadget-id))

(defclass scroll-bar (value-gadget oriented-gadget-mixin range-gadget-mixin) ()
  (:documentation "The value is a real number"))

(defclass scroll-bar-pane (scroll-bar clim-stream-pane)
  ((drag-callback :initarg :drag-callback
		  :reader scroll-bar-drag-callback)
   (scroll-to-bottom-callback :initarg :scroll-to-bottom-callback
			      :reader scroll-bar-scroll-to-bottom-callback)
   (scroll-to-top-callback :initarg :scroll-to-top-callback
			   :reader scroll-bar-scroll-to-top-callback)
   (drag-down-line-callback :initarg :drag-down-line-callback
			    :reader scroll-bar-drag-down-line-callback)
   (drag-up-line-callback :initarg :drag-up-line-callback
			  :reader scroll-bar-drag-up-line-callback)
   (drag-down-page-callback :initarg :drag-down-page-callback
			    :reader scroll-bar-drag-down-page-callback)
   (drag-up-page-callback :initarg :drag-up-page-callback
			  :reader scroll-bar-drag-up-page-callback)))

(defclass clx-scroll-bar-pane (scroll-bar-pane) ())

(defmethod drag-callback ((pane scroll-bar-pane) client gadget-id value)
  (declare (ignore client gadget-id))
  (funcall (scroll-bar-drag-callback pane) pane value))

(defmethod drag-callback :after ((pane scroll-bar-pane) client gadget-id value)
  (declare (ignore client gadget-id))
  (setf (gadget-value pane :invoke-callback t) value))

(defmacro invoke-callbacks (pane callback)
  (let ((call (gensym)))
    `(let ((,call (symbol-function ,callback)))
       (when ,call
	 (funcall ,call ,pane)))))

(defmethod scroll-to-top-callback ((pane scroll-bar-pane) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callbacks pane 'scroll-bar-scroll-to-top-callback))

(defmethod scroll-to-bottom-callback ((pane scroll-bar-pane) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callbacks pane 'scroll-bar-scroll-to-bottom-callback))

(defmethod scroll-up-line-callback ((pane scroll-bar-pane) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callbacks pane 'scroll-bar-drag-up-line-callback))

(defmethod scroll-up-page-callback ((pane scroll-bar-pane) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callbacks pane 'scroll-bar-drag-up-page-callback))

(defmethod scroll-down-line-callback ((pane scroll-bar-pane) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callbacks pane 'scroll-bar-drag-down-line-callback))

(defmethod scroll-down-page-callback ((pane scroll-bar-pane) client gadget-id)
  (declare (ignore client gadget-id))
  (invoke-callbacks pane 'scroll-bar-drag-down-page-callback))

;;
;; SLIDER gadget
;;

(defgeneric drag-callback (slider client gadget-id value))

(defclass slider-gadget (value-gadget oriented-gadget-mixin range-gadget-mixin labelled-gadget-mixin) ()
  (:documentation "The value is a real number"))
  
(defclass slider-pane (slider-gadget clim-stream-pane)
  ((drag-callback :initarg :drag-callback
		  :reader slider-drag-callback)
   (show-value-p :type (member '(t nil)) ; don't have understood where is to use ?
		 :initarg :show-value-p
		 :reader gadget-show-value-p)))

(defclass clx-slider-pane (slider-pane) ())

(defmethod drag-callback ((pane slider-pane) client gadget-id value)
  (declare (ignore client gadget-id))
  (funcall (slider-drag-callback pane) pane value))

(defmethod drag-callback :after ((pane slider-pane) client gadget-id value)
  (declare (ignore client gadget-id))
  (setf (gadget-value pane :invoke-callback t) value))


;;
;; RADIO-BOX gadget
;;

(defclass radio-box (value-gadget oriented-gadget-mixin) ()
  (:documentation "The value is a button"))
  
(defclass radio-box-pane (radio-box)
  ((current-selection :type 'toggle-button
		      :initarg :current-selection
		      :accessor radio-box-current-selection)))

(defclass clx-radio-box-pane (radio-box-pane) ())

(defmethod (setf radio-box-current-selection) :before (button (pane radio-box-pane))
  (declare (ignore button))
  (let ((old-button (radio-box-current-selection pane)))
    (setf (gadget-value old-button :invoke-callback t) nil)))

(defmethod (setf radio-box-current-selection) :after (button (pane radio-box-pane))
  (declare (ignorable pane))
  (setf (gadget-value button :invoke-callback t) t))

(defmacro with-radio-box (&rest options &body body)
  `(make-pane 'radio-box ,@options
	      :current-selection (first ,body)
	      ,@body))


;;
;; TEXT-FIELD gadget
;;

(defclass text-field (value-gadget action-gadget) ()
  (:documentation "The value is a string"))

(defclass text-field-pane (text-field clim-stream-pane) ())

(defclass clx-text-field-pane (text-field-pane) ())


;;
;; TEXT-EDITOR gadget
;;

(defclass text-editor (text-field) ()
  (:documentation "The value is a string"))

(defclass text-editor-pane (text-editor clim-stream-pane)
  ((width :type integer
	  :initarg :width
	  :reader text-editor-width)
   (height :type integer
	   :initarg :height
	   :reader text-editor-height)))

(defclass clx-text-editor-pane (text-editor-pane) ())

(defmethod compute-space ((pane text-editor-pane))
  (setf (pane-space-requirement pane)
	(make-space-requirement :width (text-editor-width pane)
				:height (text-editor-height pane))))


;;
;; GADGET-OUTPUT-RECORD
;;

(defclass gadget-output-record (output-record) ())

(defmacro with-output-as-gadget (stream &body body)
  (let ((gadget (gensym))
	(gadget-output-record (gensym)))
    `(let* ((,gadget (progn ,@body))
	    (,gadget-output-record (make-instance 'gadget-output-record 
				    :children (list ,gadget))))
       (stream-add-output-record ,stream ,gadget-output-record)
       ,gadget)))



