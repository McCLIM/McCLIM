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
;;; requires Drei.

(in-package :clim-internals)

;;; ------------------------------------------------------------------------------------------
;;;  30.4.8 The concrete text-field Gadget

(defclass text-field-pane (text-field
                           drei:drei-gadget-pane)
  ((previous-focus :accessor previous-focus :initform nil
		   :documentation
		   "The pane that previously had keyboard focus")
   (activation-gestures :accessor activation-gestures
			:initarg :activation-gestures
			:documentation "gestures that cause the
activate callback to be called"))
  (:default-initargs
   :activation-gestures *standard-activation-gestures*))

(defmethod initialize-instance :after ((object text-field-pane) &key value)
  ;; Why doesn't `value-gadget' do this for us?
  (setf (gadget-value object) value))

(defmethod compose-space ((pane text-field-pane) &key width height)
  (declare (ignore width height))
  (with-sheet-medium (medium pane)
    (let ((as (text-style-ascent (medium-text-style medium) medium))
          (ds (text-style-descent (medium-text-style medium) medium))
          (w  (text-size medium (gadget-value pane))))
      (let ((width w)
            (height (+ as ds)))
        (make-space-requirement :height height :max-height height :min-height height
                                :min-width width :width width)))))

(defmethod allocate-space ((pane text-field-pane) w h)
  (resize-sheet pane w h))

;;; ------------------------------------------------------------------------------------------
;;;  30.4.9 The concrete text-editor Gadget

(defclass text-editor-pane (text-editor drei:drei-gadget-pane)
  ((ncolumns :type (or null integer)
             :initarg :ncolumns
             :initform nil
             :accessor text-editor-ncolumns)
   (nlines :type (or null integer)
	   :initarg :nlines
	   :initform nil
           :accessor text-editor-nlines))
  (:default-initargs :activation-gestures nil))

(defmethod initialize-instance :after ((object text-editor-pane) &key value)
  ;; Why doesn't `value-gadget' do this for us?
  (setf (gadget-value object) value))

(defmethod make-pane-1 :around (fm (frame application-frame)
                                   (type (eql :text-editor))
                                   &rest args &key)
  (apply #'make-pane-1 fm frame :drei
         :drei-class 'text-editor-pane
         :minibuffer t
         args))

(defmethod compose-space ((pane text-editor-pane) &key width height)
  (declare (ignore width height))
  (with-sheet-medium (medium pane)
    (let* ((text-style (medium-text-style medium))
           (tr-height (text-style-height text-style medium))
           (tr-width (text-style-width text-style medium))
           (padding (- (bounding-rectangle-width pane)
                       (stream-text-margin pane))))
      (with-accessors ((ncolumns text-editor-ncolumns)
                       (nlines text-editor-nlines)) pane
        (apply #'make-space-requirement
               (append (when ncolumns
                         (let ((width (max (+ (* ncolumns tr-width))
                                           (bounding-rectangle-width (stream-current-output-record  pane)))))
                           (list :width width :max-width width :min-width width)))
                       (when nlines
                         (let ((height (+ (* nlines tr-height) (* 2 padding)
                                          (stream-vertical-spacing pane))))
                           (list :height height :max-height height :min-height height)))))))))

(defmethod allocate-space ((pane text-editor-pane) w h)
  (resize-sheet pane w h))

;;; ------------------------------------------------------------------------------------------
;;;  30.4.9 Alternative Goatee-based implementation

(defparameter *default-text-field-text-style*
    (make-text-style :fixed :roman :normal))

(defclass goatee-text-field-pane (text-field
			   standard-extended-output-stream
			   standard-output-recording-stream
			   enter/exit-arms/disarms-mixin
			   basic-pane)
  ((area :accessor area :initform nil
	 :documentation "The Goatee area used for text editing.")
   (previous-focus :accessor previous-focus :initform nil
		   :documentation
		   "The pane that previously had keyboard focus")
   (activation-gestures :accessor activation-gestures
			:initarg :activation-gestures
			:documentation "gestures that cause the
			   activate callback to be called"))
  (:default-initargs
    :text-style *default-text-field-text-style*
    :activation-gestures *standard-activation-gestures*))

(defmethod initialize-instance :after ((gadget text-field) &rest rest)
  (unless (getf rest :normal)
    (setf (slot-value gadget 'current-color) +white+
	  (slot-value gadget 'normal) +white+)))

(defmethod initialize-instance :after ((pane goatee-text-field-pane) &rest rest)
  (declare (ignore rest))
  #-nil (setf (medium-text-style (sheet-medium pane))
	      (slot-value pane 'text-style)))

;; Is there really a benefit to waiting until the first painting to
;; create the goatee instance? Why not use INITIALIZE-INSTANCE?
(defmethod handle-repaint :before ((pane goatee-text-field-pane) region)
  (declare (ignore region))
  (unless (area pane)
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

(defmethod armed-callback :after ((gadget goatee-text-field-pane) client id)
  (declare (ignore client id))
  (let ((port (port gadget)))
    (setf (previous-focus gadget) (port-keyboard-input-focus port))
    (setf (port-keyboard-input-focus port) gadget))
  (handle-repaint gadget +everywhere+)	;FIXME: trigger initialization
  (let ((cursor (cursor (area gadget))))
    (letf (((cursor-state cursor) nil))
      (setf (cursor-appearance cursor) :solid))))

(defmethod disarmed-callback :after ((gadget goatee-text-field-pane) client id)
  (declare (ignore client id))
  (let ((port (port gadget)))
    (setf (port-keyboard-input-focus port) (previous-focus gadget))
    (setf (previous-focus gadget) nil))
  (handle-repaint gadget +everywhere+)	;FIXME: trigger initialization
  (let ((cursor (cursor (area gadget))))
    (letf (((cursor-state cursor) nil))
      (setf (cursor-appearance cursor) :hollow))))


(defmethod handle-event ((gadget goatee-text-field-pane) (event key-press-event))
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

(defmethod (setf gadget-value) :after (new-value (gadget goatee-text-field-pane)
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
(defmethod handle-repaint ((pane goatee-text-field-pane) region)
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


(defmethod compose-space ((pane goatee-text-field-pane) &key width height)
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

(defmethod allocate-space ((pane goatee-text-field-pane) w h)
  (resize-sheet pane w h))

(defclass goatee-text-editor-pane (goatee-text-field-pane)  
  ((width :type integer                       
	  :initarg :width                      
	  :initform 300                        
	  :reader text-editor-width)           
   (height :type integer                      
	   :initarg :height                    
	   :initform 300                       
	   :reader text-editor-height))        
  (:default-initargs :activation-gestures nil))
                                              
(defmethod compose-space ((pane goatee-text-editor-pane) &key width height)
  (declare (ignore width height))             
  (let ((width (text-editor-width pane))      
	(height (text-editor-height pane)))    
  (make-space-requirement :width width        
			  :min-width width     
			  :max-width width     
			  :height height       
			  :min-height height   
			  :max-height height)))

;;; ------------------------------------------------------------------------------------------
;;;  Work some magic to make the interface convenient and implement
;;;  Drei/Goatee selection.

(defmethod make-pane-1 :around (fm (frame application-frame)
                                   (type (eql :text-field))
                                   &rest args &key)
  (if *use-goatee*
      (apply #'make-pane-1 fm frame 'goatee-text-field-pane args)
      (apply #'make-pane-1 fm frame 'text-field-pane args)))

(defmethod make-pane-1 :around (fm (frame application-frame)
                                   (type (eql :text-editor))
                                   &rest args &key)
  (if *use-goatee*
      (apply #'make-pane-1 fm frame 'goatee-text-editor-pane args)
      (apply #'make-pane-1 fm frame :drei
             :drei-class 'text-editor-pane
             :minibuffer t
             args)))
