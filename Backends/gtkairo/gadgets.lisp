;;; -*- Mode: Lisp; -*-

;;;  (c) 2006 David Lichteblau (david@lichteblau.com)

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

(in-package :clim-gtkairo)

(defclass gadget-event (window-event) ())
(defclass magic-gadget-event (gadget-event) ())

(defclass scrollbar-change-value-event (gadget-event)
    ((scroll-type :initarg :scroll-type :accessor event-scroll-type)
     (value :initarg :value :accessor event-value)))

(defclass menu-clicked-event (gadget-event)
    ((item :initarg :item :accessor event-item)))

(defclass context-menu-clicked-event (gadget-event)
    ((value :initarg :value :accessor event-value)
     (itemspec :initarg :itemspec :accessor event-itemspec)))

(defclass context-menu-cancelled-event (gadget-event) ())


;;;; Classes

;; gtk-menu-* see port.lisp

(defclass gtk-button (native-widget-mixin push-button) ())

(defclass gtk-check-button (native-widget-mixin toggle-button) ())
(defclass gtk-radio-button (native-widget-mixin toggle-button) ())

(defclass native-slider (native-widget-mixin climi::slider-gadget)
    ((climi::show-value-p :type boolean
			  :initform nil
			  :initarg :show-value-p
			  :accessor climi::gadget-show-value-p)
     (climi::decimal-places :initform 0
			    :initarg :decimal-places
			    :reader climi::slider-decimal-places)
     (climi::number-of-quanta :initform nil
			      :initarg :number-of-quanta
			      :reader climi::slider-number-of-quanta)))
(defclass gtk-vscale (native-slider) ())
(defclass gtk-hscale (native-slider) ())

(defclass native-scrollbar (native-widget-mixin scroll-bar) ())
(defclass gtk-vscrollbar (native-scrollbar) ())
(defclass gtk-hscrollbar (native-scrollbar) ())


;;;; Constructors

(defmethod realize-native-widget ((sheet gtk-button))
  (let ((button (gtk_button_new_with_label (climi::gadget-label sheet))))
    (when (pane-background sheet)
      (gtk-widget-modify-bg button (pane-background sheet)))
    button))

(defmethod realize-native-widget ((sheet gtk-menu-bar))
  (gtk_menu_bar_new))

(defmethod realize-native-widget ((sheet gtk-check-button))
  (let ((widget (gtk_check_button_new_with_label (climi::gadget-label sheet))))
    (gtk_toggle_button_set_active widget (if (gadget-value sheet) 1 0))
    widget))

(defun make-scale (fn sheet)
  (let* ((min (df (gadget-min-value sheet)))
	 (max (df (gadget-max-value sheet)))
	 (n (or (climi::slider-number-of-quanta sheet) 100))
	 (widget (funcall fn min max (/ (- max min) n))))
    (gtk_scale_set_digits widget (climi::slider-decimal-places sheet))
    (gtk_scale_set_draw_value widget
			      (if (climi::gadget-show-value-p sheet) 1 0))
    (gtk_adjustment_set_value (gtk_range_get_adjustment widget)
			      (df (gadget-value sheet)))
    widget))

(defmethod realize-native-widget ((sheet gtk-vscale))
  (make-scale #'gtk_vscale_new_with_range sheet))

(defmethod realize-native-widget ((sheet gtk-hscale))
  (make-scale #'gtk_hscale_new_with_range sheet))

(defun make-scrollbar (fn sheet)
  (let* ((min (df (gadget-min-value sheet)))
	 (page-size (df (climi::scroll-bar-thumb-size sheet)))
	 (max (+ (df (gadget-max-value sheet)) page-size))
	 (adjustment (gtk_adjustment_new 0.0d0 min max 0.0d0 0.0d0 page-size)))
    (gtk_adjustment_set_value adjustment (df (gadget-value sheet)))
    (funcall fn adjustment)))

(defmethod realize-native-widget ((sheet gtk-vscrollbar))
  (make-scrollbar #'gtk_vscrollbar_new sheet))

(defmethod realize-native-widget ((sheet gtk-hscrollbar))
  (make-scrollbar #'gtk_hscrollbar_new sheet))

(defmethod realize-native-widget ((sheet gtk-radio-button))
  (let* ((first
	  (some #'sheet-direct-mirror (sheet-children (gadget-client sheet))))
	 (group (if first
		    (gtk_radio_button_get_group (mirror-widget first))
		    (cffi:null-pointer)))
	 (result
	  (gtk_radio_button_new_with_label group (climi::gadget-label sheet))))
    (gtk_toggle_button_set_active
     result
     (if (eq sheet (gadget-value (gadget-client sheet))) 1 0))
    result))

(defun append-menu-items (port sheet menu command-table-name)
  (let ((ct (find-command-table command-table-name)))
    (dolist (menu-item (slot-value ct 'climi::menu))
      (let ((item (make-native-menu-item port sheet menu-item)))
	(gtk_menu_shell_append menu item)))))

(defun make-native-menu-item (port sheet menu-item)
  (ecase (command-menu-item-type menu-item)
    (:divider
      (gtk_separator_menu_item_new))
    (:command
      (let ((item
	     (gtk_menu_item_new_with_label
	      (climi::command-menu-item-name menu-item))))
	;; naja, ein sheet ist das nicht
	(setf (widget->sheet item port) menu-item)
	(connect-signal item "activate" 'menu-clicked-handler)
	item))
    (:menu
      (let ((item
	     (gtk_menu_item_new_with_label
	      (climi::command-menu-item-name menu-item)))
	    (menu (gtk_menu_new)))
	(setf (widget->sheet item port) sheet)
	(setf (widget->sheet menu port) sheet)
	(append-menu-items port sheet menu (command-menu-item-value menu-item))
	(gtk_menu_item_set_submenu item menu)
	item))))

(defun destructure-mc-menu-item (x)
  (cond
    ((atom x)
      (values :item x x nil))
    ((atom (cdr x))
      (values :item (car x) (cdr x) nil))
    (t
      (destructuring-bind
	  (&key value style items documentation active type)
	  (cdr x)
	(declare (ignore style documentation active))
	(values (cond (items :menu) (type) (t :item))
		(car x)
		(or value (car x))
		items)))))

;;(defclass dummy-context-menu-sheet (climi::clim-sheet-input-mixin sheet) ())

(defclass dummy-context-menu-sheet (climi::standard-sheet-input-mixin sheet)
    ())

(defclass dummy-menu-item-sheet (sheet)
    ((parent :initarg :parent :accessor dummy-menu-item-sheet-parent)
     (value :initarg :value :accessor dummy-menu-item-sheet-value)
     (itemspec :initarg :itemspec :accessor dummy-menu-item-sheet-itemspec)))

(defun make-context-menu (port sheet items)
  (let ((menu (gtk_menu_new)))
    (dolist (itemspec items)
      (multiple-value-bind (type display-object value sub-items)
	  (destructure-mc-menu-item itemspec)
	(let* ((label (princ-to-string display-object))
	       (gtkmenuitem
		(ecase type
		  (:divider
		    (gtk_separator_menu_item_new))
		  (:label
		    (gtk_menu_item_new_with_label label))
		  (:item
		    (let ((item
			   (gtk_menu_item_new_with_label label)))
		      (setf (widget->sheet item port)
			    (make-instance 'dummy-menu-item-sheet
			      :parent sheet
			      :value value
			      :itemspec itemspec))
		      (connect-signal item
				      "activate"
				      'context-menu-clicked-handler)
		      item))
		  (:menu
		    (let ((item (gtk_menu_item_new_with_label label))
			  (menu (make-context-menu port sheet sub-items)))
		      (gtk_menu_item_set_submenu item menu)
		      item)))))
	  (gtk_menu_shell_append menu gtkmenuitem))))
    (setf (widget->sheet menu port) sheet)
    (connect-signal menu "deactivate" 'popup-deactivated-handler)
    (gtk_widget_show_all menu)
    menu))


;;;; Event definition

(defmethod connect-native-signals ((sheet native-widget-mixin) widget)
  (connect-signal widget "clicked" 'magic-clicked-handler))

(defmethod connect-native-signals ((sheet native-slider) widget)
  (connect-signal widget "value-changed" 'magic-clicked-handler))

(defmethod connect-native-signals ((sheet native-scrollbar) widget)
  ;; (connect-signal widget "value-changed" 'magic-clicked-handler)
  (connect-signal widget "change-value" 'scrollbar-change-value-handler))

(defmethod connect-native-signals ((sheet gtk-menu-bar) widget)
  ;; no signals
  )


;;;; Event handling

(defmethod handle-event ((pane gtk-button) (event magic-gadget-event))
  (activate-callback pane (gadget-client pane) (gadget-id pane)))

(defmethod handle-event ((pane gtk-check-button) (event magic-gadget-event))
  (setf (gadget-value pane :invoke-callback t) (not (gadget-value pane))))

(defmethod handle-event ((pane gtk-radio-button) (event magic-gadget-event))
  (setf (gadget-value pane :invoke-callback t) (not (gadget-value pane))))

(defmethod handle-event ((pane native-slider) (event magic-gadget-event))
  (setf (gadget-value pane :invoke-callback t)
	(gtk_range_get_value (mirror-widget (sheet-direct-mirror pane)))))

(defmethod handle-event ((pane native-scrollbar) (event magic-gadget-event))
  (setf (gadget-value pane :invoke-callback t)
	(gtk_range_get_value (mirror-widget (sheet-direct-mirror pane)))))

(defun clamp (low x hi)
  (min (max low x) hi))

(defmethod handle-event
    ((pane native-scrollbar) (event scrollbar-change-value-event))
  (case (event-scroll-type event)
    (:jump
      (let ((value
	     (clamp (gadget-min-value pane)
		    (event-value event)
		    (gadget-max-value pane))))
	(setf (gadget-value pane :invoke-callback nil) value)
	(drag-callback pane (gadget-client pane) (gadget-id pane) value)))
    (:step_backward
      (scroll-up-line-callback pane (gadget-client pane) (gadget-id pane)))
    (:step_forward
      (scroll-down-line-callback pane (gadget-client pane) (gadget-id pane)))
    (:page_backward
      (scroll-up-page-callback pane (gadget-client pane) (gadget-id pane)))
    (:page_forward
      (scroll-down-page-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event
    ((pane gtk-menu) (event menu-clicked-event))
  (let ((item (event-item event)))
    (ecase (command-menu-item-type item)
      (:command
	(climi::throw-object-ptype item 'menu-item)))))

(defmethod handle-event
    ((pane gtk-nonmenu) (event magic-gadget-event))
  (funcall (gtk-nonmenu-callback pane) pane nil))


;;; COMPOSE-SPACE

;; KLUDGE: this is getting called before the sheet has been realized.
(defmethod compose-space ((gadget native-widget-mixin) &key width height)
  (declare (ignore width height))
  (let* ((widget (native-widget gadget))
	 (widgetp widget))
    (unless widgetp
      (setf widget (realize-native-widget gadget)))
    (prog1
	(cffi:with-foreign-object (r 'gtkrequisition)
	  (gtk_widget_size_request widget r)
	  (cffi:with-foreign-slots ((width height) r gtkrequisition)
	    (make-space-requirement :width width :height height)))
      (unless widgetp
	(gtk_widget_destroy widget)))))

(defmethod compose-space ((gadget gtk-menu-bar) &key width height)
  (declare (ignore width height))
  (let* ((widget (native-widget gadget))
	 (widgetp widget)
	 (item nil))
    (unless widgetp
      (setf widget (realize-native-widget gadget))
      (setf item (gtk_menu_item_new_with_label "foo"))
      (gtk_menu_shell_append widget item)
      (gtk_widget_show_all widget))
    (prog1
	(cffi:with-foreign-object (r 'gtkrequisition)
	  (gtk_widget_size_request widget r)
	  (cffi:with-foreign-slots ((height) r gtkrequisition)
	    (make-space-requirement :height height
				    :min-height height
				    :max-height height)))
      (unless widgetp
	(gtk_widget_destroy widget)))))


;;; Vermischtes

(defmethod (setf gadget-value) :after
    (value (gadget native-slider) &key invoke-callback)
  (declare (ignore invoke-callback))
  (with-gtk ()
    (let ((mirror (sheet-direct-mirror gadget)))
      (when mirror
	;; see hack in magic-clicked-handler
	(gtk_adjustment_set_value
	 (gtk_range_get_adjustment (mirror-widget mirror))
	 (df value))))))

(defmethod (setf gadget-value) :after
    (value (gadget gtk-radio-button) &key invoke-callback)
  (declare (ignore invoke-callback))
  (with-gtk ()
    (let ((mirror (sheet-direct-mirror gadget)))
      (when mirror
	;; see hack in magic-clicked-handler
	(gtk_toggle_button_set_active (mirror-widget mirror)
				      (if value 1 0))))))

(defmethod (setf gadget-value) :after
    (value (gadget gtk-check-button) &key invoke-callback)
  (declare (ignore invoke-callback))
  (with-gtk ()
    (let ((mirror (sheet-direct-mirror gadget)))
      (when mirror
	;; see hack in magic-clicked-handler
	(gtk_toggle_button_set_active (mirror-widget mirror)
				      (if value 1 0))))))


;;; Scroll bars.

;; This is all totally broken.  Why does thumb-size default to 1/4 when it's
;; not a ratio but given in value units?  Why is min==max all the time?
;; And why doesn't this work! :-(
(defun update-scrollbar-adjustment (sheet)
  (with-gtk ()
    (let* ((min (df (gadget-min-value sheet)))
	   (value (df (gadget-value sheet)))
	   (page-size (df (climi::scroll-bar-thumb-size sheet)))
	   (max (+ (df (gadget-max-value sheet)) page-size)))
      (gtk_range_set_adjustment
       (mirror-widget (sheet-direct-mirror sheet))
       (gtk_adjustment_new value min max 0.0d0 0.0d0 page-size)))))

(defmethod (setf gadget-min-value) :after (new-value (pane native-scrollbar))
  (declare (ignore new-value))
  (update-scrollbar-adjustment pane))

(defmethod (setf gadget-max-value) :after (new-value (pane native-scrollbar))
  (declare (ignore new-value))
  (update-scrollbar-adjustment pane))

(defmethod (setf gadget-value)
    :after (new-value (pane native-scrollbar) &key invoke-callback)
  (declare (ignore new-value invoke-callback))
  (update-scrollbar-adjustment pane))

(climi::defmethod* (setf climi::scroll-bar-values)
    (min-value max-value thumb-size value (scroll-bar native-scrollbar))
  (setf (slot-value scroll-bar 'climi::min-value) min-value
	(slot-value scroll-bar 'climi::max-value) max-value
	(slot-value scroll-bar 'climi::thumb-size) thumb-size
	(slot-value scroll-bar 'climi::value) value)
  (update-scrollbar-adjustment scroll-bar))

(defmethod port-set-mirror-region :after
    ((port gtkairo-port) (mirror native-scrollbar) mirror-region)
  (update-scrollbar-adjustment (widget->sheet (mirror-widget mirror) port)))
