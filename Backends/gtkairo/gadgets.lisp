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

(defclass list-selection-event (gadget-event) ())

(defclass tab-button-event (gadget-event)
    ((page :initarg :page :accessor event-page)
     (button :initarg :button :accessor event-button)))

(defclass tab-press-event (tab-button-event) ())
(defclass tab-release-event (tab-button-event) ())


;;;; Classes

;; gtk-menu-* see port.lisp

(defclass gtk-button (native-widget-mixin push-button) ())

(defclass gtk-check-button (native-widget-mixin toggle-button) ())
(defclass gtk-radio-button (native-widget-mixin toggle-button) ())

(defclass gtk-list (native-widget-mixin list-pane climi::meta-list-pane)
    ((title :initarg :title :initform "" :accessor list-pane-title)
     (tree-view :accessor list-pane-tree-view))
  ;; fixme?
  (:default-initargs :value nil))

(defclass gtk-option-pane
    (native-widget-mixin option-pane climi::meta-list-pane)
    ())

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

(defclass gtk-label-pane (native-widget-mixin label-pane)
    ((label-pane-fixed :accessor label-pane-fixed)
     (label-pane-extra-width :accessor label-pane-extra-width)
     (label-pane-extra-height :accessor label-pane-extra-height)))

(defclass gtk-tab-layout (native-widget-mixin clim-tab-layout:tab-layout)
    ((tab-layout-extra-width :accessor tab-layout-extra-width)
     (tab-layout-extra-height :accessor tab-layout-extra-height)))


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

(defmethod realize-native-widget ((sheet gtk-label-pane))
  (let ((frame (gtk_frame_new (climi::label-pane-label sheet)))
	(fixed (gtk_fixed_new))
	(child (car (sheet-children sheet))))
    (gtk_container_add frame fixed)
    (gtk_widget_show fixed)
    (when child
      (let* ((q (compose-space child))
	     (width1 (space-requirement-width q))
	     (height1 (space-requirement-height q)))
	(gtk_widget_set_size_request fixed width1 height1)
	(cffi:with-foreign-object (r 'gtkrequisition)
	  (gtk_widget_size_request frame r)
	  (cffi:with-foreign-slots ((width height) r gtkrequisition)
	    (setf (label-pane-extra-width sheet) (- width width1))
	    (setf (label-pane-extra-height sheet) (- height height1))))))
    (setf (label-pane-fixed sheet) fixed)
    frame))

(defmethod container-put ((parent gtk-label-pane) parent-widget child x y)
  (declare (ignore parent-widget))
  (gtk_fixed_put (label-pane-fixed parent) child x y))

(defmethod container-move ((parent gtk-label-pane) parent-widget child x y)
  (declare (ignore parent-widget))
  (gtk_fixed_move (label-pane-fixed parent) child x y))

(defconstant +g-type-string+ (ash 16 2))

(defun uninstall-scroller-pane (pane)
  (with-slots (climi::scroll-bar
	       climi::vscrollbar climi::hscrollbar
	       climi::x-spacing climi::y-spacing)
      pane
    (setf scroll-bar nil)
    (when climi::vscrollbar
      (sheet-disown-child pane climi::vscrollbar)
      (setf climi::vscrollbar nil))
    (when climi::hscrollbar
      (sheet-disown-child pane climi::hscrollbar)
      (setf climi::hscrollbar nil))
    (setf climi::x-spacing 0)
    (setf climi::y-spacing 0)
    (let ((r (sheet-region pane)))
      (allocate-space pane
		      (bounding-rectangle-width r)
		      (bounding-rectangle-height r)))))

(defun list-pane-selection (sheet)
  (gtk_tree_view_get_selection (list-pane-tree-view sheet)))

(defmethod realize-native-widget ((sheet gtk-list))
  (cffi:with-foreign-object (types :ulong 2)
    (setf (cffi:mem-aref types :long 0) +g-type-string+)
    (setf (cffi:mem-aref types :long 1) 0)
    (let* ((model (gtk_list_store_newv 1 types))
	   (tv (gtk_tree_view_new_with_model model))
	   (column (gtk_tree_view_column_new))
	   (renderer (gtk_cell_renderer_text_new)))
      (setf (list-pane-tree-view sheet) tv)
      (gtk_tree_view_column_pack_start column renderer 1)
      (gtk_tree_view_insert_column tv column -1)
      (gtk_tree_view_column_add_attribute column renderer "text" 0)
      (gtk_tree_view_column_set_title column (list-pane-title sheet))
      (reset-list-pane-items sheet)
      (gtk_tree_selection_set_mode
       (list-pane-selection sheet)
       (if (eq (climi::list-pane-mode sheet) :exclusive)
	   :GTK_SELECTION_BROWSE
	   :GTK_SELECTION_MULTIPLE))
      (gtk-list-reset-selection sheet)
      (let ((ancestor
	     (and (sheet-parent sheet) (sheet-parent (sheet-parent sheet))))
	    (result tv))
	(when (typep ancestor 'scroller-pane)
	  (uninstall-scroller-pane ancestor))
	(let ((wrapper (gtk_scrolled_window_new
			(gtk_tree_view_get_hadjustment tv)
			(gtk_tree_view_get_vadjustment tv))))
	  (gtk_scrolled_window_set_policy wrapper
					  GTK_POLICY_AUTOMATIC
					  GTK_POLICY_AUTOMATIC)
	  (gtk_container_add wrapper tv)
	  (setf result wrapper))
	(setf (list-pane-tree-view sheet) tv) ;?!
	(gtk_tree_selection_set_select_function
	 (list-pane-selection sheet)
	 (cffi:get-callback 'view-selection-callback)
	 result
	 (cffi:null-pointer))
	result))))

(defun reset-list-pane-items (sheet)
  (let ((model (gtk_tree_view_get_model (list-pane-tree-view sheet)))
	(name-key (climi::list-pane-name-key sheet)))
    (gtk_list_store_clear model)  
    (cffi:with-foreign-object (&iter 'gtktreeiter)
      (dolist (i (climi::list-pane-items sheet))
	(gtk_list_store_append model &iter)
	(cffi:with-foreign-string (n (funcall name-key i) :encoding :utf-8)
	  (cffi:with-foreign-object (&value 'gvalue)
	    (setf (cffi:foreign-slot-value &value 'gvalue 'type) 0)
	    (g_value_init &value +g-type-string+)
	    (g_value_set_string &value n)
	    (gtk_list_store_set_value model &iter 0 &value)))))))

(defmethod realize-native-widget ((sheet gtk-option-pane))
  (let* ((widget (gtk_combo_box_new_text))
	 (name-key (climi::list-pane-name-key sheet)))
    (dolist (i (climi::list-pane-items sheet))
      (cffi:with-foreign-string (n (funcall name-key i) :encoding :utf-8)
	(gtk_combo_box_append_text widget n)))
    (option-pane-set-active sheet widget)
    widget))

(defun gtk-list-select-value (sheet value)
  (let ((path
	 (gtk_tree_path_new_from_indices
	  (position value
		    (climi::list-pane-items sheet)
		    :key (climi::list-pane-value-key sheet)
		    :test (climi::list-pane-test sheet))
	  :int -1)))
    (gtk_tree_selection_select_path (list-pane-selection sheet) path)
    (gtk_tree_path_free path)))

(defun gtk-list-reset-selection (sheet)
  (gtk_tree_selection_unselect_all (list-pane-selection sheet))
  (let ((value (gadget-value sheet)))
    (if (eq (climi::list-pane-mode sheet) :exclusive)
	(when value			;fixme?
	  (gtk-list-select-value sheet value))
	(dolist (v value)
	  (gtk-list-select-value sheet v)))))

(defmethod (setf gadget-value) :after
	   (value (gadget gtk-list) &key invoke-callback)
  (declare (ignore invoke-callback))
  (with-gtk ()
    (let ((mirror (sheet-direct-mirror gadget)))
      (when mirror
	(gtk-list-reset-selection gadget)))))

(defmethod (setf climi::list-pane-items)
    (newval (pane gtk-list) &key invoke-callback)
  (declare (ignore invoke-callback))
  (call-next-method)
  (with-gtk ()
    (reset-list-pane-items pane)))

(defmethod climi::generic-list-pane-item-values ((pane gtk-list))
  (mapcar (climi::list-pane-value-key pane)
	  (climi::list-pane-items pane)))

(defmethod handle-event-p
    ((pane gtk-list) (event pointer-button-press-event))
  (eql (pointer-event-button event) +pointer-right-button+))

(defun gtk-list-one-value (pane)
  (if (eq (climi::list-pane-mode pane) :exclusive)
      (if (and (slot-boundp pane 'climi::value)
	       ;; FIXME: we still assume NIL == no value
	       (gadget-value pane))
	  (values (gadget-value pane) t)
	  (values nil nil))
      (if (and (slot-boundp pane 'climi::value)
	       (eql 1 (length (gadget-value pane))))
	  (values (car (gadget-value pane)) t)
	  (values nil nil))))

(defmethod handle-event ((pane gtk-list) (event pointer-button-press-event))
  (multiple-value-bind (value valuep) (gtk-list-one-value pane)
    (when valuep
      (let* ((i (position value (climi::generic-list-pane-item-values pane)))
	     (item (elt (climi::list-pane-items pane) i)))
	(climi::meta-list-pane-call-presentation-menu pane item)))))

(defmethod handle-event-p
    ((pane gtk-list) (event pointer-button-release-event))
  nil)

(defmethod realize-native-widget ((sheet gtk-tab-layout))
  (let ((result (gtk_notebook_new))
	(dummy-child (gtk_fixed_new))
	(dummy-label (gtk_label_new "foo")))
    (gtk_notebook_append_page result dummy-child dummy-label)
    (gtk_widget_show dummy-child)
    (let* ((q
	    (reduce (lambda (x y)
		      (space-requirement-combine #'max x y))
		    (mapcar #'compose-space (sheet-children sheet))
		    :initial-value
		    (make-space-requirement
		     :width 0 :min-width 0 :max-width 0
		     :height 0 :min-height 0 :max-height 0)))
	   (width1 (space-requirement-width q))
	   (height1 (space-requirement-height q)))
      (gtk_widget_set_size_request dummy-child width1 height1)
      (cffi:with-foreign-object (r 'gtkrequisition)
	(gtk_widget_size_request result r)
	(cffi:with-foreign-slots ((width height) r gtkrequisition)
	  (setf (tab-layout-extra-width sheet) (- width width1))
	  (setf (tab-layout-extra-height sheet) (- height height1))))
      (gtk_notebook_remove_page result 0))
    result))

(defmethod container-put ((parent gtk-tab-layout) parent-widget child x y)
  (declare (ignore x y))
  (let* ((page (clim-tab-layout:sheet-to-page
		(widget->sheet child (port parent))))
	 (index (position page (clim-tab-layout:tab-layout-pages parent)))
	 (label (gtk_label_new (clim-tab-layout:tab-page-title page)))
	 (box (gtk_event_box_new)))
    (gtk_event_box_set_visible_window box 0)
    (gtk_container_add box label)
    (gtk_widget_show_all box)
    ;; naja, ein sheet ist das nicht
    (setf (widget->sheet box (port parent)) page)
    (connect-signal box "button-press-event" 'tab-button-handler)
    (gtk_widget_show child)
    (gtk_notebook_insert_page parent-widget child box index)
    (set-tab-page-attributes page label)
    ;; fixme:
    (reorder-notebook-pages parent)
    (setf (clim-tab-layout:tab-layout-enabled-page parent)
	  (clim-tab-layout:tab-layout-enabled-page parent))))

(defmethod (setf clim-tab-layout:tab-layout-pages)
    :after
    (newval (parent gtk-tab-layout))
  (declare (ignore newval))
  (reorder-notebook-pages parent))

(defun reorder-notebook-pages (parent)
  (loop
      for page in (clim-tab-layout:tab-layout-pages parent)
      for i from 0
      do
	(let* ((pane (clim-tab-layout:tab-page-pane page))
	       (mirror (climi::port-lookup-mirror (port parent) pane)))
	  (when mirror
	    (gtk_notebook_reorder_child
	     (native-widget parent)
	     (mirror-widget mirror)
	     i)))))

(defmethod container-move ((parent gtk-tab-layout) parent-widget child x y)
  (declare (ignore parent-widget child x y)))

(defmethod allocate-space ((pane gtk-tab-layout) width height)
  (dolist (page (clim-tab-layout:tab-layout-pages pane))
    (let ((child (clim-tab-layout:tab-page-pane page)))
      (move-sheet child 0 0)		;dummy
      (allocate-space child
		      (- width (tab-layout-extra-width pane))
		      (- height (tab-layout-extra-height pane))))))

(defmethod allocate-space :around ((pane gtk-tab-layout) width height)
  ;; ARGH!  Force the around method in panes.lisp to c-n-m.
  (setf (climi::pane-current-width pane) nil)
  (call-next-method))

(defmethod (setf clim-tab-layout:tab-layout-enabled-page)
    :after
    (newval (parent gtk-tab-layout))
  (when (and (native-widget parent) newval)
    ;; fixme:
    (reorder-notebook-pages parent)
    (gtk_notebook_set_current_page
     (native-widget parent)
     (position newval (clim-tab-layout:tab-layout-pages parent)))))

(defun option-pane-set-active (sheet widget)
  (gtk_combo_box_set_active
   widget
   (position (gadget-value sheet)
	     (climi::list-pane-items sheet)
	     :key (climi::list-pane-value-key sheet)
	     :test (climi::list-pane-test sheet))))

(defmethod (setf gadget-value) :after
	   (value (gadget gtk-option-pane) &key invoke-callback)
  (declare (ignore invoke-callback))
  (with-gtk ()
    (let ((mirror (sheet-direct-mirror gadget)))
      (when mirror
	(option-pane-set-active gadget (mirror-widget mirror))))))

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

(defun make-context-menu (port sheet items &key printer)
  (let ((menu (gtk_menu_new)))
    (dolist (itemspec items)
      (multiple-value-bind (type display-object value sub-items)
	  (destructure-mc-menu-item itemspec)
	(let* ((label (with-output-to-string (s)
			(funcall (or printer #'print-menu-item)
				 display-object
				 s)))
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

(defmethod connect-native-signals ((sheet gtk-list) widget)
  (setf (widget->sheet (list-pane-tree-view sheet) (port sheet)) sheet)
  (connect-signal (list-pane-tree-view sheet)
		  "button-press-event"
		  'button-handler))

(defmethod connect-native-signals ((sheet gtk-label-pane) widget)
  ;; no signals
  )

(defmethod connect-native-signals ((sheet gtk-tab-layout) widget)
  ;; no signals
  )

(defmethod connect-native-signals ((sheet gtk-option-pane) widget)
  (connect-signal widget "changed" 'magic-clicked-handler))


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
    (:gtk_scroll_jump
      (let ((value
	     (clamp (gadget-min-value pane)
		    (event-value event)
		    (gadget-max-value pane))))
	(setf (gadget-value pane :invoke-callback nil) value)
	(drag-callback pane (gadget-client pane) (gadget-id pane) value)))
    (:gtk_scroll_step_backward
      (scroll-up-line-callback pane (gadget-client pane) (gadget-id pane)))
    (:gtk_scroll_step_forward
      (scroll-down-line-callback pane (gadget-client pane) (gadget-id pane)))
    (:gtk_scroll_page_backward
      (scroll-up-page-callback pane (gadget-client pane) (gadget-id pane)))
    (:gtk_scroll_page_forward
      (scroll-down-page-callback pane (gadget-client pane) (gadget-id pane)))))

(defmethod handle-event
    ((pane gtk-menu) (event menu-clicked-event))
  (let ((item (event-item event)))
    (ecase (command-menu-item-type item)
      (:command
	(climi::throw-object-ptype item 'menu-item)))))

;;;(defmethod handle-event
;;;    ((pane gtk-tab-layout) (event tab-release-event))
;;;  )

(defclass parent-ad-hoc-presentation (climi::ad-hoc-presentation)
    ((ad-hoc-children :initarg :ad-hoc-children
		      :reader output-record-children)))

(defmethod clim-tab-layout:note-tab-page-changed ((layout gtk-tab-layout) page)
  (with-gtk ()
    (let* ((pane (clim-tab-layout:tab-page-pane page))
	   (mirror (climi::port-lookup-mirror (port layout) pane)))
      (when mirror
	(let ((box (gtk_notebook_get_tab_label (native-widget layout)
					       (mirror-widget mirror))))
	  (set-tab-page-attributes page (gtk_bin_get_child box)))))))

(defun set-tab-page-attributes (page label)
  ;; fixme: wieso funktioniert das in der tabdemo, nicht aber in beirc?
  (let ((ink (getf (clim-tab-layout:tab-page-drawing-options page) :ink)))
    (when ink
      (gtk-widget-modify-fg label ink)))
  (gtk_label_set_text label (clim-tab-layout:tab-page-title page))
  (gtk_widget_queue_draw label))

(defmethod handle-event
    ((pane gtk-tab-layout) (event tab-press-event))
  (let* ((page (event-page event))
	 (ptype (clim-tab-layout:tab-page-presentation-type page))
	 (inner-presentation
	  (make-instance 'climi::ad-hoc-presentation
	    :object page
	    :single-box t
	    :type 'clim-tab-layout:tab-page))
	 (presentation
	  (make-instance 'parent-ad-hoc-presentation
	    :ad-hoc-children (vector inner-presentation)
	    :object page
	    :single-box t
	    :type ptype)))
    (case (event-button event)
      (#.+pointer-right-button+
	(call-presentation-menu
	 presentation
	 *input-context*
	 *application-frame*
	 pane
	 42 42
	 :for-menu t
	 :label (format nil "Operation on ~A" ptype)))
      (#.+pointer-left-button+
	(throw-highlighted-presentation
	 presentation
	 *input-context*
	 (make-instance 'pointer-button-press-event
	   :sheet pane
	   :x 42 :y 42
	   :modifier-state 0
	   :button (event-button event)))))))

(defmethod handle-event
    ((pane gtk-nonmenu) (event magic-gadget-event))
  (funcall (gtk-nonmenu-callback pane) pane nil))

(defvar *list-selection-result*)

(cffi:defcallback list-selection-callback :void
  ((model :pointer)
   (path :pointer)
   (iter :pointer)
   (data :pointer))
  model iter data
  (setf (gethash (cffi:mem-ref (gtk_tree_path_get_indices path) :int 0)
		 *list-selection-result*)
	t))

(defmethod handle-event
    ((pane gtk-list) (event list-selection-event))
  (with-gtk ()
    (let ((*list-selection-result* (make-hash-table))
	  (value-key (climi::list-pane-value-key pane)))
      (gtk_tree_selection_selected_foreach
       (list-pane-selection pane)
       (cffi:get-callback 'list-selection-callback)
       (cffi:null-pointer))
      (setf (gadget-value pane :invoke-callback t)
	    (if (eq (climi::list-pane-mode pane) :exclusive)
		(loop
		    for i being each hash-key in *list-selection-result*
		    do (return
			 (funcall value-key
				  (elt (climi::list-pane-items pane) i))))
		(loop
		    for i from 0
		    for value in (climi::list-pane-items pane)
		    when (gethash i *list-selection-result*)
		    collect (funcall value-key value)))))))

(defmethod handle-event ((pane gtk-option-pane) (event magic-gadget-event))
  (setf (gadget-value pane :invoke-callback t)
	(funcall (climi::list-pane-value-key pane)
		 (elt (climi::list-pane-items pane)
		      (gtk_combo_box_get_active
		       (mirror-widget (sheet-direct-mirror pane)))))))


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
	(gtk_widget_destroy widget)
	(setf (native-widget gadget) nil)))))

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
	(gtk_widget_destroy widget)
	(setf (native-widget gadget) nil)))))

(defmethod allocate-space ((pane gtk-label-pane) width height)
  (when (sheet-children pane)
    (move-sheet (first (sheet-children pane)) 0 0)
    (allocate-space (first (sheet-children pane))
		    (- width (label-pane-extra-width pane))
		    (- height (label-pane-extra-height pane)))))


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

(defmethod realize-native-widget :around ((gadget native-widget-mixin))
  (let ((widget (call-next-method)))
    (when (typep gadget 'gadget)
      (gtk_widget_set_sensitive widget (if (gadget-active-p gadget) 1 0)))
    widget))

(defmethod activate-gadget :after ((gadget native-widget-mixin))
  (with-gtk ()
    (when (native-widget gadget)
      (gtk_widget_set_sensitive (native-widget gadget) 1))))

(defmethod deactivate-gadget :after ((gadget native-widget-mixin))
  (with-gtk ()
    (when (native-widget gadget)
      (gtk_widget_set_sensitive (native-widget gadget) 0))))


;;; Scroll bars.

;; This is all totally broken.  Why does thumb-size default to 1/4 when it's
;; not a ratio but given in value units?  Why is min==max all the time?
;; And why doesn't this work! :-(
(defun update-scrollbar-adjustment (sheet)
  (when (sheet-direct-mirror sheet)
    (with-gtk ()
      (let* ((min (df (gadget-min-value sheet)))
	     (value (df (gadget-value sheet)))
	     (page-size (df (climi::scroll-bar-thumb-size sheet)))
	     (max (+ (df (gadget-max-value sheet)) page-size)))
	(gtk_range_set_adjustment
	 (mirror-widget (sheet-direct-mirror sheet))
	 (gtk_adjustment_new value min max 0.0d0 0.0d0 page-size))))))

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
