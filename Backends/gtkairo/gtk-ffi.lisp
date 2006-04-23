;;; -*- Mode: Lisp; -*-

;;; (c) 2006 David Lichteblau (david@lichteblau.com)

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

#-(or win32 mswindows windows)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:load-foreign-library "libcairo.so")
  (cffi:load-foreign-library "libgthread-2.0.so")
  (cffi:load-foreign-library "libgtk-x11-2.0.so"))

#+(or win32 mswindows windows)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:load-foreign-library "libcairo-2.dll")
  (cffi:load-foreign-library "libglib-2.0-0.dll")
  (cffi:load-foreign-library "libgthread-2.0-0.dll")
  (cffi:load-foreign-library "libgobject-2.0-0.dll")
  (cffi:load-foreign-library "libgdk-win32-2.0-0.dll")
  (cffi:load-foreign-library "libgtk-win32-2.0-0.dll"))

(defmacro defcfun (name rtype &rest argtypes)
  `(cffi:defcfun (,name ,(intern (string-upcase name) :clim-gtkairo))
       ,rtype
     ,@argtypes))


;;; Here's a hack to wait on GTK's Xlib Display's socket file descriptor
;;; without blocking in native code:

;;; (SBCL doesn't really need this, but other Lisps might.)

#-(or win32 windows mswindows)
(cffi:defcvar "gdk_display" :pointer)

#-(or win32 windows mswindows)
(cffi:defcstruct xdisplay
  (ext_data :pointer)
  (private1 :pointer)
  (fd :int)
  ;; ... and many other more, but we only care about the file descriptor
  )

#-(or win32 windows mswindows)
(defun gdk-xlib-fd ()
  (cffi:foreign-slot-value *gdk-display* 'xdisplay 'fd))


;;; let's litter the code with locking

(cffi:defcvar "g_threads_got_initialized" :int)

(defvar *have-lock* nil)

(defmacro with-gtk ((&optional) &body body)
  `(invoke-with-gtk (lambda () ,@body)))

#+sbcl
(defvar *normal-modes* (sb-int:get-floating-point-modes))

;; FIXME: Unless I disable traps, I get SIGPFE while in Cairo.  Unless I
;; reset all options afterwards, I get lisp errors like f-p-i-o for, say,
;; (ATAN -13 13/2) in McCLIM.  Isn't SBCL responsible for calling C code
;; with the with the modes C code expects?  Or does cairo change them?
(defmacro with-cairo-floats ((&optional) &body body)
  `(unwind-protect
       (progn
	 #+sbcl (sb-int:set-floating-point-modes :traps nil)
	 ,@body)
     #+sbcl (apply #'sb-int:set-floating-point-modes *normal-modes*)))

;; Note: There's no need for locking in single threaded mode for most
;; functions, except that the main loop functions try to release the
;; lock temporarily, so those need to be called with locking.  Let's do
;; locking unconditionally for now.
;;
;; Note #2: Although every medium function should grab this lock, if
;; there was a good way to grab it around the entire redisplay
;; procedure, individual functions wouldn't have to actually do
;; anything.  Probably need to find some :around method for this.

;; Note #3: we could use gdk_threads_set_lock_functions here and redirect
;; gdk locking to a Lisp lock if we wanted.  We'd need separate
;; functions to lock and unlock a recursive lock for that, which the
;; portability files currently don't provide.
(defun invoke-with-gtk (fn)
  (with-cairo-floats ()
    (unless *have-lock*
      (gdk_threads_enter))
    (unwind-protect
	(let ((*have-lock* t))
	  (funcall fn))
      (unless *have-lock*
	;; fixme: gdk documentation recommends flushing before releasing
	;; the lock.  But doing so makes everything s.l.o.w.
;;;	(gdk_flush)
	(gdk_threads_leave)))))


;;; GROVELME

;; must be a separate structure definition in order for padding on AMD64
;; to work properly.
(cffi:defcstruct gtkobject
  (gtype :unsigned-long)		;GTypeInstance
  (ref_count :unsigned-int)		;GObject
  (qdata :pointer)			;  -"-
  (flags :uint32)			;GtkObject
  )

(cffi:defcstruct gtkwidget
  (header gtkobject)
  (private_flags :uint16)
  (state :uint8)
  (saved_state :uint8)
  (name :pointer)
  (style :pointer)
  (requisition-width :int)		;GtkRequisition
  (requisition-height :int)		;  -"-
  (allocation-x :int)			;GtkAllocation
  (allocation-y :int)			;  -"-
  (allocation-width :int)		;  -"-
  (allocation-height :int)		;  -"-
  (gdkwindow :pointer)
  (parent :pointer))

(cffi:defcstruct gtkrequisition
  (width :int)
  (height :int))

(defun gtkwidget-header (widget)
  (cffi:foreign-slot-value widget 'gtkwidget 'header))

(defun gtkwidget-flags (widget)
  (cffi:foreign-slot-value (gtkwidget-header widget) 'gtkobject 'flags))

(defun (setf gtkwidget-flags) (newval widget)
  (setf (cffi:foreign-slot-value (gtkwidget-header widget) 'gtkobject 'flags)
        newval))

(cffi:defcstruct gdkeventexpose
  (type :int)
  (gdkwindow :pointer)
  (send_event :int8)
  (x :int)				;area
  (y :int)				;  -"-
  (width :int)				;  -"-
  (height :int)				;  -"-
  (region :pointer)
  (count :int))

(cffi:defcstruct gdkeventmotion
  (type :int)
  (gdkwindow :pointer)
  (send_event :int8)
  (time :uint32)
  (x :double)
  (y :double)
  (axes :pointer)
  (state :uint)
  (is_hint :int16)
  (device :pointer)
  (x_root :double)
  (y_root :double))

(cffi:defcstruct gdkeventbutton
  (type :int)
  (gdkwindow :pointer)
  (send_event :int8)
  (time :uint32)
  (x :double)
  (y :double)
  (axes :pointer)
  (state :uint)
  (button :uint)
  (device :pointer)
  (x_root :double)
  (y_root :double))

(cffi:defcstruct gdkeventkey
  (type :int)
  (gdkwindow :pointer)
  (send_event :int8)
  (time :uint32)
  (state :uint)
  (keyval :uint)
  (length :int)
  (string :string))

(cffi:defcstruct gdkeventcrossing
  (type :int)
  (gdkwindow :pointer)
  (send_event :int8)
  (subwindow :pointer)
  (time :uint32)
  (x :double)
  (y :double)
  (x_root :double)
  (y_root :double)
  (gdkcrossingmode :int)
  (gdknotifytype :int)
  (focusp :int)
  (state :uint))

(cffi:defcstruct gdkeventconfigure
  (type :int)
  (gdkwindow :pointer)
  (send_event :int8)
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(cffi:defcstruct gdkrectangle
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(cffi:defcstruct gdkallocation
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(cffi:defcstruct gdkcolor
  (pixel :uint32)
  (r :uint16)
  (g :uint16)
  (b :uint16))

(cffi:defcstruct gdkgeometry
  (min_width :int)
  (min_height :int)
  (max_width :int)
  (max_height :int)
  (base_width :int)
  (base_height :int)
  (width_inc :int)
  (height_inc :int)
  (min_aspect :double)
  (max_aspect :double)
  (win_gravity :int))

(cffi:defcenum gdkfunction
    :copy :invert :xor :clear :and :and_reverse :and_invert :noop :or :equiv
    :or_reverse :copy_invert :or_invert :nand :nor :set)


;;; GTK functions

(defconstant GTK_WINDOW_TOPLEVEL 0)
(defconstant GTK_WINDOW_POPUP 1)

(defcfun "gtk_init"
    :void
  (argc :int)
  (argv :pointer))

(defcfun "gtk_events_pending"
    :int)

(defcfun "gtk_main_iteration_do"
    :void
  (block :int))

(defcfun "gtk_window_new"
    :pointer
  (type :int))

(defcfun "gtk_widget_destroy"
    :void
  (window :pointer))

(defcfun "gtk_widget_show_all"
    :void
  (widget :pointer))

(defcfun "gtk_widget_hide_all"
    :void
  (widget :pointer))

(defcfun "gtk_widget_show"
    :void
  (widget :pointer))

(defcfun "gtk_widget_hide"
    :void
  (widget :pointer))

(defcfun "gtk_window_resize"
    :void
  (window :pointer)
  (width :int)
  (height :int))

(defcfun "gtk_window_move"
    :void
  (window :pointer)
  (x :int)
  (y :int))

(defcfun "gtk_drawing_area_new"
    :pointer)

(defcfun "gtk_widget_set_size_request"
    :void
  (widget :pointer)
  (width :int)
  (height :int))

(defcfun "gtk_widget_get_size_request"
    :void
  (widget :pointer)
  (width :pointer)
  (height :pointer))

(defcfun "gtk_widget_size_request"
    :void
  (widget :pointer)
  (requisition :pointer))

(defcfun "gtk_container_add"
    :void
  (parent :pointer)
  (child :pointer))

(defcfun "gdk_cairo_create"
    :pointer
  (gdk-window :pointer))

(defcfun "gtk_fixed_new"
    :pointer
  )

(defcfun "gtk_fixed_put"
    :void
  (fixed :pointer)
  (child :pointer)
  (x :int)
  (y :int))

(defcfun "gtk_fixed_move"
    :void
  (fixed :pointer)
  (child :pointer)
  (x :int)
  (y :int))

(defcfun "gtk_fixed_set_has_window"
    :void
  (fixed :pointer)
  (windowp :int))

(defcfun "g_signal_connect_data"
    :void
  (object :pointer)
  (event :string)
  (callback :pointer)
  (data :pointer)
  (destroy_data :pointer)
  (flags :int))

(defun g-signal-connect (object event callback &optional data)
  (g_signal_connect_data object
			 event
			 callback
			 (or data (cffi:null-pointer))
			 (cffi:null-pointer)
			 0))

(defcfun "gtk_widget_add_events"
    :void
  (widget :pointer)
  (events :int))

(defcfun "gtk_widget_set_events"
    :void
  (widget :pointer)
  (events :int))

(defcfun "gtk_widget_get_events"
    :int
  (widget :pointer))

(defcfun "gtk_widget_grab_focus"
    :void
  (widget :pointer))

(defcfun "gtk_widget_set_double_buffered"
    :void
  (widget :pointer)
  (enable :int))

(defcfun "gdk_display_flush"
    :void
  (display :pointer))

(defcfun "gdk_display_get_default"
    :pointer)

(defcfun "gdk_display_get_pointer"
    :void
  (display :pointer)
  (screen :pointer)
  (x :pointer)
  (y :pointer)
  (mask :pointer))

(defcfun "gtk_widget_get_pointer"
    :void
  (widget :pointer)
  (x :pointer)
  (y :pointer))

(defcfun "gdk_screen_get_default"
    :pointer
  )

(defcfun "gdk_screen_get_height"
    :int
  (screen :pointer))

(defcfun "gdk_screen_get_width"
    :int
  (screen :pointer))

(defcfun "gdk_screen_get_height_mm"
    :int
  (screen :pointer))

(defcfun "gdk_screen_get_width_mm"
    :int
  (screen :pointer))

(defcfun "gdk_pointer_grab"
    :int
  (gdkwindow :pointer)
  (owner_events :int)
  (event_mask :int)
  (confine_to :pointer)
  (cursor :pointer)
  (time :uint32))

(defcfun "gdk_pointer_ungrab"
    :void
  (time :uint32))

(defcfun "gdk_threads_enter"
    :void)

(defcfun "gdk_threads_leave"
    :void)

(defcfun "gdk_threads_init"
    :void)

(defcfun "g_thread_init"
    :void
  (fns :pointer))

(defcfun "gdk_flush"
    :void)

(defcfun "gdk_window_begin_paint_rect"
    :void
  (window :pointer)
  (rect :pointer))

(defcfun "gdk_window_end_paint"
    :void
  (window :pointer))

(defcfun "gdk_window_get_root_origin"
    :void
  (window :pointer)
  (x :pointer)
  (y :pointer))

(defcfun "gtk_widget_modify_bg"
    :void
  (widget :pointer)
  (state :int)
  (color :pointer))

(defcfun "gtk_window_set_default_size"
    :void
  (window :pointer)
  (width :int)
  (height :int))

(defcfun "gtk_widget_size_allocate"
    :void
  (widget :pointer)
  (allocation :pointer))

(defcfun "gtk_widget_queue_resize"
    :void
  (widget :pointer))

(defcfun "gtk_window_set_geometry_hints"
    :void
  (window :pointer)
  (widget :pointer)
  (geometry :pointer)
  (mask :int))

(defcfun "gdk_screen_get_root_window"
    :pointer
  (screen :pointer))

(defcfun "gdk_pixmap_new"
    :pointer
  (drawable :pointer)
  (width :int)
  (height :int)
  (depth :int))

(defcfun "gdk_drawable_unref"
    :void
  (drawable :pointer))

(defcfun "gdk_drawable_get_depth"
    :int
  (drawable :pointer))

(defcfun "gdk_gc_new"
    :pointer
  (drawable :pointer))

(defcfun "gdk_gc_unref"
    :void
  (drawable :pointer))

(defcfun "gdk_gc_set_function"
    :void
  (gc :pointer)
  (function gdkfunction))

(defcfun "gdk_draw_drawable"
    :void
  (drawable :pointer)
  (gc :pointer)
  (src-drawable :pointer)
  (xsrc :int)
  (ysrc :int)
  (xdest :int)
  (ydest :int)
  (width :int)
  (height :int))

(defcfun "gtk_button_new"
    :pointer
  )

(defcfun "gtk_button_new_with_label"
    :pointer
  (label :string))

(defcfun "gtk_button_set_label"
    :void
  (button :pointer)
  (label :string))

(defcfun "gtk_check_button_new"
    :pointer
  )

(defcfun "gtk_check_button_new_with_label"
    :pointer
  (label :string))

(defcfun "gtk_radio_button_new"
    :pointer
  (group :pointer))

(defcfun "gtk_radio_button_new_with_label"
    :pointer
  (group :pointer)
  (label :string))

(defcfun "gtk_toggle_button_set_active"
    :void
  (button :pointer)
  (active :int))

(defcfun "gtk_radio_button_get_group"
    :pointer
  (button :pointer))

(defcfun "gtk_vscale_new_with_range"
    :pointer
  (min :double)
  (max :double)
  (step :double))

(defcfun "gtk_hscale_new_with_range"
    :pointer
  (min :double)
  (max :double)
  (step :double))

(defcfun "gtk_scale_set_digits"
    :void
  (scale :pointer)
  (digits :int))

(defcfun "gtk_scale_set_draw_value"
    :void
  (scale :pointer)
  (draw_value :int))

(defcfun "gtk_range_get_value"
    :double
  (range :pointer))

(defcfun "gtk_range_get_adjustment"
    :pointer
  (range :pointer))

(defcfun "gtk_adjustment_get_value"
    :double
  (range :pointer))

(defcfun "gtk_adjustment_new"
    :pointer
  (value :double)
  (min :double)
  (max :double)
  (step_increment :double)
  (page_increment :double)
  (page_size :double))

(defcfun "gtk_hscrollbar_new"
    :pointer
  (adjustment :pointer))

(defcfun "gtk_vscrollbar_new"
    :pointer
  (adjustment :pointer))

(defcfun "gtk_window_set_title"
    :void
  (window :pointer)
  (title :string))

(defconstant GDK_EXPOSURE_MASK             (ash 1 1))
(defconstant GDK_POINTER_MOTION_MASK       (ash 1 2))
(defconstant GDK_POINTER_MOTION_HINT_MASK  (ash 1 3))
(defconstant GDK_BUTTON_MOTION_MASK        (ash 1 4))
(defconstant GDK_BUTTON1_MOTION_MASK       (ash 1 5))
(defconstant GDK_BUTTON2_MOTION_MASK       (ash 1 6))
(defconstant GDK_BUTTON3_MOTION_MASK       (ash 1 7))
(defconstant GDK_BUTTON_PRESS_MASK         (ash 1 8))
(defconstant GDK_BUTTON_RELEASE_MASK       (ash 1 9))
(defconstant GDK_KEY_PRESS_MASK            (ash 1 10))
(defconstant GDK_KEY_RELEASE_MASK          (ash 1 11))
(defconstant GDK_ENTER_NOTIFY_MASK         (ash 1 12))
(defconstant GDK_LEAVE_NOTIFY_MASK         (ash 1 13))
(defconstant GDK_FOCUS_CHANGE_MASK         (ash 1 14))
(defconstant GDK_STRUCTURE_MASK            (ash 1 15))
(defconstant GDK_PROPERTY_CHANGE_MASK      (ash 1 16))
(defconstant GDK_VISIBILITY_NOTIFY_MASK    (ash 1 17))
(defconstant GDK_PROXIMITY_IN_MASK         (ash 1 18))
(defconstant GDK_PROXIMITY_OUT_MASK        (ash 1 19))
(defconstant GDK_SUBSTRUCTURE_MASK         (ash 1 20))
(defconstant GDK_SCROLL_MASK               (ash 1 21))

(defconstant GDK_NOTHING           -1)
(defconstant GDK_DELETE            0)
(defconstant GDK_DESTROY           1)
(defconstant GDK_EXPOSE            2)
(defconstant GDK_MOTION_NOTIFY     3)
(defconstant GDK_BUTTON_PRESS      4)
(defconstant GDK_2BUTTON_PRESS     5)
(defconstant GDK_3BUTTON_PRESS     6)
(defconstant GDK_BUTTON_RELEASE    7)
(defconstant GDK_KEY_PRESS         8)
(defconstant GDK_KEY_RELEASE       9)
(defconstant GDK_ENTER_NOTIFY      10)
(defconstant GDK_LEAVE_NOTIFY      11)
(defconstant GDK_FOCUS_CHANGE      12)
(defconstant GDK_CONFIGURE         13)
(defconstant GDK_MAP               14)
(defconstant GDK_UNMAP             15)
(defconstant GDK_PROPERTY_NOTIFY   16)
(defconstant GDK_SELECTION_CLEAR   17)
(defconstant GDK_SELECTION_REQUEST 18)
(defconstant GDK_SELECTION_NOTIFY  19)
(defconstant GDK_PROXIMITY_IN      20)
(defconstant GDK_PROXIMITY_OUT     21)
(defconstant GDK_DRAG_ENTER        22)
(defconstant GDK_DRAG_LEAVE        23)
(defconstant GDK_DRAG_MOTION       24)
(defconstant GDK_DRAG_STATUS       25)
(defconstant GDK_DROP_START        26)
(defconstant GDK_DROP_FINISHED     27)
(defconstant GDK_CLIENT_EVENT      28)
(defconstant GDK_VISIBILITY_NOTIFY 29)
(defconstant GDK_NO_EXPOSE         30)
(defconstant GDK_SCROLL            31)
(defconstant GDK_WINDOW_STATE      32)
(defconstant GDK_SETTING           33)
(defconstant GDK_OWNER_CHANGE      34)
(defconstant GDK_GRAB_BROKEN       35)

(defconstant GTK_TOPLEVEL         (ash 1 4))
(defconstant GTK_NO_WINDOW        (ash 1 5))
(defconstant GTK_REALIZED         (ash 1 6))
(defconstant GTK_MAPPED           (ash 1 7))
(defconstant GTK_VISIBLE          (ash 1 8))
(defconstant GTK_SENSITIVE        (ash 1 9))
(defconstant GTK_PARENT_SENSITIVE (ash 1 10))
(defconstant GTK_CAN_FOCUS        (ash 1 11))
(defconstant GTK_HAS_FOCUS        (ash 1 12))
(defconstant GTK_CAN_DEFAULT      (ash 1 13))
(defconstant GTK_HAS_DEFAULT      (ash 1 14))
(defconstant GTK_HAS_GRAB         (ash 1 15))
(defconstant GTK_RC_STYLE         (ash 1 16))
(defconstant GTK_COMPOSITE_CHILD  (ash 1 17))
(defconstant GTK_NO_REPARENT      (ash 1 18))
(defconstant GTK_APP_PAINTABLE    (ash 1 19))
(defconstant GTK_RECEIVES_DEFAULT (ash 1 20))
(defconstant GTK_DOUBLE_BUFFERED  (ash 1 21))
(defconstant GTK_NO_SHOW_ALL      (ash 1 22))

(defconstant GDK_SHIFT_MASK   (ash 1 0))
(defconstant GDK_LOCK_MASK    (ash 1 1))
(defconstant GDK_CONTROL_MASK (ash 1 2))
(defconstant GDK_MOD1_MASK    (ash 1 3))
(defconstant GDK_MOD2_MASK    (ash 1 4))
(defconstant GDK_MOD3_MASK    (ash 1 5))
(defconstant GDK_MOD4_MASK    (ash 1 6))
(defconstant GDK_MOD5_MASK    (ash 1 7))
(defconstant GDK_BUTTON1_MASK  (ash 1 8))
(defconstant GDK_BUTTON2_MASK  (ash 1 9))
(defconstant GDK_BUTTON3_MASK  (ash 1 10))
(defconstant GDK_BUTTON4_MASK  (ash 1 11))
(defconstant GDK_BUTTON5_MASK  (ash 1 12))

(defconstant GDK_CROSSING_NORMAL 0)
(defconstant GDK_CROSSING_GRAB 1)
(defconstant GDK_CROSSING_UNGRAB 2)

(defconstant GDK_CURRENT_TIME 0)


;;; foo

(defun test (&optional (port :gtkairo))
  (mapc #'climi::destroy-port climi::*all-ports*)
  (setf climi::*server-path-search-order* (list port))
  (clim:run-frame-top-level
   (clim:make-application-frame 'clim-demo::address-book)))
