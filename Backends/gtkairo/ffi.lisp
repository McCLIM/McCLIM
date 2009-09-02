;;; -*- Mode: Lisp; -*-

(in-package :clim-gtkairo)

(cffi:defcstruct Depth
  (depth :int)                          ;int
  (nvisuals :int)                       ;int
  (visuals :pointer)                    ;Visual *
  )

(defcenum GConnectFlags
  (:G_CONNECT_AFTER 1)
  :G_CONNECT_SWAPPED)

(defcenum GdkCapStyle
  :GDK_CAP_NOT_LAST
  :GDK_CAP_BUTT
  :GDK_CAP_ROUND
  :GDK_CAP_PROJECTING)

(defcenum GdkCrossingMode
  :GDK_CROSSING_NORMAL
  :GDK_CROSSING_GRAB
  :GDK_CROSSING_UNGRAB)

(defcenum GdkDragAction
  (:GDK_ACTION_DEFAULT 1)
  :GDK_ACTION_COPY
  (:GDK_ACTION_MOVE 4)
  (:GDK_ACTION_LINK 8)
  (:GDK_ACTION_PRIVATE 16)
  (:GDK_ACTION_ASK 32))

(defcenum GdkDragProtocol
  :GDK_DRAG_PROTO_MOTIF
  :GDK_DRAG_PROTO_XDND
  :GDK_DRAG_PROTO_ROOTWIN
  :GDK_DRAG_PROTO_NONE
  :GDK_DRAG_PROTO_WIN32_DROPFILES
  :GDK_DRAG_PROTO_OLE2
  :GDK_DRAG_PROTO_LOCAL)

(defcenum GdkEventMask
  (:GDK_EXPOSURE_MASK 2)
  (:GDK_POINTER_MOTION_MASK 4)
  (:GDK_POINTER_MOTION_HINT_MASK 8)
  (:GDK_BUTTON_MOTION_MASK 16)
  (:GDK_BUTTON1_MOTION_MASK 32)
  (:GDK_BUTTON2_MOTION_MASK 64)
  (:GDK_BUTTON3_MOTION_MASK 128)
  (:GDK_BUTTON_PRESS_MASK 256)
  (:GDK_BUTTON_RELEASE_MASK 512)
  (:GDK_KEY_PRESS_MASK 1024)
  (:GDK_KEY_RELEASE_MASK 2048)
  (:GDK_ENTER_NOTIFY_MASK 4096)
  (:GDK_LEAVE_NOTIFY_MASK 8192)
  (:GDK_FOCUS_CHANGE_MASK 16384)
  (:GDK_STRUCTURE_MASK 32768)
  (:GDK_PROPERTY_CHANGE_MASK 65536)
  (:GDK_VISIBILITY_NOTIFY_MASK 131072)
  (:GDK_PROXIMITY_IN_MASK 262144)
  (:GDK_PROXIMITY_OUT_MASK 524288)
  (:GDK_SUBSTRUCTURE_MASK 1048576)
  (:GDK_SCROLL_MASK 2097152)
  (:GDK_ALL_EVENTS_MASK 4194302))

(defcenum GdkEventType
  (:GDK_NOTHING -1)
  :GDK_DELETE
  :GDK_DESTROY
  :GDK_EXPOSE
  :GDK_MOTION_NOTIFY
  :GDK_BUTTON_PRESS
  :GDK_2BUTTON_PRESS
  :GDK_3BUTTON_PRESS
  :GDK_BUTTON_RELEASE
  :GDK_KEY_PRESS
  :GDK_KEY_RELEASE
  :GDK_ENTER_NOTIFY
  :GDK_LEAVE_NOTIFY
  :GDK_FOCUS_CHANGE
  :GDK_CONFIGURE
  :GDK_MAP
  :GDK_UNMAP
  :GDK_PROPERTY_NOTIFY
  :GDK_SELECTION_CLEAR
  :GDK_SELECTION_REQUEST
  :GDK_SELECTION_NOTIFY
  :GDK_PROXIMITY_IN
  :GDK_PROXIMITY_OUT
  :GDK_DRAG_ENTER
  :GDK_DRAG_LEAVE
  :GDK_DRAG_MOTION
  :GDK_DRAG_STATUS
  :GDK_DROP_START
  :GDK_DROP_FINISHED
  :GDK_CLIENT_EVENT
  :GDK_VISIBILITY_NOTIFY
  :GDK_NO_EXPOSE
  :GDK_SCROLL
  :GDK_WINDOW_STATE
  :GDK_SETTING
  :GDK_OWNER_CHANGE
  :GDK_GRAB_BROKEN)

(defcenum GdkFunction
  :GDK_COPY
  :GDK_INVERT
  :GDK_XOR
  :GDK_CLEAR
  :GDK_AND
  :GDK_AND_REVERSE
  :GDK_AND_INVERT
  :GDK_NOOP
  :GDK_OR
  :GDK_EQUIV
  :GDK_OR_REVERSE
  :GDK_COPY_INVERT
  :GDK_OR_INVERT
  :GDK_NAND
  :GDK_NOR
  :GDK_SET)

(defcenum GdkGrabStatus
  :GDK_GRAB_SUCCESS
  :GDK_GRAB_ALREADY_GRABBED
  :GDK_GRAB_INVALID_TIME
  :GDK_GRAB_NOT_VIEWABLE
  :GDK_GRAB_FROZEN)

(defcenum GdkJoinStyle
  :GDK_JOIN_MITER
  :GDK_JOIN_ROUND
  :GDK_JOIN_BEVEL)

(defcenum GdkLineStyle
  :GDK_LINE_SOLID
  :GDK_LINE_ON_OFF_DASH
  :GDK_LINE_DOUBLE_DASH)

(defcenum GdkModifierType
  (:GDK_SHIFT_MASK 1)
  :GDK_LOCK_MASK
  (:GDK_CONTROL_MASK 4)
  (:GDK_MOD1_MASK 8)
  (:GDK_MOD2_MASK 16)
  (:GDK_MOD3_MASK 32)
  (:GDK_MOD4_MASK 64)
  (:GDK_MOD5_MASK 128)
  (:GDK_BUTTON1_MASK 256)
  (:GDK_BUTTON2_MASK 512)
  (:GDK_BUTTON3_MASK 1024)
  (:GDK_BUTTON4_MASK 2048)
  (:GDK_BUTTON5_MASK 4096)
  (:GDK_RELEASE_MASK 1073741824)
  (:GDK_MODIFIER_MASK 1073750015))

(defcenum GdkNotifyType
  :GDK_NOTIFY_ANCESTOR
  :GDK_NOTIFY_VIRTUAL
  :GDK_NOTIFY_INFERIOR
  :GDK_NOTIFY_NONLINEAR
  :GDK_NOTIFY_NONLINEAR_VIRTUAL
  :GDK_NOTIFY_UNKNOWN)

(defcenum GdkWindowHints
  (:GDK_HINT_POS 1)
  :GDK_HINT_MIN_SIZE
  (:GDK_HINT_MAX_SIZE 4)
  (:GDK_HINT_BASE_SIZE 8)
  (:GDK_HINT_ASPECT 16)
  (:GDK_HINT_RESIZE_INC 32)
  (:GDK_HINT_WIN_GRAVITY 64)
  (:GDK_HINT_USER_POS 128)
  (:GDK_HINT_USER_SIZE 256))

(defcenum GtkPolicyType
  :GTK_POLICY_ALWAYS
  :GTK_POLICY_AUTOMATIC
  :GTK_POLICY_NEVER)

(defcenum GtkScrollType
  :GTK_SCROLL_NONE
  :GTK_SCROLL_JUMP
  :GTK_SCROLL_STEP_BACKWARD
  :GTK_SCROLL_STEP_FORWARD
  :GTK_SCROLL_PAGE_BACKWARD
  :GTK_SCROLL_PAGE_FORWARD
  :GTK_SCROLL_STEP_UP
  :GTK_SCROLL_STEP_DOWN
  :GTK_SCROLL_PAGE_UP
  :GTK_SCROLL_PAGE_DOWN
  :GTK_SCROLL_STEP_LEFT
  :GTK_SCROLL_STEP_RIGHT
  :GTK_SCROLL_PAGE_LEFT
  :GTK_SCROLL_PAGE_RIGHT
  :GTK_SCROLL_START
  :GTK_SCROLL_END)

(defcenum GtkSelectionMode
  :GTK_SELECTION_NONE
  :GTK_SELECTION_SINGLE
  :GTK_SELECTION_BROWSE
  :GTK_SELECTION_MULTIPLE
  (:GTK_SELECTION_EXTENDED 3))

(defcenum GtkStateType
  :GTK_STATE_NORMAL
  :GTK_STATE_ACTIVE
  :GTK_STATE_PRELIGHT
  :GTK_STATE_SELECTED
  :GTK_STATE_INSENSITIVE)

(defcenum GtkWindowType
  :GTK_WINDOW_TOPLEVEL
  :GTK_WINDOW_POPUP)

(defcenum PangoStyle
  :PANGO_STYLE_NORMAL
  :PANGO_STYLE_OBLIQUE
  :PANGO_STYLE_ITALIC)

(defcenum PangoWeight
  (:PANGO_WEIGHT_ULTRALIGHT 200)
  (:PANGO_WEIGHT_LIGHT 300)
  (:PANGO_WEIGHT_NORMAL 400)
  (:PANGO_WEIGHT_SEMIBOLD 600)
  (:PANGO_WEIGHT_BOLD 700)
  (:PANGO_WEIGHT_ULTRABOLD 800)
  (:PANGO_WEIGHT_HEAVY 900))

(cffi:defcstruct Screen
  (ext_data :pointer)                   ;XExtData *
  (display :pointer)                    ;struct _XDisplay *
  (root :unsigned-long)                 ;Window
  (width :int)                          ;int
  (height :int)                         ;int
  (mwidth :int)                         ;int
  (mheight :int)                        ;int
  (ndepths :int)                        ;int
  (depths :pointer)                     ;Depth *
  (root_depth :int)                     ;int
  (root_visual :pointer)                ;Visual *
  (default_gc :pointer)                 ;GC
  (cmap :unsigned-long)                 ;Colormap
  (white_pixel :unsigned-long)          ;long unsigned int
  (black_pixel :unsigned-long)          ;long unsigned int
  (max_maps :int)                       ;int
  (min_maps :int)                       ;int
  (backing_store :int)                  ;int
  (save_unders :int)                    ;int
  (root_input_mask :long)               ;long int
  )

(defcfun "cairo_arc"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :double)                        ;double
  (arg2 :double)                        ;double
  (arg3 :double)                        ;double
  (arg4 :double)                        ;double
  (arg5 :double)                        ;double
  )

(defcfun "cairo_arc_negative"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :double)                        ;double
  (arg2 :double)                        ;double
  (arg3 :double)                        ;double
  (arg4 :double)                        ;double
  (arg5 :double)                        ;double
  )

(defcfun "cairo_clip"
    :void
  (arg0 :pointer)                       ;cairo_t *
  )

(defcfun "cairo_copy_page"
    :void
  (arg0 :pointer)                       ;cairo_t *
  )

(defcfun "cairo_create"
    :pointer
  (arg0 :pointer)                       ;cairo_surface_t *
  )

(defcfun "cairo_curve_to"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :double)                        ;double
  (arg2 :double)                        ;double
  (arg3 :double)                        ;double
  (arg4 :double)                        ;double
  (arg5 :double)                        ;double
  (arg6 :double)                        ;double
  )

(defcfun "cairo_destroy"
    :void
  (arg0 :pointer)                       ;cairo_t *
  )

(defcfun "cairo_fill"
    :void
  (arg0 :pointer)                       ;cairo_t *
  )

(defcfun "cairo_fill_extents"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :pointer)                       ;double *
  (arg2 :pointer)                       ;double *
  (arg3 :pointer)                       ;double *
  (arg4 :pointer)                       ;double *
  )

(defcfun "cairo_fill_preserve"
    :pointer
  (arg0 :pointer)                       ;cairo_t *
  )

(defcfun "cairo_font_extents"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :pointer)                       ;cairo_font_extents_t *
  )

(defcfun "cairo_font_face_status"
    cairo_status_t
  (arg0 :pointer)                       ;cairo_font_face_t *
  )

(defcfun "cairo_format_stride_for_width"
    :int
  (arg0 cairo_format_t)
  (arg1 :int)
  )

(defcfun "cairo_get_font_face"
    :pointer
  (arg0 :pointer)                       ;cairo_t *
  )

(defcfun "cairo_get_target"
    :pointer
  (arg0 :pointer)                       ;cairo_t *
  )

(defcfun "cairo_glyph_extents"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :pointer)                       ;cairo_glyph_t *
  (arg2 :int)                           ;int
  (arg3 :pointer)                       ;cairo_text_extents_t *
  )

(defcfun "cairo_glyph_path"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :pointer)                       ;cairo_glyph_t *
  (arg2 :int)                           ;int
  )

(defcfun "cairo_identity_matrix"
    :void
  (arg0 :pointer)                       ;cairo_t *
  )

(defcfun "cairo_image_surface_create"
    :pointer
  (arg0 cairo_format_t)
  (arg1 :int)                           ;int
  (arg2 :int)                           ;int
  )

(defcfun "cairo_image_surface_create_for_data"
    :pointer
  (arg0 utf8-string)                    ;unsigned char *
  (arg1 cairo_format_t)
  (arg2 :int)                           ;int
  (arg3 :int)                           ;int
  (arg4 :int)                           ;int
  )

(defcfun "cairo_in_fill"
    :int
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :double)                        ;double
  (arg2 :double)                        ;double
  )

(defcfun "cairo_in_stroke"
    :int
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :double)                        ;double
  (arg2 :double)                        ;double
  )

(defcfun "cairo_line_to"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :double)                        ;double
  (arg2 :double)                        ;double
  )

(defcfun "cairo_matrix_init"
    :pointer
  (arg0 :pointer)                       ;cairo_matrix_t *
  (arg1 :double)                        ;double
  (arg2 :double)                        ;double
  (arg3 :double)                        ;double
  (arg4 :double)                        ;double
  (arg5 :double)                        ;double
  (arg6 :double)                        ;double
  )

(defcfun "cairo_matrix_rotate"
    :void
  (arg0 :pointer)                       ;cairo_matrix_t *
  (arg1 :double)                        ;double
  )

(defcfun "cairo_matrix_translate"
    :void
  (arg0 :pointer)                       ;cairo_matrix_t *
  (arg1 :double)                        ;double
  (arg2 :double)                        ;double
  )

(defcfun "cairo_move_to"
    :pointer
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :double)                        ;double
  (arg2 :double)                        ;double
  )

(defcfun "cairo_new_path"
    :pointer
  (arg0 :pointer)                       ;cairo_t *
  )

(defcfun "cairo_paint"
    :void
  (arg0 :pointer)                       ;cairo_t *
  )

(defcfun "cairo_pattern_create_for_surface"
    :pointer
  (arg0 :pointer)                       ;cairo_surface_t *
  )

(defcfun "cairo_pattern_create_linear"
    :pointer
  (arg0 :double)                        ;double
  (arg1 :double)                        ;double
  (arg2 :double)                        ;double
  (arg3 :double)                        ;double
  )

(defcfun "cairo_pattern_create_radial"
    :pointer
  (arg0 :double)                        ;double
  (arg1 :double)                        ;double
  (arg2 :double)                        ;double
  (arg3 :double)                        ;double
  (arg4 :double)                        ;double
  (arg5 :double)                        ;double
  )

(defcfun "cairo_pattern_destroy"
    :void
  (arg0 :pointer)                       ;cairo_pattern_t *
  )

(defcfun "cairo_pattern_get_extend"
    cairo_extend_t
  (arg0 :pointer)                       ;cairo_pattern_t *
  )

(defcfun "cairo_pattern_get_filter"
    cairo_filter_t
  (arg0 :pointer)                       ;cairo_pattern_t *
  )

(defcfun "cairo_pattern_get_matrix"
    :void
  (arg0 :pointer)                       ;cairo_pattern_t *
  (arg1 :pointer)                       ;cairo_matrix_t *
  )

(defcfun "cairo_pattern_reference"
    :pointer
  (arg0 :pointer)                       ;cairo_pattern_t *
  )

(defcfun "cairo_pattern_set_extend"
    :void
  (arg0 :pointer)                       ;cairo_pattern_t *
  (arg1 cairo_extend_t))

(defcfun "cairo_pattern_set_filter"
    :void
  (arg0 :pointer)                       ;cairo_pattern_t *
  (arg1 cairo_filter_t))

(defcfun "cairo_pattern_set_matrix"
    :void
  (arg0 :pointer)                       ;cairo_pattern_t *
  (arg1 :pointer)                       ;const cairo_matrix_t *
  )

(defcfun "cairo_rectangle"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :double)                        ;double
  (arg2 :double)                        ;double
  (arg3 :double)                        ;double
  (arg4 :double)                        ;double
  )

(defcfun "cairo_reference"
    :pointer
  (arg0 :pointer)                       ;cairo_t *
  )

(defcfun "cairo_rel_curve_to"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :double)                        ;double
  (arg2 :double)                        ;double
  (arg3 :double)                        ;double
  (arg4 :double)                        ;double
  (arg5 :double)                        ;double
  (arg6 :double)                        ;double
  )

(defcfun "cairo_rel_move_to"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :double)                        ;double
  (arg2 :double)                        ;double
  )

(defcfun "cairo_reset_clip"
    :void
  (arg0 :pointer)                       ;cairo_t *
  )

(defcfun "cairo_rotate"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :double)                        ;double
  )

(defcfun "cairo_scale"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :double)                        ;double
  (arg2 :double)                        ;double
  )

(defcfun "cairo_select_font_face"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 utf8-string)                    ;const char *
  (arg2 cairo_font_slant_t)
  (arg3 cairo_font_weight_t))

(defcfun "cairo_set_antialias"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 cairo_antialias_t))

(defcfun "cairo_set_dash"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :pointer)                       ;const double *
  (arg2 :int)                           ;int
  (arg3 :double)                        ;double
  )

(defcfun "cairo_set_fill_rule"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 cairo_fill_rule_t))

(defcfun "cairo_set_font_size"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :double)                        ;double
  )

(defcfun "cairo_set_line_cap"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 cairo_line_cap_t))

(defcfun "cairo_set_line_join"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 cairo_line_join_t))

(defcfun "cairo_set_line_width"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :double)                        ;double
  )

(defcfun "cairo_set_matrix"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :pointer)                       ;const cairo_matrix_t *
  )

(defcfun "cairo_set_miter_limit"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :double)                        ;double
  )

(defcfun "cairo_set_operator"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 cairo_operator_t))

(defcfun "cairo_set_source"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :pointer)                       ;cairo_pattern_t *
  )

(defcfun "cairo_set_source_rgb"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :double)                        ;double
  (arg2 :double)                        ;double
  (arg3 :double)                        ;double
  )

(defcfun "cairo_set_source_rgba"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :double)                        ;double
  (arg2 :double)                        ;double
  (arg3 :double)                        ;double
  (arg4 :double)                        ;double
  )

(defcfun "cairo_set_source_surface"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :pointer)                       ;cairo_surface_t *
  (arg2 :double)
  (arg3 :double)
  )

(defcfun "cairo_set_tolerance"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :double)                        ;double
  )

(defcfun "cairo_show_glyphs"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :pointer)                       ;cairo_glyph_t *
  (arg2 :int)                           ;int
  )

(defcfun "cairo_show_page"
    :void
  (arg0 :pointer)                       ;cairo_t *
  )

(defcfun "cairo_show_text"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 utf8-string)                    ;const char *
  )

(defcfun "cairo_status"
    cairo_status_t
  (arg0 :pointer)                       ;cairo_t *
  )

(defcfun "cairo_stroke"
    :void
  (arg0 :pointer)                       ;cairo_t *
  )

(defcfun "cairo_stroke_extents"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :pointer)                       ;double *
  (arg2 :pointer)                       ;double *
  (arg3 :pointer)                       ;double *
  (arg4 :pointer)                       ;double *
  )

(defcfun "cairo_stroke_preserve"
    :pointer
  (arg0 :pointer)                       ;cairo_t *
  )

(defcfun "cairo_surface_create_similar"
    :pointer
  (arg0 :pointer)                       ;cairo_surface_t *
  (arg1 cairo_content_t)
  (arg2 :int)                           ;int
  (arg3 :int)                           ;int
  )

(defcfun "cairo_surface_destroy"
    :pointer
  (arg0 :pointer)                       ;cairo_surface_t *
  )

(defcfun "cairo_surface_flush"
    :void
  (arg0 :pointer)                       ;cairo_surface_t *
  )

(defcfun "cairo_surface_mark_dirty"
    :void
  (arg0 :pointer)                       ;cairo_surface_t *
  )

(defcfun "cairo_surface_reference"
    :pointer
  (arg0 :pointer)                       ;cairo_surface_t *
  )

(defcfun "cairo_text_extents"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 utf8-string)                    ;const char *
  (arg2 :pointer)                       ;cairo_text_extents_t *
  )

(defcfun "cairo_text_path"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 utf8-string)                    ;const char *
  )

(defcfun "cairo_translate"
    :void
  (arg0 :pointer)                       ;cairo_t *
  (arg1 :double)                        ;double
  (arg2 :double)                        ;double
  )

(defcfun "g_free"
    :void
  (mem :pointer)                        ;gpointer
  )

(defcfun "g_idle_add"
    :unsigned-int
  (function :pointer)                   ;GSourceFunc
  (data :pointer)                       ;gpointer
  )

(defcfun "g_object_unref"
    :void
  (_object :pointer)                    ;gpointer
  )

(defcfun "g_signal_connect_data"
    :unsigned-long
  (instance :pointer)                   ;gpointer
  (detailed_signal utf8-string)         ;const gchar *
  (c_handler :pointer)                  ;GCallback
  (data :pointer)                       ;gpointer
  (destroy_data :pointer)               ;GClosureNotify
  (connect_flags GConnectFlags))

(defcfun "g_thread_init"
    :void
  (init :pointer)                       ;GThreadFunctions *
  )

(defcfun "g_value_init"
    :pointer
  (value :pointer)                      ;GValue *
  (g_type :unsigned-long)               ;GType
  )

(defcfun "g_value_set_string"
    :void
  (value :pointer)                      ;GValue *
  (v_string utf8-string)                ;const gchar *
  )

(defcfun "gdk_cairo_create"
    :pointer
  (drawable :pointer)                   ;GdkDrawable *
  )

(defcfun "gdk_colormap_alloc_color"
    :int
  (colormap :pointer)                   ;GdkColormap *
  (color :pointer)                      ;GdkColor *
  (writeable :int)                      ;gboolean
  (best_match :int)                     ;gboolean
  )

(defcfun "gdk_colormap_get_system" :pointer)

(defcfun "gdk_display_flush"
    :void
  (display :pointer)                    ;GdkDisplay *
  )

(defcfun "gdk_display_get_default" :pointer)

(defcfun "gdk_display_get_pointer"
    :void
  (display :pointer)                    ;GdkDisplay *
  (screen :pointer)                     ;GdkScreen **
  (x :pointer)                          ;gint *
  (y :pointer)                          ;gint *
  (mask :pointer)                       ;GdkModifierType *
  )

(defcfun "gdk_drag_motion"
    :int
  (context :pointer)                    ;GdkDragContext *
  (dest_window :pointer)                ;GdkWindow *
  (protocol GdkDragProtocol)
  (x_root :int)                         ;gint
  (y_root :int)                         ;gint
  (suggested_action GdkDragAction)
  (possible_actions GdkDragAction)
  (time :uint32)                        ;guint32
  )

(defcfun "gdk_drag_status"
    :void
  (context :pointer)                    ;GdkDragContext *
  (action GdkDragAction)
  (time :uint32)                        ;guint32
  )

(defcfun "gdk_draw_arc"
    :void
  (drawable :pointer)                   ;GdkDrawable *
  (gc :pointer)                         ;GdkGC *
  (filled :int)                         ;gboolean
  (x :int)                              ;gint
  (y :int)                              ;gint
  (width :int)                          ;gint
  (height :int)                         ;gint
  (angle1 :int)                         ;gint
  (angle2 :int)                         ;gint
  )

(defcfun "gdk_draw_drawable"
    :void
  (drawable :pointer)                   ;GdkDrawable *
  (gc :pointer)                         ;GdkGC *
  (src :pointer)                        ;GdkDrawable *
  (xsrc :int)                           ;gint
  (ysrc :int)                           ;gint
  (xdest :int)                          ;gint
  (ydest :int)                          ;gint
  (width :int)                          ;gint
  (height :int)                         ;gint
  )

(defcfun "gdk_draw_layout"
    :void
  (drawable :pointer)                   ;GdkDrawable *
  (gc :pointer)                         ;GdkGC *
  (x :int)                              ;int
  (y :int)                              ;int
  (layout :pointer)                     ;PangoLayout *
  )

(defcfun "gdk_draw_line"
    :void
  (drawable :pointer)                   ;GdkDrawable *
  (gc :pointer)                         ;GdkGC *
  (x1 :int)                             ;gint
  (y1 :int)                             ;gint
  (x2 :int)                             ;gint
  (y2 :int)                             ;gint
  )

(defcfun "gdk_draw_lines"
    :void
  (drawable :pointer)                   ;GdkDrawable *
  (gc :pointer)                         ;GdkGC *
  (points :pointer)                     ;GdkPoint *
  (npoints :int)                        ;gint
  )

(defcfun "gdk_draw_point"
    :void
  (drawable :pointer)                   ;GdkDrawable *
  (gc :pointer)                         ;GdkGC *
  (x :int)                              ;gint
  (y :int)                              ;gint
  )

(defcfun "gdk_draw_polygon"
    :void
  (drawable :pointer)                   ;GdkDrawable *
  (gc :pointer)                         ;GdkGC *
  (filled :int)                         ;gboolean
  (points :pointer)                     ;GdkPoint *
  (npoints :int)                        ;gint
  )

(defcfun "gdk_draw_rectangle"
    :void
  (drawable :pointer)                   ;GdkDrawable *
  (gc :pointer)                         ;GdkGC *
  (filled :int)                         ;gboolean
  (x :int)                              ;gint
  (y :int)                              ;gint
  (width :int)                          ;gint
  (height :int)                         ;gint
  )

(defcfun "gdk_drawable_get_clip_region"
    :pointer
  (drawable :pointer)                   ;GdkDrawable *
  )

(defcfun "gdk_drawable_get_depth"
    :int
  (drawable :pointer)                   ;GdkDrawable *
  )

(defcfun "gdk_drawable_unref"
    :void
  (drawable :pointer)                   ;GdkDrawable *
  )

(defcfun "gdk_error_trap_pop" :int)

(defcfun "gdk_error_trap_push" :void)

(defcfun "gdk_flush" :void)

(defcfun "gdk_gc_new"
    :pointer
  (drawable :pointer)                   ;GdkDrawable *
  )

(defcfun "gdk_gc_set_background"
    :void
  (gc :pointer)                         ;GdkGC *
  (color :pointer)                      ;const GdkColor *
  )

(defcfun "gdk_gc_set_clip_region"
    :void
  (gc :pointer)                         ;GdkGC *
  (region :pointer)                     ;GdkRegion *
  )

(defcfun "gdk_gc_set_dashes"
    :void
  (gc :pointer)                         ;GdkGC *
  (dash_offset :int)                    ;gint
  (dash_list :pointer)                  ;gint8 *
  (n :int)                              ;gint
  )

(defcfun "gdk_gc_set_foreground"
    :void
  (gc :pointer)                         ;GdkGC *
  (color :pointer)                      ;const GdkColor *
  )

(defcfun "gdk_gc_set_function"
    :void
  (gc :pointer)                         ;GdkGC *
  (function GdkFunction))

(defcfun "gdk_gc_set_line_attributes"
    :void
  (gc :pointer)                         ;GdkGC *
  (line_width :int)                     ;gint
  (line_style GdkLineStyle)
  (cap_style GdkCapStyle)
  (join_style GdkJoinStyle))

(defcfun "gdk_gc_set_rgb_bg_color"
    :void
  (gc :pointer)                         ;GdkGC *
  (color :pointer)                      ;const GdkColor *
  )

(defcfun "gdk_gc_set_rgb_fg_color"
    :void
  (gc :pointer)                         ;GdkGC *
  (color :pointer)                      ;const GdkColor *
  )

(defcfun "gdk_gc_unref"
    :void
  (gc :pointer)                         ;GdkGC *
  )

(defcfun "gdk_pango_context_get" :pointer)

(defcfun "gdk_pango_context_get_for_screen"
    :pointer
  (screen :pointer)                     ;GdkScreen *
  )

(defcfun "gdk_pixmap_new"
    :pointer
  (drawable :pointer)                   ;GdkDrawable *
  (width :int)                          ;gint
  (height :int)                         ;gint
  (depth :int)                          ;gint
  )

(defcfun "gdk_pointer_grab"
    GdkGrabStatus
  (window :pointer)                     ;GdkWindow *
  (owner_events :int)                   ;gboolean
  (event_mask GdkEventMask)
  (confine_to :pointer)                 ;GdkWindow *
  (cursor :pointer)                     ;GdkCursor *
  (time :uint32)                        ;guint32
  )

(defcfun "gdk_pointer_ungrab"
    :void
  (time :uint32)                        ;guint32
  )

(defcfun "gdk_region_destroy"
    :void
  (r :pointer)                          ;GdkRegion *
  )

(defcfun "gdk_region_new" :pointer)

(defcfun "gdk_region_union"
    :void
  (region :pointer)                     ;GdkRegion *
  (other :pointer)                      ;GdkRegion *
  )

(defcfun "gdk_region_union_with_rect"
    :void
  (region :pointer)                     ;GdkRegion *
  (rect :pointer)                       ;GdkRectangle *
  )

(defcfun "gdk_screen_get_default" :pointer)

(defcfun "gdk_screen_get_height"
    :int
  (screen :pointer)                     ;GdkScreen *
  )

(defcfun "gdk_screen_get_height_mm"
    :int
  (screen :pointer)                     ;GdkScreen *
  )

(defcfun "gdk_screen_get_root_window"
    :pointer
  (screen :pointer)                     ;GdkScreen *
  )

(defcfun "gdk_screen_get_width"
    :int
  (screen :pointer)                     ;GdkScreen *
  )

(defcfun "gdk_screen_get_width_mm"
    :int
  (screen :pointer)                     ;GdkScreen *
  )

(defcfun "gdk_threads_enter" :void)

(defcfun "gdk_threads_init" :void)

(defcfun "gdk_threads_leave" :void)

(defcfun "gdk_window_clear_area"
    :void
  (window :pointer)                     ;GdkWindow *
  (x :int)                              ;gint
  (y :int)                              ;gint
  (width :int)                          ;gint
  (height :int)                         ;gint
  )

(defcfun "gdk_window_get_root_origin"
    :void
  (window :pointer)                     ;GdkWindow *
  (x :pointer)                          ;gint *
  (y :pointer)                          ;gint *
  )

(defcfun "gdk_window_invalidate_rect"
    :void
  (window :pointer)                     ;GdkWindow *
  (rect :pointer)                       ;GdkRectangle *
  (invalidate_children :int)            ;gboolean
  )

(defcfun "gtk_adjustment_new"
    :pointer
  (value :double)                       ;gdouble
  (lower :double)                       ;gdouble
  (upper :double)                       ;gdouble
  (step_increment :double)              ;gdouble
  (page_increment :double)              ;gdouble
  (page_size :double)                   ;gdouble
  )

(defcfun "gtk_adjustment_set_value"
    :void
  (adjustment :pointer)                 ;GtkAdjustment *
  (value :double)                       ;gdouble
  )

(defcfun "gtk_bin_get_child"
    :pointer
  (bin :pointer)                        ;GtkBin *
  )

(defcfun "gtk_button_new_with_label"
    :pointer
  (label utf8-string)                   ;const gchar *
  )

(defcfun "gtk_cell_renderer_text_new" :pointer)

(defcfun "gtk_check_button_new_with_label"
    :pointer
  (label utf8-string)                   ;const gchar *
  )

(defcfun "gtk_combo_box_append_text"
    :void
  (combo_box :pointer)                  ;GtkComboBox *
  (text utf8-string)                    ;const gchar *
  )

(defcfun "gtk_combo_box_get_active"
    :int
  (combo_box :pointer)                  ;GtkComboBox *
  )

(defcfun "gtk_combo_box_new_text" :pointer)

(defcfun "gtk_combo_box_set_active"
    :void
  (combo_box :pointer)                  ;GtkComboBox *
  (index_ :int)                         ;gint
  )

(defcfun "gtk_container_add"
    :void
  (container :pointer)                  ;GtkContainer *
  (widget :pointer)                     ;GtkWidget *
  )

(defcfun "gtk_event_box_new" :pointer)

(defcfun "gtk_event_box_set_above_child"
    :void
  (event_box :pointer)                  ;GtkEventBox *
  (above_child :int)                    ;gboolean
  )

(defcfun "gtk_event_box_set_visible_window"
    :void
  (event_box :pointer)                  ;GtkEventBox *
  (visible_window :int)                 ;gboolean
  )

(defcfun "gtk_events_pending" :int)

(defcfun "gtk_fixed_move"
    :void
  (fixed :pointer)                      ;GtkFixed *
  (widget :pointer)                     ;GtkWidget *
  (x :int)                              ;gint
  (y :int)                              ;gint
  )

(defcfun "gtk_fixed_new" :pointer)

(defcfun "gtk_fixed_put"
    :void
  (fixed :pointer)                      ;GtkFixed *
  (widget :pointer)                     ;GtkWidget *
  (x :int)                              ;gint
  (y :int)                              ;gint
  )

(defcfun "gtk_fixed_set_has_window"
    :void
  (fixed :pointer)                      ;GtkFixed *
  (has_window :int)                     ;gboolean
  )

(defcfun "gtk_frame_new"
    :pointer
  (label utf8-string)                   ;const gchar *
  )

(defcfun "gtk_get_current_event_time" :uint32)

(defcfun "gtk_hscale_new_with_range"
    :pointer
  (min :double)                         ;gdouble
  (max :double)                         ;gdouble
  (step :double)                        ;gdouble
  )

(defcfun "gtk_hscrollbar_new"
    :pointer
  (adjustment :pointer)                 ;GtkAdjustment *
  )

(defcfun "gtk_init"
    :void
  (argc :pointer)                       ;int *
  (argv :pointer)                       ;char ***
  )

(defcfun "gtk_label_new"
    :pointer
  (str utf8-string)                     ;const gchar *
  )

(defcfun "gtk_label_set_text"
    :void
  (label :pointer)                      ;GtkLabel *
  (str utf8-string)                     ;const gchar *
  )

(defcfun "gtk_list_store_append"
    :void
  (list_store :pointer)                 ;GtkListStore *
  (iter :pointer)                       ;GtkTreeIter *
  )

(defcfun "gtk_list_store_clear"
    :void
  (list_store :pointer)                 ;GtkListStore *
  )

(defcfun "gtk_list_store_newv"
    :pointer
  (n_columns :int)                      ;gint
  (types :pointer)                      ;GType *
  )

(defcfun "gtk_list_store_set_value"
    :void
  (list_store :pointer)                 ;GtkListStore *
  (iter :pointer)                       ;GtkTreeIter *
  (column :int)                         ;gint
  (value :pointer)                      ;GValue *
  )

(defcfun "gtk_main_iteration_do"
    :int
  (blocking :int)                       ;gboolean
  )

(defcfun "gtk_menu_bar_new" :pointer)

(defcfun "gtk_menu_item_new_with_label"
    :pointer
  (label utf8-string)                   ;const gchar *
  )

(defcfun "gtk_menu_item_set_submenu"
    :void
  (menu_item :pointer)                  ;GtkMenuItem *
  (submenu :pointer)                    ;GtkWidget *
  )

(defcfun "gtk_menu_new" :pointer)

(defcfun "gtk_menu_popup"
    :void
  (menu :pointer)                       ;GtkMenu *
  (parent_menu_shell :pointer)          ;GtkWidget *
  (parent_menu_item :pointer)           ;GtkWidget *
  (func :pointer)                       ;GtkMenuPositionFunc
  (data :pointer)                       ;gpointer
  (button :unsigned-int)                ;guint
  (activate_time :uint32)               ;guint32
  )

(defcfun "gtk_menu_shell_append"
    :void
  (menu_shell :pointer)                 ;GtkMenuShell *
  (child :pointer)                      ;GtkWidget *
  )

(defcfun "gtk_notebook_append_page"
    :int
  (notebook :pointer)                   ;GtkNotebook *
  (child :pointer)                      ;GtkWidget *
  (tab_label :pointer)                  ;GtkWidget *
  )

(defcfun "gtk_notebook_get_current_page"
    :int
  (notebook :pointer)                   ;GtkNotebook *
  )

(defcfun "gtk_notebook_get_tab_label"
    :pointer
  (notebook :pointer)                   ;GtkNotebook *
  (child :pointer)                      ;GtkWidget *
  )

(defcfun "gtk_notebook_insert_page"
    :int
  (notebook :pointer)                   ;GtkNotebook *
  (child :pointer)                      ;GtkWidget *
  (tab_label :pointer)                  ;GtkWidget *
  (position :int)                       ;gint
  )

(defcfun "gtk_notebook_new" :pointer)

(defcfun "gtk_notebook_remove_page"
    :void
  (notebook :pointer)                   ;GtkNotebook *
  (page_num :int)                       ;gint
  )

(defcfun "gtk_notebook_reorder_child"
    :void
  (notebook :pointer)                   ;GtkNotebook *
  (child :pointer)                      ;GtkWidget *
  (position :int)                       ;gint
  )

(defcfun "gtk_notebook_set_current_page"
    :void
  (notebook :pointer)                   ;GtkNotebook *
  (page_num :int)                       ;gint
  )

(defcfun "gtk_radio_button_get_group"
    :pointer
  (radio_button :pointer)               ;GtkRadioButton *
  )

(defcfun "gtk_radio_button_new_with_label"
    :pointer
  (group :pointer)                      ;GSList *
  (label utf8-string)                   ;const gchar *
  )

(defcfun "gtk_range_get_adjustment"
    :pointer
  (range :pointer)                      ;GtkRange *
  )

(defcfun "gtk_range_get_value"
    :double
  (range :pointer)                      ;GtkRange *
  )

(defcfun "gtk_range_set_adjustment"
    :void
  (range :pointer)                      ;GtkRange *
  (adjustment :pointer)                 ;GtkAdjustment *
  )

(defcfun "gtk_scale_set_digits"
    :void
  (scale :pointer)                      ;GtkScale *
  (digits :int)                         ;gint
  )

(defcfun "gtk_scale_set_draw_value"
    :void
  (scale :pointer)                      ;GtkScale *
  (draw_value :int)                     ;gboolean
  )

(defcfun "gtk_scrolled_window_new"
    :pointer
  (hadjustment :pointer)                ;GtkAdjustment *
  (vadjustment :pointer)                ;GtkAdjustment *
  )

(defcfun "gtk_scrolled_window_set_policy"
    :void
  (scrolled_window :pointer)            ;GtkScrolledWindow *
  (hscrollbar_policy GtkPolicyType)
  (vscrollbar_policy GtkPolicyType))

(defcfun "gtk_separator_menu_item_new" :pointer)

(defcfun "gtk_toggle_button_set_active"
    :void
  (toggle_button :pointer)              ;GtkToggleButton *
  (is_active :int)                      ;gboolean
  )

(defcfun "gtk_tree_path_free"
    :void
  (path :pointer)                       ;GtkTreePath *
  )

(defcfun "gtk_tree_path_get_indices"
    :pointer
  (path :pointer)                       ;GtkTreePath *
  )

(defcfun "gtk_tree_selection_select_path"
    :void
  (selection :pointer)                  ;GtkTreeSelection *
  (path :pointer)                       ;GtkTreePath *
  )

(defcfun "gtk_tree_selection_selected_foreach"
    :void
  (selection :pointer)                  ;GtkTreeSelection *
  (func :pointer)                       ;GtkTreeSelectionForeachFunc
  (data :pointer)                       ;gpointer
  )

(defcfun "gtk_tree_selection_set_mode"
    :void
  (selection :pointer)                  ;GtkTreeSelection *
  (type GtkSelectionMode))

(defcfun "gtk_tree_selection_set_select_function"
    :void
  (selection :pointer)                  ;GtkTreeSelection *
  (func :pointer)                       ;GtkTreeSelectionFunc
  (data :pointer)                       ;gpointer
  (destroy :pointer)                    ;GtkDestroyNotify
  )

(defcfun "gtk_tree_selection_unselect_all"
    :void
  (selection :pointer)                  ;GtkTreeSelection *
  )

(defcfun "gtk_tree_view_column_add_attribute"
    :void
  (tree_column :pointer)                ;GtkTreeViewColumn *
  (cell_renderer :pointer)              ;GtkCellRenderer *
  (attribute utf8-string)               ;const gchar *
  (column :int)                         ;gint
  )

(defcfun "gtk_tree_view_column_new" :pointer)

(defcfun "gtk_tree_view_column_pack_start"
    :void
  (tree_column :pointer)                ;GtkTreeViewColumn *
  (cell :pointer)                       ;GtkCellRenderer *
  (expand :int)                         ;gboolean
  )

(defcfun "gtk_tree_view_column_set_title"
    :void
  (tree_column :pointer)                ;GtkTreeViewColumn *
  (title utf8-string)                   ;const gchar *
  )

(defcfun "gtk_tree_view_get_hadjustment"
    :pointer
  (tree_view :pointer)                  ;GtkTreeView *
  )

(defcfun "gtk_tree_view_get_model"
    :pointer
  (tree_view :pointer)                  ;GtkTreeView *
  )

(defcfun "gtk_tree_view_get_selection"
    :pointer
  (tree_view :pointer)                  ;GtkTreeView *
  )

(defcfun "gtk_tree_view_get_vadjustment"
    :pointer
  (tree_view :pointer)                  ;GtkTreeView *
  )

(defcfun "gtk_tree_view_insert_column"
    :int
  (tree_view :pointer)                  ;GtkTreeView *
  (column :pointer)                     ;GtkTreeViewColumn *
  (position :int)                       ;gint
  )

(defcfun "gtk_tree_view_new_with_model"
    :pointer
  (model :pointer)                      ;GtkTreeModel *
  )

(defcfun "gtk_vscale_new_with_range"
    :pointer
  (min :double)                         ;gdouble
  (max :double)                         ;gdouble
  (step :double)                        ;gdouble
  )

(defcfun "gtk_vscrollbar_new"
    :pointer
  (adjustment :pointer)                 ;GtkAdjustment *
  )

(defcfun "gtk_widget_add_events"
    :void
  (widget :pointer)                     ;GtkWidget *
  (events :int)                         ;gint
  )

(defcfun "gtk_widget_destroy"
    :void
  (widget :pointer)                     ;GtkWidget *
  )

(defcfun "gtk_widget_get_child_requisition"
    :void
  (widget :pointer)                     ;GtkWidget *
  (requisition :pointer)                ;GtkRequisition *
  )

(defcfun "gtk_widget_get_events"
    :int
  (widget :pointer)                     ;GtkWidget *
  )

(defcfun "gtk_widget_get_parent"
    :pointer
  (widget :pointer)                     ;GtkWidget *
  )

(defcfun "gtk_widget_get_pointer"
    :void
  (widget :pointer)                     ;GtkWidget *
  (x :pointer)                          ;gint *
  (y :pointer)                          ;gint *
  )

(defcfun "gtk_widget_get_size_request"
    :void
  (widget :pointer)                     ;GtkWidget *
  (width :pointer)                      ;gint *
  (height :pointer)                     ;gint *
  )

(defcfun "gtk_widget_grab_focus"
    :void
  (widget :pointer)                     ;GtkWidget *
  )

(defcfun "gtk_widget_hide"
    :void
  (widget :pointer)                     ;GtkWidget *
  )

(defcfun "gtk_widget_hide_all"
    :void
  (widget :pointer)                     ;GtkWidget *
  )

(defcfun "gtk_widget_modify_bg"
    :void
  (widget :pointer)                     ;GtkWidget *
  (state GtkStateType)
  (color :pointer)                      ;const GdkColor *
  )

(defcfun "gtk_widget_modify_fg"
    :void
  (widget :pointer)                     ;GtkWidget *
  (state GtkStateType)
  (color :pointer)                      ;const GdkColor *
  )

(defcfun "gtk_widget_queue_draw"
    :void
  (widget :pointer)                     ;GtkWidget *
  )

(defcfun "gtk_widget_set_double_buffered"
    :void
  (widget :pointer)                     ;GtkWidget *
  (double_buffered :int)                ;gboolean
  )

(defcfun "gtk_widget_set_events"
    :void
  (widget :pointer)                     ;GtkWidget *
  (events :int)                         ;gint
  )

(defcfun "gtk_widget_set_sensitive"
    :void
  (widget :pointer)                     ;GtkWidget *
  (sensitive :int)                      ;gboolean
  )

(defcfun "gtk_widget_set_size_request"
    :void
  (widget :pointer)                     ;GtkWidget *
  (width :int)                          ;gint
  (height :int)                         ;gint
  )

(defcfun "gtk_widget_show"
    :void
  (widget :pointer)                     ;GtkWidget *
  )

(defcfun "gtk_widget_show_all"
    :void
  (widget :pointer)                     ;GtkWidget *
  )

(defcfun "gtk_widget_size_request"
    :void
  (widget :pointer)                     ;GtkWidget *
  (requisition :pointer)                ;GtkRequisition *
  )

(defcfun "gtk_window_get_focus"
    :pointer
  (window :pointer))

(defcfun "gtk_window_move"
    :void
  (window :pointer)                     ;GtkWindow *
  (x :int)                              ;gint
  (y :int)                              ;gint
  )

(defcfun "gtk_window_new"
    :pointer
  (type GtkWindowType))

(defcfun "gtk_window_resize"
    :void
  (window :pointer)                     ;GtkWindow *
  (width :int)                          ;gint
  (height :int)                         ;gint
  )

(defcfun "gtk_window_set_default_size"
    :void
  (window :pointer)                     ;GtkWindow *
  (width :int)                          ;gint
  (height :int)                         ;gint
  )

(defcfun "gtk_window_set_geometry_hints"
    :void
  (window :pointer)                     ;GtkWindow *
  (geometry_widget :pointer)            ;GtkWidget *
  (geometry :pointer)                   ;GdkGeometry *
  (geom_mask GdkWindowHints))

(defcfun "gtk_window_set_title"
    :void
  (window :pointer)                     ;GtkWindow *
  (title utf8-string)                   ;const gchar *
  )

(defcfun "pango_cairo_create_layout"
    :pointer
  (cr :pointer)                         ;cairo_t *
  )

(defcfun "pango_cairo_show_layout"
    :void
  (cr :pointer)                         ;cairo_t *
  (layout :pointer)                     ;PangoLayout *
  )

(defcfun "pango_context_get_font_map"
    :pointer
  (context :pointer)                    ;PangoContext *
  )

(defcfun "pango_context_get_metrics"
    :pointer
  (context :pointer)                    ;PangoContext *
  (desc :pointer)                       ;const PangoFontDescription *
  (language :pointer)                   ;PangoLanguage *
  )

(defcfun "pango_context_list_families"
    :void
  (context :pointer)                    ;PangoContext *
  (families :pointer)                   ;PangoFontFamily ***
  (n_families :pointer)                 ;int *
  )

(defcfun "pango_context_load_font"
    :pointer
  (context :pointer)                    ;PangoContext *
  (desc :pointer)                       ;const PangoFontDescription *
  )

(defcfun "pango_font_describe"
    :pointer
  (font :pointer)                       ;PangoFont *
  )

(defcfun "pango_font_description_free"
    :void
  (desc :pointer)                       ;PangoFontDescription *
  )

(defcfun "pango_font_description_from_string"
    :pointer
  (str utf8-string)                     ;const char *
  )

(defcfun "pango_font_description_get_family"
    utf8-string
  (desc :pointer)                       ;const PangoFontDescription *
  )

(defcfun "pango_font_description_new" :pointer)

(defcfun "pango_font_description_set_absolute_size"
    :void
  (desc :pointer)                       ;PangoFontDescription *
  (size :double)                        ;double
  )

(defcfun "pango_font_description_set_family"
    :void
  (desc :pointer)                       ;PangoFontDescription *
  (family utf8-string)                  ;const char *
  )

(defcfun "pango_font_description_set_size"
    :void
  (desc :pointer)                       ;PangoFontDescription *
  (size :int)                           ;gint
  )

(defcfun "pango_font_description_set_style"
    :void
  (desc :pointer)                       ;PangoFontDescription *
  (style PangoStyle))

(defcfun "pango_font_description_set_weight"
    :void
  (desc :pointer)                       ;PangoFontDescription *
  (weight PangoWeight))

(defcfun "pango_font_description_to_string"
    utf8-string
  (desc :pointer)                       ;const PangoFontDescription *
  )

(defcfun "pango_font_face_get_face_name"
    utf8-string
  (face :pointer)                       ;PangoFontFace *
  )

(defcfun "pango_font_face_list_sizes"
    :void
  (face :pointer)                       ;PangoFontFace *
  (sizes :pointer)                      ;int **
  (n_sizes :pointer)                    ;int *
  )

(defcfun "pango_font_family_get_name"
    utf8-string
  (family :pointer)                     ;PangoFontFamily *
  )

(defcfun "pango_font_family_is_monospace"
    :int
  (family :pointer)                     ;PangoFontFamily *
  )

(defcfun "pango_font_family_list_faces"
    :void
  (family :pointer)                     ;PangoFontFamily *
  (faces :pointer)                      ;PangoFontFace ***
  (n_faces :pointer)                    ;int *
  )

(defcfun "pango_font_map_load_font"
    :pointer
  (fontmap :pointer)                    ;PangoFontMap *
  (context :pointer)                    ;PangoContext *
  (desc :pointer)                       ;const PangoFontDescription *
  )

(defcfun "pango_font_metrics_get_approximate_char_width"
    :int
  (metrics :pointer)                    ;PangoFontMetrics *
  )

(defcfun "pango_font_metrics_get_ascent"
    :int
  (metrics :pointer)                    ;PangoFontMetrics *
  )

(defcfun "pango_font_metrics_get_descent"
    :int
  (metrics :pointer)                    ;PangoFontMetrics *
  )

(defcfun "pango_font_metrics_unref"
    :void
  (metrics :pointer)                    ;PangoFontMetrics *
  )

(defcfun "pango_layout_get_context"
    :pointer
  (layout :pointer)                     ;PangoLayout *
  )

(defcfun "pango_layout_get_line"
    :pointer
  (layout :pointer)                     ;PangoLayout *
  (line :int)                           ;int
  )

(defcfun "pango_layout_get_line_count"
    :int
  (layout :pointer)                     ;PangoLayout *
  )

(defcfun "pango_layout_get_pixel_extents"
    :void
  (layout :pointer)                     ;PangoLayout *
  (ink_rect :pointer)                   ;PangoRectangle *
  (logical_rect :pointer)               ;PangoRectangle *
  )

(defcfun "pango_layout_get_pixel_size"
    :void
  (layout :pointer)                     ;PangoLayout *
  (width :pointer)                      ;int *
  (height :pointer)                     ;int *
  )

(defcfun "pango_layout_get_size"
    :void
  (layout :pointer)                     ;PangoLayout *
  (width :pointer)                      ;int *
  (height :pointer)                     ;int *
  )

(defcfun "pango_layout_line_get_pixel_extents"
    :void
  (layout_line :pointer)                ;PangoLayoutLine *
  (ink_rect :pointer)                   ;PangoRectangle *
  (logical_rect :pointer)               ;PangoRectangle *
  )

(defcfun "pango_layout_new"
    :pointer
  (context :pointer)                    ;PangoContext *
  )

(defcfun "pango_layout_set_font_description"
    :void
  (layout :pointer)                     ;PangoLayout *
  (desc :pointer)                       ;const PangoFontDescription *
  )

(defcfun "pango_layout_set_single_paragraph_mode"
    :void
  (layout :pointer)                     ;PangoLayout *
  (setting :int)                        ;gboolean
  )

(defcfun "pango_layout_set_spacing"
    :void
  (layout :pointer)                     ;PangoLayout *
  (spacing :int)                        ;int
  )

(defcfun "pango_layout_set_text"
    :void
  (layout :pointer)                     ;PangoLayout *
  (text utf8-string)                    ;const char *
  (length :int)                         ;int
  )
