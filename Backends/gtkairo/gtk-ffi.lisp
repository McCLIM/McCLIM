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

(cffi:defctype utf8-string (:string :encoding :utf-8))

#-(or win32 mswindows windows darwin)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:load-foreign-library "libcairo.so")
  (cffi:load-foreign-library "libgthread-2.0.so")
  (cffi:load-foreign-library "libgtk-x11-2.0.so"))

#+darwin
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((cffi:*foreign-library-directories*
	 (cons "/opt/local/lib/" cffi:*foreign-library-directories*)))
    (cffi:load-foreign-library "libcairo.dylib")
    (cffi:load-foreign-library "libgthread-2.0.dylib")
    (cffi:load-foreign-library "libgtk-x11-2.0.dylib")))

#+(or win32 mswindows windows)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:load-foreign-library "libcairo-2.dll")
  (cffi:load-foreign-library "libglib-2.0-0.dll")
  (cffi:load-foreign-library "libgthread-2.0-0.dll")
  (cffi:load-foreign-library "libgobject-2.0-0.dll")
  (cffi:load-foreign-library "libgdk-win32-2.0-0.dll")
  (cffi:load-foreign-library "libgtk-win32-2.0-0.dll")
  (cffi:load-foreign-library "libpangocairo-1.0-0.dll")
  (cffi:load-foreign-library "libpango-1.0-0.dll"))

(defmacro defcfun (name rtype &rest argtypes)
  (if (and (eq rtype 'cairo_status_t)
	   (not (or (equal name "cairo_status")
                    (equal name "cairo_font_face_status"))))
      `(def-cairo-fun ,name ,rtype ,@argtypes)
      `(cffi:defcfun (,name ,(intern (string-upcase name) :clim-gtkairo))
	   ,rtype
	 ,@argtypes)))

(defmacro defcenum (name &rest values)
  `(progn
     (cffi:defcenum ,name ,@values)
     ,@(loop
	   for pair in values
	   for key = (if (listp pair) (car pair) pair)
	   collect `(defconstant ,(intern (symbol-name key) :clim-gtkairo)
			(cffi:foreign-enum-value ',name ,key)))))


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
#+sbcl
(defmacro with-cairo-floats ((&optional) &body body)
  `(unwind-protect
       (progn
	 (sb-int:set-floating-point-modes :traps nil)
	 ,@body)
     (apply #'sb-int:set-floating-point-modes *normal-modes*)))

#+(or scl cmu)
(defmacro with-cairo-floats ((&optional) &body body)
  `(ext:with-float-traps-masked
       (:underflow :overflow :inexact :divide-by-zero :invalid)
     ,@body))

#-(or scl cmu sbcl)
(defmacro with-cairo-floats ((&optional) &body body)
  `(progn ,@body))

(defmacro slot (o c s)
  `(cffi:foreign-slot-value ,o ,c ,s))

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
  (#-cmu progn #+cmu mp:without-scheduling
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
	  (gdk_threads_leave))))))


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
  (string utf8-string))

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

(cffi:defcstruct gdkpoint
  (x :int)
  (y :int))

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

(cffi:defcstruct gtktreeiter
  (stamp :int)
  (user_data :pointer)
  (user_data2 :pointer)
  (user_data3 :pointer))

(cffi:defcstruct gvalue
  (type :ulong)
  (data0 :uint64)
  (data1 :uint64))


(defconstant GTK_WINDOW_TOPLEVEL 0)
(defconstant GTK_WINDOW_POPUP 1)

(defun g-signal-connect (object event callback &optional data)
  (g_signal_connect_data object
			 event
			 callback
			 (or data (cffi:null-pointer))
			 (cffi:null-pointer)
			 0))

(defcfun "gtk_tree_path_new_from_indices"
    :pointer
  (index :int)
  &rest)

#-(or win32 windows mswindows)
(defcfun "XGetErrorText"
    :int
  (dpy :pointer)                        ;Display *
  (code :int)                           ;int
  (buffer :string)                      ;char *
  (nbytes :int)                         ;int
  )

#-(or win32 windows mswindows)
(defcfun "gdk_x11_drawable_get_xid"
    :unsigned-long
  (drawable :pointer)                   ;GdkDrawable *
  )

(defconstant GDK_CURRENT_TIME 0)

;; fixme: GtkWidgetFlags is an enum, why is it not in the object file?
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

(defconstant PANGO_SCALE 1024)

(cffi:defcstruct PangoRectangle
  (x :int)
  (y :int)
  (width :int)
  (height :int))


;; magic symbols for FFI code generation
(defvar *dummy*
    '(GdkFunction gtkselectionmode GtkScrollType GdkEventMask GdkEventType
      GtkWidgetFlags GdkModifierType GdkCrossingMode GtkWindowType
      GdkGrabStatus GdkWindowHints GtkStateType GdkDragAction GConnectFlags
      GdkDragProtocol GtkPolicyType gdkpoint gdklinestyle

      gdk_x11_drawable_get_xid

      pangostyle pangoweight PangoRectangle PangoFontMetrics

      cairo_format_t cairo_operator_t cairo_fill_rule_t cairo_line_cap_t
      cairo_line_join_t cairo_font_slant_t cairo_font_weight_t cairo_status_t
      cairo_filter_t cairo_extend_t))

(defun test (&optional (port :gtkairo))
  (setf climi::*server-path-search-order* (list port))
  (clim:run-frame-top-level
   (clim:make-application-frame 'clim-demo::address-book)))
