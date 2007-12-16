;;; -*- Mode: Lisp; -*-

;;;  (c) 2006 David Lichteblau (david@lichteblau.com)
;;; based on the null backend by:
;;;  (c) 2005 Christophe Rhodes (c.rhodes@gold.ac.uk)

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

;;; Locking rule for this file: WITH-GTK is used by functions that
;;; directly call foreign functions from gtk_ or gdk_.


(defmacro until (condition &body body)
  `(do () (,condition) ,@body))

(defmacro while (condition &body body)
  `(until (not ,condition) ,@body))

(defun round-coordinate (x)
  ;; Das machen wir einfach mal wie in CLIM-CLX.
  (floor (+ x .5)))


;;;; GTKAIRO-PORT

(defclass gtkairo-pointer (standard-pointer)
  ((port :initarg :port :accessor port)
   (cursor :accessor pointer-cursor :initform :upper-left)))

(defclass gtkairo-port (basic-port)
  ((pointer :accessor port-pointer)
   (events-head :accessor events-head)
   (events-tail :accessor events-tail)
   (widgets->sheets :initform (make-hash-table) :accessor widgets->sheets)
   (dirty-mediums :initform (make-hash-table) :accessor dirty-mediums)
   (gdk-metrik-medium :accessor gdk-metrik-medium)
   (cairo-metrik-medium :accessor cairo-metrik-medium)
   (pointer-grab-sheet :accessor pointer-grab-sheet :initform nil)
   (global-pango-context :accessor global-pango-context)))

;;;(defmethod print-object ((object gtkairo-port) stream)
;;;  (print-unreadable-object (object stream :identity t :type t)
;;;    (format stream "~S ~S" :id (slot-value object 'id))))

(defun parse-gtkairo-server-path (path)
  path)

;;; FIXME: if :port-type and :server-path-parser aren't CLIM-specified
;;; keywords, they should be altered to be in some mcclim-internal
;;; package instead.
(setf (get :gtkairo :port-type) 'gtkairo-port)
(setf (get :gtkairo :server-path-parser) 'parse-gtkairo-server-path)

(defmethod initialize-instance :after ((port gtkairo-port) &rest initargs)
  (declare (ignore initargs))
  (setf (events-head port) (list nil))
  (setf (events-tail port) (events-head port))
  (setf (port-pointer port) (make-instance 'gtkairo-pointer :port port))
  (push (make-graft port) (climi::port-grafts port))
  ;; FIXME: it seems bizarre for this to be necessary
  (push (make-instance 'gtkairo-frame-manager :port port)
	(slot-value port 'climi::frame-managers))
  (when (zerop *g-threads-got-initialized*)
    (g_thread_init (cffi:null-pointer))
    (gdk_threads_init)
    #-(or win32 windows mswindows)
    (gdk_error_trap_push))
  (with-gtk ()
    ;; FIXME: hier koennten wir mindestens ein anderes --display uebergeben
    ;; wenn wir wollten
    (gtk_init (cffi:null-pointer) (cffi:null-pointer))
    (let* ((root (gdk_screen_get_root_window (gdk_screen_get_default)))
	   (cr (gdk_cairo_create root)))
      (set-antialias cr)
      (setf (gdk-metrik-medium port)
            (make-instance 'gdk-metrik-medium
	      :port port
	      :gc (gdk_gc_new root)))
      (setf (cairo-metrik-medium port)
            (make-instance 'cairo-metrik-medium
	      :port port
	      :cr cr)))
    (setf (global-pango-context port) (gdk_pango_context_get)))
  (when clim-sys:*multiprocessing-p*
    (start-event-thread port)))

(defmethod destroy-port :before ((port gtkairo-port))
  )

(defun start-event-thread (port)
  (setf (climi::port-event-process port)
	(clim-sys:make-process
	 (lambda ()
	   (loop
	     (with-simple-restart
		 (restart-event-loop "Restart CLIM's event loop.")
	       (loop
		 (process-next-event port)))))
	 :name (format nil "~S's event process." port))))


;;;; Mirrors

(defclass mirror ()
    ((mediums :initform '() :accessor mirror-mediums)
     (region :initform nil :accessor mirror-region)))

(defclass widget-mirror (mirror)
    ((port :initarg :port :accessor mirror-port)
     (widget :initarg :widget :accessor mirror-widget)
     (mediums :initform '() :accessor mirror-mediums)
     (buffering-pixmap-dirty-p
      :initform t
      :accessor buffering-pixmap-dirty-p)
     (buffering-pixmap :initform nil :accessor mirror-buffering-pixmap)))

(defclass window-mirror (widget-mirror)
    ((window :initarg :window :accessor mirror-window)))

(defclass native-widget-mirror (widget-mirror)
    ((fixed :initarg :fixed :accessor mirror-fixed)))

(defclass drawable-mirror (mirror)
    ((drawable :initarg :drawable
	       :accessor mirror-drawable
	       :accessor mirror-real-drawable)
     (mediums :initform '() :accessor mirror-mediums)))

(defmethod mirror-real-drawable ((mirror widget-mirror))
  (gtkwidget-gdkwindow (mirror-widget mirror)))

(defmethod mirror-drawable ((mirror widget-mirror))
  (let ((sheet (climi::port-lookup-sheet (mirror-port mirror) mirror)))
    (if (climi::pane-double-buffering sheet)
	(or (mirror-buffering-pixmap mirror)
	    (setf (mirror-buffering-pixmap mirror)
		  (let* ((window (mirror-real-drawable mirror))
			 (region (climi::sheet-mirror-region sheet))
			 (width (floor (bounding-rectangle-max-x region)))
			 (height (floor (bounding-rectangle-max-y region)))
			 (pixmap (gdk_pixmap_new window width height -1))
			 (cr (gdk_cairo_create pixmap)))
		    (set-antialias cr)
		    (cairo_set_source_rgba cr 1.0d0 1.0d0 1.0d0 1.0d0)
		    (cairo_paint cr)
		    (cairo_destroy cr)
		    pixmap)))
	(mirror-real-drawable mirror))))

(defun widget->sheet (widget port)
  (gethash (cffi:pointer-address widget) (widgets->sheets port)))

(defun (setf widget->sheet) (newval widget port)
  (let ((address (cffi:pointer-address widget))
	(table (widgets->sheets port)))
    (if newval
	(setf (gethash address table) newval)
	(remhash address table))))

;;; Pixmaps: The spec doesn't say what a pixmap is, but the McCLIM
;;; frontend wants us to use its PIXMAP class.  So every backend
;;; copy&pastes PORT-(DE)ALLOCATE-PIXMAP to conform.  Why doesn't BASIC-PORT
;;; do this for us then?
(defmethod port-allocate-pixmap ((port gtkairo-port) sheet width height)
  (let ((pixmap (make-instance 'gtkairo-pixmap
		  :sheet sheet
		  :width width
		  :height height
		  :port port)))
    (when (sheet-grafted-p sheet)
      (realize-mirror port pixmap))
    pixmap))

(defmethod port-deallocate-pixmap ((port gtkairo-port) pixmap)
  (when (climi::port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))


;;;; REALIZE-MIRROR

(defmethod realize-window-mirror (port sheet type)
  (with-gtk ()
    (let* ((q (compose-space sheet))
	   (window (gtk_window_new type))
	   (widget (gtk_fixed_new))
	   (width (round-coordinate (space-requirement-width q)))
	   (height (round-coordinate (space-requirement-height q)))
	   (mirror (make-instance 'window-mirror
		     :port port
		     :window window
		     :widget widget)))
      (gtk_window_set_title window (frame-pretty-name (pane-frame sheet)))
      (setf (widget->sheet widget port) sheet)
      (setf (widget->sheet window port) sheet)
      (connect-signals widget)
      (connect-window-signals window)
      (gtk_widget_set_double_buffered widget 0)
      (gtk_window_set_default_size window width height)
      (gtk_container_add window widget)
      (climi::port-register-mirror (port sheet) sheet mirror)
      mirror)))

(defmethod realize-mirror
    ((port gtkairo-port) (sheet climi::top-level-sheet-pane))
  (let ((mirror (realize-window-mirror port sheet GTK_WINDOW_TOPLEVEL)))
    (port-enable-sheet port sheet)
    mirror))

(defmethod realize-mirror
    ((port gtkairo-port) (sheet climi::unmanaged-top-level-sheet-pane))
  (realize-window-mirror port sheet GTK_WINDOW_POPUP))

(defmacro with-gdkcolor ((var clim-color) &body body)
  `(invoke-with-gdkcolor (lambda (,var) ,@body) ,clim-color))

(defun invoke-with-gdkcolor (fn clim-color)
  (cffi:with-foreign-object (c 'gdkcolor)
    (setf (cffi:foreign-slot-value c 'gdkcolor 'pixel) 0)
    (setf (values (cffi:foreign-slot-value c 'gdkcolor 'r)
		  (cffi:foreign-slot-value c 'gdkcolor 'g)
		  (cffi:foreign-slot-value c 'gdkcolor 'b))
	  (multiple-value-bind (r g b)
	      (color-rgb clim-color)
	    (values (min (truncate (* r 65536)) 65535)
		    (min (truncate (* g 65536)) 65535)
		    (min (truncate (* b 65536)) 65535))))
    (funcall fn c)))

(defun gtk-widget-modify-bg (widget color)
  (with-gdkcolor (c color)
    (gtk_widget_modify_bg widget 0 c)))

(defun gtk-widget-modify-fg (widget color)
  (with-gdkcolor (c color)
    (gtk_widget_modify_fg widget 0 c)))

;; copy&paste from port.lisp|CLX:
(defun sheet-desired-color (sheet)
  (typecase sheet
    (sheet-with-medium-mixin
      (medium-background sheet))
    (basic-pane
      ;; CHECKME [is this sensible?] seems to be
      (let ((background (pane-background sheet)))
	(if (typep background 'color)
	    background
	    +white+)))
    (t
      +white+)))

(defmethod container-put ((parent sheet) parent-widget child x y)
  (gtk_fixed_put parent-widget child x y))

(defmethod container-move ((parent sheet) parent-widget child x y)
  (gtk_fixed_move parent-widget child x y))

(defmethod realize-mirror ((port gtkairo-port) (sheet mirrored-sheet-mixin))
  (with-gtk ()
    (let* ((parent (sheet-mirror (sheet-parent sheet)))
	   (q (compose-space sheet))
	   (widget (gtk_fixed_new))
	   (width (round-coordinate (space-requirement-width q)))
	   (height (round-coordinate (space-requirement-height q)))
	   (mirror (make-instance 'widget-mirror :port port :widget widget)))
      (setf (widget->sheet widget port) sheet)
      ;; Das machen wir uns mal einfach und geben jedem Widget sein eigenes
      ;; Fenster, dann haben wir naemlich das Koordinatensystem und Clipping
      ;; wie aus CLX bekannt.  An und fuer sich koennten wir das aber auch
      ;; selbst per Cairo erledigen.  Muss man sich langfristig mal ueberlegen.
      (gtk_fixed_set_has_window widget 1)
      (gtk_widget_set_double_buffered widget 0)
      (connect-signals widget)
      (gtk_widget_set_size_request widget width height)
      (multiple-value-bind (x y)
	  (transform-position (climi::%sheet-mirror-transformation sheet) 0 0)
	(setf x (round-coordinate x))
	(setf y (round-coordinate y))
	(container-put (sheet-parent sheet) (mirror-widget parent) widget x y))
      (climi::port-register-mirror (port sheet) sheet mirror)
      (gtk-widget-modify-bg widget (sheet-desired-color sheet))
      (when (sheet-enabled-p sheet)
	(gtk_widget_show widget))
      mirror)))

(defclass native-widget-mixin ()
    ((widget :initform nil :accessor native-widget)))

(defclass gtk-menu (basic-pane)
    ((label :initarg :label :accessor gtk-menu-label)
     (command-table :initform nil
		    :initarg :command-table
		    :accessor gtk-menu-command-table)))

(defclass gtk-nonmenu (basic-pane)
    ((label :initarg :label :accessor gtk-nonmenu-label)
     (callback :initarg :value-changed-callback
	       :accessor gtk-nonmenu-callback)))

(defclass gtk-menu-bar (native-widget-mixin
			sheet-multiple-child-mixin
			basic-pane)
    ((contents :initarg :contents :accessor gtk-menu-bar-contents)))

(defmethod realize-mirror ((port gtkairo-port) (sheet native-widget-mixin))
  (with-gtk ()
    (setf (native-widget sheet) (realize-native-widget sheet))
    (let* ((widget (native-widget sheet))
	   (parent (sheet-mirror (sheet-parent sheet)))
	   (q (compose-space sheet))
	   (fixed (gtk_fixed_new))
	   (width (round-coordinate (space-requirement-width q)))
	   (height (round-coordinate (space-requirement-height q)))
	   (mirror
	    (make-instance 'native-widget-mirror
	      :port port
	      :fixed fixed
	      :widget widget)))
      (setf (widget->sheet fixed port) sheet)
      (setf (widget->sheet widget port) sheet)
      (gtk_fixed_set_has_window fixed 1)
      (connect-native-signals sheet widget)
      (gtk_widget_set_size_request fixed width height)
      (gtk_widget_set_size_request widget width height)
      (multiple-value-bind (x y)
	  (transform-position (climi::%sheet-mirror-transformation sheet) 0 0)
	(setf x (round-coordinate x))
	(setf y (round-coordinate y))
	(container-put (sheet-parent sheet) (mirror-widget parent) fixed x y))
      (gtk_fixed_put fixed widget 0 0)
      (climi::port-register-mirror (port sheet) sheet mirror)
      (when (sheet-enabled-p sheet)
	(gtk_widget_show_all fixed))
      mirror)))

(defclass menu-mirror (widget-mirror)
    ((menu-item :initarg :menu-item :reader mirror-menu-item)
     (menu :initarg :menu :reader mirror-menu)))

(defclass nonmenu-mirror (widget-mirror)
    ((menu-item :initarg :menu-item :reader mirror-menu-item)))

(defmethod realize-mirror :after ((port gtkairo-port) (sheet gtk-menu-bar))
  (dolist (menu (gtk-menu-bar-contents sheet))
    (unless (integerp menu)		;?
      (sheet-adopt-child sheet menu))))

(defmethod realize-mirror ((port gtkairo-port) (sheet gtk-menu))
  (unless (climi::port-lookup-mirror port sheet)
    (with-gtk ()
      (let* ((menu-item (gtk_menu_item_new_with_label (gtk-menu-label sheet)))
	     (menu (gtk_menu_new))
	     (parent (sheet-mirror (sheet-parent sheet)))
	     (mirror
	      (make-instance 'menu-mirror :menu menu :menu-item menu-item)))
	(setf (widget->sheet menu-item port) sheet)
	(setf (widget->sheet menu port) sheet)
	(append-menu-items port sheet menu (gtk-menu-command-table sheet))
	(gtk_menu_item_set_submenu menu-item menu)
	(gtk_menu_shell_append (mirror-widget parent) menu-item)
	(climi::port-register-mirror (port sheet) sheet mirror)
	(when (sheet-enabled-p sheet)
	  (gtk_widget_show_all menu-item))
	mirror))))

(defmethod realize-mirror ((port gtkairo-port) (sheet gtk-nonmenu))
  (unless (climi::port-lookup-mirror port sheet)
    (with-gtk ()
      (let* ((menu-item
	      (gtk_menu_item_new_with_label (gtk-nonmenu-label sheet)))
	     (parent (sheet-mirror (sheet-parent sheet)))
	     (mirror (make-instance 'nonmenu-mirror :menu-item menu-item)))
	(setf (widget->sheet menu-item port) sheet)
	(connect-signal menu-item "activate" 'magic-clicked-handler)
	(gtk_menu_shell_append (mirror-widget parent) menu-item)
	(climi::port-register-mirror (port sheet) sheet mirror)
	(when (sheet-enabled-p sheet)
	  (gtk_widget_show_all menu-item))
	mirror))))

(defmethod realize-mirror ((port gtkairo-port) (pixmap-sheet climi::pixmap))
  (unless (climi::port-lookup-mirror port pixmap-sheet)
    (let* ((drawable
	    (mirror-drawable
	     (sheet-direct-mirror (climi::pixmap-sheet pixmap-sheet))))
	   (w (round (pixmap-width pixmap-sheet)))
	   (h (round (pixmap-height pixmap-sheet)))
	   (pixmap (gdk_pixmap_new drawable w h -1))
	   (mirror (make-instance 'drawable-mirror :drawable pixmap))
	   (gc (gdk_gc_new pixmap)))
      (cffi:with-foreign-object (c 'gdkcolor)
	(setf (cffi:foreign-slot-value c 'gdkcolor 'pixel) 0)
	(setf (values (cffi:foreign-slot-value c 'gdkcolor 'r)
		      (cffi:foreign-slot-value c 'gdkcolor 'g)
		      (cffi:foreign-slot-value c 'gdkcolor 'b))
	      (values 65535 65535 65535))
	(gdk_gc_set_rgb_fg_color gc c))
      (gdk_draw_rectangle pixmap gc 1 0 0 w h)
      (gdk_gc_unref gc)
      (climi::port-register-mirror port pixmap-sheet mirror)
      mirror)))


;;;; DESTROY-MIRROR

(defun destroy-window-mirror (port sheet)
  (with-gtk ()
    (let ((mirror (climi::port-lookup-mirror port sheet)))
      (destroy-mediums mirror)
      (gtk_widget_destroy (mirror-window mirror))
      (gtk-main-iteration port)
      (climi::port-unregister-mirror port sheet mirror)
      (setf (widget->sheet (mirror-widget mirror) port) nil))))

(defun destroy-mediums (mirror)
  (mapc #'destroy-medium (mirror-mediums mirror))
  (setf (mirror-mediums mirror) '()))

(defmethod destroy-mirror
    ((port gtkairo-port) (sheet climi::top-level-sheet-pane))
  (destroy-window-mirror port sheet))

(defmethod destroy-mirror
    ((port gtkairo-port) (sheet climi::unmanaged-top-level-sheet-pane))
  (destroy-window-mirror port sheet))

(defmethod destroy-mirror ((port gtkairo-port) (sheet mirrored-sheet-mixin))
  (with-gtk ()
    (let ((mirror (climi::port-lookup-mirror port sheet)))
      (destroy-mediums mirror)
      (gtk_widget_destroy (mirror-widget mirror))
      (when (mirror-buffering-pixmap mirror)
	(gdk_drawable_unref (mirror-drawable mirror)))
      (climi::port-unregister-mirror port sheet mirror)
      (setf (widget->sheet (mirror-widget mirror) port) nil))))

(defmethod destroy-mirror :after
	   ((port gtkairo-port) (sheet native-widget-mixin))
  (setf (native-widget sheet) nil))

(defmethod destroy-mirror ((port gtkairo-port) (pixmap-sheet climi::pixmap))
  (with-gtk ()
    (let ((mirror (climi::port-lookup-mirror port pixmap-sheet)))
      (when mirror
	(destroy-mediums mirror)
	(gdk_drawable_unref (mirror-drawable mirror))
	(climi::port-unregister-mirror port pixmap-sheet mirror)))))

(defmethod destroy-mirror ((port gtkairo-port) (pixmap-sheet gtk-menu))
  (with-gtk ()
    (let ((mirror (climi::port-lookup-mirror port pixmap-sheet)))
      (when mirror
	(climi::port-unregister-mirror port pixmap-sheet mirror)))))

(defmethod destroy-mirror ((port gtkairo-port) (pixmap-sheet gtk-nonmenu))
  (with-gtk ()
    (let ((mirror (climi::port-lookup-mirror port pixmap-sheet)))
      (when mirror
	(climi::port-unregister-mirror port pixmap-sheet mirror)))))


;;;; Positioning and resizing

(defun reset-mediums (mirror)
  (mapc #'destroy-medium (mirror-mediums mirror))
  (when (mirror-buffering-pixmap mirror)
    (let* ((old (mirror-buffering-pixmap mirror))
	   (new (progn
		  (setf (mirror-buffering-pixmap mirror) nil)
		  (mirror-drawable mirror)))
	   (gc (gdk_gc_new new)))
      (gdk_draw_drawable new gc old 0 0 0 0 -1 -1)
      (gdk_gc_unref gc)
      (gdk_drawable_unref old))
    (setf (buffering-pixmap-dirty-p mirror) t)))

(defmethod port-set-mirror-region
    ((port gtkairo-port) (mirror window-mirror) mirror-region)
  (with-gtk ()
    (gtk_window_resize (mirror-window mirror)
		       (floor (bounding-rectangle-max-x mirror-region))
		       (floor (bounding-rectangle-max-y mirror-region)))
    (reset-mediums mirror)
    ;; Nanu, ohne die Geometrie hier zu korrigieren kann das Fenster nur
    ;; vergroessert, nicht aber wieder verkleinert werden.
    (cffi:with-foreign-object (geometry 'gdkgeometry)
      (setf (cffi:foreign-slot-value geometry 'gdkgeometry 'min_width) 1)
      (setf (cffi:foreign-slot-value geometry 'gdkgeometry 'min_height) 1)
      (gtk_window_set_geometry_hints (mirror-window mirror)
				     (mirror-window mirror)
				     geometry
				     2))))

(defmethod port-set-mirror-region
    ((port gtkairo-port) (mirror mirror) mirror-region)
  (unless (and (mirror-region mirror)
	       (region-equal (mirror-region mirror) mirror-region))
    (with-gtk ()
      (gtk_widget_set_size_request
       (mirror-widget mirror)
       (floor (bounding-rectangle-max-x mirror-region))
       (floor (bounding-rectangle-max-y mirror-region)))
      (reset-mediums mirror))
    (setf (mirror-region mirror) mirror-region)))

(defmethod port-set-mirror-region
    ((port gtkairo-port) (mirror native-widget-mirror) mirror-region)
  (with-gtk ()
    (let ((w (floor (bounding-rectangle-max-x mirror-region)))
	  (h (floor (bounding-rectangle-max-y mirror-region))))
      (gtk_widget_set_size_request (mirror-fixed mirror) w h)
      (gtk_widget_set_size_request (mirror-widget mirror) w h))))

(defmethod port-set-mirror-transformation
    ((port gtkairo-port) (mirror window-mirror) mirror-transformation)
  (with-gtk ()
    (multiple-value-bind (x y)
	(transform-position mirror-transformation 0 0)
      (gtk_window_move (mirror-window mirror) (floor x) (floor y)))
    (reset-mediums mirror)))

(defmethod port-set-mirror-transformation
    ((port gtkairo-port) (mirror mirror) mirror-transformation)
  (with-gtk ()
    (let* ((w (mirror-widget mirror))
	   (parent-sheet (sheet-parent (climi::port-lookup-sheet port mirror)))
	   (parent (cffi:foreign-slot-value w 'gtkwidget 'parent)))
      (multiple-value-bind (x y)
	  (transform-position mirror-transformation 0 0)
	(container-move parent-sheet parent w (floor x) (floor y))))))

(defmethod port-set-mirror-transformation
    ((port gtkairo-port) (mirror native-widget-mirror) mirror-transformation)
  (with-gtk ()
    (let* ((w (mirror-fixed mirror))
	   (parent-sheet (sheet-parent (climi::port-lookup-sheet port mirror)))
	   (parent (cffi:foreign-slot-value w 'gtkwidget 'parent)))
      (multiple-value-bind (x y)
	  (transform-position mirror-transformation 0 0)
	(container-move parent-sheet parent w (floor x) (floor y))))))


;;;; An und aus

(defmethod port-enable-sheet
    ((port gtkairo-port) (sheet mirrored-sheet-mixin))
  (with-gtk ()
    (let ((mirror (climi::port-lookup-mirror port sheet)))
      (gtk_widget_show (mirror-widget mirror)))))

(defmethod port-enable-sheet
    ((port gtkairo-port) (sheet native-widget-mixin))
  (with-gtk ()
    (let ((mirror (climi::port-lookup-mirror port sheet)))
      (gtk_widget_show_all (mirror-fixed mirror)))))

(defmethod port-enable-sheet
    ((port gtkairo-port) (sheet climi::top-level-sheet-pane))
  (with-gtk ()
    (let ((mirror (climi::port-lookup-mirror port sheet)))
      (gtk_widget_show (mirror-window mirror))
      (gtk_widget_show (mirror-widget mirror)))))

(defmethod port-enable-sheet
    ((port gtkairo-port) (sheet climi::unmanaged-top-level-sheet-pane))
  (with-gtk ()
    (let ((mirror (climi::port-lookup-mirror port sheet)))
      (gtk_widget_show (mirror-window mirror))
      (gtk_widget_show (mirror-widget mirror)))))

(defmethod port-disable-sheet
    ((port gtkairo-port) (sheet mirrored-sheet-mixin))
  (with-gtk ()
    (let ((mirror (climi::port-lookup-mirror port sheet)))
      (gtk_widget_hide (mirror-widget mirror)))))

(defmethod port-disable-sheet
    ((port gtkairo-port) (sheet native-widget-mixin))
  (with-gtk ()
    (let ((mirror (climi::port-lookup-mirror port sheet)))
      (gtk_widget_hide_all (mirror-fixed mirror)))))

(defmethod port-disable-sheet
    ((port gtkairo-port) (sheet climi::top-level-sheet-pane))
  (with-gtk ()
    (let ((mirror (climi::port-lookup-mirror port sheet)))
      (gtk_widget_hide (mirror-window mirror)))))


;;;; JUNK

(defmethod mirror-transformation ((port gtkairo-port) mirror)
  ())

;; die sind verkehrt, die gibt's gar nicht
;;;(defmethod port-set-sheet-transformation
;;;    ((port gtkairo-port) (graft graft) transformation)
;;;  ())
;;;
;;;(defmethod port-set-sheet-transformation
;;;    ((port gtkairo-port) (sheet mirrored-sheet-mixin) transformation)
;;;  ())


;;;; Vermischtes

(defmethod port-motion-hints
    ((port gtkairo-port) (sheet mirrored-sheet-mixin))
  (with-gtk ()
    (logtest GDK_POINTER_MOTION_HINT_MASK
	     (gtk_widget_get_events
	      (mirror-widget (sheet-direct-mirror sheet))))))

(defmethod (setf port-motion-hints)
    (value (port gtkairo-port) (sheet mirrored-sheet-mixin))
  (with-gtk ()
    (let* ((widget (mirror-widget (sheet-direct-mirror sheet)))
	   (oldval (gtk_widget_get_events widget))
	   (newval
	    (if value
		(logior oldval GDK_POINTER_MOTION_HINT_MASK)
		(logandc2 oldval GDK_POINTER_MOTION_HINT_MASK))))
      (gtk_widget_set_events widget newval))))

(defmethod make-graft
    ((port gtkairo-port) &key (orientation :default) (units :device))
  (make-instance 'gtkairo-graft
                 :port port :mirror (gensym)
                 :orientation orientation :units units))

(defmethod text-style-mapping
    ((port gtkairo-port) text-style &optional character-set)
  (error "text-style-mapping called, what now?"))

(defmethod (setf text-style-mapping)
    (font-name (port gtkairo-port)
     (text-style text-style) &optional character-set)
  (error "(setf text-style-mapping) called, what now?"))

(defmethod port-character-width ((port gtkairo-port) text-style char)
  (error "port-character-width called, what now?"))

(defmethod port-string-width
    ((port gtkairo-port) text-style string &key (start 0) end)
  (error "port-string-width called, what now?"))

(defmethod port-mirror-width ((port gtkairo-port) sheet)
  (cffi:with-foreign-object (r 'gtkrequisition)
    (gtk_widget_size_request
     (mirror-widget (climi::port-lookup-mirror port sheet))
     r)
    (cffi:foreign-slot-value r 'gtkrequisition 'width)))

(defmethod port-mirror-height ((port gtkairo-port) sheet)
  (cffi:with-foreign-object (r 'gtkrequisition)
    (gtk_widget_size_request
     (mirror-widget (climi::port-lookup-mirror port sheet))
     r)
    (cffi:foreign-slot-value r 'gtkrequisition 'height)))

(defmethod port-mirror-width ((port gtkairo-port) (sheet gtkairo-graft))
  (graft-width sheet))

(defmethod port-mirror-height ((port gtkairo-port) (sheet gtkairo-graft))
  (graft-height sheet))

(defmethod graft ((port gtkairo-port))
  (first (climi::port-grafts port)))

(defun %gdk-display-get-pointer ()
  (with-gtk ()
    (cffi:with-foreign-object (x :int)
      (cffi:with-foreign-object (y :int)
	(cffi:with-foreign-object (mask :int)
	  (gdk_display_get_pointer (gdk_display_get_default)
				   (cffi:null-pointer) ;FIXME: screen
				   x
				   y
				   mask)
	  (values
	   (cffi:mem-aref x :int)
	   (cffi:mem-aref y :int)
	   (cffi:mem-aref mask :int)))))))

(defun mirror-pointer-position (mirror)
  (with-gtk ()
    (cffi:with-foreign-object (x :int)
      (cffi:with-foreign-object (y :int)
	(gtk_widget_get_pointer (mirror-widget mirror) x y)
	(values
	 (cffi:mem-aref x :int)
	 (cffi:mem-aref y :int))))))

(defmethod pointer-position ((pointer gtkairo-pointer))
  (%gdk-display-get-pointer))

(defmethod pointer-button-state ((pointer gtkairo-pointer))
  (with-gtk ()
    (gdkmodifiertype->all-buttons (nth-value 2 (%gdk-display-get-pointer)))))

(defmethod port-modifier-state ((port gtkairo-port))
  (with-gtk ()
    (gdkmodifiertype->modifier-state (nth-value 2 (%gdk-display-get-pointer)))))

;;; port.lisp|CLX says:
;;;   XXX Should we rely on port-pointer-sheet being correct? -- moore
(defmethod synthesize-pointer-motion-event ((pointer gtkairo-pointer))
  (with-gtk ()
    (let* ((port (port pointer))
	   (sheet (climi::port-pointer-sheet port)))
      (when sheet
	(let ((mirror (sheet-direct-mirror sheet)))
	  (when mirror
	    (multiple-value-bind (root-x root-y state)
		(%gdk-display-get-pointer)
	      ;; FIXME ;when same-screen-p
	      (multiple-value-bind (x y)
		  (mirror-pointer-position mirror)
		(make-instance 'pointer-motion-event
		  :pointer 0
		  :button (gdkmodifiertype->one-button state)
		  :x x
		  :y y
		  :graft-x root-x
		  :graft-y root-y
		  :sheet sheet
		  :modifier-state (gdkmodifiertype->modifier-state state)
		  ;; The event initialization code will give us a
		  ;; reasonable timestamp.
		  :timestamp 0)))))))))

(defmethod port-frame-keyboard-input-focus ((port gtkairo-port) frame)
  (with-gtk ()
    (let* ((sheet (frame-top-level-sheet frame))
           (mirror (climi::port-lookup-mirror port sheet))
           (widget (gtk_window_get_focus (mirror-window mirror))))
      (if (cffi:null-pointer-p widget)
          nil
          (widget->sheet widget port)))))

(defmethod (setf port-frame-keyboard-input-focus) 
    (focus (port gtkairo-port) frame)
  (with-gtk ()
    ;; could use gtk_window_set_focus here for symmetry, but we don't
    ;; have to.
    (gtk_widget_grab_focus (mirror-widget (sheet-mirror focus))))
  focus)

(defmethod port-force-output ((port gtkairo-port))
  (with-gtk ()
    (loop
	for medium being each hash-key in (dirty-mediums port)
	do (medium-force-output medium))
    (gdk_display_flush (gdk_display_get_default))
    ;; Don't know whether p-f-o is actually meant to XSync, which is
    ;; what gdk_flush does.  But it seems useful to have _some_ function
    ;; for this, so let's use p-f-o until we find a better one.
    (gdk_flush)
    (dribble-x-errors)))

;; FIXME: What happens when CLIM code calls tracking-pointer recursively?
(defmethod port-grab-pointer ((port gtkairo-port) pointer sheet)
  (with-gtk ()
    (let* ((gtkwidget (mirror-widget (sheet-direct-mirror sheet)))
	   (status
	    (gdk_pointer_grab (gtkwidget-gdkwindow gtkwidget)
			      0
			      (logior GDK_POINTER_MOTION_MASK
				      GDK_BUTTON_PRESS_MASK
				      GDK_BUTTON_RELEASE_MASK)
			      (cffi:null-pointer)
			      (cffi:null-pointer)
			      GDK_CURRENT_TIME)))
      ;; emergency ungrab:
;;;      (sb-thread:make-thread (lambda ()
;;;			       (sleep 10)
;;;			       (tr :timeout!)
;;;			       (with-gtk ()
;;;				 (gdk_pointer_ungrab GDK_CURRENT_TIME))))
      (when (zerop status)
	(setf (pointer-grab-sheet port) sheet)))))

(defmethod port-ungrab-pointer ((port gtkairo-port) pointer sheet)
  (declare (ignore pointer sheet))
  (with-gtk ()
    (when (eq (pointer-grab-sheet port) sheet)
      (gdk_pointer_ungrab GDK_CURRENT_TIME)
      (setf (pointer-grab-sheet port) nil))))

(defmethod distribute-event :around ((port gtkairo-port) event)
  (let ((grab-sheet (pointer-grab-sheet port)))
    (if grab-sheet
	(queue-event grab-sheet event)
	(call-next-method))))

(defmethod set-sheet-pointer-cursor ((port gtkairo-port) sheet cursor)
  ())        

(defmethod bind-selection ((port gtkairo-port) window &optional time)
  ())

(defmethod release-selection ((port gtkairo-port) &optional time)
  ())

(defmethod request-selection ((port gtkairo-port) requestor time)
  ())

(defmethod get-selection-from-event ((port gtkairo-port) event)
  ())

(defmethod send-selection ((port gtkairo-port) event string)
  nil)
