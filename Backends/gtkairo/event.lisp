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

;;; Locking rule for this file: The entire event loop grabs the GTK
;;; lock, individual callees don't.

(defvar *keys* (make-hash-table))

(defmacro define-key (name &rest clauses)
  `(setf (gethash ,name *keys*) ',clauses))

(defun connect-signal (widget name sym)
  (g-signal-connect widget name (cffi:get-callback sym)))

(defun connect-signals (widget)
  (gtk_widget_add_events widget
			 (logior GDK_POINTER_MOTION_MASK
				 GDK_BUTTON_PRESS_MASK
				 GDK_BUTTON_RELEASE_MASK
				 GDK_KEY_PRESS_MASK
				 GDK_KEY_RELEASE_MASK
				 GDK_ENTER_NOTIFY_MASK
				 GDK_LEAVE_NOTIFY_MASK
				 #+nil GDK_STRUCTURE_MASK))
  (setf (gtkwidget-flags widget)
        (logior (gtkwidget-flags widget) GTK_CAN_FOCUS))
  (connect-signal widget "expose-event" 'expose-handler)
  (connect-signal widget "motion-notify-event" 'motion-notify-handler)
  (connect-signal widget "button-press-event" 'button-handler)
  (connect-signal widget "button-release-event" 'button-handler)
  (connect-signal widget "key-press-event" 'key-handler)
  (connect-signal widget "key-release-event" 'key-handler)
  (connect-signal widget "enter-notify-event" 'enter-handler)
  (connect-signal widget "leave-notify-event" 'leave-handler)
  (connect-signal widget "configure-event" 'configure-handler)
  ;; override gtkwidget's focus handlers, which trigger an expose event,
  ;; causing unnecessary redraws for mouse movement
  (connect-signal widget "focus-in-event" 'noop-handler)
  (connect-signal widget "focus-out-event" 'noop-handler))

(defun connect-window-signals (widget)
  (gtk_widget_add_events widget (logior GDK_STRUCTURE_MASK
					GDK_SUBSTRUCTURE_MASK))
  (connect-signal widget "configure-event" 'configure-handler)
  (connect-signal widget "delete-event" 'delete-handler)
  (connect-signal widget "destroy-event" 'destroy-handler))

(defvar *port*)

(defun enqueue (event &optional (port *port*))
;;;  (tr event)
;;;  (tr (event-sheet event))
  (push event (cdr (events-tail port)))
  (pop (events-tail port))
  event)

(defun tr (&rest x)
  (when x
    (format *trace-output* "~&~A~&" x)
    (finish-output *trace-output*))
  x)

(defun dequeue (port)
  (with-gtk ()				;let's simply use the gtk lock here
    (let ((c (cdr (events-head port))))
      (when c
	(pop (events-head port))
	(car c)))))

(defun dribble-x-errors ()
  #-(or win32 windows mswindows)
  (let ((code (gdk_error_trap_pop)))
    (unless (zerop code)
      (warn "Ignoring X error ~D: ~A"
            code
            (cffi:with-foreign-pointer-as-string (buf 64)
              (XGetErrorText *gdk-display* code buf 63))))
    (gdk_error_trap_push)))

;; thread-safe entry function
(defun gtk-main-iteration (port &optional block)
  (with-gtk ()
    (let ((*port* port))
      (if block
	  (gtk_main_iteration_do 1)
	  (while (plusp (gtk_events_pending))
	    (gtk_main_iteration_do 0))))
    (dribble-x-errors)))

(defmethod get-next-event
    ((port gtkairo-port) &key wait-function (timeout nil))
  (declare (ignore wait-function))
  #-clim-mp (port-force-output port)
  (gtk-main-iteration port)
  (cond
    ((dequeue port))
    (t
      #+clim-gtkairo::do-not-block-in-ffi
      (sb-sys:wait-until-fd-usable (gdk-xlib-fd) :input 0.1)
      #+cmu (mp:process-yield)
      (gtk-main-iteration port #-clim-gtkairo::do-not-block-in-ffi t)
      (dequeue port))))

(defmacro define-signal (name+options (widget event &rest args) &body body)
  (destructuring-bind (name &key (return-type :void))
      (if (listp name+options)
	  name+options
	  (list name+options))
    (let ((impl (intern (concatenate 'string (symbol-name name) "-IMPL")))
	  (args (if (symbolp event)
		    `((,event :pointer) ,@args)
		    (cons event args))))
      ;; jump through a trampoline so that C-M-x works without having to
      ;; restart:
      `(progn
	 (defun ,impl (,widget ,@(mapcar #'car args))
	   ,@body)
	 (cffi:defcallback ,name ,return-type
	   ((widget :pointer) ,@args (data :pointer))
	   data
	   (,impl widget ,@(mapcar #'car args)))))))

(define-signal noop-handler (widget event))

(define-signal expose-handler (widget event)
  (let* ((sheet (widget->sheet widget *port*))
	 (mirror (climi::port-lookup-mirror *port* sheet)))
    (unless
	;; fixme: this shouldn't happen
	(typep mirror 'drawable-mirror)
      (if (buffering-pixmap-dirty-p mirror)
	  (cffi:with-foreign-slots ((x y width height) event gdkeventexpose)
	    (if (mirror-buffering-pixmap mirror)
		(setf (buffering-pixmap-dirty-p mirror) nil)
		(gdk_window_clear_area (gtkwidget-gdkwindow widget)
				       x y
				       width height))
	    (enqueue
	     (make-instance 'window-repaint-event
	       :timestamp (get-internal-real-time)
	       :sheet (widget->sheet widget *port*)
	       :region (make-rectangle* x y (+ x width) (+ y height)))))
	  (cffi:with-foreign-slots ((x y width height) event gdkeventexpose)
	    (let* ((from (mirror-buffering-pixmap mirror))
		   (to (gtkwidget-gdkwindow (mirror-widget mirror)))
		   (gc (gdk_gc_new to)))
	      (gdk_draw_drawable to gc from x y x y width height)
	      (gdk_gc_unref gc)))))))

(defun gdkmodifiertype->modifier-state (state)
  (logior
   (if (logtest GDK_SHIFT_MASK state) +shift-key+ 0)
   (if (logtest GDK_CONTROL_MASK state) +control-key+ 0)
   (if (logtest GDK_MOD1_MASK state) +meta-key+ 0)
   ;; (if (logtest GDK_MOD2_MASK state) +super-key+ 0)
   ;; (if (logtest GDK_MOD3_MASK state) +hyper-key+ 0)
;;;   (if (logtest GDK_MOD4_MASK state) ??? 0)
;;;   (if (logtest GDK_MOD5_MASK state) ??? 0)
;;;   (if (logtest GDK_LOCK_MASK state) ??? 0)
   ))

(defun gdkmodifiertype->one-button (state)
  (cond
    ((logtest GDK_BUTTON1_MASK state) +pointer-left-button+)
    ((logtest GDK_BUTTON2_MASK state) +pointer-middle-button+)
    ((logtest GDK_BUTTON3_MASK state) +pointer-right-button+)
    ((logtest GDK_BUTTON4_MASK state) +pointer-wheel-up+)
    ((logtest GDK_BUTTON5_MASK state) +pointer-wheel-down+)
    (t nil)))

(defun gdkmodifiertype->all-buttons (state)
  (logior
   (if (logtest GDK_BUTTON1_MASK state) +pointer-left-button+ 0)
   (if (logtest GDK_BUTTON2_MASK state) +pointer-middle-button+ 0)
   (if (logtest GDK_BUTTON3_MASK state) +pointer-right-button+ 0)
   (if (logtest GDK_BUTTON4_MASK state) +pointer-wheel-up+ 0)
   (if (logtest GDK_BUTTON5_MASK state) +pointer-wheel-down+ 0)))

(define-signal motion-notify-handler (widget event)
  (gtk_widget_grab_focus widget)
  (enqueue
   (cffi:with-foreign-slots
       ((state x y x_root y_root time) event gdkeventmotion)
     (make-instance 'pointer-motion-event
       :timestamp time
       :pointer 0
       :button (gdkmodifiertype->one-button state)
       :x (truncate x)
       :y (truncate y)
       :graft-x (truncate x_root)
       :graft-y (truncate y_root)
       :sheet (widget->sheet widget *port*)
       :modifier-state (gdkmodifiertype->modifier-state state)))))

(defun state-without-buttons (state)
  (logand state (1- GDK_BUTTON1_MASK)))

;; aus CLIM-CLX geklaut:
(defconstant +clim-modifiers+ '(((:meta-left :meta-right) #.+meta-key+)
				((:hyper-left :hyper-right) #.+hyper-key+)
				((:super-left :super-right) #.+super-key+)
				((:shift-left :shift-right) #.+shift-key+)
				((:control-left :control-right)
				 #.+control-key+)))
(defun modify-modifiers (type keysym-keyword modifiers)
  (let ((keysym-modifier (loop for (keysyms modifier) in +clim-modifiers+
			       if (member keysym-keyword keysyms)
			       return modifier)))
    (cond ((and keysym-modifier (eql type GDK_KEY_PRESS))
	   (logior modifiers keysym-modifier))
	  ((and keysym-modifier (eql type GDK_KEY_RELEASE))
	   (logandc2 modifiers keysym-modifier))
	  (t modifiers))))

(define-signal key-handler (widget event)
  (let ((sheet (widget->sheet widget *port*)))
    (multiple-value-bind (root-x root-y)
	(%gdk-display-get-pointer)
      (multiple-value-bind (x y)
	  (mirror-pointer-position (sheet-direct-mirror sheet))
	(cffi:with-foreign-slots
	    ((type time state keyval string length) event gdkeventkey)
	  (let ((state (state-without-buttons state))
		(modifier-state (gdkmodifiertype->modifier-state state)))
	    (let ((clauses (gethash keyval *keys*))
		  sym char)
	      (loop
		  for (st sy ch) in clauses
		  when (or (eql st t) (find state st))
		  do
		    (setf sym sy)
		    (setf char ch)
		    (return))
	      (unless char
		(setf modifier-state
		      (modify-modifiers type sym modifier-state)))
	      (unless (eq sym 'throw-away)
		(enqueue
		 (make-instance (if (eql type GDK_KEY_PRESS)
				    'key-press-event
				    'key-release-event)
		   :key-name sym
		   :key-character char
		   :x x
		   :y y
		   :graft-x root-x
		   :graft-y root-y
		   :sheet sheet
		   :modifier-state modifier-state
		   :timestamp time))))))))))

(defvar *last-seen-button* 3)

(defgeneric handle-event-p (sheet event))

(defmethod handle-event-p (sheet event)
  t)

(define-signal (button-handler :return-type :int) (widget event)
  (cffi:with-foreign-slots
      ((type time button state x y x_root y_root) event gdkeventbutton)
    (when (eql type GDK_BUTTON_PRESS)
      ;; Hack alert: Menus don't work without this.
      (gdk_pointer_ungrab GDK_CURRENT_TIME))
    (setf *last-seen-button* button)
    (let* ((sheet (widget->sheet widget *port*))
	   (event
	    (make-instance (if (eql type GDK_BUTTON_PRESS)
			       'pointer-button-press-event
			       'pointer-button-release-event)
	      :pointer 0
	      :button (ecase button
			(1 +pointer-left-button+)
			(2 +pointer-middle-button+)
			(3 +pointer-right-button+)
			(4 +pointer-wheel-up+)
			(5 +pointer-wheel-down+))
	      :x (truncate x)
	      :y (truncate y)
	      :graft-x (truncate x_root)
	      :graft-y (truncate y_root)
	      :sheet sheet
	      :modifier-state (gdkmodifiertype->modifier-state state)
	      :timestamp time)))
      (cond
	((handle-event-p sheet event)
	  (enqueue event)
	  1)
	(t
	  0)))))

(define-signal (tab-button-handler :return-type :int) (widget event)
  (cffi:with-foreign-slots
      ((type time button state x y x_root y_root) event gdkeventbutton)
    (when (eql type GDK_BUTTON_PRESS)
      ;; Hack alert: Menus don't work without this.
      (gdk_pointer_ungrab GDK_CURRENT_TIME))
    (setf *last-seen-button* button)
    (let ((page (widget->sheet widget *port*)))
      (enqueue (make-instance
		   (if (eql type GDK_BUTTON_PRESS)
		       'tab-press-event
		       'tab-release-event)
		 :button (ecase button
			   (1 +pointer-left-button+)
			   (2 +pointer-middle-button+)
			   (3 +pointer-right-button+)
			   (4 +pointer-wheel-up+)
			   (5 +pointer-wheel-down+))
		 :page page
		 :sheet (clim-tab-layout:tab-page-tab-layout page)))))
  1)

(define-signal enter-handler (widget event)
  (cffi:with-foreign-slots
      ((time state x y x_root y_root) event gdkeventcrossing)
    ;; The frontend sets p-p-s for us, but apparently that sometimes
    ;; happens too late, leaving NIL in the slot.  Test case is the Drag and
    ;; Drop demo.  (Even weirder: Starting it from demodemo for a second time
    ;; makes the problem go away, only the first invocation has this problem.)
    (setf (climi::port-pointer-sheet *port*) (widget->sheet widget *port*))
    (enqueue
     (make-instance 'pointer-enter-event
       :pointer 0
       :button (gdkmodifiertype->all-buttons state)
       :x x
       :y y
       :graft-x x_root
       :graft-y y_root
       :sheet (widget->sheet widget *port*)
       :modifier-state (gdkmodifiertype->modifier-state state)
       :timestamp time))))

(define-signal leave-handler (widget event)
  (cffi:with-foreign-slots
      ((time state x y x_root y_root gdkcrossingmode) event gdkeventcrossing)
    (enqueue
     (make-instance (if (eql gdkcrossingmode GDK_CROSSING_UNGRAB)
			'climi::pointer-ungrab-event
			'pointer-exit-event)
       :pointer 0
       :button (gdkmodifiertype->all-buttons state)
       :x x
       :y y
       :graft-x x_root
       :graft-y y_root
       :sheet (widget->sheet widget *port*)
       :modifier-state (gdkmodifiertype->modifier-state state)
       :timestamp time))))

(define-signal configure-handler (widget event)
  (cffi:with-foreign-slots ((x y width height) event gdkeventconfigure)
    (let ((sheet (widget->sheet widget *port*)))
      (when sheet			;FIXME
	(enqueue
	 (if (eq (sheet-parent sheet) (graft sheet))
	     (cffi:with-foreign-object (&x :int)
	       (cffi:with-foreign-object (&y :int)
		 ;; FIXME: Does this actually change anything about decoration
		 ;; handling?
		 (gdk_window_get_root_origin (gtkwidget-gdkwindow widget) &x &y)
		 (make-instance 'window-configuration-event
		   :sheet sheet
		   :x (cffi:mem-aref &x :int)
		   :y (cffi:mem-aref &y :int)
		   :width width
		   :height height)))
	     (make-instance 'window-configuration-event
	       :sheet sheet
	       :x x
	       :y y
	       :width width
	       :height height)))))))

(define-signal delete-handler (widget event)
  (enqueue
   (make-instance 'clim:window-manager-delete-event
     :sheet (widget->sheet widget *port*))))

(define-signal destroy-handler (widget event)
  (enqueue  
   (make-instance 'climi::window-destroy-event
     :sheet (widget->sheet widget *port*))))

;; native widget handlers:

(define-signal magic-clicked-handler (widget event)
  (declare (ignore event))
  (when (boundp '*port*)		;hack alert
    (enqueue
     (make-instance 'magic-gadget-event
       :sheet (widget->sheet widget *port*)))))

(define-signal menu-clicked-handler (widget event)
  (declare (ignore event))
  (let ((parent (cffi:foreign-slot-value widget 'gtkwidget 'parent)))
    (enqueue
     (make-instance 'menu-clicked-event
       :sheet (widget->sheet parent *port*)
       :item (widget->sheet widget *port*)))))

(define-signal context-menu-clicked-handler (widget event)
  (declare (ignore event))
  (let ((dummy-item (widget->sheet widget *port*)))
    (enqueue
     (make-instance 'context-menu-clicked-event
       :sheet (dummy-menu-item-sheet-parent dummy-item)
       :value (dummy-menu-item-sheet-value dummy-item)
       :itemspec (dummy-menu-item-sheet-itemspec dummy-item)))))

(define-signal popup-deactivated-handler (widget (menu :pointer))
  menu
  (enqueue
   (make-instance 'context-menu-cancelled-event
     :sheet (widget->sheet widget *port*))))

#-sbcl
(define-signal (scrollbar-change-value-handler :return-type :int)
    (widget (scroll gtkscrolltype) (value :double))
  (enqueue (make-instance 'scrollbar-change-value-event
	     :scroll-type scroll
	     :value value
	     :sheet (widget->sheet widget *port*)))
  1)

#+sbcl
;; :double in callbacks doesn't work:
(define-signal (scrollbar-change-value-handler :return-type :int)
    (widget (scroll gtkscrolltype) (lo :unsigned-int) (hi :int))
  (enqueue (make-instance 'scrollbar-change-value-event
	     :scroll-type scroll
	     :value (sb-kernel:make-double-float hi lo)
	     :sheet (widget->sheet widget *port*)))
  1)

(defvar *later-table* (make-hash-table))
(defvar *later-counter* 0)

(defun invoke-later (fun)
  (with-gtk ()
    (let ((i (incf *later-counter*)))
      (setf (gethash i *later-table*) fun)
      (g_idle_add (cffi:get-callback 'idle-function) (cffi:make-pointer i)))))

(cffi:defcallback idle-function :int
  ((data :long))			;hack
  (let ((fun (gethash data *later-table*)))
    (remhash data *later-table*)
    (funcall fun))
  0)

(cffi:defcallback view-selection-callback :int
  ((selection :pointer)
   (model :pointer)
   (path :pointer)
   (isselected :int)
   (data :pointer))
  selection model path isselected
  (when (boundp '*port*)		;kludge
    (let ((sheet (widget->sheet data *port*)))
      (enqueue (make-instance 'list-selection-event :sheet sheet))))
  1)
