;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2001 by  Julien Boninfante (boninfan@emi.u-bordeaux.fr)

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


; should be in the port structure...
(defparameter *x-visual* nil)
; perhaps not this one
(defparameter *opengl-glx-context* nil)

(defmethod initialize-instance :after ((port opengl-port) &rest rest)
  (declare (ignore rest))
  (push (make-instance 'opengl-frame-manager :port port) (slot-value port 'frame-managers))
  (initialize-signature-count)
  (initialize-opengl-port port))

(defmethod initialize-opengl-port ((port opengl-port))
  (let* ((options (cdr (port-server-path port)))
	 (hostname (getf options :host ""))
	 (screen-id (getf options :screen-id 0)))
    (clim-ffi:with-c-strings ((host (if (string= hostname "localhost")
					""
					hostname)))
      (with-slots (display screen root) port
	(setf display (xlib-gl:XOpenDisplay host)
	      screen (xlib-gl:XScreenOfDisplay display screen-id)
	      root (xlib-gl:XRootWindow display screen-id)))
      (make-graft port))))

(defmethod destroy-port :before ((port opengl-port))
  (let ((display (opengl-port-display port)))
    (when *opengl-glx-context*
      (gl:glXMakeCurrent display xlib-gl:None xlib-gl:Null)
      (xlib-gl:free-xvisualinfo *x-visual*)
      (gl:glXDestroyContext display *opengl-glx-context*)
      (setf *opengl-glx-context* nil
	    *x-visual* nil
	    *current-sheet-signature* 0))
    (xlib-gl:XCloseDisplay display)))


(defmethod bell ((port opengl-port))
  (xlib-gl:XBell (opengl-port-display port) 100))


;; Events

(defun event-not-present-p (port)
  (= (xlib-gl:XPending (opengl-port-display port)) 0))

; The philosophy is to send the pointer button number, ignoring
; the key masks, and returning the higher button corresponding
; to the mask
(defun find-button (mask)
  (cond ((< mask xlib-gl:Button1MotionMask) 0)
	((< mask xlib-gl:Button2MotionMask) 1)
	((< mask xlib-gl:Button3MotionMask) 2)
	((< mask xlib-gl:Button4MotionMask) 3)
	((< mask xlib-gl:Button5MotionMask) 4)
	(t 5)))

; There is a hierarchy in modifier. BUTTON1MOTIONMASK is just
; above the key modifier.
(defmacro key-mod (mask)
  `(mod ,mask xlib-gl:Button1MotionMask))

(defun find-leaf-sheet-containing-point (frame x y)
  (loop with last = nil
        with prev = nil
        while (setf prev last
                    last (child-containing-position (frame x y)))
        finally (return last)))

;;; This still needs a lot of work, particularly in the handling of
;;; modifiers.

(defun event-to-keysym-and-modifiers (port event)
  (clim-ffi:with-c-data ((str (array char 16))
			 (sym (array long 1)))
    (let* ((num-chars (xlib-gl:XLookupString
		       event str 16 sym (clim-ffi:null-pointer)))
	   (event-char (when (< 0 num-chars)
			 (code-char (clim-ffi:cref str char))))
	   (event-keysym (clim-xcommon:lookup-keysym (clim-ffi:cref sym
								    long)))
	   (event-type (xlib-gl:xkeyevent-type event)))
      (clim-xcommon:x-keysym-to-clim-modifiers port
					       (if (eql event-type
							xlib-gl:keypress)
						   :key-press
						   :key-release)
					       (or event-char event-keysym)
					       event-keysym
					       (xlib-gl:xkeyevent-state
						event)))))

;;; Relieve some of the tedium of getting values out of events
(defmacro with-event-slots ((event-type &rest vars) event-form  &body body)
  (with-gensyms (event-form-var)
    (let* ((event-name (symbol-name event-type))
	   (var-forms (mapcar (lambda (var)
				(let ((accessor-name
				       (format nil "~A-~A"
					       event-name
					       (symbol-name var))))
				  (multiple-value-bind (accessor status)
				      (find-symbol accessor-name :xlib-gl)
				    (unless (eq status :external)
				      (error "~A is not a known event slot")))
				  `(,var (,accessor ,event-form-var)))))))
      `(let ((,event-form-var ,event-form))
	 (let ,var-forms
	   ,@body)))))

(defun decode-x-button-code (code)
  (aref #.(vector +pointer-left-button+
                  +pointer-middle-button+
                  +pointer-right-button+
		  nil
		  nil)
        (1- code)))

(defun do-key-event (port event)
  (with-event-slots (:xkeyevent type window x y x_root y_root time)
    event
    (multiple-value-bind (keyname modifier-state)
	(event-to-keysym-and-modifiers port event)
      (make-instance (if (eql type xlib-gl:keypress)
			 'key-press-event
			 'key-release-event)
		     :keyname keyname
		     :key-character (and (characterp keyname) keyname)
		     :x x :y y
		     :graft-x x_root :graft-y y_root
		     :sheet (port-sheet-from-coords port window x y)
		     :modifier-state modifier-state
		     :timestamp time))))

(defun do-button-event (port event)
  (with-event-slots (:xbuttonevent window x y x_root y_root time button state)
    event
    (let ((modifier-state (clim-xcommon:x-event-state-modifiers port state)))
      (make-instance (if (eq event-type xlib-gl:buttonpress)
			 'pointer-button-press-event
			 'pointer-button-release-event)
		     :pointer 0
		     :button (decode-x-button-code button)
		     :x x :y y
		     :graft-x x_root :graft-y y_root
		     :sheet (port-sheet-from-coords port window x y)
		     :modifier-state modifier-state
		     :timestamp time))))

(defun do-crossing-event (port event)
  (with-event-slots (:xcrossingevent x y x_root y_root state time button
				     window)
    event
    (let ((sheet (port-sheet-from-coords port window x y))
	  (modifier-state (clim-xcommon:x-event-state-modifiers port state)))
      (when sheet
	(make-instance (cond ((eq event-type xlib-gl:enternotify)
			      'pointer-enter-event)
			     ((eq (xlib-gl:xcrossingevent-mode event)
				  xlib-gl:NotifyGrab)
			      'pointer-ungrab-event)
			     (t 'pointer-exit-event))
		       :pointer 0
		       :button (decode-x-button-code button)
		       :x x :y y
		       :graft-x x_root :graft-y y_root
		       :sheet (find-related-sheet port)
		       :modifier-state modifier-state
		       :timestamp time)))))

(defun query-pointer (port window)
  (clim-ffi:with-c-data ((root :<W>indow)
			 (child :<W>indow)
			 (root-x int)
			 (root-y int)
			 (win-x int)
			 (win-y int)
			 (mask unsigned))
    (let ((result (xquerypointer (opengl-port-display port)
				 window
				 root
				 child
				 root-x
				 root-y
				 win-x
				 win-y
				 mask)))
      (if (zerop result)
	  (values 0 0 nil)
	  (values win-x win-y t child mask root-x root-y)))))
 
(defun do-motion-event (port event)
  (with-event-slots (:xmotionevent x y x_root y_root state time button window
				   is_hint)
    event
    ;; If this is a hint, update the event values with the current pointer
    ;; state.
    (let ((pointer-x x)
	  (pointer-y y)
	  (root-x x_root)
	  (root-y y_root)
	  ()))
    (if (zerop is_hint))
    (let ((sheet (port-sheet-from-coords port window x y)))
      (if (eq sheet (current-sheet port))
	  (make-instance 'pointer-motion-event
			 :pointer 0
			 :button (find-button modifier)
			 :x x
			 :y y
			 :sheet sheet
			 :modifier-state (key-mod modifier)
			 :timestamp time)))))

(defun get-next-event-aux (port)
  (let* ((event   (opengl-port-xevent port))
	 (display (opengl-port-display port))
	 (clim-event
	  (if (synthesized-events port)
	      (pop (synthesized-events port))
	      (progn
		(xlib-gl:XNextEvent (opengl-port-display port) event)
		(let ((event-type (xlib-gl:xanyevent-type event)))
		  (case event-type
		    ((xlib-gl:keypress xlib-gl:keyrelease)
		     (multiple-value-bind (keyname modifier-state)
			 (event-to-keysym-and-modifiers port event)
		       (with-event-slots (:xkeyevent window x y x_root y_root
						     time)
			 event
			 (make-instance (if (eql event-type xlib-gl:keypress)
					    'key-press-event
					    'key-release-event)
					:keyname keyname
					:key-character (and (characterp keyname)
							    keyname)
					:x x :y y
					:graft-x x_root :graft-y y_root
					:sheet (port-sheet-from-coords
						port window x y)
					:modifier-state modifier-state
					:timestamp time))))
		    ((xlib-gl:buttonpress xlib-gl:buttonrelease)
		     (with-event-slots (:xbuttonevent window x y x_root y_root
						      time button state)
		       event
		       (let ((modifier-state
			      (clim-xcommon:x-event-state-modifiers port
								    state)))
			 (make-instance (if (eq event-type xlib-gl:buttonpress)
					    'pointer-button-press-event
					    'pointer-button-release-event)
					:pointer 0
					:button (decode-x-button-code code)
					:x x :y y
					:graft-x x_root :graft-y y_root
					:sheet (port-sheet-from-coords
						port window x y)
					:modifier-state modifier-state
					:timestamp time))))
		    ((xlib-gl:enternotify xlib-gl:leavenotify)
		     (with-event-slots (:xcrossingevent x y x_root y_root
							state time code window)
		       event
		       (let ((sheet (port-sheet-from-coords port window x y))
			     (modifier-state
			      (clim-xcommon:x-event-state-modifiers port
								    state)))
			 (when sheet
			   (make-instance (cond ((eq event-type
						     xlib-gl:enternotify)
						 'pointer-enter-event)
						((eq
						  (xlib-gl:xcrossingevent-mode
						   event)
						  xlib-gl:NotifyGrab)
						 'pointer-ungrab-event)
						(t 'pointer-exit-event))
					  :pointer 0
					  :button (decode-x-button-code code)
					  :x x :y y
					  :graft-x x_root :graft-y y_root
					  :sheet (find-related-sheet port)
					  :modifier-state modifier-state
					  :timestamp time ))))
		     (xlib-gl:motionnotify
		      (with-event-slots (:xmotionevent x y x_root y_root state
						       time code window)
			event
			(let ((sheet (port-sheet-from-coords port window x y)))
			  )))
	 
		     ((eq event-type xlib-gl:motionnotify)
		      (let* ((x (xlib-gl:xmotionevent-x event))
			     (y (xlib-gl:xmotionevent-y event))
			     (modifier (xlib-gl:xmotionevent-state event))
			     (time (xlib-gl:xmotionevent-time event))
			     (sheet-signature (find-sheet-signature port x y))
			     (sheet (recognize-sheet port sheet-signature)))
			(declare (type fixnum x y modifier)
				 (type (unsigned-byte 24) sheet-signature)
				 (type bignum  time))
			(when (eq (xlib-gl:xmotionevent-window event)
				  (sheet-direct-mirror (opengl-port-top-level port)))
			  (if (= sheet-signature *current-sheet-signature*)
			      ;; pointer is in the same sheet as for the previous event
			      (let ((peek    (opengl-port-xpeek  port)))
				(unless (and (when (> (xlib-gl:XPending display) 0)
					       (xlib-gl:XPeekEvent display peek)
					       t)
					     (eq (xlib-gl:XAnyEvent-Type peek)
						 xlib-gl:MotionNotify))
				  (make-instance 'pointer-motion-event
						 :pointer 0
						 :button (find-button modifier)
						 :x x
						 :y y
						 :sheet sheet
						 :modifier-state (key-mod modifier)
						 :timestamp time)))
			      

			      ;; not in same sheet
			      (when sheet
				(let ((button (find-button modifier))
				      (modifier (key-mod modifier))
				      (last-sheet (find-related-sheet port)))
				  (declare (type fixnum button modifier)
					   (type sheet last-sheet))
				  (progn
				    (unless (= *current-sheet-signature* 0)
				      (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-native-region last-sheet)
								(declare (type coordinate x1 y1 x2 y2))
								(dispatch-event last-sheet
										(make-instance 'pointer-exit-event
											       :pointer 0
											       :button button
											       :x (max x1 (min x x2))
											       :y (max y1 (min y y2))
											       :sheet last-sheet
											       :modifier-state modifier
											       :timestamp (- time 2)))))
					; change the current-sheet
				    (setf *current-sheet-signature* sheet-signature)
				    (dispatch-event sheet
						    (make-instance 'pointer-enter-event
								   :pointer 0
								   :button button
								   :x x
								   :y y
								   :sheet sheet
								   :modifier-state modifier
								   :timestamp (1- time)))
				    (make-instance 'pointer-motion-event
						   :pointer 0
						   :button button
						   :x x
						   :y y
						   :sheet sheet
						   :modifier-state modifier
						   :timestamp time))))))))
			 
		     ((eq event-type xlib-gl:configurenotify)
					; the configure notification will be only send to the top-level-sheet
		      (make-instance 'window-configuration-event
				     :sheet (port-lookup-sheet port (xlib-gl:xconfigureevent-window event))
				     :x (xlib-gl:xconfigureevent-x event)
				     :y (xlib-gl:xconfigureevent-y event)
				     :width (xlib-gl:xconfigureevent-width event)
				     :height (xlib-gl:xconfigureevent-height event)))

		     ((eq event-type xlib-gl:mapnotify)
					; the mapping notification will be only send to the top-level-sheet
		      (make-instance 'window-map-event :sheet (port-lookup-sheet port (xlib-gl:xmapevent-window event))))

		     ((eq event-type xlib-gl:destroynotify)
		      (let ((top-level-sheet (opengl-port-top-level port)))
			(opengl-reshape (port-mirror-width port top-level-sheet)
					(port-mirror-height port top-level-sheet))
			(draw-the-entire-scene port)))
                
		     ((eq event-type xlib-gl:expose)
					; the exposure notification will be only send to the top-level-sheet
		      (let ((x (xlib-gl:xexposeevent-x event))
			    (y (xlib-gl:xexposeevent-y event)))
			(make-instance 'window-repaint-event
				       :sheet (port-lookup-sheet port (xlib-gl:xexposeevent-window event))
				       :region (make-bounding-rectangle x y
									(+ x (xlib-gl:xexposeevent-width event))
									(+ y (xlib-gl:xexposeevent-height event))))))
		
		     (t nil))))))))
    (let ())
    (when (synthesized-events port)
      (return-from get-next-event-aux ))
    )


;; OpenGL graft

(defun get-geometry (port mirror)
  (let ((x (make-array 1 :element-type '(unsigned-byte 32)))
	(y (make-array 1 :element-type '(unsigned-byte 32)))
	(width (make-array 1 :element-type '(unsigned-byte 32)))
	(height (make-array 1 :element-type '(unsigned-byte 32)))
	(border-width (make-array 1 :element-type '(unsigned-byte 32)))
	(depth (make-array 1 :element-type '(unsigned-byte 32)))
	(root (make-array 1 :element-type '(unsigned-byte 32))))
    (xlib-gl:XGetGeometry (opengl-port-display port) mirror
		       root x y width height border-width depth)
    (values (aref x 0) (aref y 0) (aref width 0) (aref height 0) (aref border-width 0) (aref depth 0))))

(defmethod make-graft ((port opengl-port)  &key (orientation :default) (units :device))
  (let* ((mirror (opengl-port-root port))
	 (graft (make-instance 'opengl-graft :port port
			       :mirror mirror
			       :orientation orientation :units units)))
    (multiple-value-bind (x y width height) (get-geometry port mirror)
      (declare (ignore x y)
	       (type fixnum width height))
      (setf (sheet-region graft) (make-bounding-rectangle 0 0 width height))
      (push graft (port-grafts port))
      graft)))

(defmethod port-set-sheet-region ((port opengl-port) (graft graft) region)
  (declare (ignore region)
	   (ignorable port graft))
  nil)

(defmethod port-set-sheet-transformation ((port opengl-port) (graft graft) transformation)
  (declare (ignore transformation)
	   (ignorable port graft))
  nil)

(defmethod graft ((port opengl-port))
  (first (port-grafts port)))

;; top-level-sheet-pane

; Integrate with the aux
(defmethod realize-mirror ((port opengl-port) (sheet top-level-sheet-pane))
  (let ((display (opengl-port-display port))
	(screen-id (or (cadr (member :screen-id (port-server-path port))) 0))
	(root (opengl-port-root port)))
    (unless *x-visual*
      (setf *x-visual*
	    (gl:glXChooseVisual
                display screen-id
                (make-array 3
                     :element-type '(signed-byte 32)
                     :initial-contents (list gl:GLX_RGBA gl:GLX_DOUBLEBUFFER xlib-gl:None)))))
    (when (zerop *x-visual*)
      (error "Error with X-Windows : couldn't get an RGB, double-buffered visual."))
    (let ((attributes (xlib-gl:make-XSetWindowAttributes)))
      (xlib-gl:set-XSetWindowAttributes-Bit_Gravity! attributes xlib-gl:NorthWestGravity)
      (xlib-gl:set-XSetWindowAttributes-Colormap!
         attributes
         (xlib-gl:XCreateColormap
            display
            root
            (xlib-gl:XVisualInfo-visual *x-visual*)
            xlib-gl:AllocNone))
      (xlib-gl:Set-XSetWindowAttributes-Event_Mask!
         attributes
         (logior xlib-gl:ExposureMask    xlib-gl:KeyPressMask      xlib-gl:KeyReleaseMask
                 xlib-gl:ButtonPressMask xlib-gl:ButtonReleaseMask xlib-gl:PointerMotionMask
                 xlib-gl:EnterWindowMask xlib-gl:LeaveWindowMask   xlib-gl:StructureNotifyMask))
      (let ((window (xlib-gl:XCreateWindow display root 0 0 100 100 0
                        (xlib-gl:XVisualInfo-depth *x-visual*)
                        xlib-gl:InputOutput
                        (xlib-gl:XVisualInfo-visual *x-visual*)
                        (+ xlib-gl:CWBitGravity xlib-gl:CWColormap xlib-gl:CWEventMask)
                        attributes))
	    (pretty-name (frame-pretty-name (pane-frame sheet))))
	(setf *opengl-glx-context* (gl:glXCreateContext display *x-visual* NULL 1))
	(when (zerop *opengl-glx-context*)
	  (error "Error with X-Window : Unable to create an OpenGL context."))
	(port-register-mirror port sheet window)
	(with-slots (signature->sheet) port
	  (setf (gethash 0 signature->sheet) sheet))
	(setf (opengl-port-top-level port) sheet)
	(xlib-gl:XStoreName display window pretty-name)
	(xlib-gl:XSetIconName display window pretty-name)
	(gl:GLXMakeCurrent display window *opengl-glx-context*)
	(xlib-gl:free-xsetwindowattributes attributes)))))

(defmethod port-mirror-width ((port opengl-port) (sheet top-level-sheet-pane))
  (multiple-value-bind (x y width) (get-geometry port (sheet-direct-mirror sheet))
    (declare (ignore x y)
	     (type fixnum width))
    width))

(defmethod port-mirror-height ((port opengl-port) (sheet top-level-sheet-pane))
  (multiple-value-bind (x y width height) (get-geometry port (sheet-direct-mirror sheet))
    (declare (ignore x y width)
	     (type fixnum height))
    height))

(defmethod unrealize-mirror ((port opengl-port) (sheet top-level-sheet-pane))
  (let ((mirror (sheet-direct-mirror sheet)))
    (xlib-gl:XDestroyWindow (opengl-port-display port) mirror)
    (with-slots (signature->sheet) port
      (remhash 0 signature->sheet))
    (port-unregister-mirror port sheet mirror)))

(defmethod port-set-sheet-region ((port opengl-port) (sheet top-level-sheet-pane) region)
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
    (xlib-gl:XResizeWindow (opengl-port-display port) (sheet-direct-mirror sheet)
			(round (- x2 x1)) (round (- y2 y1)))))

(defmethod port-set-sheet-transformation ((port opengl-port) (sheet top-level-sheet-pane) transformation)
  (multiple-value-bind (x y) (transform-position transformation 0 0)
    (xlib-gl:XMoveWindow (opengl-port-display port) (sheet-direct-mirror sheet)
		      (round x) (round y))))

(defmethod port-compute-native-region ((port opengl-port) (sheet top-level-sheet-pane))
  (declare (ignorable port))
  (sheet-region sheet))

(defmethod compute-extremum :after ((pane top-level-sheet-pane))
  (with-slots (space-requirement) pane
    (let ((size-hints (xlib-gl:make-xsizehints)))
      (xlib-gl:set-xsizehints-width! size-hints (round (space-requirement-width space-requirement)))
      (xlib-gl:set-xsizehints-height! size-hints (round (space-requirement-height space-requirement)))
      (xlib-gl:set-xsizehints-max_width! size-hints (round (space-requirement-max-width space-requirement)))
      (xlib-gl:set-xsizehints-max_height! size-hints (round (space-requirement-max-height space-requirement)))
      (xlib-gl:set-xsizehints-min_width! size-hints (round (space-requirement-min-width space-requirement)))
      (xlib-gl:set-xsizehints-min_height! size-hints (round (space-requirement-min-height space-requirement)))
      (xlib-gl:XSetWMNormalHints (opengl-port-display (port pane)) (sheet-direct-mirror pane) size-hints)
      (xlib-gl:free-xsizehints size-hints))))

;; unmanaged-top-level-sheet-pane

(defmethod realize-mirror ((port opengl-port) (sheet unmanaged-top-level-sheet-pane))
  (let ((display (opengl-port-display port))
	(root (opengl-port-root port))
	(attributes (xlib-gl:make-xsetwindowattributes)))
    (xlib-gl:set-xsetwindowattributes-bit_gravity! attributes xlib-gl:NorthWestgravity)
    (xlib-gl:set-xsetwindowattributes-override_redirect! attributes 1)
    (xlib-gl:set-xsetwindowattributes-colormap!
       attributes
       (xlib-gl:XcreateColormap display root
          (xlib-gl:XVisualInfo-visual *x-visual*)
          xlib-gl:AllocNone))
    (xlib-gl:set-xsetwindowattributes-event_mask!
       attributes
       (logior xlib-gl:ExposureMask
               xlib-gl:KeyPressMask
               xlib-gl:KeyReleaseMask
	       xlib-gl:ButtonPressMask
               xlib-gl:ButtonReleaseMask
	       xlib-gl:PointerMotionMask
               xlib-gl:EnterWindowMask
               xlib-gl:LeaveWindowMask
               xlib-gl:StructureNotifyMask
               xlib-gl:OwnerGrabButtonMask))
    (let ((window (xlib-gl:XCreateWindow display root 0 0 100 100 1
                     (xlib-gl:XVisualInfo-depth *x-visual*) xlib-gl:InputOutput
                     (xlib-gl:XVisualInfo-visual *x-visual*)
                     (logior xlib-gl:CWBitGravity xlib-gl:CWOverrideRedirect
                             xlib-gl:CWColormap xlib-gl:CWEventMask)
                     attributes)))
      (port-register-mirror port sheet window)
      (setf (opengl-port-top-level port) sheet)
      (with-slots (signature->sheet) port
	  (setf (gethash 0 signature->sheet) sheet))
      (gl:glXMakeCurrent display window *opengl-glx-context*)
      (xlib-gl:free-xsetwindowattributes attributes))))

(defmethod unrealize-mirror ((port opengl-port) (sheet unmanaged-top-level-sheet-pane))
  (let ((mirror (sheet-direct-mirror sheet)))
    (xlib-gl:XDestroyWindow (opengl-port-display port) mirror)
    (port-unregister-mirror port sheet mirror)))

(defmethod unrealize-mirror :after ((port opengl-port) (sheet unmanaged-top-level-sheet-pane))
  (declare (ignorable sheet))
  (let ((top-level-sheet (frame-top-level-sheet *application-frame*)))
    (gl:glXMakeCurrent (opengl-port-display port) (sheet-direct-mirror top-level-sheet) *opengl-glx-context*)
    (setf (opengl-port-top-level port) top-level-sheet
	  (gethash 0 (slot-value port 'signature->sheet)) top-level-sheet)
    ; reset the current signature
    (setf *current-sheet-signature* 0)))
    

;; Font and text styles

(defconstant *opengl-text-families* '(:fix "adobe-courier"
				      :serif "adobe-times"
				      :sans-serif "adobe-helvetica"))

(defconstant *opengl-text-faces* '(:roman "medium-r"
				   :bold "bold-r"
				   :italic "medium-i"
				   :bold-italic "bold-i"
				   :italic-bold "bold-i"))

(defconstant *opengl-text-sizes* '(:normal 14
			  	   :tiny 8
				   :very-small 10
				   :small 12
				   :large 18
				   :very-large 20
				   :huge 24))

(defun open-font (display font-name)
  (let* ((font-name "fixed") ; <- FIXME
	; this is what should be done, be at this moment, it doesn't work
	;(number (make-array 1 :element-type '(unsigned-byte 32)))
	;(fonts (xlib-gl:XListFonts display font-name 1 number))
	;(font (if fonts
	;	   (xlib-gl:XLoadFont display (alien:deref fonts 0)) ; problems
	;	   (xlib-gl:XLoadFont display "fixed")))
	 (font (xlib-gl:XLoadFont display font-name))
	 (font-struct (xlib-gl:XQueryFont display font))
	 (start (+ (* 256 (xlib-gl:xfontstruct-min_byte1 font-struct))
		   (xlib-gl:xfontstruct-min_char_or_byte2 font-struct)))
	 (end (+ (* 256 (xlib-gl:xfontstruct-max_byte1 font-struct))
		 (xlib-gl:xfontstruct-max_char_or_byte2 font-struct)))
	 (number-of-char (- end start))
	 (font-list (gl:glGenLists number-of-char)))
  (declare (type fixnum start end number-of-char font-list))
  (prog2
      (gl:glXUseXFont font start number-of-char font-list)
      (list font font-list start)
      (xlib-gl:free-xfontstruct font-struct))))
     ;(xlib-gl:XFreeFontNames fonts)

(defmethod text-style-to-X-font ((port opengl-port) text-style)
  (let ((table (slot-value port 'font-table)))
    (or (first (gethash text-style table))
	(with-slots (family face size) text-style
	  (let* ((family-name (if (stringp family)
				  family
				  (or (getf *opengl-text-families* family)
				      (getf *opengl-text-families* :fix))))
		 (face-name (if (stringp face)
				face
				(or (getf *opengl-text-faces*
					  (if (listp face)
					      (intern (format nil "~A-~A"
							      (first face)
							      (second face))
						      :keyword)
					      face))
				    (getf *opengl-text-faces* :roman))))
		 (size-number (if (numberp size)
				  (round size)
				  (or (getf *opengl-text-sizes* size)
				      (getf *opengl-text-sizes* :normal))))
		 (font-name (format nil "-~A-~A-*-*-~D-*-*-*-*-*-*-*"
				    family-name face-name size-number)))
	    (first (setf (gethash text-style table)
		       (open-font (opengl-port-display port) font-name))))))))

(defmacro with-font-struct ((port text-style font-struct-name) &body body)
  (let ((font (gensym)))
    `(let* ((,font (text-style-to-X-font ,port ,text-style))
	    (,font-struct-name (xlib-gl:XQueryFont (opengl-port-display ,port) ,font)))
       (unwind-protect
	    (progn ,@body)
	 (xlib-gl:free-xfontstruct ,font-struct-name)))))


(defmethod text-style-height (text-style (port opengl-port))
  (declare (type text-style text-style))
  (with-font-struct (port text-style font-struct)
    (+ (xlib-gl:xfontstruct-ascent font-struct) (xlib-gl:xfontstruct-descent font-struct))))

(defmethod text-style-ascent (text-style (port opengl-port))
  (declare (type text-style text-style))
  (with-font-struct (port text-style font-struct)
    (xlib-gl:xfontstruct-ascent font-struct)))

(defmethod text-style-descent (text-style (port opengl-port))
  (declare (type text-style text-style))
  (with-font-struct (port text-style font-struct)
    (xlib-gl:xfontstruct-descent font-struct)))

;; The text-style width is the average width between min_bounds and max_bounds
(defmethod text-style-width (text-style (port opengl-port))
  (declare (type text-style text-style))
  (with-font-struct (port text-style font-struct)
    (round (+ (xlib-gl:xfontstruct-min_bounds-width font-struct)
	      (xlib-gl:xfontstruct-max_bounds-width font-struct))
	   2)))

(defmethod port-character-width ((port opengl-port) text-style char)
  (declare (type standard-char char)
	   (type text-style text-style))
  (with-font-struct (port text-style font-struct)
    (xlib-gl:XTextWidth font-struct (make-string 1 :element-type 'standard-char :initial-element char) 1)))

(defmethod port-string-width ((port opengl-port) text-style string &key (start 0) end)
  (declare (type string string)
	   (type text-style text-style)
	   (type fixnum start))
  (let ((s (if end (subseq string start end) (subseq string start))))
    (with-font-struct (port text-style font-struct)
      (xlib-gl:XTextWidth font-struct s (length s)))))

#|
; [Julien] This is what should be done, but at this moment, it doesn't work
(defmethod port-text-extents ((port opengl-port) text-style string &key start end)
  (declare (type text-style text-style)
	   (type string string)
	   (type fixnum start end))
  (let ((s (subseq string start end))
	(direction (make-array 1 :element-type '(unsigned-byte 32)))
	(font_ascent (make-array 1 :element-type '(unsigned-byte 32)))
	(font_descent (make-array 1 :element-type '(unsigned-byte 32)))
	(string_measures 0))
    (declare (type fixnum string_measures)) ;direction font_ascent font_descent))
    (with-font-struct (port text-style font-struct)
      (prog2
	  (xlib-gl:XTextExtents font-struct s (length s) direction font_ascent font_descent string_measures)
	  (values (xlib-gl:xcharstruct-width string_measures)
		  (xlib-gl:xcharstruct-ascent string_measures)
		  (xlib-gl:xcharstruct-descent string_measures)
		  (xlib-gl:xcharstruct-lbearing string_measures)
		  (xlib-gl:xcharstruct-rbearing string_measures)
		  (aref font_ascent 0) (aref direction 0) nil)
	(xlib-gl:free-xcharstruct string_measures)))))	      
|#

; In order to patch this problem, informations from the biggest sizes of the font are chosen
(defmethod port-text-extents ((port opengl-port) text-style string &key start end)
  (declare (type text-style text-style)
	   (type string string)
	   (type fixnum start end))
  (let* ((s (subseq string start end))
	 (size (length s))
	 result)
    (declare (type string s)
	     (type fixnum size))
    (with-font-struct (port text-style font-struct)
      (let ((direction (xlib-gl:xfontstruct-direction font-struct))
	    (font_ascent (xlib-gl:xfontstruct-ascent font-struct))
	    (ascent (xlib-gl:xfontstruct-max_bounds-ascent font-struct))
	    (descent (xlib-gl:xfontstruct-max_bounds-descent font-struct))
	    (width (* (xlib-gl:xfontstruct-max_bounds-width font-struct) size))
	    (left 0)
	    (right 0))
	(declare (type fixnum direction font_ascent ascent descent width left right))
	(if (eq direction xlib-gl:FontLeftToRight)
	    (setf left (xlib-gl:xfontstruct-max_bounds-lbearing font-struct)
		  right (+ (* (xlib-gl:xfontstruct-max_bounds-width font-struct)
			      (1- size))
			   (xlib-gl:xfontstruct-max_bounds-rbearing
			    font-struct)))
	    (setf right (xlib-gl:xfontstruct-max_bounds-rbearing font-struct)
		  left (+ (* (xlib-gl:xfontstruct-max_bounds-width font-struct)
			     (1- size))
			  (xlib-gl:xfontstruct-max_bounds-lbearing
			   font-struct))))
	(values width ascent descent left right font_ascent direction nil)))))


(defmethod text-style-list-and-start ((port opengl-port) text-style)
  (declare (type text-style text-style))
  (let ((infos (gethash text-style (slot-value port 'font-table))))
    (values (second infos) (third infos))))
  

;; Device-Font-Text-Style

(defmethod port-make-font-text-style ((port opengl-port) device-font-name)
  (let ((text-style (make-instance 'device-font-text-style
				   :text-family device-font-name
				   :text-face nil
				   :text-size nil)))
    (setf (gethash text-style (slot-value port 'font-table))
	  (open-font (opengl-port-display port) device-font-name))
    text-style))


;;; Pixmap

(defclass opengl-mirrored-pixmap (mirrored-pixmap)
  ((glx-pixmap :initform nil :reader opengl-mirrored-pixmap-glx-pixmap)))

(defmethod realize-mirror ((port opengl-port) (pixmap pixmap))
  (declare (ignorable port pixmap))
  (error "OpenGL : can't realize a mirror for a generic pixmap"))

(defmethod realize-mirror ((port opengl-port) (pixmap opengl-mirrored-pixmap))
  (when (null (port-lookup-mirror port pixmap))
    (let ((display (opengl-port-display port))
	  (window (sheet-mirror (pixmap-sheet pixmap))))
      (multiple-value-bind (x y width height border-width depth)
	  (get-geometry port window)
	(declare (ignore x y width height border-width)
		 (type fixnum depth))
	(let* ((x-pixmap (xlib-gl:XCreatePixmap display window
					     (round (pixmap-width pixmap))
					     (round (pixmap-height pixmap))
					     depth))
	       (glx-pixmap (gl:glXCreateGLXPixmap display *x-visual* x-pixmap)))
	  (setf (slot-value pixmap 'glx-pixmap) glx-pixmap)
	  (port-register-mirror port pixmap (cons x-pixmap glx-pixmap))
	  (values))))))

(defmethod unrealize-mirror ((port opengl-port) (pixmap opengl-mirrored-pixmap))
  (when (port-lookup-mirror port pixmap)
    (xlib-gl:XFreePixmap (opengl-port-display port) (port-lookup-mirror port pixmap))
    (gl:glXDestroyGLXPixmap (opengl-port-display port) (opengl-mirrored-pixmap-glx-pixmap pixmap))
    (port-unregister-mirror port pixmap (port-lookup-mirror port pixmap))))

(defmethod port-allocate-pixmap ((port opengl-port) sheet width height)
  (declare (type sheet sheet)
	   (type real width height))
  (let ((pixmap (make-instance 'opengl-mirrored-pixmap
		  :sheet sheet
		  :width width
		  :height height
		  :port port)))
    (when (sheet-grafted-p sheet)
      (realize-mirror port pixmap))
    pixmap))

(defmethod port-deallocate-pixmap ((port opengl-port) pixmap)
  (when (port-lookup-mirror port pixmap)
    (unrealize-mirror port pixmap)))

(defmethod port-copy-to-pixmap ((port opengl-port) (sheet sheet) from-x from-y 
				width height pixmap to-x to-y)
  (declare (type coordinate from-x from-y to-x to-y)
	   (type real width height))
  (assert (typep pixmap 'opengl-mirrored-pixmap))
  (let ((native-transformation (sheet-native-transformation sheet)))
    (declare (type transformation native-transformation))
    (multiple-value-bind (x y) (bounding-rectangle* (sheet-region sheet))
      (declare (type coordinate x y))
      (multiple-value-bind (from-tx from-ty) (transform-position native-transformation (- from-x x) (- from-y y))
	(declare (type coordinate from-tx from-ty))
	(multiple-value-bind (twidth theight) (transform-position native-transformation width height)
	  (declare (type coordinate twidth theight))
	  (let* ((itwidth (round twidth))
		 (itheight (round theight))
		 (addr_pixels (find-array-address (make-array `(,itheight ,itwidth)
							      :element-type 'single-float))))
	  (gl:glReadBuffer gl:GL_FRONT)
	  (gl:glReadPixels (round from-tx) (round from-ty) itwidth itheight
			   gl:GL_RGBA gl:GL_FLOAT addr_pixels)
	  (gl:glXMakeCurrent (opengl-port-display port)
			     (opengl-mirrored-pixmap-glx-pixmap pixmap)
			     *opengl-glx-context*)
	  ; always copy in GL_FRONT buffer of the glXPixmap
	  (gl:glDrawBuffer gl:GL_FRONT)
	  (gl:glRasterPos2d to-x to-y)
	  (gl:glDrawPixels itwidth itheight gl:GL_RGBA gl:GL_FLOAT addr_pixels)
	  (gl:glXMakeCurrent (opengl-port-display port) (opengl-port-top-level port) *opengl-glx-context*)))))))

(defmethod port-copy-to-pixmap ((port opengl-port) (medium medium) from-x from-y 
				width height pixmap to-x to-y)
  (when (medium-sheet medium) ; temporary solution
    (port-copy-to-pixmap port (medium-sheet medium) from-x from-y 
			 width height pixmap to-x to-y)))

(defmethod port-copy-to-pixmap ((port opengl-port) (stream stream) from-x from-y 
				width height pixmap to-x to-y)
  (declare (ignore from-x from-y width height pixmap to-x to-y))
  (error "copy-to-pixmap with a stream as source is not implemented."))

(defmethod port-copy-from-pixmap ((port opengl-port) pixmap from-x from-y
				  width height sheet to-x to-y)
  (declare (type coordinate from-x from-y to-x to-y)
	   (type real width height)
	   (type sheet sheet))
  (assert (typep pixmap 'opengl-mirrored-pixmap))
  (let ((native-transformation (sheet-native-transformation sheet)))
    (declare (type transformation native-transformation))
    (multiple-value-bind (x y) (bounding-rectangle* (sheet-region sheet))
      (declare (type coordinate x y))
      (multiple-value-bind (to-tx to-ty) (transform-position native-transformation (- to-x x) (- to-y y))
	(declare (type coordinate to-tx to-ty))
	(multiple-value-bind (twidth theight) (transform-position native-transformation width height)
	  (declare (type coordinate twidth theight))
	  (let* ((itwidth (round twidth))
		 (itheight (round theight))
		 (addr_pixels (find-array-address (make-array `(,itheight ,itwidth)
							      :element-type 'single-float))))
	    (gl:glXMakeCurrent (opengl-port-display port)
			       (opengl-mirrored-pixmap-glx-pixmap pixmap)
			       *opengl-glx-context*)
	  ; always read from the GL_FRONT buffer of the glXPixmap
	    (gl:glReadBuffer gl:GL_FRONT)
	    (gl:glReadPixels (round from-x) (round from-y) itwidth itheight
			     gl:GL_RGBA gl:GL_FLOAT addr_pixels)
	    (gl:glXMakeCurrent (opengl-port-display port) (opengl-port-top-level port) *opengl-glx-context*)
	    (gl:glDrawBuffer gl:GL_BACK)
	    (gl:glRasterPos2d to-tx to-ty)
	    (gl:glDrawPixels itwidth itheight gl:GL_RGBA gl:GL_FLOAT addr_pixels)))))))
	
(defmacro with-context ((sheet) &body body)
  (if (typep sheet 'opengl-mirrored-pixmap)
      (let ((port (gensym)))
	`(let ((,port (port ,sheet)))
	   (gl:glXMakeCurrent (opengl-port-display ,port)
			      (opengl-mirrored-pixmap-glx-pixmap ,sheet)
			      *opengl-glx-context*)
	   (unwind-protect
	       (progn
		 ,@body))
	   (gl:glXMakeCurrent (opengl-port-display ,port)
			      (opengl-port-top-level ,port)
			      *opengl-glx-context*)))
      `(progn
	 ,@body)))
