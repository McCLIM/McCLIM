;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)

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

;;; CLX-PORT class

(defclass clx-port (port)
  ((display :initform nil
	    :accessor clx-port-display)
   (screen :initform nil
	   :accessor clx-port-screen)
   (window :initform nil
	   :accessor clx-port-window))
  )

(setf (get :x11 :port-type) 'clx-port)

(defmethod initialize-instance :after ((port clx-port) &rest args)
  (declare (ignore args))
  (push (make-instance 'clx-frame-manager :port port) (slot-value port 'frame-managers))
  (initialize-clx port))

(defmethod initialize-clx ((port clx-port))
  (let ((options (cdr (port-server-path port))))
    (setf (clx-port-display port)
      (xlib:open-display (getf options :host "") :display (getf options :display-id 0)))
    (setf (clx-port-screen port) (nth (getf options :screen-id 0)
				      (xlib:display-roots (clx-port-display port))))
    (setf (clx-port-window port) (xlib:screen-root (clx-port-screen port)))
    (make-graft port)
    ))

(defmethod realize-mirror ((port clx-port) (sheet sheet))
  (when (null (port-lookup-mirror port sheet))
    (let* ((space (sheet-region sheet))
	   (desired-color (medium-background (sheet-medium sheet)))
	   (color (multiple-value-bind (r g b)
		      (color-rgb desired-color)
		    (xlib:make-color :red r :green g :blue b)))
	   (pixel (xlib:alloc-color (xlib:screen-default-colormap (clx-port-screen port))
				    color))
	   (window (xlib:create-window
		    :parent (sheet-mirror (sheet-parent sheet))
		    :width  (round (bounding-rectangle-width space))
		    :height (round (bounding-rectangle-height space))
		    :x      (round (bounding-rectangle-min-x space))
		    :y      (round (bounding-rectangle-min-y space))
		    :border-width 1
		    :border 0
		    :background pixel
		    :event-mask (xlib:make-event-mask
				 :exposure :key-press :key-release
				 :button-press :button-release))))
      (port-register-mirror (port sheet) sheet window)
      (loop for child in (sheet-children sheet)
	  do (realize-mirror port child))
      (xlib:map-window window))))

(defmethod realize-mirror ((port clx-port) (sheet border-pane))
  (when (null (port-lookup-mirror port sheet))
    (let* ((space (sheet-region sheet))
	   (desired-color (medium-background (sheet-medium sheet)))
	   (color (multiple-value-bind (r g b)
		      (color-rgb desired-color)
		    (xlib:make-color :red r :green g :blue b)))
	   (pixel (xlib:alloc-color (xlib:screen-default-colormap (clx-port-screen port))
				    color))
	   (window (xlib:create-window
		    :parent (sheet-mirror (sheet-parent sheet))
		    :width (bounding-rectangle-width space)
		    :height (bounding-rectangle-height space)
		    :x (bounding-rectangle-min-x space)
		    :y (bounding-rectangle-min-y space)
		    :border-width (border-pane-width sheet)
		    :border 0
		    :background pixel
		    :event-mask (xlib:make-event-mask
				 :exposure :key-press :key-release
				 :button-press :button-release))))
      (port-register-mirror (port sheet) sheet window)
      (loop for child in (sheet-children sheet)
	  do (realize-mirror port child))
      (xlib:map-window window))))

(defmethod unrealize-mirror ((port clx-port) (sheet sheet))
  (loop for child in (sheet-children sheet)
      do (unrealize-mirror port child))
  (when (port-lookup-mirror port sheet)
    (format t "unrealize-mirror ~S~&" sheet)
    (port-unregister-mirror (port sheet) sheet (port-lookup-mirror port sheet))
    (xlib:destroy-window (port-lookup-mirror port sheet))))

(defmethod destroy-port :before ((port clx-port))
  (xlib:close-display (clx-port-display port)))

(defun event-handler (&rest event-slots
                      &key display window event-key code state x y
                      &allow-other-keys)
  (let ((sheet (and window
		    (port-lookup-sheet *clx-port* window))))
    (declare (special *clx-port*))
    (case event-key
      (:key-press
       (make-instance 'key-press-event :key-name (xlib:keycode->character display code state)
		      :sheet sheet :modifier-state state))
      (:key-release
       (make-instance 'key-release-event :key-name (xlib:keycode->character display code state)
		      :sheet sheet :modifier-state state))
      (:button-press
       (make-instance 'pointer-button-click-event :pointer 0 :button code :x x :y y
		      :sheet sheet :modifier-state state))
      (t
       nil))))

(defmethod get-next-event ((port clx-port) &key wait-function timeout)
  (declare (ignore wait-function timeout))
  (let ((*clx-port* port))
    (declare (special *clx-port*))
    (xlib:display-finish-output (clx-port-display port))
    (xlib:process-event (clx-port-display port) :handler #'event-handler :discard-p t)))

(defmethod make-graft ((port clx-port) &key (orientation :default) (units :device))
  (let ((graft (make-instance 'clx-graft
		 :port port :mirror (clx-port-window port)
		 :orientation orientation :units units)))
    (setf (sheet-region graft) (make-bounding-rectangle 0 0 (xlib:screen-width (clx-port-screen port)) (xlib:screen-height (clx-port-screen port))))
    (push graft (port-grafts port))
    graft))

(defmethod make-medium ((port clx-port) sheet)
  (make-instance 'clx-medium :port port :graft (graft sheet) :sheet sheet))

(defconstant *clx-text-families* '(:fix "adobe-courier"
				   :serif "adobe-times"
				   :sans-serif "adobe-helvetica"))

(defconstant *clx-text-faces* '(:roman "medium-r"
				:bold "bold-r"
				:italic "medium-i"
				:bold-italic "bold-i"
				:italic-bold "bold-i"))

(defconstant *clx-text-sizes* '(:normal 14
				:tiny 8
				:very-small 10
				:small 12
				:large 18
				:very-large 20
				:huge 24))

(defun open-font (display font-name)
  (let ((fonts (xlib:list-font-names display font-name :max-fonts 1)))
    (if fonts
	(xlib:open-font display (first fonts))
      (xlib:open-font display "fixed"))))

(defmethod text-style-to-X-font ((port clx-port) text-style)
  (with-slots (family face size) text-style
    (let* ((family-name (if (stringp family)
			    family
			  (or (getf *clx-text-families* family)
			      (getf *clx-text-families* :fix))))
	   (face-name (if (stringp face)
			  face
			(or (getf *clx-text-faces*
				  (if (listp face)
				      (intern (format nil "~A-~A" (first face) (second face))
					      :keyword)
				    face))
			    (getf *clx-text-faces* :roman))))
	   (size-number (if (numberp size)
			    (round size)
			  (or (getf *clx-text-sizes* size)
			      (getf *clx-text-sizes* :normal))))
	   (font-name (format nil "-~A-~A-*-*-~D-*-*-*-*-*-*-*"
			      family-name face-name size-number)))
      (open-font (clx-port-display port) font-name))))

(defmethod text-style-height (text-style (port clx-port))
  (let ((font (text-style-to-X-font port text-style)))
    (+ (xlib:font-ascent font) (xlib:font-descent font))))

(defmethod text-style-ascent (text-style (port clx-port))
  (let ((font (text-style-to-X-font port text-style)))
    (xlib:font-ascent font)))

(defmethod text-style-descent (text-style (port clx-port))
  (let ((font (text-style-to-X-font port text-style)))
    (xlib:font-descent font)))

(defmethod text-style-width (text-style (port clx-port))
  (xlib:char-width (text-style-to-X-font port text-style) (char-code #\m)))

(defmethod port-character-width ((port clx-port) text-style char)
  (let* ((font (text-style-to-X-font port text-style))
	 (width (xlib:char-width font (char-code char))))
    width))

(defmethod port-string-width ((port clx-port) text-style string &key (start 0) end)
  (xlib:text-width (text-style-to-X-font port text-style) string :start start :end end))

(defmethod beep ((port clx-port))
  (xlib:bell (clx-port-display port)))

(defmethod X-pixel ((port clx-port) color)
  (multiple-value-bind (r g b) (color-rgb color)
    (xlib:alloc-color (xlib:screen-default-colormap
		       (first (xlib:display-roots (clx-port-display port))))
		      (xlib:make-color :red r :green g :blue b))))

(defmethod port-allocate-pixmap ((port clx-port) sheet width height)
  (declare (ignore sheet width height))
  (error "ALLOCATE-PIXMAP is not implemented for CLX-PORTs"))

(defmethod port-deallocate-pixmap ((port clx-port) pixmap)
  (declare (ignore pixmap))
  (error "DEALLOCATE-PIXMAP is not implemented for CLX-PORTs"))

(defmethod port-copy-to-pixmap ((port clx-port) sheet from-x from-y width height
				pixmap to-x to-y)
  (declare (ignore sheet from-x from-y width height pixmap to-x to-y))
  (error "COPY-TO-PIXMAP is not implemented for CLX-PORTs"))

(defmethod port-copy-area ((port clx-port) sheet from-x from-y width height to-x to-y)
  (let* ((mirror (port-lookup-mirror port sheet))
	 (gc (medium-gcontext (sheet-medium sheet) +background-ink+)))
    (xlib:copy-area mirror gc from-x from-y width height mirror to-x to-y)))

(defmethod port-mirror-width ((port clx-port) sheet)
  (let ((mirror (port-lookup-mirror port sheet)))
    (xlib:drawable-width mirror)))

(defmethod port-mirror-height ((port clx-port) sheet)
  (let ((mirror (port-lookup-mirror port sheet)))
    (xlib:drawable-height mirror)))