;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by 
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;           Robert Strandh (strandh@labri.u-bordeaux.fr)

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
	   :accessor clx-port-window)
   (color-table :initform (make-hash-table :test #'eq))
   (font-table :initform (make-hash-table :test #'eq)))
  )

(setf (get :x11 :port-type) 'clx-port)

(defmethod initialize-instance :after ((port clx-port) &rest args)
  (declare (ignore args))
  (push (make-instance 'clx-frame-manager :port port) (slot-value port 'frame-managers))
  (initialize-clx port))

(defun clx-error-handler (display error-name &key &allow-other-keys)
  (format *error-output* "clx-error: ~a~%" error-name))

(defmethod initialize-clx ((port clx-port))
  (let ((options (cdr (port-server-path port))))
    (setf (clx-port-display port)
	  (xlib:open-display (getf options :host "") :display (getf options :display-id 0)))
    (setf (xlib:display-error-handler (clx-port-display port))
	  #'clx-error-handler)
    (setf (clx-port-screen port) (nth (getf options :screen-id 0)
				      (xlib:display-roots (clx-port-display port))))
    (setf (clx-port-window port) (xlib:screen-root (clx-port-screen port)))
    (make-graft port)
    ))

(defun realize-mirror-aux (port sheet
				&key (width 100) (height 100) (x 0) (y 0)
				(border-width 0) (border 0)
				(override-redirect :off)
				(map t)
				(event-mask `(:exposure :key-press :key-release
							:button-press :button-release
							:enter-window :leave-window
							:structure-notify
							:pointer-motion)))
  (when (null (port-lookup-mirror port sheet))
    (with-sheet-medium (medium sheet)
      (let* ((desired-color (medium-background (sheet-medium sheet)))
	     (color (multiple-value-bind (r g b)
			(color-rgb desired-color)
		      (xlib:make-color :red r :green g :blue b)))
	     (pixel (xlib:alloc-color (xlib:screen-default-colormap (clx-port-screen port))
				      color))
	     (window (xlib:create-window
		      :parent (sheet-mirror (sheet-parent sheet))
		      :width width :height height :x x :y y
		      :border-width border-width
		      :border border
		      :override-redirect override-redirect
		      :background pixel
		      :event-mask (apply #'xlib:make-event-mask
					 event-mask))))
	(port-register-mirror (port sheet) sheet window)
	(when map
	  (xlib:map-window window)))))
  (port-lookup-mirror port sheet))

(defmethod realize-mirror ((port clx-port) (sheet sheet))
  (realize-mirror-aux port sheet :border-width 0))

(defmethod realize-mirror ((port clx-port) (sheet border-pane))
  (rotatef (medium-background (sheet-medium sheet)) (medium-foreground (sheet-medium sheet)))
  (realize-mirror-aux port sheet
		      :border-width 0 ; (border-pane-width sheet)
		      :event-mask '(:exposure
				    :structure-notify)))

(defmethod realize-mirror ((port clx-port) (sheet top-level-sheet-pane))
  (let ((frame (pane-frame sheet))
	(window (realize-mirror-aux port sheet
				    :map nil
				    :event-mask '(:structure-notify))))
    (setf (xlib:wm-name window) (frame-pretty-name frame))
    (setf (xlib:wm-icon-name window) (frame-pretty-name frame))))

(defmethod realize-mirror ((port clx-port) (sheet unmanaged-top-level-sheet-pane))
  (realize-mirror-aux port sheet
		      :override-redirect :on
		      :map nil
		      :event-mask '(:structure-notify)))

(defmethod realize-mirror ((port clx-port) (sheet menu-button-pane))
  (realize-mirror-aux port sheet
		      :event-mask '(:exposure
				    :key-press :key-release
				    :button-press :button-release
				    :enter-window :leave-window
				    :structure-notify
				    :pointer-motion
				    :owner-grab-button)))

(defmethod unrealize-mirror ((port clx-port) (sheet sheet))
  (when (port-lookup-mirror port sheet)
    (xlib:destroy-window (port-lookup-mirror port sheet))
    (port-unregister-mirror port sheet (sheet-mirror sheet))))

(defmethod port-set-sheet-region ((port clx-port) (graft graft) region)
  (declare (ignore region))
  nil)

(defmethod port-set-sheet-region ((port clx-port) (sheet sheet) region)
  (let ((mirror (sheet-direct-mirror sheet)))
    (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
      (setf (xlib:drawable-width mirror) (round (- x2 x1))
	    (xlib:drawable-height mirror) (round (- y2 y1))))))

(defmethod port-set-sheet-transformation ((port clx-port) (graft graft) transformation)
  (declare (ignore transformation))
  nil)

(defmethod port-set-sheet-transformation ((port clx-port) (pane application-pane) transformation)
  (declare (ignore transformation))
  nil)

(defmethod port-set-sheet-transformation ((port clx-port) (pane interactor-pane) transformation)
  (declare (ignore transformation))
  nil)

(defmethod port-set-sheet-transformation ((port clx-port) (sheet sheet) transformation)
  (let ((mirror (sheet-direct-mirror sheet)))
    (multiple-value-bind (x y) (transform-position transformation 0 0)
      (setf (xlib:drawable-x mirror) (round x)
	    (xlib:drawable-y mirror) (round y)))))


(defmethod destroy-port :before ((port clx-port))
  (xlib:close-display (clx-port-display port)))

(defun event-handler (&rest event-slots
                      &key display window event-key code state mode time width height x y
                      &allow-other-keys)
  (let ((sheet (and window
		    (port-lookup-sheet *clx-port* window))))
    (declare (special *clx-port*))
    (when sheet
      (case event-key
	(:key-press
	 (make-instance 'key-press-event :key-name (xlib:keycode->character display code state)
			:sheet sheet :modifier-state state :timestamp time))
	(:key-release
	 (make-instance 'key-release-event :key-name (xlib:keycode->character display code state)
			:sheet sheet :modifier-state state :timestamp time))
	(:button-release
	 (make-instance 'pointer-button-release-event :pointer 0 :button code :x x :y y
			:sheet sheet :modifier-state state :timestamp time))
	(:button-press
	 (make-instance 'pointer-button-press-event :pointer 0 :button code :x x :y y
			:sheet sheet :modifier-state state :timestamp time))
	(:enter-notify
	 (make-instance 'pointer-enter-event :pointer 0 :button code :x x :y y
			:sheet sheet :modifier-state state :timestamp time))
	(:leave-notify
	 (make-instance (if (eq mode :ungrab) 'pointer-ungrab-event 'pointer-exit-event)
	   :pointer 0 :button code :x x :y y
	   :sheet sheet :modifier-state state :timestamp time))
	(:configure-notify
	 (make-instance 'window-configuration-event :sheet sheet
			:x x :y y :width width :height height))
	(:destroy-notify
	 (make-instance 'window-destroy-event :sheet sheet))
	(:motion-notify
	 (make-instance 'pointer-motion-event :pointer 0 :button code :x x :y y
			:sheet sheet :modifier-state state :timestamp time))
	((:exposure :display)
	 (make-instance 'window-repaint-event :sheet sheet))
	(t
	 nil)))))

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
  (make-instance 'clx-medium 
		 :port port 
		 :graft (find-graft :port port) 
		 :sheet sheet))

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
  (let ((table (slot-value port 'font-table)))
    (or (gethash text-style table)
	(with-slots (family face size) text-style
	  (let* ((family-name (if (stringp family)
				  family
				  (or (getf *clx-text-families* family)
				      (getf *clx-text-families* :fix))))
		 (face-name (if (stringp face)
				face
				(or (getf *clx-text-faces*
					  (if (listp face)
					      (intern (format nil "~A-~A"
							      (first face)
							      (second face))
						      :keyword)
					      face))
				    (getf *clx-text-faces* :roman))))
		 (size-number (if (numberp size)
				  (round size)
				  (or (getf *clx-text-sizes* size)
				      (getf *clx-text-sizes* :normal))))
		 (font-name (format nil "-~A-~A-*-*-~D-*-*-*-*-*-*-*"
				    family-name face-name size-number)))
	    (setf (gethash text-style table)
		  (open-font (clx-port-display port) font-name)))))))

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
  (let ((table (slot-value port 'color-table)))
    (or (gethash color table)
	(setf (gethash color table)
	      (multiple-value-bind (r g b) (color-rgb color)
		(xlib:alloc-color (xlib:screen-default-colormap
				   (first (xlib:display-roots (clx-port-display port))))
				  (xlib:make-color :red r :green g :blue b)))))))

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

(defmethod graft ((port clx-port))
  (port-grafts port))

;; clim-stream-pane drawings

(defmethod window-clear :before ((sheet mirrored-sheet))
  (xlib:clear-area (sheet-direct-mirror sheet)))

(defmethod window-clear ((sheet mirrored-sheet))
  (declare (ignorable sheet))
  nil)
  
(defmethod clear-area ((sheet mirrored-sheet))
  (xlib:clear-area (sheet-direct-mirror sheet)))