;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000,2001 by 
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

(defclass clx-port (basic-port)
  ((display :initform nil
	    :accessor clx-port-display)
   (screen :initform nil
	   :accessor clx-port-screen)
   (window :initform nil
	   :accessor clx-port-window)
   (color-table :initform (make-hash-table :test #'eq))
   (font-table :initform (make-hash-table :test #'eq))) )

(setf (get :x11 :port-type) 'clx-port)

(defmethod initialize-instance :after ((port clx-port) &rest args)
  (declare (ignore args))
  (push (make-instance 'clx-frame-manager :port port) (slot-value port 'frame-managers))
  (initialize-clx port))

(defun clx-error-handler (display error-name &key &allow-other-keys)
  (declare (ignore display))
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
    (when *multiprocessing-p*
      (setf (port-event-process port)
        (clim-sys:make-process
         (lambda ()
           (loop
             (with-simple-restart
                 (restart-event-loop
                  "Restart CLIM's event loop.")
               (loop
                 (process-next-event port)))))
         :name (format nil "~S's event process." port))))
    ))

(defun realize-mirror-aux (port sheet
				&key (width 100) (height 100) (x 0) (y 0)
				(border-width 0) (border 0)
				(override-redirect :off)
				(map t)
				(backing-store :not-useful)
				(event-mask `(:exposure 
					      :key-press :key-release
					      :button-press :button-release
					      :enter-window :leave-window
					      :structure-notify
					      :pointer-motion)))
  (when (null (port-lookup-mirror port sheet))
    (let* ((desired-color +white+ #+NIL (medium-background sheet))
           (color (multiple-value-bind (r g b)
                      (color-rgb desired-color)
                    (xlib:make-color :red r :green g :blue b)))
           (pixel (xlib:alloc-color (xlib:screen-default-colormap (clx-port-screen port))
                                    color))
           (window (xlib:create-window
                    :parent (sheet-mirror (sheet-parent sheet))
                    :width width 
                    :height height
                    :x x :y y
                    :border-width border-width
                    :border border
                    :override-redirect override-redirect
                    :backing-store backing-store
                    :gravity :north-west
                    :background pixel
                    :event-mask (apply #'xlib:make-event-mask
                                       event-mask))))
      (port-register-mirror (port sheet) sheet window)
      (when map
        (xlib:map-window window))))
  (port-lookup-mirror port sheet))

(defmethod realize-mirror ((port clx-port) (sheet mirrored-sheet-mixin))
  (realize-mirror-aux port sheet :border-width 0))

(defmethod realize-mirror ((port clx-port) (sheet border-pane))
  ;;(rotatef (medium-background (sheet-medium sheet)) (medium-foreground (sheet-medium sheet)))
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

(defmethod destroy-mirror ((port clx-port) (sheet mirrored-sheet-mixin))
  (when (port-lookup-mirror port sheet)
    (xlib:destroy-window (port-lookup-mirror port sheet))
    (port-unregister-mirror port sheet (sheet-mirror sheet))))

(defmethod mirror-transformation ((port clx-port) mirror)
  (make-translation-transformation (xlib:drawable-x mirror)
                                   (xlib:drawable-y mirror)))

(defmethod port-set-sheet-region ((port clx-port) (graft graft) region)
  (declare (ignore region))
  nil)

(defmethod port-set-sheet-region ((port clx-port) (sheet mirrored-sheet-mixin) region)
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

(defmethod port-set-sheet-transformation ((port clx-port) (sheet mirrored-sheet-mixin) transformation)
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
         (multiple-value-bind (keyname modifier-state) (x-event-to-key-name-and-modifiers display code state)
           (make-instance 'key-press-event 
             :key-name keyname
             :sheet sheet :modifier-state modifier-state :timestamp time)))
	(:key-release
         (multiple-value-bind (keyname modifier-state) (x-event-to-key-name-and-modifiers display code state)
           (make-instance 'key-release-event 
             :key-name keyname
             :sheet sheet :modifier-state modifier-state :timestamp time)))
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
	 (make-instance 'window-repaint-event
           :sheet sheet
           :region (make-rectangle* x y (+ x width) (+ y height))))
	(t
	 nil)))))

(defmethod get-next-event ((port clx-port) &key wait-function (timeout nil))
  (declare (ignore wait-function))
  (let ((*clx-port* port))
    (declare (special *clx-port*))
    (xlib:display-finish-output (clx-port-display port))
;    (xlib:process-event (clx-port-display port) :timeout timeout :handler #'event-handler :discard-p t)))
    ; temporary solution
    (or (xlib:process-event (clx-port-display port) :timeout timeout :handler #'event-handler :discard-p t)
	:timeout)))
;; [Mike] Timeout and wait-functions are both implementation 
;;        specific and hence best done in the backends.


(defmethod make-graft ((port clx-port) &key (orientation :default) (units :device))
  (let ((graft (make-instance 'clx-graft
		 :port port :mirror (clx-port-window port)
		 :orientation orientation :units units)))
    (setf (sheet-region graft) (make-bounding-rectangle 0 0 (xlib:screen-width (clx-port-screen port)) (xlib:screen-height (clx-port-screen port))))
    (push graft (port-grafts port))
    graft))

(defmethod make-medium ((port clx-port) sheet)
  (make-instance 'clx-medium 
		 ;; :port port 
		 ;; :graft (find-graft :port port) 
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

(defmethod port-character-width ((port clx-port) text-style char)
  (let* ((font (text-style-to-X-font port text-style))
	 (width (xlib:char-width font (char-code char))))
    width))

(defmethod port-string-width ((port clx-port) text-style string &key (start 0) end)
  (xlib:text-width (text-style-to-X-font port text-style) 
		   string :start start :end end))

(defmethod X-pixel ((port clx-port) color)
  (let ((table (slot-value port 'color-table)))
    (or (gethash color table)
	(setf (gethash color table)
	      (multiple-value-bind (r g b) (color-rgb color)
		(xlib:alloc-color (xlib:screen-default-colormap
				   (first (xlib:display-roots (clx-port-display port))))
				  (xlib:make-color :red r :green g :blue b)))))))

(defmethod port-mirror-width ((port clx-port) sheet)
  (let ((mirror (port-lookup-mirror port sheet)))
    (xlib:drawable-width mirror)))

(defmethod port-mirror-height ((port clx-port) sheet)
  (let ((mirror (port-lookup-mirror port sheet)))
    (xlib:drawable-height mirror)))

(defmethod graft ((port clx-port))
  (first (port-grafts port)))

;;; Pixmap

(defmethod realize-mirror ((port clx-port) (pixmap pixmap))
  (when (null (port-lookup-mirror port pixmap))
    (let* ((window (sheet-direct-mirror (pixmap-sheet pixmap)))
	   (pix (xlib:create-pixmap 
		    :width (round (pixmap-width pixmap))
		    :height (round (pixmap-height pixmap))
		    :depth (xlib:drawable-depth window)
		    :drawable window)))
      (port-register-mirror port pixmap pix))
    (values)))

(defmethod destroy-mirror ((port clx-port) (pixmap pixmap))
  (when (port-lookup-mirror port pixmap)
    (xlib:free-pixmap (port-lookup-mirror port pixmap))
    (port-unregister-mirror port pixmap (port-lookup-mirror port pixmap))))

(defmethod port-allocate-pixmap ((port clx-port) sheet width height)
  (let ((pixmap (make-instance 'mirrored-pixmap
			       :sheet sheet
			       :width width
			       :height height
			       :port port)))
    (when (sheet-grafted-p sheet)
      (realize-mirror port pixmap))
    pixmap))

(defmethod port-deallocate-pixmap ((port clx-port) pixmap)
  (when (port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))

;; Device-Font-Text-Style

(defmethod port-make-font-text-style ((port clx-port) device-font-name)
  (let ((text-style (make-instance 'device-font-text-style
				   :text-family device-font-name
				   :text-face nil
				   :text-size nil)))
    (setf (gethash text-style (slot-value port 'font-table))
	  (open-font (clx-port-display port) device-font-name))
    text-style))

;; Top-level-sheet

(defmethod compute-extremum :after ((pane top-level-sheet-pane))
  (with-slots (space-requirement) pane
    (setf (xlib:wm-normal-hints (sheet-direct-mirror pane))
	  (xlib:make-wm-size-hints 
	   :width (round (space-requirement-width space-requirement))
	   :height (round (space-requirement-height space-requirement))
	   :max-width (min 65535 (round (space-requirement-max-width space-requirement)))
	   :max-height (min 65535 (round (space-requirement-max-height space-requirement)))
	   :min-width (round (space-requirement-min-width space-requirement))
	   :min-height (round (space-requirement-min-height space-requirement))))))


