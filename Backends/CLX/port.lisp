;;; -*- Mode: Lisp; Package: CLIM-CLX; -*-

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

(in-package :CLIM-CLX)

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

(defun parse-clx-server-path (path)
  (pop path)
  (let* ((s (get-environment-variable "DISPLAY"))
	 (colon (position #\: s))
	 (dot (position #\. s :start colon))
	 (host-name (subseq s 0 colon))
	 (display-number (parse-integer s :start (1+ colon) :end dot))
	 (screen-number (if dot (parse-integer s :start (1+ dot)) 0)))
    (list :clx
	  :host (getf path :host host-name)
	  :display-id (getf path :display-id display-number)
	  :screen-id (getf path :screen-id screen-number))))

(setf (get :x11 :port-type) 'clx-port)
(setf (get :x11 :server-path-parser) 'parse-clx-server-path)
(setf (get :clx :port-type) 'clx-port)
(setf (get :clx :server-path-parser) 'parse-clx-server-path)

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
      #-sbcl
      (xlib:open-display (getf options :host "") :display (getf options :display-id 0))
      #+sbcl
      (xlib:open-display "localhost" :display (getf options :display-id 0)))

    (progn
      #+NIL
      (setf (xlib:display-error-handler (clx-port-display port))
        #'clx-error-handler)
    
      #-NIL
      (setf (xlib:display-after-function (clx-port-display port)) #'xlib:display-finish-output))
    
    (setf (clx-port-screen port) (nth (getf options :screen-id 0)
				      (xlib:display-roots (clx-port-display port))))
    (setf (clx-port-window port) (xlib:screen-root (clx-port-screen port)))
    (make-graft port)
    (when clim-sys:*multiprocessing-p*
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

#+NIL
(defmethod (setf sheet-mirror-transformation) :after (new-value (sheet mirrored-sheet-mixin))
  )

(defun invent-sheet-mirror-transformation-and-region (sheet)
  ;; -> tr region
  (let* ((r (sheet-region sheet))
         (r* (transform-region
              (sheet-native-transformation (sheet-parent sheet))
              (transform-region (sheet-transformation sheet) r)))
         #+NIL
         (r*
          (bounding-rectangle
           (region-intersection r*
                                (make-rectangle* 0 0
                                                 (port-mirror-width (port sheet) (sheet-parent sheet))
                                                 (port-mirror-height (port sheet) (sheet-parent sheet))))))
         (mirror-transformation
          (if (region-equal r* +nowhere+)
              (make-translation-transformation 0 0)
            (make-translation-transformation 
             (bounding-rectangle-min-x r*)
             (bounding-rectangle-min-y r*))))
         (mirror-region
          (untransform-region mirror-transformation r*)))
    (values
     mirror-transformation
     mirror-region)))

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
					     ;:pointer-motion
                                              :button-motion)))
  (when (null (port-lookup-mirror port sheet))
    (let* ((desired-color (typecase sheet
                            (sheet-with-medium-mixin
                              (medium-background sheet))
                            (basic-pane ; CHECKME [is this sensible?] seems to be
                              (let ((background (pane-background sheet)))
                                (if (typep background 'color)
                                    background
                                    +white+)))
                            (t
                              +white+)))
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
  (realize-mirror-aux port sheet
                      :border-width 0
                      :map (sheet-enabled-p sheet)))

(defmethod realize-mirror ((port clx-port) (sheet border-pane))
  ;;(rotatef (medium-background (sheet-medium sheet)) (medium-foreground (sheet-medium sheet)))
  (realize-mirror-aux port sheet
		      :border-width 0 ; (border-pane-width sheet)
		      :event-mask '(:exposure
				    :structure-notify)
                      :map (sheet-enabled-p sheet)))

(defmethod realize-mirror ((port clx-port) (sheet top-level-sheet-pane))
  (let ((frame (pane-frame sheet))
	(window (realize-mirror-aux port sheet
				    :map nil
				    :event-mask '(:structure-notify))))
    (setf (xlib:wm-name window) (frame-pretty-name frame))
    (setf (xlib:wm-icon-name window) (frame-pretty-name frame))
    (setf (xlib:wm-protocols window) `(:wm_delete_window))))

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
				   ;:pointer-motion
				    :button-motion
				    :owner-grab-button)
                      :map (sheet-enabled-p sheet)))

(defmethod realize-mirror ((port clx-port) (sheet interactor-pane))
  (realize-mirror-aux port sheet
		      :event-mask '(:exposure
				    :key-press :key-release
				    :button-press :button-release
				    :enter-window :leave-window
				    :structure-notify
				    :pointer-motion
				    :button-motion
				    :owner-grab-button)
                      :map (sheet-enabled-p sheet)))

(defmethod realize-mirror ((port clx-port) (sheet application-pane))
  (realize-mirror-aux port sheet
		      :event-mask '(:exposure
				    :key-press :key-release
				    :button-press :button-release
				    :enter-window :leave-window
				    :structure-notify
				    :pointer-motion
				    :button-motion
				    :owner-grab-button)
                      :map (sheet-enabled-p sheet)))

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

(defmethod port-set-sheet-transformation ((port clx-port) (graft graft) transformation)
  (declare (ignore transformation))
  nil)

#+NIL
(defmethod port-set-sheet-transformation ((port clx-port) (pane application-pane) transformation)
  (declare (ignore transformation))
  nil)

#+NIL
(defmethod port-set-sheet-transformation ((port clx-port) (pane interactor-pane) transformation)
  (declare (ignore transformation))
  nil)

(defmethod port-set-sheet-transformation ((port clx-port) (sheet mirrored-sheet-mixin) transformation)
  (declare (ignore transformation)) ;; why?
  (let ((mirror (sheet-direct-mirror sheet)))
    (multiple-value-bind (tr rg) (invent-sheet-mirror-transformation-and-region sheet)
      (multiple-value-bind (x y) (transform-position tr 0 0)
        (multiple-value-bind (x1 y1 x2 y2) (if (eql rg +nowhere+)
                                               (values 0 0 0 0)
                                               (bounding-rectangle* rg))
          (declare (ignore x1 y1))      ;XXX assumed to be 0
          (setf (xlib:drawable-x mirror) (round x)
                (xlib:drawable-y mirror) (round y))
          (setf (xlib:drawable-width mirror)  (clamp 1 (round x2) #xFFFF)
                (xlib:drawable-height mirror) (clamp 1 (round y2) #xFFFF))
          ;;(xlib:clear-area mirror :exposures-p t)
          (invalidate-cached-transformations sheet)
          )))))

(defmethod port-set-sheet-region ((port clx-port) (sheet mirrored-sheet-mixin) region)
  (declare (ignore region)) ;; why?
  (let ((mirror (sheet-direct-mirror sheet)))
    (multiple-value-bind (tr rg) (invent-sheet-mirror-transformation-and-region sheet)
      (declare (ignore tr))
      (multiple-value-bind (x1 y1 x2 y2) (if (eql rg +nowhere+)
                                             (values 0 0 0 0)
                                             (bounding-rectangle* rg))
        (declare (ignore x1 y1))      ;XXX assumed to be 0
        (setf x2 (round x2))
        (setf y2 (round y2))
        (cond ((or (<= x2 0) (<= y2 0))
               ;; XXX
               ;; now X does not allow for a zero width/height window,
               ;; we should unmap instead ...
               ;; Nevertheless we simply clamp
               ))
        (setf (xlib:drawable-width mirror)  (clamp x2 1 #xFFFF)
              (xlib:drawable-height mirror) (clamp y2 1 #xFFFF))))))

(defmethod port-enable-sheet ((port clx-port) (mirror mirrored-sheet-mixin))
  (xlib:map-window (sheet-direct-mirror mirror)) )

(defmethod port-disable-sheet ((port clx-port) (mirror mirrored-sheet-mixin))
  (xlib:unmap-window (sheet-direct-mirror mirror)) )

(defmethod destroy-port :before ((port clx-port))
  (xlib:close-display (clx-port-display port)))

; think about rewriting this macro to be nicer
(defmacro peek-event ((display &rest keys) &body body)
  (let ((escape (gensym)))
    `(block ,escape
       (xlib:process-event ,display :timeout 0 :peek-p t :handler
         #'(lambda (&key ,@keys &allow-other-keys)
             (return-from ,escape
               (progn
                 ,@body)))))))

(defun decode-x-button-code (code)
  ;; FIXME: X mouse has 5 buttons.
  (aref #.(vector +pointer-left-button+
                  +pointer-middle-button+
                  +pointer-right-button+)
        (1- code)))

(defun event-handler (&rest event-slots
                      &key display window event-key code state mode time width height x y data count
                      &allow-other-keys)
  ;; XXX :button code -> :button (decode-x-button-code code)
  (declare (ignorable event-slots))
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
	 (make-instance 'pointer-button-release-event :pointer 0
                        :button (decode-x-button-code code) :x x :y y
			:sheet sheet :modifier-state state :timestamp time))
	(:button-press
	 (make-instance 'pointer-button-press-event :pointer 0
                        :button (decode-x-button-code code) :x x :y y
			:sheet sheet :modifier-state state :timestamp time))
	(:enter-notify
	 (make-instance 'pointer-enter-event :pointer 0 :button code :x x :y y
			:sheet sheet :modifier-state state :timestamp time))
	(:leave-notify
	 (make-instance (if (eq mode :ungrab) 'pointer-ungrab-event 'pointer-exit-event)
	   :pointer 0 :button code :x x :y y
	   :sheet sheet :modifier-state state :timestamp time))
	(:configure-notify
         ; it would be nice to consolidate these for resizes, but because of the
         ; interleaving exposures it becomes a bit tricky to do at this point. - BTS
	 (make-instance 'window-configuration-event :sheet sheet
			:x x :y y :width width :height height))
	(:destroy-notify
	 (make-instance 'window-destroy-event :sheet sheet))
	(:motion-notify
         (unless (eq :motion-notify (peek-event (display event-key) event-key))
           ; consolidate motion notifications
	   (make-instance 'pointer-motion-event :pointer 0 :button code :x x :y y
			  :sheet sheet :modifier-state state :timestamp time)))
	((:exposure :display) ; what is a :display event?
         (when (eq count 0)
           ; this should also consolidate the areas, but try this for now
           (make-instance 'window-repaint-event
             :sheet sheet
             :region (untransform-region (sheet-native-transformation sheet)
                                         (make-rectangle* x y (+ x width) (+ y height))))))
	(:client-message
	 (when (eq (xlib:atom-name display (aref data 0)) :wm_delete_window)
	   (destroy-mirror (port sheet) sheet)
	   (make-instance 'window-manager-delete-event
	     :sheet sheet
	     :timestamp time)))
	(t
	 nil)))))

(defmethod get-next-event ((port clx-port) &key wait-function (timeout nil))
  (declare (ignore wait-function))
  (let* ((*clx-port* port)
         (display    (clx-port-display port)))
    (declare (special *clx-port*))
    (unless (xlib:event-listen display)
      (xlib:display-finish-output (clx-port-display port)))
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
        (multiple-value-bind (family face size) (text-style-components text-style)
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

;; this is evil.
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


