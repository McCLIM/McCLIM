(in-package :clim-clx-fb)

(defclass clx-fb-port (render-port-mixin
		       clim-xcommon:keysym-port-mixin
		       clim-clx::clx-basic-port)
  ())

(defmethod find-port-type ((type (eql :clx-fb)))
  (values 'clx-fb-port (nth-value 1 (find-port-type :clx))))

(defmethod initialize-instance :after ((port clx-fb-port) &rest args)
  (declare (ignore args))
  (push (make-instance 'clx-fb-frame-manager :port port)
	(slot-value port 'frame-managers))
  (setf (slot-value port 'pointer)
	(make-instance 'clim-clx::clx-basic-pointer :port port))
  (initialize-clx port)
  (initialize-clx-framebuffer port)
  (clim-extensions:port-all-font-families port))

(defun initialize-clx-framebuffer (port)
  (clim-sys:make-process
   (lambda ()
     (loop with mirror->%image = (slot-value port 'mirror->%image)
           do (handler-case
                  (maphash-values
                   (lambda (image)
                     (when (typep image 'clx-fb-mirror)
                       (image-mirror-to-x image)))
                   mirror->%image)
                (condition (condition)
                  (format *debug-io* "~A~%" condition)))
              (xlib:display-force-output (clx-port-display port))
              (sleep 0.01)))
   :name (format nil "~S's event process." port)))

(defparameter *event-mask* '(:exposure
			     :key-press :key-release
			     :button-press :button-release
			     :owner-grab-button
			     :enter-window :leave-window
			     :structure-notify
			     :pointer-motion :button-motion))

(defmethod realize-mirror ((port clx-fb-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (clim-clx::%realize-mirror port sheet)))
    (port-register-mirror port sheet mirror)
    (setf (mirror->%image port mirror)
          (make-instance 'clx-fb-mirror :xmirror mirror))
    mirror))

(defmethod realize-mirror ((port clx-fb-port) (pixmap pixmap))
  )

(defmethod clim-clx::%realize-mirror ((port clx-fb-port) (sheet basic-sheet))
  (clim-clx::realize-mirror-aux port sheet
		      :event-mask *event-mask*
                      :map (sheet-enabled-p sheet)))

(defmethod clim-clx::%realize-mirror ((port clx-fb-port) (sheet top-level-sheet-mixin))
  (let ((q (compose-space sheet)))
    (let ((frame (pane-frame sheet))
          (window (clim-clx::realize-mirror-aux port sheet
				      :event-mask *event-mask*
                                      :map nil
                                      :width (clim-clx::round-coordinate (space-requirement-width q))
                                      :height (clim-clx::round-coordinate (space-requirement-height q)))))
      (setf (xlib:wm-hints window) (xlib:make-wm-hints :input :on))
      (setf (xlib:wm-name window) (frame-pretty-name frame))
      (setf (xlib:wm-icon-name window) (frame-pretty-name frame))
      (xlib:set-wm-class
       window
       (string-downcase (frame-name frame))
       (string-capitalize (string-downcase (frame-name frame))))
      (setf (xlib:wm-protocols window) `(:wm_delete_window))
      (xlib:change-property window
                            :WM_CLIENT_LEADER (list (xlib:window-id window))
                            :WINDOW 32)
      window)))

(defmethod clim-clx::%realize-mirror ((port clx-fb-port) (sheet unmanaged-sheet-mixin))
  (clim-clx::realize-mirror-aux port sheet
		      :event-mask *event-mask*
		      :override-redirect :on
		      :map nil))

(defmethod make-medium ((port clx-fb-port) sheet)
  (make-instance 'clx-fb-medium
		 ;; :port port
		 ;; :graft (find-graft :port port)
		 :sheet sheet))

(defmethod make-graft ((port clx-fb-port) &key (orientation :default) (units :device))
  (let ((graft (make-instance 'clx-graft
		              :port port :mirror (clx-port-window port)
		              :orientation orientation :units units))
        (width (xlib:screen-width (clx-port-screen port)))
        (height (xlib:screen-height (clx-port-screen port))))
    (let ((region (make-bounding-rectangle 0 0 width height)))
      (climi::%%set-sheet-region region graft))
    graft))

(defmethod graft ((port clx-fb-port))
  (first (port-grafts port)))

(defmethod port-force-output ((port clx-fb-port))
  (maphash-values (lambda (image)
                    (when (typep image 'clx-fb-mirror)
                      (%mirror-force-output image)))
                  (slot-value port 'mirror->%image))
  (xlib:display-force-output (clx-port-display port)))

;;; Pixmap

(defmethod destroy-mirror ((port clx-fb-port) (pixmap image-pixmap-mixin))
  (call-next-method))

(defmethod realize-mirror ((port clx-fb-port) (pixmap image-pixmap-mixin))
  (setf (sheet-parent pixmap) (graft port))
  (let ((mirror (make-instance 'image-mirror-mixin)))
    (port-register-mirror port pixmap mirror)
    (setf (mirror->%image port mirror) mirror)
    (%make-image mirror pixmap)
    mirror))

(defmethod port-allocate-pixmap ((port clx-fb-port) sheet width height)
  (let ((pixmap (make-instance 'clx-fb-pixmap
			       :sheet sheet
			       :width width
			       :height height
			       :port port)))
    (when (sheet-grafted-p sheet)
      (realize-mirror port pixmap))
    pixmap))

(defmethod port-deallocate-pixmap ((port clx-fb-port) pixmap)
  (when (pixmap-mirror pixmap)
    (destroy-mirror port pixmap)))
