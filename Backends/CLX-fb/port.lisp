(in-package #:clim-clx-fb)

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
           for display = (clx-port-display port)
           while display
           do (handler-case
                  (maphash-values
                   (lambda (image)
                     (when (typep image 'clx-fb-mirror)
                       (image-mirror-to-x image)))
                   mirror->%image)
                (condition (condition)
                  (format *debug-io* "~A~%" condition)))
              (ignore-errors
               (xlib:display-force-output display))
              (sleep 1/30)))
   :name (format nil "~S's event process." port)))

(defparameter *event-mask* '(:exposure
                             :key-press :key-release
                             :button-press :button-release
                             :owner-grab-button
                             :enter-window :leave-window
                             :structure-notify
                             :pointer-motion :button-motion))

(defmethod realize-mirror ((port clx-fb-port) (sheet mirrored-sheet-mixin))
  (let* ((window (clim-clx::%realize-mirror port sheet))
         (mirror (make-instance 'clx-mirror :window window))
         (%image (make-instance 'clx-fb-mirror :xmirror mirror)))
    (setf (getf (xlib:window-plist window) 'sheet) sheet)
    (setf (slot-value %image 'gcontext)
          (xlib:create-gcontext :drawable window
                                :background (values 0 0 0)
                                :foreground (values 255 255 255)))
    (setf (mirror->%image port mirror) %image)
    mirror))

(defmethod destroy-mirror ((port clx-fb-port) (sheet mirrored-sheet-mixin))
  (let* ((mirror (sheet-direct-mirror sheet))
         (%image (mirror->%image port mirror))
         (window (window mirror)))
    (with-slots (gcontext clx-image) %image
      (xlib:free-gcontext gcontext)
      ;;(xlib:destroy-image clx-image)
      (setf gcontext nil
            clx-image nil))
    (remf (xlib:window-plist window) 'sheet)
    (xlib:destroy-window window)
    (setf (mirror->%image port mirror) nil)))

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
                                      :width (space-requirement-width q)
                                      :height (space-requirement-height q))))
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
                 :port port
                 ;; :graft (find-graft :port port)
                 :sheet sheet))

(defmethod port-force-output ((port clx-fb-port))
  (maphash-values (lambda (image)
                    (when (typep image 'clx-fb-mirror)
                      (%mirror-force-output image)))
                  (slot-value port 'mirror->%image))
  (xlib:display-force-output (clx-port-display port)))

