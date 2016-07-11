(in-package :clim-clxv2)

(defclass clxv2-port (clim-clx::clx-port)
  ((mirroring :accessor clxv2-port-mirroring)
   (port-pointer-pressed-sheet :initform nil :accessor port-pointer-pressed-sheet)))

(defun parse-clxv2-server-path (path)
  (let ((server-path (clim-clx::parse-clx-server-path path)))
    (pop path)
    (cons :clxv2 (append (list :mirroring  (getf path :mirroring :none)) (cdr server-path)))))

(setf (get :clxv2 :port-type) 'clxv2-port)
(setf (get :clxv2 :server-path-parser) 'parse-clxv2-server-path)

(defmethod initialize-instance :after ((port clxv2-port) &rest args)
  (declare (ignore args))
  (push (make-instance 'clxv2-frame-manager :port port)
	(slot-value port 'frame-managers))
  (setf (slot-value port 'mirroring)
	(getf (cdr (port-server-path port)) :mirroring)))


(defparameter *event-mask* '(:exposure 
			     :key-press :key-release
			     :button-press :button-release
			     :owner-grab-button
			     :enter-window :leave-window
			     :structure-notify
			     :pointer-motion :button-motion))

(defmethod clim-clx::realize-mirror ((port clxv2-port) (sheet mirrored-sheet-mixin))
  (clim-clx::%realize-mirror port sheet))

(defmethod clim-clx::%realize-mirror ((port clxv2-port) (sheet basic-sheet))
  (clim-clx::realize-mirror-aux port sheet
		      :event-mask *event-mask*
                      :border-width 0
                      :map (sheet-enabled-p sheet)))

(defmethod clim-clx::%realize-mirror ((port clxv2-port) (sheet top-level-sheet-pane))
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
                            :WINDOW 32))))

(defmethod clim-clx::%realize-mirror ((port clxv2-port) (sheet unmanaged-top-level-sheet-pane))
  (clim-clx::realize-mirror-aux port sheet
		      :event-mask *event-mask*
		      :override-redirect :on
		      :map nil))



;;;
;;;
;;;

(defclass clxv2-mirrored-sheet-mixin (mirrored-sheet-mixin)
  ())


