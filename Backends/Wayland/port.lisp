(in-package #:clim-wayland)

(defclass wayland-port (basic-port)
  ((device :initform nil
           :accessor wayland-port-device)
   (display :initform nil
            :accessor wayland-port-display)
   (compositor :initform nil
               :accessor wayland-port-compositor)
   (surface :initform nil
            :accessor wayland-port-surface)
   (window :initform nil
           :accessor wayland-port-window)
   (registry :initform nil
             :accessor wayland-port-registry)
   (wm-base :initform nil
            :accessor %wayland-wm-base)
   (top-level :initform nil
              :accessor %xdg-top-level)))

(defun parse-server-path (server-path)
  (format t "server path: ~a~%" server-path)
  (list :display-id (get-environment-variable "WAYLAND_DISPLAY")))

(defmethod find-port-type ((port (eql :wayland-ffi)))
  (values (find-class 'wayland-port) 'parse-server-path))

(defmethod find-port-type :after ((port (eql :wayland-ffi)))
  (format t "find-port-type wayland-ffi: ~a" port))

;;; wl-callbacks created from this class will call the callback fun
(defclass %wayland-invoking-callback (wlc:wl-callback)
  ((fun :type (function ((unsigned-byte 32)) *) :accessor fun :initarg :fun)))

(defmethod wlc:wl-callback-done ((self %wayland-invoking-callback) data)
  (funcall (fun self) data))

(defun roundtrip (display)
  "Wait for all previous requests to be processed by the wayland compositor"
  (let (callback done-p)
    (unwind-protect
         (flet ((set-done (x)
                  (declare (ignore x))
                  (setf done-p t)))
           ;; This request simply invokes the provided callback as
           ;; soon as it's processed. Since Wayland processes requests
           ;; in order, it won't be processed until all prior requests
           ;; are done being processed.
           (setf callback (wlc:wl-display-sync
                           display
                           (make-instance '%wayland-invoking-callback
                                          :fun #'set-done)))
           (loop until done-p
                 do (wlc:wl-display-dispatch display)))
      (when callback (wlc:wayland-destroy callback)))))

;;; Create a xdg_wm_base subclass which responds to pings
(defclass xdg-wm-base-pingpong (xdg:xdg-wm-base) ())

;;; Every time we receive a ping, send back a pong
(defmethod xdg:xdg-wm-base-ping ((self xdg-wm-base-pingpong) serial)
  (xdg:xdg-wm-base-pong self serial))

(defclass wayland-globals (wlc:wl-registry)
  ((globals :type list :accessor globals :initform nil)))

(defmethod wlc:wl-registry-global :after
    ((registry wayland-globals) name interface version)
  (pushnew (list name interface version) (globals registry)))

(defmethod wlc:wl-registry-global-remove :after
    ((registry wayland-globals) name)
  (setf (globals registry) (delete name (globals registry) :key #'first)))

(defun wl-registry-find-or-lose (registry interface &optional version)
  (or (dolist (global (globals registry))
        (destructuring-bind (gname ginterface gversion) global
          (when (and (equal ginterface interface)
                     (or (null version) (>= gversion version)))
            (return (values gname gversion)))))
      (error "Wayland: could not find interface ~A~@[ version ~A~] in registry"
             interface version)))

(defun bind-wayland-registry (port wl-protocol registry-string version)
  (with-accessors ((registry wayland-port-registry)) port
    (let ((registry-match (wl-registry-find-or-lose registry
                                                    registry-string
                                                    version)))
      (format t "binding: ~S ~S ~S ~S~%" port wl-protocol registry-string version)
      (wlc:wl-registry-bind registry registry-match wl-protocol))))


(defun initialize-wayland (port)
  (setf (wayland-port-display port) (wlc:wl-display-connect nil)

        (wayland-port-registry port) (wlc:wl-display-get-registry
                                      (wayland-port-display port)
                                      (make-instance 'wayland-globals)))

  ;; Initiates a waylond connection and is informed by the server of the server's globals
  (roundtrip (wayland-port-display port))

  ;; Now we bind to some of the globals for local port state. The act of binding also causes some wayland events to fire
  (alx:when-let* ((compositor (bind-wayland-registry
                               port
                               (make-instance 'wlc:wl-compositor :version 4)
                               "wl_compositor" 4))
                  (device (bind-wayland-registry
                           port
                           (make-instance 'wayland-port-screen :version 3)
                           "wl_output" 3)))
    (with-accessors ((port-compositor wayland-port-compositor)
                     (port-device wayland-port-device)
                     (port-window wayland-port-window)
                     (port-wm-base %wayland-wm-base)
                     (port-surface wayland-port-surface)
                     (port-top-level %xdg-top-level))
        port

      (setf port-compositor compositor

            port-device device

            port-window (wlc:wl-compositor-create-surface
                         compositor
                         (make-instance 'wlc:wl-surface))

            port-wm-base (bind-wayland-registry
                          port
                          (make-instance 'xdg-wm-base-pingpong :version 1)
                          "xdg_wm_base" 1)

            port-surface (xdg:xdg-wm-base-get-xdg-surface
                          port-wm-base
                          (make-instance 'xdg:xdg-surface)
                          port-window)

            port-top-level (xdg:xdg-surface-get-toplevel
                            port-surface
                            (make-instance 'xdg:xdg-toplevel))))

    ;; Make one more round trip to handle events triggered by bindings above
    ;; before the main event loop is started
    (roundtrip (wayland-port-display port))))

(defmethod initialize-instance :after ((port wayland-port) &key)
  (with-slots (frame-managers) port
    (push (apply #'make-instance 'wayland-frame-manager :port port nil)
          frame-managers))
  (initialize-wayland port)
  (make-graft port)
  (when clim-sys:*multiprocessing-p*
    (flet ((wayland-port-event-loop ()
             (loop
               (with-simple-restart
                   (restart-event-loop
                    "Restart Wayland event loop.")
                 (loop (process-next-event port))))))
      (clim-sys:make-process #'wayland-port-event-loop
                             :name (format nil "~S's event process." port)))))


(defmethod destroy-port :before ((port wayland-port))
  (alx:when-let ((display (wayland-port-display port)))
    (wlc:wayland-destroy (%wayland-wm-base port))
    (wlc:wayland-destroy (wayland-port-window port))
    (wlc:wayland-destroy (wayland-port-compositor port))
    (wlc:wayland-destroy (wayland-port-registry port))

    (setf (wayland-port-display port) nil)
    (wlc:wl-display-disconnect display)))

(defmethod port-force-output ((port wayland-port))
  (wlc:wl-surface-commit (wayland-port-window port)))

;;; Grafts

(defclass wayland-graft (graft)
  ())

(defmethod graft-width ((graft wayland-graft) &key (units :device))
  (let ((screen (wayland-port-device (port graft))))
    (ecase units
      (:device (screen-width screen))
      (:inches (/ (screen-physical-width screen) 25.4s0))
      (:millimeters (screen-physical-width screen))
      (:screen-sized 1))))

(defmethod graft-height ((graft wayland-graft) &key (units :device))
  (let ((screen (wayland-port-device (port graft))))
    (ecase units
      (:device (screen-height screen))
      (:inches (/ (screen-physical-height screen) 25.4s0))
      (:millimeters (screen-physical-height screen))
      (:screen-sized 1))))

(defmethod make-graft ((port wayland-port) &key (orientation :default)
                                             (units :device))
  (let* ((screen (wayland-port-device port))
         (root   (wayland-port-window port))
         (mirror (make-instance 'wayland-egl-mirror :window root))
         (width  (screen-width screen))
         (height (screen-height screen))
         (region (make-bounding-rectangle 0 0 width height)))
    (make-instance 'wayland-graft :port port
                                  :region region
                                  :mirror mirror
                                  :orientation orientation
                                  :units units)))

(defmethod instance-initialize :after ((graft wayland-graft))
  (with-slots (egl-window egl-context egl-display egl-surface)
      (sheet-mirror graft)
    (setf egl-window (create-native-window graft))
    (multiple-value-setq (egl-display egl-surface egl-context)
      (create-egl-context graft))))

(defun create-native-window (graft)
  (with-accessors ((port-compositor wayland-port-compositor)
                   (port-window wayland-port-window))
      (port graft)
    (let ((region
            (wlc:wl-compositor-create-region port-compositor
                                             (make-instance 'wlc:wl-region)))
          (width (graft-width graft))
          (height (graft-height graft)))
      (wlc:wl-region-add region 0 0 width height)
      (wlc:wl-surface-set-opaque-region port-window region)
      (wl-egl:wl-egl-window-create (wl-core:pointer port-window)
                                   width
                                   height))))

(defun create-egl-context (graft)
  (with-accessors ((native-display wayland-port-display))
      (port graft)
    (let* ((egl-display (egl:get-display (wl-core:pointer native-display))))
      (format t "egl init ~s~%"
              (multiple-value-list (egl:initialize egl-display)))
      (egl:bind-api :opengl-api)

      (let* ((config (egl:choose-config egl-display 1
                                        :surface-type :window-bit
                                        :renderable-type :opengl-bit
                                        :red-size 8
                                        :green-size 8
                                        :blue-size 8
                                        :none))
             (surface (egl:create-window-surface egl-display
                                                 (first config)
                                                 (wayland-egl-mirror-window (sheet-mirror graft))
                                                 (cffi:null-pointer)))
             (context (egl:create-context egl-display
                                          (first config)
                                          (cffi:null-pointer)
                                          :context-major-version 2
                                          :none)))
        (egl:make-current egl-display surface surface context)
        (values egl-display surface context)))))


(defclass wayland-frame-manager (standard-frame-manager)
  ((mirroring :initarg :mirroring
              :initform nil
              :reader mirroring)
   (class-gensym :initarg :class-gensym
                 :initform (gensym "WAYLAND-")
                 :reader class-gensym)))
