(in-package #:mcclim-wayland)

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

(defun roundtrip (port)
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
                           (wayland-port-display port)
                           (make-instance '%wayland-invoking-callback
                                          :fun #'set-done)))
           (loop until done-p
                 do (process-next-event port)))
      (when callback (wlc:wayland-destroy callback)))))

;;; Create a xdg_wm_base subclass which responds to pings
(defclass xdg-wm-base-pingpong (xdg:xdg-wm-base) ())

;;; Every time we receive a ping, send back a pong
(defmethod xdg:xdg-wm-base-ping ((self xdg-wm-base-pingpong) serial)
  (xdg:xdg-wm-base-pong self serial))

(defclass wayland-globals (wlc:wl-registry)
  ((globals :type list :accessor globals :initform nil)))

(defclass wayland-xdg-toplevel (xdg:xdg-toplevel)
  ())

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
  (roundtrip port)

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

            ;; Create XDG Surface
            port-surface (xdg:xdg-wm-base-get-xdg-surface
                          port-wm-base
                          (make-instance 'xdg:xdg-surface)
                          port-window)

            ;; Assign toplevel role to XDG surface
            port-top-level (xdg:xdg-surface-get-toplevel
                            port-surface
                            (make-instance 'wayland-xdg-toplevel))))

    ;; Make one more round trip to handle events triggered by bindings above
    ;; before the main event loop is started
    (roundtrip port)
    (wlc:wl-surface-commit (wayland-port-window port))))

(defmethod initialize-instance :after ((port wayland-port) &key)
  (with-slots (frame-managers) port
    (push (apply #'make-instance 'standard-frame-manager :port port nil)
          frame-managers))
  (initialize-wayland port)
  (make-graft port)
  (format t "Starting Event Loop~%")
  (when clim-sys:*multiprocessing-p*
    (flet ((wayland-port-event-loop ()
             (loop
               (with-simple-restart
                   (restart-event-loop
                    "Restart Wayland event loop.")
                 (loop (process-next-event port))))))
      (setf (port-event-process port)
            (clim-sys:make-process
             #'wayland-port-event-loop
             :name (format nil "~S's event process." port))))))


(defmethod destroy-port :before ((port wayland-port))
  (alx:when-let ((display (wayland-port-display port)))
    (with-accessors ((wm-base %wayland-wm-base)
                     (window wayland-port-window)
                     (compositor wayland-port-compositor)
                     (registry wayland-port-registry))
        port
      (loop for wayland-object in (list wm-base window compositor registry)
            when wayland-object
              do (wlc:wayland-destroy wayland-object)))

    (setf (wayland-port-display port) nil)
    (wlc:wl-display-disconnect display)))

(defmethod port-force-output ((port wayland-port))
  (format t "PORT-FORCE-OUTPUT~%")
  ;; (alx:when-let* ((graft (graft port))
  ;;                 (mirror (sheet-mirror graft))
  ;;                 (egl-display (wayland-egl-mirror-display mirror))
  ;;                 (egl-surface (wayland-egl-mirror-surface mirror)))
  ;;   (format t "egl swap buffers~%")
  ;;   (egl:swap-buffers egl-display egl-surface))
  (map-over-grafts #'%graft-force-output port)
  (wlc:wl-surface-commit (wayland-port-window port)))

;;; Port event handling
(defmethod handle-event
    ((port wayland-port) (event window-configuration-event))

  (let ((sheet (event-sheet event))
        (native-region (window-event-native-region event)))
    (format t "window-configuration-event ~S ~S~%" sheet native-region)))

(defmethod port-set-mirror-geometry
    ((port wayland-port) (sheet mirrored-sheet-mixin) region)
  (alx:when-let ((mirror (sheet-direct-mirror sheet)))
    (with-bounding-rectangle* (x1 y1 x2 y2 :width w :height h) region
      (with-bounding-rectangle* (ox1 oy1 ox2 oy2) (sheet-mirror-geometry sheet)
        (let ((window (window mirror)))
          (when (some #'/=
                      (list x1 y1 x2 y2)
                      (list ox1 oy1 ox2 oy2))
            (format t "port-set-mirror-geometry~%")
            ;TODO: determine if we need CLX's ROUND-COORDINATE
            (xdg:xdg-surface-set-window-geometry (slot-value port 'surface) x1 y1 w h))))
      (values x1 y1 x2 y2))))

(defmethod port-enable-sheet ((port wayland-port) (sheet mirrored-sheet-mixin))
  (format t "port enable-sheet ~a ~a ~%" port sheet)
  (alx:when-let ((mirror (sheet-direct-mirror sheet)))
    (port-force-output port)))

(defmethod port-disable-sheet ((port wayland-port) (sheet mirrored-sheet-mixin))
  (format t "port disable-sheet ~a ~a ~%" port sheet)
  (alx:when-let ((mirror (sheet-direct-mirror sheet)))
    (wlc:wl-surface-attach (wayland-port-window port) nil 0 0)
    (wlc:wl-surface-commit (wayland-port-window port))))

(defmethod graft ((port wayland-port))
  (first (port-grafts port)))

;;; Grafts

(defclass wayland-graft (graft)
  ())

(defmethod handle-event :before
    ((sheet top-level-sheet-pane) (event window-configuration-event))
  ;; Is this where I should set the egl-window width and swap buffers?
  ;; Should this be handled by the medium? or te mirror?
  (alx:when-let ((mirror (sheet-mirror sheet)))
    (format *debug-io* "handle-event window-cfg-event: top level reconfigure? Should I resize all the children?~%")
    ;; QQQQ Should I use port-set-mirror-geometry or is there some other way
    ;; to trigger it?
    ;; (break)
    (unless (eql +everywhere+ (window-event-region event))
      (with-bounding-rectangle* (x1 y1 x2 y2 :width w :height h)
          (window-event-region event)
        (when (every #'plusp (list w h))
          (with-bounding-rectangle* (ox1 oy1 ox2 oy2)
              ;; QQQQ why is this -5 1 -5 1 still
              (sheet-mirror-geometry sheet)
            (when (some #'/=
                        (list x1 y1 x2 y2)
                        (list ox1 oy1 ox2 oy2))
              (format *debug-io* "resize surface geometry new:~s current:~s~%"
                      (list x1 y1 x2 y2)
                      (list ox1 oy1 ox2 oy2))
              ;; TODO: determine if we need CLX's ROUND-COORDINATE
              ;; This xdg request will cause another window configuration event.
              (xdg:xdg-surface-set-window-geometry
               (slot-value (port sheet) 'surface) x1 y1 w h)

              (wl-egl:wl-egl-window-resize
               (wayland-egl-mirror-window mirror) w h x1 y1)
              ;; Is this overkill?
              (port-force-output (port sheet))
              ;; (wlc:wl-surface-commit (wayland-port-window (port sheet)))
              )))))))

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

(defclass wayland-egl-medium (basic-medium) ())

(defmethod make-medium ((port wayland-port) sheet)
  (make-instance 'wayland-egl-medium :port port :sheet sheet))

(defmethod medium-finish-output ((medium wayland-egl-medium))
  (alx:when-let ((mirror (sheet-mirror (medium-sheet medium))))
    (format t "medium-finish-output~%")
    (egl:swap-buffers (wayland-egl-mirror-display mirror)
                      (wayland-egl-mirror-window mirror))))

(defmethod medium-force-output ((medium wayland-egl-medium))
  (alx:when-let ((mirror (sheet-mirror (medium-sheet medium))))
    (gl:flush)
    (format t "medium-force-output~%")
    (egl:swap-buffers (wayland-egl-mirror-display mirror)
                      (wayland-egl-mirror-window mirror))))

(defmethod medium-clear-area :after
    ((medium wayland-egl-medium) left top right bottom)
  (format t "EGL medium clear area called~%"))

(defmethod medium-draw-polygon*
    ((medium wayland-egl-medium) coord-seq closed filled)
  (declare (ignore coord-seq closed filled))
  nil)
