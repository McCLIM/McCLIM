(in-package #:mcclim-wayland)

;;; Mirror protocol?

(defclass wayland-mirror ()
  ((window :initarg :window :reader window)
   (buffering-p :initarg :buffering-p :accessor buffering-p))
  (:default-initargs :window (alx:required-argument :window)
                     :buffering-p nil))

(defclass wayland-egl-mirror (wayland-mirror)
  ((egl-window :initform nil :accessor wayland-egl-mirror-window)
   (egl-context :initform nil :accessor wayland-egl-mirror-context)
   (egl-display :initform nil :accessor wayland-egl-mirror-display)
   (egl-surface  :initform nil :accessor wayland-egl-mirror-surface)))

(defun create-native-window (port)
  ;; TODO: "native" has specific meaning in CLIM; figure out if I'm using it
  ;; correctly here.
  ;; If so, I may need to set the native transformation and region on the
  ;; mirror, graft, or both, here. Maybe /this/ is where I could set the
  ;; flipped Y of opengl.
  (format *debug-io* "starting CREATE-NATIVE-WINDOW~%")
  (with-accessors ((port-compositor wayland-port-compositor)
                   (port-window wayland-port-window))
      port
    (let ((region
            (wlc:wl-compositor-create-region port-compositor
                                             (make-instance 'wlc:wl-region)))
          (width (graft-width (graft port)))
          (height (graft-height (graft port))))
      (wlc:wl-region-add region 0 0 width height)
      (wlc:wl-surface-set-opaque-region port-window region)
      (wl-egl:wl-egl-window-create (wl-core:pointer port-window)
                                   width
                                   height))))

(defun create-egl-context (port mirror)
  (with-accessors ((native-display wayland-port-display))
      port
    (let* ((egl-display (egl:get-display (wl-core:pointer native-display))))
      (format *debug-io* "egl init ~s~%"
              (multiple-value-list (egl:initialize egl-display)))
      (egl:bind-api :opengl-api)

      (let* ((config (first (egl:choose-config egl-display 1
                                               :surface-type :window-bit
                                               :renderable-type :opengl-bit
                                               :red-size 8
                                               :green-size 8
                                               :blue-size 8
                                               :none)))

             (surface (egl:create-window-surface egl-display
                                                 config
                                                 (wayland-egl-mirror-window mirror)
                                                 (cffi:null-pointer)))
             (context (egl:create-context egl-display
                                          config
                                          (cffi:null-pointer)
                                          :context-major-version 2
                                          :none)))
        (egl:make-current egl-display surface surface context)
        (values egl-display surface context)))))

(defmethod port-set-mirror-name
    ((port wayland-port) (sheet mirrored-sheet-mixin) name)
  (format *debug-io* "PORT-SET-MIRROR-NAME ~a~%" name)
  (alx:when-let ((top-level-window (%xdg-top-level port)))
    (xdg:xdg-toplevel-set-title top-level-window name)))

(defmethod realize-mirror ((port wayland-port) (sheet mirrored-sheet-mixin))
  (format *debug-io* "realizing mirror mirrored-sheet-mixin~%")
  ;; (break "realize-mirror" sheet)
  (with-bounding-rectangle* (x y :width w :height h) sheet
    (let ((native-region (make-rectangle* 0 0 w h))
          (native-transformation (make-translation-transformation (- x) (- y)))
         ;; QQQQ I'm still having conceptual ignorance on the lifecycle of graft
         ;; mirror instantiation and realize mirror. I hope it's not too
         ;; cargo-culted from CLX.
         (mirror (sheet-direct-mirror (graft port))))
     ;; we'd like this set earlier than the basic-port specialization

      ;; (resize-sheet mirror w h)

      ;; (setf (climi::%sheet-direct-mirror sheet) mirror)
      ;; (climi::update-mirror-geometry sheet)
      ;; (break "realize-mirror" sheet mirror)
      ;; (climi::dispatch-repaint sheet +everywhere+)
      ;; (setf (climi::%sheet-native-region sheet) native-region
      ;;       (climi::%sheet-native-transformation sheet) native-transformation)

      mirror)))

(defmethod realize-mirror :before
    ((port wayland-port) (sheet mirrored-sheet-mixin))


  (with-accessors ((port-compositor wayland-port-compositor)
                   (port-window wayland-port-window)
                   (port-wm-base %wayland-wm-base)
                   (port-surface wayland-port-surface)
                   (toplevel-surface %xdg-top-level))
      port
    (format *debug-io*
            "realizing mirror top-level; assigning xdg toplevel role~%")
    (unless port-window
      ;; create compositor window if it's not there
      (format *debug-io* "creating surface for compositor~%")
      (setf port-window (wlc:wl-compositor-create-surface
                         port-compositor
                         (make-instance 'wlc:wl-surface))))
    (setf
     ;; Create XDG surface for top-level
     port-surface
     (xdg:xdg-wm-base-get-xdg-surface port-wm-base
                                      (make-instance 'xdg:xdg-surface)
                                      port-window)
     ;; Assign toplevel role to XDG surface
     toplevel-surface (xdg:xdg-surface-get-toplevel
                       port-surface
                       (make-instance 'wayland-xdg-toplevel)))
    (wlc:wl-surface-commit port-window))


  )

(defmethod realize-mirror :before
    ((port wayland-port) (sheet top-level-sheet-mixin))
  ;; this is probably too cute with CLOS precedence and should be merged into the
  ;; REALIZE-MIRROR for mirrored-sheet-mixin
  (format *debug-io* "BEFORE realize-mirror top-level ~%")
  (let ((mirror (sheet-direct-mirror (graft sheet))))
    (with-slots (egl-window egl-context egl-display egl-surface)
        mirror
      (setf egl-window (create-native-window port))
      (format *debug-io* "creating egl context~%")
      (multiple-value-setq (egl-display egl-surface egl-context)
        (create-egl-context port mirror)))))

;; (defmethod realize-mirror :after
;;     ((port wayland-port) (sheet top-level-sheet-mixin))
;;   (climi::update-mirror-geometry (graft sheet)))

(defmethod destroy-mirror ((port wayland-port) (sheet mirrored-sheet-mixin))
  (format *debug-io* "destroying mirror~%")
  (let ((mirror (sheet-direct-mirror sheet)))
    ;; (break "destroy-mirror" mirror sheet)
    (with-slots (egl-window egl-context egl-display egl-surface)
        mirror
      ;; Unmap the window
      (port-disable-sheet port sheet)

      (wl-egl:wl-egl-window-destroy egl-window)
      (wlc:wl-display-flush (wayland-port-display port)))))

(defmethod destroy-mirror :before ((port wayland-port)
                                   (sheet top-level-sheet-mixin))
  (format *debug-io* "destroying top-level-sheet-mixin step~%")
  (with-accessors ((port-window wayland-port-window)
                   (port-surface wayland-port-surface)
                   (toplevel-surface %xdg-top-level))
      port
    ;; Destroy toplevel role and XDG surface
    (wlc:wayland-destroy toplevel-surface)
    (wlc:wayland-destroy port-surface)

    ;; Ensure port slots are nil
    (setf port-surface nil
          toplevel-surface nil)
    (wlc:wl-surface-commit port-window)))

(defun %graft-force-output (graft)
  ;; I no longer think this is the right thing to do but it still is critical
  ;; for something to show on the screen
  ;; Because we don't know the right rendering flow, we are likely swapping
  ;; buffers too often.
  ;; I believe we need to find a way to be :around redisplay-frame-panes and
  ;; swap the buffers only once
  (let ((mirror (sheet-mirror graft)))
    (format *debug-io* "egl swap buffers graft-force-output ~%")
    (egl:swap-buffers (wayland-egl-mirror-display mirror)
                      (wayland-egl-mirror-surface mirror))))

;;; TODO: determine if I need to specialize REALIZE-MIRROR on
;;; top-level-sheet-mixin and create various contexts... either wayland, egl,
;;; or opengl

(defmethod handle-event :before
    ((sheet wayland-egl-mirror) (event window-configuration-event))
  (format *debug-io* "hmmm ~%"))
