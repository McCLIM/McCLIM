(in-package #:mcclim-wayland)

;;; Mirror protocol?

(defclass wayland-mirror ()
  ((window :initarg :window :reader window))
  (:default-initargs :window (alx:required-argument :window)))

(defclass wayland-egl-mirror (wayland-mirror)
  ((egl-window :initform nil :accessor wayland-egl-mirror-window)
   (egl-context :initform nil :accessor wayland-egl-mirror-context)
   (egl-display :initform nil :accessor wayland-egl-mirror-display)
   (egl-surface  :initform nil :accessor wayland-egl-mirror-surface)))

(defun create-native-window (port)
  (format t "starting CREATE-NATIVE-WINDOW~%")
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
      (format t "egl init ~s~%"
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
  (alx:when-let ((top-level-window (%xdg-top-level port)))
    (xdg:xdg-toplevel-set-title top-level-window name)
    (port-force-output port)))

(defmethod realize-mirror ((port wayland-port) (sheet mirrored-sheet-mixin))
  (format t "realizing mirror~%")
  (let (
         ;; QQQQ I'm still having conceptual ignorance on the lifecycle of graft
         ;; mirror instantiation and realize mirror. I hope it's not too
         ;; cargo-culted from CLX.
        (mirror (sheet-direct-mirror (graft port))))
    ;; we'd like this set earlier than the basic-port specialization
    (setf (climi::%sheet-direct-mirror sheet) mirror)))

(defmethod realize-mirror :after ((port wayland-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (sheet-direct-mirror sheet)))
   (with-slots (egl-window egl-context egl-display egl-surface)
       mirror
     (setf egl-window (create-native-window port))
     (format *debug-io* "creating egl context")
     (multiple-value-setq (egl-display egl-surface egl-context)
       (create-egl-context port mirror)))))

(defmethod realize-mirror ((port wayland-port) (sheet top-level-sheet-mixin))
  (format *debug-io* "realizing mirror top-level~%"))

(defmethod destroy-mirror ((port wayland-port) (sheet mirrored-sheet-mixin))
  (format *debug-io* "destroying mirror~%")
  (port-disable-sheet port sheet)
  (let ((mirror (sheet-direct-mirror sheet)))
   (with-slots (egl-window egl-context egl-display egl-surface)
       mirror
     (cffi:foreign-free egl-window)
     (cffi:foreign-free egl-context)
     (cffi:foreign-free egl-surface)
     (cffi:foreign-free egl-display)
     (setf egl-window nil
           egl-context nil
           egl-surface nil
           egl-display nil))))

(defun %graft-force-output (graft)
  (let ((mirror (sheet-mirror graft)))
    (format t "egl swap buffers graft-force-output ~%")
    (egl:swap-buffers (wayland-egl-mirror-display mirror)
                      (wayland-egl-mirror-surface mirror))))

;;; TODO: determine if I need to specialize REALIZE-MIRROR on
;;; top-level-sheet-mixin and create various contexts... either wayland, egl,
;;; or opengl