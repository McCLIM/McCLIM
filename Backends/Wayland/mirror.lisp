(in-package #:clim-wayland)

;;; Mirror protocol?

(defclass wayland-mirror ()
  ((window :initarg :window :reader window))
  (:default-initargs :window (alx:required-argument :window)))

(defclass wayland-egl-mirror (wayland-mirror)
  ((egl-window :initform nil :accessor wayland-egl-mirror-window)
   (egl-context :initform nil :accessor wayland-egl-mirror-context)
   (egl-display :initform nil :accessor wayland-egl-mirror-display)
   (egl-surface  :initform nil :accessor wayland-egl-mirror-surface)))


(defmethod port-set-mirror-name
    ((port wayland-port) (sheet mirrored-sheet-mixin) name)
  (alx:when-let ((top-level-window (%xdg-top-level port)))
    (xdg:xdg-toplevel-set-title top-level-window name)
    (port-force-output port)))



(defmethod realize-mirror ((port wayland-port) (sheet mirrored-sheet-mixin))
  (format t "realizing mirror~%")
  (let ((root (wayland-port-window port)))
    (make-instance 'wayland-egl-mirror :window root)))

;;; TODO: determine if I need to specialize REALIZE-MIRROR on
;;; top-level-sheet-mixin and create various contexts... either wayland, egl,
;;; or opengl
