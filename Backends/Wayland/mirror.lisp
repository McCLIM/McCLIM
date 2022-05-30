(in-package #:clim-wayland)

;;; Mirror protocol?

(defclass wayland-mirror ()
  ((window :initarg :window :reader window))
  (:default-initargs :window (alx:required-argument :window)))

(defclass wayland-egl-mirror (wayland-mirror)
  ((egl-window :initform nil :reader wayland-egl-mirror-window)
   (egl-context :initform nil :reader wayland-egl-mirror-context)
   (egl-display :initform nil :reader wayland-egl-mirror-display)
   (egl-surface  :initform nil :reader wayland-egl-mirror-surface)))


(defmethod port-set-mirror-name
    ((port wayland-port) (sheet mirrored-sheet-mixin) name)
  (alx:when-let ((mirror (sheet-direct-mirror sheet)))
    ))

(defmethod realize-mirror ((port wayland-port) (sheet mirrored-sheet-mixin))
  )
