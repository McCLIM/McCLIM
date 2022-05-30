(in-package #:clim-wayland)

(defvar *wayland-port*)
(defvar *wayland-wait-func*)

(defmethod process-next-event ((port wayland-port) &key wait-function (timeout nil))
  (let ((*wayland-port* port)
        (*wayland-wait-func* wait-function))
    (let ((event-status (wlc:wl-display-dispatch
                         (wayland-port-display *wayland-port*))))
      ;; Return t for now since I seem beholden to the wlc event loop and I'm
      ;; not sure when I should return nil
      t
      )))

;;; Now we connect all of wayland-client events to our port's events

(defmethod xdg:xdg-toplevel-configure :after (wayland-proxy width height states)
  (declare (ignore states wayland-proxy))
  (format t "toplevel configure event: w: ~a  h: ~a~%" width height)
  ;; (wl-egl:wl-egl-window-resize *egl-window* width height 0 0)
  ;; (wl-surface-commit *window*)
  (distribute-event *wayland-port*
                    (make-instance 'window-configuration-event
                                   :width width
                                   :height height
                                   ; :region?
                                   )))

(defmethod xdg:xdg-toplevel-close :after (wayland-proxy)
  (declare (ignore wayland-proxy))
  (format t "close toplevel event")
  (dispatch-event *wayland-port* (make-instance 'window-destroy-event
                                                ;; :region?
                                                )))

(defclass wayland-port-screen (wlc:wl-output)
  ((x :initform 0 :accessor screen-x)
   (y :initform 0 :accessor screen-y)
   (height :initform 0 :accessor screen-height)
   (width :initform 0 :accessor screen-width)
   (physical-height :initform 0 :accessor screen-physical-height)
   (physical-width :initform 0 :accessor screen-physical-width)
   (refresh-rate :initform nil :accessor screen-refresh-rate)
   (scale :initform 1 :accessor screen-scale)))

(defmethod wlc:wl-output-geometry
    ((screen wayland-port-screen) x y physical-width physical-height
     subpixel make model transform)
  (declare (ignorable subpixel make model transform))
  ;; this is essentially an event listener for the wayland event... should it
  ;; have it's own state as the examples use? Or fire a McCLIM event?
  (with-accessors ((sx screen-x)
                   (sy screen-y)
                   (pw screen-physical-width)
                   (ph screen-physical-height))
      screen
    (setf sx x
          sy y
          pw physical-width
          ph physical-height)))

(defmethod wlc:wl-output-mode ((screen wayland-port-screen) flags width height
                               refresh)
  (with-accessors ((sw screen-width)
                   (sh screen-height)
                   (rr screen-refresh-rate))
      screen
    (setf sw width
          sh height
          rr refresh)))

(defmethod wlc:wl-output-scale ((screen wayland-port-screen) scale-factor)
  (setf (screen-scale screen) scale-factor))

(defmethod wlc:wl-output-done ((screen wayland-port-screen))
  (format t "ALL OUTPUT EVENTS FINISHED ~S~%" screen))
