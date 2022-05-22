(in-package #:clim-wayland)

(defclass wayland-port (basic-port)
  ((display :initform nil
            :accessor wayland-port-display)
   (compositor :initform nil
               :accessor wayland-port-compositor)
   (surface :initform nil
            :accessor wayland-port-surface)
   (window :initform nil
           :accessor wayland-port-window)
   (registry :initform nil
             :accessor %wayland-registry)
   (wm-base :initform nil
            :accessor %wayland-wm-base)
   (top-level :initform nil
              :accessor %xdg-top-level)))

;;; wl-callbacks created from this class will call the callback fun
(defclass %wayland-invoking-callback (wlc:wl-callback)
  ((fun :type (function ((unsigned-byte 32)) *) :accessor fun :initarg :fun)))

(defmethod wlc:wl-callback-done ((self %wayland-invoking-callback) data)
  (funcall (fun self) data))

(defun roundtrip (display)
  "Wait for all previous requests to be processed by the wayland compositor"
  (let (callback done-p)
    (unwind-protect
         (flet ((set-done ()
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
      (when callback (wayland-destroy callback)))))

;;; Create a xdg_wm_base subclass which responds to pings
(defclass xdg-wm-base-pingpong (xdg:xdg-wm-base) ())

;;; Every time we receive a ping, send back a pong
(defmethod xdg-wm-base-ping ((self xdg-wm-base-pingpong) serial)
  (xdg-wm-base-pong self serial))


(defun bind-wayland-registry (port wl-protocol registry-string version)
  (let* ((registry (%wayland-registry port))
         (registry-match (wl-registry-find-or-lose registry
                                                   registry-string
                                                   version)))
    (wl-registry-bind registry registry-match wl-protocol)))

(defun initialize-wayland (port)
  (setf (wayland-port-display port) (wlc:wl-display-connect nil)

        (%wayland-registry port) (wlc:wl-display-get-registry
                                  (wayland-port-display port)
                                  (make-instance 'wlc:wl-registry)))

  (roundtrip (wayland-port-display port))

  (alx:when-let* ((registry (%wayland-registry port))
              (compositor (bind-wayland-registry
                           port
                           (make-instance 'wlc:wl-compositor :version 4)
                           "wl_compositor" 4)))

    (setf (wayland-port-compositor port) compositor

          (wayland-port-window port)
          (wlc:wl-compositor-create-surface
           compositor
           (make-instance 'wlc:wl-surface))

          (%wayland-wm-base port)
          (bind-wayland-registry
           port
           (make-instance 'xdg-wm-base-pingpong :version 1)
           "xdg_wm_base" 1)

          (wayland-port-surface port)
          (xdg:xdg-wm-base-get-xdg-surface
           (%wayland-wm-base port)
           (make-instance 'xdg:xdg-surface)
           (wayland-port-window port))

          (%xdg-top-level port)
          (xdg:xdg-surface-get-toplevel (wayland-port-surface port)
                                        (make-instance 'xdg:xdg-toplevel)))))

(defmethod initialize-instance :after ((port wayland-port) &key)
  (initialize-wayland port))

(defmethod destroy-port :before ((port wayland-port))
  (alx:when-let ((display (wayland-port-display port)))
    (wlc:wayland-destroy (%wayland-wm-base port))
    (wlc:wayland-destroy (wayland-port-window port))
    (wlc:wayland-destroy (wayland-port-compositor port))
    (wlc:wayland-destroy (wayland-port-registry port))

    (setf (wayland-port-display port) nil)
    (wlc:wl-display-disconnect display)))
