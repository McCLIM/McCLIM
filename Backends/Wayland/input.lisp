(in-package #:mcclim-wayland)

(defvar *wayland-port*)
(defvar *wayland-wait-func*)

(defmethod process-next-event ((port wayland-port) &key wait-function (timeout nil))
  (let ((*wayland-port* port)
        (*wayland-wait-func* wait-function))
    (let ((dispatched-events-count (wlc:wl-display-dispatch
                                    (wayland-port-display *wayland-port*))))
      (format *debug-io* "process-next-event status? ~a~%" dispatched-events-count)
      ;; Return t for now since we still utilize the wlc event loop and its
      ;; use of CLOS for eventing. I'm not sure if there is an analog for
      ;; timeout or wait functions.

      (values t dispatched-events-count))))

;;; Now we connect all of wayland-client events to our port's events
(defun %hacky-top-level-sheet ()
  (alx:when-let ((frame (first (frame-manager-frames
                                (find-frame-manager :port *wayland-port*)))))
    (frame-top-level-sheet frame)))

(defmethod xdg:xdg-surface-configure ((surface xdg:xdg-surface) serial)
  ;; We assume we handle configuration events immediately, so we can
  ;; acknowledge right away. TODO: find where in the CLIM flow this should
  ;; really happen. This ACK is very important for the PROCESS-NEXT-EVENT to
  ;; continue even though it might be in the wrong place
  (xdg:xdg-surface-ack-configure surface serial))

(defmethod xdg-surface-configure :after ((surface xdg:xdg-surface) serial)
  (format *debug-io* "xdg surface configured ~a  surface: ~a~%" serial surface))

(defmethod xdg:xdg-toplevel-configure
    ((wayland-proxy wayland-xdg-toplevel) width height states)
  (declare (ignore wayland-proxy))
  (format *debug-io*
          "toplevel configure event: w: ~a  h: ~a  states: ~s frame?: ~s ~%"
          width height states
          (frame-manager-frames (find-frame-manager :port *wayland-port*)))
  (flet ((state-has-keyword-p (keyword)
           (eql keyword
                (rest (xdg:xdg-toplevel-unmarshal-state (first states))))))
   (when (and (every #'plusp (list width height))
              (not (state-has-keyword-p :activated)))
     (format *debug-io* "distributing toplevel configure~%")
     (alx:when-let* ((top-level-sheet (%hacky-top-level-sheet)))
       (distribute-event *wayland-port*
                         (make-instance 'window-configuration-event
                                        :sheet top-level-sheet
                                        :region (make-bounding-rectangle 0 0 width height)))))))

(defmethod xdg:xdg-toplevel-close ((wayland-proxy wayland-xdg-toplevel))
  (declare (ignore wayland-proxy))
  (format *debug-io* "close toplevel event ~s~%" *application-frame*)
  ;; QQQQ this feels terrible but it seems *application-frame* isn't
  ;; bound. Extremely hacky way to get top-level-frame-sheet
  (alx:when-let ((sheet (%hacky-top-level-sheet)))
    ;; (break)
    (distribute-event *wayland-port*
                      (make-instance 'window-manager-delete-event
                                     :sheet sheet))))

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
  (with-accessors ((width screen-width)
                   (height screen-height))
      screen
    (alx:when-let* ((sheet (graft *wayland-port*))
                    (clim-event (make-instance 'window-configuration-event
                                               :sheet sheet
                                               :region (sheet-native-region sheet))))
      (format *debug-io* "ALL OUTPUT EVENTS FINISHED ~S~%" screen)

      clim-event)))
