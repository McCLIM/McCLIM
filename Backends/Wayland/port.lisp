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
              :accessor %xdg-top-level)
   (seat :initform nil
         :accessor wayland-port-seat)))

(defun parse-server-path (server-path)
  (format *debug-io* "server path: ~a~%" server-path)
  (list :display-id (get-environment-variable "WAYLAND_DISPLAY")))

(defmethod find-port-type ((port (eql :wayland-ffi)))
  (values (find-class 'wayland-port) 'parse-server-path))

(defmethod find-port-type :after ((port (eql :wayland-ffi)))
  (format *debug-io* "find-port-type wayland-ffi: ~a~%" port))

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
      (format *debug-io* "binding: ~S ~S ~S ~S~%" port wl-protocol registry-string version)
      (wlc:wl-registry-bind registry registry-match wl-protocol))))


(defclass wayland-seat (wlc:wl-seat)
  ((capabilities :type list :accessor capabilities :initform nil)
   (name :type string :accessor name :initform "")
   (%pointer :initform nil :accessor %pointer)
   (%keyboard :initform nil :accessor %keyboard)
   ))

(defclass wayland-keyboard (wlc:wl-keyboard)
  ((xkb-context :initform (cffi:null-pointer) :accessor xkb-context)
   (xkb-keymap :initform (cffi:null-pointer) :accessor xkb-keymap)
   (xkb-state :initform (cffi:null-pointer) :accessor xkb-state)))

(defclass wayland-pointer (wlc:wl-pointer)
  (x y wl-surface timestamp))

(defmethod initialize-instance :after ((keyboard wayland-keyboard) &key)
  (with-slots (xkb-context) keyboard
    (setf xkb-context (xkb:xkb-context-new ()))))

(defmethod wlc:wl-seat-capabilities ((seat wayland-seat) capabilities)
  (setf (capabilities seat) capabilities)

  ;; init pointer if capability exists
  (let ((has-pointer-p (member :pointer capabilities)))
    (with-slots (%pointer) seat
      (cond ((and has-pointer-p (not %pointer))
             (setf %pointer
                   (wlc:wl-seat-get-pointer seat
                                            (make-instance 'wayland-pointer))))
            ;; release when capability lost
            ((and (not has-pointer-p) %pointer)
             (wlc:wl-pointer-release %pointer)
             (setf %pointer nil)))))

  ;; init keyboard if capability exists
  (let ((has-keyboard-p (member :keyboard capabilities)))
    (cond ((and has-keyboard-p
                (not (%keyboard seat)))
           (setf (%keyboard seat)
                 (wlc:wl-seat-get-keyboard seat
                                           (make-instance 'wayland-keyboard))))
          ((and (not has-keyboard-p) (%keyboard seat))
           (wlc:wl-keyboard-release (%keyboard seat))
           (setf (%keyboard seat) nil)))))

(defmethod wlc:wl-seat-name ((seat wayland-seat) name)
  (setf (name seat) name))

;;; Pointer Events

(defmethod wlc:wl-pointer-enter
    ((pointer wayland-pointer) serial surface surface-x surface-y)
  (with-slots (x y wl-surface) pointer
    (setf x surface-x
          y surface-y
          wl-surface surface))
  (distribute-event *wayland-port*
                    (make-instance 'pointer-enter-event
                                   :pointer (port-pointer *wayland-port*)
                                   :button nil
                                   :graft-x surface-x
                                   :graft-y surface-y
                                   :modifier-state nil
                                   :sheet (%hacky-top-level-sheet))))

(defmethod wlc:wl-pointer-enter :after
    ((pointer wayland-pointer) serial surface surface-x surface-y)
  (format *debug-io* "WL pointer ENTER (we should set cursor image)~%   ~s~%"
          (list surface-x surface-y serial surface)))

(defmethod wlc:wl-pointer-motion
    ((pointer wayland-pointer) time surface-x surface-y)
  (with-slots (x y timestamp) pointer
    (setf x surface-x
          y surface-y
          timestamp time))
  (distribute-event *wayland-port*
                    (make-instance 'pointer-motion-event
                                   :pointer (port-pointer *wayland-port*)
                                   :button nil
                                   :modifier-state nil
                                   :graft-x surface-x
                                   :graft-y surface-y
                                   ;; FIXME, region intersection to find sheet?
                                   :sheet (%hacky-top-level-sheet)
                                   :timestamp time))
  )

(defmethod wlc:wl-pointer-motion :after
    ((pointer wayland-pointer) time surface-x surface-y)
  (format *debug-io* "WL pointer MOTION ~s~%"
          (list time surface-x surface-y)))

(defmethod wlc:wl-pointer-button :after
    ((pointer wayland-pointer) serial time button state)
  (format *debug-io* "WL pointer BUTTON ~s~%"
          (list serial time button state)))

(defmethod wlc:wl-pointer-axis :after
    ((pointer wayland-pointer) time axis value)
  (format *debug-io* "WL pointer AXIS ~s~%"
          (list time axis value)))

(defmethod wlc:wl-pointer-frame ((pointer wayland-pointer))
  ;; atomic "commit" of pointer events. This is where we should distribute the
  ;; event
  )

(defmethod wlc:wl-pointer-frame :after ((pointer wayland-pointer))
  (format *debug-io* "WL pointer FRAME~%"))

(defmethod wlc:wl-pointer-axis-source :after
    ((pointer wayland-pointer) axis-source)
  (format *debug-io* "WL pointer AXIS-SOURCE ~s~%"
          (list axis-source)))

(defmethod wlc:wl-pointer-axis-stop :after
    ((pointer wayland-pointer) time axis)
  (format *debug-io* "WL pointer AXIS-STOP ~s~%"
          (list time axis)))

;;; Keyboard Events
(defmethod wlc:wl-keyboard-enter :after
    ((keyboard wlc:wl-keyboard) serial surface keys)
  (format *debug-io* "WL keyboard ENTER! ~s~%"
          (list keys serial surface)))

(defmethod wlc:wl-keyboard-leave :after
    ((keyboard wlc:wl-keyboard) serial surface)
  (format *debug-io* "WL keyboard LEAVE ~s~%"
          (list serial surface)))

(defmethod wlc:wl-keyboard-key
    ((keyboard wayland-keyboard) serial time key state)
  (declare (ignorable serial time)
           (fixnum key))
  (with-slots (xkb-state) keyboard
    (let* ((key-code (+ 8 key))         ; from wayland docs
           (keysym (xkb:xkb-state-key-get-one-sym xkb-state key-code))
           (key-name (clim-xcommon:keysym-to-keysym-name keysym))
           (key-utf8 (xkb:xkb-keysym-to-utf8 key-code))
           (key-character (and (characterp key-utf8)
                               key-utf8)))
      (distribute-event *wayland-port*
                        (make-instance (if (eq state :pressed)
                                           'key-press-event
                                           'key-release-event)
                                       :sheet (port-keyboard-input-focus *wayland-port*)
                                       :x 0 :y 0 ; ?? appear to be required
                                       :modifier-state nil
                                       :key-name key-name
                                       :key-character key-character
                                       :timestamp time))
      (format *debug-io* "MAPPED KEY EVENT ~s~%"
              (list key state key-code keysym key-name key-utf8 key-character)))))

(defmethod wlc:wl-keyboard-key :after
    ((keyboard wlc:wl-keyboard) serial time key state)
  (format *debug-io* "WL keyboard KEY! ~s~%"
          (list serial time key state)))

(defmethod wlc:wl-keyboard-keymap
    ((keyboard wayland-keyboard) format fd size)
  (unwind-protect
       (with-slots (xkb-keymap xkb-state xkb-context) keyboard
         ;; FIXME: SBCL specific
         (let* ((mem
                  (sb-posix:mmap
                   nil size sb-posix:prot-read sb-posix:map-private fd 0))
                (keymap
                  (xkb:xkb-keymap-new-from-string xkb-context mem :text-v1 nil))
                (state (xkb:xkb-state-new keymap)))
           (xkb:xkb-keymap-unref xkb-keymap)
           (xkb:xkb-state-unref xkb-state)

           (setf xkb-keymap keymap)
           (setf xkb-state state)))
    (sb-posix:close fd)))

(defmethod wlc:wl-keyboard-keymap :after
    ((keyboard wlc:wl-keyboard) format fd size)
  (format *debug-io* "WL Keymap ready! ~s~%"
          (list format fd size)))

;;; Port protocols

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
                     (port-seat wayland-port-seat)
                     (port-window wayland-port-window)
                     (port-wm-base %wayland-wm-base)
                     (port-surface wayland-port-surface))
        port

      (setf port-compositor compositor

            port-device device

            port-seat (bind-wayland-registry
                         port
                         (make-instance 'wayland-seat :version 7)
                         "wl_seat" 7)

            port-wm-base (bind-wayland-registry
                          port
                          (make-instance 'xdg-wm-base-pingpong :version 1)
                          "xdg_wm_base" 1)))

    ;; Make one more round trip to handle events triggered by bindings above
    ;; before the main event loop is started
    (roundtrip port)))

(defmethod initialize-instance :after ((port wayland-port) &key)
  (with-slots (frame-managers pointer) port
    (push (apply #'make-instance 'standard-frame-manager :port port nil)
          frame-managers)
    (setf pointer (make-instance 'standard-pointer :port port)))
  (initialize-wayland port)
  (make-graft port)
  (format *debug-io* "Starting Event Loop~%")
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
  (format *debug-io* "PORT-FORCE-OUTPUT~%")
  ;; XXX temporary double buffering hack. It seems the correct place to flip
  ;; buffers is in medium-f*-output but there seem to be some elements that
  ;; are causing the buffer to flip before the entire "scene" has been output

  ;; (map-over-grafts (lambda (graft)
  ;;                    (swap-buffers (sheet-direct-mirror graft))
  ;;                    (clear-buffered-drawable (sheet-direct-mirror graft)))
  ;;                  port)
  )

;; (defmethod port-set-mirror-geometry
;;     ((port wayland-port) (sheet mirrored-sheet-mixin) region)
;;   (alx:when-let ((mirror (sheet-direct-mirror sheet)))
;;     (with-bounding-rectangle* (x1 y1 x2 y2 :width w :height h) region
;;       (format *debug-io* "port-set-mirror-geometry ~s~%" (list x1 y1 w h))
;;       ;; TODO: determine if we need CLX's ROUND-COORDINATE -- yes, wayland
;;       ;; only handles integers but CLIM allows the full Lisp rational types
;;       (xdg:xdg-surface-set-window-geometry (slot-value port 'surface) x1 y1 w h)
;;       (wlc:wl-surface-commit (wayland-port-window port))
;;       (wl-egl:wl-egl-window-resize
;;        (wayland-egl-mirror-window mirror) w h x1 y1)

;;       (values x1 y1 x2 y2))))

(defmethod port-set-mirror-geometry
    ((port wayland-port) (sheet mirrored-sheet-mixin) region)
  (alx:when-let ((mirror (sheet-direct-mirror sheet)))
    (with-bounding-rectangle* (x1 y1 x2 y2 :width w :height h) region
      (with-bounding-rectangle* (ox1 oy1 ox2 oy2) (sheet-mirror-geometry sheet)
        (when (some #'/=
                    (list x1 y1 x2 y2)
                    (list ox1 oy1 ox2 oy2))
          (format *debug-io* "port-set-mirror-geometry ~s~%" (list x1 y1 w h))
          ;; TODO: determine if we need CLX's ROUND-COORDINATE -- yes, wayland
          ;; only handles integers but CLIM allows the full Lisp rational types
          (xdg:xdg-surface-set-window-geometry (slot-value port 'surface) x1 y1 w h)
          (wlc:wl-surface-commit (wayland-port-window port))
          (wl-egl:wl-egl-window-resize
           (wayland-egl-mirror-window mirror) w h x1 y1)
          ))
      (values x1 y1 x2 y2))))

(defun clear-buffered-drawable (mirrored-sheet)
  (format *debug-io* "clearing back buffer~%")
  (gl:clear :color-buffer-bit :depth-buffer-bit :stencil-buffer-bit)
  (gl:clear-color 1.0 0.0 (/ 204 255) 1.0)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (with-bounding-rectangle* (x1 y1 :width w :height h)
      (graft mirrored-sheet)
    ;; Getting the right bounding rectangle is what I need next. When it is
    ;; hard-coded to the graft size, it rendered more correct than ever before.
    ;; 2022-09-05
    (format *debug-io* "buffer device region ~S~%" (list ':x x1 ':y y1 ':w w ':h h))
    (gl:ortho 0.0 w 0.0 h -1.0 1.0))
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defun swap-buffers (medium)
  (alx:when-let ((mirror (medium-drawable medium)))
    (egl:swap-buffers (wayland-egl-mirror-display mirror)
                      (wayland-egl-mirror-surface mirror))))

(defmethod port-enable-sheet ((port wayland-port) (sheet mirrored-sheet-mixin))
  (format *debug-io* "port enable-sheet ~a ~a ~%" port sheet)
  (alx:when-let ((mirror (sheet-mirror sheet)))
   (with-slots (egl-window egl-context egl-display egl-surface)
       mirror
     (with-accessors ((port-window wayland-port-window))
         port
       (egl:make-current egl-display egl-surface egl-surface egl-context)
       ;; todo there's got to be a better place in McCLIMs lifecycle for this:
      ;; (gl:viewport 0 0 (bounding-rectangle-width sheet) (bounding-rectangle-height sheet))
       (clear-buffered-drawable sheet)
       ;; (wlc:wl-surface-commit port-window)
       ))))

(defmethod port-disable-sheet ((port wayland-port) (sheet mirrored-sheet-mixin))
  (format *debug-io* "port disable-sheet ~a ~a ~%" port sheet)
  (alx:when-let ((mirror (sheet-direct-mirror sheet)))
    (wlc:wl-surface-attach (wayland-port-window port) nil 0 0)
    (wlc:wl-surface-commit (wayland-port-window port))))

(defmethod graft ((port wayland-port))
  (first (port-grafts port)))

;;; Grafts

(defclass wayland-graft (sheet-y-inverting-transformation-mixin graft)
  ())

;; (defmethod distribute-event :before
;;     ((port wayland-port) (event window-configuration-event))
;;   (alx:when-let ((mirror (sheet-mirror (graft port))))
;;     (with-bounding-rectangle* (x1 y1 x2 y2 :width w :height h)
;;         (window-event-region event)
;;       (format *debug-io* "before config-event ~s~%" (list x1 y1 x2 y2 w h))
;;       (wl-egl:wl-egl-window-resize
;;        (wayland-egl-mirror-window mirror) w h x1 y1))))

;; From double buffer branch
;; (defmethod handle-event
;;     ((sheet top-level-sheet-mixin) (event window-configuration-event))
;;   (format *debug-io* "Other top-level window-cfg-event")
;;   (let ((*configuration-event-p* sheet))
;;     (resize-sheet sheet
;;                   (window-configuration-event-native-width event)
;;                   (window-configuration-event-native-height event))))

(defmethod handle-event
    ((sheet top-level-sheet-pane) (event window-configuration-event))
  ;; Is this where I should set the egl-window width and swap buffers?
  ;; Should this be handled by the medium? or te mirror?
  (flet ((should-resize-p (new-dimensions original-dimensions)
           (some #'/= new-dimensions original-dimensions)))
    (alx:when-let ((mirror (sheet-mirror sheet)))
      (format *debug-io* "handle-event window-cfg-event: top level reconfigure? Should I resize all the children?~%")
      ;; QQQQ Should I use port-set-mirror-geometry or is there some other way
      ;; to trigger it?
      ;; (break)
      (unless (eql +everywhere+ (window-event-native-region event))
        (with-bounding-rectangle* (x1 y1 x2 y2 :width w :height h)
            (window-event-native-region event)
          (let ((*configuration-event-p* sheet))
            (resize-sheet sheet w h))
          (progn
            ;; FIXME: Why is resize-sheet above not calling
            ;; port-set-mirror-geometry? This would be more desired. For now
            ;; we resize the surface to aid visual debugging
            (wl-egl:wl-egl-window-resize
             (wayland-egl-mirror-window mirror) w h x1 y1)
            ;; This xdg request will cause another window configuration event.
            (xdg:xdg-surface-set-window-geometry
             (slot-value (port sheet) 'surface) x1 y1 w h))
          ))
      )))

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

;; The standard behavior is to check for and use a the %drawable slot on the
;; medium otherwise it will fallback to the mirror. I think this is what we
;; want already. But is it more appropriate to store the opengl context on the
;; medium?
;; (defmethod medium-drawable ((medium wayland-egl-medium)))


(defmethod invoke-with-output-to-drawing-stream
    (continuation backend destination &key)
  ;; the "wrapper" for opengl / etc rendering it looks like...
  ;; for windowing systems, is destination the sheet-medium here?
  ;; sheet-mirror?
  (break backend destination)
  )

(defmethod medium-buffering-output-p ((medium wayland-egl-medium))
  (alx:if-let ((drawable (medium-drawable medium)))
    (buffering-p drawable)
    (call-next-method)))

(defmethod (setf medium-buffering-output-p) (new-value (medium wayland-egl-medium))
  (alx:if-let ((drawable (medium-drawable medium)))
    (setf (buffering-p drawable) new-value)
    (call-next-method)))

;; (defmethod invoke-with-output-recording-options :before
;;     (stream continuation record draw)
;;   (format *debug-io* "ok getting somewhere~%")
;;   (when draw
;;     ;;     (gl:clear :color-buffer-bit :depth-buffer-bit :stencil-buffer-bit)
;;     ;; (gl:load-identity)
;;     ;; (gl:ortho 0.0
;;     ;;           (bounding-rectangle-width mirror)
;;     ;;           0.0
;;     ;;           (bounding-rectangle-height mirror)
;;     ;;           -1.0
;;     ;;           1.0))

;;   ))

(defmethod medium-finish-output ((medium wayland-egl-medium))
  (format *debug-io* "medium-finish-output buffering? ~a~%"
          (medium-buffering-output-p medium))
  )

(defmethod medium-force-output ((medium wayland-egl-medium))
  (format *debug-io* "medium-FORCE-output buffering? ~a~%"
          (medium-buffering-output-p medium))
  ;; Anything more todo here? force wayland output?
  ;; Better place to start a frame?
  (unless (medium-buffering-output-p medium)
    (alx:when-let ((mirror (medium-drawable medium)))
      (swap-buffers medium)
      (clear-buffered-drawable (medium-sheet medium)))) )

;; (defmethod medium-clear-area :around
;;     ((medium wayland-egl-medium) left top right bottom)
;;   (gl:with-pushed-matrix
;;     (call-next-method medium left top right bottom)))

;; (defmethod medium-clear-area
;;     ((medium wayland-egl-medium) left top right bottom)
;;   (multiple-value-bind (r g b a)
;;       (clime:color-rgba (medium-background medium))
;;     (gl:with-pushed-attrib (:scissor-bit)
;;       (gl:scissor left top right bottom)
;;       (gl:clear-color r g b a)
;;       ;; (gl:ortho left right bottom top -1.0 1.0)
;;       (gl:clear :color-buffer-bit))))

(defmethod medium-clear-area :before
    ((medium wayland-egl-medium) left top right bottom)
  (format *debug-io* "calling EGL medium clear area ~s~%" (list left top right bottom)))

(defmethod medium-clear-area :after
    ((medium wayland-egl-medium) left top right bottom)
  (format *debug-io* "EGL medium clear area called ~s~%" (list left top right bottom)))

;; (defmethod medium-draw-polygon* :around
;;     ((medium wayland-egl-medium) coord-seq closed filled)
;;   (gl:with-pushed-matrix
;;     (call-next-method medium coord-seq closed filled)))

(defmethod medium-draw-polygon*
    ((medium wayland-egl-medium) coord-seq closed filled)
  (alx:when-let ((drawable (medium-drawable medium)))
    (multiple-value-bind (r g b a)
        (clime:color-rgba (medium-ink medium))
      (gl:color r g b a)
      (gl:polygon-mode :front-and-back (if filled :fill :line))
      (gl:with-pushed-attrib (:enable-bit)
        (gl:enable :polygon-offset-fill)
        ;; gl:with-pushed-matrix
        (gl:with-primitive (if closed :polygon :line-strip)
          (loop with transform = (medium-device-transformation medium)
                for (x y) on coord-seq by #'cddr do
                  (multiple-value-bind (tx ty)
                      (transform-position transform x y)
                    ;;   (break "insided draw-polygon ~S" (list transform tx ty))
                    (format *debug-io* "poly* xy:~a txy:~a ink:~s~%" (list x y) (list tx ty) (list r g b a))
                    ;; reverted to x y until transform-position error understood
                    (gl:vertex tx ty))))))))

(defmethod text-style-mapping
    ((port wayland-port) (text-style text-style) &optional character-set)
  (declare (ignore port text-style character-set))
  nil)

(defmethod (setf text-style-mapping) (font-name
                                      (port wayland-port)
                                      (text-style text-style)
                                      &optional character-set)
  (declare (ignore font-name text-style character-set))
  nil)


(defmethod text-style-ascent (text-style (medium wayland-egl-medium))
  (declare (ignore text-style))
  1)

(defmethod text-style-descent (text-style (medium wayland-egl-medium))
  (declare (ignore text-style))
  1)

(defmethod text-style-height (text-style (medium wayland-egl-medium))
  (+ (text-style-ascent text-style medium)
     (text-style-descent text-style medium)))

(defmethod text-style-character-width (text-style (medium wayland-egl-medium) char)
  (declare (ignore text-style char))
  1)

(defmethod text-style-width (text-style (medium wayland-egl-medium))
  (text-style-character-width text-style medium #\m))

(defmethod text-size
    ((medium wayland-egl-medium) string &key text-style (start 0) end)
  (setf string (etypecase string
                 (character (string string))
                 (string string)))
  (let ((width 0)
        (height (text-style-height text-style medium))
        (x (- (or end (length string)) start))
        (y 0)
        (baseline (text-style-ascent text-style medium)))
    (do ((pos (position #\Newline string :start start :end end)
              (position #\Newline string :start (1+ pos) :end end)))
        ((null pos) (values width height x y baseline))
      (let ((start start)
            (end pos))
        (setf x (- end start))
        (setf y (+ y (text-style-height text-style medium)))
        (setf width (max width x))
        (setf height (+ height (text-style-height text-style medium)))
        (setf baseline (+ baseline (text-style-height text-style medium)))))))

(defmethod climb:text-bounding-rectangle*
    ((medium wayland-egl-medium) string &key text-style (start 0) end align-x align-y direction)
  (declare (ignore align-x align-y direction)) ; implement me!
  (multiple-value-bind (width height x y baseline)
      (text-size medium string :text-style text-style :start start :end end)
    (declare (ignore baseline))
    (values x y (+ x width) (+ y height))))

(defmethod medium-draw-text* ((medium wayland-egl-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (ignore string x y start end align-x align-y toward-x toward-y transform-glyphs))
  nil)
