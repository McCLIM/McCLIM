(in-package #:mcclim-sdl2)

(defmethod realize-mirror ((port sdl2-port) (sheet unmanaged-sheet-mixin))
  (with-bounding-rectangle* (x y :width w :height h) sheet
    (let* ((title "(McCLIM)")
           (flags '(:borderless))
           (mirror (sdl2-create-mirror
                    sheet title x y w h flags :synchronize t)))
      (register-resource port mirror)
      mirror)))

(defmethod realize-mirror ((port sdl2-port) (sheet top-level-sheet-mixin))
  (with-bounding-rectangle* (x y :width w :height h) sheet
    (let* ((title (sheet-pretty-name sheet))
           (flags '(:shown :resizable))
           (mirror (sdl2-create-mirror
                    sheet title x y w h flags :synchronize t)))
      (register-resource port mirror)
      (alx:when-let ((icon (sheet-icon sheet)))
        (let ((resource (ensure-resource (port (alx:ensure-car icon))
                          (sdl2-create-rgb-surface-from-image
                           ^clim-object :synchronize t))))
          (change-window-icon mirror resource)))
      mirror)))

#+ (or) ;; SDL2 port does not implement mirrored sub-windows.
(defmethod realize-mirror ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (error "ENOTIMLEMENTED"))

(defmethod destroy-mirror ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (sheet-direct-mirror sheet)))
    (destroy-window mirror :synchronize t)
    (deregister-resource port mirror)))

(defmethod port-set-mirror-geometry
    ((port sdl2-port) (sheet mirrored-sheet-mixin) region)
  (with-bounding-rectangle* (x1 y1 x2 y2 :width w :height h) region
    (change-window-size (sheet-direct-mirror sheet) x1 y1 w h)
    (values x1 y1 x2 y2)))

(defmethod port-enable-sheet ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (show-window (sheet-direct-mirror sheet)))

(defmethod port-disable-sheet ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (hide-window (sheet-direct-mirror sheet)))

;;; The following functions are specific to top-level sheets.

(defmethod port-set-mirror-name
    ((port sdl2-port) (sheet top-level-sheet-mixin) name)
  (change-window-title (sheet-direct-mirror sheet) name))

(defmethod port-set-mirror-icon
    ((port sdl2-port) (sheet top-level-sheet-mixin) icon)
  (alx:when-let ((window (sheet-direct-mirror sheet)))
    (change-window-icon (sheet-direct-mirror sheet)
                        (ensure-resource (port (alx:ensure-car icon))
                          (sdl2-create-rgb-surface-from-image ^clim-object)))))

(defmethod port-shrink-sheet ((port sdl2-port) (sheet top-level-sheet-mixin))
  (minimize-window (sheet-direct-mirror sheet)))

(defmethod port-unshrink-sheet ((port sdl2-port) (sheet top-level-sheet-mixin))
  (restore-window (sheet-direct-mirror sheet)))

(defmethod raise-mirror ((port sdl2-port) (sheet top-level-sheet-mixin))
  (raise-window (sheet-direct-mirror sheet)))

(defmethod bury-mirror ((port sdl2-port) (sheet top-level-sheet-mixin))
  (log:warn "Unsupported operation."))


;;; Requests
(define-sdl2-request create-window (title x y width height flags)
  (let* ((flags (autowrap:mask-apply 'sdl-window-flags flags))
         (window (sdl2-ffi.functions:sdl-create-window
                  title x y width height flags)))
    (sdl2-ffi.functions:sdl-get-window-id window)))

(define-sdl2-request destroy-window (mirror)
  (free-sdl2-resource mirror)
  nil)

(define-sdl2-request change-window-title (mirror title)
  (let ((window (sdl2-mirror-window mirror)))
    (sdl2-ffi.functions:sdl-set-window-title window title)))

(define-sdl2-request change-window-icon (mirror object)
  (let ((window (sdl2-mirror-window mirror)))
    (sdl2-ffi.functions:sdl-set-window-icon window (sdl2-image-surface object))))

(define-sdl2-request change-window-size (mirror x y w h)
  (let ((window (sdl2-mirror-window mirror)))
    (sdl2-ffi.functions:sdl-set-window-position window x y)
    (sdl2-ffi.functions:sdl-set-window-size window w h)))

(define-sdl2-request hide-window (mirror)
  (let ((window (sdl2-mirror-window mirror)))
    (sdl2-ffi.functions:sdl-hide-window window)))

(define-sdl2-request show-window (mirror)
  (let ((window (sdl2-mirror-window mirror)))
    (sdl2-ffi.functions:sdl-show-window window)))

(define-sdl2-request minimize-window (mirror)
  (let ((window (sdl2-mirror-window mirror)))
    (sdl2-ffi.functions:sdl-minimize-window window)))

(define-sdl2-request maximize-window (mirror)
  (let ((window (sdl2-mirror-window mirror)))
    (sdl2-ffi.functions:sdl-maximize-window window)))

(define-sdl2-request restore-window (mirror)
  (let ((window (sdl2-mirror-window mirror)))
    (sdl2-ffi.functions:sdl-restore-window window)))

;;; Specified by SDL2 but doesn't seem to work on X11.
(define-sdl2-request raise-window (mirror)
  (let ((window (sdl2-mirror-window mirror)))
    (sdl2-ffi.functions:sdl-raise-window window)))

#+ (or) ;; Not specified by SDL2.
(define-sdl2-request bury-window (mirror)
  (let ((window (sdl2-mirror-window mirror)))
    (sdl2-ffi.functions:sdl-bury-window window)))


;;; Window SDL2 event handlers.

;;; Between pressing quit and the actual close the user may still use the
;;; window for a brief period, so i.e a window event may sneak in. The window
;;; event handler should ignore events to windows that are already destroyed.

(define-sdl2-handler (ev :windowevent) (event window-id timestamp data1 data2)
  (alx:when-let ((mirror (find-sdl2-resource *sdl2-port* window-id)))
    (let ((sheet (sdl2-mirror-sheet mirror))
          (event-key (autowrap:enum-key '(:enum (windowevent.event)) event)))
      (handle-sdl2-window-event event-key sheet timestamp data1 data2))))

(defgeneric handle-sdl2-window-event (event-key sheet timestamp data1 data2)
  (:method (event-key sheet timestamp data1 data2)
    (log:info "Unhandled window event ~s." event-key)))

;;; Close and redraw.

(defmethod handle-sdl2-window-event ((key (eql :close)) sheet stamp d1 d2)
  (make-instance 'window-manager-delete-event :sheet sheet :timestamp stamp))

(defmethod handle-sdl2-window-event ((key (eql :exposed)) sheet stamp d1 d2)
  (make-instance 'window-repaint-event :sheet sheet :region +everywhere+))

;;; FIXME we access the internal interface sheet-mirror-geometry - we should
;;; either query the window position with sdl2 interfaces, export the
;;; interface, or move updating the mirror geometry to the backend. Because it
;;; is not an obvious decision I'm leaving this until we resolve the issue.

(defmethod handle-sdl2-window-event ((key (eql :moved)) sheet stamp d1 d2)
  (log:info "Window position changed to ~s ~s" d1 d2)
  (with-bounding-rectangle* (:width w :height h)
      (climi::sheet-mirror-geometry sheet)
    (make-instance 'window-configuration-event
                   :sheet sheet
                   :region (make-bounding-rectangle d1 d2 (+ d1 w) (+ d2 h)))))

(defmethod handle-sdl2-window-event ((key (eql :size-changed)) sheet stamp d1 d2)
  (log:info "Window size changed to ~s ~s" d1 d2)
  (with-bounding-rectangle* (x y) (climi::sheet-mirror-geometry sheet)
    (make-instance 'window-configuration-event
                   :sheet sheet
                   :region (make-bounding-rectangle x y (+ x d1) (+ y d2)))))

;;; Keyboard focus

(defmethod handle-sdl2-window-event ((key (eql :focus-gained)) sheet stamp d1 d2)
  (make-instance 'window-manager-focus-event :sheet sheet))

(defmethod handle-sdl2-window-event ((key (eql :focus-lost)) sheet stamp d1 d2)
  (log:debug "Window keyboard focus lost (ignoring)"))

(defmethod handle-sdl2-window-event ((key (eql :take-focus)) sheet stamp d1 d2)
  (log:debug "Window keyboard take focus opportunity (ignoring)"))

;;; Pointer focus

;;; XXX SDL2 specifies both enter and leave events as window events while
;;; McCLIM treats them as pointer events. To generate an appropriate boundary
;;; event we need to query the pointer position.

(defmethod handle-sdl2-window-event ((key (eql :enter)) sheet stamp d1 d2)
  (multiple-value-bind (sheet-x sheet-y)
      (climi::sheet-pointer-position sheet (port-pointer *sdl2-port*))
    (make-instance 'pointer-enter-event
                   :sheet sheet
                   :pointer (port-pointer *sdl2-port*)
                   :x sheet-x :y sheet-y)))

(defmethod handle-sdl2-window-event ((key (eql :leave)) sheet stamp d1 d2)
  (multiple-value-bind (sheet-x sheet-y)
      (climi::sheet-pointer-position sheet (port-pointer *sdl2-port*))
    (make-instance 'pointer-exit-event
                   :sheet sheet
                   :pointer (port-pointer *sdl2-port*)
                   :x sheet-x :y sheet-y)))
