(in-package #:mcclim-sdl2)

(defclass sdl2-pointer (standard-pointer) ())

(defmethod port-pointer ((port sdl2-port))
  (or (call-next-method)
      (setf (port-pointer port)
            (make-instance 'sdl2-pointer :port port))))

;;; The specification requires us to return only a single button so we pick a
;;; canonical order after the clx backend: R, M, L, W-UP, W-DN.
(defun sdl2-button-to-clim-button (button-state)
  (cond ((logtest (expt 2 2) button-state) +pointer-right-button+)
        ((logtest (expt 2 1) button-state) +pointer-middle-button+)
        ((logtest (expt 2 0) button-state) +pointer-left-button+)
        ((logtest (expt 2 3) button-state) +pointer-wheel-up+)
        ((logtest (expt 2 4) button-state) +pointer-wheel-down+)
        ((zerop button-state)       climi::+pointer-no-button+)))

;;; https://wiki.libsdl.org/SDL2/SDL_GetGlobalMouseState
;;;
;;; SDL2 does not distinguish between different pointers and treats them all
;;; as a "mouse". That's fine for a desktop, but there is a problem that it
;;; does not pick some events from pointers other than a primary one until a
;;; button is pressed, because it queries only the primary pointer.
(define-sdl2-request sdl2-get-global-mouse-state ()
  (plus-c:c-with ((x :int) (y :int))
    (let ((buttons (sdl2-ffi.functions:sdl-get-global-mouse-state
                    (x plus-c:&)
                    (y plus-c:&))))
      (list x y (sdl2-button-to-clim-button buttons)))))

(defun port-pointer-state (port pointer)
  (declare (ignore port pointer))
  (apply #'values (sdl2-get-global-mouse-state :synchronize t)))

(defmethod pointer-button-state ((pointer sdl2-pointer))
  (multiple-value-bind (x y button-state)
      (port-pointer-state (port pointer) pointer)
    (declare (ignore x y))
    button-state))

(defmethod pointer-position ((pointer sdl2-pointer))
  (multiple-value-bind (x y button-state)
      (port-pointer-state (port pointer) pointer)
    (declare (ignore button-state))
    (values x y)))

;;; https://wiki.libsdl.org/SDL2/SDL_WarpMouseGlobal
(define-sdl2-request sdl2-warp-mouse-global (x y)
  (sdl2-ffi.functions:sdl-warp-mouse-global x y)
  nil)

(clim-sys:defmethod* (setf pointer-position)
    (x y (pointer sdl2-pointer))
  (sdl2-warp-mouse-global x y :synchronize t))


;;; Global grab
(define-sdl2-request sdl2-capture-mouse (enabled)
  (zerop (sdl2-ffi.functions:sdl-capture-mouse enabled)))

(define-sdl2-request sdl2-set-window-grab (mirror enabled)
  (let ((window (sdl2-mirror-window mirror)))
    (sdl2-ffi.functions:sdl-set-window-grab window enabled)))

(define-sdl2-request sdl2-get-window-grab (mirror)
  (let ((window (sdl2-mirror-window mirror)))
    (sdl2-ffi.functions:sdl-get-window-grab window)))

(define-sdl2-request sdl2-get-grabbed-window ()
  (alx:when-let ((window (sdl2-ffi.functions:sdl-get-grabbed-window)))
    (find-sdl2-resource *sdl2-port*
                        (sdl2-ffi.functions:sdl-get-window-id  window))))

(defmethod port-grab-pointer
    ((port sdl2-port) (pointer sdl2-pointer) sheet &key multiple-window)
  (if multiple-window
      (sdl2-capture-mouse t :synchronize t)
      (sdl2-set-window-grab (sheet-direct-mirror sheet) t :synchronize t)))

(defmethod port-ungrab-pointer ((port sdl2-port) (pointer sdl2-pointer) sheet)
  (declare (ignore port pointer))
  (let* ((mirror (sheet-direct-mirror sheet))
         (multiple-window (not (sdl2-get-window-grab mirror :synchronize t))))
    (if multiple-window
        (sdl2-capture-mouse nil)
        (sdl2-set-window-grab mirror nil))))


;;; The cursor is set for all SDL2 windows.
(define-sdl2-request sdl2-set-cursor (cursor)
  (sdl2-ffi.functions:sdl-set-cursor (sdl2-cursor-ptr cursor)))

;;; Returns an object of type SDL2-FFI:SDL-CURSOR. It is not "our" rsource.
(define-sdl2-request sdl2-get-cursor ()
  (sdl2-ffi.functions:sdl-get-cursor))

(define-sdl2-request sdl2-set-pointer-cursor (design)
  (sdl2-set-cursor
   (ensure-resource (*sdl2-port* (list :cursor design))
     (etypecase design
       (symbol (sdl2-create-system-cursor design :synchronize t))
       (design (sdl2-create-color-cursor design :synchronize t))))))

(defmethod (setf pointer-cursor) ((design image-pattern) (pointer sdl2-pointer))
  (sdl2-set-pointer-cursor design :synchronize t))

(defmethod (setf pointer-cursor) ((object symbol) (pointer sdl2-pointer))
  (sdl2-set-pointer-cursor object :synchronize t))


;;; Sheet pointer cursor:

(define-sdl2-request sdl2-get-mouse-focus ()
  (let* ((win (sdl2-ffi.functions:sdl-get-mouse-focus))
         (wid (sdl2-ffi.functions:sdl-get-window-id win)))
    (find-sdl2-resource *sdl2-port* wid)))

(defmethod set-sheet-pointer-cursor
    ((port sdl2-port) (sheet mirrored-sheet-mixin) cursor)
  (alx:when-let ((mirror (sdl2-get-mouse-focus :synchronize t)))
    (when (eq mirror (sheet-direct-mirror sheet))
      (setf (pointer-cursor (port-pointer (port sheet))) cursor))))

(defmethod distribute-event :before ((port sdl2-port) (event pointer-enter-event))
  (setf (pointer-cursor (pointer-event-pointer event))
        (sheet-pointer-cursor (event-sheet event))))


;;; Pointer events [wip]

;;; Mouse SDL2 event handlers.

;;; The variable WHICH contains the mouse ID or SDL2-FFI:+SDL-TOUCH-MOUSEID+ for
;;; touch events. If we decide to support finger events, then we should ignore
;;; touch events or accept the fact that we receive duplicated input.

#+ (or)
(progn
  (define-sdl2-handler (ev :mousemotion)
      (event window-id which state x y #|timestamp xrel yrel|#)
    sdl2-ffi:+sdl-touch-mouseid+)

  (define-sdl2-handler (ev :mousewheel)
      (event window-id which x y direction precise-x precise-y)
    sdl2-ffi:+sdl-touch-mouseid+)

  (define-sdl2-handler (ev :mousebuttondown)
      (event window-id which button state clicks x y)
    (let* ((port *sdl2-port*)
           (mirror (find-sdl2-resource *sdl2-port* window-id))
           (sheet (sdl2-resource-clim-object mirror)))
      (make-instance 'pointer-button-press-event
                     :sheet sheet
                     :pointer (port-pointer port)
                     :x x :y y
                     :button (sdl2-button-to-clim-button button)
                     :modifier-state (port-modifier-state port))))

  (define-sdl2-handler (ev :mousebuttonup)
      (event window-id which button state clicks x y)
    )
  )
