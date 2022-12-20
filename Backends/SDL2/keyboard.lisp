(in-package #:mcclim-sdl2)

;;; Handling the keyboard with SDL2.

;;; Requests
(define-sdl2-request sdl2-get-keyboard-focus ()
  (let* ((win (sdl2-ffi.functions:sdl-get-keyboard-focus))
         (wid (sdl2-ffi.functions:sdl-get-window-id win)))
    (find-sdl2-resource *sdl2-port* wid)))

(define-sdl2-request sdl2-grab-keyboard (mirror grabbed)
  (let ((window (sdl2-mirror-window mirror)))
    (sdl2-ffi.functions:sdl-set-window-keyboard-grab window grabbed)))

(defun sdl2-mod-to-clim-mod (state)
  (flet ((maybe-mod (mod &rest mask)
           (if (plusp (logand state (autowrap:mask-apply 'sdl2-keymod mask)))
               mod
               0)))
    (logior (maybe-mod +shift-key+ :shift)
            (maybe-mod +control-key+ :ctrl)
            (maybe-mod +meta-key+ :lalt)
            (maybe-mod +super-key+ :lgui)
            (maybe-mod +hyper-key+ :rgui))))

(define-sdl2-request sdl2-get-modifier-state ()
  (sdl2-mod-to-clim-mod (sdl2-ffi.functions:sdl-get-mod-state)))

(defmethod port-modifier-state ((port sdl2-port))
  (sdl2-get-modifier-state :synchronize t))

;;; Keyboard SDL2 event handlers.
;;;
;;; This already supports the keyboard layout. The SCANCODE contains a
;;; "physical" key while the KEYCODE contains the "mapped" character.
;;;
;;; It is worth thinking how we could incorporate this distinction in CLIM.

#+ (or)
(defparameter *sdl2-keycode-translation-table*
  (list sdl2-ffi:+sdlk-0+ :|0| #\0 #\)
        sdl2-ffi:+sdlk-0+ :|0| #\0 #\)))

(defun process-keyboard-event (window-id keysym class)
  (let* ((mirror (find-sdl2-resource *sdl2-port* window-id))
         (sheet (sdl2-resource-clim-object mirror)))
    (let ((scancode (plus-c:c-ref keysym sdl2-ffi:sdl-keysym :scancode))
          (keycode  (plus-c:c-ref keysym sdl2-ffi:sdl-keysym :sym))
          (modifier (plus-c:c-ref keysym sdl2-ffi:sdl-keysym :mod) ))
      (log:info "XXX ~s ~s ~s ~s (~a)" class sheet scancode keycode
                (sdl2-ffi.functions:sdl-get-key-name keycode))
      (make-instance class
                     :sheet sheet
                     :key-name scancode
                     :key-character keycode
                     :modifier-state (sdl2-mod-to-clim-mod modifier)))))

(define-sdl2-handler (ev :keydown) (window-id keysym repeat)
  (process-keyboard-event window-id keysym 'key-press-event))

(define-sdl2-handler (ev :keyup) (window-id keysym)
  (process-keyboard-event window-id keysym 'key-release-event))


;;; The support for IME.
;;; https://wiki.libsdl.org/SDL2/Tutorials-TextInput

(define-sdl2-handler (ev :textediting) (text)
  (log:info "textinput pending: ~s" text))

(define-sdl2-handler (ev :textinput) (window-id text)
  (let ((text* (cffi:foreign-string-to-lisp
                (plus-c:c-ref ev sdl2-ffi:sdl-event :text :text plus-c:&))))
    (log:info "textinput finished: ~s" text*)))

;;; Mentioned, but not documented(!)
(define-sdl2-handler (ev :keymapchanged) ()
  (log:debug "Keymap changed (???)"))
