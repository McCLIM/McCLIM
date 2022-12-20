;;; Syntactic sugar for code cariosity.

(in-package #:mcclim-sdl2)


;;; Bitmasks, type accessors etc -- cl-libsdl2 has incomplete definitions, so
;;; we use these only as a reference, while the actual data is curbed from
;;; wiki.libsdl.org and the library source code (via autowrap and manually).

;;; https://wiki.libsdl.org/SDL_Init
(autowrap:define-bitmask-from-constants (sdl-init-flags)
  sdl2-ffi:+sdl-init-timer+
  sdl2-ffi:+sdl-init-audio+
  sdl2-ffi:+sdl-init-video+
  sdl2-ffi:+sdl-init-joystick+
  sdl2-ffi:+sdl-init-haptic+
  sdl2-ffi:+sdl-init-gamecontroller+
  sdl2-ffi:+sdl-init-events+
  sdl2-ffi:+sdl-init-sensor+
  sdl2-ffi:+sdl-init-noparachute+       ; ignored (compatibility)
  sdl2-ffi:+sdl-init-everything+)

(autowrap:define-bitmask-from-enum
    (sdl2-keymod sdl2-ffi:sdl-keymod))

(autowrap:define-bitmask-from-enum
    (sdl2-cursor-flags sdl2-ffi:sdl-system-cursor))

(defun map-system-cursor (cursor-name)
  (autowrap:mask 'sdl2-cursor-flags
                 (case cursor-name
                   ((:default :arrow) :system-cursor-arrow)
                   ((:prompt :i-beam) :system-cursor-ibeam)
                   ((:button :hand)   :system-cursor-hand)
                   ((:busy :wait)     :system-cursor-wait)
                   ((:not-allowed)    :system-cursor-no)
                   ((:position)       :system-cursor-crosshair)
                   ((:move)           :system-cursor-sizeall)
                   ((:arrow-we)       :system-cursor-sizewe)
                   ((:arrow-ns)       :system-cursor-sizens)
                   ((:grab)           :system-cursor-hand) ;not perfect
                   ((:help)           :system-cursor-hand) ;not perfect
                   ;; AUTOWRAP:MASK will signal a warning and use 0 (arrow).
                   (otherwise :system-cursor-unknown))))

;; https://wiki.libsdl.org/SDL2/SDL_WindowFlags
(autowrap:define-bitmask-from-enum
    (sdl-window-flags sdl2-ffi:sdl-window-flags))

;;; https://wiki.libsdl.org/SDL_Event - describes the event data structure.
;;; CL-SDL2 provides an abstraction that allows referring to SDL2 constants as
;;; keywords (via autowrap) - for example SDL_QuitEvent becomes :QUIT. Event
;;; types will be used to specialize the function HANDLE-SDL2-EVENT.

;;; https://wiki.libsdl.org/SDL_Event
(defvar *event-type-accessors*
  '(((:audiodeviceadded :audiodeviceremoved)                                     :adevice)
    ((:controlleraxismotion)                                                     :caxis)
    ((:controllerbuttondown :controllerbuttonup)                                 :cbutton)
    ((:controllerdeviceadded :controllerdeviceremoved :controllerdeviceremapped) :cdevice)
    ((:dollargesture :dollarrecord)                                              :dgesture)
    ((:dropfile :droptext :dropbegin :dropcomplete)                              :drop)
    ((:fingermotion :fingerdown :fingerup)                                       :tfinger)
    ((:keydown :keyup)                                                           :key)
    ((:joyaxismotion)                                                            :jaxis)
    ((:joyballmotion)                                                            :jball)
    ((:joyhatmotion)                                                             :jhat)
    ((:joybuttondown :joybuttonup)                                               :jbutton)
    ((:joydeviceadded :joydeviceremoved)                                         :jdevice)
    ((:mousemotion)                                                              :motion)
    ((:mousebuttondown :mousebuttonup)                                           :button)
    ((:mousewheel)                                                               :wheel)
    ((:multigesture)                                                             :mgesture)
    ((:quit)                                                                     :quit)
    ((:syswmevent)                                                               :syswm)
    ((:textediting)                                                              :edit)
    ((:textinput)                                                                :text)
    ((:userevent)                                                                :user)
    ((:windowevent)                                                              :window)))

;;; https://wiki.libsdl.org/SDL_WindowEventID
(autowrap:define-enum-from-constants (windowevent.event)
  sdl2-ffi:+sdl-windowevent-none+
  sdl2-ffi:+sdl-windowevent-shown+
  sdl2-ffi:+sdl-windowevent-hidden+
  sdl2-ffi:+sdl-windowevent-exposed+
  sdl2-ffi:+sdl-windowevent-moved+
  sdl2-ffi:+sdl-windowevent-resized+      ; always preceded by size-changed
  sdl2-ffi:+sdl-windowevent-size-changed+ ; use this event to handle all resizes
  sdl2-ffi:+sdl-windowevent-minimized+
  sdl2-ffi:+sdl-windowevent-maximized+
  sdl2-ffi:+sdl-windowevent-restored+
  sdl2-ffi:+sdl-windowevent-enter+
  sdl2-ffi:+sdl-windowevent-leave+
  sdl2-ffi:+sdl-windowevent-focus-gained+
  sdl2-ffi:+sdl-windowevent-focus-lost+
  sdl2-ffi:+sdl-windowevent-close+
  sdl2-ffi:+sdl-windowevent-take-focus+
  sdl2-ffi:+sdl-windowevent-hit-test+)


;;; SDL2 is expected to run in the main thread.

(defparameter *multiprocessing-p* clim-sys:*multiprocessing-p*)

(defvar *initialized-p* nil)
(defvar *initialized-cv* (clim-sys:make-condition-variable))
(defvar *initialized-lock* (clim-sys:make-lock "SDL2 init cv lock"))

(defun %init-sdl2 ()
  (unless *initialized-p*
    (sdl2-ffi.functions:sdl-init (autowrap:mask-apply 'sdl-init-flags '(:everything)))
    (setf *initialized-p* (clim-sys:current-process))
    (clim-sys:condition-notify *initialized-cv*)
    (log:info "Hello!")))

(defun %quit-sdl2 ()
  (when *initialized-p*
    (log:info "Good bye.")
    (setf *initialized-p* nil)
    (sdl2-ffi.functions:sdl-quit)))

(defun %read-sdl2 (event timeout)
  (let ((rc (if (null timeout)
                (sdl2-ffi.functions:sdl-wait-event event)
                (sdl2-ffi.functions:sdl-wait-event-timeout event timeout))))
    (= rc 1)))

;;; This implements semantics of process-next-event but without distributing
;;; the event - there is no need for the port argument.
(defun %next-sdl2 (wait-function timeout)
  (when (maybe-funcall wait-function)
    (return-from %next-sdl2 (values nil :wait-function)))
  (sdl2:with-sdl-event (event)
    (alx:if-let ((ev (%read-sdl2 event timeout)))
      (let ((event-type (sdl2:get-event-type event)))
        (values (handle-sdl2-event event-type event) event-type))
      (if (maybe-funcall wait-function)
          (values nil :wait-function)
          (values nil :timeout)))))

(defun sdl2-window (window-id)
  (let ((window (sdl2-ffi.functions:sdl-get-window-from-id window-id)))
    (if (autowrap:wrapper-null-p window)
        (log:warn "Window ~s doesn't exist." window-id)
        window)))

;;; The function HANDLE-SDL2-EVENT performs libsdl calls and must be called
;;; from the same thread. All methods for that function must be specified with
;;; DEFINE-SDL2-HANDLER, or a macro DEFINE-SDL2-REQUEST that is used to define
;;; also functions that perform safe asynchronous calls with user-specified
;;; events.

(defgeneric handle-sdl2-event (key event)
  (:method (key event)
    (log:debug "Ignoring ~s ~a" key event)))

;;; Requests are submitted in a slightly roundabout way to ensure receiving
;;; results from the requesting thread:
;;;
;;; - the request pushes an sdl event with a queue to read the result
;;; - the main thread reads the event and invokes the event handler
;;; - the event handler sends the return value by a received queue
;;;
;;; When the request is not synchronized (default), then no queue is send and
;;; the return value is ignored.
;;;
;;; FIXME ensure that the port is running.
;;; FIXME queues should be reused (make a pool).
;;; NIH implement our own "user events" similar to cl-sdl2
(defmacro define-sdl2-request (name args &body body)
  (assert (null (intersection args lambda-list-keywords)))
  (alx:with-gensyms (sync queue timeout)
    (let ((event-type (intern (format nil "CLIM-~a" name) :keyword))
          (user-data `(list ,@args :synchronize ,queue))
          (user-data-no-sync `(list ,@args)))
      `(progn
         (sdl2:register-user-event-type ,event-type)
         (defun ,name (,@args &key ((:synchronize ,sync) nil))
           (cond
             ((or (null *multiprocessing-p*)
                  ;; This is to ensure that requests are reentrant.
                  (eq (clim-sys:current-process) *initialized-p*))
              (handle-sdl2-event ,event-type ,user-data-no-sync))
             ((null ,sync)
              (sdl2:push-user-event ,event-type ,user-data-no-sync)
              nil)
             ((let ((,queue (tch:make-queue))
                    (,timeout (and (numberp ,sync) ,sync)))
                (sdl2:push-user-event ,event-type ,user-data)
                (if ,timeout
                    (values (tch:try-pop-queue ,queue :timeout ,timeout))
                    (tch:pop-queue ,queue))))))
         (define-sdl2-handler (^event ,event-type) ,args ,@body)))))

(defmacro define-sdl2-handler ((event type) args &body body)
  (alx:with-gensyms (event-key)
    `(defmethod handle-sdl2-event ((,event-key (eql ,type)) ,event)
       (declare (ignorable ,event-key ,event))
       ,@(expand-handler-for-event event type args body))))

(defun expand-handler-for-event (event-var event-type args body)
  (alx:if-let ((r-key (second (find event-type *event-type-accessors*
                                    :key #'first :test #'member))))
    (expand-handler-for-core-event event-var r-key args body)
    (expand-handler-for-user-event event-var args body)))

(defun expand-handler-for-core-event (event-var reference args body)
  (when (null args)
    (return-from expand-handler-for-core-event body))
  `((let ,(loop for arg-name in args
                for arg-key = (alx:make-keyword arg-name)
                for arg-val = `(plus-c:c-ref
                                ,event-var sdl2-ffi:sdl-event ,reference ,arg-key)
                collect `(,arg-name ,arg-val))
      (declare (ignorable ,@args))
      ,@body)))

(defun expand-user-event-args (event-var)
  ;; When the event-var is a list then we are called "directly" so there is no
  ;; need get user data from the event. Otherwise it is an asynchronous call.
  `(if (listp ,event-var)
       ,event-var
       (let ((code (plus-c:c-ref ,event-var sdl2-ffi:sdl-event :user :code)))
         (sdl2::get-user-data code))))

(defun expand-handler-for-user-event (event-var args body)
  (alx:with-gensyms (result condition queue)
    `((destructuring-bind (,@args &key ((:synchronize ,queue) nil))
          ,(expand-user-event-args event-var)
        (declare (ignorable ,@args))
        (if (null ,queue)
            (progn ,@body)
            (let (,result)
              (unwind-protect
                   (handler-bind ((serious-condition
                                    (lambda (,condition)
                                      (setf ,result ,condition))))
                     (setf ,result (progn ,@body)))
                (tch:push-queue ,result ,queue))
              ,result))))))
