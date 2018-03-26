(in-package :clim-mezzano)

(defvar *port* NIL)
(defvar *current-focus* NIL)

;;======================================================================
;; Define pointer class
;;======================================================================

(defclass mezzano-pointer (standard-pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)
   (x :initform 0)
   (y :initform 0)))

(defmethod synthesize-pointer-motion-event ((pointer mezzano-pointer))
  ;; TODO - write this function
  )

(defmethod pointer-position ((pointer mezzano-pointer))
  (values *last-mouse-x* *last-mouse-y*))

;;======================================================================
;; Define port class
;;======================================================================

;;
;; All mezzano events are piped through a single fifo (mez-fifo) which
;; is read by get-next-event. get-next-event translates mezzano events
;; to mcclim events using mez-window->sheet to figure out which mcclim
;; sheet corresponds to the mezzano window which received the
;; event. The mcclim events are placed in mcclim-fifo because a single
;; mezzano event can generate multiple mcclim events. get-next-event
;; then returns the events from mcclim-fifo one at a time.
;;

(defclass mezzano-port (standard-handled-event-port-mixin
		       render-port-mixin
                       standard-port)
  ((pointer            :reader   port-pointer)
   (window             :accessor mezzano-port-window)
   (display-thread     :accessor mezzano-display-thread)
   (event-thread       :accessor mezzano-event-thread)
   (cursor-table       :accessor mezzano-cursor-table)
   (mez-window->sheet  :initform (make-hash-table :test #'eq))
   (mez-window->mirror :initform (make-hash-table :test #'eq))
   (mez-fifo           :initform (mos:make-fifo 50)
                       :reader   mezzano-mez-fifo)
   (mcclim-fifo        :initform (mos:make-fifo 10)
                       :reader   mezzano-mcclim-fifo)))

(defmethod port-lookup-sheet ((port mezzano-port) (mez-window mos:window))
  (gethash mez-window (slot-value port 'mez-window->sheet)))

(defmethod port-lookup-mirror ((port mezzano-port) (mez-window mos:window))
  (gethash mez-window (slot-value port 'mez-window->mirror)))

(defun parse-mezzano-server-path (path)
  (declare (ignore path))
  (list :mezzano
        :host       :mezzano
        :display-id 0
        :screen-id  0
        :protocol   :native))

(setf (get :mezzano :port-type) 'mezzano-port)
(setf (get :mezzano :server-path-parser) 'parse-mezzano-server-path)

(defun initialize-display-thread (port)
  (mos:make-thread
   (lambda ()
     (loop
        (handler-case
            (maphash #'(lambda (key val)
                         (when (typep key 'mezzano-mirrored-sheet-mixin)
                           (image-mirror-to-mezzano (sheet-mirror key))))
                     (slot-value port 'climi::sheet->mirror))
          (condition (condition)
            (format *debug-io* "~A~%" condition)))
        (sleep 0.01)))
   :name "McCLIM Display"))

(defun initialize-event-thread (port)
  (when clim-sys:*multiprocessing-p*
    (mos:make-thread
     (lambda ()
       (loop
          (with-simple-restart
              (restart-event-loop
               "Restart CLIM's event loop.")
            (loop
               (process-next-event port)))))
     :name "McCLIM Events")))

(defmethod initialize-instance :after ((port mezzano-port) &rest args)
  (declare (ignore args))
  (setf *port* port
        (slot-value port 'pointer) (make-instance 'mezzano-pointer :port port)
        (mezzano-port-window port) (mos:current-framebuffer))
  (push (make-instance 'mezzano-frame-manager :port port)
	(slot-value port 'frame-managers))
  (clim-extensions:port-all-font-families port)
  (setf (mezzano-display-thread port) (initialize-display-thread port)
        (mezzano-event-thread port) (initialize-event-thread port)
        ))

(defmethod destroy-port :before ((port mezzano-port))
  ;; TODO - there's more to clean up here:
  ;; close any mez-windows/mez-frames
  ;; destroy the associated frame-manager
  (bt:destroy-thread (mezzano-display-thread port))
  (bt:destroy-thread (mezzano-event-thread port)))

(defmethod port-enable-sheet ((port mezzano-port) (mirror mirrored-sheet-mixin))
  )

(defmethod port-frame-keyboard-input-focus ((port mezzano-port) frame)
  (frame-properties frame 'focus))

(defmethod (setf port-frame-keyboard-input-focus)
    (focus (port mezzano-port) frame)
  (setf *current-focus* focus)
  (setf (frame-properties frame 'focus) focus))

(defun create-mezzano-mirror (port sheet title width height top-border
                              &key (close-button-p t) (resizablep t))
  ;; Assumes left, right and bottom borders are 1, which is the
  ;; default when creating a frame
  (setf width (max 5 width))
  (setf height (max 5 height))
  (let* ((fwidth (+ width 2))
         (fheight (+ height 1 top-border))
         (mirror (make-instance 'mezzano-mirror))
         (fifo (mezzano-mez-fifo port))
         (window (mos:make-window fifo fwidth fheight))
         (surface (mos:window-buffer window))
         (frame (make-instance 'mos:frame
                               :top top-border
                               :framebuffer surface
                               :title title
                               :close-button-p close-button-p
                               :resizablep resizablep
                               :damage-function (mos:default-damage-function window)
                               :set-cursor-function (mos:default-cursor-function window))))
    (setf (slot-value mirror 'mcclim-render-internals::dirty-region) nil
          (slot-value mirror 'fwidth) fwidth
          (slot-value mirror 'fheight) fheight
          (slot-value mirror 'dx) 1
          (slot-value mirror 'dy) top-border
          (slot-value mirror 'width) width
          (slot-value mirror 'height) height
          (slot-value mirror 'mez-pixels) (mos:surface-pixels surface)
          (slot-value mirror 'mez-window) window
          (slot-value mirror 'mez-frame) frame)
    (port-register-mirror port sheet mirror)
    (setf (gethash window (slot-value port 'mez-window->sheet)) sheet
          (gethash window (slot-value port 'mez-window->mirror)) mirror)))

(defmethod realize-mirror ((port mezzano-port) (sheet mirrored-sheet-mixin))
  (%realize-mirror port sheet)
  (port-lookup-mirror port sheet)
  )

(defmethod realize-mirror ((port mezzano-port) (pixmap pixmap))
  )

(defmethod %realize-mirror ((port mezzano-port) (sheet basic-sheet))
  (debug-format "%realize-mirror ((port mezzano-port) (sheet basic-sheet))")
  (debug-format "    ~S ~S" port sheet)
  (break)
  )

(defmethod %realize-mirror ((port mezzano-port) (sheet top-level-sheet-pane))
  (let* ((q (compose-space sheet))
         (frame (pane-frame sheet))
         (mirror (create-mezzano-mirror
                  port sheet
                  (frame-pretty-name frame)
                  (round-coordinate (space-requirement-width q))
                  (round-coordinate (space-requirement-height q))
                  19)))
    (setf (slot-value mirror 'top-levelp) t)
    mirror))

(defmethod %realize-mirror ((port mezzano-port) (sheet unmanaged-top-level-sheet-pane))
  (create-mezzano-mirror port sheet "" 300 300 2
                         :close-button-p NIL
                         :resizablep NIL))

(defmethod make-medium ((port mezzano-port) sheet)
  (make-instance 'mezzano-medium
		 ;; :port port
		 ;; :graft (find-graft :port port)
		 :sheet sheet))


(defmethod make-graft ((port mezzano-port) &key (orientation :default) (units :device))
  (let ((graft (make-instance 'mezzano-graft
                              :port port
                              :mirror (mezzano-port-window port)
                              :orientation orientation
                              :units units)))
    (setf (sheet-region graft)
          (make-bounding-rectangle
           0 0
           (graft-width graft)
           (graft-height graft)))
    (push graft (port-grafts port))
    graft))

(defmethod graft ((port mezzano-port))
  (first (port-grafts port)))


(defmethod port-force-output ((port mezzano-port))
  (maphash
   #'(lambda (key val)
       (when (typep key 'mezzano-mirrored-sheet-mixin)
         (mcclim-render-internals::%mirror-force-output (sheet-mirror key))))
   (slot-value port 'climi::sheet->mirror))
  )

;;
;; Polling for events every 10ms
;; TODO would be better if we could set a timer and wait on the timer
;; or a new fifo entry instead.
;;

;; return :timeout on timeout
;;
(defmethod get-next-event ((port mezzano-port) &key wait-function (timeout nil))
  (declare (ignore wait-function))
  (let ((mez-fifo (mezzano-mez-fifo port))
        (mcclim-fifo (mezzano-mcclim-fifo port)))
    (if (null timeout)
        ;; check for a mcclim event - if one is available return
        ;; it. If none available wait for a mezzano event, which may
        ;; or may not generate a mcclim event, so check for a mcclim
        ;; event again.  Don't have to worry about a race condition of
        ;; a mcclim event arriving after checking while waiting on a
        ;; mezzano event because only this thread puts events in the
        ;; mcclim event fifo.

        ;; TODO - this loop needs to read from the mcclim-fifo until
        ;; it's empty before going to the mezzano-fifo. This doesn't
        ;; happen anymore because it sometimes ignores a mcclim
        ;; event. It works accidently because the only mcclim event it
        ;; ignores is a keyboard-event which only occurs singlely in
        ;; the mcclim-fifo.
        (loop
           (multiple-value-bind (event validp)
               (mos:fifo-pop mcclim-fifo nil)
             (when validp
               ;; ignore keyboard events if there's no event sheet to
               ;; handle them
               (unless (and (typep event 'keyboard-event)
                            (null (event-sheet event)))
                 (return event))))
           (mez-event->mcclim-event
            mcclim-fifo (mos:fifo-pop mez-fifo t)))
        (mos:panic "timeout not supported")
        ;; (loop
        ;;    (multiple-value-bind (event validp)
        ;;        (mos:fifo-pop fifo NIL)
        ;;      (cond (validp (return (convert-event event)))
        ;;            ((< timeout 0.005) (return :timeout))
        ;;            (T (sleep 0.01)
        ;;               (decf timeout 0.01)))))
        )))

;;; Pixmap

(defmethod destroy-mirror ((port mezzano-port) (pixmap mcclim-render-internals:image-pixmap-mixin))
  (call-next-method))

(defmethod realize-mirror ((port mezzano-port) (pixmap image-pixmap-mixin))
  (setf (sheet-parent pixmap) (graft port))
  (let ((mirror (make-instance 'image-mirror-mixin)))
    (port-register-mirror port pixmap mirror)
    (mcclim-render-internals::%make-image mirror pixmap)))

(defmethod port-allocate-pixmap ((port mezzano-port) sheet width height)
  (let ((pixmap (make-instance 'mezzano-pixmap
			       :sheet sheet
			       :width width
			       :height height
			       :port port)))
    (when (sheet-grafted-p sheet)
      (realize-mirror port pixmap))
    pixmap))

(defmethod port-deallocate-pixmap ((port mezzano-port) pixmap)
  (when (port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))

(defmethod set-sheet-pointer-cursor
    ((port mezzano-port) (sheet mirrored-sheet-mixin) cursor)
  ;; TODO - do we need to do anything here - or is the pointer cursor
  ;; completely managed by compositor?
  (unless (eq cursor :default)
    (debug-format "set-sheet-pointer-cursor ((port mezzano-port) (sheet mirrored-sheet-mixin) cursor)")
    (debug-format "    ~S ~S ~S" port sheet cursor)
    (break))
  )

(defmethod mcclim-render-internals::%set-image-region (mirror region)
  (debug-format "mcclim-render-internals::%set-image-region (mirror region)")
  (debug-format "    ~S ~S" mirror region))
