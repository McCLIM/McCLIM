(in-package :clim-gtk)

(defclass gtk-pointer (standard-pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)
   (x :initform 0)
   (y :initform 0)))

(defclass gtk-port (climi::standard-port) #+nil(basic-port)
  ((pointer        :accessor port-pointer
                   :initform (make-instance 'gtk-pointer))
   (event-queue    :initform nil
                   :accessor gtk-port/event-queue)
   (gtk-lock       :initform (bordeaux-threads:make-lock "GTK Port Lock")
                   :reader gtk-port/lock)
   (condvar        :initform (bordeaux-threads:make-condition-variable :name "GTK Port Condition Variable")
                   :reader gtk-port/condvar)
   (stop-request   :initform nil
                   :accessor gtk-port/stop-request)
   (image-fallback :initarg image-fallback
                   :reader gtk-port/image-fallback)))

(defclass gtk-mirror ()
  ((window                 :initarg :window
                           :accessor gtk-mirror/window)
   (image                  :initarg :image
                           :initform (alexandria:required-argument :image)
                           :accessor gtk-mirror/image)
   (requested-image-width  :initform nil
                           :accessor gtk-mirror/requested-image-width)
   (requested-image-height :initform nil
                           :accessor gtk-mirror/requested-image-height)
   (sheet                  :initarg :sheet
                           :initform (alexandria:required-argument :sheet)
                           :reader gtk-mirror/sheet)
   (drawing-area           :initarg :drawing-area
                           :accessor gtk-mirror/drawing-area)
   (pango-context          :initarg :pango-context
                           :accessor gtk-mirror/pango-context)
   (lock                   :initform (bordeaux-threads:make-lock)
                           :reader gtk-mirror/lock
                           :documentation "This lock must be held while accessing the requested dimension")
   (input-context          :initarg :input-context
                           :accessor gtk-mirror/input-context)
   (need-redraw            :initform nil
                           :accessor gtk-mirror/need-redraw)))

(defmethod find-port-type ((type (eql :null)))
  (values 'gtk-port 'identity))

(defun parse-gtk-server-path (path)
  path)

(setf (get :gtk :port-type) 'gtk-port)
(setf (get :gtk :server-path-parser) 'parse-gtk-server-path)

(defun call-with-no-traps (fn)
  #+sbcl
  (sb-int:with-float-traps-masked (:divide-by-zero)
    (funcall fn))
  #-sbcl
  (funcall fn))

(defun gtk-main-no-traps ()
  (call-with-no-traps #'gtk:gtk-main))

(defmethod initialize-instance :after ((port gtk-port) &rest initargs)
  (declare (ignore initargs))
  (push (make-instance 'gtk-frame-manager :port port)
	(slot-value port 'climi::frame-managers))
  (setf (slot-value port 'image-fallback) (cairo:cairo-image-surface-create :argb32 10 10))
  (bordeaux-threads:make-thread #'gtk-main-no-traps :name "GTK Event Thread")
  (start-port-event-thread port))

(defclass gtk-renderer-sheet (mirrored-sheet-mixin)
  ())

(defclass gtk-top-level-sheet-pane (gtk-renderer-sheet climi::top-level-sheet-pane)
  ())

(defmethod port-set-mirror-region ((port gtk-port) sheet region)
  ())

(defmethod port-set-mirror-transformation ((port gtk-port) sheet transformation)
  ())

#+nil
(defmethod climi::port-lookup-mirror ((port gtk-port) (sheet gtk-renderer-sheet))
  (log:info "LOOKING UP (port=~s) ~s to ~s" port sheet (gethash sheet (gtk-port/sheet-to-mirror port)))
  (gethash sheet (gtk-port/sheet-to-mirror port)))

#+nil
(defmethod climi::port-register-mirror ((port gtk-port) (sheet gtk-renderer-sheet) mirror)
  (setf (gethash sheet (gtk-port/sheet-to-mirror port)) mirror))

#+nil
(defmethod climi::port-unregister-mirror ((port gtk-port) (sheet gtk-renderer-sheet) mirror)
  (remhash sheet (gtk-port/sheet-to-mirror port)))

(defun draw-window-content (cr mirror)
  (let ((image (bordeaux-threads:with-lock-held ((gtk-mirror/lock mirror))
                 (let ((v (gtk-mirror/image mirror)))
                   (cairo:cairo-surface-reference v)
                   v))))
    (cairo:cairo-set-source-surface cr image 0 0)
    (cairo:cairo-rectangle cr 0 0
                           (cairo:cairo-image-surface-get-width image)
                           (cairo:cairo-image-surface-get-height image))
    (cairo:cairo-fill cr)
    (cairo:cairo-surface-destroy image)))

(defun make-backing-image (ink width height)
  (let* ((image (cairo:cairo-image-surface-create :argb32 (round-coordinate width) (round-coordinate height)))
         (cr (cairo:cairo-create image)))
    (apply-colour-from-ink cr ink)
    (cairo:cairo-paint cr)
    image))

(defmethod realize-mirror ((port gtk-port) (sheet mirrored-sheet-mixin))
  (log:info "Realising mirror: port=~s sheet=~s" port sheet)
  (let* ((q (compose-space sheet))
         (width (climi::space-requirement-width q))
         (height (climi::space-requirement-height q))
         (image (make-backing-image (medium-background (sheet-medium sheet)) width height))
         (mirror (make-instance 'gtk-mirror :sheet sheet :image image)))
    (multiple-value-bind (window input-context drawing-area pango-context)
        (in-gtk-thread ()
          (let ((window (make-instance 'gtk:gtk-window
                                       :type :toplevel
                                       :default-width (round-coordinate width)
                                       :default-height (round-coordinate height))))
            (let ((drawing-area (make-instance 'gtk:gtk-drawing-area))
                  (input-context (make-instance 'gtk:gtk-im-context-simple)))
              (gobject:g-signal-connect drawing-area "draw"
                                        (lambda (widget cr)
                                          (declare (ignore widget))
                                          (draw-window-content (gobject:pointer cr) mirror)
                                          gdk:+gdk-event-stop+))
              (create-event-listeners window port sheet mirror input-context)
              (gtk:gtk-container-add window drawing-area)
              (let ((pango-context (gtk:gtk-widget-create-pango-context drawing-area)))
                (gtk:gtk-im-context-set-client-window input-context (gtk:gtk-widget-window window))
                (gtk:gtk-widget-show-all window)
                (values window input-context drawing-area pango-context)))))
      (setf (gtk-mirror/window mirror) window)
      (setf (gtk-mirror/input-context mirror) input-context)
      (setf (gtk-mirror/drawing-area mirror) drawing-area)
      (setf (gtk-mirror/pango-context mirror) pango-context)
      (setf (climi::%sheet-direct-mirror sheet) mirror))))

(defmethod destroy-mirror ((port gtk-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (sheet-direct-mirror sheet)))
    (in-gtk-thread ()
      (gtk:gtk-widget-destroy (gtk-mirror/window mirror)))
    (cairo:cairo-surface-destroy (gtk-mirror/image mirror))))

(defmethod climi::port-lookup-mirror ((port gtk-port) (sheet null))
  nil)

(defmethod mirror-transformation ((port gtk-port) mirror)
  nil)

(defmethod port-enable-sheet ((port gtk-port) (mirror mirrored-sheet-mixin))
  nil)

(defmethod port-disable-sheet ((port gtk-port) (mirror mirrored-sheet-mixin))
  nil)

(defmethod destroy-port :before ((port gtk-port))
  (bordeaux-threads:with-lock-held ((gtk-port/lock port))
    (setf (gtk-port/stop-request port) t))
  (in-gtk-thread ()
    (gtk:gtk-main-quit)))

(defmethod make-graft ((port gtk-port) &key (orientation :default) (units :device))
  (make-instance 'gtk-graft
                 :port port :mirror (gensym)
                 :orientation orientation :units units))

(defmethod make-medium ((port gtk-port) sheet)
  (make-instance 'gtk-medium :sheet sheet))

(defmethod graft ((port gtk-port))
  (first (climi::port-grafts port)))

(defmethod port-allocate-pixmap ((port gtk-port) sheet width height)
  (declare (ignore sheet width height))
  ;; FIXME: this isn't actually good enough; it leads to errors in
  ;; WITH-OUTPUT-TO-PIXMAP
  nil)

(defmethod port-deallocate-pixmap ((port gtk-port) pixmap)
  #+nil
  (when (pixmap-mirror port pixmap)
    (destroy-mirror port pixmap)))

(defmethod pointer-position ((pointer gtk-pointer))
  (values (slot-value pointer 'x) (slot-value pointer 'y)))

#+nil
(defmethod pointer-button-state ((pointer gtk-pointer))
  nil)

#+nil
(defmethod port-modifier-state ((port gtk-port))
  nil)

#+nil
(defmethod synthesize-pointer-motion-event ((pointer gtk-pointer))
  nil)

#+nil
(defmethod (setf port-keyboard-input-focus) (focus (port gtk-port))
  focus)

#+nil
(defmethod port-keyboard-input-focus ((port gtk-port))
  nil)

(defmethod port-force-output ((port gtk-port))
  nil)

(defvar *cursor-names* '((:busy . "wait")
                         (:button . "default")
                         (:default . "default")
                         (:horizontal-scroll . "default")
                         (:horizontal-thumb . "default")
                         (:lower-left . "sw-resize")
                         (:lower-right . "se-resize")
                         (:move . "move")
                         (:position . "crosshair")
                         (:prompt . "context-menu")
                         (:scroll-down . "default")
                         (:scroll-left . "default")
                         (:scroll-right . "default")
                         (:scroll-up . "default")
                         (:upper-left . "nw-resize")
                         (:upper-right . "ne-resize")
                         (:vertical-scroll . "default")
                         (:vertical-thumb . "default")
                         (:i-beam . "text")
                         (:vertical-pointer . "pointer")
                         (:pencil . "pointer")
                         (:rotate . "pointer")
                         (:choose . "pointer")))

(defvar *cursor-name-map* (let ((m (make-hash-table)))
                            (loop
                              for (clim-name . gtk-name) in *cursor-names*
                              do (setf (gethash clim-name m) gtk-name))
                            m))

(defmethod set-sheet-pointer-cursor ((port gtk-port) (sheet mirrored-sheet-mixin) cursor)
  (let* ((mirror (sheet-direct-mirror sheet))
         (window (gtk-mirror/window mirror))
         (gdk-cursor (or (gethash cursor *cursor-name-map*) "default")))
    (in-gtk-thread ()
      (gdk:gdk-window-set-cursor (gtk:gtk-widget-window window)
                                 (gdk:gdk-cursor-new-from-name (gtk:gtk-widget-get-display window) gdk-cursor)))))
