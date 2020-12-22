(in-package :clim-gtk)

(defclass gtk-pointer (standard-pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)
   (x :initform 0)
   (y :initform 0)))

(defclass gtk-port (basic-port)
  ((id)
   (pointer :accessor port-pointer :initform (make-instance 'gtk-pointer))
   (window :initform nil :accessor gtk-port-window)))

(defmethod find-port-type ((type (eql :null)))
  (values 'gtk-port 'identity))

(defun parse-gtk-server-path (path)
  path)

(setf (get :gtk :port-type) 'gtk-port)
(setf (get :gtk :server-path-parser) 'parse-gtk-server-path)

(defmethod initialize-instance :after ((port gtk-port) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value port 'id) (gensym "GTK-PORT-"))
  ;; FIXME: it seems bizarre for this to be necessary
  (push (make-instance 'gtk-frame-manager :port port)
	(slot-value port 'climi::frame-managers))
  (bordeaux-threads:make-thread #'gtk:gtk-main :name "GTK Event Thread"))

(defmethod print-object ((object gtk-port) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~S ~S" :id (slot-value object 'id))))

(defclass gtk-renderer-sheet (mirrored-sheet-mixin)
  ())

(defclass gtk-top-level-sheet-pane (gtk-renderer-sheet climi::top-level-sheet-pane)
  ())

(defmethod port-set-mirror-region ((port gtk-port) sheet region)
  ())
                                   
(defmethod port-set-mirror-transformation ((port gtk-port) sheet transformation)
  ())

(defmethod climi::port-lookup-mirror ((port gtk-port) (sheet gtk-renderer-sheet))
  (break))

(defmethod climi::port-register-mirror ((port gtk-port) (sheet gtk-renderer-sheet) mirror)
  (break))

(defmethod climi::port-unregister-mirror ((port gtk-port) (sheet gtk-renderer-sheet) mirror)
  (break))

(defmethod realize-mirror ((port gtk-port) (sheet mirrored-sheet-mixin))
  (log:info "Realising mirror: port=~s sheet=~s" port sheet)
  (let* ((q (compose-space sheet))
         (win (in-gtk-thread ()
                (let ((window (make-instance 'gtk:gtk-window
                                             :type :toplevel
                                             :default-width (climi::space-requirement-width q)
                                             :default-height (climi::space-requirement-height q))))
                  (gtk:gtk-widget-add-events window '(:all-events-mask))
                  (gtk:gtk-widget-show-all window)
                  window))))
    (break)
    (climi::port-register-mirror port sheet win)))

(defmethod destroy-mirror ((port gtk-port) (sheet mirrored-sheet-mixin))
  ())

(defmethod mirror-transformation ((port gtk-port) mirror)
  ())

(defmethod port-enable-sheet ((port gtk-port) (mirror mirrored-sheet-mixin))
  nil)

(defmethod port-disable-sheet ((port gtk-port) (mirror mirrored-sheet-mixin))
  nil)

(defmethod destroy-port :before ((port gtk-port))
  (in-gtk-thread ()
    (gtk:gtk-main-quit)))

(defmethod process-next-event ((port gtk-port) &key wait-function (timeout nil))
  (cond ((maybe-funcall wait-function)
         (values nil :wait-function))
        ((not (null timeout))
         (sleep timeout)
         (if (maybe-funcall wait-function)
             (values nil :wait-function)
             (values nil :timeout)))
        ((not (null wait-function))
         (loop do (sleep 0.1)
               until (funcall wait-function)
               finally (return (values nil :wait-function))))
        (t
         (error "Game over. Listening for an event on GTK backend."))))

(defmethod make-graft
    ((port gtk-port) &key (orientation :default) (units :device))
  (make-instance 'gtk-graft
                 :port port :mirror (gensym)
                 :orientation orientation :units units))

(defmethod make-medium ((port gtk-port) sheet)
  (make-instance 'gtk-medium :sheet sheet))

(defmethod text-style-mapping
    ((port gtk-port) (text-style text-style) &optional character-set)
  (declare (ignore port text-style character-set))
  nil)

(defmethod (setf text-style-mapping) (font-name
                                      (port gtk-port)
                                      (text-style text-style)
                                      &optional character-set)
  (declare (ignore font-name text-style character-set))
  nil)

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

(defmethod pointer-button-state ((pointer gtk-pointer))
  nil)

(defmethod port-modifier-state ((port gtk-port))
  nil)

(defmethod synthesize-pointer-motion-event ((pointer gtk-pointer))
  nil)

(defmethod (setf port-keyboard-input-focus) (focus (port gtk-port))
  focus)

(defmethod port-keyboard-input-focus ((port gtk-port))
  nil)

(defmethod port-force-output ((port gtk-port))
  nil)

(defmethod distribute-event :around ((port gtk-port) event)
  (declare (ignore event))
  nil)

(defmethod set-sheet-pointer-cursor ((port gtk-port) sheet cursor)
  (declare (ignore sheet cursor))
  nil)        
