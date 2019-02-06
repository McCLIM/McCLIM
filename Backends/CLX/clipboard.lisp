(in-package :clim-clx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following declarations belongs in core, not in the CLX
;;; backend.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;;  Functions dealing with copying to the clipboard.
;;;

(defgeneric copy-to-clipboard (port object)
  (:documentation "Copy OBJECT to the clipboard."))

(defmethod copy-to-clipboard ((port clim:port) object)
  "Fallback implementation if the port does not support copying to the clipboard."
  nil)

(defgeneric clipboard-representation-types (object)
  (:documentation "Returns the representation types that OBJECT can be converted into."))

(defmethod clipboard-representation-types ((object string))
  '(:string))

(defgeneric convert-clipboard-object (object type)
  (:documentation "Convert OBJECT to representation type TYPE.

When specialising this function, make sure that
CLIPBOARD-REPRESENTATION-TYPES is also implemented to return the
corresponding type."))

(defmethod convert-clipboard-object ((object string) (type (eql :string)))
  object)

;;;
;;;  The following functions implement the standard API to request
;;;  content from the clipboard.
;;;

(defgeneric request-clipboard-content-from-port (port pane type)
  (:documentation "Backend implementation of REQUEST-CLIPBOARD-CONTENT.")
  (:method ((port clim:port) pane type)
    (error "Clipboard not implemented for port: ~s" port)))

(defun request-clipboard-content (pane type)
  (request-clipboard-content-from-port (port pane) pane type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLX implementation of clipboard management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric process-clipboard-event (port window event-key target property requestor selection time)
  (:method (port window event-key target property requestor selection time)
    ;; Default implementation doesn't support clipboard
    nil))

(defclass clx-clipboard-port-mixin ()
  ((clipboard-window :initform nil)
   (clipboard-content :initform nil
                      :accessor clipboard-content)))

(defmethod copy-to-clipboard ((port clx-clipboard-port-mixin) obj)
  (let ((window (clipboard-window port)))
    (xlib:set-selection-owner (xlib:window-display window) :clipboard window nil)
    (when (eq (xlib:selection-owner (xlib:window-display window) :clipboard) window)
      (setf (clipboard-content port) obj))))

(defun clipboard-window (port)
  (check-type port clx-clipboard-port-mixin)
  (or (slot-value port 'clipboard-window)
      (setf (slot-value port 'clipboard-window) (create-clipboard-window port))))

(defun create-clipboard-window (port)
  (let* ((display (clx-port-display port))
         (window (xlib:create-window :parent (xlib:screen-root (car (xlib:display-roots display)))
                                     :class :input-only
                                     :x 0
                                     :y 0
                                     :width 100
                                     :height 100)))
    window))

(deftype representation-type-name () '(member :string :html))

(defun representation-type-to-native (type)
  (case type
    (:string '(:utf8_string :text :string))
    (:html '(:|text/html|))))

(defun make-targets-response (port)
  (let ((display (clx-port-display port))
        (types (loop
                 for type in (clipboard-representation-types (clipboard-content port))
                 append (representation-type-to-native type))))
    (mapcar (lambda (v)
              (xlib:intern-atom display v))
            (remove-duplicates (cons :targets types)))))

(defun process-selection-request (port window target property requestor selection time)
  (let ((display (xlib:window-display window))
        (content (clipboard-content port)))
    (declare (ignore display))
    (flet ((send-reply-event ()
             (xlib:send-event requestor :selection-notify nil
                                        :window requestor
                                        :event-window requestor
                                        :selection selection
                                        :target target
                                        :property property
                                        :time time)))
      (case target
        ((:targets)
         (let ((targets (make-targets-response port)))
           (xlib:change-property requestor property targets target 32)
           (send-reply-event)))
        ((:utf8_string :text :string)
         (let ((content-as-string (convert-clipboard-object content :string)))
           (xlib:change-property requestor property (babel:string-to-octets content-as-string :encoding :utf-8) :utf8_string 8)
           (send-reply-event)))
        ((:|text/html|)
         (let ((content-as-html (convert-clipboard-object content :html)))
           (xlib:change-property requestor property (babel:string-to-octets content-as-html :encoding :utf-8) :|text/html| 8)
           (send-reply-event)))))))

(defun process-selection-notify (port window target property requestor selection time)
  (case target
    ((:targets)
     (process-targets-reply port window target property requestor selection time))
    ((:utf8_string)
     (let ((content (babel:octets-to-string (coerce (xlib:get-property window property) '(vector (unsigned-byte 8)))
                                            :encoding :utf-8)))
       (process-string-reply content)))))

(defmethod process-clipboard-event ((port clx-clipboard-port-mixin) window event-key target property requestor selection time)
  (when (eq window (clipboard-window port))
    (log:info "Clipboard event: key=~s target=~s prop=~s req=~s sel=~s time=~s" event-key target property requestor selection time)
    (ecase event-key
      (:selection-request (process-selection-request port window target property requestor selection time))
      (:selection-notify (process-selection-notify port window target property requestor selection time))))
  nil)

;;;
;;;  Paste support
;;;

(defvar *outstanding-request* nil)

(defmethod request-clipboard-content-from-port ((port clx-clipboard-port-mixin) pane type)
  (check-type type representation-type-name)
  (setq *outstanding-request* (list pane type))
  (xlib:convert-selection :clipboard :targets (clipboard-window port) nil nil))

(defun process-targets-reply (port window target property requestor selection time)
  (declare (ignore target property requestor selection))
  (when *outstanding-request*
    (let* ((display (xlib:window-display window))
           (targets (mapcar (lambda (v)
                              (xlib:atom-name display v))
                            (xlib:get-property window :targets)))
           (native-representation (representation-type-to-native (second *outstanding-request*)))
           (selected-type (loop
                            for type in native-representation
                            when (member type targets)
                              return type)))
      (if selected-type
          (xlib:convert-selection :clipboard selected-type (clipboard-window port) nil time)
          ;; ELSE: The clipboard doesn't support content of this type, send a negative response here
          nil))))

(defun process-string-reply (content)
  (when *outstanding-request*
    ;; Need to send the event here
    (setq *outstanding-request* nil)))
