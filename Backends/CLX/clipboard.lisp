(in-package :clim-clx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLX implementation of clipboard management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Port mixin

(defclass clx-clipboard-stored-object ()
  ((content :initarg :content
            :reader clipboard-stored-object-content)
   (type    :initarg :type
            :reader clipboard-stored-object-type)
   (owner   :initarg :owner
            :reader clipboard-stored-object-owner)))

(defclass clx-clipboard-port-mixin ()
  ((selection-content :initform nil
                      :accessor selection-content)
   (clipboard-content :initform nil
                      :accessor clipboard-content)
   (outstanding-request-pane :initform nil
                             :accessor clipboard-outstanding-request-pane)
   (outstanding-request-type  :initform nil
                              :accessor clipboard-outstanding-request-type)
   (outstanding-request-selection :initform nil
                                  :accessor clipboard-outstanding-request-selection)))

(defun find-stored-object (port selection)
  (case selection
    (:primary (selection-content port))
    (:clipboard (clipboard-content port))))

(defun set-stored-object (port selection value)
  (case selection
    (:primary (setf (selection-content port) value))
    (:clipboard (setf (clipboard-content port) value))))

;;; Event classes

(defclass clx-selection-event (window-event)
  ())

(defclass clx-selection-notify-event (clx-selection-event)
  ((selection :initarg :selection
              :reader selection-event-selection)
   (target   :initarg :target
             :reader selection-event-target)
   (property :initarg :property
             :reader selection-event-property)))

(defclass clx-selection-request-event (window-event)
  ((selection :initarg :selection
              :reader selection-event-selection)
   (target    :initarg :target
              :reader selection-event-target)
   (property  :initarg :property
              :reader selection-event-property)
   (requestor :initarg :requestor
              :reader selection-event-requestor)))

(defclass clx-selection-clear-event (window-event)
  ((selection :initarg :selection
              :reader selection-event-selection)))

;;; Main entry point

(defmethod clim-backend:copy-to-clipboard-with-port ((port clx-clipboard-port-mixin) sheet clipboard-p obj presentation-type)
  (let ((window (sheet-direct-xmirror sheet)))
    ;; We're not actually supposed to call set-selection-owner without
    ;; a timestamp due to the following statemnt in ICCCM:
    ;;
    ;;     Clients attempting to acquire a selection must set the time
    ;;     value of the SetSelectionOwner request to the timestamp of
    ;;     the event triggering the acquisition attempt, not to
    ;;     CurrentTime. A zero-length append to a property is a way to
    ;;     obtain a timestamp for this purpose; the timestamp is in
    ;;     the corresponding PropertyNotify event.
    ;;
    ;; The reasons for his seems to be to ensure that the ownership is
    ;; actually transferred correctly. This shouldn't be a major issue
    ;; in practice, and it significantly simplifies the
    ;; implementation.
    (let ((selection (if clipboard-p :clipboard :primary)))
      (xlib:set-selection-owner (xlib:window-display window) selection window nil)
      (let ((success-p (eq (xlib:selection-owner (xlib:window-display window) selection) window)))
        (set-stored-object port selection
                           (if success-p
                               (make-instance 'clx-clipboard-stored-object
                                              :content obj
                                              :type presentation-type
                                              :owner sheet)
                               nil))
        success-p))))

(defmethod clim-backend:clear-clipboard-with-port ((port clx-clipboard-port-mixin) sheet clipboard-p)
  (let ((window (sheet-direct-xmirror sheet))
        (selection (if clipboard-p :clipboard :primary)))
    (alexandria:when-let ((stored-object (find-stored-object port selection)))
      (when (eq (clipboard-stored-object-owner stored-object) sheet)
        (xlib:set-selection-owner (xlib:window-display window) selection nil nil)
        (set-stored-object port selection nil)))))

(defun representation-type-to-native (type)
  (case type
    (:string '(:utf8_string :text :string :|text/plain;charset=utf-8| :|text/plain|))
    (:html '(:|text/html|))
    (:image '(:|image/png| :|image/jpeg|))))

(defun make-targets-response (port content presentation-type)
  (let ((display (clx-port-display port))
        (types (loop
                 for output-type in '(:string :html)
                 when (clim-extensions:convert-clipboard-content content output-type :type presentation-type :check-only t)
                   append (representation-type-to-native output-type))))
    (mapcar (lambda (v)
              (xlib:intern-atom display v))
            (remove-duplicates (cons :targets types)))))

(defun process-selection-request (port window target property requestor selection time)
  (declare (ignore window))
  (let ((stored-object (find-stored-object port selection)))
    (when stored-object
      (let ((content (clipboard-stored-object-content stored-object))
            (presentation-type (clipboard-stored-object-type stored-object)))
        (flet ((send-reply-event (&optional omit-prop)
                 (xlib:send-event requestor :selection-notify nil
                                            :window requestor
                                            :event-window requestor
                                            :selection selection
                                            :target target
                                            :property (if omit-prop nil property)
                                            :time time)))
          (case target
            ((:targets)
             (let ((targets (make-targets-response port content presentation-type)))
               (xlib:change-property requestor property targets :atom 32)
               (send-reply-event)))
            ((:utf8_string :text :string :|text/plain;charset=utf-8| :|text/plain|)
             (let ((content-as-string (clim-extensions:convert-clipboard-content content :string :type presentation-type)))
               (xlib:change-property requestor property (babel:string-to-octets content-as-string :encoding :utf-8) :string 8)
               (send-reply-event)))
            ((:|text/html|)
             (let ((s (format nil "<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\">~a"
                              (clim-extensions:convert-clipboard-content content :html :type presentation-type))))
               (xlib:change-property requestor property
                                     (babel:string-to-octets s :encoding :utf-8)
                                     :string 8))
             (send-reply-event))
            (t
             (send-reply-event t)))
          nil)))))

(defun convert-image-array (array type)
  (flexi-streams:with-input-from-sequence (s array)
    (let ((image-content (ecase type
                           (:png (opticl:read-png-stream s))
                           (:jpeg (opticl:read-jpeg-stream s)))))
      (make-instance 'clim-extensions:image-pattern :array (climi::convert-opticl-img image-content)))))

(defun process-selection-notify (port window target property selection time)
  (case target
    ((:targets)
     (process-targets-reply port window selection time))
    ((:utf8_string :text :string  :|text/plain;charset=utf-8| :|text/plain|)
     (let ((content (babel:octets-to-string (coerce (xlib:get-property window property) '(vector (unsigned-byte 8)))
                                            :encoding :utf-8)))
       (process-string-reply port selection content :string)))
    ((:|text/html|)
     (let ((content (babel:octets-to-string (coerce (xlib:get-property window property) '(vector (unsigned-byte 8)))
                                            :encoding :utf-8)))
       (process-string-reply port selection content :html)))
    ((:|image/png|)
     (let ((image (convert-image-array (coerce (xlib:get-property window property) '(vector (unsigned-byte 8))) :png)))
       (process-string-reply port selection image :image)))
    ((:|image/jpeg|)
     (let ((image (convert-image-array (coerce (xlib:get-property window property) '(vector (unsigned-byte 8))) :jpeg)))
       (process-string-reply port selection image :image)))
    (t
     nil)))

(defun process-selection-clear (port selection)
  (set-stored-object port selection nil)
  nil)

(defmethod distribute-event ((port clx-clipboard-port-mixin) (event clx-selection-request-event))
  ;; When the event is generated from EVENT-HANDLER, the pane is
  ;; wrong. This is becasue there isn't enough information in the
  ;; event itself to determine to which pane it should be sent.
  ;;
  ;; This method checks who actually performed the original request
  ;; and adjusts the event to specify the correct receiver.
  (alexandria:when-let ((stored-object (find-stored-object port (selection-event-selection event))))
    (let ((pane (clipboard-stored-object-owner stored-object)))
      (setf (slot-value event 'clim:sheet) pane)
      (dispatch-event pane event))))

(defmethod distribute-event ((port clx-clipboard-port-mixin) (event clx-selection-notify-event))
  (alexandria:when-let ((outstanding-request-pane (clipboard-outstanding-request-pane port)))
    (when (eq (clipboard-outstanding-request-selection port)
              (selection-event-selection event))
      (setf (slot-value event 'clim:sheet) outstanding-request-pane)
      (dispatch-event outstanding-request-pane event))))

;;; The following methods all use an :AROUND method for their
;;; HANDLE-EVENT implementations. This is because we want to
;;; discriminate on the event type only, which means the event will
;;; never be delivered otherwise.
(defmethod handle-event :around (pane (event clx-selection-request-event))
  (process-selection-request (port pane)
                             (sheet-direct-xmirror pane)
                             (selection-event-target event)
                             (selection-event-property event)
                             (selection-event-requestor event)
                             (selection-event-selection event)
                             (event-timestamp event)))

(defmethod handle-event :around (pane (event clx-selection-notify-event))
  (process-selection-notify (port pane)
                            (sheet-direct-xmirror pane)
                            (selection-event-target event)
                            (selection-event-property event)
                            (selection-event-selection event)
                            (event-timestamp event)))

(defmethod handle-event :around (pane (event clx-selection-clear-event))
  (process-selection-clear (port pane) (selection-event-selection event)))

;;;
;;;  Paste support
;;;

(defmethod clim-backend:request-clipboard-content-with-port ((port clx-clipboard-port-mixin) pane clipboard-p type)
  (let ((selection (if clipboard-p :clipboard :primary)))
    (setf (clipboard-outstanding-request-pane port) pane)
    (setf (clipboard-outstanding-request-type port) (if (listp type) type (list type)))
    (setf (clipboard-outstanding-request-selection port) selection)
    (xlib:convert-selection selection :targets (sheet-direct-xmirror pane) :mcclim nil)))

(defun process-targets-reply (port window selection time)
  (when (and (clipboard-outstanding-request-pane port)
             (eq (clipboard-outstanding-request-selection port) selection))
    (let* ((display (xlib:window-display window))
           (targets (mapcar (lambda (v)
                              (xlib:atom-name display v))
                            (xlib:get-property window :mcclim)))
           (selected-type (loop
                            named outer
                            for request-type in (clipboard-outstanding-request-type port)
                            for v = (loop
                                      for type in (representation-type-to-native request-type)
                                      when (member type targets)
                                        return type)
                            when v
                              return v)))
      (if selected-type
          (xlib:convert-selection selection selected-type window :mcclim time)
          ;; ELSE: The clipboard doesn't support content of this type, send a negative response here
          nil))))

(defun process-string-reply (port selection content type)
  (declare (ignore selection))
  (when (clipboard-outstanding-request-pane port)
    (let ((event (make-instance 'clim-extensions:clipboard-send-event
                                :content content
                                :sheet (clipboard-outstanding-request-pane port)
                                :type type)))
      (setf (clipboard-outstanding-request-pane port) nil)
      (setf (clipboard-outstanding-request-type port) nil)
      (setf (clipboard-outstanding-request-selection port) nil)
      (queue-event (event-sheet event) event))))

;;;
;;;  Access to the local content
;;;

(defmethod clim-extensions:local-selection-content ((port clx-clipboard-port-mixin))
  (alexandria:if-let ((stored-object (find-stored-object port :primary)))
    (values (clipboard-stored-object-content stored-object)
            (clipboard-stored-object-type stored-object))
    nil))
