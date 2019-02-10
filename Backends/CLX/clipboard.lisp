(in-package :clim-clx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following declarations belongs in core, not in the CLX
;;; backend.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;;  Drei integration
;;;

(clim:define-command (com-yank-from-clipboard :name t :command-table drei:editing-table) ()
  "Insert the contents of the clipboard at point."
  (climi::request-clipboard-content (drei:editor-pane (drei:drei-instance)) :string)
  #+nil
  (handler-case (insert-sequence (point) (kill-ring-yank *kill-ring*))
    (empty-kill-ring ()
      (display-message "Kill ring is empty"))))

(esa:set-key 'com-yank-from-clipboard
             'drei:editing-table
             '((:insert :shift)))

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
                              :accessor clipboard-outstanding-request-type)))

(defun find-stored-object (port selection)
  (case selection
    (:selection (selection-content port))
    (:clipboard (clipboard-content port))))

(defun set-stored-object (port selection value)
  (case selection
    (:selection (setf (selection-content port) value))
    (:clipboard (setf (clipboard-content port) value))))

;;; Event classes

#+nil
(defclass clx-selection-event (window-event)
  ())

#+nil
(defclass clx-selection-notify-event (clx-selection-event)
  ((selection :initarg :selection
              :reader selection-event-selection)
   (target   :initarg :target
             :reader selection-event-target)
   (property :initarg :property
             :reader selection-event-property)))

#+nil
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

(defmethod climi::copy-to-clipboard-with-port ((port clx-clipboard-port-mixin) sheet obj presentation-type)
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
    (xlib:set-selection-owner (xlib:window-display window) :clipboard window nil)
    (let ((success-p (eq (xlib:selection-owner (xlib:window-display window) :clipboard) window)))
      (set-stored-object port
                         :clipboard
                         (if success-p
                             (make-instance 'clx-clipboard-stored-object
                                            :content obj
                                            :type presentation-type
                                            :owner sheet)
                             nil))
      success-p)))

(defun representation-type-to-native (type)
  (case type
    (:string '(:utf8_string :text :string :|text/plain;charset=utf-8| :|text/plain|))
    (:html '(:|text/html|))))

(defun make-targets-response (port content presentation-type)
  (let ((display (clx-port-display port))
        (types (loop
                 for output-type in '(:string :html)
                 when (climi::convert-clipboard-content content output-type :type presentation-type :check-only t)
                   append (representation-type-to-native output-type))))
    (mapcar (lambda (v)
              (xlib:intern-atom display v))
            (remove-duplicates (cons :targets types)))))

(defun process-selection-request (port window target property requestor selection time)
  (let ((display (xlib:window-display window))
        (stored-object (find-stored-object port selection)))
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
               (xlib:change-property requestor property targets target 32)
               (log:info "Sending targets reply: ~s" (mapcar (lambda (v) (xlib:atom-name display v)) targets))
               (send-reply-event)))
            ((:utf8_string :text :string :|text/plain;charset=utf-8| :|text/plain|)
             (let ((content-as-string (climi::convert-clipboard-content content :string :type presentation-type)))
               (xlib:change-property requestor property (babel:string-to-octets content-as-string :encoding :utf-8) target 8)
               (log:info "Sending string reply")
               (send-reply-event)))
            ((:|text/html|)
             (xlib:change-property requestor property
                                   (babel:string-to-octets (format nil "~a~a~a"
                                                                   "<meta http-equiv=\"content-type\" "
                                                                   "content=\"text/html; charset=utf-8\">"
                                                                   (climi::convert-clipboard-content content :html
                                                                                                     :type presentation-type))
                                                           :encoding :utf-8)
                                   target 8)
             (log:info "Sending html reply")
             (send-reply-event))
            (t
             (log:info "Sending negative reply")
             (send-reply-event t)))
          nil)))))

(defun process-selection-notify (port window target property time)
  (case target
    ((:targets)
     (process-targets-reply port window time))
    ((:utf8_string :text :string  :|text/plain;charset=utf-8| :|text/plain|)
     (let ((content (babel:octets-to-string (coerce (xlib:get-property window property) '(vector (unsigned-byte 8)))
                                            :encoding :utf-8)))
       (process-string-reply port content :string)))
    ((:|text/html|)
     (let ((content (babel:octets-to-string (coerce (xlib:get-property window property) '(vector (unsigned-byte 8)))
                                            :encoding :utf-8)))
       (process-string-reply port content :html)))
    (t
     nil)))

(defun process-selection-clear (port selection)
  (set-stored-object port selection nil)
  nil)

(defmethod distribute-event ((port clx-clipboard-port-mixin) (event clx-selection-event))
  (log:info "Redistributing clipboard event")
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

;;; The following methods all use an :AROND method for their
;;; HANDLE-EVENT implementations. This is because we want to
;;; discriminate on the event type only, which means the event will
;;; never be delivered otherwise.
(defmethod handle-event :around (pane (event clx-selection-request-event))
  (log:info "Selection request: ~s" event)
  (process-selection-request (port pane)
                             (sheet-direct-xmirror pane)
                             (selection-event-target event)
                             (selection-event-property event)
                             (selection-event-requestor event)
                             (selection-event-selection event)
                             (event-timestamp event)))

(defmethod handle-event :around (pane (event clx-selection-notify-event))
  (log:info "Selection notify: ~s" event)
  (process-selection-notify (port pane)
                            (sheet-direct-xmirror pane)
                            (selection-event-target event)
                            (selection-event-property event)
                            (event-timestamp event)))

(defmethod handle-event :around (pane (event clx-selection-clear-event))
  (log:info "Selection clear: ~s" event)
  (process-selection-clear (port pane) (selection-event-selection event)))

;;;
;;;  Paste support
;;;

(defmethod climi::request-clipboard-content-from-port ((port clx-clipboard-port-mixin) pane type)
  (check-type type climi::representation-type-name)
  (setf (clipboard-outstanding-request-pane port) pane)
  (setf (clipboard-outstanding-request-type port) type)
  (xlib:convert-selection :clipboard :targets (sheet-direct-xmirror pane) :mcclim nil))

(defun process-targets-reply (port window time)
  (when (clipboard-outstanding-request-pane port)
    (let* ((display (xlib:window-display window))
           (targets (mapcar (lambda (v)
                              (xlib:atom-name display v))
                            (xlib:get-property window :mcclim)))
           (native-representation (representation-type-to-native (clipboard-outstanding-request-type port)))
           (selected-type (loop
                            for type in native-representation
                            when (member type targets)
                              return type)))
      (log:info "Available tagets: ~s, selected: ~s" targets selected-type)
      (if selected-type
          (xlib:convert-selection :clipboard selected-type window :mcclim time)
          ;; ELSE: The clipboard doesn't support content of this type, send a negative response here
          nil))))

(defun process-string-reply (port content type)
  (log:info "Got string reply: type=~s content=~s" type content)
  (when (clipboard-outstanding-request-pane port)
    (let ((event (make-instance 'climi::clipboard-send-event
                                :sheet (clipboard-outstanding-request-pane port)
                                :type (clipboard-outstanding-request-type port))))
      (setf (clipboard-outstanding-request-pane port) nil)
      (setf (clipboard-outstanding-request-type port) nil)
      event)))
