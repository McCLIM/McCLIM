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

(defgeneric process-clipboard-event (port window event-key target property requestor selection time)
  (:method (port window event-key target property requestor selection time)
    ;; Default implementation doesn't support clipboard
    nil))

(defclass clx-clipboard-port-mixin ()
  ((clipboard-window :initform nil)
   (clipboard-content :initform nil
                      :accessor clipboard-content)))

(defmethod climi::copy-to-clipboard ((port clx-clipboard-port-mixin) obj)
  (let ((window (clipboard-window port)))
    ;; We're not actually supposed to call set-selection-owner without
    ;; a timestamp due to the follow statemnt in ICCCM:
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
      (setf (clipboard-content port) (if success-p obj nil))
      success-p)))

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

(defun representation-type-to-native (type)
  (case type
    (:string '(:utf8_string :text :string :|text/plain;charset=utf-8| :|text/plain|))
    (:html '(:|text/html|))))

(defun make-targets-response (port)
  (let ((display (clx-port-display port))
        (types (loop
                 for type in '(:string :html)
                 when (climi::representation-type-supported-p (clipboard-content port) type)
                   append (representation-type-to-native type))))
    (mapcar (lambda (v)
              (xlib:intern-atom display v))
            (remove-duplicates (cons :targets types)))))

(defun process-selection-request (port window target property requestor selection time)
  (let ((display (xlib:window-display window))
        (content (clipboard-content port)))
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
         (let ((targets (make-targets-response port)))
           (xlib:change-property requestor property targets target 32)
           (log:info "Sending targets reply: ~s" (mapcar (lambda (v) (xlib:atom-name display v)) targets))
           (send-reply-event)))
        ((:utf8_string :text :string :|text/plain;charset=utf-8| :|text/plain|)
         (let ((content-as-string (convert-clipboard-object content :string)))
           (xlib:change-property requestor property (babel:string-to-octets content-as-string :encoding :utf-8) target 8)
           (log:info "Sending string reply")
           (send-reply-event)))
        ((:|text/html|)
         (xlib:change-property requestor property
                               (babel:string-to-octets (format nil "~a~a~a"
                                                               "<meta http-equiv=\"content-type\" "
                                                               "content=\"text/html; charset=utf-8\">"
                                                               (convert-clipboard-object content :html))
                                                       :encoding :utf-8)
                               target 8)
         (log:info "Sending html reply")
         (send-reply-event))
        (t
         (log:info "Sending negative reply")
         (send-reply-event t)))
      nil)))

(defun process-selection-notify (port window target property requestor selection time)
  (case target
    ((:targets)
     (process-targets-reply port window target property requestor selection time))
    ((:utf8_string :text :string  :|text/plain;charset=utf-8| :|text/plain|)
     (let ((content (babel:octets-to-string (coerce (xlib:get-property window property) '(vector (unsigned-byte 8)))
                                            :encoding :utf-8)))
       (process-string-reply content :string)))
    ((:|text/html|)
     (let ((content (babel:octets-to-string (coerce (xlib:get-property window property) '(vector (unsigned-byte 8)))
                                            :encoding :utf-8)))
       (process-string-reply content :html)))
    (t
     nil)))

(defun process-selection-clear (port)
  (setf (clipboard-content port) nil)
  nil)

(defmethod process-clipboard-event ((port clx-clipboard-port-mixin) window event-key target property requestor selection time)
  (when (eq window (clipboard-window port))
    (log:info "Clipboard event: key=~s target=~s prop=~s req=~s sel=~s time=~s" event-key target property requestor selection time)
    (ecase event-key
      (:selection-request (process-selection-request port window target property requestor selection time))
      (:selection-notify (process-selection-notify port window target property requestor selection time))
      (:selection-clear (process-selection-clear port)))))

;;;
;;;  Paste support
;;;

(defvar *outstanding-request* nil)

(defmethod climi::request-clipboard-content-from-port ((port clx-clipboard-port-mixin) pane type)
  (check-type type climi::representation-type-name)
  (setq *outstanding-request* (list pane type))
  (xlib:convert-selection :clipboard :targets (clipboard-window port) :mcclim nil))

(defun process-targets-reply (port window target property requestor selection time)
  (declare (ignore target property requestor selection))
  (when *outstanding-request*
    (let* ((display (xlib:window-display window))
           (targets (mapcar (lambda (v)
                              (xlib:atom-name display v))
                            (xlib:get-property window :mcclim)))
           (native-representation (representation-type-to-native (second *outstanding-request*)))
           (selected-type (loop
                            for type in native-representation
                            when (member type targets)
                              return type)))
      (log:info "Available tagets: ~s, selected: ~s" targets selected-type)
      (if selected-type
          (xlib:convert-selection :clipboard selected-type (clipboard-window port) :mcclim time)
          ;; ELSE: The clipboard doesn't support content of this type, send a negative response here
          nil))))

(defun process-string-reply (content type)
  (log:info "Got string reply: type=~s content=~s" type content)
  (when *outstanding-request*
    (let ((event (make-instance 'climi::clipboard-send-event
                                :sheet (first *outstanding-request*)
                                :type (second *outstanding-request*))))
      (setq *outstanding-request* nil)
      event)))
