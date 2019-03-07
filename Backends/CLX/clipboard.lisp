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
  (ecase selection
    (:primary (selection-content port))
    (:clipboard (clipboard-content port))))

(defun set-stored-object (port selection value)
  (ecase selection
    (:primary (setf (selection-content port) value))
    (:clipboard (setf (clipboard-content port) value))))

;;; Main entry point

(defun clx-copy-to-clipboard (port sheet selection obj presentation-type)
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
    (xlib:set-selection-owner (xlib:window-display window) selection window nil)
    (let ((success-p (eq (xlib:selection-owner (xlib:window-display window) selection) window)))
      (set-stored-object port selection
                         (if success-p
                             (make-instance 'clx-clipboard-stored-object
                                            :content obj
                                            :type presentation-type
                                            :owner sheet)
                             nil))
      success-p)))

(defmethod climb:copy-to-selection ((port clx-clipboard-port-mixin) sheet obj &key presentation-type)
  (clx-copy-to-clipboard port sheet :primary obj (or presentation-type (clim:presentation-type-of obj))))

(defmethod climb:copy-to-clipboard ((port clx-clipboard-port-mixin) sheet obj &key presentation-type)
  (clx-copy-to-clipboard port sheet :clipboard obj (or presentation-type (clim:presentation-type-of obj))))

(defun clx-clear-clipboard (port sheet selection)
  (let ((window (sheet-direct-xmirror sheet)))
    (alexandria:when-let ((stored-object (find-stored-object port selection)))
      (when (eq (clipboard-stored-object-owner stored-object) sheet)
        (xlib:set-selection-owner (xlib:window-display window) selection nil nil)
        (set-stored-object port selection nil)))))

(defmethod clear-selection ((port clx-clipboard-port-mixin) sheet)
  (clx-clear-clipboard port sheet :primary))

(defmethod climb:clear-clipboard ((port clx-clipboard-port-mixin) sheet)
  (clx-clear-clipboard port sheet :clipboard))

(defun representation-type-to-native (type)
  (case type
    (:string '(:utf8_string :text :string :|text/plain;charset=utf-8| :|text/plain|))
    (:html '(:|text/html|))
    (:image '(:|image/png| :|image/jpeg|))))

(defun make-targets-response (port content presentation-type)
  (let ((display (clx-port-display port))
        (types (loop
                 for output-type in '(:string :html)
                 when (clime:convert-clipboard-content content output-type :type presentation-type :check-only t)
                   append (representation-type-to-native output-type))))
    (mapcar (lambda (v)
              (xlib:intern-atom display v))
            (remove-duplicates (cons :targets types)))))

(defun process-selection-request (port window target property requestor selection time)
  (alexandria:when-let ((stored-object (find-stored-object port selection)))
    (when (eq (sheet-direct-xmirror (clipboard-stored-object-owner stored-object)) window)
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
             (let ((content-as-string (clime:convert-clipboard-content content :string :type presentation-type)))
               (xlib:change-property requestor property (babel:string-to-octets content-as-string :encoding :utf-8) :string 8)
               (send-reply-event)))
            ((:|text/html|)
             (let ((s (format nil "<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\">~a"
                              (clime:convert-clipboard-content content :html :type presentation-type))))
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
      (make-instance 'clime:image-pattern :array (climi::convert-opticl-img image-content)))))

(defun process-selection-notify (port selection target property time)
  (alexandria:when-let ((outstanding-request-pane (clipboard-outstanding-request-pane port)))
    (when (eq (clipboard-outstanding-request-selection port) selection)
      (let ((window (sheet-direct-xmirror outstanding-request-pane)))
        (case target
          ((:targets)
           (process-targets-reply port window selection time))
          ((:utf8_string :text :string  :|text/plain;charset=utf-8| :|text/plain|)
           (let ((content (babel:octets-to-string (coerce (xlib:get-property window property) '(vector (unsigned-byte 8)))
                                                  :encoding :utf-8)))
             (process-clipboard-reply port selection content :string)))
          ((:|text/html|)
           (let ((content (babel:octets-to-string (coerce (xlib:get-property window property) '(vector (unsigned-byte 8)))
                                                  :encoding :utf-8)))
             (process-clipboard-reply port selection content :html)))
          ((:|image/png|)
           (let ((image (convert-image-array (coerce (xlib:get-property window property) '(vector (unsigned-byte 8))) :png)))
             (process-clipboard-reply port selection image :image)))
          ((:|image/jpeg|)
           (let ((image (convert-image-array (coerce (xlib:get-property window property) '(vector (unsigned-byte 8))) :jpeg)))
             (process-clipboard-reply port selection image :image)))
          (t
           nil))))))

(defun process-selection-clear (port selection)
  (set-stored-object port selection nil))

;;;
;;;  Paste support
;;;
;;;  When a client requests the content of the clipboard or selection,
;;;  a TYPE parameter is used to indicate what type the caller expects
;;;  to receive. This value is a keyword (or list of keywords) that
;;;  includes the values :STRING, :HTML and :IMAGE.
;;;
;;;  When the function REQUEST-SELECTION-CONTENT or
;;;  REQUEST-CLIPBOARD-CONTENT arrives, the requestor information is
;;;  stored in the port, and a TARGETS request is then sent to the
;;;  selection owner. As per the ICCCM, when a client receives a
;;;  TARGETS request, it should return a list of supported types.
;;;
;;;  When the TARGETS response arrives, the list of available types is
;;;  compared with the list of requested types. If there is no match,
;;;  no further processis occurs. If any types match, the first match
;;;  is chosen and another request is sent to the selection owner with
;;;  the type set to the matched type. Once this response arrives, it
;;;  is delivered as an event to the pane that performed the request.

(defun clx-request-clipboard-content (port pane selection type)
  (setf (clipboard-outstanding-request-pane port) pane)
  (setf (clipboard-outstanding-request-type port) (alexandria:ensure-list type))
  (setf (clipboard-outstanding-request-selection port) selection)
  (xlib:convert-selection selection :targets (sheet-direct-xmirror pane) :mcclim nil))

(defmethod climb:request-selection-content ((port clx-clipboard-port-mixin) pane type)
  (clx-request-clipboard-content port pane :primary type))

(defmethod climb:request-clipboard-content ((port clx-clipboard-port-mixin) pane type)
  (clx-request-clipboard-content port pane :clipboard type))

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

(defun process-clipboard-reply (port selection content type)
  (declare (ignore selection))
  (when (clipboard-outstanding-request-pane port)
    (let ((event (make-instance 'clime:clipboard-send-event
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

(defmethod climb:local-selection-content ((port clx-clipboard-port-mixin))
  (alexandria:if-let ((stored-object (find-stored-object port :primary)))
    (values (clipboard-stored-object-content stored-object)
            (clipboard-stored-object-type stored-object))
    nil))
