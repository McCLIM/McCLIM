(in-package :clim-clx)

(defclass clx-text-selection-port-mixin ()
  ((selection-owner :initform nil :accessor selection-owner)
   (selection-timestamp :initform nil :accessor selection-timestamp)))

;;;; Backend component of text selection support

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

;;; Conversions

;; we at least want to support:

;;; :TEXT, :STRING
;;;
;;; :UTF8_STRING
;;;    As seen from xterm [make that the preferred encoding]
;;;
;;; :COMPOUND_TEXT
;;;    Perhaps relatively easy to produce, hard to grok.
;;;
;;; :TARGETS
;;;    Clients want legitimately to find out what we support.
;;;
;;; :TIMESTAMP
;;;    Clients want to know when we took ownership of the selection.

;;; Utilities

(defun utf8-string-encode (code-points)
  (let ((res (make-array (length code-points)
                         :adjustable t
                         :fill-pointer 0)))
    (map 'nil
         (lambda (code-point)
           (cond ((< code-point (expt 2 7))
                  (vector-push-extend code-point res))
                 ((< code-point (expt 2 11))
                  (vector-push-extend (logior #b11000000 (ldb (byte 5 6) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 0) code-point)) res))
                 ((< code-point (expt 2 16))
                  (vector-push-extend (logior #b11100000 (ldb (byte 4 12) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 6) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 0) code-point)) res))
                 ((< code-point (1- (expt 2 21)))
                  (vector-push-extend (logior #b11110000 (ldb (byte 3 18) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 12) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 6) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 0) code-point)) res))
                 ((< code-point (1- (expt 2 26)))
                  (vector-push-extend (logior #b11110000 (ldb (byte 2 24) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 18) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 12) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 6) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 0) code-point)) res))
                 ((< code-point (1- (expt 2 31)))
                  (vector-push-extend (logior #b11110000 (ldb (byte 1 30) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 24) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 18) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 12) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 6) code-point)) res)
                  (vector-push-extend (logior #b10000000 (ldb (byte 6 0) code-point)) res))
                 (t
                  (error "Bad code point: ~D." code-point))))
         code-points)
    res))

;;; Protocol functions

(defmethod bind-selection ((port clx-text-selection-port-mixin) window &optional time)
  (xlib:set-selection-owner
   (xlib:window-display (sheet-direct-xmirror window))
   :primary (sheet-direct-xmirror window) time)
  (eq (xlib:selection-owner
       (xlib:window-display (sheet-direct-xmirror window))
       :primary)
      (sheet-direct-xmirror window)))

(defmethod release-selection ((port clx-text-selection-port-mixin) &optional time)
  (xlib:set-selection-owner
   (clx-port-display port)
   :primary nil time)
  (setf (selection-owner port) nil)
  (setf (selection-timestamp port) nil))

(defmethod request-selection ((port clx-text-selection-port-mixin) requestor time)
  (xlib:convert-selection :primary :UTF8_STRING requestor :bounce time))

(defmethod get-selection-from-event ((port clx-text-selection-port-mixin) (event clx-selection-notify-event))
  (if (null (selection-event-property event))
      (progn
        (format *trace-output* "~&;; Oops, selection-notify property is null. Trying the cut buffer instead..~%")
        (xlib:cut-buffer (clx-port-display port)))                
      (let ((v (xlib:get-property (sheet-xmirror (event-sheet event))
                                  (selection-event-property event)
                                  ;; :type :text
                                  :delete-p t
                                  :result-type '(vector (unsigned-byte 8)))))
        (case (clim-clx::selection-event-target event)
          (:string (babel:octets-to-string v :encoding :iso-88519-1))
          (:utf8_string (babel:octets-to-string v :encoding :utf-8))))))

;; Incredibly crappy broken unportable Latin 1 encoder which should be
;; replaced by various implementation-specific versions.
(flet ((latin1-code-p (x)
	 (not (or (< x 9) (< 10 x 32) (< #x7f x #xa0) (> x 255)))))
  (defun string-encode (string)
    (delete-if-not #'latin1-code-p (map 'vector #'char-code string)))
  (defun exactly-encodable-as-string-p (string)
    (every #'latin1-code-p (map 'vector #'char-code string))))

;;; TODO: INCR property?
;;;
;;; FIXME: per ICCCM we MUST support :MULTIPLE
(defmethod send-selection ((port clx-text-selection-port-mixin) (event clx-selection-request-event) string)
  (let ((requestor (selection-event-requestor event))
        (property  (selection-event-property event))
        (target    (selection-event-target event))
        (time      (event-timestamp event)))
    (when (null property)
      (format *trace-output* "~&* Requestor property is null! *~%"))
    (flet ((send-event (&key target (property property))
	     ;; debugging output, but the KDE Klipper client turns out
	     ;; to poll other clients for selection, which means it
	     ;; would be bad to print at every request.
             (xlib:send-event requestor
			      :selection-notify nil
			      :window requestor
			      :event-window requestor
			      :selection (climi::selection-event-selection event)
			      :target target
			      :property property
			      :time time)))
      (case target
	((:UTF8_STRING)
	 (xlib:change-property requestor property
			       (utf8-string-encode
				(map 'vector #'char-code string))
			       :UTF8_STRING 8)
	 (send-event :target :UTF8_STRING))
	((:STRING :COMPOUND_TEXT)
	 (xlib:change-property requestor property
			       (string-encode string)
			       target 8)            
	 (send-event :target target))
	((:TEXT)
	 (cond
	   ((exactly-encodable-as-string-p string)
	    (xlib:change-property requestor property
				  (string-encode string)
				  :STRING 8)
	    (send-event :target :STRING))
	   (t 
	    (xlib:change-property requestor property
				  (utf8-string-encode
				   (map 'vector #'char-code string))
				  :UTF8_STRING 8)
	    (send-event :target :UTF8_STRING))))
	((:TARGETS)
	 (let* ((display (clx-port-display port))
		(targets (mapcar (lambda (x) (xlib:intern-atom display x))
				 '(:TARGETS :STRING :TEXT :UTF8_STRING
				   :COMPOUND_TEXT :TIMESTAMP))))
	   (xlib:change-property requestor property targets target 32))
	 (send-event :target :TARGETS))
	((:TIMESTAMP)
	 (when (null (selection-timestamp port))
	   (format *trace-output* "~&;; selection-timestamp is null!~%"))
	 (xlib:change-property requestor property
			       (list (selection-timestamp port))
			       target 32)
	 (send-event :target :TIMESTAMP))
	(t
	 (format *trace-output*
		 "~&;; Warning, unhandled type \"~A\". ~
                  Sending property NIL to target.~%" target)
	 (send-event :target target :property nil))))
    (xlib:display-force-output (xlib:window-display requestor))))
