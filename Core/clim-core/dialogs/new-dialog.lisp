(in-package #:clim-internals)

(define-application-frame accept-values
    (standard-application-frame standard-encapsulating-stream)
  ((queries :accessor queries :initform (make-hash-table :test #'equal))
   ;; This is an ordered sequence used by next/prev commands.
   (queries* :accessor queries* :initform (make-array 0 :adjustable t :fill-pointer t))
   (selected-query :accessor selected-query :initform nil)
   (accepting-query :accessor accepting-query :initform nil)
   (continuation :reader continuation :initarg :continuation)
   (align-prompts :reader align-prompts :initarg :align-prompts :initform nil))
  (:pane :application
   :display-function 'display-accept-values
   :incremental-redisplay t))

(add-menu-item-to-command-table 'accept-values "Accepting Values"
                                :divider nil
                                :text-style (make-text-style nil :italic :normal)
                                :errorp nil)

(defclass accepting-values-query ()
  ((query-identifier :reader query-identifier :initarg :query-identifier)
   (query-ptype :accessor query-ptype :initarg :query-ptype)
   (query-record :accessor query-record :initarg :query-record)
   ;;
   (query-default-value :accessor query-default-value :initarg :query-default-value)
   (query-default-ptype :accessor query-default-ptype :initarg :query-default-ptype)
   (query-current-value :accessor query-current-value :initarg :query-current-value)
   (query-current-ptype :accessor query-current-ptype :initarg :query-current-ptype)
   ;;
   (invalidp :accessor query-invalid-p :initform nil)
   (changedp :accessor query-changed-p :initform nil)
   ;; debug
   (counter :accessor counter :initform 0))
  (:default-initargs
   :query-identifier    (alexandria:required-argument :query-identifier)
   :query-ptype         (alexandria:required-argument :query-ptype)
   ;; :query-record        (alexandria:required-argument :query-record)
   :query-default-value (alexandria:required-argument :query-default-value)
   :query-default-ptype (alexandria:required-argument :query-default-ptype)
   :query-current-value (alexandria:required-argument :query-current-value)
   :query-current-ptype (alexandria:required-argument :query-current-ptype)))

(defvar *accepting-values-stream* nil)

;;; FIXME shouldn't we redisplay the parent output record?
(define-accept-values-command (com-edit-query :keystroke :return)
    ()
  (when-let* ((av *accepting-values-stream*)
              (query (selected-query av)))
    (let ((stream (encapsulating-stream-stream av)))
      (letf (((accepting-query av) query))
        (redisplay-output-record (query-record query) stream)
        (handler-case
            (progn
              (accept (query-ptype query) :stream (accepting-query av) :prompt nil)
              (setf (query-invalid-p query) nil))
          (error (c)
            (format *debug-io* "error is ~a~%" c)
            (setf (query-invalid-p query) c)))
        (format *debug-io* "~s" (coerce (stream-input-buffer (accepting-query av))
                                         'string)))
      (redisplay-output-record (query-record query) stream))))

(define-accept-values-command (com-select-query)
    ((arg 'accepting-values-query :gesture :select))
  (when-let ((av *accepting-values-stream*))
    (setf (selected-query av) arg)))

(define-accept-values-command (com-deselect-query)
    ()
  (when-let ((av *accepting-values-stream*))
    (setf (selected-query av) nil)))

(define-presentation-to-command-translator tr-deselect-query
    (blank-area com-deselect-query* accept-values) (object)
  '())

(define-accept-values-command (com-next-query :keystroke (#\n :meta))
    ()
  (when-let ((av *accepting-values-stream*))
    (let* ((selected-query (selected-query av))
           (all-queries (coerce (queries* av) 'list))
           (member-query (member selected-query all-queries)))
      (assert (or (null selected-query) member-query))
      (setf (selected-query av)
            (cond ((null selected-query)
                   (first all-queries))
                  ((rest member-query)
                   (second member-query))
                  (t
                   (first all-queries)))))))

(define-accept-values-command (com-prev-query :keystroke (#\p :meta))
    ()
  (when-let ((av *accepting-values-stream*))
    (let* ((selected-query (selected-query av))
           (all-queries (reverse (coerce (queries* av) 'list)))
           (member-query (member selected-query all-queries)))
      (assert (or (null selected-query) member-query))
      (setf (selected-query av)
            (cond ((null selected-query)
                   (first all-queries))
                  ((rest member-query)
                   (second member-query))
                  (t
                   (first all-queries)))))))

;;; This method is used to intercept calls to ACCEPT create when necessary the
;;; intermediate query object. The object maintains the state and the output
;;; record of the accept form across the accepting values loop iterations.
;;; 
;;; Creating a query also adds an updating output record that initially
;;; displays ACCEPT-PRESENT-DEFAULT. We can't call eagerly ACCEPT-1 to avoid
;;; blocking the execution of the continuation. Only after that we redisplay
;;; the selected record to ACCEPT-1 input from the user.
(defmethod stream-accept ((stream accept-values) ptype &rest args
                          &key default default-type &allow-other-keys)
  (assert (presentation-subtypep default-type ptype))
  (when (and (null default-type) (presentation-typep default ptype))
    (setf default-type ptype))
  (let ((query (apply #'ensure-query stream ptype args)))
    (values (query-current-value query)
            (query-current-ptype query)
            (query-changed-p query))))

;;; It is possible that the query properties change and then the object must
;;; be updated accordingly.
(defun ensure-query (stream ptype &rest args
                     &key query-identifier default default-type
                       (view +textual-view+) &allow-other-keys)
  (check-type stream accept-values)
  ;; This assert is to ensure, that in the body there are no two accept
  ;; invocations with the same QUERY-IDENTIFIER.
  ;; FIXME signal a proper error.
  (assert (not (find query-identifier (queries* stream)
                     :test #'equal :key #'query-identifier)))  
  (multiple-value-bind (query presentp)
      (ensure-gethash query-identifier (queries stream)
                      (make-instance 'accepting-values-query
                                     :query-ptype ptype
                                     :query-identifier query-identifier
                                     :query-default-ptype default-type
                                     :query-default-value default
                                     :query-current-ptype default-type
                                     :query-current-value default))
    ;; We maintain a separate sequence for queries to maintains their order
    ;; for the keyboard navigation (next/previous query). The fill pointer is
    ;; re-set before each iteration.
    (vector-push-extend query (queries* stream))
    (when presentp
      (setf (query-default-ptype query) default-type
            (query-default-value query) default)
      (unless (presentation-subtypep (query-current-ptype query) ptype)
        (setf (query-current-ptype query) default-type
              (query-current-value query) default
              (query-changed-p query) t
              (query-invalid-p query) nil)))
    (let* ((selected (selected-query stream))
           (selectedp (equal selected query))
           (real-stream (encapsulating-stream-stream stream)))
      (setf (query-record query)
            (updating-output (real-stream :unique-id query-identifier
                                          :cache-value selectedp ;foo
                                          :cache-test #'eql)
              (format stream "[~s] " (counter query))
              (let ((accepting (accepting-query stream)))
                (with-output-as-presentation
                    (real-stream query 'accepting-values-query :single-box t)
                  (with-drawing-options (stream :ink (if (query-invalid-p query)
                                                         +dark-red+
                                                         +foreground-ink+)
                                                :text-face (if selectedp
                                                               :bold
                                                               :roman))
                    (apply #'prompt-for-accept stream ptype view :display-default nil args)
                    (if accepting
                        (setf (accepting-query stream)
                              (make-instance 'standard-input-editing-stream
                                             :stream real-stream
                                             :single-line t))
                        ;; (princ "..." stream)
                        (accept-present-default* ptype stream view
                                                 (query-current-value query)
                                                 (query-current-ptype query) ;not null
                                                 query query-identifier))))))))
    query))

;;; The parameter present-p is not documented in the specification. We
;;; use it internally to pass the query object that contains the current
;;; value, the record etc. Less sophisticated methods may ignore it.
;;;
;;; When user edits the query then we pass its presentation type and
;;; value as the presentation-type and default parameters. This method
;;; may be ignorant whether it is really the "default" value.
(defun accept-present-default* (ptype stream view
                                default default-supplied-p
                                present-p query-identifier)
  (declare (ignorable ptype stream view
                      default default-supplied-p
                      present-p query-identifier))
  (incf (counter present-p))
  (if default-supplied-p
      (present default ptype :stream stream :view view)
      (with-text-face (stream :italic)
        (describe-presentation-type (query-ptype present-p) stream 1))))

(defun display-accept-values (frame stream)
  (updating-output (stream)
    (setf (slot-value frame 'stream) stream)
    (setf (fill-pointer (queries* frame)) 0)
    (funcall (continuation frame) frame)))

(defun accept-values-top-level (frame)
  (let* ((*command-parser* #'menu-command-parser)
         (*command-unparser* nil)
         (*partial-command-parser* nil)
         (stream (encapsulating-stream-stream frame))
         (needs-redisplay t)
         (av-record (updating-output (stream)
                      (display-accept-values frame stream))))
    (handler-case
        (restart-case
            (flet ((execute-command ()
                     (when-let ((command (read-frame-command frame :stream frame)))
                       (setq needs-redisplay t)
                       (execute-frame-command frame command))))
              (loop
                (when needs-redisplay
                  (redisplay-output-record av-record stream nil)
                  (setq needs-redisplay nil))
                (execute-command)))
          (abort ()
            :report "Return from ACCEPTING-VALUES loop."))
      (abort-gesture (c)
        (declare (ignore c))))))

(defun invoke-accepting-values (stream continuation &rest args
                                &key own-window &allow-other-keys)
  (declare (ignore args))
  (let* ((frame (make-application-frame 'accept-values :continuation continuation
                                                       :stream stream))
         (*accepting-values-stream* frame))
    (if own-window
        (run-frame-top-level frame)
        (letf (((frame-command-table (pane-frame stream)) 'accept-values))
          (accept-values-top-level frame)))))

(defmacro accepting-values ((&optional (stream t) &rest args)
                            &body body)
  (setq stream (stream-designator-symbol stream '*standard-input*))
  (with-gensyms (fun)
    `(flet ((,fun (,stream) ,@body))
       (invoke-accepting-values ,stream (function ,fun) ,@args))))


;;; test code

(defun display-test-frame (frame stream)
  (declare (ignore frame))
  (format stream "This is a frame!~%~%"))

(define-application-frame test-frame ()
  ()
  (:pane :application :display-function 'display-test-frame))

(define-test-frame-command (com-ask :menu t :keystroke (#\n :control)) ()
  (let ((stream *standard-output*))
    (accepting-values (stream)
      (accept 'integer :stream stream :query-identifier :a :default 42)
      (terpri stream)
      (accept 'string  :stream stream :query-identifier :b)
      (terpri stream)
      (accept '(integer 3 4)  :stream stream :query-identifier :c)
      (terpri stream))))

;; #+mcclim

;; (find-application-frame 'test-frame)

