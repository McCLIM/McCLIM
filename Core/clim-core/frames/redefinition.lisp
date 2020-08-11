;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2019, 2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Update application frame instance when application frame class is
;;; redefined.

(in-package #:clim-internals)

;;; Frame class redefinition notification protocol
;;;
;;; This is used for notifying application frame classes after aspects
;;; such as the class itself, the `generate-panes' method or the
;;; command table have been redefined. The notification sequence
;;; begins with a call to `note-frame-class-redefined' which either
;;; directly calls `handle-frame-class-redefinition' or schedules the
;;; call to be made when leaving a
;;; `with-delayed-redefinition-notifications'.
;;;
;;; This protocol is probably most commonly initiated by re-evaluating
;;; the `define-application-frame' form that defined the frame class
;;; in question, but any event that causes the `reinitialize-instance'
;;; method of the class to execute also triggers it. In the context of
;;; `define-application-frame',
;;; `with-delayed-redefinition-notifications' delays the
;;; `note-frame-class-redefinition' call until after all aspects
;;; (`generate-panes' method, command table definition, etc) have been
;;; executed.

(defgeneric handle-frame-class-redefinition (class)
  (:method ((class t))))

(defvar *delayed-redefinition-notifications*)

(defun invoke-with-delayed-redefinition-notifications (thunk)
  (if (boundp '*delayed-redefinition-notifications*)
      (funcall thunk)
      (let ((*delayed-redefinition-notifications* '()))
        (multiple-value-prog1
            (funcall thunk)
          (mapc #'handle-frame-class-redefinition
                *delayed-redefinition-notifications*)))))

(defmacro with-delayed-redefinition-notifications (() &body body)
  `(invoke-with-delayed-redefinition-notifications (lambda () ,@body)))

(defun note-frame-class-redefined (class)
  (if (boundp '*delayed-redefinition-notifications*)
      (pushnew class *delayed-redefinition-notifications* :test #'eq)
      (handle-frame-class-redefinition class)))

;;; `frame-class-redefined-event'
;;;
;;; Sent to frame instances whose class has been redefined. Causes the
;;; following actions in the frame receiving the event:
;;;
;;; 1) A new command table is installed in the frame.
;;;
;;; 2) The current layout of the frame is reset to the first layout in
;;;    the layout list of the frame.
;;;
;;; 3) Panes are reinitialized where possible or else regenerated for
;;;    the potentially changed layout.

(define-event-class frame-class-redefined-event (standard-event)
  ((%new-pretty-name   :initarg :new-pretty-name
                       :reader  new-pretty-name)
   (%new-layouts       :initarg :new-layouts
                       :reader  new-layouts)
   (%new-command-table :initarg :new-command-table
                       :reader  new-command-table)))

(defmethod handle-event ((client application-frame)
                         (event frame-class-redefined-event))
  ;; We try to preserve the frame size. Store the current size before
  ;; we make any changes.
  (multiple-value-bind (width height)
      (bounding-rectangle-size (frame-top-level-sheet client))
    (setf (frame-pretty-name client) (new-pretty-name event)
          (frame-command-table client) (new-command-table event)
          (%frame-layouts client) (new-layouts event))
    (let ((new-layout (first (frame-all-layouts client))))
      ;; Changing the layout involves signaling FRAME-LAYOUT-CHANGED
      ;; which performs a non-local exit out of this method.
      (if (eq (frame-current-layout client) new-layout)
          (when-let ((frame-manager (frame-manager client)))
            (generate-panes frame-manager client)
            (layout-frame client width height)
            (signal 'frame-layout-changed :frame client))
          (unwind-protect ; Call LAYOUT-FRAME despite non-local exit.
               (setf (frame-current-layout client) new-layout)
            (layout-frame client width height))))))

;;; `redefinition-updates-instances-class'
;;;
;;; A metaclass for `application-frame' subclasses that update their
;;; instances when redefined.
;;;
;;; Such a subclasses tracks created instances via a list of weak
;;; pointers stored in the class. When the class is redefined, a
;;; `frame-class-redefined-event' is dispatched to each frame in that
;;; list (unless the frame has become garbage in the meantime).

(defclass redefinition-updates-instances-class (standard-class)
  ((%instance-lock :accessor instance-lock
                   :initform (make-lock "instance lock"))
   (%instances     :accessor instances
                   :initform '())))

(defmethod c2mop:validate-superclass
    ((class      redefinition-updates-instances-class)
     (superclass standard-class))
  t)

(defmethod shared-initialize :before
    ((instance   redefinition-updates-instances-class)
     (slot-names t)
     &key (direct-superclasses nil direct-superclasses-supplied-p))
  (when direct-superclasses-supplied-p
    (unless (some (alexandria:rcurry #'subtypep 'application-frame)
                  direct-superclasses)
      (error "~@<Instances of ~S must be subclasses of ~S.~@:>"
             'redefinition-updates-instances-class 'application-frame))))

(defmethod reinitialize-instance :after
    ((instance redefinition-updates-instances-class) &key)
  ;; Trigger redefinition notification protocol which will directly
  ;; call `handle-frame-class-redefinition' or schedule it for later.
  (note-frame-class-redefined instance))

(defmethod make-instance :around ((class redefinition-updates-instances-class)
                                  &key)
  ;; Store a weak pointer to the created instance for updating it when
  ;; CLASS is redefines.
  (let ((instance (call-next-method)))
    (with-lock-held ((instance-lock class))
      (push (tg:make-weak-pointer instance) (instances class)))
    instance))

(defmethod handle-frame-class-redefinition
    ((class redefinition-updates-instances-class))
  (let ((remaining         '())
        (new-pretty-name   nil)
        (new-layouts       nil)
        (new-command-table nil))
    (labels ((initarg-value (name)
               (when-let* ((initargs (c2mop:class-default-initargs class))
                           (initarg  (find name initargs :key #'first)))
                 (funcall (third initarg))))
             (new-pretty-name ()
               (or new-pretty-name
                   (setf new-pretty-name (initarg-value :pretty-name))))
             (new-layouts ()
               (or new-layouts
                   (setf new-layouts (initarg-value :layouts))))
             (new-command-table ()
               (or new-command-table
                   (setf new-command-table (initarg-value :command-table))))
             (update (instance)
               (with-simple-restart (continue "~@<Skip updating frame ~A~@:>"
                                              instance)
                 (event-queue-append
                  (frame-event-queue instance)
                  (make-instance 'frame-class-redefined-event
                                 :sheet             instance
                                 :new-pretty-name   (new-pretty-name)
                                 :new-layouts       (new-layouts)
                                 :new-command-table (new-command-table)))))
             (maybe-update (weak-instance)
               ;; Update instances that haven't become garbage and
               ;; haven't been CHANGE-CLASSed. Implicitly drop other
               ;; instances.
               (when-let ((live-instance (tg:weak-pointer-value weak-instance)))
                 (when (typep live-instance class)
                   (push weak-instance remaining)
                   (update live-instance)))))
      (with-lock-held ((instance-lock class))
        (mapc #'maybe-update (instances class))
        (setf (instances class) remaining)))))
