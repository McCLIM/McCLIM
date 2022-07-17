;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 1998-2001 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) Copyright 2000 by Iban Hatchondo <hatchond@emi.u-bordeaux.fr>
;;;  (c) Copyright 2000 by Julien Boninfante <boninfan@emi.u-bordeaux.fr>
;;;  (c) Copyright 2000,2014 by Robert Strandh <robert.strandh@gmail.com>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-internals)

;;; Server path and global port registry

(defvar *default-server-path* nil)

;;; - CLX is the de-facto reference backend. We have few flavours of
;;;   it where the one using Lisp TTF renderer implementation and
;;;   Xrender extensions is default.
;;;
;;; - Null are in this list mostly to document its existence, and is
;;;   not currently a complete backend we would want to make a
;;;   default.  Put it after CLX, so that it won't actually be
;;;   reached.
(defvar *server-path-search-order*
  '(#.(cond ((member :mcclim-ffi-freetype *features*) :clx-ff)
            ((member :mcclim-clx-fb       *features*) :clx-fb)
            ((member :mcclim-ugly         *features*) :clx)
            (t :clx-ttf))
    :null))

(defmethod find-port-type ((port symbol))
  (values (get port :port-type)
          (get port :server-path-parser)))

(defvar *all-ports* nil)
(defvar *all-ports-lock* (make-lock "All ports lock."))

(defun find-port (&key (server-path *default-server-path*))
  (setf server-path (alexandria:ensure-list server-path))
  (let (port-class)
    (flet ((try-port (type path)
             (multiple-value-bind (class parser) (find-port-type type)
               (when class
                 (setf port-class class
                       server-path (if (null parser)
                                       path
                                       (funcall parser path)))))))
      (if (null server-path)
          (loop for port-type in *server-path-search-order*
                do (try-port port-type (list port-type))
                while (null port-class))
          (try-port (first server-path) server-path))
      (when (null port-class)
        (if server-path
            (error "No CLIM backends have been loaded!~%Server-path: ~s." server-path)
            (error "No CLIM backends have been loaded!")))
      (with-lock-held (*all-ports-lock*)
        (if-let ((port (find server-path *all-ports* :test #'equal :key #'port-server-path)))
          (values port t)
          (values (first (push (make-instance port-class :server-path server-path) *all-ports*))
                  nil))))))

(defmacro with-port ((port-var server &rest args &key &allow-other-keys)
                     &body body)
  `(invoke-with-port (lambda (,port-var) ,@body) ,server ,@args))

(defun invoke-with-port (continuation server &rest args &key &allow-other-keys)
  (multiple-value-bind (port foundp)
      (find-port :server-path (list* server args))
    (if foundp
        (funcall continuation port)
        (unwind-protect (funcall continuation port)
          (destroy-port port)))))

;;; Basic port

(defclass basic-port (port)
  ((server-path :initform nil
                :initarg :server-path
                :reader port-server-path)
   (properties :initform nil
               :initarg :properties)
   (grafts :initform nil
           :accessor port-grafts)
   (frame-managers :initform nil
                   :accessor frame-managers)
   (event-process
    :initform nil
    :initarg  :event-process
    :accessor port-event-process
    :documentation "In a multiprocessing environment, the particular process
                    reponsible for calling PROCESS-NEXT-EVENT in a loop.")
   (lock
    :initform (make-recursive-lock "port lock")
    :accessor port-lock)
   (text-style-mappings :initform (make-hash-table :test #'eq)
                        :reader port-text-style-mappings)
   (focused-sheet :initform nil :accessor port-focused-sheet
                  :reader port-keyboard-input-focus
                  :documentation "The sheet for the keyboard events, if any")
   (pointer :initform nil :initarg :pointer :accessor port-pointer
            :documentation "The pointer of the port")
   (cursors :initform (make-hash-table) :reader port-cursors)
   (selections :initform (make-hash-table) :reader port-selections)
   ;; The difference between grabbed-sheet and pressed-sheet is that
   ;; the former takes all pointer events while pressed-sheet receives
   ;; replicated pointer motion events. -- jd 2019-08-21
   ;;
   ;; If GRABBED-SHEET is T, then WITH-POINTER-GRABBED was invoked with
   ;; :MULTIPLE-WINDOW T, that is events should be delivered to their owner.
   ;; -- jd 2020-11-02
   (grabbed-sheet :initform nil :accessor port-grabbed-sheet
                  :documentation "The sheet the pointer is grabbing, if any")
   (pressed-sheet :initform nil :accessor port-pressed-sheet
                  :documentation "The sheet the pointer is pressed on, if any")))

(defmethod note-input-focus-changed (sheet state)
  (declare (ignore sheet state)))

(defmethod (setf port-keyboard-input-focus)
    ((top-sheet top-level-sheet-mixin) (port basic-port))
  (let ((new-sheet (focused-sheet top-sheet))
        (old-sheet (port-keyboard-input-focus port)))
    (unless (eq new-sheet old-sheet)
      (setf (port-focused-sheet port) new-sheet)
      (note-input-focus-changed old-sheet nil)
      (note-input-focus-changed new-sheet t))))

(defmethod (setf port-keyboard-input-focus)
    ((new-sheet sheet) (port basic-port))
  (let ((old-sheet (port-keyboard-input-focus port)))
    (unless (eq new-sheet old-sheet)
      (setf (port-focused-sheet port) new-sheet)
      (when-let ((top-sheet (get-top-level-sheet new-sheet)))
        (setf (focused-sheet top-sheet) new-sheet))
      (note-input-focus-changed old-sheet nil)
      (note-input-focus-changed new-sheet t))))

(defmethod destroy-port :before ((port basic-port))
  (when (and *multiprocessing-p* (port-event-process port))
    (destroy-process (port-event-process port))
    (setf (port-event-process port) nil)))


;;; Mirrors

(defmethod realize-mirror ((port basic-port) (sheet mirrored-sheet-mixin))
  (error "Don't know how to realize the mirror of a generic mirrored-sheet"))

(defmethod realize-mirror :before
    ((port basic-port) (sheet basic-sheet))
  (check-type sheet mirrored-sheet-mixin))

(defmethod realize-mirror :around
    ((port basic-port) (sheet mirrored-sheet-mixin))
  (or (sheet-direct-mirror sheet)
      (setf (%sheet-direct-mirror sheet) (call-next-method))))

(defmethod destroy-mirror ((port basic-port) (sheet mirrored-sheet-mixin))
  (error "Don't know how to destroy the mirror of a generic mirrored-sheet"))

(defmethod destroy-mirror :around
    ((port basic-port) (sheet mirrored-sheet-mixin))
  (when-let ((mirror (sheet-direct-mirror sheet)))
    (call-next-method)
    (setf (%sheet-direct-mirror sheet) nil)))

(defmethod port-properties ((port basic-port) indicator)
  (with-slots (properties) port
    (getf properties indicator)))

(defmethod (setf port-properties) (value (port basic-port) indicator)
  (with-slots (properties) port
    (setf (getf properties indicator) value)))

;;; This function determines the sheet to which the pointer event
;;; should be delivered. The right thing is not obvious:
;;;
;;; - we may assume that event-sheet is set correctly by port
;;; - we may find the innermost sheet's child and deliver to it
;;; - we may deliver event to sheet's graft and let it dispatch
;;;
;;; Third option would require a default handle-event method to call
;;; handle-event on its child under the cursor. For now we implement
;;; the second option with the innermost child. In general case we
;;; need z-ordering for both strategies. -- jd 2019-08-21
(defun compute-pointer-event-sheet (event &aux (sheet (event-sheet event)))
  ;; Traverse all descendants of EVENT's sheet which contain EVENT's
  ;; pointer position. The innermost such child that does not have a
  ;; direct mirror should be the new pointer event sheet (we do not
  ;; consider children with a direct mirror since for those EVENT's
  ;; sheet would have been the child in question).
  (labels ((rec (sheet x y)
             (if-let ((child (child-containing-position sheet x y))) ; TODO this only considers enabled children
               (if (sheet-direct-mirror child)
                   sheet
                   (multiple-value-call #'rec
                     child (untransform-position
                            (sheet-transformation child) x y)))
               sheet)))
    (get-pointer-position (sheet event)
      (rec sheet x y))))

(defun common-ancestor (sheet-a sheet-b)
  (flet ((candidatep (sheet)
           (not (or (null sheet) (graftp sheet)))))
    (loop (cond ((eq sheet-a sheet-b)
                 (return sheet-a))
                ((or (not (candidatep sheet-a))
                     (not (candidatep sheet-b)))
                 (return nil))
                ((sheet-ancestor-p sheet-b sheet-a)
                 (return sheet-a))
                (t
                 (setf sheet-a (sheet-parent sheet-a)))))))

;;; Function is responsible for making a copy of an immutable event
;;; and adjusting its coordinates to be in the target-sheet
;;; coordinates. Optionally it may change event's class.
(defun dispatch-event-copy (target-sheet event &optional new-class
                            &aux (sheet (event-sheet event)))
  (if (and (eql target-sheet sheet)
           (or (null new-class)
               (eql new-class (class-of event))))
      (dispatch-event sheet event)
      (let* ((event-class (if (null new-class)
                              (class-of event)
                              (find-class new-class)))
             (new-event (shallow-copy-object event event-class)))
        (when (typep new-event 'pointer-event)
          (get-pointer-position (target-sheet new-event)
            (setf (slot-value new-event 'sheet-x) x
                  (slot-value new-event 'sheet-y) y)))
        (setf (slot-value new-event 'sheet) target-sheet)
        (dispatch-event target-sheet new-event))))

;;; Synthesizing and dispatching boundary events
;;;
;;; PORT only generates boundary-events for mirrored sheets. For
;;; sheets without a mirror, we must synthesize boundary-events.
;;;
;;; This function works in two phases:
;;;
;;; 1) Retrieve the current pointer sheet of PORT and compute a new
;;;    pointer sheet for PORT. This has to be done differently,
;;;    depending on whether EVENT is an enter, exit or any other kind
;;;    of event.
;;;
;;; 2) Based on the old and new pointer sheets, synthesize and
;;;    dispatch boundary events and potentially dispatch EVENT.
;;;
;;; In the first phase, if EVENT is not an exit event, the new port
;;; pointer sheet is the innermost unmirrored child containing the
;;; pointer position of EVENT. If EVENT is an exit event, the new
;;; pointer sheet is the parent of the sheet of EVENT or NIL if the
;;; parent is a graft.
;;;
;;; In the second phase, exit and enter events are synthesized and
;;; dispatched based on the old and new pointer sheet of PORT (both
;;; can be NIL). If EVENT is an enter or exit event, it is dispatched
;;; as part of this process.
(defun synthesize-boundary-events (port event)
  (declare (ignore port))
  (let* ((event-sheet (event-sheet event))
         (pointer (pointer-event-pointer event))
         (old-pointer-sheet (pointer-sheet pointer))
         (new-pointer-sheet old-pointer-sheet)
         (dispatch-event-p nil))
    ;; First phase: compute new pointer sheet for PORT.
    (flet ((update-pointer-sheet (new-sheet)
             (when new-sheet
               (unless (eql old-pointer-sheet new-sheet)
                 (setf (pointer-sheet pointer) new-sheet
                       new-pointer-sheet new-sheet)))))
      (typecase event
        ;; Ignore grab-enter and ungrab-leave boundary events.
        ((or pointer-grab-enter-event pointer-ungrab-leave-event))
        ;; For enter events, update PORT's pointer sheet to the
        ;; innermost child of EVENT's sheet containing EVENT's pointer
        ;; position. Mark EVENT to be dispatched together with
        ;; synthesize events.
        (pointer-enter-event
         ;; Only perform the update and dispatch EVENT if either
         ;; 1) EVENT is not a POINTER-UNGRAB-ENTER-EVENT
         ;; 2) EVENT is a POINTER-UNGRAB-ENTER-EVENT and its sheet is
         ;;    not an ancestor of the old pointer sheet (i.e. ensure
         ;;    that processing EVENT does not re-enter any sheets)
         (when (or (not (typep event 'pointer-ungrab-enter-event))
                   (not (and old-pointer-sheet
                             (sheet-ancestor-p old-pointer-sheet event-sheet))))
           (update-pointer-sheet (compute-pointer-event-sheet event))
           (setf dispatch-event-p t)))
        ;; For exit events, update PORT's pointer sheet to the parent
        ;; of EVENT's sheet, or NIL if that parent is a graft. Mark
        ;; EVENT to be dispatched together with synthesize events.
        (pointer-exit-event
         ;; Only perform the update and dispatch EVENT if either
         ;; 1) EVENT is not a POINTER-GRAB-ENTER-EVENT
         ;; 2) EVENT is a POINTER-GRAB-ENTER-EVENT and the old pointer
         ;;    sheet is an ancestor of its sheet (i.e. ensure that
         ;;    processing EVENT only exits sheets that are currently
         ;;    on the (imaginary) stack of entered sheets).
         (when (or (not (typep event 'pointer-grab-leave-event))
                   (and old-pointer-sheet
                        (sheet-ancestor-p event-sheet old-pointer-sheet)))
           (when (and event-sheet old-pointer-sheet)
             (let ((parent (sheet-parent event-sheet)))
               (update-pointer-sheet (if (graftp parent) nil parent))))
           (setf dispatch-event-p t)))
        ;; For non-boundary events, update PORT's pointer sheet to the
        ;; innermost child of EVENT's sheet containing EVENT's pointer
        ;; position (like for enter events). However, do not dispatch
        ;; EVENT (will be done elsewhere).
        (otherwise
         ;; Only update the pointer sheet if the current pointer sheet
         ;; is non-NIL since we can get such events with the current
         ;; pointer sheet being NIL and the pointer position being
         ;; outside of the top-level sheet's region due to grabbing.
         (when old-pointer-sheet
           (update-pointer-sheet (compute-pointer-event-sheet event))))))

    ;; Second phase: synthesize and dispatch boundary events.
    (flet ((should-synthesize-for-sheet-p (sheet)
             (or (and dispatch-event-p
                      (eq sheet event-sheet))
                 (not (sheet-direct-mirror sheet))))
           (synthesize-enter (sheet)
             (dispatch-event-copy sheet event 'pointer-enter-event))
           (synthesize-exit (sheet)
             (dispatch-event-copy sheet event 'pointer-exit-event)))
      (let ((common-ancestor (when (and old-pointer-sheet new-pointer-sheet)
                               (common-ancestor old-pointer-sheet
                                                new-pointer-sheet))))
        ;; Distribute exit events for OLD-POINTER-SHEET and its
        ;; non-direct-mirrored ancestors (innermost first).
        (do ((sheet old-pointer-sheet (sheet-parent sheet)))
            ((or (eq sheet common-ancestor)
                 (graftp sheet)))
          (when (should-synthesize-for-sheet-p sheet)
            (synthesize-exit sheet)))
        ;; Distribute enter events for NEW-POINTER-SHEET and its
        ;; non-direct-mirrored ancestors (innermost last).
        (do ((sheet new-pointer-sheet (sheet-parent sheet))
             (sheets '()))
            ((or (eq sheet common-ancestor)
                 (graftp sheet))
             (map nil #'synthesize-enter sheets))
          (when (should-synthesize-for-sheet-p sheet)
            (push sheet sheets)))))
    new-pointer-sheet))

(defmethod distribute-event ((port basic-port) event)
  (dispatch-event (event-sheet event) event))

(defmethod distribute-event ((port basic-port) (event keyboard-event))
  (when-let ((focused (port-keyboard-input-focus port)))
    (dispatch-event-copy focused event)))

;;; We focus a sheet in the CLIM thread.
(defmethod handle-event ((sheet top-level-sheet-mixin)
                         (event window-manager-focus-event))
  (setf (port-keyboard-input-focus (port sheet)) sheet))

;;; In the most general case we can't tell whether all sheets are mirrored or
;;; not. So this default method for pointer-events operates under the
;;; assumption that we must deliver events to sheets which doesn't have a
;;; mirror and that the sheet grabbing, pressing and input focusing is
;;; implemented locally. -- jd 2019-08-21
(defmethod distribute-event ((port basic-port) (event pointer-event))
  ;; When we receive pointer event we need to take into account
  ;; unmirrored sheets and grabbed/pressed sheets.
  ;;
  ;; - Grabbed sheet steals all pointer events (non-local exit)
  ;; - Pressed sheet receives replicated motion events
  ;; - Pressing/releasing the button assigns pressed-sheet
  ;; - Pressing the button sends the focus event
  ;; - Pointer motion may result in synthesized boundary events
  ;; - Events are delivered to the innermost child of the sheet
  (flet ((update-cursor (cursor-sheet)
           (let* ((event-sheet (event-sheet event))
                  (old-pointer-cursor
                    (port-lookup-current-pointer-cursor port event-sheet))
                  (new-pointer-cursor (sheet-pointer-cursor cursor-sheet)))
             (unless (eql old-pointer-cursor new-pointer-cursor)
               (set-sheet-pointer-cursor port event-sheet new-pointer-cursor)))))
    (when-let ((grabbed-sheet (port-grabbed-sheet port)))
      (unless (sheetp grabbed-sheet)
        (setf grabbed-sheet (synthesize-boundary-events port event)))
      (update-cursor grabbed-sheet)
      (unless (typep event 'pointer-boundary-event)
        (dispatch-event-copy grabbed-sheet event))
      (return-from distribute-event))
    ;; Synthesize boundary events and update the port-pointer-sheet.
    (let ((pressed-sheet (port-pressed-sheet port))
          (pointer-sheet (synthesize-boundary-events port event)))
      ;; Set the pointer cursor.
      (update-cursor (or pressed-sheet pointer-sheet))
      ;; Handle some events specially.
      (typecase event
        ;; Pressing the pointer button over a sheet makes a sheet pressed and
        ;; focused. The pressed sheet is assigned only when there is currently
        ;; none, while the event for focusing the sheet is always dispatched.
        (pointer-button-press-event
         (when (null pressed-sheet)
           (setf (port-pressed-sheet port) pointer-sheet))
         (when pointer-sheet
           (dispatch-event-copy pointer-sheet event 'window-manager-focus-event)))
        ;; Releasing the button sets the pressed sheet to NIL without changing
        ;; the focus.
        (pointer-button-release-event
         (when pressed-sheet
           (unless (eql pressed-sheet pointer-sheet)
             (dispatch-event-copy pressed-sheet event))
           (setf (port-pressed-sheet port) nil)))
        ;; Boundary events are dispatched in SYNTHESIZE-BOUNDARY-EVENTS.
        (pointer-boundary-event
         (return-from distribute-event))
        ;; Unless pressed sheet is already a target of the motion event,
        ;; event is duplicated and dispatched to it.
        (pointer-motion-event
         (when (and pressed-sheet (not (eql pressed-sheet pointer-sheet)))
           (dispatch-event-copy pressed-sheet event))))
      ;; Distribute event to the innermost child (may be none).
      (when pointer-sheet
        (dispatch-event-copy pointer-sheet event)))))

(defmacro with-port-locked ((port) &body body)
  (let ((fn (gensym "CONT.")))
    `(labels ((,fn ()
                ,@body))
       (declare (dynamic-extent #',fn))
       (invoke-with-port-locked ,port #',fn))))

(defmethod invoke-with-port-locked ((port basic-port) continuation)
  (with-recursive-lock-held ((port-lock port))
    (funcall continuation)))

(defun map-over-ports (function)
  (mapc function *all-ports*))

(defmethod restart-port ((port basic-port))
  nil)

(defmethod destroy-port ((port basic-port))
  nil)

(defmethod destroy-port :around ((port basic-port))
  (unwind-protect
       (call-next-method)
    (setf *all-ports* (remove port *all-ports*))))

;;; Graft

(defmethod make-graft
    ((port basic-port) &key (orientation :default) (units :device))
  (make-instance 'graft :port port :mirror nil
                        :orientation orientation :units units))

(defmethod make-graft :around ((port basic-port) &key orientation units)
  (declare (ignore orientation units))
  (let ((graft (call-next-method)))
    (push graft (port-grafts port))
    graft))

(defmethod map-over-grafts (function (port basic-port))
  (mapc function (port-grafts port)))

(defun find-graft (&key (port nil)
                     (server-path *default-server-path*)
                     (orientation :default)
                     (units :device))
  (when (null port)
    (setq port (find-port :server-path server-path)))
  (map-over-grafts #'(lambda (graft)
                       (if (and (eq orientation (graft-orientation graft))
                                (eq units (graft-units graft)))
                           (return-from find-graft graft)))
                   port)
  (make-graft port :orientation orientation :units units))

(defmethod port-force-output ((port basic-port))
  (values))

;;; Design decision: Recursive grabs are a no-op.

(defmethod port-grab-pointer ((port basic-port) pointer sheet &key multiple-window)
  (declare (ignore pointer sheet multiple-window))
  (warn "Port ~A has not implemented pointer grabbing." port))

(defmethod port-grab-pointer :around ((port basic-port) pointer sheet &key multiple-window)
  (declare (ignore pointer))
  (unless (port-grabbed-sheet port)
    (when (call-next-method)
      (setf (port-grabbed-sheet port)
            (if multiple-window
                t
                sheet)))))

(defmethod port-ungrab-pointer ((port basic-port) pointer sheet)
  (declare (ignore pointer sheet))
  (warn "Port ~A  has not implemented pointer grabbing." port))

(defmethod port-ungrab-pointer :around ((port basic-port) pointer sheet)
  (declare (ignore pointer sheet))
  (when (port-grabbed-sheet port)
    (setf (port-grabbed-sheet port) nil)
    (call-next-method)))

(defmacro with-pointer-grabbed ((port sheet &key pointer multiple-window)
                                &body body)
  (with-gensyms (the-port the-sheet the-pointer)
    `(let* ((,the-port ,port)
            (,the-sheet ,sheet)
            (,the-pointer (or ,pointer (port-pointer ,the-port))))
       (if (not (port-grab-pointer ,the-port ,the-pointer ,the-sheet
                                   :multiple-window ,multiple-window))
           (warn "Port ~A failed to grab a pointer." ,the-port)
           (unwind-protect
                (handler-bind
                    ((serious-condition
                      #'(lambda (c)
                          (declare (ignore c))
                          (port-ungrab-pointer ,the-port
                                               ,the-pointer
                                               ,the-sheet))))
                  ,@body)
             (port-ungrab-pointer ,the-port ,the-pointer ,the-sheet))))))

(defmethod set-sheet-pointer-cursor ((port basic-port) sheet cursor)
  (declare (ignore sheet cursor))
  (warn "Port ~A has not implemented sheet pointer cursors." port))

(defmethod set-sheet-pointer-cursor :before ((port basic-port) sheet cursor)
  (setf (gethash sheet (port-cursors port)) cursor))

(defmethod port-lookup-current-pointer-cursor ((port basic-port) sheet)
  (gethash sheet (port-cursors port)))

(defmethod destroy-mirror :after ((port basic-port) sheet)
  (remhash sheet (slot-value port 'cursors)))

(defun stored-object (port selection)
  (check-type port basic-port)
  (check-type selection symbol)
  (gethash selection (port-selections port)))

(defsetf stored-object (port selection) (value)
  (once-only (port selection)
    `(progn
       (check-type ,port basic-port)
       (check-type ,selection symbol)
       (setf (gethash ,selection (port-selections ,port)) ,value))))

(defun remove-stored-object (port selection)
  (check-type port basic-port)
  (check-type selection symbol)
  (remhash selection (port-selections port)))
