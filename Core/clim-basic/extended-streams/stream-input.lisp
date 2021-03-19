;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000,2014 by Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2001,2002 by Tim Moore (moore@bricoworks.com)
;;;  (c) copyright 2019,2020 by Daniel Kochmański (daniel@turtleware.eu)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; Part VI: Extended Stream Input Facilities
;;; Chapter 22: Extended Stream Input

(in-package #:clim-internals)

(defvar *input-wait-test* nil)
(defvar *input-wait-handler* nil)
(defvar *pointer-button-press-handler* nil)

;;; This variable is a default input buffer for newly created input stream
;;; instances which are based on the INPUT-STREAM-KERNEL. This variable is
;;; here to allow sharing the input buffer between different streams.
(defvar *input-buffer* nil)

;;; X11 returns #\Return where we want to see #\Newline at the stream-read-char
;;; level.  Dunno if this is the right place to do the transformation... --moore
(defun char-for-read (char)
  (check-type char (or character null))
  (case char
    (#\return #\newline)
    (otherwise char)))

(defun event-char (event)
  (let ((modifiers (event-modifier-state event)))
    (when (or (zerop modifiers)
              (eql modifiers +shift-key+))
      (keyboard-event-character event))))


(deftype input-gesture-object ()
  '(or character event))

;;; CLIM's input kernel is only hinted in the specification. This class makes
;;; that concept explicit. Classes STANDARD-BASIC-INPUT-STREAM and
;;; STANDARD-EXTENDED-INPUT-STREAM are based on this class. -- jd 2019-06-24
(defclass input-stream-kernel (standard-sheet-input-mixin)
  (;; The input-buffer representation is CONCURRENT-EVENT-QUEUE (not a vector)
   ;; because it is a suitable implementation to hold gestures. Vector is not
   ;; suitable because it is not clear when it should be emptied.
   (input-buffer :initarg :input-buffer :accessor stream-input-buffer))
  (:default-initargs
   :input-buffer (or *input-buffer* (make-instance 'concurrent-event-queue))))

(defgeneric stream-append-gesture (stream gesture)
  (:method ((stream input-stream-kernel) gesture)
    (check-type gesture input-gesture-object)
    (event-queue-append (stream-input-buffer stream) gesture)))

;;; This function is like (a fictional) PEEK-NO-HANG but only locally in the
;;; buffer, i.e it does not trigger wait on the event queue.
(defgeneric stream-gesture-available-p (stream)
  (:method ((stream input-stream-kernel))
    (event-queue-peek (stream-input-buffer stream))))

;;; Default input-stream-kernel methods.

;;; The input buffer is expected to be filled from HANDLE-EVENT methods. All
;;; tests operate on the stream's input buffer. -- jd 2020-07-28
(defmethod stream-input-wait ((stream input-stream-kernel) &key timeout input-wait-test)
  (loop
    with wait-fun = (and input-wait-test (curry input-wait-test stream))
    with timeout-time = (and timeout (+ timeout (now)))
    when (stream-gesture-available-p stream)
      do (return-from stream-input-wait t)
    do (multiple-value-bind (available reason)
           (event-listen-or-wait stream :timeout timeout
                                        :wait-function wait-fun)
         (when (and (null available) (eq reason :timeout))
           (return-from stream-input-wait (values nil :timeout)))
         (when-let ((event (event-read-no-hang stream)))
           (handle-event (event-sheet event) event))
         (when timeout
           (setf timeout (compute-decay timeout-time nil)))
         (when (maybe-funcall input-wait-test stream)
           (return-from stream-input-wait
             (values nil :input-wait-test))))))

(defmethod stream-listen ((stream input-stream-kernel))
  (stream-input-wait stream))

(defmethod stream-process-gesture ((stream input-stream-kernel) gesture type)
  (declare (ignore stream))
  (values gesture type))

(defmethod stream-read-gesture ((stream input-stream-kernel)
                                &key timeout peek-p
                                  (input-wait-test *input-wait-test*)
                                  (input-wait-handler *input-wait-handler*)
                                  (pointer-button-press-handler
                                   *pointer-button-press-handler*))
  (when peek-p
    (return-from stream-read-gesture
      (stream-gesture-available-p stream)))
  (let ((*input-wait-test* input-wait-test)
        (*input-wait-handler* input-wait-handler)
        (*pointer-button-press-handler* pointer-button-press-handler)
        (input-buffer (stream-input-buffer stream)))
    (flet ((process-gesture (raw-gesture)
             (when (and pointer-button-press-handler
                        (typep raw-gesture '(or pointer-button-press-event
                                                pointer-scroll-event)))
               (funcall pointer-button-press-handler stream raw-gesture))
             (when-let ((gesture (stream-process-gesture stream raw-gesture t)))
               (return-from stream-read-gesture gesture))))
      (loop
        (multiple-value-bind (available reason)
            (stream-input-wait stream :timeout timeout
                                      :input-wait-test input-wait-test)
          (if available
              (process-gesture (event-queue-read input-buffer))
              ;; STREAM-READ-GESTURE specialized on the string input stream
              ;; may return (values nil :eof) (see "XXX Evil hack" below),
              ;; however that should not happen for INPUT-STREAM-KERNEL,
              ;; unless STREAM-INPUT-WAIT returns :EOF and it is not specified
              ;; to do so.
              ;;
              ;; In principle the specification could be extended so
              ;; PROCESS-NEXT-EVENT could return :EOF when the port is
              ;; destroyed. Then that value would be propagated to
              ;; STREAM-INPUT-WAIT and this CASE would be extended with:
              ;;
              ;;   (:eof (return-from stream-read-gesture (values nil :eof))
              (case reason
                (:input-wait-test
                 (maybe-funcall input-wait-handler stream))
                (:timeout
                 (return-from stream-read-gesture (values nil :timeout)))
                (otherwise
                 ;; Invalid reason for no input.
                 (error "STREAM-INPUT-WAIT: Game over (~s)." reason)))))))))

(defmethod stream-unread-gesture ((stream input-stream-kernel) gesture)
  (check-type gesture input-gesture-object)
  (event-queue-prepend (stream-input-buffer stream) gesture)
  nil)

(defmethod stream-clear-input ((stream input-stream-kernel))
  (let ((queue (stream-input-buffer stream)))
    (setf (event-queue-head queue) nil
          (event-queue-tail queue) nil)))

;;; Trampolines for stream-read-gesture and stream-unread-gesture.
(defun read-gesture (&key (stream *standard-input*) timeout peek-p
                       (input-wait-test *input-wait-test*)
                       (input-wait-handler *input-wait-handler*)
                       (pointer-button-press-handler *pointer-button-press-handler*))
  (stream-read-gesture stream
                       :timeout timeout
                       :peek-p peek-p
                       :input-wait-test input-wait-test
                       :input-wait-handler input-wait-handler
                       :pointer-button-press-handler pointer-button-press-handler))

(defun unread-gesture (gesture &key (stream *standard-input*))
  (stream-unread-gesture stream gesture))


;;; This method is deliberately not specialized. -- jd 2019-08-23
(defmethod stream-set-input-focus (stream)
  (let ((port (or (port stream)
                  (port *application-frame*))))
    (prog1 (port-keyboard-input-focus port)
      (setf (port-keyboard-input-focus port) stream))))

(defmacro with-input-focus ((stream) &body body)
  (when (eq stream t)
    (setq stream '*standard-input*))
  (let ((old-stream (gensym "OLD-STREAM")))
    `(let ((,old-stream (stream-set-input-focus ,stream)))
       (unwind-protect (locally ,@body)
         ;; XXX Should we set the port-keyboard-input-focus to NIL
         ;; when there was no old-stream?
         (when ,old-stream
           (stream-set-input-focus ,old-stream))))))


;;; 22.1 Basic Input Streams

;;; Basic input streams are character streams. It should not happen that
;;; input-buffer contains anything besides characters. This class is not a
;;; base class of the STANDARD-EXTENDED-INPUT-STREAM.
;;;
;;; Part of the extended input stream protocol is mixed in thanks to the
;;; INPUT-STREAM-KERNEL. It is good because these parts make sense here.

(defclass standard-input-stream (input-stream-kernel
                                 input-stream
                                 fundamental-character-input-stream)
  ())

(defmethod stream-append-gesture :before ((stream standard-input-stream) gesture)
  (check-type gesture character))

(defmethod handle-event :after
    ((client standard-input-stream) (event key-press-event))
  (when-let ((ch (event-char event)))
    (stream-append-gesture client ch)))

(defmethod stream-process-gesture
    ((stream standard-input-stream) gesture type)
  (declare (ignore type))
  (typecase gesture
    (character
     (values gesture 'character))
    (key-press-event
     (when-let ((char (event-char gesture)))
       (values char 'character)))))

(defmethod stream-read-char ((stream standard-input-stream))
  (stream-read-gesture stream
                       :input-wait-test nil
                       :input-wait-handler nil
                       :pointer-button-press-handler nil))

(defmethod stream-read-char-no-hang ((stream standard-input-stream))
  (stream-read-gesture stream
                       :timeout 0
                       :input-wait-test nil
                       :input-wait-handler nil
                       :pointer-button-press-handler nil))

(defmethod stream-unread-char ((stream standard-input-stream) char)
  (check-type char character)
  (stream-unread-gesture stream char))

(defmethod stream-peek-char ((stream standard-input-stream))
  (stream-read-gesture stream :peek-p t))

;;; STREAM-READ-LINE returns a second value of t if terminated by the fact,
;;; that there is no input (currently) available.
(defmethod stream-read-line ((stream standard-input-stream))
  (loop with input-buffer = (stream-input-buffer stream)
        with result = (make-array 1 :element-type 'character
                                    :adjustable t
                                    :fill-pointer 0)
        for char = (event-read-no-hang input-buffer)
        do (cond ((null char)
                  (return (values result nil)))
                 ((char= #\Newline char)
                  (return (values result t)))
                 (t
                  (vector-push-extend char result)))))


;;; 22.2 Extended Input Streams

;;; 22.2.2 Extended Input Stream Conditions
(defvar *abort-gestures* '(:abort))
(defvar *accelerator-gestures* nil)

(define-condition abort-gesture (condition)
  ((event :reader abort-gesture-event :initarg :event)))

(define-condition accelerator-gesture (condition)
  ((event :reader accelerator-gesture-event :initarg :event)
   (numeric-argument :reader accelerator-gesture-numeric-argument
                     :initarg :numeric-argument
                     :initform 1)))

;;;
(defclass dead-key-merging-mixin ()
  ((state :initform *dead-key-table*)
   ;; Avoid name clash with standard-extended-input-stream.
   (last-deadie-gesture)
   (last-state))
  (:documentation "A mixin class for extended input streams that
takes care of handling dead keys. This is done by still passing
every gesture on, but accenting the final one as per the dead
keys read."))

(defmethod stream-read-gesture :around
    ((stream dead-key-merging-mixin)
     &key timeout peek-p
       (input-wait-test *input-wait-test*)
       (input-wait-handler *input-wait-handler*)
       (pointer-button-press-handler
        *pointer-button-press-handler*))
  (with-slots (state last-deadie-gesture last-state) stream
    (handler-case
        (loop with start-time = (get-internal-real-time)
              with end-time = start-time
              do (multiple-value-bind (gesture reason)
                     (call-next-method stream
                                       :timeout (when timeout
                                                  (- timeout (/ (- end-time start-time)
                                                                internal-time-units-per-second)))
                                       :peek-p peek-p
                                       :input-wait-test input-wait-test
                                       :input-wait-handler input-wait-handler
                                       :pointer-button-press-handler
                                       pointer-button-press-handler)
                   (when (null gesture)
                     (return (values nil reason)))
                   (setf end-time (get-internal-real-time)
                         last-deadie-gesture gesture
                         last-state state)
                   (merging-dead-keys (gesture state)
                     (return gesture))))
      ;; Policy decision: an abort cancels the current composition.
      (abort-gesture (c)
        (setf state *dead-key-table*)
        (signal c)))))

(defmethod stream-unread-gesture :around ((stream dead-key-merging-mixin) gesture)
  (if (typep gesture '(or keyboard-event character))
      (with-slots (state last-deadie-gesture last-state) stream
        (setf state last-state)
        (call-next-method stream last-deadie-gesture))
      (call-next-method)))

;;; Extended input streams are more versatile than basic input streams. They
;;; allow manipulating arbitrary user gestures (not necessarily characters),
;;; i.e pointer button presses. This class does not implement the
;;; FUNDAMENTAL-CHARACTER-INPUT-STREAM protocol (read-char etc).

(defclass standard-extended-input-stream (input-stream-kernel
                                          extended-input-stream
                                          dead-key-merging-mixin)
  ((pointer)
   (cursor :initarg :text-cursor)))

(defmethod handle-event :after
    ((client standard-extended-input-stream) (event keyboard-event))
  (stream-append-gesture client event))

(defmethod handle-event :after
    ((client standard-extended-input-stream) (event pointer-event))
  (stream-append-gesture client event))

(defmethod stream-process-gesture
    ((stream standard-extended-input-stream) gesture type)
  (declare (ignore type))
  (typecase gesture
    (key-press-event
     (if-let ((character (and (zerop (event-modifier-state gesture))
                              (event-char gesture))))
       (values character 'character)
       (values gesture (type-of gesture))))
    (pointer-event
     (values gesture (type-of gesture)))
    (character
     (values gesture 'character))))

(defmethod stream-read-gesture ((stream standard-extended-input-stream)
                                &key &allow-other-keys)
  (loop
    (multiple-value-bind (gesture unavailable-reason)
        (call-next-method)
      (if (null gesture)
          (return-from stream-read-gesture
            (values nil unavailable-reason))
          (flet ((abort-gesture-p (gesture)
                   (loop for gesture-name in *abort-gestures*
                           thereis (event-matches-gesture-name-p gesture gesture-name)))
                 (accelerator-gesture-p (gesture)
                   (loop for gesture-name in *accelerator-gestures*
                           thereis (event-matches-gesture-name-p gesture gesture-name))))
            (cond
              ((abort-gesture-p gesture)
               (signal 'abort-gesture :event gesture))
              ((accelerator-gesture-p gesture)
               (signal 'accelerator-gesture :event gesture))
              (t
               (return-from stream-read-gesture gesture))))))))


;;; stream-read-gesture on string strings. Needed for accept-from-string.

(defmethod stream-read-gesture ((stream string-stream)
                                &key peek-p &allow-other-keys)
  (if-let ((char (if peek-p
                     (peek-char nil stream nil nil)
                     (read-char stream nil nil))))
    char
    (values nil :eof)))

(defmethod stream-unread-gesture ((stream string-stream) gesture)
  (unread-char gesture stream))


;;; 22.3 Gestures and Gesture Names

(defparameter *gesture-names* (make-hash-table))

(defmacro define-gesture-name (name type gesture-spec &key (unique t))
  `(add-gesture-name ',name ',type ',gesture-spec ,@(and unique
                                                         `(:unique ',unique))))

(defun delete-gesture-name (name)
  "Delete the gesture named by the symbol `name' from the list of
known gestures."
  (remhash name *gesture-names*))

;;; XXX perhaps this should be in the backend somewhere?
(defconstant +name-to-char+ '((:newline . #\newline)
                              (:linefeed . #\linefeed)
                              (:return . #\return)
                              (:tab . #\tab)
                              (:backspace . #\backspace)
                              (:page . #\page)
                              (:rubout . #\rubout)))

(defun realize-gesture-spec (type gesture-spec)
  ;; Some CLIM code (scigraph) assumes that gesture-spec can be a symbol.
  (unless (listp gesture-spec)
    (setq gesture-spec (list gesture-spec)))
  (destructuring-bind (device-name . modifiers)
      gesture-spec
    (let* ((modifier-state (apply #'make-modifier-state modifiers)))
      (cond ((and (eq type :keyboard)
                  (symbolp device-name))
             (setq device-name (or (cdr (assoc device-name +name-to-char+))
                                   device-name)))
            ((member type '(:pointer-button
                            :pointer-button-press
                            :pointer-button-release
                            :pointer-scroll)
                     :test #'eq)
             (let ((real-device-name
                     (case device-name
                       (:left        +pointer-left-button+)
                       (:middle      +pointer-middle-button+)
                       (:right       +pointer-right-button+)
                       (:wheel-up    +pointer-wheel-up+)
                       (:wheel-down  +pointer-wheel-down+)
                       (:wheel-left  +pointer-wheel-left+)
                       (:wheel-right +pointer-wheel-right+)
                       (t (error "~S is not a known button" device-name)))))
               (setq device-name real-device-name))))
      (values type device-name modifier-state))))

(defun add-gesture-name (name type gesture-spec &key unique)
  (let ((gesture-entry (multiple-value-list (realize-gesture-spec type gesture-spec))))
    (if unique
        (setf (gethash name *gesture-names*) (list gesture-entry))
        (push gesture-entry (gethash name *gesture-names*)))))

(defgeneric character-gesture-name (name))

(defmethod character-gesture-name ((name character))
  name)

(defmethod character-gesture-name ((name symbol))
  (let ((entry (car (gethash name *gesture-names*))))
    (if entry
        (destructuring-bind (type device-name modifier-state)
            entry
          (if (and (eq type :keyboard)
                   (eql modifier-state 0))
              device-name
              nil))
        nil)))

(defgeneric %event-matches-gesture (event type device-name modifier-state)
  (:method (event type device-name modifier-state)
    (declare (ignore event type device-name modifier-state))
    nil)
  (:method ((event key-press-event)
            (type (eql :keyboard))
            device-name
            modifier-state)
    (let ((character (keyboard-event-character event))
          (name      (keyboard-event-key-name event)))
      (and (if character
               (eql character device-name)
               (eql name device-name))
           (eql (event-modifier-state event) modifier-state))))
  (:method ((event pointer-button-press-event)
            type
            device-name
            modifier-state)
    (and (or (eql type :pointer-button-press)
             (eql type :pointer-button))
         (eql (pointer-event-button event) device-name)
         (eql (event-modifier-state event) modifier-state)))
  (:method ((event pointer-button-release-event)
            type
            device-name
            modifier-state)
    (and (or (eql type :pointer-button-release)
             (eql type :pointer-button))
         (eql (pointer-event-button event) device-name)
         (eql (event-modifier-state event) modifier-state)))
  (:method ((event pointer-scroll-event)
            type
            device-name
            modifier-state)
    (and (or (eql type :pointer-scroll)
             (eql type :pointer-button))
         (eql (pointer-event-button event) device-name)
         (eql (event-modifier-state event) modifier-state)))
  (:method ((event pointer-button-event)
            type
            device-name
            modifier-state)
    (and (or (eql type :pointer-button-press)
             (eql type :pointer-button-release)
             (eql type :pointer-scroll)
             (eql type :pointer-button))
         (eql (pointer-event-button event) device-name)
         (eql (event-modifier-state event) modifier-state)))
  (:method ((event character)
            (type (eql :keyboard))
            device-name
            modifier-state)
    ;; Because gesture objects are either characters or event objects,
    ;; support characters here too.
    (and (eql event device-name)
         (eql modifier-state 0))))

(defun event-matches-gesture-name-p (event gesture-name)
  ;; Just to be nice, we special-case literal characters here.  We also
  ;; special-case literal 'physical' gesture specs of the form (type device-name
  ;; modifier-state).  The CLIM spec requires neither of these things.
  (let ((gesture-entry
          (typecase gesture-name
            (character (list (multiple-value-list (realize-gesture-spec :keyboard gesture-name))))
            (cons (list gesture-name)) ; Literal physical gesture
            (t (gethash gesture-name *gesture-names*)))))
    (loop for (type device-name modifier-state) in gesture-entry
          do (when (%event-matches-gesture event
                                           type
                                           device-name
                                           modifier-state)
               (return-from event-matches-gesture-name-p t))
          finally (return nil))))

(defun modifier-state-matches-gesture-name-p (modifier-state gesture-name)
  (loop for (nil nil gesture-state) in (gethash gesture-name *gesture-names*)
        do (when (eql gesture-state modifier-state)
             (return-from modifier-state-matches-gesture-name-p t))
        finally (return nil)))


(defun make-modifier-state (&rest modifiers)
  (loop for result = 0 then (logior (case modifier
                                      (:shift +shift-key+)
                                      (:control +control-key+)
                                      (:meta +meta-key+)
                                      (:super +super-key+)
                                      (:hyper +hyper-key+)
                                      (t (error "~S is not a known modifier" modifier)))
                                    result)
        for modifier in modifiers
        finally (return result)))

;;; Standard gesture names

(define-gesture-name :abort :keyboard (#\c :control))
(define-gesture-name :clear-input :keyboard (#\u :control))
(define-gesture-name :complete :keyboard (:tab))
(define-gesture-name :help :keyboard (#\/ :control))
(define-gesture-name :possibilities :keyboard (#\? :control))

(define-gesture-name :select :pointer-button-press (:left))
(define-gesture-name :describe :pointer-button-press (:middle))
(define-gesture-name :menu :pointer-button-press (:right))
(define-gesture-name :edit :pointer-button-press (:left :meta))
(define-gesture-name :delete :pointer-button-press (:middle :shift))

(define-gesture-name :scroll-up :pointer-scroll (:wheel-up))
(define-gesture-name :scroll-down :pointer-scroll (:wheel-down))
(define-gesture-name :scroll-left :pointer-scroll (:wheel-left))
(define-gesture-name :scroll-right :pointer-scroll (:wheel-right))

;;; Define so we have a gesture for #\newline that we can use in
;;; *standard-activation-gestures*

(define-gesture-name :newline :keyboard (#\newline))
(define-gesture-name :newline :keyboard (#\return) :unique nil)

(define-gesture-name :return :keyboard (#\return))

;;; The standard delimiter

(define-gesture-name command-delimiter :keyboard (#\space))

;;; Extension: support for handling abort gestures that appears to be
;;; in real CLIM

;;; From the hyperspec, more or less

(defun invoke-condition-restart (c)
  (let ((restarts (compute-restarts c)))
    (loop for i from 0
          for restart in restarts
          do (format t "~&~D: ~A~%" i restart))
    (loop with n = nil
          and k = (length restarts)
          until (and (integerp n) (>= n 0) (< n k))
          do (progn
               (format t "~&Option: ")
               (setq n (read))
               (fresh-line))
          finally
          #-cmu (invoke-restart (nth n restarts))
          #+cmu (funcall (conditions::restart-function (nth n restarts))))))

(defmacro catch-abort-gestures (format-args &body body)
  `(restart-case
       (handler-bind ((abort-gesture #'invoke-condition-restart))
         ,@body)
     (nil ()
       :report (lambda (s) (format s ,@format-args))
       :test (lambda (c) (typep c 'abort-gesture))
       nil)))


;;; 22.4 The Pointer Protocol
;;;
;;; Implemented by the back end.  Sort of.

;;; FIXME: I think the standard-pointer should absorb some of the
;;; common methods that are currently entirely provided by the
;;; backends.

(defclass standard-pointer (pointer)
  ((port :reader port :initarg :port)
   (state-lock :reader state-lock :initform (make-lock "pointer lock"))
   (button-state :initform 0 )
   (sheet :initform nil :initarg :sheet :accessor pointer-sheet)))

(defgeneric pointer-sheet (pointer))

(defgeneric (setf pointer-sheet) (sheet pointer))

(defgeneric pointer-button-state (pointer))

(defgeneric pointer-position (pointer))

(defgeneric* (setf pointer-position) (x y pointer))

(defgeneric synthesize-pointer-motion-event (pointer)
  (:documentation "Create a CLIM pointer motion event based on the current pointer state."))

(defgeneric pointer-cursor (pointer))

(defgeneric (setf pointer-cursor) (cursor pointer))

(defmethod pointer-button-state ((pointer standard-pointer))
  (with-lock-held ((state-lock pointer))
    (slot-value pointer 'button-state)))

(defmethod pointer-update-state
    ((pointer standard-pointer) (event pointer-button-press-event))
  (with-lock-held ((state-lock pointer))
    (setf (slot-value pointer 'button-state)
          (logior (slot-value pointer 'button-state)
                  (pointer-event-button event)))))

(defmethod pointer-update-state
    ((pointer standard-pointer) (event pointer-button-release-event))
  (with-lock-held ((state-lock pointer))
    (setf (slot-value pointer 'button-state)
          (logandc2 (slot-value pointer 'button-state)
                    (pointer-event-button event)))))

(defmethod stream-pointer-position ((stream standard-extended-input-stream)
                                    &key (pointer
                                          (port-pointer (port stream))))
  (multiple-value-bind (x y)
      (pointer-position pointer)
    (let ((graft (graft (port pointer))))
      (untransform-position (sheet-delta-transformation stream graft) x y))))

(defmethod* (setf stream-pointer-position) (x y (stream standard-extended-input-stream))
  (let ((graft (graft stream))
        (pointer (port-pointer (port stream))))
    (multiple-value-bind (x y)
        (transform-position (sheet-delta-transformation stream graft) x y)
      (setf (pointer-position pointer) (values x y)))))
