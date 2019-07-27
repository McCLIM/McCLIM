;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000,2014 by Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2001,2002 by Tim Moore (moore@bricoworks.com)
;;;  (c) copyright 2019 by Daniel Kochmański (daniel@turtleware.eu)
;;;
;;; License: LGPL-2.1+

;;; Part VI: Extended Stream Input Facilities
;;; Chapter 22: Extended Stream Input

(in-package #:clim-internals)

;; Input gesture object may be a character or event. Keywords are
;; returned to indicate the failure to obtain a gesture. We could extend
;; this list with a string (look up noise strings). -- jd
(deftype input-gesture-object ()
  `(or character event keyword))

(defvar *input-wait-test* nil)
(defvar *input-wait-handler* nil)
(defvar *pointer-button-press-handler* nil)


;;; CLIM's input kernel is only hinted in the CLIM II Specification. -- jd 2019-06-24
(defclass input-stream-kernel (standard-sheet-input-mixin)
  ((buffer :initarg :input-buffer :accessor stream-input-buffer
           :type (vector input-gesture-object))
   (sp :accessor stream-scan-pointer
       :type integer :documentation "Scan pointer.")
   (ip :accessor stream-insertion-pointer
       :type integer :documentation "Insertion pointer.")))

(defmethod slot-unbound (class (instance input-stream-kernel) (slot-name (eql 'buffer)))
  (setf (slot-value instance 'buffer)
        (make-array 0 :adjustable t :fill-pointer 0)))

(defmethod slot-unbound (class (instance input-stream-kernel) (slot-name (eql 'sp)))
  (setf (slot-value instance 'sp) 0))

(defmethod slot-unbound (class (instance input-stream-kernel) (slot-name (eql 'ip)))
  (setf (slot-value instance 'ip)
        (length (stream-input-buffer instance))))

(defmethod (setf stream-input-buffer) :after (buf (stream input-stream-kernel))
  (setf (stream-scan-pointer stream) 0
        (stream-insertion-pointer stream) (length buf)))

(defmethod (setf stream-scan-pointer) :before (sp (stream input-stream-kernel))
  (let ((ip (stream-insertion-pointer stream)))
    (unless (<= 0 sp ip)
      (error "STREAM-SCAN-POINTER ~s is not in range [0; ~s]." sp ip))))

(defmethod (setf stream-insertion-pointer) :before (ip (stream input-stream-kernel))
  (let ((sp (stream-scan-pointer stream))
        (fp (length (stream-input-buffer stream))))
    (unless (<= sp ip fp)
      (error "STREAM-INSERTION-POINTER ~s is not in range [~s; ~s]." ip sp fp))))

(defgeneric stream-append-gesture (stream gesture)
  (:method ((stream input-stream-kernel) gesture)
    (with-slots (buffer ip) stream
      (vector-push-extend gesture buffer)
      (incf ip))))

;;; This function is like "peek-no-hang" but only locally in the
;;; buffer, i.e it does not trigger wait on event-queue.
(defgeneric stream-gesture-available-p (stream)
  (:method ((stream input-stream-kernel))
    (with-slots (ip sp buffer) stream
      (if (< sp ip)
          (aref buffer sp)
          nil))))

;;; Default input-stream-kernel methods.

;;; Event queue is accessed by streams only in STREAM-INPUT-WAIT.
;;; INPUT-BUFFER is filled in handle-event methods. -- jd 2019-06-26
(defmethod stream-input-wait ((stream input-stream-kernel) &key timeout input-wait-test)
  (loop
     with wait-fun = (and input-wait-test (curry input-wait-test stream))
     until (stream-gesture-available-p stream)
     do (multiple-value-bind (available reason)
            (event-listen-or-wait stream :timeout timeout :wait-function wait-fun)
          (when (null available)
            (return-from stream-input-wait (values nil reason)))
          ;; Defensive programming: we could do without when-let if we assume
          ;; that nobody accessess asynchronously the queue. -- jd
          (alexandria:when-let* ((event (event-read-no-hang stream))
                                 (sheet (event-sheet event)))
            (handle-event sheet event)))
     finally (return t)))

(defmethod stream-listen ((stream input-stream-kernel))
  (stream-input-wait stream))

(defmethod stream-read-gesture ((stream input-stream-kernel)
                                &key timeout peek-p
                                  (input-wait-test *input-wait-test*)
                                  (input-wait-handler *input-wait-handler*)
                                  (pointer-button-press-handler *pointer-button-press-handler*))
  (let ((*input-wait-test* input-wait-test)
        (*input-wait-handler* input-wait-handler)
        (*pointer-button-press-handler* pointer-button-press-handler))
    ;; Check for available input.
    (multiple-value-bind (available reason)
        (stream-input-wait stream :timeout timeout :input-wait-test input-wait-test)
      (when (null available)
        (maybe-funcall input-wait-handler stream)
        (return-from stream-read-gesture (values nil reason))))
    ;; Input is available (or the stream is exhausted).
    (with-slots (sp ip buffer) stream
      (assert (<= 0 sp ip (length buffer)))
      (when (= sp ip)
        (return-from stream-read-gesture (values nil :eof)))
      (prog1 (aref buffer sp)
        (unless peek-p
          (incf sp))))))

(defmethod stream-unread-gesture ((stream input-stream-kernel) gesture)
  (with-slots (sp ip buffer) stream
    (assert (<= 1 sp ip (length buffer)))
    (assert (eql (aref buffer (1- sp)) gesture))
    (decf sp))
  nil)

(defmethod stream-clear-input ((stream input-stream-kernel))
  (with-slots (sp ip buffer) stream
    (setf sp 0
          ip 0)
    ;; XXX: in principle input buffer should have a fill-pointer but in
    ;; practice supporting strings as input buffers is profitable.  Such
    ;; buffers will break if anyone tries to append to them. When
    ;; cleared we swap the buffer with a fresh one. --jd 2019-06-26
    (if (array-has-fill-pointer-p buffer)
        (setf (fill-pointer buffer) 0)
        (slot-makunbound stream 'buffer)))
  nil)

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


;;; 22.1 Basic Input Streams

;;; Basic input streams are character streams. It should not happen
;;; that input-buffer contains anything else than characters. This
;;; class is disjoint to the extended-input-stream protocol.
;;;
;;; Part of the extended input stream protocol is mixed in thanks to
;;; the input-stream-kernel (STREAM-INPUT-BUFFER, STREAM-SCAN-POINTER
;;; and STREAM-INSERTION-POINTER accessors and :INPUT-BUFFER
;;; initarg). It is good because this protocol makes sense here.

(defclass standard-input-stream (input-stream-kernel
                                 input-stream
                                 fundamental-character-input-stream)
  ())

(defmethod handle-event :after ((client standard-input-stream) (event key-press-event))
  (when-let ((ch (keyboard-event-character event)))
    (stream-append-gesture client ch)))

;;; X11 returns #\Return where we want to see #\Newline at the
;;; stream-read-char level.  Dunno if this is the right place to do
;;; the transformation... --moore
(defun voodoo (char)
  (check-type char (or character null))
  (case char
    (#\return #\newline)
    (otherwise char)))

(defmethod stream-read-char ((stream standard-input-stream))
  (voodoo (stream-read-gesture stream
                               :input-wait-test nil
                               :input-wait-handler nil
                               :pointer-button-press-handler nil)))

(defmethod stream-read-char-no-hang ((stream standard-input-stream))
  (voodoo (stream-read-gesture stream
                               :timeout 0
                               :input-wait-test nil
                               :input-wait-handler nil
                               :pointer-button-press-handler nil)))

(defmethod stream-unread-char ((stream standard-input-stream) char)
  ;; Normally we would simply call (stream-unread-gesture stream char)
  ;; but standard-input-stream does some voodoo with the character fixup
  ;; so we must skip the second assertion (the unread gesture is the
  ;; same as the supplied one). -- jd 2019-06-25
  (check-type char character)
  (with-slots (sp ip buffer) stream
    (assert (<= 1 sp ip (length buffer)))
    ;; (assert (eql (aref buffer (1- sp)) gesture))
    (decf sp))
  nil)

(defmethod stream-peek-char ((stream standard-input-stream))
  (let ((char (stream-read-gesture stream :peek-p t)))
    (check-type char character)
    char))

(defmethod stream-read-line ((stream standard-input-stream))
  (with-slots (sp ip buffer) stream
    (assert (<= 0 sp ip (length buffer)))
    (let ((end (position #\newline buffer :start sp :end ip :test #'char=)))
      (multiple-value-prog1 (values (subseq buffer sp (or end ip)) (not end))
        (if (null end)
            (setf sp ip)
            (setf sp (1+ end)))))))


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

;;; Extended input streams are more versatile than basic input
;;; streams. They allow manipulating arbitrary user gestures (not
;;; necessarily characters), i.e pointer button presses. This class is
;;; disjoint from STANDARD-INPUT-STREAM and does not implement
;;; fundamental-character-input-stream protocol (read-char etc).

(defclass standard-extended-input-stream (input-stream-kernel
                                          extended-input-stream)
  ())

(defmethod handle-event :after ((client standard-extended-input-stream) (event key-press-event))
  (if-let ((ch (keyboard-event-character event)))
    (stream-append-gesture client ch)
    (stream-append-gesture client event)))

(defmethod handle-event :after ((client standard-extended-input-stream)
                                (event pointer-event))
  (stream-append-gesture client event))

(defmethod stream-read-gesture ((stream standard-extended-input-stream)
                                &key pointer-button-press-handler &allow-other-keys)
  (multiple-value-bind (gesture unavailable-reason)
      (call-next-method)
    (if (null gesture)
        (values nil unavailable-reason)
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
            ((and pointer-button-press-handler
                  (typep gesture 'pointer-button-press-event))
             (format *debug-io* "calling pointer button press handler")
             (funcall pointer-button-press-handler stream gesture))
            (t
             (return-from stream-read-gesture gesture)))))))

;;; stream-input-buffer, (setf stream-input-buffer) are implemented
;;; for input-stream-kernel.

(defmethod stream-set-input-focus ((stream standard-extended-input-stream))
  (when-let ((port (port stream)))
    (prog1 (port-keyboard-input-focus port)
      (setf (port-keyboard-input-focus port) stream))))

(defmethod invoke-with-input-focus ((stream standard-extended-input-stream) continuation)
  (let ((old-stream (stream-set-input-focus stream)))
    (unwind-protect (funcall continuation stream)
      (when old-stream
        (stream-set-input-focus old-stream)))))

(defmacro with-input-focus ((stream) &body body)
  (setq stream (stream-designator-symbol stream '*standard-input*))
  (with-gensyms (cont arg)
    `(flet ((,cont (,arg) ,@body))
       (declare (dynamic-extent #',cont))
       (invoke-with-input-focus ,stream #',cont))))


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
            ((and (member type '(:pointer-button
                                 :pointer-button-press
                                 :pointer-button-release)
                          :test #'eq))
             (let ((real-device-name
                    (case device-name
                      (:left       +pointer-left-button+)
                      (:middle     +pointer-middle-button+)
                      (:right      +pointer-right-button+)
                      (:wheel-up   +pointer-wheel-up+)
                      (:wheel-down +pointer-wheel-down+)
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
  (if-let ((entry (car (gethash name *gesture-names*))))
    (destructuring-bind (type device-name modifier-state)
        entry
      (if (and (eq type :keyboard)
               (eql modifier-state 0))
          device-name
          nil))
    nil))

(defgeneric %event-matches-gesture (event type device-name modifier-state))

(defmethod %event-matches-gesture (event type device-name modifier-state)
  (declare (ignore event type device-name modifier-state))
  nil)

(defmethod %event-matches-gesture ((event key-press-event)
                                   (type (eql :keyboard))
                                   device-name
                                   modifier-state)
  (let ((character (keyboard-event-character event))
        (name      (keyboard-event-key-name event)))
    (and (if character
             (eql character device-name)
             (eql name device-name))
         (eql (event-modifier-state event) modifier-state))))

(defmethod %event-matches-gesture ((event pointer-button-press-event)
                                   type
                                   device-name
                                   modifier-state)
  (and (or (eql type :pointer-button-press)
           (eql type :pointer-button))
       (eql (pointer-event-button event) device-name)
       (eql (event-modifier-state event) modifier-state)))

(defmethod %event-matches-gesture ((event pointer-button-release-event)
                                   type
                                   device-name
                                   modifier-state)
  (and (or (eql type :pointer-button-release)
           (eql type :pointer-button))
       (eql (pointer-event-button event) device-name)
       (eql (event-modifier-state event) modifier-state)))

(defmethod %event-matches-gesture ((event pointer-button-event)
                                   type
                                   device-name
                                   modifier-state)
  (and (or (eql type :pointer-button-press)
           (eql type :pointer-button-release)
           (eql type :pointer-button))
       (eql (pointer-event-button event) device-name)
       (eql (event-modifier-state event) modifier-state)))

;;; Because gesture objects are either characters or event objects, support
;;; characters here too.

(defmethod %event-matches-gesture ((event character)
                                   (type (eql :keyboard))
                                   device-name
                                   modifier-state)
  (and (eql event device-name)
       (eql modifier-state 0)))

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

;;; Define so we have a gesture for #\newline that we can use in
;;; *standard-activation-gestures*

(define-gesture-name :newline :keyboard (#\newline))
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
   (modifier-state :initform 0)))

(defgeneric pointer-sheet (pointer))

(defmethod pointer-sheet ((pointer pointer))
  (port-pointer-sheet (port pointer)))

(defgeneric (setf pointer-sheet) (sheet pointer))

(defgeneric pointer-button-state (pointer))

(defgeneric pointer-modifier-state (pointer))

(defgeneric pointer-position (pointer))

(defgeneric* (setf pointer-position) (x y pointer))

(defgeneric synthesize-pointer-motion-event (pointer)
  (:documentation "Create a CLIM pointer motion event based on the current pointer state.")) 

(defgeneric pointer-cursor (pointer))

(defgeneric (setf pointer-cursor) (cursor pointer))

;;; Should this go in sheets.lisp?  That comes before events and ports...
;;; it certainly should!

(defmethod handle-event :before ((sheet mirrored-sheet-mixin)
                                 (event pointer-enter-event))
  (setf (port-pointer-sheet (port sheet)) sheet))

(defmethod handle-event :before ((sheet mirrored-sheet-mixin)
                                 (event pointer-exit-event))
  (with-accessors ((port-pointer-sheet port-pointer-sheet))
      (port sheet)
    (when (eq port-pointer-sheet sheet)
      (setq port-pointer-sheet nil))))

(defmethod pointer-button-state ((pointer standard-pointer))
  (with-lock-held ((state-lock pointer))
    (slot-value pointer 'button-state)))

(defmethod pointer-modifier-state ((pointer standard-pointer))
  (with-lock-held ((state-lock pointer))
    (slot-value pointer 'modifier-state)))

(defmethod pointer-update-state
    ((pointer standard-pointer) (event keyboard-event))
  (with-lock-held ((state-lock pointer))
    (setf (slot-value pointer 'modifier-state) (event-modifier-state event))))

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
                                    &key (pointer (port-pointer (port stream))))
  (flet ((sheet-pointer-position (sheet pointer)
           (multiple-value-bind (x y)
               (pointer-position pointer)
             (let ((pointer-sheet (port-pointer-sheet (port sheet))))
               (if (eq sheet pointer-sheet)
                   (values x y)
                   ;; Is this right?
                   (multiple-value-bind (native-x native-y)
                       (transform-position (sheet-native-transformation sheet) x y)
                     (untransform-position (sheet-native-transformation pointer-sheet)
                                           native-x
                                           native-y)))))))
    (sheet-pointer-position stream pointer)))


(defmethod* (setf stream-pointer-position) (x y (stream standard-extended-input-stream))
  ;; XXX: defgeneric* doesn't expand properly when &key is present.
  ;; &key (pointer (port-pointer (port stream)))
  (let ((graft (graft stream))
        (pointer (port-pointer (port stream))))
    (multiple-value-bind (x y)
        (transform-position (sheet-delta-transformation stream graft) x y)
      (setf (pointer-position pointer) (values x y)))))


;;; 22.X The hacks

;;; stream-read-gesture on string strings. Needed for
;;; accept-from-string. We will implement it by creating a stream with
;;; input-buffer assigned to the string later.
;;;
;;; XXX Evil hack because "string-stream" isn't the superclass of string
;;; streams in CMUCL/SBCL...

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *string-input-stream-class* (with-input-from-string (s "foo")
                                        (class-name (class-of s)))))

(defmethod stream-read-gesture ((stream #.*string-input-stream-class*)
                                &key peek-p
                                &allow-other-keys)
  (let ((char (if peek-p
                  (peek-char nil stream nil nil)
                  (read-char stream nil nil))))
    (if char
        char
        (values nil :eof))))

(defmethod stream-unread-gesture ((stream #.*string-input-stream-class*)
                                  gesture)
  (unread-char gesture stream))

;;; SEIS *must not* implement character protocol, our accept
;;; implementation assumes it does though. We will implement them
;;; temporarily until we remove this invalid assumption in the following
;;; commits (to keep changes atomic).

;;; eat all gestures which are not characters
(defmethod stream-read-char-no-hang ((stream standard-extended-input-stream))
  (loop
     for ch = (stream-read-gesture stream)
     when (or (characterp ch) (null ch))
     return (voodoo ch)))

;;; skip assertions for it is only a hack
(defmethod stream-unread-char ((stream standard-extended-input-stream) char)
  (decf (stream-scan-pointer stream)))
