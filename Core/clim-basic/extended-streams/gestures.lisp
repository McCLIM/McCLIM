;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998,1999,2000 Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2000,2014 Robert Strandh <robert.strandh@gmail.com>
;;;  (c) copyright 2001,2002 Tim Moore <moore@bricoworks.com>
;;;  (c) copyright 2019,2020 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Parsing and definition of gestures and matching of events against
;;; gestures.
;;;

(in-package #:clim-internals)

;;; 22.3 Gestures and Gesture Names

(defparameter *gesture-names* (make-hash-table))

(defmacro define-gesture-name (name type gesture-spec &key (unique t))
  `(add-gesture-name ',name ',type ',gesture-spec ,@(and unique
                                                         `(:unique ',unique))))

(defun delete-gesture-name (name)
  (remhash name *gesture-names*))

;;; TODO perhaps this should be in the backend somewhere?
(defconstant +name-to-char+ '((:newline   . #\newline)
                              (:linefeed  . #\linefeed)
                              (:return    . #\return)
                              (:tab       . #\tab)
                              (:backspace . #\backspace)
                              (:page      . #\page)
                              (:rubout    . #\rubout)))

(defun realize-gesture-spec (type gesture-spec)
  ;; Some CLIM code (scigraph) assumes that gesture-spec can be a symbol.
  (destructuring-bind (device-name . modifiers)
      (alexandria:ensure-list gesture-spec)
    (let ((modifier-state (apply #'make-modifier-state modifiers)))
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
                       (t            (error "~S is not a known button"
                                            device-name)))))
               (setq device-name real-device-name))))
      (values type device-name modifier-state))))

(defun add-gesture-name (name type gesture-spec &key unique)
  (let ((gesture-entry (multiple-value-list
                        (realize-gesture-spec type gesture-spec))))
    (if unique
        (setf (gethash name *gesture-names*) (list gesture-entry))
        (push gesture-entry (gethash name *gesture-names*)))))

(defgeneric character-gesture-name (name))

(defmethod character-gesture-name ((name character))
  name)

(defmethod character-gesture-name ((name symbol))
  (if-let ((entry (first (gethash name *gesture-names*))))
    (destructuring-bind (type device-name modifier-state) entry
      (if (and (eq type :keyboard)
               (eql modifier-state 0))
          device-name
          nil))
    nil))

(defgeneric %event-matches-gesture (event type device-name modifier-state)
  (:method (event type device-name modifier-state)
    (declare (ignore event type device-name modifier-state))
    nil)
  (:method ((event character)
            (type (eql :keyboard))
            device-name
            modifier-state)
    ;; Because gesture objects are either characters or event objects,
    ;; support characters here too.
    (and (eql event device-name)
         (eql modifier-state 0)))
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
         (eql (event-modifier-state event) modifier-state))))

(defun event-matches-gesture-name-p (event gesture-name)
  ;; Just to be nice, we special-case literal characters here.  We also
  ;; special-case literal 'physical' gesture specs of the form (type device-name
  ;; modifier-state).  The CLIM spec requires neither of these things.
  (let ((gesture-entry
          (typecase gesture-name
            (character (list (multiple-value-list
                              (realize-gesture-spec :keyboard gesture-name))))
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
                                      (:shift   +shift-key+)
                                      (:control +control-key+)
                                      (:meta    +meta-key+)
                                      (:super   +super-key+)
                                      (:hyper   +hyper-key+)
                                      (t        (error "~S is not a known modifier"
                                                       modifier)))
                                    result)
        for modifier in modifiers
        finally (return result)))
