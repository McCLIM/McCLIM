;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998,1999,2000 Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2000,2014 Robert Strandh <robert.strandh@gmail.com>
;;;  (c) copyright 2001,2002 Tim Moore <moore@bricoworks.com>
;;;  (c) copyright 2019,2020 Daniel Kochmański <daniel@turtleware.eu>
;;;  (c) copyright 2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Parsing and definition of gestures and matching of events against
;;; gestures.
;;;

(in-package #:clim-internals)

;;; Types for gestures

(deftype gesture-name ()
  'symbol)

(defun check-gesture-name (name)
  (unless (typep name 'gesture-name)
    (error "~S is not a valid gesture name." name)))

(deftype pointer-gesture-type ()
  '(member :pointer-button         ; standard
           :pointer-button-press   ; standard
           :pointer-button-release ; standard
           :pointer-scroll))       ; extension

(deftype gesture-type ()
  '(or (eql :keyboard) pointer-gesture-type))

(deftype physical-gesture ()
  '(cons gesture-type (cons (or symbol character integer) (cons integer null))))

;;; 22.3 Gestures and Gesture Names

(defconstant +gesture-modifier-key-to-event-modifier+
  `((:shift   . ,+shift-key+)
    (:control . ,+control-key+)
    (:meta    . ,+meta-key+)
    (:super   . ,+super-key+)
    (:hyper   . ,+hyper-key+)))

(defun make-modifier-state (&rest modifiers)
  (reduce #'logior modifiers
          :key (lambda (modifier-key-name)
                 (or (alexandria:assoc-value
                      +gesture-modifier-key-to-event-modifier+ modifier-key-name)
                     (error "~@<~S is not a known modifier key ~
                            name. Valid modifier key names are ~{~S~^, ~
                            ~}.~@:>"
                            modifier-key-name
                            (map 'list #'car +gesture-modifier-key-to-event-modifier+))))))

(defconstant +gesture-key-name-to-char+
  '((:newline   . #\newline)
    (:linefeed  . #\linefeed)
    (:return    . #\return)
    (:tab       . #\tab)
    (:backspace . #\backspace)
    (:page      . #\page)
    (:rubout    . #\rubout)))

(defun normalize-keyboard-physical-gesture (gesture-spec)
  (destructuring-bind (key-or-name &rest modifiers)
      (alexandria:ensure-list gesture-spec) ; extension
    (values (cond ((or (eq key-or-name t)
                       (characterp key-or-name))
                   key-or-name)
                  ((alexandria:assoc-value
                    +gesture-key-name-to-char+ key-or-name))
                  (t
                   #+ (or)
                   (warn "~@<~S is not a known symbolic key ~
                          name. Known symbolic key names are ~{~S~^, ~
                          ~}~@:>"
                         key-or-name
                         (map 'list #'car +gesture-key-name-to-char+))
                   ;; Non-non-standard symbolic gesture.
                   key-or-name))
            (if (equal modifiers '(t))
                t
                (apply #'make-modifier-state modifiers)))))

(defconstant +gesture-button-to-event-button+
  `((:left        . ,+pointer-left-button+)
    (:middle      . ,+pointer-middle-button+)
    (:right       . ,+pointer-right-button+)
    (:wheel-up    . ,+pointer-wheel-up+)
    (:wheel-down  . ,+pointer-wheel-down+)
    (:wheel-left  . ,+pointer-wheel-left+)
    (:wheel-right . ,+pointer-wheel-right+)))

(defun normalize-pointer-physical-gesture (gesture-spec)
  (destructuring-bind (button-name &rest modifiers)
      (alexandria:ensure-list gesture-spec) ; extension
    (values (cond ((eq button-name t)
                   button-name)
                  ((alexandria:assoc-value
                    +gesture-button-to-event-button+ button-name))
                  (t
                   (error "~@<~S is not a known pointer button. Known ~
                           buttons are ~{~S~^, ~}.~@:>"
                          button-name
                          (map 'list #'car +gesture-button-to-event-button+))))
            (if (equal modifiers '(t))
                t
                (apply #'make-modifier-state modifiers)))))

(defun normalize-physical-gesture (type gesture-spec)
  (multiple-value-call #'values
    type (typecase type
           ((eql :keyboard)
            (normalize-keyboard-physical-gesture gesture-spec))
           (pointer-gesture-type
            (normalize-pointer-physical-gesture gesture-spec))
           (t
            (error "~@<~S is not a known gesture type.~@:>" type)))))

;;; A mapping from names which are symbols to lists of type
;;; `physical-gesture'.
(defparameter *gesture-names* (make-hash-table))

(defun find-gesture (name)
  (check-gesture-name name)
  (gethash name *gesture-names*))

(defun add-gesture-name (name type gesture-spec &key unique)
  (check-gesture-name name)
  (let ((gesture-entry (multiple-value-list
                        (normalize-physical-gesture type gesture-spec))))
    (if unique
        (setf (gethash name *gesture-names*) (list gesture-entry))
        (push gesture-entry (gethash name *gesture-names*)))))

(defun delete-gesture-name (name)
  (check-gesture-name name)
  (remhash name *gesture-names*))

;;; Extension: GESTURE-SPEC can be an atom which is treated like a
;;; device name (that is pointer button or keyboard key) without
;;; modifiers.
(defmacro define-gesture-name (name type gesture-spec &key (unique t))
  (with-current-source-form (name)
    (check-gesture-name name))
  (with-current-source-form (gesture-spec)
    (normalize-physical-gesture type gesture-spec)) ; for effect
  `(add-gesture-name ',name ',type ',gesture-spec ,@(when unique
                                                      `(:unique ',unique))))

(defun ensure-physical-gesture (designator)
  ;; Just to be nice, we special-case literal characters here.  We
  ;; also special-case literal 'physical' gesture specs of the form
  ;; (type device-name modifier-state).  The CLIM spec requires
  ;; neither of these things.
  (flet ((make-keyboard-gesture (gesture-spec)
           (multiple-value-list
            (normalize-physical-gesture :keyboard gesture-spec))))
    (typecase designator
      ((cons gesture-type) ; Physical gesture
       designator)
      (cons
       (make-keyboard-gesture designator))
      (character
       (make-keyboard-gesture designator))
      (symbol ; could be a key name or a gesture name
       (make-keyboard-gesture
        (if-let ((character (alexandria:assoc-value
                             +gesture-key-name-to-char+ designator)))
          character
          designator))))))

(defun ensure-gesture (designator)
  (if (and (symbolp designator) (find-gesture designator))
      designator
      (ensure-physical-gesture designator)))

(defgeneric character-gesture-name (name)
  (:method ((name character))
    name)
  (:method ((name symbol))
    (when-let ((entry (first (find-gesture name))))
      (destructuring-bind (type device-name modifier-state) entry
        (when (and (eq type :keyboard) (eql modifier-state 0))
          device-name)))))

;;; GESTURE is T or a list of normalized physical gestures.
(defun event-data-matches-gesture-p (type device-name modifier-state
                                     physical-gestures)
  (labels ((matches-with-wildcards-p (value gesture-value)
             (or (eq gesture-value t)
                 (eq value nil)
                 (eql value gesture-value)))
           (physical-gesture-matches-p (gesture)
             (destructuring-bind
                 (gesture-type gesture-device-name gesture-modifier-state)
                 gesture
               (and (or (matches-with-wildcards-p type gesture-type)
                        (and (eq gesture-type :pointer-button)
                             (typep type 'pointer-gesture-type)))
                    (matches-with-wildcards-p device-name gesture-device-name)
                    (matches-with-wildcards-p
                     modifier-state gesture-modifier-state)))))
    (or (eq physical-gestures t)
        (and (eq type nil) (eq device-name nil) (eq modifier-state nil))
        (some #'physical-gesture-matches-p physical-gestures))))

(defgeneric event-matches-gesture-p (event physical-gestures)
  (:method (event physical-gestures)
    nil)
  (:method ((event character) physical-gestures)
    (event-data-matches-gesture-p :keyboard event 0 physical-gestures))
  (:method ((event keyboard-event) physical-gestures)
    (event-data-matches-gesture-p
     :keyboard
     (or (keyboard-event-character event)
         (keyboard-event-key-name event))
     (event-modifier-state event)
     physical-gestures))
  (:method ((event pointer-button-event) physical-gestures)
    (event-data-matches-gesture-p (event-type event)
                                  (pointer-event-button event)
                                  (event-modifier-state event)
                                  physical-gestures)))

(defun event-matches-gesture-name-p (event gesture-name)
  (let ((physical-gestures
          (cond ((when (symbolp gesture-name)
                   (find-gesture gesture-name)))
                ((when-let ((gesture (ensure-physical-gesture gesture-name)))
                   (list gesture))))))
    (event-matches-gesture-p event physical-gestures)))

(defun modifier-state-matches-gesture-name-p (modifier-state gesture-name)
  (some (lambda (physical-gesture)
          (eql modifier-state (third physical-gesture)))
        (gethash gesture-name *gesture-names*)))
