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

(deftype keyboard-gesture-type ()
  '(eql :keyboard))

;;; A correspondence between these keywords and POINTER-BUTTON-EVENTs is
;;; defined by a result of the function (EVENT-TYPE EVENT).
(deftype pointer-button-gesture-type ()
  '(member :pointer-button         ; standard
           :pointer-button-press   ; standard
           :pointer-button-release ; standard
           :pointer-scroll))       ; extension

;;; FIXME EVENT-MATCHES-GESTURE-NAME-P handles correctly all this gesture type
;;; but STREAM-READ-GESTURE does not call POINTER-BUTTON-PRESS-HANDLER for it
;;; - that means that presentation translators won't work with these gestures.
;;; The same issue applies to a type :POINTER-BUTTON-RELEASE. -- jd 2023-02-02
(deftype pointer-motion-gesture-type ()
  '(eql :pointer-motion))

;;; "Other" gestures are defined only by the type and the qualifier -
;;; modifiers are ignored.
(deftype other-gesture-type ()
  '(member :timer))

(deftype gesture-type ()
  '(or keyboard-gesture-type
       pointer-button-gesture-type
       pointer-motion-gesture-type
       other-gesture-type))

(deftype physical-gesture ()
  '(cons gesture-type (cons (or symbol character integer) (cons integer null))))

;;; 22.3 Gestures and Gesture Names

(defun error-unknown-name (what value members)
  (error "~@<~S is not a known ~a name. ~
          Valid ~a names are ~{~S~^, ~}.~@:>"
         value what what (map 'list #'car members)))

(defun normalize-union (what value members)
  (cond ((assoc-value members value))
        (t (error-unknown-name what value members))))

(defun normalize-bmask (what values members)
  (reduce #'logior values
          :key (lambda (name)
                 (or (assoc-value members name)
                     (error-unknown-name what name members)))))

(defun normalize-state-with-wildcards (type function value)
  (when (or (eq value t)
            (and (eq type :bmask) (equal value '(t))))
    (return-from normalize-state-with-wildcards t))
  (ecase type
    (:union (funcall function value))
    (:bmask (apply function (ensure-list value)))))

(defconstant +gesture-modifier-key-to-event-modifier+
  `((:shift   . ,+shift-key+)
    (:control . ,+control-key+)
    (:meta    . ,+meta-key+)
    (:super   . ,+super-key+)
    (:hyper   . ,+hyper-key+)))

(defconstant +gesture-button-to-event-button+
  `((:none        . ,+pointer-no-button+)
    (:left        . ,+pointer-left-button+)
    (:middle      . ,+pointer-middle-button+)
    (:right       . ,+pointer-right-button+)
    (:wheel-up    . ,+pointer-wheel-up+)
    (:wheel-down  . ,+pointer-wheel-down+)
    (:wheel-left  . ,+pointer-wheel-left+)
    (:wheel-right . ,+pointer-wheel-right+)))

(defun make-modifier-state (&rest modifiers)
  (normalize-bmask "modifer key" modifiers +gesture-modifier-key-to-event-modifier+))

(defun make-button-state (&rest buttons)
  (normalize-bmask "pointer button" buttons +gesture-button-to-event-button+))

(defun make-pointer-button (button)
  (normalize-union "pointer button" button +gesture-button-to-event-button+))

(defun normalize-keyboard-physical-gesture (gesture-spec)
  (destructuring-bind (key-or-name &rest modifiers)
      (ensure-list gesture-spec)        ; extension
    (check-type key-or-name (or character symbol))
    (values key-or-name
            (normalize-state-with-wildcards
             :bmask #'make-modifier-state modifiers))))

(defun normalize-pointer-button-physical-gesture (gesture-spec)
  (destructuring-bind (button-name &rest modifiers)
      (ensure-list gesture-spec)        ; extension
    (values (normalize-state-with-wildcards
             :union #'make-pointer-button button-name)
            (normalize-state-with-wildcards
             :bmask #'make-modifier-state modifiers))))

(defun normalize-pointer-motion-physical-gesture (gesture-spec)
  (destructuring-bind (button-names &rest modifiers)
      (ensure-list gesture-spec)        ; extension
    (values (normalize-state-with-wildcards
             :bmask #'make-button-state button-names)
            (normalize-state-with-wildcards
             :bmask #'make-modifier-state modifiers))))

(defun normalize-other-gesture (gesture-spec)
  (destructuring-bind (qualifier)
      (ensure-list gesture-spec)
    (values qualifier t)))

(defun normalize-physical-gesture (type gesture-spec)
  (multiple-value-call #'values
    type (typecase type
           (keyboard-gesture-type
            (normalize-keyboard-physical-gesture gesture-spec))
           (pointer-button-gesture-type
            (normalize-pointer-button-physical-gesture gesture-spec))
           (pointer-motion-gesture-type
            (normalize-pointer-motion-physical-gesture gesture-spec))
           (other-gesture-type
            (normalize-other-gesture gesture-spec))
           (t
            (error "~@<~S is not a known gesture type.~@:>" type)))))

;;; A mapping from names which are symbols to lists of type PHYSICAL-GESTURE.
(defvar *gesture-names* (make-hash-table))

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

;;; Extension: GESTURE-SPEC can be an atom which is treated like a device name
;;; (that is pointer button or keyboard key) without modifiers.
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
      ((or cons character symbol)
       (make-keyboard-gesture designator)))))

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
        (when (and (typep type 'keyboard-gesture-type)
                   (eql modifier-state 0))
          device-name)))))

;;; GESTURE is T or a list of normalized physical gestures.
(defun event-data-matches-gesture-p
    (type device-name modifier-state physical-gestures)
  (labels ((matches-with-wildcards-p (value gesture-value)
             (or (eq gesture-value t)
                 (eq value :ignore)
                 (eql value gesture-value)))
           (physical-gesture-matches-p (gesture)
             (destructuring-bind
                 (gesture-type gesture-device-name gesture-modifier-state)
                 gesture
               (and (or (matches-with-wildcards-p type gesture-type)
                        (and (eq gesture-type :pointer-button)
                             (typep type 'pointer-button-gesture-type)))
                    (matches-with-wildcards-p device-name gesture-device-name)
                    (matches-with-wildcards-p
                     modifier-state gesture-modifier-state)))))
    (or (eq physical-gestures t)
        (and (eq type :ignore)
             (eq device-name :ignore)
             (eq modifier-state :ignore))
        (some #'physical-gesture-matches-p physical-gestures))))

(defgeneric event-matches-gesture-p (event physical-gestures)
  (:method (event physical-gestures)
    (declare (ignore event physical-gestures))
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
                                  physical-gestures))
  (:method ((event pointer-motion-event) physical-gestures)
    (event-data-matches-gesture-p (event-type event)
                                  (pointer-button-state event)
                                  (event-modifier-state event)
                                  physical-gestures))
  (:method ((event timer-event) physical-gestures)
    (event-data-matches-gesture-p (event-type event)
                                  (timer-event-qualifier event)
                                  :ignore
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

;;; Wrapper around event-matches-gesture-name-p to match against characters too.
(defgeneric gesture-matches-spec-p (gesture spec)
  (:documentation "Match a gesture against a gesture name or character.")
  (:method (gesture (spec symbol))
    (event-matches-gesture-name-p gesture spec))
  (:method ((gesture character) (spec character))
    (char-equal gesture spec))
  (:method (gesture spec)
    nil))

(defun gesture-match (gesture list)
  "Returns t if gesture matches any gesture spec in list."
  (some #'(lambda (name)
            (gesture-matches-spec-p gesture name))
        list))
