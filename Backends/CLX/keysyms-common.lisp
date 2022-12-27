;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) 2002 Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) 2022 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; X11 keysym handling
;;;

(in-package #:clim-xcommon)

;;; Recall some terminology.  A KEYCODE is an integer between 8 and 255 and it
;;; corresponds to some key on the keyboard.  The correspondence is arbitrary in
;;; that it can vary from one X11 server to another.
;;;
;;; A KEYSYM is non-negative integer with an equivalent Common Lisp type of
;;; (UNSIGNED-BYTE 32).  A keysym has a fixed meaning as a some upper or
;;; lower-case letter, as some special character, or as some operation that does
;;; not have a character equivalent, for example the UP arrow key or the
;;; CapsLock key.
;;;
;;; In addition, we give one or more NAMEs to some important keysyms.  Such a
;;; KEYSYM NAME is a symbol in the KEYWORD package.  Even ordinary letters
;;; follow this pattern, so that the keysym name for the lower-case letter #\a
;;; is the symbol :|a|.
;;;
;;; Let us introduce some terminology related to modifiers.  A MODIFIER is some
;;; abstract concept with no representation in code.  Examples of modifiers are
;;; SHIFT, CONTROL, CAPSLOCK, HYPER, etc.  A MODIFIER KEYCODE is a keycode that
;;; is currently working as a modifier.  A MODIFIER KEYSYM is one of the keysyms
;;; in the fixed set of keysyms corresponding to modifier keys.  A MODIFIER NAME
;;; is the keysym name of modifier keysym.  A MODIFIER VALUE is an integer that
;;; is a power of 2 and that uniquely identifies a particular modifier.  A
;;; MODIFIER MASK is an integer made up the logical OR of modifier values.

;;; This hash table maps a keysym to a list of keysym names for that keysym.
(defvar *keysym-name-table*
  (make-hash-table :test #'eql))

;;; This hash table maps a keysym name to the corresponding keysym.
(defvar *keysym-table*
  (make-hash-table :test #'eq))

(defun define-keysym (name value)
  (pushnew name (gethash value *keysym-name-table* nil))
  (setf (gethash name *keysym-table*) value))

(defun keysym-to-keysym-name (value)
  (car (last (gethash value *keysym-name-table*))))

(defun keysym-name-to-keysym (value)
  (gethash value *keysym-table*))

(defclass keysym-port-mixin ()
  ((modifier-cache :accessor modifier-cache :initform nil)))

;;; The X state is the state before the current event, so key events for the
;;; modifier keys don't reflect the state that results from pressing or
;;; releasing those keys.  We want the CLIM modifiers to reflect the post event
;;; state.

(defun x-keysym-to-clim-modifiers (port event-key keychar keysym-name state)
  "Return modifiers for PORT with STATE.
   If KEYCHAR is a special key(like shift, caps-lock, etc.), update
   the modifiers cache. EVENT-KEY is :key-press or :key-release"
  (multiple-value-bind (clim-modifiers caps-lock? mode-switch?)
      (x-event-state-modifiers port state)
    (declare (ignore caps-lock? mode-switch?))
    (if (characterp keychar)
        clim-modifiers
        (modify-modifiers event-key keysym-name clim-modifiers))))

;;; Modifier cache
;;;
;;; A cache entry is a CONS of two bit masks, each one represented as an
;;; integer.  Each bit mask is the logical OR of constants, each of which is a
;;; power of 2 and defining some modifier.
;;;
;;; The CAR of the cache entry is the bit mask for modifiers defined by the CLIM
;;; II specification.  The corresponding constants are not backend specific, so
;;; they are defined elsewhere.
;;;
;;; The CDR of the cache entry is the bit mask for backend-specific modifiers,
;;; in this case CLX modifiers for shift lock, caps lock, and mode switch.
;;; Recall that the mode switch modifier is the one that determines which of the
;;; two groups of keysyms should be used as an interpretation of a particular
;;; keycode.

;;; Definition of constants for the backend-specific modifier mask.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +caps-lock+ 1)
  (defconstant +mode-switch+ 4))

;;; This dictionary maps CLX keysym names to the power-of-two
;;; constants that the CLIM II specification requires.
(defconstant +clim-modifiers+
  ;; TODO We treat both alt keys as +META-KEY+ (instead of +ALT-KEY+)
  ;; because
  ;; 1) +ALT-KEY+ is non-standard
  ;; 2) +ALT-KEY+ is not exported
  ;; 3) The gesture infrastructure does not accept :alt as a modifier
  '(((:meta-left :meta-right :alt-left :alt-right) #.+meta-key+)
    ((:hyper-left :hyper-right)                    #.+hyper-key+)
    ((:super-left :super-right)                    #.+super-key+)
    ((:shift-left :shift-right)                    #.+shift-key+)
    ((:control-left :control-right)                #.+control-key+)))

;;; This dictionary maps CLX keysym names to the power-of-two constants that are
;;; not required by the CLIM II specification, but that we need anyway, in order
;;; to determine what keysym to choose based on current modifiers.
(defconstant +other-modifiers+
  '((:caps-lock #.+caps-lock+)
    (:mode-switch #.+mode-switch+)))

;;; We need a way to interpret the individual bits of an X11 modifier mask.
;;; This is not a trivial thing to do, because X11 uses different rules for
;;; different types of modifiers, and for some cases there are no fixed rules.
;;;
;;; The three least significant bit positions in a mask have a fixed
;;; interpretation.  Bit 0 means shift, bit 1 means lock and bit 2 means
;;; control.  A keycode assigned to one of these positions takes the meaning of
;;; the position, independently of the keycode and the keysym that the keycode
;;; is associated with.
;;;
;;; Some modifiers, notably num-lock and mode switch, work very differently.
;;; Here, the keysym is important.  To get the effect of num-lock, there has to
;;; be a keycode with the associated keysym that is specific to this modifier
;;; assigned to a bit position.  Similarly, to get the effect of mode switch,
;;; there has to be a keycode with the associated keysym that is specific to
;;; this modifier assigned to a bit position.
;;;
;;; Finally, for some modifiers, there are no specific rules.  The ones we are
;;; particularly interested in are META, SUPER, and HYPER.  So, we have a
;;; choice.  We could either use a fixed-position rule.  That solution makes it
;;; possible to associate any keycode, independently of its associated keysym to
;;; one of these modifiers.  The other possibility is to use the rule of the
;;; associated keysym.  That solution would require the user to change the
;;; mapping from keycodes to keysyms in order to obtain these mappings.
;;;
;;; We are opting for the fixed-position, for the following reasons: There is a
;;; tradition for mod1 to mean ALT or META, and for mod4 to mean
;;; SUPER. Furthermore, mod3 is not assigned to anything in most default
;;; configurations, so we can use it for HYPER.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +shift-bit+      #b00000001)
  (defconstant +lock-bit+       #b00000010)
  (defconstant +control-bit+    #b00000100)
  (defparameter *meta-bit*      #b00001000)
  (defparameter *hyper-bit*     #b00100000)
  (defparameter *super-bit*     #b01000000))

;;; Given an X11/CLX modifier mask, return a CLIM modifier mask with
;;; the relevant bits set.  Recall that the CLIM modifier mask does
;;; not contain bits corresponding to modifiers such as locks or mode
;;; switches.
(defun create-clim-modifier-mask (clx-modifier-mask)
  (let ((m clx-modifier-mask))
    (logior (if (plusp (logand m +shift-bit+)) +shift-key+ 0)
            (if (plusp (logand m +control-bit+)) +control-key+ 0)
            (if (plusp (logand m *meta-bit*)) +meta-key+ 0)
            (if (plusp (logand m *hyper-bit*)) +hyper-key+ 0)
            (if (plusp (logand m *super-bit*)) +super-key+ 0))))

;;; Return the keysym name for the keysym associated with KEYCODE in DISPLAY.
(defun code-to-name (keycode display)
  (keysym-to-keysym-name (xlib:keycode->keysym display keycode 0)))

;;; Return the bit position in a modifier mask that should be interpreted as the
;;; MODE-SWITCH modifier, or NIL if no modifier is to be interpreted as
;;; MODE-SWITCH.
(defun mode-switch-position (display)
  (position-if (lambda (keycodes)
                 (find :mode-switch keycodes
                       :key (lambda (keycode) (code-to-name keycode display))))
               (multiple-value-list (xlib:modifier-mapping display))))

;;; Return the bit position in a modifier mask that should be interpreted as the
;;; NUM-LOCK modifier, or NIL if no modifier is to be interpreted as NUM-LOCK.
(defun num-lock-position (display)
  (position-if (lambda (keycodes)
                 (find :num-lock keycodes
                       :key (lambda (keycode) (code-to-name keycode display))))
               (multiple-value-list (xlib:modifier-mapping display))))

(defun position-to-mask (position)
  (if (null position)
      0
      (ash 1 position)))

;;; Return true if and only if the lock modifier should be interpreted as
;;; CAPS-LOCK.
(defun lock-is-caps-lock-p (display)
  (find :caps-lock (nth-value 1 (xlib:modifier-mapping display))
        :key (lambda (keycode) (code-to-name keycode display))))

;;; Given an X11/CLX modifier mask, return a backend-specific modifier mask with
;;; the relevant bits set.
(defun create-other-modifier-mask (clx-modifier-mask
                                   caps-lock-mask
                                   mode-switch-mask)
  (let ((m clx-modifier-mask))
    (logior (if (plusp (logand m caps-lock-mask)) +caps-lock+ 0)
            (if (plusp (logand m mode-switch-mask)) +mode-switch+ 0))))

;;; Recall that the function CLIM-XCOMMON:KEYSYM-TO-KEYSYM-NAME simply consults
;;; a fixed hash table that maps X11 keysyms (which are numbers) to keysym names
;;; (which are Common Lisp symbols in the KEYWORD package).
;;;
;;; This function returns a list of length 0, 1 or 2.  It returns the empty list
;;; if the keysym with index 0 is 0.  I don't see how this can be the case,
;;; though.  Otherwise, it returns a singleton list if the keysyms with index 0
;;; and 1 are the same, and a list of the keysyms with index 0 and 1 if the two
;;; are different.
;;;
;;; I am guessing that for all modifier keys, the two are the same, so that this
;;; function always returns a singleton list.
(defun modifier-keycode->keysyms (display keycode)
  (let ((first-x-keysym (xlib:keycode->keysym display keycode 0)))
    (when (zerop first-x-keysym)
      (return-from modifier-keycode->keysyms nil))
    (let ((second-x-keysym (xlib:keycode->keysym display keycode 1)))
      (cons (clim-xcommon:keysym-to-keysym-name first-x-keysym)
            (if (eql first-x-keysym second-x-keysym)
                '()
                (list (clim-xcommon:keysym-to-keysym-name second-x-keysym)))))))

;;; Modifier cache support

;;; Recall that XLIB:MODIFIER-MAPPING returns 8 values.  Each value is a list of
;;; keycodes (in some arbitrary order) that are currently used to mean a
;;; particular modifier.  Each value as the following meaning:
;;;
;;;   value number  meaning
;;;        0        shift keycodes
;;;        1        lock keycodes
;;;        2        control keycodes
;;;        3        mod1 keycodes
;;;        4        mod2 keycodes
;;;        5        mod3 keycodes
;;;        6        mod4 keycodes
;;;        7        mod5 keycodes
;;;
;;; The problem here is that a keycode can be a member of more than one list.
;;; For example, if you turn your caps lock key into an additional control key,
;;; then the keycode for the caps lock key may very well be a member both of the
;;; list in value 1 and the list in value 2.
;;;
;;; Let us take the case of caps lock.  The X11 programming manual tells us that
;;; lock modifier is interpreted as caps lock when the keysym named :CAPS-LOCK
;;; (as used by CLX) is attached to some keycode and that keycode is also
;;; attached (as determined by XLIB:MODIFIER-MAPPING) to the lock modifier,
;;; i.e., that keycode is a member of the list in value 1.  The converse seems
;;; to be untrue, though.  Just because someone pressed a key that satisfies
;;; those criteria does not mean that the X11 server will switch on the lock
;;; modifier next time a key is pressed.  It is unclear what the criteria the
;;; X11 server uses.  But for our purpose it is important to start by checking
;;; the lock modifier first.

(defun make-modifier-cache (port)
  (let* ((cache (make-array 256))
         (display (clim-clx::clx-port-display port))
         (caps-lock-mask (if (lock-is-caps-lock-p display) +lock-bit+ #b00))
         (mode-switch-position (mode-switch-position display))
         (mode-switch-mask (position-to-mask mode-switch-position)))
    (loop for x-modifier-mask from 0 below 256
          do (setf (aref cache x-modifier-mask)
                   (cons (create-clim-modifier-mask x-modifier-mask)
                         (create-other-modifier-mask x-modifier-mask
                                                     caps-lock-mask
                                                     mode-switch-mask))))
    cache))

(defun x-event-state-modifiers (port state)
  "For the X STATE, returns as multiple values, the corresponding set of CLIM
modifiers and flags for shift lock, caps lock, and mode switch."
  (with-accessors ((modifier-cache modifier-cache)) port
    (unless modifier-cache
      (setf modifier-cache (make-modifier-cache port)))
    (destructuring-bind (clim-modifiers . other-modifiers)
        ;; Mask off the button state bits.
        (aref modifier-cache
              (mod state (length modifier-cache)))
      (values clim-modifiers
              (logtest +caps-lock+ other-modifiers)
              (logtest +mode-switch+ other-modifiers)))))

(defun modify-modifiers (event-key keysym-name modifiers)
  (let ((keysym-modifier (loop for (keysyms modifier) in +clim-modifiers+
                               if (member keysym-name keysyms)
                               return modifier)))
    (cond ((and keysym-modifier (eq event-key :key-press))
           (logior modifiers keysym-modifier))
          ((and keysym-modifier (eq event-key :key-release))
           (logandc2 modifiers keysym-modifier))
          (t modifiers))))

;;; The X state is the state before the current event, so key events for the
;;; modifier keys don't reflect the state that results from pressing or
;;; releasing those keys.  We want the CLIM modifiers to reflect the post event
;;; state.

;;; Recall that X11 defines how to map keycodes to keysyms by defining a "list"
;;; (not a Common Lisp list, though.  More like a vector in fact.) of possible
;;; keysyms for each keycode.  The third argument to XLIB:KEYCODE->KEYSYM is an
;;; index into that list.  The standard rules make use only of indices 0 to 3 in
;;; that list.  Indices 0 and 1 are considered members of "Group 1" and indices
;;; 2 and 3 are members of "Group 2".
;;;
;;; The Xlib C language library function XKeycodeToKeysym might return some
;;; value corresponding NoSymbol for certain values of the index, in particular
;;; for index 1 when the keycode corresponds to an alphabetic symbol with both a
;;; lower and an upper case version, CLX applies the rules for us, so that in
;;; that case, index 1 is the keysym of the upper-case version of the character.
;;;
;;; The parameter STATE is a bit mask represented as the logical OR of
;;; individual bits.  Each bit corresponds to a modifier or a pointer button
;;; that is active immediately before the key was pressed or released.  The bits
;;; have the following meaning:
;;;
;;;   position  value    meaning
;;;     0         1      shift
;;;     1         2      lock
;;;     2         4      control
;;;     3         8      mod1
;;;     4        16      mod2
;;;     5        32      mod3
;;;     6        64      mod4
;;;     7       128      mod5
;;;     8       256      button1
;;;     9       512      button2
;;;    10      1024      button3
;;;    11      2048      button4
;;;    12      4096      button5

(defun x-event-to-key-name-and-modifiers (port event-key keycode state)
  (multiple-value-bind (clim-modifiers caps-lock? mode-switch?)
      (x-event-state-modifiers port state)
    (let* ((display (clim-clx::clx-port-display port))
           (shift? (logtest +shift-key+ clim-modifiers))
           (shifted-keysym (xlib:keycode->keysym display keycode
                                                 (+ 1 (if mode-switch?
                                                          2 0))))
           (unshifted-keysym (xlib:keycode->keysym display keycode
                                                   (if mode-switch?
                                                       2 0)))
           (keysym-char (xlib:keysym->character display unshifted-keysym
                                                (if mode-switch? 2 0)))
           (alpha-char? (and (characterp keysym-char)
                             (alpha-char-p keysym-char)))
           (keysym
             (if shift?
                 ;; Shift + caps lock cancel themselves for alphabetic chars
                 (if (and caps-lock? alpha-char?)
                     unshifted-keysym
                     shifted-keysym)
                 (if (and caps-lock? alpha-char?)
                     shifted-keysym
                     unshifted-keysym))))
      (let* ((keysym-name (keysym-to-keysym-name keysym))
             (char (xlib:keysym->character display keysym
                                           (+ (if shift?
                                                  1 0)
                                              (if mode-switch?
                                                  2 0))))
             ;; Cache might be updated at this step.
             (modifiers (x-keysym-to-clim-modifiers
                         port event-key char keysym-name state)))
        (values char
                ;; We filter away the shift state if there is a difference
                ;; between the shifted and unshifted keysym. This is so eg. #\A
                ;; will not look like "#\A with a Shift modifier", as this makes
                ;; gesture processing more difficult.
                (if (= shifted-keysym unshifted-keysym)
                    modifiers
                    (logandc2 modifiers +shift-key+))
                keysym-name)))))
