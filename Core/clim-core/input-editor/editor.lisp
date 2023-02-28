;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2020-2023 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; The base for input editing. This file does not implement stream interfaces.
;;; A concrete implementation is Edward (based on Cluffer), on top of which both
;;; gadgets and input editing streams are defined. A common abstraction allows
;;; redefining gesture-sets for both streams and gadgets.
;;;
;;; TODO make the editor use command tables instead of hash tables
;;; TODO add-input-editor-command adds a function but we want a name - checkme
;;; TODO implement the syntax-aware editing
;;; TODO import syntaxes from drei (most notably indentation and inks for lisp)

(in-package #:clim-internals)

(defgeneric input-editor-buffer (sheet)
  (:documentation "Returns an opaque editor buffer."))

;; (define-command-table editor-common :inherit-from nil)
;; (define-command-table editor-emacs  :inherit-from '(editor-command-table))
;; (define-command-table editor-cua    :inherit-from '(editor-command-table))
;; (define-command-table editor-vim    :inherit-from '(editor-command-table))

;;; The key is a gesture name and the value is an input editor command or a
;;; sub-table. This should be replaced by a command table.
(defvar *input-editor-commands*
  (make-hash-table :test #'equal))

;;; Clipboard integration:
;;; - kill puts the text line in :clipboard
;;; - yank uses the clipboard if available but does not modify the killring
(defparameter *killring-uses-clipboard* t)

(defgeneric input-editor-table (sheet)
  (:documentation "Returns the input editor command table.")
  (:method (sheet)
    (declare (ignore sheet))
    *input-editor-commands*))

;;; Another singletons for naive implementations. Also implements killring.

(defparameter *input-editor-last-command* nil)
(defparameter *input-editor-kill-history*
  ;; Who doesn't like a good metacircular implementation of the kill buffer?
  (nth-value 1 (make-kluffer)))

(macrolet ((def-reader (name variable)
             `(defgeneric ,name (editor)
                (:method (editor) ,variable)))
           (def-writer (name variable)
             `(defgeneric (setf ,name) (new-value editor)
                (:method (new-value editor)
                  (setf ,variable new-value))))
           (def-accessor (name variable)
             `(progn
                (def-reader ,name ,variable)
                (def-writer ,name ,variable))))
  (def-accessor input-editor-last-command *input-editor-last-command*)
  (def-reader   input-editor-kill-history *input-editor-kill-history*))

(defun input-editor-kill-object (editor object merge)
  (check-type merge (member :front :back nil))
  (when (zerop (length object))
    (beep editor)
    (return-from input-editor-kill-object nil))
  (let ((history (input-editor-kill-history editor))
        (cmdtype (input-editor-last-command editor)))
    (if (eq cmdtype :kill)
        (smooth-kill-object history object merge)
        (smooth-kill-object history object nil))
    (when *killring-uses-clipboard*
      (clime:publish-selection editor :clipboard
                               (line-string history) 'string))))

(defun input-editor-yank-kill (editor)
  (let* ((history (input-editor-kill-history editor))
         (items (smooth-yank-kill history)))
    (when *killring-uses-clipboard*
      (when-let ((clipboard (clime:request-selection editor :clipboard 'string)))
        ;; ensure that yank-next won't drop the top-most entry.
        (smooth-yank-kill history +1)
        (setf items clipboard)))
    (when (zerop (length items))
      (beep editor)
      (return-from input-editor-yank-kill nil))
    items))

(defun input-editor-yank-next (editor)
  (unless (eq (input-editor-last-command editor) :yank)
    (beep editor)
    (setf (input-editor-last-command editor) :abort)
    (return-from input-editor-yank-next nil))
  (let* ((history (input-editor-kill-history editor))
         (items (smooth-yank-next history)))
    (when (zerop (length items))
      (beep editor)
      (return-from input-editor-yank-next nil))
    items))

;;; GESTURES - gestures that must be typed in order to invoke the command
;;; FUNCTION - lambda list: (sheet buffer event numeric-argument)
;;; COMMAND-TABLE - [McCLIM extension] the command table of the new command
(defun add-input-editor-command
    (gestures command &optional (editor-table *input-editor-commands*))
  (when (rest gestures)
    (error "Chained editor commands are not yet supported."))
  (setf (gethash (first gestures) editor-table) command))

(defun find-input-editor-command (event editor-command-table)
  (maphash (lambda (gesture-name function-or-table)
             ;; Insert object is matched last.
             (when (and (not (eq gesture-name :ie-insert-object))
                        (event-matches-gesture-name-p event gesture-name))
               ;; Should we look for other gesture names that match the event?
               ;; Or maybe the ie-command definer should error for duplicates?
               (return-from find-input-editor-command
                 function-or-table)))
           editor-command-table)
  (if (event-matches-gesture-name-p event :ie-insert-object)
      #'ie-insert-object
      nil))

(defun handle-editor-event (client event)
  "This function returns true when the editor command was executed."
  (if-let ((command (find-input-editor-command event (input-editor-table client))))
    (let ((buffer (input-editor-buffer client))
          (numarg (numeric-argument client)))
      ;; Reset the numarg before invoking a command - it may set it itself.
      (setf (numeric-argument client) 1)
      (funcall command client buffer event numarg)
      t)
    nil))

(defgeneric note-input-editor-command-executed (editor type)
  (:method (editor type)))

(defmacro define-input-editor-command (name-and-options gestures)
  (destructuring-bind (name &key (rescan t) type) (ensure-list name-and-options)
    (let ((gesture-name (make-keyword name)))
      `(progn
         (defgeneric ,name (sheet buffer event numeric-argument)
           (:method (sheet buffer event numeric-argument)
             (format *debug-io* "~a not defined for args ~a (~a).~%" ',name
                     (mapcar (alexandria:compose #'class-name #'class-of)
                             (list sheet buffer event))
                     numeric-argument))
           (:method :after (sheet buffer event numeric-argument)
             (if (eq (input-editor-last-command sheet) :abort)
                 (setf (input-editor-last-command sheet) nil)
                 (setf (input-editor-last-command sheet) ,type))
             (when (input-editor-last-command sheet)
               (note-input-editor-command-executed sheet ,type)))
           (:method ((stream encapsulating-stream) buffer event num-arg)
             (,name (encapsulating-stream-stream stream) buffer event num-arg))
           (:method :after ((stream input-editing-stream) buffer event num-arg)
             (declare (ignorable stream buffer event num-arg))
             ,@(ecase rescan
                 (:immediate `((immediate-rescan stream)))
                 ((t)        `((queue-rescan stream)))
                 ((nil)      `()))))
         (delete-gesture-name ,gesture-name)
         ,@(loop for (type . gesture-spec) in gestures
                 collect `(add-gesture-name ,gesture-name ,type ',gesture-spec))
         (add-input-editor-command '(,gesture-name) (function ,name))))))

;;; This command is by default a no-op.
(define-input-editor-command ie-default-command ())

#+ ()
(defmethod ie-default-command (s i e &optional n)
  (declare (ignore s i e n)))

;;; Commands proposed in the spec.

;;; Motion
(define-input-editor-command (ie-forward-object :rescan nil :type :motion)
    ((:keyboard #\f :control)
     (:keyboard :right)))

(define-input-editor-command (ie-forward-word :rescan nil :type :motion)
    ((:keyboard #\f :meta)
     (:keyboard :right :control)))

(define-input-editor-command (ie-backward-object :rescan nil :type :motion)
    ((:keyboard #\b :control)
     (:keyboard :left)))

(define-input-editor-command (ie-backward-word :rescan nil :type :motion)
    ((:keyboard #\b :meta)
     (:keyboard :left :control)))

(define-input-editor-command (ie-beginning-of-line :rescan nil :type :motion)
    ((:keyboard #\a :control)
     (:keyboard :home)))

(define-input-editor-command (ie-end-of-line :rescan nil :type :motion)
    ((:keyboard #\e :control)
     (:keyboard :end)))

(define-input-editor-command (ie-next-line :rescan nil :type :motion)
    ((:pointer-scroll :wheel-down)
     (:keyboard #\n :control)
     (:keyboard :down)))

(define-input-editor-command (ie-previous-line :rescan nil :type :motion)
    ((:pointer-scroll :wheel-up)
     (:keyboard #\p :control)
     (:keyboard :up)))

(define-input-editor-command (ie-beginning-of-buffer :rescan nil :type :motion)
    ((:keyboard #\< :meta)
     (:keyboard :home :control)))

(define-input-editor-command (ie-end-of-buffer :rescan nil :type :motion)
    ((:keyboard #\> :meta)
     (:keyboard :end :control)))

;;; I'd like to have a different _default_ numeric argument for each gesture:
;;; for the keyboard gesture and C-wheel_down it would be one page, and for
;;; :wheel-down it would be four lines.
(define-input-editor-command (ie-scroll-forward :rescan nil :type :motion)
    ((:pointer-scroll :wheel-down :control)
     ;; (:keyboard #\v :control)
     (:keyboard :page-down)
     (:keyboard :next)))

(define-input-editor-command (ie-scroll-backward :rescan nil :type :motion)
    ((:pointer-scroll :wheel-up :control)
     ;; (:keyboard #\v :meta)
     (:keyboard :page-up)
     (:keyboard :prior)))

(define-input-editor-command (ie-select-object :rescan nil :type :motion)
    (;; :select ; <- support named gesturs to copy their specs?
     (:pointer-button-press :left t)
     (:keyboard #\space :control)))

(define-input-editor-command (ie-release-object :rescan nil :type :motion)
    ((:pointer-button-release :left t)))

;;; The dragged object may be either a selected region or the edit cursor
;;; modifying the selected region.
(define-input-editor-command (ie-select-region :rescan nil :type :motion)
    ((:pointer-motion :left t)))

(define-input-editor-command (ie-context-menu :rescan nil :type :motion)
    ((:pointer-button-press :right)))

;;; Deletion commands

;;; XXX don't use the character #\delete because some (ekhm CCL) implementations
;;; think that it is the same as #\backspace. To avoid confusion use #\rubout.

(define-input-editor-command (ie-delete-object :type :edit)
    ((:keyboard #\d :control)
     (:keyboard #\rubout)))

(define-input-editor-command (ie-erase-object :type :edit)
    ((:keyboard #\backspace)))

;;; XXX Should this put contents in the kill ring?  :type :kill
(define-input-editor-command (ie-clear-input-buffer :type :edit)
    ((:keyboard #\backspace :control :meta)))

;;; Killing spree

(define-input-editor-command (ie-delete-word :type :kill)
    ((:keyboard #\d :meta)
     (:keyboard #\rubout :control)))

(define-input-editor-command (ie-erase-word :type :kill)
    ((:keyboard #\Backspace :meta)
     (:keyboard #\Backspace :control)))

(define-input-editor-command (ie-kill-line :type :kill)
    ((:keyboard #\k :control)))

(define-input-editor-command (ie-kill-slide :type :kill)
    ((:keyboard #\w :control)))

#+ (or)
(define-input-editor-command (ie-kill-region :type :kill)
    ((:keyboard #\w :control)))

;;; Editing operations

(define-input-editor-command (ie-insert-newline :type :edit)
    ((:keyboard #\j :control)
     (:keyboard #\Newline)
     (:keyboard #\Return)
     (:keyboard :kp-enter)))

(define-input-editor-command (ie-insert-newline-after-cursor :type :edit)
    ((:keyboard #\o :control)))

;;; Transposition commands seem to be savoured by some Emacs users, so we'll
;;; leave them be. They don't seem to be present in the "cua" world.
(define-input-editor-command (ie-transpose-objects :type :edit)
    ((:keyboard #\t :control)))

(define-input-editor-command (ie-transpose-words :type :edit)
    ((:keyboard #\t :meta)))

;;; IE-YANK-HISTORY is for input editing streams. Should IE-YANK-KILL-RING
;;; first look in the clipboard? Should IE-KILL-* put killed content in the
;;; clipboard? The answer to both question is "rather yes".
(define-input-editor-command (ie-yank-kill-ring :type :yank)
    ((:keyboard #\y :control)))

(define-input-editor-command (ie-yank-next-item :type :yank)
    ((:keyboard #\y :meta)))

(define-input-editor-command (ie-yank-history :type :yank)
    ((:keyboard #\y :control :meta)))


;;; The clipboard and the primary selection
(define-input-editor-command (ie-request-primary :type :edit)
    ((:pointer-button-press :middle)))

(define-input-editor-command (ie-cut :type :edit)
    ((:keyboard #\x :control)))

(define-input-editor-command (ie-copy :type nil)
    ((:keyboard #\c :control)))

(define-input-editor-command (ie-paste :type :edit)
    ((:keyboard #\v :control)))

;;;
(define-input-editor-command (ie-abort :type :abort)
    ((:keyboard #\g :control)))

;;; implementme(?) C-z (cua) C-/ (emacs), redo C-y (cua) C-spooky (emacs)

;;; Numeric arguments.

;;; FIXME this does not accept the integer - good enough for testing.
;;; FIXME do we really want to handle universal arguments as commands?
(define-input-editor-command (ie-numeric-argument :rescan nil :type nil)
    ((:keyboard #\u :control)))

;;; TODO predicate gesture qualifiers (digit-char-p)
;;; TODO input editor command arguments (read with accept or lower?)
;;; TODO do we want to allow providing a default implementation?
#+ (or)
(progn ;; mind that both arglists are incorrect and mutually exclusive (!).

  (define-input-editor-command (ie-numeric-argument :rescan nil :type nil)
      ((:keyboard #'digit-char-p :control))
    (stream buffer event numeric-argument)
    (setf (numeric-argument stream) (parse-integer event)))

  (define-input-editor-command (ie-read-numeric-argument :rescan nil :type nil)
      ((:keyboard #\u :control))
      ((value 'integer :default 4))
    (setf (numeric-argument stream) value)))

;;; Inserts an object in the buffer.
(define-input-editor-command (ie-insert-object :rescan t :type :edit)
    ((:keyboard t)))
