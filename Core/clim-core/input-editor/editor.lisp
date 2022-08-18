;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2020 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; The base for input editing. This file does not implement stream interfaces.
;;; A concrete implementation is Edward (based on Cluffer), on top of which both
;;; gadgets and input editing streams are defined. A common abstraction allows
;;; redefining gesture-sets for both streams and gadgets.
;;;
;;; TODO syntax-aware editing
;;; TODO import syntaxes from drei (most notably indentation and inks for lisp)

(in-package #:clim-internals)

(defgeneric input-editor-buffer (sheet)
  (:documentation "Returns an opaque editor buffer."))

(defgeneric input-editor-numarg (sheet)
  (:documentation "Returns the accelerator value.")
  (:method (sheet)
    (declare (ignore sheet))
    nil))

;;; Keys are gesture specs (lists canonicalized with realize-gesture-spec) and
;;; values are the input editor commands another hash table constructed in the
;;; same manner.
(defvar *input-editor-commands*
  (make-hash-table :test #'equal))

(defgeneric input-editor-table (sheet)
  (:documentation "Returns the input editor command table.")
  (:method (sheet)
    (declare (ignore sheet))
    *input-editor-commands*))

;;; GESTURES - when list, gestures must be made in order
;;; FUNCTION - four arguments: sheet, buffer, event, numeric-argument
;;; COMMAND-TABLE - McCLIM extension to have different editor tables
(defun add-input-editor-command
    (gestures function &optional (editor-table *input-editor-commands*))
  (loop for gesture in gestures
        do (setf (gethash gesture editor-table) function)))

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
  (if-let ((command (find-input-editor-command event (input-editor-table client))))
    (let ((buffer (input-editor-buffer client)))
      (if-let ((numarg (input-editor-numarg client)))
        (funcall command client buffer event numarg)
        (funcall command client buffer event))
      t)
    nil))

;; (define-command-table editor-common :inherit-from nil)
;; (define-command-table editor-emacs  :inherit-from '(editor-command-table))
;; (define-command-table editor-cua    :inherit-from '(editor-command-table))
;; (define-command-table editor-vim    :inherit-from '(editor-command-table))

(defmacro define-input-editor-command (name-and-options gestures)
  (destructuring-bind (name &key (rescan t) type) (ensure-list name-and-options)
    (declare (ignore type))
    (let ((gesture-name (make-keyword name)))
      `(progn
         (defgeneric ,name (sheet input-buffer event &optional numeric-argument))
         (defmethod ,name :after (sheet input-buffer event &optional numeric-arg)
           (declare (ignore input-buffer event numeric-arg))
           ,(ecase rescan
              ((t) `(progn
                      (format *debug-io* "calling queue rescan~%")
                      (queue-rescan sheet)))
              (:immediate `(immediate-rescan sheet))
              ((nil) (format *debug-io* "not calling rescan~%"))))
         (defmethod ,name (sheet input-buffer event &optional numeric-arg)
           (declare (ignorable sheet input-buffer event numeric-arg))
           (format *debug-io* "~a not defined for args ~a.~%" ',name
                   (mapcar (alexandria:compose #'class-name #'class-of)
                           (list sheet input-buffer event))))
         ;; add-keystroke-to-command-table?
         (delete-gesture-name ,gesture-name)
         ,@(loop for (type . gesture-spec) in gestures
                 collect `(add-gesture-name ,gesture-name ,type ',gesture-spec))
         (add-input-editor-command '(,gesture-name) (function ,name))))))

#+ (or)
(defmacro define-input-stream-command
    ((name &key rescan history)
     (stream input-buffer gesture numeric-argument) &body body)
  (assert (subtypep (second stream) 'input-editing-stream))
  `(defmethod ,name (,stream ,input-buffer ,gesture ,numeric-argument)
     ;; ,@(unless history
     ;;     (setf previous-history nil))
     ,@body
     ,@(case rescan
         (:immediate `(immediate-rescan ,(first stream)))
         ((t) `(queue-rescan ,(first stream)))
         (otherwise nil))))

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
     (:keyboard #\v :control)
     (:keyboard :page-down)
     (:keyboard :next)))

(define-input-editor-command (ie-scroll-backward :rescan nil :type :motion)
    ((:pointer-scroll :wheel-up :control)
     (:keyboard #\v :meta)
     (:keyboard :page-up)
     (:keyboard :prior)))

(define-input-editor-command (ie-select-object :rescan nil :type :motion)
    (#+ (or) :select ; <- support named gesturs to copy their specs?
     (:pointer-button-press :left)))

;;; Deletion commands

(define-input-editor-command (ie-delete-object :type :deletion)
    ((:keyboard #\d :control)
     (:keyboard #\rubout)))

;;; XXX don't use the character #\delete because some (ekhm CCL) implementations
;;; think that it is the same as #\backspace. To avoid confusion use #\rubout.

(define-input-editor-command (ie-delete-word :type :deletion)
    ((:keyboard #\d :meta)
     (:keyboard #\rubout :control)))

(define-input-editor-command (ie-erase-object :type :deletion)
    ((:keyboard #\backspace)))

(define-input-editor-command (ie-erase-word :type :deletion)
    ((:keyboard #\Backspace :meta)
     (:keyboard #\Backspace :control)))

(define-input-editor-command (ie-kill-line :type :deletion)
    ((:keyboard #\k :control)))

(define-input-editor-command (ie-clear-input-buffer :type :deletion)
    ((:keyboard #\backspace :control :meta)))

(define-input-editor-command (ie-insert-newline :type :editing)
    ((:keyboard #\j :control)
     ;(:keyboard #\1 :control)
     (:keyboard #\Newline)
     (:keyboard #\Return)
     (:keyboard :kp-enter)))

(define-input-editor-command (ie-insert-newline-after-cursor :type :editing)
    ((:keyboard #\o :control)))

;;; Transposition commands seem to be savoured by some Emacs users, so we'll
;;; leave them be. They don't seem to be present in the "cua" world.
(define-input-editor-command (ie-transpose-objects :type :editing)
    ((:keyboard #\t :control)))

(define-input-editor-command (ie-transpose-words :type :editing)
    ((:keyboard #\t :meta)))

;;; IE-YANK-HISTORY is for input editing streams. Should IE-YANK-KILL-RING
;;; first look in the clipboard? Should IE-KILL-* put killed content in the
;;; clipboard? The answer to both question is "rather yes".
(define-input-editor-command (ie-yank-kill-ring :type :editing)
    ((:keyboard #\y :control)))

(define-input-editor-command (ie-yank-history :type :editing)
    ((:keyboard #\y :control :meta)))

(define-input-editor-command (ie-yank-next-item :type :editing)
    ((:keyboard #\y :meta)))

;;; implementme(?) C-z (cua) C-/ (emacs), redo C-y (cua) C-spooky (emacs)

;;; Inserts an object in the buffer.
(define-input-editor-command (ie-insert-object :rescan t :type :editing)
    ((:keyboard t)))
