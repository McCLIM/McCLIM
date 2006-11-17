;;; -*- Mode: Lisp; Package: DREI -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)

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
;;;
;;; Drei is an editing substrate designed for use in CLIM, and used to
;;; implement text-editor gadgets and input-editing-streams in
;;; McCLIM. It is also used as the editor engine in Climacs, from
;;; which Drei was originally extracted. At it's base, Drei has a
;;; `drei' class that contains the buffer and some marks into the
;;; buffer - from this, we derive concrete Drei implementations that
;;; implement usage-dependent semantics for redisplay and input
;;; handling. The essense of Drei is that a set of protocols can be
;;; used to define editing commands and functionality that can be used
;;; in all Drei derivatives, from Climacs to the tiniest of
;;; input-fields, and hence make it as easy for the user (and hacker)
;;; to customize every text-editing task in the CLIM environment, as
;;; it is to customize Emacs.
;;;
;;; In this file, we wrap all the various bits and parts together and
;;; build the basic Drei primitives, such as the buffer and the
;;; abstract `drei' class.
;;;
;;; Officially, Drei stands for "Drei Replaces EINE's Inheritor", but
;;; there are alternatives:
;;;
;;; * Drei Reimplements Emacs Intelligently
;;;
;;; * Drei Reimplements Emacs' Internals
;;;
;;; * Drei Raises Engineer's Interest
;;;
;;; * Drei Revives Eremites' Interest
;;;
;;; * Drei Recursively Expands Itself
;;;
;;; * Drei Erhbar Emacs Ist

(in-package :drei)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convenience stuff.

(defun current-point ()
  "Return the current panes point."
  (point *current-window*))

(defun current-mark ()
  "Return the current panes mark."
  (mark *current-window*))

(defun current-syntax ()
  "Return the syntax of the current buffer."
  (syntax *current-buffer*))

(defparameter *current-point* nil
  "The current point.")

(defparameter *current-mark* nil
  "The current mark.")

(defparameter *current-syntax* nil
  "The syntax of the current buffer.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions.

(define-condition user-condition-mixin ()
  ()
  (:documentation "Conditions of this type are caught by the Drei
command loop and their report displayed to the user in the
minibuffer, instead of being propagated further (and possibly
invoking the debugger)."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tabify

(defvar *use-tabs-for-indentation* nil
  "If non-NIL, use tabs when indenting lines. Otherwise, use spaces.")

;; To ease use in non-tabifying-panes/streams, we define sensible
;; defaults.
(defgeneric space-width (tabify)
  (:method ((pane clim-stream-pane))
    (text-size pane " ")))
(defgeneric tab-width (tabify)
  (:method ((pane clim-stream-pane))
    (text-size pane #.(format nil "~A" #\Tab))))
(defgeneric tab-space-count (tabify)
  (:method ((stream extended-output-stream))
    8))

(defclass tabify-mixin ()
  ((space-width :initform nil :reader space-width)
   (tab-width :initform nil :reader tab-width)))

(defmethod tab-space-count ((tabify t))
  1)

(defmethod tab-space-count ((tabify tabify-mixin))
  (round (tab-width tabify) (space-width tabify)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Undo

(defclass undo-mixin ()
  ((tree :initform (make-instance 'standard-undo-tree) :reader undo-tree)
   (undo-accumulate :initform '() :accessor undo-accumulate)
   (performing-undo :initform nil :accessor performing-undo)))

(defclass drei-undo-record (standard-undo-record)
  ((buffer :initarg :buffer)))

(defclass simple-undo-record (drei-undo-record)
  ((offset :initarg :offset :reader undo-offset)))

(defclass insert-record (simple-undo-record)
  ((objects :initarg :objects)))

(defclass delete-record (simple-undo-record)
  ((length :initarg :length)))

(defclass compound-record (drei-undo-record)
  ((records :initform '() :initarg :records)))

(defmethod print-object  ((object delete-record) stream)
  (with-slots (offset length) object
    (format stream "[offset: ~a length: ~a]" offset length)))

(defmethod print-object  ((object insert-record) stream)
  (with-slots (offset objects) object
    (format stream "[offset: ~a objects: ~a]" offset objects)))

(defmethod print-object  ((object compound-record) stream)
  (with-slots (records) object
    (format stream "[records: ~a]" records)))

(defmethod insert-buffer-object :before ((buffer undo-mixin) offset object)
  (declare (ignore object))
  (unless (performing-undo buffer)
    (push (make-instance 'delete-record
                         :buffer buffer :offset offset :length 1)
	  (undo-accumulate buffer))))

(defmethod insert-buffer-sequence :before ((buffer undo-mixin) offset sequence)
  (unless (performing-undo buffer)
    (push (make-instance 'delete-record
                         :buffer buffer :offset offset :length (length sequence))
	  (undo-accumulate buffer))))

(defmethod delete-buffer-range :before ((buffer undo-mixin) offset n)
  (unless (performing-undo buffer)
    (push (make-instance 'insert-record
                         :buffer buffer :offset offset
                         :objects (buffer-sequence buffer offset (+ offset n)))
	  (undo-accumulate buffer))))

(defmacro with-undo ((get-buffers-exp) &body body)
  "Evaluate `body', registering any changes to buffer contents in
the undo memory for the respective buffer, permitting individual
undo for each buffer. `get-buffers-exp' should be a form, that
will be evaluated whenever a complete list of buffers is
needed (to set up all buffers to prepare for undo, and to check
them all for changes after `body' has run)."
  (with-gensyms (buffer)
    `(progn
       (dolist (,buffer ,get-buffers-exp)
         (setf (undo-accumulate ,buffer) '()))
       (unwind-protect (progn ,@body)
         (dolist (,buffer ,get-buffers-exp)
           (cond ((null (undo-accumulate ,buffer)) nil)
                 ((null (cdr (undo-accumulate ,buffer)))
                  (add-undo (car (undo-accumulate ,buffer))
                            (undo-tree ,buffer)))
                 (t
                  (add-undo (make-instance 'compound-record
                                           :buffer ,buffer
                                           :records (undo-accumulate ,buffer))
                            (undo-tree ,buffer)))))))))

(defmethod flip-undo-record :around ((record drei-undo-record))
  (with-slots (buffer) record
    (let ((performing-undo (performing-undo buffer)))
      (setf (performing-undo buffer) t)
      (unwind-protect (call-next-method)
        (setf (performing-undo buffer) performing-undo)))))

(defmethod flip-undo-record ((record insert-record))
  (with-slots (buffer offset objects) record
    (change-class record 'delete-record
                  :length (length objects))
    (insert-buffer-sequence buffer offset objects)))

(defmethod flip-undo-record ((record delete-record))
  (with-slots (buffer offset length) record
    (change-class record 'insert-record
                  :objects (buffer-sequence buffer offset (+ offset length)))
    (delete-buffer-range buffer offset length)))

(defmethod flip-undo-record ((record compound-record))
  (with-slots (records) record
    (mapc #'flip-undo-record records)
    (setf records (nreverse records))))

(defgeneric clear-undo-history (undo-maintainer)
  (:documentation "Clear the undo history for `undo-maintainer',
preventing the undoing to before the state of whatever
`undo-maintainer' is maintaining undo for."))

(defmethod clear-undo-history ((undo-maintainer undo-mixin))
  (setf (slot-value undo-maintainer 'tree)
        (make-instance 'standard-undo-tree)))

;;; undo-mixin delegation (here because of the package)

(defmethod undo-tree ((buffer delegating-buffer))
  (undo-tree (implementation buffer)))

(defmethod undo-accumulate ((buffer delegating-buffer))
  (undo-accumulate (implementation buffer)))

(defmethod (setf undo-accumulate) (object (buffer delegating-buffer))
  (setf (undo-accumulate (implementation buffer)) object))

(defmethod performing-undo ((buffer delegating-buffer))
  (performing-undo (implementation buffer)))

(defmethod (setf performing-undo) (object (buffer delegating-buffer))
  (setf (performing-undo (implementation buffer)) object))

(defmethod clear-undo-history ((buffer delegating-buffer))
  (clear-undo-history (implementation buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Isearch

(defclass isearch-state ()
  ((search-string :initarg :search-string :accessor search-string)
   (search-mark :initarg :search-mark :accessor search-mark)
   (search-forward-p :initarg :search-forward-p :accessor search-forward-p)
   (search-success-p :initarg :search-success-p :accessor search-success-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Query replace

(defclass query-replace-state ()
  ((string1 :initarg :string1 :accessor string1)
   (string2 :initarg :string2 :accessor string2)
   (mark :initarg :mark :accessor mark)
   (occurences :initform 0 :accessor occurrences)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Readonly

(defclass read-only-mixin ()
  ((read-only-p :initform nil :accessor read-only-p)))

(define-condition buffer-read-only (user-condition-mixin simple-error)
  ((buffer :reader condition-buffer :initarg :buffer))
  (:report (lambda (condition stream)
	     (format stream "Attempt to change read only buffer: ~a"
		     (condition-buffer condition))))
  (:documentation "This condition is signalled whenever an attempt
is made to alter a buffer which has been set read only."))

(defmethod insert-buffer-object ((buffer read-only-mixin) offset object)
  (if (read-only-p buffer)
      (error 'buffer-read-only :buffer buffer)
      (call-next-method)))

(defmethod insert-buffer-sequence ((buffer read-only-mixin) offset sequence)
  (if (read-only-p buffer)
      (error 'buffer-read-only :buffer buffer)
      (call-next-method)))

(defmethod delete-buffer-range ((buffer read-only-mixin) offset n)
  (if (read-only-p buffer)
      (error 'buffer-read-only :buffer buffer)
      (call-next-method)))

(defmethod (setf buffer-object) (object (buffer read-only-mixin) offset)
  (if (read-only-p buffer)
      (error 'buffer-read-only :buffer buffer)
      (call-next-method)))

(defmethod read-only-p ((buffer delegating-buffer))
  (read-only-p (implementation buffer)))

(defmethod (setf read-only-p) (flag (buffer delegating-buffer))
  (setf (read-only-p (implementation buffer)) flag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Single-line buffer.

(defclass single-line-mixin ()
  ((%single-line-p :initform nil
                   :accessor single-line-p
                   :initarg :single-line))
  (:documentation "Prevent the insertion of #\Newline characters
into the buffer if `single-line-p' is true."))

(define-condition buffer-single-line (user-condition-mixin simple-error)
  ((buffer :reader condition-buffer :initarg :buffer))
  (:report "Attempt to insert newline into single-line buffer.")
  (:documentation "This condition is signalled whenever an
attempt is made to insert a #\Newline character into a
single-line buffer."))

(defmethod insert-buffer-object :before ((buffer single-line-mixin) offset (object (eql #\Newline)))
  (when (single-line-p buffer)
    (error 'buffer-single-line :buffer buffer)))

(defmethod insert-buffer-sequence :before ((buffer single-line-mixin) offset sequence)
  (when (and (single-line-p buffer)
             (find #\Newline sequence))
    (error 'buffer-single-line :buffer buffer)))

(defmethod (setf buffer-object) :before ((object (eql #\Newline)) (buffer single-line-mixin) offset)
  (when (single-line-p buffer)
    (error 'buffer-single-line :buffer buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; View

(defclass drei-view (tabify-mixin)
  ()
  (:documentation "The base class for all Drei views."))

(defclass drei-textual-view (drei-view textual-view)
  ())

(defparameter +drei-textual-view+ (make-instance 'drei-textual-view))

;; (defgeneric indent-tabs-mode (drei-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Drei buffer.

(defclass extended-standard-buffer (single-line-mixin read-only-mixin standard-buffer undo-mixin abbrev-mixin) ()
  (:documentation "Extensions accessible via marks."))

(defclass extended-binseq2-buffer (single-line-mixin read-only-mixin binseq2-buffer p-undo-mixin abbrev-mixin) ()
  (:documentation "Extensions accessible via marks."))

(defclass drei-buffer (delegating-buffer esa-buffer-mixin)
  ((needs-saving :initform nil :accessor needs-saving)
   (syntax :accessor syntax)
   (point :initform nil :initarg :point :accessor point)
   (indent-tabs-mode :initarg indent-tabs-mode
                     :initform *use-tabs-for-indentation*
                     :accessor indent-tabs-mode))
  (:default-initargs
   :name "*scratch*"
    :implementation (make-instance 'extended-standard-buffer)))

(defmethod initialize-instance :after ((buffer drei-buffer) &rest args
                                       &key initial-contents (syntax *default-syntax*))
  (declare (ignore args))
  (check-type syntax symbol)
  (with-accessors ((buffer-syntax syntax)
                   (point point)) buffer
    (when initial-contents
      (check-type initial-contents array)
      (insert-sequence (low-mark buffer) initial-contents))
    (setf buffer-syntax (make-instance syntax :buffer (implementation buffer))
          point (clone-mark (low-mark buffer) :right))))

(defmethod (setf syntax) :after (syntax (buffer drei-buffer))
  (setf (offset (low-mark buffer)) 0
        (offset (high-mark buffer)) (size buffer))
  ;; We must never end up with a non-updated syntax, that is a basic
  ;; invariant.
  (update-syntax buffer syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drei command tables.

;;; Commenting.
(make-command-table 'comment-table :errorp nil)
;;; Deleting.
(make-command-table 'deletion-table :errorp nil)
;;; Editing - making changes to a buffer.
(make-command-table 'editing-table :errorp nil)
;;; Filling.
(make-command-table 'fill-table :errorp nil)
;;; Dealing with charcase.
(make-command-table 'case-table :errorp nil)
;;; Indentation.
(make-command-table 'indent-table :errorp nil)
;;; Marking things.
(make-command-table 'marking-table :errorp nil)
;;; Moving around.
(make-command-table 'movement-table :errorp nil)
;;; Searching.
(make-command-table 'search-table :errorp nil)
;;; Information about buffer contents.
(make-command-table 'info-table :errorp nil)
;;; Self-insertion.
(make-command-table 'self-insert-table :errorp nil)

;;; Command table for concrete editor stuff.
(define-syntax-command-table editor-table
    :errorp nil
    :inherit-from '(comment-table
                    deletion-table
                    editing-table
                    case-table
                    fill-table
                    indent-table
                    marking-table
                    movement-table
                    search-table
                    info-table
                    self-insert-table
                    keyboard-macro-table))

;; Command table for commands that are only available when Drei is a
;; gadget. There is no pane-exclusive table because the Drei pane is
;; not meant to be used as-is, but is meant to be subclassed, so we do
;; not want to force users to work around too much default behavior.
(make-command-table 'exclusive-gadget-table :errorp nil)

;; Command table for input-editor-only commands.
(make-command-table 'exclusive-input-editor-table :errorp nil)

(define-command (com-drei-extended-command :command-table exclusive-gadget-table)
    ()
  "Prompt for a command name and arguments, then run it."
  (let ((item (handler-case
                  (accept
                   `(command :command-table ,(command-table *current-window*))
                   ;; this gets erased immediately anyway
                   :prompt "" :prompt-mode :raw)
                ((or command-not-accessible command-not-present) ()
                  (beep)
                  (display-message "No such command")
                  (return-from com-drei-extended-command nil)))))
    (execute-drei-command *current-window* item)))

(set-key 'com-drei-extended-command
         'exclusive-gadget-table
         '((#\x :meta)))

(defclass drei-command-table (standard-command-table)
  ()
  (:documentation "This class is used to provide the kind of
indirection we need to support syntax-specific command tables in
Drei. Commands should *NOT* be added to it."))

(defmethod additional-command-tables append ((frame application-frame)
                                             (command-table syntax-command-table))
  "This method allows users of Drei to extend syntaxes with new,
app-specific commands, as long as they inherit from a Drei class
and specialise a method for it."
  (additional-command-tables *current-window* command-table))

(defmethod command-table-inherit-from ((table drei-command-table))
  (let ((syntax-table (command-table *current-syntax*)))
    (list* syntax-table
           (when (use-editor-commands-p syntax-table)
             'editor-table)
           (additional-command-tables *current-window* table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The basic Drei class.

(defclass drei ()
  ((buffer :initform (make-instance 'drei-buffer) :initarg :buffer :accessor buffer)
   (point :initform nil :initarg :point :accessor point)
   (mark :initform nil :initarg :mark :accessor mark)
   (top :reader top)
   (bot :reader bot)
   (cursor-x :initform 2)
   (cursor-y :initform 2)
   (auto-fill-mode :initform nil :accessor auto-fill-mode)
   (auto-fill-column :initform 70 :accessor auto-fill-column)
   (isearch-mode :initform nil :accessor isearch-mode)
   (isearch-states :initform '() :accessor isearch-states)
   (isearch-previous-string :initform nil :accessor isearch-previous-string)
   (query-replace-mode :initform nil :accessor query-replace-mode)
   (query-replace-state :initform nil :accessor query-replace-state)
   (region-visible-p :initform nil :accessor region-visible-p)
   (full-redisplay-p :initform nil :accessor full-redisplay-p)
   ;; for next-line and previous-line commands
   (goal-column :initform nil :accessor goal-column)
   ;; for dynamic abbrev expansion
   (original-prefix :initform nil :accessor original-prefix)
   (prefix-start-offset :initform nil :accessor prefix-start-offset)
   (dabbrev-expansion-mark :initform nil :accessor dabbrev-expansion-mark)
   (overwrite-mode :initform nil :accessor overwrite-mode)
   (%view :initform (make-instance 'drei-textual-view)
          :initarg :view
          :accessor view
          :documentation "The CLIM view that will be used
whenever this Drei is being displayed. During redisplay, the
`stream-default-view' of the output stream will be temporarily
bound to this value.")
   (%kill-ring :initform (make-instance 'kill-ring :max-size 7)
               :initarg :kill-ring
               :accessor kill-ring
               :type kill-ring
               :documentation "The kill ring object associated
with the Drei instance.")
   (%previous-command :initform nil
                      :accessor previous-command
                      :documentation "The previous CLIM command
executed by this Drei instance. May be NIL if no command has been
executed.")
   (%point-cursor :accessor point-cursor
                  :initarg :point-cursor
                  :type cursor
                  :documentation "The cursor object associated
with point. This is guaranteed to be displayed
on top of all other cursors.")
   (%cursors :accessor cursors
             :initform '()
             :documentation "A list of which cursors are
associated with the Drei instance. During redisplay,
`display-drei-cursor' is called on each element of
this list.")
   (%editor-pane :reader editor-pane
                 :initarg :editor-pane
                 :type clim-stream-pane
                 :documentation "The stream or pane that the Drei
instance will perform output to.")
   (%active :accessor active
            :initarg :active
            :initform t
            :type boolean
            :documentation "Whether or not the Drei instance is
\"active\". The precise definition of what this means depends on
the concrete Drei implementation. ")
   (%minibuffer :initform nil
                :accessor minibuffer
                :initarg :minibuffer
                :type (or minibuffer-pane null)
                :documentation "The minibuffer pane (or null)
associated with the Drei instance. This may be NIL.")
   (%command-table :initform (make-instance 'drei-command-table
                                            :name 'drei-dispatching-table)
                   :reader command-table
                   :initarg :command-table
                   :type standard-command-table
                   :documentation "The command table used for
looking up commands for the Drei instance. Has a sensible
default, don't override it unless you know what you are doing."))
  (:default-initargs :active t :editable-p t)
  (:documentation "The abstract Drei class that maintains
standard Drei editor state. It should not be directly
instantiated, a subclass implementing specific behavior (a Drei
variant) should be used instead."))

(defmethod (setf active) :after (new-val (drei drei))
  (mapcar #'(lambda (cursor)
              (setf (active cursor) new-val))
          (cursors drei)))

(defmethod initialize-instance :after ((object drei) &rest args &key
                                       active single-line (editable-p t))
  (declare (ignore args))
  (setf (single-line-p (implementation (buffer object))) single-line)
  (with-slots (buffer point mark top bot scan) object
    (setf (read-only-p buffer) (not editable-p))
    (setf point (clone-mark (point buffer)))
    (when (null point)
      (setf point (clone-mark (low-mark buffer) :right)))
    (when (null mark)
      (setf mark (clone-mark (low-mark buffer) :right)))
    (setf top (clone-mark (low-mark buffer) :left)
          bot (clone-mark (high-mark buffer) :right)))
  (with-accessors ((point-cursor point-cursor)
                   (cursors cursors)) object
    (setf point-cursor
          (make-instance 'point-cursor
                         :drei-instance object
                         :active active))
    (push point-cursor cursors)
    (push (make-instance 'mark-cursor
                         :drei-instance object
                         :active active)
          cursors)))

(defmethod (setf buffer) :after (buffer (object drei))
  (with-slots (point mark top bot) object
    (setf point (clone-mark (point buffer))
          mark (clone-mark (low-mark buffer) :right)
          top (clone-mark (low-mark buffer) :left)
          bot (clone-mark (high-mark buffer) :right))))

;; Main redisplay entry point.
(defgeneric display-drei (drei)
  (:documentation "Display the given Drei instance."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Some standard building block machinery.

(defmacro handling-drei-conditions (&body body)
  "Evaluate `body' while handling Drei user notification
signals. The handling consists of displaying their meaning to the
user in the minibuffer. This is the macro that ensures conditions
such as `motion-before-end' does not land the user in the
debugger."
  `(handler-case (progn ,@body)
     (offset-before-beginning ()
       (beep) (display-message "Beginning of buffer"))
     (offset-after-end ()
       (beep) (display-message "End of buffer"))
     (motion-before-beginning ()
       (beep) (display-message "Beginning of buffer"))
     (motion-after-end ()
       (beep) (display-message "End of buffer"))
     (no-expression ()
       (beep) (display-message "No expression around point"))
     (no-such-operation ()
       (beep) (display-message "Operation unavailable for syntax"))
     (buffer-read-only ()
       (beep) (display-message "Buffer is read only"))
     ;; I'd like a situation where all conditions that should result
     ;; in the user being informed of something just inherit from
     ;; `user-condition-mixin'. Enumerating all possible conditions
     ;; where is not scaleable.
     (user-condition-mixin (c)
       (beep) (with-minibuffer-stream (minibuffer)
                (let ((*print-escape* nil))
                  (princ c minibuffer))))))

(defmacro with-bound-drei-special-variables ((drei-instance &key
                                              current-buffer
                                              current-window
                                              current-mark
                                              current-point
                                              current-syntax
                                              kill-ring
                                              minibuffer
                                              command-parser
                                              partial-command-parser
                                              previous-command
                                              prompt)
                                              &body body)
  "Evaluate `body' with a set of Drei special
variables (`*current-buffer*', `*current-window*',
`*current-mark*', `*current-point*', `*current-syntax*',
`*kill-ring*', `*minibuffer*', `*command-parser*',
`*partial-command-parser*', `*previous-command*',
`*extended-command-prompt*') bound to their proper values, taken
from `drei-instance'. The keyword arguments can be used to
provide forms that will be used to obtain values for the
respective special variables, instead of finding their value in
`drei-instance'. This macro binds all of the usual Drei special
variables, but also some CLIM special variables needed for
ESA-style command parsing."
  (once-only (drei-instance)
    `(let* ((*current-buffer* ,(or current-buffer `(buffer ,drei-instance)))
            (*current-window* ,(or current-window drei-instance))
            (*current-mark* ,(or current-mark `(mark ,drei-instance)))
            (*current-point* ,(or current-point `(point ,drei-instance)))
            (*current-syntax* ,(or current-syntax `(syntax *current-buffer*)))
            (*kill-ring* ,(or kill-ring `(kill-ring ,drei-instance)))
            (*minibuffer* ,(or minibuffer `(or (minibuffer ,drei-instance) *minibuffer*)))
            (*command-parser* ,(or command-parser ''esa-command-parser))
            (*partial-command-parser* ,(or partial-command-parser ''esa-partial-command-parser))
            (*previous-command* ,(or previous-command `(previous-command ,drei-instance)))
            (*extended-command-prompt* ,(or prompt "Extended command: ")))
       ,@body)))

(defgeneric invoke-performing-drei-operations (drei continuation &key with-undo update-syntax redisplay)
  (:documentation "Invoke `continuation', setting up and
performing the operations specified by the keyword arguments for
the given Drei instance."))

(defmethod invoke-performing-drei-operations ((drei drei) (continuation function)
                                              &key with-undo (update-syntax t) (redisplay t))
  (with-accessors ((buffer buffer)) drei
    (with-undo ((when with-undo (list buffer)))
      (funcall continuation))
    (when (or update-syntax redisplay)
      (update-syntax buffer (syntax buffer)))
    (unless with-undo
      (clear-undo-history (buffer drei)))
    (when redisplay
      (etypecase drei
        (pane
         (redisplay-frame-pane *application-frame* drei))
        (t
         (display-drei drei))))))

(defmacro performing-drei-operations ((drei &rest args &key with-undo
                                            (update-syntax t)
                                            (redisplay t))
                                      &body body)
  "Provide various Drei maintenance services around the
evaluation of `body'. This macro provides a convenient way to
perform some operations on a Drei, and make sure that they are
properly reflected in the undo tree, that the Drei is
redisplayed, the syntax updated, etc. Exactly what is done can be
controlled via the keyword arguments. Note that if `with-undo' is
false, the *entire* undo history will be cleared after `body' has
been evaluated. This macro expands into a call to
`invoke-performing-drei-operations'."
  (declare (ignore with-undo update-syntax redisplay))
  `(invoke-performing-drei-operations ,drei (lambda ()
                                              ,@body)
                                      ,@args))

(defmacro with-drei-options ((drei &key
                                   (syntax nil syntax-provided-p)
                                   keep-syntax)
                             &body body)
  "Evaluate `body' with the Drei instance `drei' changed to
reflect the given options. The Drei instance will revert to the
original options after `body' has been evaluated."
  ;; Build a list consisting of lists of three elements, the first
  ;; element being how to save the old value, the second element being
  ;; how to set the new value, the third element being how to restore
  ;; the old value.
  (once-only (drei syntax)
    (let (triple-list)
      (when syntax-provided-p
        (push (list (unless keep-syntax
                      `(old-syntax (syntax (buffer ,drei))))
                    `(setf (syntax (buffer ,drei))
                           (etypecase ,syntax
                             (string (make-instance (or (syntax-from-name ,syntax)
                                                        (error "No such syntax: ~A" ,syntax))
                                                    :buffer (buffer ,drei)))
                             (symbol (make-instance ,syntax
                                                    :buffer (buffer ,drei)))
                             (syntax ,syntax)))
                    (unless keep-syntax
                      `(setf (syntax (buffer ,drei)) old-syntax)))
              triple-list))
      `(progn
         (check-type ,drei drei)
         (let ,(remove-if #'null (mapcar #'first triple-list))
           ,@(remove-if #'null (mapcar #'second triple-list))
           (unwind-protect (progn ,@body)
             ,@(remove-if #'null (mapcar #'third triple-list))))))))

(defgeneric invoke-accepting-from-user (drei continuation)
  (:documentation "Set up `drei' and the environment so that
calls to `accept' will behave properly. Then call
`continuation'."))

(defmethod invoke-accepting-from-user ((drei drei) (continuation function))
  ;; By default, everything should work.
  (funcall continuation))

(defmacro accepting-from-user ((drei) &body body)
  "Modidfy `drei' and the environment so that calls to `accept'
can be done to arbitrary streams from within `body'. Or, at
least, make sure the Drei instance will not be a problem. When
Drei calls a command, it will be wrapped in this macro, so it
should be safe to use `accept' within Drei commands. This macro
expands into a call to `invoke-accepting-from-user'."
  `(invoke-accepting-from-user ,drei #'(lambda () ,@body)))

;;; Plain `execute-frame-command' is not good enough for us. Our
;;; event-handler method uses this function to invoke commands, note
;;; that it is also responsible for updating the syntax of the buffer
;;; in the pane.
(defgeneric execute-drei-command (drei-instance command)
  (:documentation "Execute `command' for `drei'. This is the
standard function for executing Drei commands - it will take care
of reporting to the user if a condition is signalled, updating
the syntax, setting the `previous-command' of `drei' and
recording the operations performed by `command' for undo."))

(defmethod execute-drei-command ((drei drei) command)
  (with-accessors ((buffer buffer)) drei
    (let ((*standard-input* (or *minibuffer* *standard-input*)))
      (performing-drei-operations (drei :redisplay nil
                                        :update-syntax t
                                        :with-undo t)
        (handling-drei-conditions
          (accepting-from-user (drei)
            (apply (command-name command) (command-arguments command)))
          (setf (previous-command drei) command))))))
