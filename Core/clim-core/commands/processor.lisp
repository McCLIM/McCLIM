;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by Robert Strandh (strandh@labri.u-bordeaux.fr)
;;;  (c) copyright 2002 by Tim Moore (moore@bricoworks.com)
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; The command processor.
;;;

(in-package #:clim-internals)

(defvar +null-command+ '(com-null-command))
(defvar *command-dispatchers* '(#\:))
(defvar *command-name-delimiters* '(command-delimiter))
(defvar *command-argument-delimiters* '(command-delimiter))

(defmacro with-command-table-keystrokes ((keystroke-var command-table)
                                         &body body)
  `(let ((,keystroke-var
           (let ((result '()))
             (map-over-command-table-keystrokes*
              (lambda (item table)
                (declare (ignore table))
                (let ((keystroke (command-menu-item-keystroke item)))
                  (setf result (adjoin keystroke result :test #'equal))))
              (find-command-table ,command-table))
             result)))
     ,@body))

(defun command-line-command-parser (command-table stream)
  (let ((command-name nil))
    (with-delimiter-gestures (*command-name-delimiters* :override t)
      ;; While reading the command name we want use the history of the
      ;; (accept 'command ...) that's calling this function.
      (setq command-name (accept `(command-name :command-table ,command-table)
                                 :stream stream :prompt nil :history nil)))
    (let ((gesture (read-gesture :stream stream :timeout 0 :peek-p t)))
      (when (and gesture (activation-gesture-p gesture))
        (return-from command-line-command-parser `(,command-name))))
    (collect (command-args)
      (with-delimiter-gestures (*command-argument-delimiters* :override t)
        (flet ((arg-parser (stream ptype &rest args
                            &key (mentioned-default nil md-p)
                            &allow-other-keys)
                 ;; :DOCUMENTATION is supposed to be used only in parsers
                 ;; for the sake of providing hints for the operator.
                 (with-keywords-removed (args (:documentation :mentioned-default))
                   (let ((result
                           (apply #'accept ptype :stream stream
                                  (if (null md-p)
                                      args
                                      (list* :default mentioned-default args)))))
                     (command-args result)
                     result)))
               (del-parser (stream type)
                 ;; Eat the delimiter or the activator.
                 (case type
                   ((:arg :key :val)
                    (let ((gesture (read-gesture :stream stream :peek-p t)))
                      (when (or (null gesture)
                                (activation-gesture-p gesture))
                        (read-gesture :stream stream)
                        (return-from command-line-command-parser
                          (cons command-name (command-args))))
                      (when (delimiter-gesture-p gesture)
                        (read-gesture :stream stream))))
                   (:opt
                    (input-editor-format stream "(keywords) "))
                   ((:pos :end)))))
          ;; The NULL context is a barrier which prevents other commands from
          ;; being selected during reading arguments and cancelling the current
          ;; command composition. When the argument's type is COMMAND, then a new
          ;; context will be estabilished by the parser and it will be possible to
          ;; select commands. -- jd 2020-09-03
          (with-input-context ('null :override t) ()
              (parse-command command-name #'arg-parser #'del-parser stream))))
      (cons command-name (command-args)))))

(defun command-line-command-unparser (command-table stream command &rest args)
  (let* ((command-name (command-name command))
         (command-name-ptype `(command-name :command-table ,command-table)))
    (apply #'present command-name command-name-ptype :stream stream args)
    (when-let ((command-args (command-arguments command)))
      (flet ((arg-parser (stream ptype &rest parser-args)
               (declare (ignore parser-args))
               (unless command-args
                 (return-from command-line-command-unparser))
               (let ((value (pop command-args)))
                 (if (unsupplied-argument-p value)
                     (with-text-face (stream :italic)
                       (write-string "<unsupplied>" stream))
                     (apply #'present value ptype :stream stream args))
                 value))
             (del-parser (stream type)
               (case type
                 ((:arg :key :val)
                  (write-char #\space stream))
                 ((:pos :opt :end)))))
        (parse-command command-name #'arg-parser #'del-parser stream)))))

(defun accept-partial-command-1 (stream label partial-command)
  (fresh-line stream)
  (let* ((command-name (command-name partial-command))
         (command-arguments (command-arguments partial-command))
         (initial-query
           (block nil
             (parse-command command-name
                      (let ((args command-arguments))
                        (lambda (stream ptype &key query-identifier
                                 &allow-other-keys)
                          (declare (ignore stream ptype))
                          (when (or (null args)
                                    (unsupplied-argument-p (pop args)))
                            (return query-identifier))))
                      (constantly nil) stream))))
    ;; KLUDGE the macro ACCEPTING-VALUES is not yet defined so we directly
    ;; invoke the function that it expands to.
    (flet ((accepting-body (stream)
             (let ((command-args command-arguments))
               (collect (all-args)
                 (block nil
                   (flet ((arg-parser (stream ptype &rest args)
                            (let* ((arg-p (consp command-args))
                                   (arg (pop command-args))
                                   (missingp (or (null arg-p)
                                                 (unsupplied-argument-p arg))))
                              (multiple-value-bind (value ptype changedp)
                                  (if missingp
                                      (apply #'accept ptype :stream stream args)
                                      (apply #'accept ptype :stream stream :default arg args))
                                (declare (ignore ptype))
                                (when (and missingp (not changedp))
                                  (setf value *unsupplied-argument-marker*))
                                (all-args value)
                                value)))
                          (del-parser (stream type)
                            (declare (ignore stream))
                            (when (eq type :opt)
                              (return))))
                     (parse-command command-name #'arg-parser #'del-parser stream)))
                 (setf command-arguments (all-args))
                 `(,command-name ,@(all-args))))))
      (invoke-accepting-values stream #'accepting-body
                               :align-prompts t :label label
                               :initially-select-query-identifier initial-query))))

;;; In order for this to work, the input-editing-stream must implement a
;;; method for the nonstandard function `input-editing-stream-output-record'.
(defun command-line-read-remaining-arguments-for-partial-command
    (command-table stream partial-command start-position)
  (declare (ignore start-position))
  (let* ((name (command-name partial-command))
         (cli (command-line-name-for-command name command-table :errorp nil))
         (label (format nil "You are being prompted for arguments to ~S." cli))
         (label* (format nil "~a~%Please supply all arguments." label)))
    (flet ((loop-accepting-values (stream)
             (loop for title = label then label*
                   do (setf partial-command
                            (accept-partial-command-1
                             stream title partial-command))
                   while (partial-command-p partial-command)
                   finally (return partial-command))))
      (if (encapsulating-stream-p stream)
          (let ((interactor (encapsulating-stream-stream stream)))
            (with-bounding-rectangle* (:x1 x1 :y2 y2)
                (input-editing-stream-output-record stream)
              ;; Start the dialog below the editor area
              (letf (((stream-cursor-position interactor) (values x1 y2)))
                ;; FIXME error checking needed here? -- moore
                (loop-accepting-values interactor))))
          (loop-accepting-values stream)))))

;;; Presentation translators (also for menu-item) will throw the command
;;; presentation in the command input context so we need only to ensure that
;;; the event queue progresses by reading gestures in a loop. -- jd 2021-11-05
(defun menu-command-parser (command-table stream)
  (declare (ignore command-table))
  (loop (read-gesture :stream stream)))

;;; This never invokes accepting-values because there is no need for that,
;;; there is no linear order for command arguments when there is no cli.
(defun menu-read-remaining-arguments-for-partial-command
    (command-table stream partial-command start-position)
  (declare (ignore command-table start-position))
  (let* ((command-name (pop partial-command)))
    (collect (command)
      (command command-name)
      (flet ((arg-parser (stream ptype &rest args)
               (declare (ignore args))
               (let ((arg (pop partial-command)))
                 (when (unsupplied-argument-p arg)
                   (setf arg
                         (with-input-context (ptype :override t)
                             (object)
                             (loop (read-gesture :stream stream))
                           (t object))))
                 (prog1 arg
                   (command arg)))))
        (parse-command command-name #'arg-parser (constantly nil) stream)
        (command)))))

(defvar *command-parser* #'command-line-command-parser)
(defvar *command-unparser* #'command-line-command-unparser)
(defvar *partial-command-parser*
  #'command-line-read-remaining-arguments-for-partial-command)

(defun ensure-complete-command (command command-table stream)
  (unless command
    (return-from ensure-complete-command))
  (unless (consp command)
    (setf command (list command)))
  (let ((canonical (partial-command-from-name (car command))))
    ;; When the command has more arguments than its "canonical form", that is
    ;; the command with all required arguments filled, that means that it has
    ;; all required arguments *and* some optional arguments.
    (unless (> (length command) (length canonical))
      (map-into canonical #'identity command)
      (setf command canonical))
    (if (partial-command-p command)
        (funcall *partial-command-parser*
                 command-table stream command
                 (position *unsupplied-argument-marker* command))
        command)))

(defun read-command (command-table
                     &key (stream *standard-input*)
                          (command-parser *command-parser*)
                          (command-unparser *command-unparser*)
                          (partial-command-parser *partial-command-parser*)
                          use-keystrokes)
  (let ((*command-parser* command-parser)
        (*command-unparser* command-unparser)
        (*partial-command-parser* partial-command-parser))
    (cond (use-keystrokes
           (with-command-table-keystrokes (keystrokes command-table)
             (read-command-using-keystrokes command-table keystrokes
                                            :stream stream)))
          ((or (typep stream 'interactor-pane)
               (typep stream 'input-editing-stream))
           (handler-case
               (multiple-value-bind (command ptype)
                   (accept `(command :command-table ,command-table)
                           :stream stream
                           :prompt nil
                           :default +null-command+
                           :default-type 'null-command)
                 (if (eq ptype 'null-command)
                     nil
                     (ensure-complete-command command command-table stream)))
             ((or simple-parse-error input-not-of-required-type)  (c)
               (beep)
               (fresh-line *query-io*)
               (princ c *query-io*)
               (terpri *query-io*)
               nil)))
          ;; KLUDGE when there is no interactor we estabilish the input
          ;; context manually and call the parser. This is to prevent
          ;; opening an unnecessary input editor i.e for menu parsers.
          (t
           (with-input-context (`(command :command-table ,command-table))
               (object)
               (funcall *command-parser* command-table stream)
             (t
              (ensure-complete-command object command-table stream)))))))

(defun read-command-using-keystrokes
    (command-table keystrokes
     &key (stream *standard-input*)
          (command-parser *command-parser*)
          (command-unparser *command-unparser*)
          (partial-command-parser *partial-command-parser*))
  (let ((*command-parser* command-parser)
        (*command-unparser* command-unparser)
        (*partial-command-parser* partial-command-parser)
        (*accelerator-gestures* keystrokes))
    (let ((command
            (handler-bind
                ((accelerator-gesture
                   (lambda (c)
                     ;; LOOKUP-KEYSTROKE-COMMAND-ITEM is specified to return a
                     ;; gesture if no command can be found. To avoid problems
                     ;; in ENSURE-COMPLETE-COMMAND we sanitize the returned
                     ;; value. -- jd 2022-02-18
                     (let* ((event (accelerator-gesture-event c))
                            (numeric-arg (accelerator-gesture-numeric-argument c))
                            (command (lookup-keystroke-command-item
                                      event command-table :numeric-arg numeric-arg)))
                       (when (typep command `(or symbol (cons symbol)))
                         (return-from read-command-using-keystrokes
                           (ensure-complete-command command command-table stream)))))))
              (read-command command-table :stream stream))))
      (ensure-complete-command command command-table stream))))


(defmethod display-command-table-menu ((command-table standard-command-table)
                                       (stream fundamental-output-stream)
                                       &rest args
                                       &key max-width max-height n-rows n-columns
                                       x-spacing y-spacing initial-spacing
                                       row-wise (cell-align-x :left)
                                       (cell-align-y :top) (move-cursor t))
  (formatting-item-list (stream :max-width max-width
                                :max-height max-height
                                :n-rows n-rows
                                :n-columns n-columns
                                :x-spacing x-spacing
                                :y-spacing y-spacing
                                :initial-spacing initial-spacing
                                :row-wise row-wise
                                :move-cursor move-cursor)
    (map-over-command-table-menu-items
     #'(lambda (item-name accelerator item)
         (declare (ignore accelerator))
         (formatting-cell (stream :align-x cell-align-x :align-y cell-align-y)
           (cond ((eq (command-menu-item-type item) :menu)
                  (with-text-family (stream :serif)
                    (with-text-face (stream '(:bold :italic))
                      (write-string item-name stream))
                    (terpri stream))
                  (surrounding-output-with-border (stream)
                    (apply #'display-command-table-menu
                           (find-command-table (command-menu-item-value item))
                           stream args)))
                 ((eq (command-menu-item-type item) :command)
                  (let ((name (command-menu-item-name item))
                        (value (command-menu-item-value item)))
                    (with-output-as-presentation (stream value 'command)
                      (write-string name stream)))))))
     command-table)))

#+nyi ;; IMPLEMENTME
(defun menu-choose-command-from-command-table
    (command-table
     &key associated-window default-style label
          cache unique-id id-test cache-value cache-test))

(define-presentation-type command-table ())

(define-presentation-method present
    (object (type command-table) stream (view textual-view)
     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (let ((name (command-table-name object)))
    (if acceptably
        (prin1 name stream)
        (princ name stream))))

(define-presentation-method accept
    ((type command-table) stream (view textual-view) &key)
  (multiple-value-bind (table success string)
      (completing-from-suggestions (stream)
        (loop
           for name being the hash-key of *command-tables*
             using (hash-value table)
           do (suggest (symbol-name name) table)))
    (if success
        table
        (simple-parse-error "~A is not the name of a command table" string))))

;;; A type indicating empty input. For example, if one types <space>
;;; to get the default value of a keyword argument, and then types
;;; <return>, we don't want to see "None" in the output history. So,
;;; we define this subtype of null that has no output. It's not meant
;;; to be read if the form is ever accepted again.

(define-presentation-type empty ()
  :inherit-from 'null)

(define-presentation-method present
    (object (type empty) stream (view textual-view) &key &allow-other-keys)
  (declare (ignore object stream)))


;;;  27.6.1 Command Presentation Types

(define-presentation-type command-name
    (&key (command-table (frame-command-table *application-frame*)))
  :inherit-from t)

(define-presentation-method presentation-typep (object (type command-name))
  (and (command-accessible-in-command-table-p object command-table)
       (command-enabled object *application-frame*)))

(define-presentation-method presentation-subtypep
    ((type command-name) maybe-supertype)
  (with-presentation-type-parameters (command-name maybe-supertype)
    (let ((super-table command-table))
      (with-presentation-type-parameters (command-name type)
        (command-table-inherits-from-p super-table command-table)))))

(define-presentation-method present
    (object (type command-name) stream (view textual-view) &key)
  (let ((command-line-name (command-line-name-for-command object command-table
                                                          :errorp nil)))
    (if command-line-name
        (write-string command-line-name stream)
        (prin1 object stream))))

(define-presentation-method accept
    ((type command-name) stream (view textual-view) &key)
  (flet ((generator (string suggester)
           (declare (ignore string))
           (let ((possibilities nil))
             (map-over-command-table-names
              (lambda (cline-name command-name)
                (unless (member command-name
                                (disabled-commands *application-frame*))
                  (pushnew (cons cline-name command-name) possibilities
                           :key #'car :test #'string=)))
              command-table)
             (loop for (cline-name . command-name) in possibilities
                   do (funcall suggester cline-name command-name)))))
    ;; KLUDGE: here, we used to bind the frame's command table so that
    ;; a test with COMMAND-ENABLED passed with the command-table being
    ;; accepted from.  Unfortunately, that interfered awfully with
    ;; drei gadgets and their command-table inheritance; the dynamic
    ;; inheritance from (frame-command-table *application-frame*) [
    ;; which is needed to get things like frame menu items and other
    ;; commands to work ] works really badly if (frame-command-table
    ;; *application-frame*) is set/bound to the dispatching
    ;; command-table itself.
    ;;
    ;; Instead we now use the knowledge of how disabled commands are
    ;; implemented to satisfy the constraint that only enabeled
    ;; commands are acceptable (with the "accessible" constraint being
    ;; automatically satisfied by the generator mapping over the
    ;; command-table).
    ;;
    ;; This means that someone implementing their own version of the
    ;; "enabled-command" protocol will lose.  Sorry.  CSR, 2009-02-17
    (multiple-value-bind (object success string)
        (complete-input stream
                        #'(lambda (so-far mode)
                            (complete-from-generator so-far
                                                     #'generator
                                                     '(#\space)
                                                     :action mode))
                        :partial-completers '(#\space))
      (if success
          (values object type)
          (simple-parse-error "No command named ~S" string)))))

(define-presentation-type command
    (&key (command-table (frame-command-table *application-frame*)))
  :inherit-from t)

(define-presentation-method presentation-typep (object (type command))
  (let ((name (command-name object)))
    (and (command-accessible-in-command-table-p name command-table)
         (command-enabled name *application-frame*))))

(define-presentation-method presentation-subtypep
    ((type command) maybe-supertype)
  (with-presentation-type-parameters (command maybe-supertype)
    (let ((super-table command-table))
      (with-presentation-type-parameters (command type)
        (command-table-inherits-from-p super-table command-table)))))

(define-presentation-method present
    (object (type command) stream (view textual-view)
     &rest args &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (apply *command-unparser* command-table stream object args))

(define-presentation-method accept
    ((type command) stream (view textual-view) &key)
  (let ((command-table (find-command-table command-table))
        (start-position (and (getf options :echo t)
                             (input-editing-stream-p stream)
                             (stream-scan-pointer stream))))
    (flet ((maybe-replace-input (input options)
             (when (and start-position (getf options :echo t))
               ;; Rescan is nil in case the clicked on thing returned
               ;; something unparseable. -- heshrobe 2020-05-15
               (presentation-replace-input stream input type view
                                           :buffer-start start-position
                                           :rescan nil))
             input)
           (handle-command (command)
             (ensure-complete-command command command-table stream)))
      (with-input-context (type)
          (object type event options)
          (if-let ((command (funcall *command-parser* command-table stream)))
            (values (maybe-replace-input (handle-command command) '()) type)
            (simple-parse-error "Empty command"))
        (command
         (values (maybe-replace-input (handle-command object) options) type))))))

;;; A presentation type for empty input at the command line; something for
;;; read-command to supply as a default.  The command is defined in
;;; builtin-commands.lisp.

(define-presentation-type null-command ()
  :inherit-from '(command :command-table global-command-table))

(define-presentation-method presentation-typep (object (type null-command))
  (eq (command-name object) 'com-null-command))

(define-presentation-method present
    (object (type null-command) stream (view textual-view) &key)
  (declare (ignore object stream view)))


(define-presentation-type command-or-form
    (&key (command-table (frame-command-table *application-frame*)))
  :inherit-from t)

;;; What's the deal with this use of with-input-context inside of
;;; accept? When this accept method is called, we want to accept both
;;; commands and forms via mouse clicks, both before and after the
;;; command dispatch character is typed. But command translators to
;;; command or form won't be applicable... translators from command or
;;; form to command-or-form won't help either because translators aren't
;;; applied more than once.
;;;
;;; By calling the input context continuation directly -- which was
;;; established by the call to (accept 'command-or-form ...) -- we let it do
;;; all the cleanup like replacing input, etc.

(define-presentation-method accept
    ((type command-or-form) stream (view textual-view) &key)
  (let ((command-ptype `(command :command-table ,command-table)))
    (with-input-context (`(or ,command-ptype form))
        (object type event options)
        (let ((initial-char (read-gesture :stream stream :peek-p t)))
          (if (member initial-char *command-dispatchers*)
              (progn
                (read-gesture :stream stream)
                (accept command-ptype :stream stream :view view
                                      :prompt nil :history 'command))
              (accept 'form :stream stream :view view
                            :prompt nil :history 'command-or-form)))
      (t
       (funcall (cdar *input-context*) object type event options)))))

;;; MENU-ITEM is used as a default presentation type for displaying
;;; objects in the menu. Display object is described in the spec of
;;; the function FRAME-MANAGER-MENU-CHOOSE. From this description it
;;; seems that anything may be a display object, so we fix the
;;; predicate PRESENTATION-TYPEP to always return T. -- jd 2020-07-02
(define-presentation-type menu-item ())

(define-presentation-method presentation-typep (object (type menu-item))
  (declare (ignore object))
  t)

(define-presentation-translator menu-item-command-to-command
    (menu-item command global-command-table
     :tester ((object event frame)
              (and (typep object '%menu-item)
                   (member (command-menu-item-type object)
                           '(:function :command))
                   (let ((command (extract-menu-item-command object event)))
                     (command-enabled (command-name command) frame))))
     :tester-definitive t)
    (object event)
  (extract-menu-item-command object event))

;;; Output destination extension for DEFINE-COMMAND
;;;
;;; The backend part, i.e. INVOKE-WITH-STANDARD-OUTPUT and the destination
;;; classes, is defined in clim-basic/extended-streams/stream-output.lisp.

(define-presentation-method accept
    ((type stream-destination) stream (view textual-view)
     &key (default '*standard-output*) (prompt "stream form"))
  (let ((dest (eval (accept 'form :stream stream :view view
                                  :default default :prompt prompt))))
    (if (and (streamp dest)
             (output-stream-p dest))
        (make-instance 'stream-destination :destination-stream dest)
        (input-not-of-required-type dest type))))

(define-presentation-method accept
    ((type file-destination) stream (view textual-view)
     &key (prompt "destination file"))
  (let ((path (accept 'pathname :stream stream :view view :prompt prompt)))
    ;; Give subclasses a shot
    (with-presentation-type-decoded (type-name) type
      (make-instance type-name :file path))))

(define-presentation-method accept
    ((type output-destination) stream (view textual-view)
     &key (default "Stream") (prompt nil))
  (let ((type (accept `(member-alist ,*output-destination-types*)
                      :stream stream :view view
                      :default default :prompt prompt
                      :additional-delimiter-gestures '(#\space))))
    (read-gesture :stream stream)
    (accept type :stream stream :view view :prompt nil)))
