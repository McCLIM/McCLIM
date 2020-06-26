;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2001,2002 by Tim Moore (moore@bricoworks.com)

;;; Definitions for the standard presentation types and generic functions

(in-package :clim-internals)

;;; The presentation type for T is the built-in type T. The correspondence is
;;; established by hand in presentations.lisp.
;(define-presentation-type t ())

;;; AUTO-ACTIVATE is described in the Franz user guide; it controls whether an
;;; accepting an expression returns immediately after typing the closing
;;; delimiter -- a la Genera et Mac Lisp -- or if an activation gesture is
;;; required.
;;; PRESERVE-WHITESPACE controls whether the accept method uses read or
;;; READ-PRESERVING-WHITESPACE. This is used in our redefinitions of read and
;;; READ-PRESERVING-WHITESPACE that accept forms.

(define-presentation-type expression ()
  :options (auto-activate (preserve-whitespace t) (subform-read nil))
  :inherit-from t)

(define-presentation-type form ()
  :options (auto-activate (preserve-whitespace t) (subform-read nil))
  :inherit-from `((expression) :auto-activate ,auto-activate
                  :preserve-whitespace ,preserve-whitespace
                  :subform-read ,subform-read ))

(define-presentation-generic-function %presentation-default-processor
    presentation-default-processor
    (type-key parameters default type &key default-type))

(define-default-presentation-method presentation-default-processor
    (default type &key (default-type nil default-type-p))
  (values default (if default-type-p
                      default-type
                      type)))

;;; I believe this obsolete... --moore
(defmethod presentation-replace-input
    ((stream input-editing-stream) object type view
     &key (buffer-start nil buffer-start-supplied-p)
     (rescan nil rescan-supplied-p)
     query-identifier
     (for-context-type type))
  (declare (ignore query-identifier))
  (let ((result (present-to-string object type
                                   :view view :acceptably nil
                                   :for-context-type for-context-type)))
    (apply #'replace-input stream result `(,@(and buffer-start-supplied-p
                                                  `(:buffer-start
                                                    ,buffer-start))
                                           ,@(and rescan-supplied-p
                                                  `(:rescan ,rescan))))))

;;; Presentation histories.

;;; This allocates a hash table even if the frame doesn't support
;;; histories!  I'm over it already. -- moore
(defclass presentation-history-mixin ()
  ((presentation-history :accessor frame-presentation-history
                         :initform (make-hash-table :test #'eq)))
  (:documentation "Mixin class for frames that implements presentation
type histories"))

(define-presentation-generic-function %presentation-type-history
    presentation-type-history
  (type-key parameters type))

;;; This function should exist for convenience even if not mentioned in the
;;; spec.

(defun presentation-type-history (type)
  (funcall-presentation-generic-function presentation-type-history type))

(defclass presentation-history ()
  ((stack :accessor presentation-history-array
          :initform (make-array 1 :fill-pointer 0
                                  :adjustable t)
          :documentation "The history, with the newest objects at
the end of the array. Should contain conses with the car being
the object and the cdr being the type.")
   (pointer :accessor presentation-history-pointer
            :initform nil
            :documentation "The index of the \"current\" object,
used when navigating the history. If NIL, means that no
navigation has yet been performed."))
  (:documentation "Class for objects that contain the history for
a specific type."))

(define-default-presentation-method presentation-type-history (type)
  (if (and *application-frame*
           (frame-maintain-presentation-histories *application-frame*))
      (with-presentation-type-decoded (name)
        type
        (let* ((ptype (get-ptype-metaclass name))
               (history (history ptype)))
          (case history
            ((t)
             (let* ((history-table (frame-presentation-history
                                    *application-frame*))
                    (history-object (gethash name history-table)))
               (unless history-object
                 (setf history-object
                       (make-instance 'presentation-history)
                       (gethash name history-table)
                       history-object))
               history-object))
            ((nil)
             nil)
            (otherwise
             (funcall-presentation-generic-function presentation-type-history
                                                    type)))))))

;;; Not in the spec, but I think this is necessary (or at any rate, the easiest
;;; way) to control whether or not to use histories in a given context.
(define-presentation-generic-function %presentation-type-history-for-stream
    presentation-type-history-for-stream
  (type-key parameters type stream)
  (:documentation "Returns a type history or nil for a presentation TYPE and
STREAM. This is used by McCLIM to decide whether or not to use histories in a
given situation. A primary method specialized on just the type should
call-next-method to get the \"real\" answer based on the stream type."))

(define-default-presentation-method presentation-type-history-for-stream
    (type stream)
  (declare (ignore stream))
  nil)

;;; method for clim-stream-pane in panes.lisp

(define-presentation-method presentation-type-history-for-stream
    ((type t) (stream input-editing-stream))
  ;; What is the purpose of this? Makes stuff harder to do, so
  ;; commented out...
  ;;(if (not (stream-rescanning-p stream))
  ;;       (funcall-presentation-generic-function presentation-type-history type)
  ;;       nil)
  (funcall-presentation-generic-function presentation-type-history type))

(defun presentation-history-insert (history object ptype)
  "Unconditionally insert `object' as an input of presentation
type `type' at the top of the presentation history `history', as
the most recently added object."
  (vector-push-extend (cons object ptype)
                      (presentation-history-array history)))

(defun presentation-history-top (history ptype)
  "Find the topmost (most recently added object) of `history'
that is of the presentation type `ptype' or a subtype. Two values
will be returned, the object and the presentation type of the
object. If no applicable object can be found, these values will
both be NIL."
  (loop
     with array = (presentation-history-array history)
     for index from (1- (fill-pointer array)) downto 0
     for (object . object-ptype) = (aref array index)
     do
     (when (presentation-subtypep object-ptype ptype)
       (return (aref array index)))
     finally (return (values nil nil))))

(defun presentation-history-reset-pointer (history)
  "Set the pointer to point at the object most recently added
object."
  (setf (presentation-history-pointer history) nil))

(defun presentation-history-next (history ptype)
  "Go to the next input (forward in time) in `history' that is a
presentation-subtype of `ptype', respective to the pointer in
`history'. Returns two values: the found object and its
presentation type, both of which will be NIL if no applicable
object can be found."
  (with-accessors ((pointer presentation-history-pointer)
                   (array presentation-history-array)) history
    ;; If no navigation has been performed, we have no object to go
    ;; forwards to.
    (if (or (null pointer) (>= (1+ pointer) (length array)))
        (values nil nil)
        (progn
          (incf pointer)
          (destructuring-bind (object . object-ptype)
              (aref array pointer)
            (if object-ptype
                (if (presentation-subtypep object-ptype ptype)
                    (values object object-ptype)
                    (presentation-history-next history ptype))
                (values nil nil)))))))

(defun presentation-history-previous (history ptype)
  "Go to the previous input (backward in time) in `history' that
is a presentation-subtype of `ptype', respective to the pointer
in `history'. Returns two values: the found object and its
presentation type, both of which will be NIL if no applicable
object can be found."
  (with-accessors ((pointer presentation-history-pointer)
                   (array presentation-history-array)) history
    (if (and (numberp pointer) (zerop pointer))
        (values nil nil)
        (progn
          (cond ((and (numberp pointer) (plusp pointer))
                 (decf pointer))
                ((plusp (length array))
                 (setf pointer (1- (fill-pointer array)))))
          (if (and (numberp pointer) (array-in-bounds-p array pointer))
              (destructuring-bind (object . object-ptype)
                  (aref array pointer)
                (if object-ptype
                    (if (presentation-subtypep object-ptype ptype)
                        (values object object-ptype)
                        (progn (presentation-history-previous history ptype)))
                    (values nil nil)))
              (values nil nil))))))

(defmacro with-object-on-history ((history object ptype) &body body)
  "Evaluate `body' with `object' as `ptype' as the head (most
recently added object) on `history', and remove it again after
`body' has run. If `body' as `ptype' is already the head, the
history will be unchanged."
  (with-gensyms (added)
    `(let ((,added (presentation-history-add ,history ,object ,ptype)))
       (unwind-protect (progn ,@body)
         (when ,added
           (decf (fill-pointer (presentation-history-array ,history))))))))

(defun presentation-history-add (history object ptype)
  "Add OBJECT and PTYPE to the HISTORY unless they are already at the head of
 HISTORY"
  (multiple-value-bind (top-object top-ptype)
      (presentation-history-top history ptype)
    (unless (and top-ptype (eql object top-object) (equal ptype top-ptype))
      (presentation-history-insert history object ptype))))

(define-presentation-generic-function %accept accept
    (type-key parameters options type stream view
     &key default default-type &allow-other-keys))

(defvar *recursive-accept-p* nil)
(defvar *recursive-accept-1-p* nil)
(defvar *active-history-type* nil)

;;; The spec says "default-type most be a presentation type specifier", but the
;;; examples we have imply that default-type is optional, so we'll be liberal
;;; in what we accept.

(defun accept (type &rest rest-args &key
               (stream *standard-input*)
               (view nil viewp)
               (default nil defaultp)
               (default-type nil default-type-p)
               provide-default insert-default replace-input
               (history nil historyp)   ; true default supplied below
               active-p                 ; Don't think this will be used
               prompt prompt-mode display-default
               query-identifier
               activation-gestures additional-activation-gestures
               delimiter-gestures additional-delimiter-gestures)
  (declare (ignore insert-default replace-input active-p prompt prompt-mode
                   display-default query-identifier
                   activation-gestures additional-activation-gestures
                   delimiter-gestures additional-delimiter-gestures))
  (handler-bind ((abort-gesture (lambda (condition)
                                  (signal condition) ;; to give outer handlers a chance to say "I know how to handle this"
                                  (abort condition))))
    (let* ((real-type (expand-presentation-type-abbreviation type))
           (real-default-type (cond (default-type-p
                                     (expand-presentation-type-abbreviation
                                      default-type))
                                    ((or defaultp provide-default)
                                     real-type)
                                    (t nil)))
           (real-history-type (cond ((null historyp) real-type)
                                    ((null history) nil)
                                    (t (expand-presentation-type-abbreviation
                                        history))))
           (*recursive-accept-p* *recursive-accept-1-p*)
           (*recursive-accept-1-p* t))
      (with-keywords-removed (rest-args (:stream))
        (when (or default-type-p defaultp)
          (setf rest-args
                (list* :default-type real-default-type rest-args)))
        (when historyp
          (setf rest-args (list* :history real-history-type rest-args)))
        (cond ((and viewp (symbolp view))
               (setf rest-args
                     (list* :view (funcall #'make-instance view) rest-args)))
              ((consp view)
               (setf rest-args
                     (list* :view (apply #'make-instance view) rest-args))))
        ;; Presentation type history interaction. According to the spec,
        ;; if provide-default is true, we take the default from the
        ;; presentation history. In addition, we'll implement the Genera
        ;; behavior of temporarily putting the default on the history
        ;; stack so the user can conveniently suck it in.
        (labels ((get-history ()
                   (when real-history-type
                     (funcall-presentation-generic-function
                      presentation-type-history-for-stream
                      real-history-type stream)))
                 (do-accept (args)
                   (apply #'stream-accept stream real-type args)))
          (let* ((default-from-history (and (not defaultp) provide-default))
                 (history (get-history))
                 (results
                  (multiple-value-list
                   (if history
                       (unwind-protect
                            (let ((*active-history-type* real-history-type))
                              (cond (defaultp
                                     (with-object-on-history
                                         (history default real-default-type)
                                       (do-accept rest-args)))
                                    (default-from-history
                                     (multiple-value-bind
                                           (history-default history-type)
                                         (presentation-history-top history
                                                                   real-default-type)
                                       (do-accept (if history-type
                                                      (list* :default history-default
                                                             :default-type history-type
                                                             rest-args)
                                                      rest-args))))
                                    (t (do-accept rest-args))))
                         (unless *recursive-accept-p*
                           (presentation-history-reset-pointer (get-history))))
                       (do-accept rest-args))))
                 (results-history (get-history)))
            (when results-history
              (presentation-history-add results-history
                                        (car results)
                                        real-type))
            (values-list results)))))))

(defmethod stream-accept ((stream standard-extended-input-stream) type
                          &rest args
                          &key (view (stream-default-view stream))
                          &allow-other-keys)
  (apply #'prompt-for-accept stream type view args)
  (apply #'accept-1 stream type args))

(defmethod stream-accept ((stream #.*string-input-stream-class*) type
                          &key (view (stream-default-view stream))
                          (default nil defaultp)
                          (default-type nil default-type-p)
                          (activation-gestures nil activationsp)
                          (additional-activation-gestures
                           nil additional-activations-p)
                          (delimiter-gestures nil delimitersp)
                          (additional-delimiter-gestures
                           nil additional-delimiters-p)
                          &allow-other-keys)
  (with-activation-gestures ((if additional-activations-p
                                 additional-activation-gestures
                                 activation-gestures)
                             :override activationsp)
    (with-delimiter-gestures ((if additional-delimiters-p
                                  additional-delimiter-gestures
                                  delimiter-gestures)
                              :override delimitersp)
      (multiple-value-bind (object object-type)
          (apply-presentation-generic-function
           accept
           type stream view
           `(,@(and defaultp `(:default ,default))
             ,@(and default-type-p `(:default-type ,default-type))))
        (values object (or object-type type))))))

(defun accept-1 (stream type &key
                 (view (stream-default-view stream))
                 (default nil defaultp)
                 (default-type nil default-type-p)
                 provide-default
                 insert-default
                 (replace-input t)
                 history
                 active-p
                 prompt
                 prompt-mode
                 display-default
                 query-identifier
                 (activation-gestures nil activationsp)
                 (additional-activation-gestures nil additional-activations-p)
                 (delimiter-gestures nil delimitersp)
                 (additional-delimiter-gestures nil  additional-delimiters-p))
  (declare (ignore provide-default history active-p
                   prompt prompt-mode
                   display-default query-identifier))
  (when (and defaultp (not default-type-p))
    (error ":default specified without :default-type"))
  (when (and activationsp additional-activations-p)
    (error "only one of :activation-gestures or ~
            :additional-activation-gestures may be passed to accept."))
  (unless (or activationsp additional-activations-p *activation-gestures*)
    (setq activation-gestures *standard-activation-gestures*))
  (let ((sensitizer-object nil)
        (sensitizer-type 'null))
    (with-input-editing
        (stream
         :input-sensitizer #'(lambda (stream cont)
                               (with-output-as-presentation
                                   (stream sensitizer-object sensitizer-type)
                                 (funcall cont))))
      (with-input-position (stream)     ; support for calls to replace-input
        (when (and insert-default
                   (not (stream-rescanning-p stream)))
          ;; Insert the default value to the input stream. It should
          ;; become fully keyboard-editable. We do not want to insert
          ;; the default if we're rescanning, only during initial
          ;; setup.
          (presentation-replace-input stream default default-type view))
        (setf (values sensitizer-object sensitizer-type)
              (with-input-context (type)
                  (object object-type event options)
                (with-activation-gestures ((if additional-activations-p
                                               additional-activation-gestures
                                               activation-gestures)
                                           :override activationsp)
                  (with-delimiter-gestures ((if additional-delimiters-p
                                                additional-delimiter-gestures
                                                delimiter-gestures)
                                            :override delimitersp)
                    (let ((accept-results nil))
                      (handle-empty-input (stream)
                          (setq accept-results
                                (multiple-value-list
                                 (if defaultp
                                     (funcall-presentation-generic-function
                                      accept type stream view
                                      :default default
                                      :default-type default-type)
                                     (funcall-presentation-generic-function
                                      accept type stream view))))
                        ;; User entered activation or delimiter
                        ;; gesture without any input.
                        (if defaultp
                            (progn
                              (presentation-replace-input
                               stream default default-type view :rescan nil))
                            (simple-parse-error
                             "Empty input for type ~S with no supplied default"
                             type))
                        (setq accept-results (list default default-type)))
                      ;; Eat trailing activation gesture
                      ;; XXX what about pointer gestures?
                      ;; XXX and delimiter gestures?
                      (unless *recursive-accept-p*
                        (let ((ag (read-char-no-hang stream nil stream t)))
                          (unless (or (null ag) (eq ag stream))
                            (unless (activation-gesture-p ag)
                              (unread-char ag stream)))))
                      (values (car accept-results) (if (cdr accept-results)
                                                       (cadr accept-results)
                                                       type)))))
                ;; A presentation was clicked on, or something
                (t
                 (when (and replace-input
                            (getf options :echo t)
                            (not (stream-rescanning-p stream)))
                   (presentation-replace-input stream object object-type view
                                               :rescan nil))
                 (values object object-type))))
        ;; Just to make it clear that we're returning values
        (values sensitizer-object sensitizer-type)))))

(defmethod prompt-for-accept ((stream t)
                              type view
                              &rest accept-args
                              &key &allow-other-keys)
  (declare (ignore view))
  (apply #'prompt-for-accept-1 stream type accept-args))

(defun prompt-for-accept-1 (stream type
                            &key
                            (default nil defaultp)
                            (default-type type)
                            (insert-default nil)
                            (prompt t)
                            (prompt-mode :normal)
                            (display-default prompt)
                            &allow-other-keys)
  (flet ((display-using-mode (stream prompt default)
           (ecase prompt-mode
             (:normal
              (if *recursive-accept-p*
                  (input-editor-format stream "(~A~@[[~A]~]) " prompt default)
                  (input-editor-format stream "~A~@[[~A]~]: " prompt default)))
             (:raw
              (input-editor-format stream "~A" prompt)))))
    (let ((prompt-string (if (eq prompt t)
                             (format nil "~:[Enter ~;~]~A"
                                     *recursive-accept-p*
                                     (describe-presentation-type type nil nil))
                             prompt))
          ;; Don't display the default in the prompt if it is to be
          ;; inserted into the input stream.
          (default-string (and defaultp
                                (not insert-default)
                                display-default
                                (present-to-string default default-type))))
      (cond ((null prompt)
           nil)
          (t
           (display-using-mode stream prompt-string default-string))))))

(defmethod prompt-for-accept ((stream #.*string-input-stream-class*)
                              type view
                              &rest other-args
                              &key &allow-other-keys)

  (declare (ignore type view other-args))
  nil)

;;; For ACCEPT-FROM-STRING, use this barebones input-editing-stream.
(defclass string-input-editing-stream (input-editing-stream fundamental-character-input-stream)
  ((input-buffer :accessor stream-input-buffer)
   (insertion-pointer :accessor stream-insertion-pointer
                      :initform 0
                      :documentation "This is not used for anything at any point.")
   (scan-pointer :accessor stream-scan-pointer
                 :initform 0
                 :documentation "This is not used for anything at any point."))
  (:documentation "An implementation of the input-editing stream
protocol retrieving gestures from a provided string."))

(defmethod initialize-instance :after ((stream string-input-editing-stream)
                                       &key (string (error "A string must be provided"))
                                       (start 0) (end (length string))
                                       &allow-other-keys)
  (setf (stream-input-buffer stream)
        (replace (make-array (- end start) :fill-pointer (- end start))
                 string :start2 start :end2 end)))

(defmethod stream-element-type ((stream string-input-editing-stream))
  'character)

(defmethod close ((stream string-input-editing-stream) &key abort)
  (declare (ignore abort)))

(defmethod stream-peek-char ((stream string-input-editing-stream))
  (or (stream-read-gesture stream :peek-p t)
      :eof))

(defmethod stream-read-char-no-hang ((stream string-input-editing-stream))
  (if (> (stream-scan-pointer stream) (length (stream-input-buffer stream)))
   :eof
   (stream-read-gesture stream)))

(defmethod stream-read-char ((stream string-input-editing-stream))
  (stream-read-gesture stream))

(defmethod stream-listen ((stream string-input-editing-stream))
  (< (stream-scan-pointer stream) (length (stream-input-buffer stream))))

(defmethod stream-unread-char ((stream string-input-editing-stream) char)
  (stream-unread-gesture stream char))

(defmethod invoke-with-input-editor-typeout ((stream string-input-editing-stream) continuation
                                             &key erase)
  (declare (ignore erase)))

(defmethod input-editor-format ((stream string-input-editing-stream) format-string
                                &rest args)
  (declare (ignore args)))

(defmethod stream-rescanning-p ((stream string-input-editing-stream))
  t)

(defmethod reset-scan-pointer ((stream string-input-editing-stream)
                               &optional scan-pointer)
  (declare (ignore scan-pointer)))

(defmethod immediate-rescan ((stream string-input-editing-stream)))

(defmethod queue-rescan ((stream string-input-editing-stream)))

(defmethod rescan-if-necessary ((stream string-input-editing-stream)
                                &optional inhibit-activation)
  (declare (ignore inhibit-activation)))

(defmethod erase-input-buffer ((stream string-input-editing-stream)
                                &optional start-position)
  (declare (ignore start-position)))

(defmethod redraw-input-buffer ((stream string-input-editing-stream)
                                &optional start-position)
  (declare (ignore start-position)))

(defmethod stream-process-gesture ((stream string-input-editing-stream) gesture type)
  (when (characterp gesture)
    (values gesture type)))

(defmethod stream-read-gesture ((stream string-input-editing-stream)
                                &key peek-p &allow-other-keys)
  (unless (> (stream-scan-pointer stream) (length (stream-input-buffer stream)))
    (prog1 (if (= (stream-scan-pointer stream) (length (stream-input-buffer stream)))
               (second (first (gethash (first *activation-gestures*)
                                       climi::*gesture-names*))) ; XXX - will always be non-NIL?
               (aref (stream-input-buffer stream) (stream-scan-pointer stream)))
      (unless peek-p
        (incf (stream-scan-pointer stream))))))

(defmethod stream-unread-gesture ((stream string-input-editing-stream) gesture)
  (decf (stream-scan-pointer stream)))

(defmethod stream-accept ((stream string-input-editing-stream) type &rest args)
  (apply #'accept-1 stream type args))

;;; XXX This needs work! It needs to do everything that accept does for
;;; expanding ptypes and setting up recursive call procesusing
(defun accept-from-string (type string
                           &rest args
                           &key view
                           (default nil defaultp)
                           (default-type nil default-type-p)
                           (activation-gestures nil activationsp)
                           (additional-activation-gestures
                            nil
                            additional-activations-p)
                           (delimiter-gestures nil delimitersp)
                           (additional-delimiter-gestures
                            nil
                            additional-delimiters-p)
                           (start 0)
                           (end (length string)))
  (declare (ignore view))
  ;; XXX work in progress here.
  (with-activation-gestures ((if additional-activations-p
                                 additional-activation-gestures
                                 activation-gestures)
                             :override activationsp)
    (with-delimiter-gestures ((if additional-delimiters-p
                                  additional-delimiter-gestures
                                  delimiter-gestures)
                              :override delimitersp)))
  (when (zerop (- end start))
    (if defaultp
        (return-from accept-from-string (values default
                                                (if default-type-p
                                                    default-type
                                                    type)
                                                0))
        (simple-parse-error "Empty string")))
  (let ((stream (make-instance 'string-input-editing-stream
                 :string string :start start :end end)))
    (multiple-value-bind (val ptype)
        (with-keywords-removed (args (:start :end))
          (apply #'stream-accept stream type :history nil :view +textual-view+ args))
      (values val ptype (+ (stream-scan-pointer stream) start)))))

(define-presentation-generic-function %presentation-refined-position-test
    presentation-refined-position-test
  (type-key parameters options type record x y))

(define-default-presentation-method presentation-refined-position-test
    (type record x y)
  (declare (ignore type))
  ;;; output-record-hit-detection-rectangle* has already been called
  (let ((single-box (presentation-single-box record)))
    (if (or (eq single-box t) (eq single-box :position))
        t
        (labels ((tester (record)
                   (typecase record
                     (displayed-output-record
                      (return-from presentation-refined-position-test t))
                     (compound-output-record
                      (map-over-output-records-containing-position
                       #'tester record x y))
                     (t nil))))
          (tester record)
          nil))))

(defun presentation-contains-position (record x y)
  (let ((single-box (presentation-single-box record)))
    (multiple-value-bind (min-x min-y max-x max-y)
        (output-record-hit-detection-rectangle* record)
      (if (and (<= min-x x max-x) (<= min-y y max-y))
          (if (or (null single-box) (eq single-box :highlighting))
              (funcall-presentation-generic-function
               presentation-refined-position-test
               (presentation-type record) record x y)
              t)
          nil))))

(defun accept-using-read (stream ptype &key ((:read-eval *read-eval*) nil))
  (let* ((token (read-token stream)))
    (if (string= "" token)
	(values nil ptype)
	(let ((result (handler-case (read-from-string token)
			   (error (c)
			     (declare (ignore c))
			     (simple-parse-error "Error parsing ~S for presentation type ~S"
						 token
						 ptype)))))
	     (if (presentation-typep result ptype)
		 (values result ptype)
		 (input-not-of-required-type result ptype))))))

(defun accept-using-completion (type stream func
                                &rest complete-with-input-key-args)
  "A wrapper around complete-with-input that returns the presentation type with
  the completed object."
  (multiple-value-bind (object success input)
      (apply #'complete-input stream func complete-with-input-key-args)
    (if success
        (values object type)
        (simple-parse-error "Error parsing ~S for presentation type ~S"
                            input
                            type))))

;;; When no accept method has been defined for a type, allow some kind of
;;; input.  The accept can be satisfied with pointer input, of course, and this
;;; allows the clever user a way to input the type at the keyboard, using #. or
;;; some other printed representation.
;;;
;;; XXX Once we "go live" we probably want to disable this, probably with a
;;; beep and warning that input must be clicked on.

(define-default-presentation-method accept
    (type stream (view textual-view) &key default default-type)
  (declare (ignore default default-type))
  (accept-using-read stream type :read-eval t))

;;; The presentation types

(define-presentation-method presentation-typep (object (type t))
  (declare (ignore object))
  t)

(define-presentation-method present (object (type t)
                                            stream
                                            (view textual-view)
                                            &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (let ((*print-readably* acceptably))
    (if acceptably
        (prin1 object stream)
        (princ object stream))))

(define-presentation-type nil ())

(define-presentation-method presentation-typep (object (type nil))
  (declare (ignore object))
  nil)

(define-presentation-type null ()
  :inherit-from t)

(define-presentation-method presentation-typep (object (type null))
  (eq object nil))

(define-presentation-method present (object (type null)
                                     stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore object acceptably for-context-type))
  (write-string "None" stream))

(define-presentation-method accept ((type null) stream (view textual-view)
                                    &key)
  (values (completing-from-suggestions (stream)
            (suggest "None" nil)
            (suggest "" nil))))

(define-presentation-type boolean ()
  :inherit-from t)

(define-presentation-method presentation-typep (object (type boolean))
  (or (eq object t) (eq object nil)))

(define-presentation-method present (object (type boolean) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (if object
      (write-string "Yes" stream)
      (write-string "No" stream)))

(define-presentation-method accept ((type boolean) stream (view textual-view)
                                    &key)
  (accept-using-completion 'boolean
                           stream
                           #'(lambda (input-string mode)
                               (complete-from-possibilities
                                input-string
                                '(("yes" t) ("no" nil))
                                nil
                                :action mode))))

(define-presentation-type symbol ()
  :inherit-from 't)

(define-presentation-method presentation-typep (object (type symbol))
  (symbolp object))

(define-presentation-method present (object (type symbol) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (if acceptably
      (prin1 object stream)
      (princ object stream)))

(define-presentation-method accept ((type symbol) stream (view textual-view)
                                                  &key (default-type type)
                                                  default)
  (let ((read-result (accept-using-read stream type)))
    (if (and (null read-result) default)
        (values default default-type)
        (values read-result type))))

(define-presentation-type keyword () :inherit-from 'symbol)

(define-presentation-method presentation-typep (object (type keyword))
  (keywordp object))

(define-presentation-method present (object (type keyword) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (prin1 object stream))

(define-presentation-type blank-area ()
  :inherit-from t)

(define-presentation-method highlight-presentation ((type blank-area)
                                                    record
                                                    stream
                                                    state)
  (declare (ignore record stream state))
  nil)

;;; Do other slots of this have to be bound in order for this to be useful?
;;; Guess we'll see.
(defparameter *null-presentation* (make-instance 'standard-presentation
                                                 :object nil
                                                 :type 'blank-area
                                                 :view +textual-view+))

(define-presentation-type number ()
  :inherit-from 't)

(define-presentation-method presentation-typep (object (type number))
  (numberp object))



(define-presentation-type complex (&optional (type 'real))
  :inherit-from 'number)

(define-presentation-method presentation-typep (object (type complex))
  (and (complexp object)
       (typep (realpart object) type)
       (typep (imagpart object) type)))

(define-presentation-method presentation-subtypep ((type complex)
                                                   maybe-supertype)
  (with-presentation-type-parameters (complex type)
    (let ((component-type type))        ;i.e., the parameter named "type"
      (with-presentation-type-parameters (complex maybe-supertype)
        (let ((super-component-type type))
          (presentation-subtypep component-type super-component-type))))))

(define-presentation-method present (object (type complex) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (present (realpart object) (presentation-type-of (realpart object))
           :stream stream :view view :sensitive nil)
  (write-char #\Space stream)
  (present (imagpart object) (presentation-type-of (imagpart object))
           :stream stream :view view :sensitive nil))

(define-presentation-type real (&optional low high) :options ((base 10) radix)
                          :inherit-from 'number)

(define-presentation-method presentation-typep (object (type real))
  (and (realp object)
       (or (eq low '*)
           (<= low object))
       (or (eq high '*)
           (<= object high))))

(define-presentation-method present (object (type real) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((*print-base* base)
        (*print-radix* radix))
    (princ object stream)))

(define-presentation-method accept ((type real) stream (view textual-view) &key
                                                (default-type type)
                                                default)
  (let ((*read-base* base)
         (read-result (accept-using-read stream type)))
    (if (and (null read-result) default)
        (values default default-type)
        (values read-result type))))

;;; Define a method that will do the comparision for all real types.  It's
;;; already determined that that the numeric class of type is a subtype of
;;;supertype.

(defun number-subtypep (low high super-low super-high)
  (if (eq low '*)
      (unless (eq super-low '*)
        (return-from number-subtypep nil))
      (unless (or (eq super-low '*) (>= low super-low))
        (return-from number-subtypep nil)))
  (if (eq high '*)
      (unless (eq super-high '*)
        (return-from number-subtypep nil))
      (unless (or (eq super-high '*) (<= high super-high))
        (return-from number-subtypep nil)))
  t)

(define-presentation-type rational (&optional low high)
  :options ((base 10) radix)
  :inherit-from `((real ,low ,high) :base ,base :radix ,radix))

(define-presentation-method presentation-typep (object (type rational))
  (and (rationalp object)
       (or (eq low '*)
           (<= low object))
       (or (eq high '*)
           (<= object high))))

(define-presentation-method present (object (type rational) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((*print-base* base)
        (*print-radix* radix))
    (princ object stream)))

(define-presentation-type integer (&optional low high)
  :options ((base 10) radix)
  :inherit-from `((rational ,low ,high) :base ,base :radix ,radix))

(define-presentation-method presentation-typep (object (type integer))
  (and (integerp object)
       (or (eq low '*)
           (<= low object))
       (or (eq high '*)
           (<= object high))))

(define-presentation-method present (object (type integer) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((*print-base* base)
        (*print-radix* radix))
    (princ object stream)))

(define-presentation-type ratio (&optional low high)
  :options ((base 10) radix)
  :inherit-from `((rational ,low ,high) :base ,base :radix ,radix))

(define-presentation-method presentation-typep (object (type ratio))
  (and (not (integerp object))
       (rationalp object)
       (or (eq low '*)
           (<= low object))
       (or (eq high '*)
           (<= object high))))

(define-presentation-method present (object (type ratio) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((*print-base* base)
        (*print-radix* radix))
    (princ object stream)))

(define-presentation-type float (&optional low high)
  :options ((base 10) radix)
  :inherit-from `((real ,low ,high) :base ,base :radix ,radix))

(define-presentation-method presentation-typep (object (type float))
  (and (floatp object)
       (or (eq low '*)
           (<= low object))
       (or (eq high '*)
           (<= object high))))

(define-presentation-method present (object (type float) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((*print-base* base)
        (*print-radix* radix))
    (princ object stream)))

(macrolet ((frob (num-type)
             `(define-presentation-method presentation-subtypep ((type
                                                                  ,num-type)
                                                                 maybe-supertype)
                (with-presentation-type-parameters (,num-type maybe-supertype)
                  (let ((super-low low)
                        (super-high high))
                    (with-presentation-type-parameters (,num-type type)
                      (values (number-subtypep low high super-low super-high)
                              t)))))))
  (frob real)
  (frob rational)
  (frob ratio)
  (frob integer)
  (frob float))

(define-presentation-type character ()
  :inherit-from 't)

(define-presentation-method presentation-typep (object (type character))
  (characterp object))

(define-presentation-method present (object (type character) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (princ object stream))

(define-presentation-type string (&optional length)
  :inherit-from 't)

(define-presentation-method presentation-typep (object (type string))
  (and (stringp object)
       (or (eq length '*) (eql (length object) length))))

(define-presentation-method presentation-subtypep ((type string)
                                                   maybe-supertype)
  (with-presentation-type-parameters (string maybe-supertype)
    (let ((super-length length))
      (with-presentation-type-parameters (string type)
        (values (or (eq super-length '*)
                    (eql length super-length))
                t)))))

(define-presentation-method present (object (type string) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (if acceptably
      (prin1 object stream)
      (princ object stream)))

(define-presentation-method accept ((type string) stream (view textual-view)
                                    &key (default nil defaultp)
                                    (default-type type))
  (let ((result (read-token stream)))
    (cond ((numberp length)
           (if (eql length (length result))
               (values result type)
               (input-not-of-required-type result type)))
          ((and (zerop (length result)) defaultp)
           (values default default-type))
          (t (values result type)))))

(define-presentation-type pathname ()
  :options ((default-version :newest) default-type (merge-default t))
  :inherit-from 't)

(define-presentation-method presentation-typep (object (type pathname))
  (pathnamep object))

(define-presentation-method present ((object pathname) (type pathname)
                                     stream (view textual-view) &key)
  ;; XXX: We can only visually represent the pathname if it has a name
  ;; - making it wild is a compromise. If the pathname is completely
  ;; blank, we leave it as-is, though.

  ;; The above comment was meant to indicate that if the pathname had
  ;; neither a name NOR a directory, then it couldn't be visually
  ;; represented.  Some discussion has ensued on the possbility of
  ;; emitting something like "A pathname of type <foo>"
  ;; [2007/01/08:rpg]
  (let ((pathname (if (equal object #.(make-pathname))
                      object
                      (merge-pathnames object (make-pathname :name :wild)))))
    (declare (ignore pathname))
    (princ object stream)))

(define-presentation-method present ((object string) (type pathname)
                                     stream (view textual-view)
                                     &rest args &key)
  (apply-presentation-generic-function
   present (pathname object) type stream view args))

(defun filename-completer-get-directory (string)
  (flet
      ((deal-with-home (pathname his-directory)
         ;; SBCL (and maybe others) treat "~/xxx" specially, returning a pathname
         ;; whose directory is (:ABSOLUTE :HOME xxx)
         ;; But if you call Directory on that pathname the returned list
         ;; are all complete pathnames without the :Home part!.
         ;; So this replaces the :HOME with what it actually means
         (let* ((home-env-variable (get-environment-variable "HOME"))
                (home (loop for pos = 1 then (1+ next-pos)
                         for next-pos = (position #\/ home-env-variable :start pos)
                         collect (subseq home-env-variable pos next-pos)
                         until (null next-pos)))
                (new-directory (cons
                                (first his-directory)
                                (append home (rest (rest his-directory))))))
           (make-pathname :host (pathname-host pathname)
                          :device (pathname-device pathname)
                          :name (pathname-name pathname)
                          :version (pathname-version pathname)
                          :type (pathname-type pathname)
                          :directory new-directory))))
    (let* ((raw-pathname (pathname string))
           (raw-directory (pathname-directory raw-pathname))
           (original-pathname (if (and (listp raw-directory)
                                       (eql (first raw-directory) :absolute)
                                       (eql (second raw-directory) :Home))
                                  (deal-with-home raw-pathname raw-directory)
                                  raw-pathname))
           ;; Complete logical pathnames as well as regular pathnames
           ;; strategy is to keep track of both original string provided and translated string
           ;; but to return pathname built from original components except for the name.
           (logical-pathname-p (typep original-pathname 'logical-pathname))
           (actual-pathname (if logical-pathname-p
                                (translate-logical-pathname original-pathname)
                                original-pathname))
           ;; merge in *default-pathname-defaults*
           (merged-pathname (merge-pathnames actual-pathname))
           (search-pathname  (make-pathname :host (pathname-host merged-pathname)
                                            :device (pathname-device merged-pathname)
                                            :directory (pathname-directory merged-pathname)
                                            :version :unspecific
                                            :type :wild
                             :name :wild)))
      (values search-pathname (pathname-type actual-pathname) original-pathname))))

(defun filename-completer-get-candidates (search-pathname pathname-type original-pathname)
  (let ((orginal-is-logical-pathname (typep original-pathname 'logical-pathname))
        (completions (directory search-pathname #+sbcl :resolve-symlinks #+sbcl nil)))
    ;; Now prune out all completions that don't start with the string
    (when (null pathname-type)
      (flet ((legitimate-logical-pathname (name)
               (let ((word (string name)))
                 (loop for i below (length word)
                       for ch = (schar word i)
                       always (and (standard-char-p ch)
                                   (or (alphanumericp ch) (char= ch #\-)))))))
        (let ((new-completions nil))
          (loop for pathname in completions
                for pathname-name = (pathname-name pathname)
                for pathname-type = (pathname-type pathname)
                for pathname-directory = (pathname-directory pathname)
                for pathname-host = (pathname-host original-pathname)
                for pathname-device = (pathname-device original-pathname)
                do (cond
                     ;; meaning this is actually a directory
                     ((and (null pathname-name)
                           (null pathname-type))
                      (when (and (loop for word in  (butlast pathname-directory)
                                       always (legitimate-logical-pathname word))
                                 (legitimate-logical-pathname (first (last pathname-directory))))
                        (pushnew (if orginal-is-logical-pathname
                                     (make-pathname :host pathname-host
                                                    :device pathname-device
                                                    :directory (first (last pathname-directory))
                                                    :name nil
                                                    :type nil)
                                     (make-pathname :host pathname-host
                                                    :device pathname-device
                                                    :directory (butlast pathname-directory)
                                                    :name (first (last pathname-directory))
                                                    :type nil))
                                 new-completions)))
                     (t
                      (when (or (not orginal-is-logical-pathname)
                                (and (legitimate-logical-pathname pathname-name)
                                     (legitimate-logical-pathname pathname-type)))
                        (pushnew (make-pathname :host pathname-host
                                                :device pathname-device
                                                :directory (pathname-directory original-pathname)
                                                :name pathname-name
                                                :type pathname-type)
                                 new-completions)))))
          (nreverse new-completions))))))


(defun filename-completer (string action)
  (multiple-value-bind (search-pathname pathname-type original-pathname)
      (filename-completer-get-directory string)
    (let ((candidates (filename-completer-get-candidates search-pathname pathname-type original-pathname)))
        (complete-from-possibilities (namestring original-pathname) candidates '(#\Space)
                                     :action action
                                     :name-key #'namestring
                                     :value-key #'identity))))

(define-presentation-method accept ((type pathname) stream (view textual-view)
                                    &key (default *default-pathname-defaults* defaultp)
                                    ((:default-type accept-default-type) type))
  (multiple-value-bind (pathname success string)
      (complete-input stream
                      #'filename-completer
                      :allow-any-input t)
    (cond ((and pathname success)
           (values (if merge-default
                       (progn
                         (unless (or (pathname-type pathname)
                                     (null default-type))
                           (setf pathname (make-pathname :defaults pathname
                                                         :type default-type)))
                         (merge-pathnames pathname default default-version))
                       pathname)
                   type))
          ((and (zerop (length string))
                defaultp)
           (values default accept-default-type))
          (t (values string 'string)))))

(defmethod presentation-replace-input :around
    ((stream input-editing-stream)
     (object pathname) (type (eql 'pathname))
     view &rest args &key &allow-other-keys)
  ;; This is fully valid and compliant, but it still smells slightly
  ;; like a hack.
  (let ((name (pathname-name object))
        (directory (when (pathname-directory object)
                     (directory-namestring object)))
        (type (pathname-type object))
        (string "")
        (old-insp (stream-insertion-pointer stream)))
    (setf string (or directory string))
    (setf string (concatenate 'string string
                              (cond ((and name type)
                                     (file-namestring object))
                                    (name name)
                                    (type (subseq
                                           (namestring
                                            (make-pathname
                                             :name " "
                                             :type type))
                                           1)))))
    (apply #'replace-input stream string args)
    (when directory
      (setf (stream-insertion-pointer stream)
            (+ old-insp (if directory (length directory) 0)))
      ;; If we moved the insertion pointer, this might be a good idea.
      (redraw-input-buffer stream old-insp))))

(defgeneric default-completion-name-key (item))

(defmethod default-completion-name-key ((item string))
  item)

(defmethod default-completion-name-key ((item null))
  "NIL")

(defmethod default-completion-name-key ((item cons))
  (string (car item)))

(defmethod default-completion-name-key ((item symbol))
  (string-capitalize (symbol-name item)))

(defmethod default-completion-name-key (item)
  (princ-to-string item))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; This function is copied from CLIM Franz code
  (defun highlight-completion-choice (continuation object stream)
    (with-text-face (stream :bold)
      (funcall continuation object stream)))

  (defconstant +completion-options+
    '((name-key 'default-completion-name-key)
      documentation-key
      (partial-completers '(#\Space))
      (printer #'write-token)
      (highlighter #'highlight-completion-choice))))

(define-presentation-type completion (sequence
                                      &key (test 'eql) (value-key 'identity))
  :options #.+completion-options+
  :inherit-from t)

(define-presentation-method presentation-typep (object (type completion))
  (map nil #'(lambda (obj)
               (when (funcall test object (funcall value-key obj))
                 (return-from presentation-typep t)))
       sequence)
  nil)

;;; Useful for subtype comparisons for several of the "member" style types

(defun sequence-subset-p (seq1 test1 value-key1 seq2 test2 value-key2)
  (let ((test-fun (if (eq test1 test2)
                      test1
                      ;; The object has to pass both type's equality test
                      #'(lambda (obj1 obj2)
                          (and (funcall test1 obj1 obj2)
                               (funcall test2 obj1 obj2))))))
    (map nil #'(lambda (type-obj)
                 (unless (find (funcall value-key1 type-obj)
                               seq2
                               :test test-fun :key value-key2)
                   (return-from sequence-subset-p nil)))
         seq1)
    t))

(define-presentation-method presentation-subtypep ((type completion)
                                                   maybe-supertype)
  (with-presentation-type-parameters (completion maybe-supertype)
    (let ((super-sequence sequence)
          (super-test test)
          (super-value-key value-key))
      (with-presentation-type-parameters (completion type)
        (values (sequence-subset-p sequence test value-key
                                   super-sequence super-test super-value-key)
                t)))))

(define-presentation-method present (object (type completion) stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((obj-pos (position object sequence :test test :key value-key)))
    (if obj-pos
        (write-string (funcall name-key (elt sequence obj-pos)) stream)
        ;; Should define a condition type here.
        (error "~S is not of presentation type ~S" object type))))

(define-presentation-method accept ((type completion)
                                    stream
                                    (view textual-view)
                                    &key)
  (accept-using-completion (make-presentation-type-specifier
                            `(completion ,@parameters)
                            options)
                           stream
                           #'(lambda (input-string mode)
                               (complete-from-possibilities
                                input-string
                                sequence
                                partial-completers
                                :action mode
                                :name-key name-key
                                :value-key value-key))
                           :partial-completers partial-completers))

(define-presentation-type-abbreviation member (&rest elements)
  (make-presentation-type-specifier `(completion ,elements)
                                    :name-key name-key
                                    :documentation-key documentation-key
                                    :partial-completers partial-completers
                                    :printer printer
                                    :highlighter highlighter)
  :options #.+completion-options+)

(define-presentation-type-abbreviation member-sequence (sequence
                                                        &key (test 'eql testp))
  (make-presentation-type-specifier
   `(completion ,sequence ,@(and testp `(:test ,test)))
   :name-key name-key
   :documentation-key documentation-key
   :partial-completers partial-completers
   :printer printer
   :highlighter highlighter)
  :options #.+completion-options+)

(defun member-alist-value-key (element)
  (cond ((atom element)
         element)
        ((atom (cdr element))
         (cdr element))
        ((null (cddr element))
         (cadr element))
        (t (getf (cdr element) :value))))

(defun member-alist-doc-key (element)
  (if (and (consp element) (consp (cdr element)) (consp (cddr element)))
      (getf (cdr element) :documentation)))

(define-presentation-type-abbreviation member-alist (alist
                                                     &key (test 'eql testp))
  (make-presentation-type-specifier
   `(completion ,alist ,@(and testp `(:test ,test))
                :value-key member-alist-value-key)
   :name-key name-key
   :documentation-key documentation-key
   :partial-completers partial-completers
   :printer printer
   :highlighter highlighter)
  :options ((name-key 'default-completion-name-key)
            (documentation-key 'member-alist-doc-key)
            (partial-completers '(#\Space))
            (printer #'write-token)
            (highlighter #'highlight-completion-choice)))

(define-presentation-type subset-completion (sequence
                                             &key (test 'eql)
                                             (value-key 'identity))
  :options ((name-key 'default-completion-name-key)
            documentation-key
            (partial-completers '(#\Space))
            (separator #\,)
            (echo-space t)
            (printer #'write-token)
            (highlighter #'highlight-completion-choice))
  :inherit-from t)

(define-presentation-method presentation-typep (object
                                                (type subset-completion))
  (map nil #'(lambda (obj)
               (unless (find obj sequence :test test :key value-key)
                 (return-from presentation-typep nil)))
       object)
  t)

(define-presentation-method presentation-subtypep ((type subset-completion)
                                                   maybe-supertype)
  (with-presentation-type-parameters (subset-completion maybe-supertype)
    (let ((super-sequence sequence)
          (super-test test)
          (super-value-key value-key))
      (with-presentation-type-parameters (subset-completion type)
        (values (sequence-subset-p sequence test value-key
                                   super-sequence super-test super-value-key)
                t)))))

(define-presentation-method present ((object list) (type subset-completion)
                                     stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for tail on object
        for (obj) = tail
        do (progn
             (present obj (presentation-type-of object)
                        :stream stream :view view
                        :acceptably acceptably
                        :sensitive nil)
             (when (cdr tail)
               (if acceptably
                   (princ separator stream)
                   (terpri stream))))))

(define-presentation-method present ((object vector) (type subset-completion)
                                     stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for i from 0 below (length object)
        for obj = (aref object i)
        do (progn
             (present obj (presentation-type-of object)
                        :stream stream :view view
                        :acceptably acceptably
                        :sensitive nil)
             (when (< i (1- (length object)))
               (if acceptably
                   (princ separator stream)
                   (terpri stream))))))

;;; XXX is it a typo in the spec that subset, subset-sequence and subset-alist
;;; have the same options as completion, and not subset-completion?

(define-presentation-type-abbreviation subset (&rest elements)
  (make-presentation-type-specifier `(subset-completion ,elements)
                                    :name-key name-key
                                    :documentation-key documentation-key
                                    :partial-completers partial-completers
                                    :printer printer
                                    :highlighter highlighter)
  :options #.+completion-options+)

(define-presentation-type-abbreviation subset-sequence (sequence
                                                        &key (test 'eql testp))
  (make-presentation-type-specifier
   `(subset-completion ,sequence ,@(and testp `(:test ,test)))
   :name-key name-key
   :documentation-key documentation-key
   :partial-completers partial-completers
   :printer printer
   :highlighter highlighter)
  :options #.+completion-options+)

(define-presentation-type-abbreviation subset-alist (alist
                                                     &key (test 'eql testp))
  (make-presentation-type-specifier
   `(subset-completion ,alist ,@(and testp `(:test ,test))
                       :value-key member-alist-value-key)
   :name-key name-key
   :documentation-key documentation-key
   :partial-completers partial-completers
   :printer printer
   :highlighter highlighter)
  :options ((name-key 'default-completion-name-key)
            (documentation-key 'member-alist-doc-key)
            (partial-completers '(#\Space))
            (printer #'write-token)
            (highlighter #'highlight-completion-choice)))

(define-presentation-type sequence (type)
  :options ((separator #\,) (echo-space t))
  :inherit-from 't
  :parameters-are-types t)

(define-presentation-method presentation-type-specifier-p ((type sequence))
  (and (listp type)
       (consp (rest type))
       (presentation-type-specifier-p (second type))))

(define-presentation-method presentation-typep (object (type sequence))
  ;; XXX TYPE here is the sequence element type, not the whole type specifier
  (unless (or (listp object) (vectorp object))
    (return-from presentation-typep nil))
  (let ((real-type (expand-presentation-type-abbreviation type)))
    (map nil #'(lambda (obj)
                 (unless (presentation-typep obj real-type)
                   (return-from presentation-typep nil)))
         object)
    t))

(define-presentation-method presentation-subtypep ((type sequence)
                                                   maybe-supertype)
  (with-presentation-type-parameters (sequence type)
    ;; now TYPE is bound to the parameter TYPE
    (let ((real-type (expand-presentation-type-abbreviation type)))
      (with-presentation-type-parameters (sequence maybe-supertype)
        (let ((real-super-type (expand-presentation-type-abbreviation type)))
          (presentation-subtypep real-type real-super-type))))))

(define-presentation-method present ((object list) (type sequence)
                                     stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for tail on object
        for (obj) = tail
        do (progn
             (present obj type          ; i.e., the type parameter
                        :stream stream :view view
                        :acceptably acceptably
                        :sensitive nil)
             (when (cdr tail)
               (write-char separator stream)))))

(define-presentation-method present ((object vector) (type sequence)
                                     stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for i from 0 below (length object)
        for obj = (aref object i)
        do (progn
             (present obj type          ; i.e., the type parameter
                        :stream stream :view view
                        :acceptably acceptably
                        :sensitive nil)
             (when (< i (1- (length object)))
               (write-char separator stream)))))


(define-presentation-method accept ((type sequence)
                                    stream
                                    (view textual-view)
                                    &key)
  (loop
     with separators = (list separator)
     for element = (accept type         ; i.e., the type parameter
                           :stream stream
                           :view view
                           :prompt nil
                           :additional-delimiter-gestures separators)
     collect element
     do (progn
          (when (not (eql (peek-char nil stream nil nil) separator))
            (loop-finish))
          (read-char stream)
          (when echo-space
            ;; Make the space a noise string
            (input-editor-format stream " ")))))


(define-presentation-type sequence-enumerated (&rest types)
  :options ((separator #\,) (echo-space t))
  :inherit-from 't
  :parameters-are-types t)

(define-presentation-method presentation-typep (object
                                                (type sequence-enumerated))
  (unless (or (listp object) (vectorp object))
    (return-from presentation-typep nil))
  (map nil #'(lambda (obj type)
               (let ((real-type (expand-presentation-type-abbreviation type)))
                 (unless (presentation-typep obj real-type)
                   (return-from presentation-typep nil))))
       object
       types)
  t)

(define-presentation-method presentation-subtypep ((type sequence-enumerated)
                                                   maybe-supertype)
  (with-presentation-type-parameters (sequence-enumerated maybe-supertype)
    (let ((supertypes types))
      (with-presentation-type-parameters (sequence-enumerated type)
        (unless (eql (length supertypes) (length types))
          (return-from presentation-subtypep (values nil t)))
        (map nil
             #'(lambda (element-type element-supertype)
                 (let ((real-type (expand-presentation-type-abbreviation
                                   element-type))
                       (real-supertype (expand-presentation-type-abbreviation
                                        element-supertype)))
                   (multiple-value-bind (subtypep determined)
                       (presentation-subtypep real-type real-supertype)
                     (cond ((not determined)
                            (return-from presentation-subtypep
                              (values nil nil)))
                           ((not subtypep)
                            (return-from presentation-subtypep
                              (values nil t)))))))
             types
             supertypes)
        (values t t)))))

(define-presentation-method present ((object list) (type sequence-enumerated)
                                     stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for tail on object
        for (obj) = tail
        for obj-type in types
        do (progn
             (present obj obj-type
                        :stream stream :view view
                        :acceptably acceptably
                        :sensitive nil)
             (when (cdr tail)
               (if acceptably
                   (princ separator stream)
                   (terpri stream))))))

(define-presentation-method present ((object vector) (type sequence-enumerated)
                                     stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for i from 0 below (length object)
        for obj = (aref object i)
        for obj-type in types
        do (progn
             (present obj obj-type
                        :stream stream :view view
                        :acceptably acceptably
                        :sensitive nil)
             (when (< i (1- (length object)))
               (if acceptably
                   (princ separator stream)
                   (terpri stream))))))

(define-presentation-method accept ((type sequence-enumerated)
                                    stream
                                    (view textual-view)
                                    &key)
  (loop
     with element = nil and element-type = nil
       and separators = (list separator)
     for type-tail on types
     for (this-type) = type-tail
     do (setf (values element element-type)
              (accept this-type
                      :stream stream
                      :view view
                      :prompt t
                      :display-default nil
                      :additional-delimiter-gestures separators))
     collect element into sequence-val
     do (progn
          (when (not (eql (peek-char nil stream nil nil) separator))
            (loop-finish))
          (read-char stream)
          (when echo-space
            ;; Make the space a noise string
            (input-editor-format stream " ")))
     finally (if (cdr type-tail)
                 (simple-parse-error "Input ~S too short for ~S."
                                     sequence-val
                                     types)
                 (return sequence-val))))

(define-presentation-type or (&rest types)
  :inherit-from t
  :parameters-are-types t)

(define-presentation-method presentation-typep (object (type or))
  (loop for type in types
        for real-type = (expand-presentation-type-abbreviation type)
        do (when (presentation-typep object real-type)
             (return-from presentation-typep t)))
  nil)

(define-presentation-method present (object (type or)
                                     stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (loop for or-type in types
        for expanded-type = (expand-presentation-type-abbreviation or-type)
        do (when (presentation-typep object expanded-type)
             (present object expanded-type
                      :stream stream :view view
                      :acceptably acceptably
                      :for-context-type for-context-type)
             (loop-finish))))

(define-presentation-method accept ((type or)
                                    (stream input-editing-stream)
                                    (view textual-view)
                                    &key)
  (with-input-context (type)
      (object type-var)
      (let ((str (read-token stream)))
	(loop for or-type in types
	   do
	     (handler-case
		 (progn
		   (return (accept-from-string or-type
					       str
					       :view view)))
	       (parse-error ()))
	   finally (simple-parse-error "Input type is not one of ~S" types)))
    (t
     (presentation-replace-input stream object type-var view :rescan nil)
     (return-from accept (values object type-var)))))

;;; What does and inherit from?  Maybe we'll punt on that for the moment.
;;; Unless it inherits from its arguments...

(define-presentation-type and (&rest types)
  :parameters-are-types t)

(define-presentation-method presentation-typep (object (type and))
  (loop for type in types
        for real-type = (expand-presentation-type-abbreviation type)
        do (with-presentation-type-decoded (name parameters)
             real-type
             (cond ((eq name 'satisfies)
                    (unless (funcall (car parameters) object)
                      (return-from presentation-typep nil)))
                   ((eq name 'not)
                    (unless (not (presentation-typep object (car parameters)))
                      (return-from presentation-typep nil)))
                   (t (unless (presentation-typep object real-type)
                        (return-from presentation-typep nil))))))
  t)

(define-presentation-method present (object (type and)
                                     stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (present object (expand-presentation-type-abbreviation (car types))
           :stream stream :view view
           :acceptably acceptably
           :for-context-type for-context-type))

(define-presentation-method accept
    ((type and) (stream input-editing-stream) (view textual-view) &rest args &key)
  (let ((subtype (first types)))
    (multiple-value-bind (obj ptype)
        (apply-presentation-generic-function accept subtype stream view args)
      (declare (ignore ptype))
      (unless (presentation-typep obj type)
        (simple-parse-error "Input object ~S is not of type ~S" obj type))
      obj)))

(define-presentation-type-abbreviation token-or-type (tokens type)
  `(or (member-alist ,tokens) ,type))

(define-presentation-type-abbreviation null-or-type (type)
  `(or null ,type))

(define-presentation-type-abbreviation type-or-string (type)
  `(or ,type string))

(define-presentation-method presentation-typep (object (type expression))
  (declare (ignore object))
  t)

(define-presentation-method present (object (type expression)
                                     stream
                                     (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (let ((*print-readably* acceptably))
    (prin1 object stream)))

(define-presentation-generic-function %accept-present-default
    accept-present-default
  (type-key parameters options type
   stream view default default-supplied-p present-p query-identifier))

;;; All the expression and form reading stuff is in builtin-commands.lisp
