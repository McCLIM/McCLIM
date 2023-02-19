;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998-2000 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2001-2002 by Tim Moore <moore@bricoworks.com>
;;;  (c) copyright 2019 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Implementation of presentation translators as defined in 23.7.
;;;

(in-package #:clim-internals)


;;; 23.7.1 Defining Presentation Translators
(defclass presentation-translator ()
  ((name :reader name :initarg :name)
   (from-type :reader from-type :initarg :from-type)
   (to-type :reader to-type :initarg :to-type)
   (gesture :reader gesture :initarg :gesture)
   (tester :reader tester :initarg :tester)
   (tester-definitive :reader tester-definitive :initarg :tester-definitive)
   (documentation :reader translator-documentation :initarg :documentation)
   (pointer-documentation :reader pointer-documentation
                          :initarg :pointer-documentation)
   (menu :reader menu :initarg :menu)
   (priority :reader priority :initarg :priority :initform 0)
   (translator-function :reader translator-function
                        :initarg :translator-function)))

(defmethod initialize-instance :after
    ((obj presentation-translator) &key from-type to-type &allow-other-keys)
  (unless (slot-boundp obj 'pointer-documentation)
    (setf (slot-value obj 'pointer-documentation)
          (translator-documentation obj)))
  (unless (get-ptype from-type)
    (error "~s is not a presentation type." from-type))
  (unless (get-ptype to-type)
    (error "~s is not a presentation type." to-type)))

(defmethod print-object ((obj presentation-translator) stream)
  (print-unreadable-object (obj stream :identity t)
    (format stream "Translator ~S from ~S to ~S"
            (name obj) (from-type obj) (to-type obj))))

(defclass presentation-action (presentation-translator)
  ())

(defmethod initialize-instance :after ((obj presentation-action)
                                       &key &allow-other-keys)
  (setf (slot-value obj 'tester-definitive) t))

(defmethod print-object ((obj presentation-action) stream)
  (print-unreadable-object (obj stream :identity t)
    (format stream "Action from ~S to ~S" (from-type obj) (to-type obj))))

;; Wraps the tester function with a test that determines if the
;; command is enabled.
(defclass presentation-command-translator (presentation-translator)
  ())

(defmethod initialize-instance :after ((obj presentation-command-translator)
                                       &key tester command-name)
  (setf (slot-value obj 'tester)
        #'(lambda (&rest args)
            (if (command-enabled command-name *application-frame*)
                (maybe-apply tester args)
                nil))))

(defclass selection-translator (presentation-translator)
  ())

;;; This lives in a command table

(defvar *current-translator-cache-generation* 0
  "This is incremented whenever presentation translators are defined,
and used to ensure that presentation-translators-caches are up to date.")

(defclass translator-table ()
  ((translators
    :accessor translators :initarg :translators
    :initform (make-hash-table :test #'eq)
    :documentation "Translators keyed by name.")
   (simple-type-translators :accessor simple-type-translators
                            :initarg :simple-type-translators
                            :initform (make-hash-table :test #'eq)
                            :documentation "Holds transators with a simple
  from-type (i.e. doesn't contain \"or\" or \"and\").")
   (cache-generation
    :initform 0
    :accessor translator-table-cache-generation)
   (cache
    :initform (make-hash-table :test #'equal)
    :reader translator-table-cache)))

(defun invalidate-translator-caches ()
  (incf *current-translator-cache-generation*))

(defun presentation-translators-cache (table)
  (check-type table translator-table)
  (let ((cache (translator-table-cache table))
        (cache-generation (translator-table-cache-generation table)))
    (unless (or (= cache-generation *current-translator-cache-generation*)
                (zerop (hash-table-size cache)))
      (clrhash cache))
    (setf (translator-table-cache-generation table)
          *current-translator-cache-generation*)
    cache))

(defun default-translator-tester (object-arg &key &allow-other-keys)
  (declare (ignore object-arg))
  t)

(defun add-translator (table translator)
  (invalidate-translator-caches)
  (setf (gethash (name translator) (translators table)) translator)
  translator)

(defun remove-translator (table translator)
  (let ((presentation-translators
          (if (symbolp table)
              (presentation-translators (find-command-table table))
              table))
        (translator-name
          (if (symbolp translator)
              translator
              (name translator))))
    (remhash translator-name (translators presentation-translators)))
  (invalidate-translator-caches)
  translator)

(defun make-translator-fun (args body)
  (cond ((null args)
         (warn "OBJECT parameter is obligatory (adding ignored parameter)")
         (let ((object-arg (gensym "OBJECT-ARG")))
           `(lambda (,object-arg &key &allow-other-keys)
              (declare (ignore ,object-arg))
              ,@body)))
        (t
         `(lambda (,(first args) &key ,@(rest args) &allow-other-keys)
            (declare (ignorable ,(first args)))
            ,@body))))

(defun make-documentation-fun (doc-arg)
  (cond ((and doc-arg (symbolp doc-arg))
         doc-arg)
        ((consp doc-arg)
         (make-translator-fun (car doc-arg) (cdr doc-arg)))
        ((stringp doc-arg)
         `(lambda (object &key stream &allow-other-keys)
            (declare (ignore object))
            (write-string ,doc-arg stream)))
        ((null doc-arg)
         `(lambda (object &key presentation stream &allow-other-keys)
            (present object (presentation-type presentation)
                     :stream stream :sensitive nil)))
        (t (error "Can't handle doc-arg ~S" doc-arg))))

(defun compute-translator-or-action-initargs
    (name from-type to-type gesture tester tester-definitive
     documentation documentationp pointer-documentation pointer-documentation-p
     menu priority arglist body)
  (let* ((real-from-type (expand-presentation-type-abbreviation from-type))
         (real-to-type (expand-presentation-type-abbreviation to-type)))
    `(:name ',name
      :from-type ',real-from-type
      :to-type ',real-to-type
      :gesture ,(if (eq gesture t)
                    t
                    `(find-gesture ',gesture))
      :tester ,(if (symbolp tester)
                   `',tester
                   `(function ,(make-translator-fun (car tester) (cdr tester))))
      :tester-definitive ',tester-definitive
      :documentation (function ,(make-documentation-fun
                                 (if documentationp
                                     documentation
                                     (command-name-from-symbol name))))
      ,@(when pointer-documentation-p
          `(:pointer-documentation
            (function ,(make-documentation-fun pointer-documentation))))
      :menu ',menu
      :priority ,priority
      :translator-function (function ,(make-translator-fun arglist body)))))

(defmacro define-presentation-translator
    (name (from-type to-type command-table
           &rest translator-options
           &key (gesture :select)
                (tester 'default-translator-tester testerp)
                (tester-definitive (if testerp nil t))
                (documentation nil documentationp)
                (pointer-documentation nil pointer-documentation-p)
                (menu t)
                (priority 0)
                (translator-class 'presentation-translator)
           &allow-other-keys)
     arglist
     &body body)
  ;; null tester should be the same as no tester
  (unless tester
    (setq tester 'default-translator-tester)
    (setq tester-definitive t))
  (let ((initargs (compute-translator-or-action-initargs
                   name from-type to-type gesture tester tester-definitive
                   documentation documentationp
                   pointer-documentation pointer-documentation-p
                   menu priority arglist body)))
    (with-keywords-removed (translator-options
                            (:gesture :tester :tester-definitive :documentation
                             :pointer-documentation :menu :priority
                             :translator-class))
      `(add-translator
        (presentation-translators (find-command-table ',command-table))
        (make-instance ',translator-class ,@initargs ,@translator-options)))))

(defmacro define-presentation-action
    (name (from-type to-type command-table
           &key (gesture :select)
                (tester 'default-translator-tester)
                (documentation nil documentationp)
                (pointer-documentation nil pointer-documentation-p)
                (menu t)
                (priority 0))
     arglist
     &body body)
  (let ((initargs (compute-translator-or-action-initargs
                   name from-type to-type gesture tester t
                   documentation documentationp
                   pointer-documentation pointer-documentation-p
                   menu priority arglist body)))
    `(add-translator
      (presentation-translators (find-command-table ',command-table))
      (make-instance 'presentation-action ,@initargs))))

(defmacro define-presentation-to-command-translator
    (name (from-type command-name command-table
           &key (gesture :select)
                (tester 'default-translator-tester)
                (documentation nil documentationp)
                (pointer-documentation (command-name-from-symbol command-name))
                (menu t)
                (priority 0)
                (echo t))
     arglist
     &body body)
  (let ((command-args (gensym "COMMAND-ARGS")))
    `(define-presentation-translator ,name
         (,from-type (command :command-table ,command-table) ,command-table
                     :gesture ,gesture
                     :tester ,tester
                     :tester-definitive t
                     ,@(and documentationp `(:documentation ,documentation))
                     :pointer-documentation ,pointer-documentation
                     :menu ,menu
                     :priority ,priority
                     :translator-class presentation-command-translator
                     :command-name ',command-name)
         ,arglist
       (let ((,command-args (let () ,@body)))
         (values (cons ',command-name ,command-args)
                 '(command :command-table ,command-table)
                 '(:echo ,echo))))))

(defmacro define-selection-translator
    (name (from-type to-type command-table &rest args &key &allow-other-keys)
     arglist &body body)
  (with-current-source-form (arglist)
    (let* ((forbidden-args '(context-type frame event window x y))
           (intersection (intersection arglist forbidden-args :test #'string=)))
      (unless (null intersection)
        (error "Selection translator ~s arglist can't have args ~a but has ~a."
               name forbidden-args intersection))))
  (with-keywords-removed (args (:translator-class :tester-definitive :gesture))
    `(define-presentation-translator ,name
         (,from-type ,to-type ,command-table
                     :gesture :select
                     :tester-definitive t
                     :translator-class selection-translator
                     ,@args)
         ,arglist
       ,@body)))


;;; 23.7.2 Presentation Translator Functions

(defun find-presentation-translators (from-type to-type command-table)
  (let* ((command-table (find-command-table command-table))
         (from-name     (presentation-type-name from-type))
         (to-name       (presentation-type-name to-type))
         (cache-key     (cons from-name to-name))
         (cache-table   (presentation-translators-cache
                         (presentation-translators command-table))))
    (when-let ((cached-translators (gethash cache-key cache-table)))
      (return-from find-presentation-translators cached-translators))
    (let ((translator-vector (make-array 8 :adjustable t :fill-pointer 0))
          (table-counter 0))
      (do-command-table-inheritance (table command-table)
        ;; We need to go over each translator in all tables, because
        ;; the FROM-TYPE may be a subtype of a translator FROM-TYPE
        ;; despite not having the same presentation metaclass, like
        ;; "meta" presentation types OR, AND etc. -- jd 2020-06-24
        (loop with translators = (translators (presentation-translators table))
              for tr being the hash-value of translators
              if (and (stupid-subtypep from-type (from-type tr))
                      (stupid-subtypep (to-type tr) to-type))
                do (vector-push-extend (cons tr table-counter)
                                       translator-vector))
        (incf table-counter))
      (let ((from-super-names nil))
        (map-over-ptype-superclasses (lambda (super)
                                         (push (type-name super)
                                               from-super-names))
                                     from-name)
        (setq from-super-names (nreverse from-super-names))
        ;; The Spec mentions "high order priority" and "low order priority"
        ;; without saying what that is!  Fortunately, the Franz CLIM user guide
        ;; says that high order priority is (floor priority 10), low order
        ;; priority is (mod priority 10) That's pretty wacked...
        (flet ((translator-lessp (a b)
                 (nest
                  (destructuring-bind (tr-a . table-num-a) a)
                  (destructuring-bind (tr-b . table-num-b) b)
                  (multiple-value-bind (hi-a low-a) (floor (priority tr-a) 10))
                  (multiple-value-bind (hi-b low-b) (floor (priority tr-b) 10))
                  (let* ((a-name (presentation-type-name (from-type tr-a)))
                         (b-name (presentation-type-name (from-type tr-b)))
                         ;; FROM-TYPE of a translator may be a "meta"
                         ;; presentation type like OR and AND. in this
                         ;; case POSITION will yield NIL. We give them
                         ;; the lowest precedence because they are not
                         ;; in the presentation class precedence list.
                         ;; -- jd 2020-06-24
                         (a-prec (or (position a-name from-super-names)
                                     (length from-super-names)))
                         (b-prec (or (position b-name from-super-names)
                                     (length from-super-names))))
                    (cond
                      ;; 1. High order priority
                      ((> hi-a hi-b) (return-from translator-lessp t))
                      ((< hi-a hi-b) (return-from translator-lessp nil))
                      ;; 2. More specific "from type"
                      ((< a-prec b-prec) (return-from translator-lessp t))
                      ((> a-prec b-prec) (return-from translator-lessp nil))
                      ;; 3. Low order priority
                      ((> low-a low-b) (return-from translator-lessp t))
                      ((< low-a low-b) (return-from translator-lessp nil))
                      ;; 4. Command table inheritance
                      (t (< table-num-a table-num-b)))))))
          ;; Add translators to their caches.
          (let* ((sorted (sort translator-vector #'translator-lessp))
                 (translators (map 'list #'car sorted)))
            (setf (gethash cache-key cache-table)
                  (remove-duplicates translators))))))))

;;; When we test the presentation translator we may do it for various reasons.
;;;
;;; - display the presentation type menu (we ignore all gesture criteria)
;;; - throw the applicable presentation matched with the input event
;;; - display the pointer documentation under the pointer (partial match)
;;; - highlight the applicable presentation under the pointer (ditto)
;;;
;;; To fulfill these requirements the function TEST-PRESENTATION-TRANSLATOR
;;; accepts five keyword parameters:
;;;
;;; - FOR-MENU :: ignore all input criteria and test only for applicability
;;; - GESTURE-TYPE, BUTTON and MODIFIER-STATE :: criteria for a partial match
;;; - EVENT :: verify whether the argument matches the translator's gesture
;;;
;;; It is explicitly unspecified what happens when EVENT and MODIFIER-STATE
;;; are supplied. McCLIM exploits this undefined behvaior as follows:
;;;
;;; 1. When FOR-MENU is not NIL then all matching criteria are :IGNORE.
;;; 2. When any supplied criteria is not NIL then the event is ignored.
;;; 3. Otherwise matching criteria are initialized based on EVENT.
;;;
;;; Matching criteria are:
;;;
;;; - gesture-type   :: the 2nd argument to DEFINE-GESTURE-NAME
;;; - button         :: a bitmask of pressed pointer buttons
;;; - modifier-state :: a bitmask of pressed keyboard modifiers
;;;
;;; -- jd 2023-02-02
(defun test-presentation-translator
    (translator presentation context-type frame window x y
     &key for-menu event gesture-type button modifier-state)
  (if (or (null event) gesture-type button modifier-state)
      (setf modifier-state (or modifier-state 0)
            button (or button :ignore)
            gesture-type (or gesture-type :ignore))
      (setf modifier-state (typecase event
                             (device-event
                              (event-modifier-state event))
                             (otherwise
                              :ignore))
            button (typecase event
                     (pointer-button-event
                      (pointer-event-button event))
                     (pointer-event
                      (pointer-button-state event))
                     (otherwise
                      :ignore))
            gesture-type (event-type event)))
  (when (event-data-matches-gesture-p
         (if for-menu :ignore gesture-type)
         (if for-menu :ignore button)
         (if for-menu :ignore modifier-state)
         (gesture translator))
    (let ((from-type (from-type translator))
          (to-type (to-type translator))
          (ptype (presentation-type presentation))
          (object (presentation-object presentation)))
      (and ;; We call PRESENTATION-SUBTYPEP because applicable translators are
           ;; matched only by the presentation type's name.
           ;;
           ;; - we are liberal with FROM-TYPE to allow translators from types
           ;; like COMPLETION - it is correct because when the type has
           ;; parameters we always call PRESENTATION-TYPEP
           ;;
           ;; - we are conservative with TO-TYPE, because the tester may be
           ;; definitive and then we could succeed with a wrong translator
           ;;
           ;; -- jd 2020-09-01
           (multiple-value-bind (yesp surep)
               (presentation-subtypep ptype from-type)
             (or yesp (not surep)))
           (multiple-value-bind (yesp surep)
               (presentation-subtypep to-type context-type)
             (and yesp surep))
           (or (null (decode-parameters from-type))
               (presentation-typep object from-type))
           (or (null (tester translator))
               (funcall (tester translator) object
                        :presentation presentation :context-type context-type
                        :frame frame :window window :x x :y y :event event))
           (or (tester-definitive translator)
               (null (decode-parameters context-type))
               (presentation-typep
                (call-presentation-translator translator presentation context-type
                                              frame event window x y)
                context-type))
           t))))

(defun map-applicable-translators (func presentation input-context frame window x y
                                   &key event modifier-state for-menu button)
  (labels ((process-presentation (context presentation)
             (let* ((context-ptype (first context))
                    (maybe-translators
                      (find-presentation-translators (presentation-type presentation)
                                                     context-ptype
                                                     (frame-command-table frame))))
               ;; KLUDGE the function FRAME-PRINT-POINTER-DOCUMENTATION passes
               ;; :MENU :FOR-DOCUMENTATION to match translators that are
               ;; specified with :MENU NIL. This is to hint other modifiers in
               ;; the pointer documentation pane. -- jd 2021-02-25
               (loop for translator in maybe-translators
                     when (and (or (not for-menu)
                                   (eql for-menu :for-documentation)
                                   (eql for-menu (menu translator)))
                               (test-presentation-translator
                                translator presentation context-ptype
                                frame window x y
                                :event event :modifier-state modifier-state
                                :for-menu for-menu :button button))
                       do (funcall func translator presentation context))))
           (mopscp (context record)
             "maps recursively over all presentations in record, including record."
             (if (and x y)
                 (map-over-output-records-containing-position
                  (curry #'mopscp context) record x y)
                 (map-over-output-records (curry #'mopscp context) record))
             ;; presentation-contains-position is in presentation-defs.lisp
             (when (and (presentationp record)
                        (or (and (null x) (null y)) ; allow wildcards
                            (presentation-contains-position record x y)))
               (process-presentation context record))))
    (if (and (presentationp presentation)
             (presentation-subtypep (presentation-type presentation) 'blank-area))
        (loop for context in input-context
              do (process-presentation context presentation))
        (loop for context in input-context
              do (mopscp context presentation)))))

(defun find-applicable-translators
    (presentation input-context frame window x y
     &key event modifier-state for-menu fastp)
  (when (and (not modifier-state) (not event))
    (setf modifier-state 0))
  (let ((results nil))
    (flet ((fast-func (translator presentation context)
             (declare (ignore translator presentation context))
             (return-from find-applicable-translators t))
           (slow-func (translator presentation context)
             (push (list translator presentation (input-context-type context))
                   results)))
      (map-applicable-translators (if fastp #'fast-func #'slow-func)
                                  presentation input-context frame window x y
                                  :event event
                                  :modifier-state modifier-state
                                  :for-menu for-menu)
      (nreverse results))))

(defun presentation-matches-context-type
    (presentation context-type frame window x y &key event modifier-state)
  (let* ((ptype (expand-presentation-type-abbreviation (presentation-type presentation)))
         (ctype (expand-presentation-type-abbreviation context-type))
         (table (frame-command-table frame)))
    (and (some (lambda (translator)
                 (test-presentation-translator translator presentation ctype
                                               frame window x y
                                               :event event
                                               :modifier-state modifier-state))
               (find-presentation-translators ptype ctype table))
         t)))

(defgeneric call-presentation-translator
    (translator presentation context-type frame event window x y)
  (:method ((translator presentation-translator) presentation context-type
            frame event window x y)
    ;; Let the translator return an explict ptype of nil to, in effect, abort the
    ;; presentation throw.
    (multiple-value-call
        #'(lambda (object &optional (ptype (to-type translator)) options)
            (values object ptype options))
      (funcall (translator-function translator) (presentation-object presentation)
               :presentation presentation :context-type context-type
               :frame frame :event event :window window :x x :y y)))
  (:method ((translator presentation-action) presentation context-type
            frame event window x y)
    (funcall (translator-function translator) (presentation-object presentation)
             :presentation presentation :context-type context-type
             :frame frame :event event :window window :x x :y y)
    (values nil nil nil)))

(defun document-presentation-translator (translator
                                         presentation
                                         context-type
                                         frame
                                         event
                                         window
                                         x y
                                         &key (stream *standard-output*)
                                           (documentation-type :normal))
  (funcall (if (eq documentation-type :normal)
               (translator-documentation translator)
               (pointer-documentation translator))
           (presentation-object presentation)
           :presentation presentation
           :context-type context-type
           :frame frame
           :event event
           :window window
           :x x
           :y y
           :stream stream))

(defstruct presentation-translator-menu-item
  translator
  presentation
  context)

(defun call-presentation-menu (presentation input-context frame window x y
                               &key (for-menu t) label
                               &aux (items nil) (processed nil))
  (map-applicable-translators
   #'(lambda (translator presentation context
              &aux (key (cons translator presentation)))
       (unless (member key processed :test #'equal)
         (push key processed)
         (push
          `(,(make-presentation-translator-menu-item :translator translator
                                                     :presentation presentation
                                                     :context context)
            :documentation ,(with-output-to-string (stream)
                              (document-presentation-translator
                               translator
                               presentation
                               input-context
                               frame nil window x y
                               :stream stream)))
          items)))
   presentation input-context frame window x y :for-menu for-menu)
  (unless items
    (return-from call-presentation-menu))
  (setq items (nreverse items))
  (multiple-value-bind (item object event)
      (menu-choose
       items
       :associated-window window
       :printer #'(lambda (item stream)
                    (let ((object (first item)))
                      (document-presentation-translator
                       (presentation-translator-menu-item-translator object)
                       (presentation-translator-menu-item-presentation object)
                       (presentation-translator-menu-item-context object)
                       frame nil window x y
                       :stream stream)))
       :label label
       :pointer-documentation *pointer-documentation-output*)
    (declare (ignore object))
    (when item
      (multiple-value-bind (object ptype options)
          (call-presentation-translator
           (presentation-translator-menu-item-translator item)
           (presentation-translator-menu-item-presentation item)
           (input-context-type (presentation-translator-menu-item-context item))
           frame event window x y)
        (when ptype
          (funcall (cdr (presentation-translator-menu-item-context item))
                   object ptype event options))))))



;;; 23.7.3 Finding Applicable Presentations

(defun find-innermost-presentation-match
    (input-context top-record frame window x y event modifier-state button)
  "Helper function that implements the \"innermost-smallest\" input-context
  presentation matching algorithm.  Returns presentation, translator, and
  matching input context."
  (let ((result nil)
        (result-translator nil)
        (result-context nil)
        (result-size nil))
    (map-applicable-translators
     #'(lambda (translator presentation context)
         (if (and result-context (not (eq result-context context)))
             ;; Return inner presentation
             (return-from find-innermost-presentation-match
               (values result result-translator result-context))
             (multiple-value-bind (min-x min-y max-x max-y)
                 (output-record-hit-detection-rectangle* presentation)
               (let ((size (* (- max-x min-x) (- max-y min-y))))
                 (when (or (not result) (< size result-size))
                   (setq result presentation)
                   (setq result-translator translator)
                   (setq result-context context)
                   (setq result-size size))))))
     top-record
     input-context
     frame
     window
     x y
     :event event
     :modifier-state modifier-state
     :button button)
    (when result
      (return-from find-innermost-presentation-match
        (values result result-translator result-context)))
    (map-applicable-translators
     #'(lambda (translator presentation context)
         (return-from find-innermost-presentation-match
           (values presentation translator context)))
     (make-blank-area-presentation window x y event)
     input-context
     frame
     window
     x y
     :event event
     :modifier-state modifier-state
     :button button))
  nil)

(defun find-innermost-applicable-presentation
    (input-context window x y
     &key (frame *application-frame*) modifier-state event)
  (when (and (not modifier-state) (not event))
    (setf modifier-state 0))
  (values (find-innermost-presentation-match input-context
                                             (stream-output-history window)
                                             frame
                                             window
                                             x y
                                             event
                                             modifier-state
                                             nil)))

(defun throw-highlighted-presentation (presentation input-context event)
  (let ((x (pointer-event-x event))
        (y (pointer-event-y event))
        (window (event-sheet event)))
    (multiple-value-bind (p translator context)
        (find-innermost-presentation-match input-context
                                           presentation
                                           *application-frame*
                                           (event-sheet event)
                                           x y
                                           event
                                           nil
                                           nil)
      (when p
        (multiple-value-bind (object ptype options)
            (call-presentation-translator translator
                                          p
                                          (input-context-type context)
                                          *application-frame*
                                          event
                                          window
                                          x y)
          (when ptype
            (funcall (cdr context) object ptype event options)))))))

(defun throw-object-ptype (object type &key (input-context *input-context*) sheet)
  "Throw an object and presentation type within input-context without
a presentation"
  (throw-highlighted-presentation
   (make-instance 'standard-presentation
                  :object object :type type
                  :single-box t)
   input-context
   (make-instance 'pointer-button-press-event
                  :sheet sheet
                  :x 0 :y 0
                  :modifier-state 0
                  :button +pointer-left-button+)))

(defun highlight-applicable-presentation (frame stream input-context
                                          &optional (prefer-pointer-window t))
  (when-let* ((event (stream-gesture-available-p stream))
              (sheet (typecase event
                       (pointer-event
                        (event-sheet event))
                       (keyboard-event
                        (pointer-sheet (port-pointer (port stream)))))))
    (when (or prefer-pointer-window (eq stream sheet))
      (frame-input-context-track-pointer frame input-context sheet event))))

;;; FIXME missing functions
;;;
;;; set-highlighted-presentation
;;; unhighlight-highlighted-presentation
