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
;;; Implementation of the command definition, items and accessors.
;;;

(in-package #:clim-internals)

;;; Container for info about a command
(defclass command-item ()
  ((command-name
    :initarg :command-name
    :reader command-item-name)
   (command-line-name
    :initarg :command-line-name
    :reader command-line-name)))

(defmethod print-object ((obj command-item) stream)
  (print-unreadable-object (obj stream :identity t :type t)
    (cond ((slot-boundp obj 'command-line-name)
           (format stream "~A" (command-line-name obj)))
          ((slot-boundp obj 'command-name)
           (format stream "~S" (command-item-name obj)))
          (t nil))))

;;; According to the specification, command menu items are stored as
;;; lists.  This way seems better, and I hope nothing will break.
(defclass %menu-item (command-item)
  ((menu-name
    :initarg :menu-name
    :accessor command-menu-item-name)
   (type
    :initarg :type
    :reader command-menu-item-type)
   (value
    :initarg :value
    :reader command-menu-item-value)
   (text-style
    :initarg :text-style
    :reader command-menu-item-text-style)
   (keystroke
    :initarg :keystroke
    :accessor command-menu-item-keystroke)
   (documentation
    :initarg :documentation))
  (:default-initargs :menu-name nil
                     :type (alexandria:required-argument :type)
                     :value (alexandria:required-argument :value)
                     :text-style nil
                     :keystroke nil
                     :documentation nil))

(defmethod print-object ((item %menu-item) stream)
  (print-unreadable-object (item stream :identity t :type t)
    (let ((menu-name (command-menu-item-name item))
          (keystroke (slot-value item 'keystroke)))
     (when menu-name
       (format stream "~S" menu-name))
     (when keystroke
       (format stream "~:[~; ~]keystroke ~A"
               menu-name keystroke)))))

(defun command-menu-item-options (menu-item)
  (with-slots (documentation text-style) menu-item
    (list :documentation documentation :text-style text-style)))

(defun menu-items-from-list (menu)
  (mapcar
   #'(lambda (item)
       (if (atom item)
           item
           (destructuring-bind (name type value &rest args) item
             (apply #'make-menu-item name type value args))))
   menu))

(defun make-menu-item (name type value &key documentation
                                            keystroke
                                            text-style
                                            command-name
                                            command-line-name
                                       &allow-other-keys)
  (ecase type
    (:command
     (check-type value command-designator))
    (:function
     ;; A function of two arguments (funcalled).
     (check-type value function-designator))
    (:menu
     ;; The value is specified to be either a command table designator.
     ;; McCLIM extends that set to allow also lists which are verbatim
     ;; sub-menus.
     (check-type value (or command-table symbol cons))
     (when (listp value)
       (let ((items (menu-items-from-list value)))
         (setf value (make-command-table nil :menu items)))))
    (:divider
     ;; The value of a divider is ignored.
     ))
  (make-instance '%menu-item
                 :menu-name name :type type :value value
                 :documentation documentation
                 :keystroke (when keystroke (ensure-gesture keystroke))
                 :text-style text-style
                 :command-name command-name
                 :command-line-name command-line-name))

(defun extract-menu-item-command
    (menu-item gesture &optional numeric-argument)
  (check-type menu-item %menu-item)
  (let ((type (command-menu-item-type menu-item))
        (value (command-menu-item-value menu-item)))
    (ecase type
      (:function (funcall value gesture numeric-argument))
      (:command value))))

(defun command-name (command)
  (if (listp command)
      (first command)
      command))

(defun command-arguments (command)
  (if (listp command)
      (rest command)
      nil))

(defun partial-command-p (command)
  (and (listp command)
       (member *unsupplied-argument-marker* command)))

(defun make-unsupplied-arguments (count)
 (make-list count :initial-element '*unsupplied-argument-marker*))

(defun make-command-function-name (command-name &rest suffixes)
  (intern (format nil "~A~{%~A~}" command-name suffixes)
          (symbol-package command-name)))

;;; Helper function to create command presentation translators for a
;;; command.
(defun make-command-translators (command-name command-table args)
  (let ((readable-command-name
          ;; XXX or :NAME
          (command-name-from-symbol command-name)))
    (labels ((make-default-documentation ()
               `((object stream)
                 (orf stream *standard-output*)
                 (format stream "~A " ,readable-command-name)
                 (present object (presentation-type-of object) ; type?
                          :stream stream
                          :acceptably nil
                          :sensitive nil)))
             (make-define-gesture-translator (gesture-arg name ptype gesture)
               (let ((command-args
                       (loop for arg in args
                             for (name ptype . options) = arg
                             collect (if (eq arg gesture-arg)
                                         'object
                                         (getf options :default
                                               '*unsupplied-argument-marker*)))))
                 (multiple-value-bind (gesture translator-options)
                     (if (listp gesture)
                         (values (car gesture) (cdr gesture))
                         (values gesture nil))
                   `(define-presentation-to-command-translator
                        ,(make-command-function-name
                          command-name ':translate name)
                        (,(eval ptype) ,command-name ,command-table
                         :gesture ,gesture
                         ,@(unless (getf translator-options :documentation)
                             `(:documentation ,(make-default-documentation)))
                         ,@translator-options)
                        (object)
                      (list ,@command-args))))))
      (loop for arg in args
            for (name ptype . options) = arg
            for gesture = (getf options :gesture)
            when gesture
            collect (make-define-gesture-translator arg name ptype gesture)))))

(defparameter *command-parser-table* (make-hash-table)
  "Mapping from command names to argument parsing functions.")

(defun parse-command (command-name arg-parser del-parser stream)
  (if-let ((parser (gethash command-name *command-parser-table*)))
    (funcall (the-parser parser) arg-parser del-parser stream)
    (error 'type-error :expected-type 'command-name :datum command-name)))

(defun partial-command-from-name (command-name)
  (if-let ((parsers (gethash command-name *command-parser-table*)))
    (cons command-name
          (mapcar #'(lambda (foo)
                      (declare (ignore foo))
                      *unsupplied-argument-marker*)
                  (required-args parsers)))
    (error 'type-error :expected-type 'command-name :datum command-name)))

;;; Vanilla define-command, as defined by the standard
(defmacro %define-command (name-and-options args &body body)
  (unless (listp name-and-options)
    (setq name-and-options (list name-and-options)))
  (destructuring-bind (command-name &key command-table name menu keystroke)
      name-and-options
    (multiple-value-bind (required-args keyword-args)
        (loop for (first-arg . rest-args) on args
              until (eq first-arg '&key)
              collect first-arg into required
              finally (return (values required rest-args)))
      (let ((command-func-args
              `(,@(mapcar #'car required-args)
                ,@(when keyword-args
                    `(&key ,@(mapcar
                              (lambda (arg-clause)
                                `(,(car arg-clause)
                                  ,(getf (cddr arg-clause) :default
                                         '*unsupplied-argument-marker*)))
                              keyword-args)))))
            (parser-name (make-command-function-name command-name '#:parser)))
        `(progn
           (defun ,command-name ,command-func-args
             ,@body)
           ,(when command-table
              `(add-command-to-command-table ',command-name ',command-table
                :name ,name :menu ',menu :keystroke ',keystroke :errorp nil))
           ,(make-command-parser parser-name required-args keyword-args)
           ,@(when command-table
               (make-command-translators command-name command-table required-args))
           (setf (gethash ',command-name *command-parser-table*)
                 (make-instance 'command-parsers
                  :the-parser    (function ,parser-name)
                  :required-args (quote ,required-args)
                  :keyword-args  (quote ,keyword-args)))
           ',command-name)))))

;;; The default for :provide-output-destination-keyword is nil until we fix
;;; some unfortunate problems with completion, defaulting, and keyword
;;; arguments.

(defmacro define-command (name-and-options args &body body)
  (unless (listp name-and-options)
    (setq name-and-options (list name-and-options)))
  ;; According to the specification all argument description elements except
  ;; the parameter name are evaluated. We lax this requirement a little and
  ;; evaluate only type specifier *if* it is a list. Atom types are not
  ;; evaluated to reasemble method specialization. Moreover we validate here
  ;; argument description keyword arguments in destructuring-bind (as
  ;; suggested by Xof in the spec annotation we require keywords being
  ;; macroexpand-time constant). We allow two custom key arguments, but we
  ;; should fix ESA instead. -- jd 2018-09-14
  (loop for argument-description in args
        unless (eq argument-description '&key)
          do ;; Ensure correct structure and valid keywords.
             (destructuring-bind
                 (parameter type
                  &key
                    default default-type display-default mentioned-default
                    prompt documentation when gesture
                    ;; These two are not standard, but ESA uses them.
                    prompt-mode insert-default)
                 argument-description
               (declare (ignore parameter default default-type display-default
                                mentioned-default prompt documentation when
                                gesture prompt-mode insert-default))
               ;; Autoquoting is an ugly (and non-conforming) hack that should
               ;; be removed. Signal a warning for now. -- jd 2021-11-09
               (when (and (atom type) (not (constantp type)))
                 (alexandria:simple-style-warning
                  "Presentation type specifiers are evaluated.~@
                   Autoquoting is deprected and will be removed soon.")
                 (setf (second argument-description) `(quote ,type)))))
  (destructuring-bind (func &rest options
                       &key (provide-output-destination-keyword nil)
                       &allow-other-keys)
      name-and-options
    (with-keywords-removed (options (:provide-output-destination-keyword))
      (if provide-output-destination-keyword
          (let* ((key-supplied (find '&key args))
                 (destination-arg '(output-destination 'output-destination
                                    :default nil :display-default nil))
                 (new-args (if key-supplied
                               `(,@args ,destination-arg)
                               `(,@args &key ,destination-arg))))
            (multiple-value-bind (decls new-body)
                (get-body-declarations body)
              (with-gensyms (destination-continuation)
                `(%define-command (,func ,@options) ,new-args
                   ,@decls
                   (flet ((,destination-continuation ()
                            ,@new-body))
                     (declare (dynamic-extent #',destination-continuation))
                     (invoke-with-standard-output #',destination-continuation
                                                  output-destination))))))
          `(%define-command (,func ,@options)
               ,args
             ,@body)))))
