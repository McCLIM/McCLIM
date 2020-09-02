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
    :reader command-menu-item-name)
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
    :initarg :keystroke)
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
       (destructuring-bind (name type value &rest args) item
         (apply #'make-menu-item name type value args)))
   menu))

(defun make-menu-item (name type value
                       &key
                         documentation
                         keystroke
                         text-style
                         command-name
                         command-line-name
                       &allow-other-keys)
  (make-instance '%menu-item
                 :menu-name name :type type :value value
                 :documentation documentation
                 :keystroke keystroke
                 :text-style text-style
                 :command-name command-name
                 :command-line-name command-line-name))

(defun command-name (command)
  (first command))

(defun command-arguments (command)
  (rest command))

(defun partial-command-p (command)
  (member *unsupplied-argument-marker* command))

(defun make-command-translators (command-name command-table args)
  "Helper function to create command presentation translators for a command."
  (loop with readable-command-name = (command-name-from-symbol command-name) ; XXX or :NAME
        for arg in args
        for arg-index from 0
        append (when (getf (cddr arg) :gesture)
                 (destructuring-bind (name ptype
                                      &key gesture &allow-other-keys)
                     arg
                   (declare (ignore name))
                   (let ((command-args (loop for a in args
                                             for i from 0
                                             if (eql i arg-index)
                                               collect 'object
                                             else
                                               collect (getf (cddr a) :default)
                                             end))
                         (translator-name (intern (format nil
                                                          ".~A-ARG~D."
                                                          command-name
                                                          arg-index)
                                                  (symbol-package command-name))))
                     (multiple-value-bind (gesture translator-options)
                         (if (listp gesture)
                             (values (car gesture) (cdr gesture))
                             (values gesture nil))
                       (destructuring-bind (&key (documentation
                                                  `((object stream)
                                                    (orf stream *standard-output*)
                                                    (format stream "~A "
                                                            ,readable-command-name)
                                                    (present object (presentation-type-of object) ; type?
                                                             :stream stream
                                                             :acceptably nil
                                                             :sensitive nil))
                                                  documentationp)
                                            &allow-other-keys)
                           translator-options
                       `(define-presentation-to-command-translator
                            ,translator-name
                            (,(eval ptype) ,command-name ,command-table
                             :gesture ,gesture
                             ,@(unless documentationp `(:documentation ,documentation))
                             ,@translator-options)
                          (object)
                          (list ,@command-args)))))))))

(defparameter *command-parser-table* (make-hash-table)
  "Mapping from command names to argument parsing functions.")

;;; Vanilla define-command, as defined by the standard
(defmacro %define-command (name-and-options args &body body)
  (unless (listp name-and-options)
    (setq name-and-options (list name-and-options)))
  (destructuring-bind (func &key command-table name menu keystroke)
      name-and-options
    (multiple-value-bind (required-args keyword-args)
        (loop for arg-tail on args
              for (arg) = arg-tail
              until (eq arg '&key)
              collect arg into required
              finally (return (values required (cdr arg-tail))))
      (let* ((command-func-args
               `(,@(mapcar #'car required-args)
                 ,@(and
                    keyword-args
                    `(&key ,@(mapcar #'(lambda (arg-clause)
                                         (destructuring-bind (arg-name ptype
                                                              &key default
                                                              &allow-other-keys)
                                             arg-clause
                                           (declare (ignore ptype))
                                           `(,arg-name ,default)))
                                     keyword-args)))))
             (accept-fun-name (gentemp (format nil "~A%ACCEPTOR%"
                                               (symbol-name func))
                                       (symbol-package func)))
             (partial-parser-fun-name (gentemp (format nil "~A%PARTIAL%"
                                                       (symbol-name func))
                                               (symbol-package func)))
             (arg-unparser-fun-name (gentemp (format nil "~A%unparser%"
                                                     (symbol-name func))
                                             (symbol-package func))))
        `(progn
           (defun ,func ,command-func-args
             ,@body)
           ,(when command-table
              `(add-command-to-command-table
                ',func ',command-table
                :name ,name :menu ',menu
                :keystroke ',keystroke :errorp nil
                ,@(and menu
                       `(:menu-command
                         (list ',func
                               ,@(make-list (length required-args)
                                            :initial-element
                                            '*unsupplied-argument-marker*))))))
           ,(make-argument-accept-fun accept-fun-name
                                      required-args
                                      keyword-args)
           ,(make-partial-parser-fun partial-parser-fun-name required-args)
           ,(make-unprocessor-fun arg-unparser-fun-name
                                  required-args
                                  keyword-args)
           ,(and command-table
                 (make-command-translators func command-table required-args))
           (setf (gethash ',func *command-parser-table*)
                 (make-instance 'command-parsers
                                :parser #',accept-fun-name
                                :partial-parser #',partial-parser-fun-name
                                :required-args ',required-args
                                :keyword-args  ',keyword-args
                                :argument-unparser #',arg-unparser-fun-name))
           ',func)))))

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
               ;; Quote atomic types to reassemble defmethod more.
               (when (atom type)
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
