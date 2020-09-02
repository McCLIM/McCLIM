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
;;; Implementation of the command parsing.
;;;

(in-package #:clim-internals)

(defclass command-parsers ()
  ((parser :accessor parser :initarg :parser)
   (partial-parser :accessor partial-parser :initarg :partial-parser)
   (argument-unparser :accessor argument-unparser
                      :initarg :argument-unparser)
   (required-args :accessor required-args :initarg :required-args)
   (keyword-args :accessor keyword-args :initarg :keyword-args))

  (:documentation "A container for a command's parsing functions and
  data for unparsing"))

(defvar *unsupplied-argument-marker* '%unsupplied-argument-marker%)

(defun unsupplied-argument-p (val)
  (eq *unsupplied-argument-marker* val))

(defun present-argument-value (value ptype stream)
  (if (unsupplied-argument-p value)
      (with-text-face (stream :italic)
        (write-string "<unsupplied>" stream))
      (present value ptype :stream stream)))

(defun accept-form-for-argument (stream arg)
  (let ((accept-keys '(:default :default-type :display-default
                       :prompt :documentation :insert-default)))
    (destructuring-bind (name ptype &rest key-args
                         &key (mentioned-default nil mentioned-default-p)
                         &allow-other-keys)
        arg
      (declare (ignore name))
      `(accept ,ptype :stream ,stream
               ,@(loop for (key val) on key-args by #'cddr
                       when (member key accept-keys)
                         append `(,key ,val) into args
                       finally (return (if mentioned-default-p
                                           `(:default ,mentioned-default
                                                      ,@args)
                                           args)))))))

;;; In the partial command reader accepting-values dialog, default
;;; values come either from the input command arguments, if a value
;;; was supplied, or from the default option for the command argument.
;;;
;;; accept for the partial command reader.  Can this be refactored to
;;; share code with accept-form-for-argument? Probably not.
;;;
;;; original-command-arg is value entered by the user, or
;;; *unsupplied-argument-marker*. command-arg is the current value for the
;;; argument, originally bound to original-command-arg and now possibly
;;; changed by the user.
(defun accept-form-for-argument-partial (stream ptype-arg command-arg
                                         original-command-arg )
  (let ((accept-keys '(:default :default-type :display-default
                       :prompt :documentation :insert-default)))
    (destructuring-bind (name ptype &rest key-args)
        ptype-arg
      (declare (ignore name))
      (let ((args (loop
                    for (key val) on key-args by #'cddr
                    if (eq key :default)
                      append `(:default (if (eq ,command-arg
                                                *unsupplied-argument-marker*)
                                            ,val
                                            ,command-arg))
                    else if (member key accept-keys :test #'eq)
                           append `(,key ,val))))
        (setq args (append args `(:query-identifier ',(gensym "COMMAND-PROMPT-ID"))))
        (if (member :default args :test #'eq)
            `(accept ,ptype :stream ,stream ,@args)
            `(if (eq ,original-command-arg *unsupplied-argument-marker*)
                 (accept ,ptype :stream ,stream ,@args)
                 (accept ,ptype :stream ,stream :default ,command-arg
                         ,@args)))))))

(defun make-key-acceptors (stream keyword-args key-results)
  ;; We don't use the name as a variable, and we do want a symbol in the
  ;; keyword package.
  (when (null keyword-args)
    (return-from make-key-acceptors nil))
  (setq keyword-args (mapcar #'(lambda (arg)
                                 (cons (make-keyword (car arg)) (cdr arg)))
                             keyword-args))
  (let ((key-possibilities (gensym "KEY-POSSIBILITIES"))
        (member-ptype (gensym "MEMBER-PTYPE"))
        (key-result (gensym "KEY-RESULT"))
        (val-result (gensym "VAL-RESULT")))
    `(let ((,key-possibilities nil))
       ,@(mapcar #'(lambda (key-arg)
                     (destructuring-bind (name ptype
                                          &key (when t) &allow-other-keys)
                         key-arg
                       (declare (ignore ptype))
                       (let ((key-arg-name (concatenate
                                            'string
                                            ":"
                                            (keyword-arg-name-from-symbol
                                             name))))
                         `(when ,when
                            (push `(,,key-arg-name ,,name)
                                  ,key-possibilities)))))
                 keyword-args)
       (setq ,key-possibilities (nreverse ,key-possibilities))
       (when ,key-possibilities
         (input-editor-format ,stream "(keywords) ")
         (let ((,member-ptype `(token-or-type ,,key-possibilities empty)))
           (loop
             (let* ((,key-result (prog1 (accept ,member-ptype
                                                :stream ,stream
                                                :prompt nil
                                                :default nil)
                                   (eat-delimiter-or-activator)))
                    (,val-result
                      (case ,key-result
                        ,@(mapcar
                           #'(lambda (key-arg)
                               `(,(car key-arg)
                                 ,(accept-form-for-argument stream
                                                            key-arg)))
                           keyword-args))))
               (setq ,key-results (list* ,key-result
                                         ,val-result
                                         ,key-results)))
             (eat-delimiter-or-activator))))

       ,key-results)))

(defun make-argument-accept-fun (name required-args keyword-args)
  (let ((stream-var (gensym "STREAM"))
        (required-arg-names (mapcar #'car required-args))
        (key-results (gensym "KEY-RESULTS")))
    `(defun ,name (,stream-var)
       (let (,@(mapcar #'(lambda (arg)
                           `(,arg *unsupplied-argument-marker*))
                       required-arg-names)
             (,key-results nil))
         (block activated
           (flet ((eat-delimiter-or-activator ()
                    (let ((gesture (read-gesture :stream ,stream-var)))
                      (when (or (null gesture)
                                (activation-gesture-p gesture))
                        (return-from activated nil))
                      (unless (delimiter-gesture-p gesture)
                        (unread-gesture gesture
                                        :stream ,stream-var)))))
             (declare (ignorable (function eat-delimiter-or-activator)))
             (let ((gesture (read-gesture :stream ,stream-var
                                          :timeout 0
                                          :peek-p t)))
               (cond ((and gesture (activation-gesture-p gesture))
                      (return-from activated nil)))
               ,@(mapcan #'(lambda (arg)
                             (copy-list
                              `((setq ,(car arg)
                                      ,(accept-form-for-argument stream-var
                                                                 arg))
                                (eat-delimiter-or-activator))))
                         required-args)
               ,(make-key-acceptors stream-var keyword-args key-results))))
         (list* ,@required-arg-names ,key-results)))))

(defun make-partial-parser-fun (name required-args)
  (with-gensyms (command-table stream label partial-command
                 command-name command-line-name)
    (let* ((required-arg-names (mapcar #'car required-args))
           (original-args (mapcar #'(lambda (arg)
                                      (gensym (format nil "~A-ORIGINAL"
                                                      (symbol-name arg))))
                                  required-arg-names)))
      ;; We don't need fresh gensyms of these variables for each accept form.
      (with-gensyms (value ptype changedp)
        `(defun ,name (,command-table ,stream ,partial-command)
           (do ((still-missing nil t))
               (nil)
             (destructuring-bind (,command-name ,@original-args)
                 ,partial-command
               (let* ((,command-line-name (command-line-name-for-command
                                           ,command-name
                                           ,command-table
                                           :errorp nil))
                      (,label (format nil
                                      "You are being prompted for arguments to ~S"
                                      ,command-line-name))
                      ,@(mapcar #'list required-arg-names original-args))
                 (accepting-values (,stream :select-first-query t
                                            :align-prompts t
                                            :label ,label)
                   ,@(loop
                       for var in required-arg-names
                       for original-var in original-args
                       for parameter in required-args
                       for first-arg = t then nil
                       append `((multiple-value-bind (,value ,ptype ,changedp)
                                    ,(accept-form-for-argument-partial
                                      stream parameter var original-var)
                                  (declare (ignore ,ptype))
                                  ,@(unless first-arg `((terpri ,stream)))
                                  (when ,changedp
                                    (setq ,var ,value)))))
                   (when still-missing
                     (format ,stream
                             "~&Please supply all arguments.~%")))
                 (setf ,partial-command (list ,command-name ,@required-arg-names))
                 (unless (partial-command-p ,partial-command)
                   (return ,partial-command))))))))))

;;; XXX What do to about :acceptably? -- moore
(defun make-unprocessor-fun (name required-args key-args)
  (with-gensyms (command command-args stream key key-arg-val seperator arg-tail)
    ;; Bind the argument variables because expressions in the following
    ;; arguments (including the presentation type!) might reference them.
    (let ((required-arg-bindings nil)
          (key-case-clauses nil))
      (loop
        for (arg ptype-form) in required-args
        collect `(,arg (let ((value (pop ,command-args)))
                         (write-char ,seperator ,stream)
                         (present-argument-value value ,ptype-form ,stream)
                         value))
          into arg-bindings
        finally (setq required-arg-bindings arg-bindings))
      (loop
        for (arg ptype-form) in key-args
        for arg-key = (make-keyword arg)
        collect `(,arg-key
                  (format ,stream "~C:~A~C"
                          ,seperator
                          ,(keyword-arg-name-from-symbol arg)
                          ,seperator)
                  (present-argument-value ,key-arg-val ,ptype-form ,stream))
          into key-clauses
        finally (setq key-case-clauses key-clauses))
      `(defun ,name (,command ,stream)
         ,(declare-ignorable-form* stream)
         (let* ((,seperator #\Space) (,command-args (cdr ,command))
                ,@required-arg-bindings)
           (declare (ignorable ,seperator ,command-args
                               ,@(mapcar #'car required-arg-bindings )))
           ,@(when key-args
               `((loop
                   for ,arg-tail on ,command-args by #'cddr
                   for (,key ,key-arg-val) = ,arg-tail
                   do (progn
                        (case ,key
                          ,@key-case-clauses)
                        (when (cddr ,arg-tail)
                          (write-char ,seperator ,stream)))))))))))
