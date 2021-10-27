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

(defvar *unsupplied-argument-marker*
  '%unsupplied-argument-marker%)

(defun unsupplied-argument-p (val)
  (eq *unsupplied-argument-marker* val))

(defclass command-parsers ()
  ((the-parser :accessor the-parser :initarg :the-parser)
   (required-args :accessor required-args :initarg :required-args)
   (keyword-args :accessor keyword-args :initarg :keyword-args))
  (:documentation "A container for a command's parsing functions and
  data for unparsing"))

;;; The command parser first parsed the required arguments and then (if any)
;;; parses the keyword arguments. This mechanism is inspired and roughly
;;; compatible with CLIM-TOS parsers.
;;;
;;; The argument-parser accepts a stream and the argument description.
;;; Additionally the argument name is passed as :query-identifier and
;;; "(arg-name)" as :prompt (if not supplied).
;;;
;;;     (funcall argument-description stream ptype
;;;              :query-identifier ',argument-name
;;;              ,@argument-description-keys
;;;              :prompt ,(format nil "~(~a~)" argument-name)
;;;
;;; The delimiter-parser accepts a stream and a single argument denoting what
;;; will be parsed next:
;;;
;;;     :arg - the next argument is a positional argument
;;;     :key - the next argument is a keyword name
;;;     :val - the next argument is a keyword value
;;;
;;; Additionally
;;;
;;;     :pos - positional arguments will be parsed
;;;     :opt - keyword (named) arguments will be parsed
;;;     :end - nothing more will to parsed
;;;
;;; Note that :opt stands for "options" and not for "optional", because named
;;; arguments are not optional unless they have a default value.
(defun make-command-parser (name req-arg-desc key-arg-desc)
  (unless (or req-arg-desc key-arg-desc)
    (return-from make-command-parser
      `(defun ,name (arg del str)
         (declare (ignore arg del str)))))
  (collect (pos-bindings pos-process
            key-bindings key-clauses key-pos-1 key-pos-2)
    (with-gensyms (argument-parser delimiter-parser stream
                                   this-key all-keys key-ptype)
      (labels ((make-req (arg ptype key-args)
                 (assert (null (or (member :when key-args)
                                   (member :mentioned-default key-args)))
                         () "Invalid required argument description options.")
                 ;; :GESTURE is not evaluated and it is used by the
                 ;; presentation translator constructor so we remove it.
                 (with-keywords-removed (key-args (:gesture))
                   `(funcall ,argument-parser ,stream ,ptype
                             :query-identifier ',arg
                             ,@key-args
                             :prompt ,(format nil "~(~a~)" arg))))
               (make-key (possibilities)
                 `(let ((,key-ptype `((member ,@,possibilities)
                                      :name-key keyword-arg-name-from-symbol)))
                    (funcall ,argument-parser ,stream ,key-ptype
                             :prompt nil
                             :default (first ,possibilities))))
               (make-val (arg ptype key-args)
                 ;; :WHEN is used in this function body and it is not passed
                 ;; to the argument parser. For :GESTURE see MAKE-REQ.
                 (with-keywords-removed (key-args (:gesture :when))
                   `(funcall ,argument-parser ,stream ,ptype
                             :query-identifier ',arg
                             ,@key-args
                             :prompt ,(format nil "~(~a~)" arg))))
               (make-del (what)
                 `(funcall ,delimiter-parser ,stream ,what)))
        ;; collect information for required arguments
        (when req-arg-desc
          (pos-process (make-del :pos)
                       (make-del :arg)))
        (loop for rem-args on req-arg-desc
              for (arg ptype . args) in req-arg-desc
              do (pos-bindings
                  `(,arg *unsupplied-argument-marker*))
              do (pos-process
                  `(setf ,arg ,(make-req arg ptype args)))
              do (when (rest rem-args)
                   (pos-process (make-del :arg))))
        ;; collect information for keyword arguments
        (loop for (arg ptype . args) in key-arg-desc
              for keyword = (make-keyword arg)
              for default = (getf args :default '*unsupplied-argument-marker*)
              for when-p  = (getf args :when t)
              do (if (constantp when-p)
                     (when (eval when-p)
                       (key-pos-1 keyword))
                     (key-pos-2 `(when ,when-p
                                   (push ,keyword ,all-keys))))
              do (key-bindings `(,arg ,default))
              do (key-clauses  `(,keyword
                                 (setf ,arg ,(make-val arg ptype args)))))
        `(defun ,name (,argument-parser ,delimiter-parser ,stream)
           (let ,(pos-bindings)
             (declare (ignorable ,@(mapcar #'car (pos-bindings))))
             ,@(pos-process)
             ,@(unless (null key-arg-desc)
                 `(,(make-del :opt)
                   (let ((,all-keys ',(key-pos-1)))
                     ,@(key-pos-2)
                     (when ,all-keys
                       ,(make-del :key)
                       (let ,(key-bindings)
                         (declare (ignorable ,@(mapcar #'car (key-bindings))))
                         (loop for ,this-key = ,(make-key all-keys)
                               while ,this-key
                               do ,(make-del :val)
                               do (ecase ,this-key
                                    ,@(key-clauses))
                               do (setf ,all-keys (remove ,this-key ,all-keys))
                               do (when ,all-keys
                                    ,(make-del :key))
                               until (null ,all-keys)))))))
             ,(make-del :end)))))))
