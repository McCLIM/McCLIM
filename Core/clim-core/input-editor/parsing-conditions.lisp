;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2001 by Tim Moore (moore@bricoworks.com)
;;;  (c) copyright 2006-2008 by Troels Henriksen (athas@sigkill.dk)
;;;  (c) copyright 2022-2023 by Daniel Kochmański (daniel@turtleware.eu)
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-internals)

;;; Signalling Errors Inside present (sic)

(define-condition simple-parse-error (simple-condition parse-error)
  ()
  (:documentation "The error that is signalled by
`simple-parse-error'. This is a subclass of `parse-error'.

This condition handles two initargs, `:format-string' and
`:format-arguments', which are used to specify a control string
and arguments for a call to `format'."))

(defun simple-parse-error (format-string &rest format-args)
  "Signals a `simple-parse-error' error while parsing an input
token. Does not return. `Format-string' and `format-args' are as
for format."
  (error 'simple-parse-error
         :format-control format-string :format-arguments format-args))

(define-condition input-not-of-required-type (parse-error)
  ((string :reader not-required-type-string :initarg :string)
   (type :reader not-required-type-type :initarg :type))
  (:report (lambda (condition stream)
             (format stream "Input ~S is not of required type ~S."
                     (not-required-type-string condition)
                     (not-required-type-type condition))))
  (:documentation "The error that is signalled by
`input-not-of-required-type'. This is a subclass of
`parse-error'.

This condition handles two initargs, `:string' and `:type', which
specify a string to be used in an error message and the expected
presentation type."))

(defun input-not-of-required-type (object type)
  "Reports that input does not satisfy the specified type by
signalling an `input-not-of-required-type' error. `Object' is a
parsed object or an unparsed token (a string). `Type' is a
presentation type specifier. Does not return."
  (error 'input-not-of-required-type :string object :type type))

(define-condition simple-completion-error (simple-parse-error)
  ((input-so-far :reader completion-error-input-so-far
                 :initarg :input-so-far))
  (:documentation "The error that is signalled by
`complete-input' when no completion is found. This is a subclass
of `simple-parse-error'."))

(define-condition empty-input-condition (simple-condition)
  ((stream :reader empty-input-condition-stream :initarg :stream))
  (:default-initargs :format-control "The input is empty."
                     :format-arguments '()))

(define-condition rescan-condition (condition)
  ())
