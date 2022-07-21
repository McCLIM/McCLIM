;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2001 by Tim Moore (moore@bricoworks.com)
;;;  (c) copyright 2006-2008 by Troels Henriksen (athas@sigkill.dk)
;;;  (c) copyright 2022 by Daniel Kochmański (daniel@turtleware.eu)
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-internals)

;;; Signalling Errors Inside present (sic)

(define-condition simple-parse-error (simple-condition parse-error)
  ())

(defun simple-parse-error (format-string &rest format-args)
  (error 'simple-parse-error
         :format-control format-string :format-arguments format-args))

(define-condition input-not-of-required-type (parse-error)
  ((string :reader not-required-type-string :initarg :string)
   (type :reader not-required-type-type :initarg :type))
  (:report (lambda (condition stream)
             (format stream "Input ~S is not of required type ~S."
                     (not-required-type-string condition)
                     (not-required-type-type condition)))))

(defun input-not-of-required-type (object type)
  (error 'input-not-of-required-type :string object :type type))

(define-condition simple-completion-error (simple-parse-error)
  ((input-so-far :reader completion-error-input-so-far
                 :initarg :input-so-far)))

(defun simple-completion-error (input-so-far gesture)
  (signal 'simple-completion-error
          :format-control "complete-input: While rescanning, can't match ~A~A"
          :format-arguments (list input-so-far gesture)
          :input-so-far input-so-far))
