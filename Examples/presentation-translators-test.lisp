;;;;  Copyright (c) 2020 Daniel Kochmański
;;;;
;;;;    License: BSD-2-Clause.

(defpackage #:clim-demo.presentation-translators-test
  (:use #:clim #:clim-lisp)
  (:export #:run #:presentation-translators-test))
(in-package #:clim-demo.presentation-translators-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass foo () ())
  (defclass bar () ())
  (defclass qux () ())
  (defclass sen (foo bar) ())

  (defvar *foo* (make-instance 'foo))
  (defvar *bar* (make-instance 'bar))
  (defvar *qux* (make-instance 'qux))
  (defvar *sen* (make-instance 'sen))

  ;; MAKE-LOAD-FORM methods are necessary, because COMPLETION
  ;; externalizes its elements to FASL.

  (defmethod make-load-form ((object (eql *qux*)) &optional env)
    (declare (ignore object env))
    `*qux*)

  (defmethod make-load-form ((object (eql *sen*)) &optional env)
    (declare (ignore object env))
    `*sen*)

  (define-presentation-type foo ())
  (define-presentation-type bar ())
  (define-presentation-type qux ())
  (define-presentation-type qux* () :inherit-from 'qux)
  (define-presentation-type sen ())

  ;; FIXME method inheritance doesn't work right under certain
  ;; conditions and QUX doesn't inherit the default behavior of the
  ;; function PRESENTATION-TYPEP from the presentation type QUX.
  (define-presentation-method presentation-typep (object (type qux*))
    (typep object 'qux))

  (define-presentation-type-abbreviation foobar ()
    `(or foo bar))

  (define-presentation-type-abbreviation foobar* ()
    `(and foo bar))

  (define-presentation-type-abbreviation *qux*/*sen* ()
    `(member ,*qux* ,*sen*))

  (define-presentation-method present
      ((object foo) (type foo) stream view &key)
    (princ "FOO" stream))

  (define-presentation-method present
      ((object bar) (type bar) stream view &key)
    (princ "BAR" stream))

  (define-presentation-method present
      ((object qux) (type qux) stream view &key)
    (princ "QUX" stream))

  (define-presentation-method present
      ((object qux) (type qux*) stream view &key)
    (princ "QUX*" stream))

  (define-presentation-method present
      ((object sen) (type sen) stream view &key)
    (princ "SEN" stream)))

(defun display (frame pane)
  (declare (ignore pane))
  (format-textual-list (list *foo* *bar* *qux* *sen*)
                       (lambda (obj strm)
                         (declare (ignore strm))
                         (present obj)))
  (format t ", ")
  (present *qux* 'qux*)
  (fresh-line)
  (let* ((sel (sel frame))
         (str (car sel))
         (obj (cdr sel)))
    (format t "~a ~a" str (class-of obj)))
  (stream-increment-cursor-position *standard-output* 0 50)
  (fresh-line)
  (formatting-table (t :x-spacing 30)
    (formatting-column ()
      (formatting-cell () (with-text-face (*standard-output* :bold)
                            (princ "Translator")))
      (formatting-cell () (princ "QUX"))
      (formatting-cell () (princ "(OR FOO BAR)"))
      (formatting-cell () (princ "(AND FOO BAR)"))
      (formatting-cell () (princ "(MEMBER *QUX* *SEN*)")))
    (formatting-column ()
      (formatting-cell () (with-text-face (*standard-output* :bold)
                            (princ "Applies to")))
      (formatting-cell () (princ "QUX and QUX*"))
      (formatting-cell () (princ "FOO, BAR and SEN"))
      (formatting-cell () (princ "SEN"))
      (formatting-cell () (princ "QUX and SEN")))))

(define-application-frame presentation-translators-test ()
  ((sel :initform nil :accessor sel))
  (:menu-bar nil)
  (:geometry :width 1600 :height 900)
  (:pane :application :display-function #'display
   :text-margins '(:left 20 :top 10))
  (:command-table (test))
  (:command-definer nil))

(define-command (com-sel :command-table test) ((object t))
  (setf (sel *application-frame*) object))

(define-presentation-to-command-translator normal-translator
    (qux com-sel test) (object)
  (list (cons "Normal translator from " object)))

(define-presentation-to-command-translator or-translator
    (foobar com-sel test) (object)
  (list (cons "OR translator from " object)))

(define-presentation-to-command-translator and-translator
    (foobar* com-sel test) (object)
  (list (cons "AND translator from " object)))

(define-presentation-to-command-translator member-translator
    (*qux*/*sen* com-sel test) (object)
  (list (cons "MEMBER translator from " object)))

(defun run ()
  (run-frame-top-level
   (make-application-frame 'presentation-translators-test)))
