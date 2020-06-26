;;; ---------------------------------------------------------------------------
;;;     Title: Context-dependent (typed) input
;;;   Created: 2020-06-26 15:00
;;;    Author: Daniel Kochmański <daniel@turtleware.eu>
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998-2000 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2001-2002 by Tim Moore <moore@bricoworks.com>
;;;  (c) copyright 2020 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Implementation of the typed input.
;;;

(in-package #:clim-internals)

;;; The input context is a cons of a presentation type and a continuation
;;; to call to return a presentation to that input context.
(defvar *input-context* nil)

(defun input-context-type (context-entry)
  (car context-entry))

;;; Many presentation functions, internal and external, take an input
;;; context as an argument, but they really only need to look at one
;;; presentation type.
(defun make-fake-input-context (ptype)
  (list (cons (expand-presentation-type-abbreviation ptype)
              #'(lambda (object type event options)
                  (declare (ignore event options))
                  (error "Fake input context called with object ~S type ~S. ~
                          This shouldn't happen!"
                         object type)))))

(defun input-context-wait-test (stream)
  (let* ((queue (stream-input-buffer stream))
         (event (event-queue-peek queue)))
    (when event
      (let ((sheet (event-sheet event)))
        (when (and (output-recording-stream-p sheet)
                   (or (typep event 'pointer-event)
                       (typep event 'keyboard-event))
                   (not (gadgetp sheet)))
          (return-from input-context-wait-test t))))
    nil))

(defun input-context-event-handler (stream)
  (highlight-applicable-presentation *application-frame*
                                     stream
                                     *input-context*))

(defun input-context-button-press-handler (stream button-event)
  (declare (ignore stream))
  (frame-input-context-button-press-handler *application-frame*
                                            (event-sheet button-event)
                                            button-event))

(defun highlight-current-presentation (frame input-context)
  (alexandria:when-let* ((port-pointer (port-pointer (port *application-frame*)))
                         (event (synthesize-pointer-motion-event port-pointer))
                         (sheet (event-sheet event)))
    (frame-input-context-track-pointer frame input-context sheet event)))

(defmacro with-input-context ((type &key override)
                              (&optional (object-var (gensym))
                                 (type-var (gensym))
                                 event-var
                                 options-var)
                              form
                              &body pointer-cases)
  (let ((vars `(,object-var
                ,type-var
                ,@(and event-var `(,event-var))
                ,@(and options-var `(,options-var))))
        (return-block (gensym "RETURN-BLOCK"))
        (context-block (gensym "CONTEXT-BLOCK")))
    `(block ,return-block
       (multiple-value-bind ,vars
           (block ,context-block
             (let ((*input-context*
                     (cons (cons (expand-presentation-type-abbreviation ,type)
                                 #'(lambda (object type event options)
                                     (return-from ,context-block
                                       (values object type event options))))
                           ,(if override nil '*input-context*)))
                   (*pointer-button-press-handler*
                     #'input-context-button-press-handler)
                   (*input-wait-test* #'input-context-wait-test)
                   (*input-wait-handler* #'input-context-event-handler))
               (return-from ,return-block ,form )))
         (declare (ignorable ,@vars))
         (highlight-current-presentation *application-frame* *input-context*)
         (cond ,@(mapcar #'(lambda (pointer-case)
                             (destructuring-bind (case-type &body case-body)
                                 pointer-case
                               `((presentation-subtypep ,type-var ',case-type)
                                 ,@case-body)))
                         pointer-cases))))))
