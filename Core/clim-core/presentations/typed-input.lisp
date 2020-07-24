;;; ---------------------------------------------------------------------------
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
  (when-let ((gesture (stream-gesture-available-p stream)))
    (when (eventp gesture)
      (output-recording-stream-p (event-sheet gesture)))))

(defun input-context-event-handler (stream)
  (highlight-applicable-presentation *application-frame* stream *input-context*))

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

(define-presentation-generic-function %presentation-default-processor
    presentation-default-processor
    (type-key parameters default type &key default-type))

(define-default-presentation-method presentation-default-processor
    (default type &key (default-type nil default-type-p))
  (values default (if default-type-p
                      default-type
                      type)))

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


(define-presentation-generic-function %accept accept
  (type-key parameters options type stream view
   &key default default-type &allow-other-keys))

(define-presentation-generic-function %accept-present-default
  accept-present-default
  (type-key parameters options type
   stream view default default-supplied-p present-p query-identifier))

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
  (handler-bind ((abort-gesture
                   (lambda (condition)
                     ;; to give outer handlers a chance to say "I know
                     ;; how to handle this"
                     (signal condition)
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
        ;; Presentation type history interaction. According to the
        ;; spec, if provide-default is true, we take the default from
        ;; the presentation history. In addition, we'll implement the
        ;; Genera behavior of temporarily putting the default on the
        ;; history stack so the user can conveniently suck it in.
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

;;; Remaining functions for accept are defined in input-editing.lisp
