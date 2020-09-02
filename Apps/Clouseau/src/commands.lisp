;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2018-2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Infrastructure for inspector commands and commands that are
;;; specific to a particular kind of inspected object.
;;;

(cl:in-package #:clouseau)

;;; Utilities

(defun call-with-command-error-handling (do-thunk undo-thunk
                                         &optional format-control
                                         &rest format-arguments)
  (handler-case
      (funcall do-thunk)
    (error (condition)
      (funcall undo-thunk)
      (let ((stream (frame-standard-output *application-frame*)))
        (with-style (stream :error)
          (format stream "~&~@<~?: ~A~@:>~%"
                  (or format-control "Error executing command")
                  format-arguments
                  condition))))))

(defmacro with-command-error-handling ((&optional format-control
                                        &rest format-arguments)
                                       do-form &body undo-forms)
  `(call-with-command-error-handling
    (lambda () ,do-form) (lambda () ,@undo-forms)
    ,format-control ,@format-arguments))

(defun eval-with-bindings (form &key object-state place root-place)
  (let* ((place       (cond (object-state (place object-state))
                            (place)))
         (object      (when (and place (valuep place))
                        (value place)))
         (root-place  (cond (root-place)
                            (place (root place))))
         (root-object (value root-place))
         (output      (make-string-output-stream))
         (warnings    '()))
    (multiple-value-bind (function warningsp failurep)
        (let ((*standard-output* output)
              (*error-output*    output)
              (*trace-output*    output))
          (handler-bind ((style-warning #'muffle-warning)
                         (warning       (lambda (condition)
                                          (push condition warnings))))
            (compile nil `(lambda ()
                            (let (,@(when root-object
                                      `((** (quote ,root-object))))
                                  ,@(when object
                                      `((* (quote ,object)))))
                              ,form)))))
      (declare (ignore warningsp))
      (when failurep
        (error "~:[Invalid expression~;~:*~{~%~A~}~]" warnings))
      (funcall function))))

;;; Inspector command table
;;;
;;; Contains commands that should be available in any context (as
;;; opposed to, say, the standalone inspector application).

(define-command-table inspector-command-table)

;;; Commands on all inspected objects

(defun toggle-expand-documentation (which object stream)
  (format stream "~A " which)
  (with-print-error-handling (stream)
    (with-safe-and-terse-printing (stream)
      (princ (object object) stream))))

(define-command (com-expand :command-table inspector-command-table
                            :name          t)
    ((object inspected-object))
  (setf (style object) :expanded))

(define-presentation-to-command-translator object->expand
    (inspected-object com-expand inspector-command-table
     :tester ((object) (not (eq (style object) :expanded)))
     :documentation "Expand object"
     :pointer-documentation ((object stream)
                             (toggle-expand-documentation
                              "Expand" object stream)))
    (object)
  (list object))

(define-command (com-collapse :command-table inspector-command-table
                              :name          t)
    ((object inspected-object))
  (setf (style object) :collapsed))

(define-presentation-to-command-translator object->collapse
    (inspected-object com-collapse inspector-command-table
     :tester ((object) (eq (style object) :expanded))
     :documentation "Collapse object"
     :pointer-documentation ((object stream)
                             (toggle-expand-documentation
                              "Collapse" object stream)))
    (object)
  (list object))

(define-command (com-eval-with-context :command-table inspector-command-table
                                       :name          t)
    ((object 'inspected-object :prompt  "context object"
                               :default (state (root-place (inspector-state))))
     (form   'clim:form))
  (with-command-error-handling ("Error evaluating form")
    (let ((result (eval-with-bindings form :object-state object)))
      (present result 'clim:expression))))

(define-presentation-to-command-translator object->eval-with-context
    (inspected-object com-eval-with-context inspector-command-table
     :priority      -1
     :documentation "Evaluate a form in this context")
    (object)
  (list object (accept 'clim:form :prompt "form")))

;;; Commands on all places

(defun valid-value-transfer-p (from-place to-place)
  (if (safe-valuep from-place)
      (and (supportsp to-place 'setf)
           (accepts-value-p to-place (value from-place)))
      (supportsp to-place 'remove-value)))

(defun transfer-value (place value valuep)
  (if valuep
      (setf (value place) value)
      (remove-value place)))

(define-command (com-set-place :command-table inspector-command-table
                               :name          t)
    ((place 'place     :prompt "Place to set value of")
     (form  'clim:form :prompt "New place value (evaluated)"))
  (with-command-error-handling ("Error evaluating and setting value")
      (let ((new-value (eval-with-bindings form :place place)))
        (setf (value place) new-value))))

(define-presentation-to-command-translator place->com-set-place
    (place com-set-place inspector-command-table
     :gesture       :edit
     :tester        ((object) (supportsp object 'setf))
     :documentation "Set value of place")
    (object)
  (list object (accept 'clim:form :prompt "New place value (evaluated)")))

(define-command (com-remove-place-value :command-table inspector-command-table
                                        :name          t)
    ((place 'place :prompt "Place to remove value of"))
  (with-command-error-handling
      ("Could not remove value of place ~A" place)
      (remove-value place)))

(define-presentation-to-command-translator place->com-remove-place-value
    (place com-remove-place-value inspector-command-table
     :gesture :delete
     :tester ((object)
              (and (supportsp object 'remove-value)
                   (safe-valuep object)))
     :documentation "Remove value of place")
    (object)
  (list object))

(define-command (com-copy-place-value :command-table inspector-command-table
                                      :name          t)
    ((from-place 'place :prompt "From place")
     (to-place   'place :prompt "To place"))
  (with-command-error-handling
      ("Could not copy value from ~A to ~A" from-place to-place)
      (multiple-value-bind (value valuep)
          (when (safe-valuep from-place)
            (values (value from-place) t))
        (transfer-value to-place value valuep)
        (setf (state to-place) (if valuep
                                   (make-object-state value to-place)
                                   nil)))))

(define-gesture-name :copy :pointer-button-press (:left :control))

(define-drag-and-drop-translator drag-copy-place-value
    (place command place inspector-command-table
     :gesture :copy :menu nil
     :destination-tester ((object destination-object)
                          ;; Cannot copy a place with itself and
                          ;; otherwise "value" (including unbound)
                          ;; transfer must be possible.
                          (and (not (eq object destination-object))
                               (valid-value-transfer-p
                                object destination-object)))
     :pointer-documentation ((object destination-object stream)
                             (with-print-error-handling (stream)
                               (with-safe-and-terse-printing (stream)
                                 (if destination-object
                                     (format stream "Copy value of ~A ~
                                                     into ~A"
                                             object destination-object)
                                     (format stream "Drag onto place to ~
                                                     copy value of ~A"
                                             object))))))
    (object destination-object)
  (list 'com-copy-place-value object destination-object))

(define-command (com-swap-place-values :command-table inspector-command-table
                                       :name          t)
    ((place1 'place :prompt "First place")
     (place2 'place :prompt "Second place"))
  ;; Attempt to change values (without children and states) first so
  ;; that fewer things need undoing if, for example, a slot type check
  ;; signals an error.
  (multiple-value-bind (old-value1 old-value1-p)
      (when (safe-valuep place1)
        (values (value place1) t))
    (multiple-value-bind (old-value2 old-value2-p)
        (when (safe-valuep place2)
          (values (value place2) t))
      (with-command-error-handling
          ("Could not swap ~A and ~A" place1 place2)
          (progn
            (transfer-value place1 old-value2 old-value2-p)
            (transfer-value place2 old-value1 old-value1-p)
            (rotatef (children place1) (children place2))
            (rotatef (state place1)    (state place2)))
        (transfer-value place1 old-value1 old-value1-p)
        (transfer-value place2 old-value2 old-value2-p)))))

(define-drag-and-drop-translator drag-swap-place-values
    (place command place inspector-command-table
     :gesture :select :menu nil
     :tester ((object)
              ;; Swapping must either write a new value into the place
              ;; OBJECT or write the unbound "value" into the place
              ;; OBJECT.
              (or (supportsp object 'remove-value)
                  (supportsp object 'setf)))
     :destination-tester ((object destination-object)
                          ;; Cannot swap a place with itself and
                          ;; otherwise "value" (including unbound)
                          ;; transfer must be possible in both
                          ;; directions.
                          (and (not (eq object destination-object))
                               (valid-value-transfer-p
                                object destination-object)
                               (valid-value-transfer-p
                                destination-object object)))
     :documentation ((object stream)
                     (with-print-error-handling (stream)
                       (with-safe-and-terse-printing (stream)
                         (format stream "Drag ~A onto another slot to ~
                                         swap their contents."
                                 object))))
     :pointer-documentation ((object destination-object stream)
                             (with-print-error-handling (stream)
                               (with-safe-and-terse-printing (stream)
                                 (if destination-object
                                     (format stream "Swap ~A and ~A"
                                             object destination-object)
                                     (format stream "Drag onto place ~
                                                     to swap with ~A"
                                             object))))))
    (object destination-object)
  (list 'com-swap-place-values object destination-object))

;;; Commands on Boolean-valued places

(macrolet
    ((define (command-name value value-name)
       (let ((translator-name (alexandria:symbolicate
                               '#:place-> command-name)))
         `(progn
            (define-command (,command-name :command-table inspector-command-table
                                           :name          t)
                ((place 'place))
              (with-command-error-handling ("Could not set value of ~A to ~A"
                                            place ,value)
                  (setf (value place) ,value)))

            (define-presentation-to-command-translator ,translator-name
                (place ,command-name inspector-command-table
                 :tester ((object)
                          (and (supportsp object 'setf)
                               (accepts-value-p object ,value)
                               (safe-valuep object)
                               (not (eq (value object) ,value))))
                 :priority 2
                 :documentation ,(format nil "Set to ~A" value-name)
                 :pointer-documentation
                 ((object stream)
                  (with-print-error-handling (stream)
                    (with-safe-and-terse-printing (stream)
                      (format stream "Set value of ~A to ~S" object ,value)))))
                (object)
              (list object))))))
  (define com-set-place-to-false nil "false")
  (define com-set-place-to-true  t   "true"))

;;; Commands on real-valued places

(macrolet
    ((define (command-name operator operator-name gesture pointer-button)
       (let ((translator-name (alexandria:symbolicate
                               '#:place-> command-name)))
         `(progn
            (define-command (,command-name :command-table inspector-command-table
                                           :name          t)
                ((place 'place))
              (with-command-error-handling (,(format nil "Could not ~A the value of ~~A"
                                                     operator-name)
                                            place)
                  (setf (value place) (,operator (value place)))))

            (define-gesture-name ,gesture
              :pointer-scroll (,pointer-button :control))

            (define-presentation-to-command-translator ,translator-name
                (place ,command-name inspector-command-table
                 :gesture ,gesture
                 :tester ((object)
                          (and (supportsp object 'setf)
                               (safe-valuep object)
                               (let ((value (value object)))
                                 (and (typep value 'real)
                                      (accepts-value-p object (1+ value))))))
                 :documentation ,(format nil "~@(~A~) by 1" operator-name)
                 :pointer-documentation ((object stream)
                                         (with-print-error-handling (stream)
                                           (with-safe-and-terse-printing (stream)
                                             (format stream ,(format nil "~@(~A~) ~~A by 1"
                                                                     operator-name)
                                                     object)))))
                (object)
              (list object))))))
  (define com-increment-place 1+ "increment" :increment :wheel-up)
  (define com-decrement-place 1- "decrement" :decrement :wheel-down))
