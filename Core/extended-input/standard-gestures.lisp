;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2001,2002 Tim Moore <moore@bricoworks.com>
;;;  (c) copyright 2019-2023 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Standard gesture definitions. Not in gestures.lisp to avoid
;;; defining some of the gesture helper functions at compile-time.
;;;

(in-package #:climi)

;;; Standard gesture names

(define-gesture-name :abort         :keyboard             (#\c :control))
(define-gesture-name :clear-input   :keyboard             (#\u :control))
(define-gesture-name :complete      :keyboard             (#\tab))
(define-gesture-name :help          :keyboard             (#\/ :control))
(define-gesture-name :possibilities :keyboard             (#\? :control))

(define-gesture-name :select        :pointer-button-press (:left))
(define-gesture-name :describe      :pointer-button-press (:middle))
(define-gesture-name :menu          :pointer-button-press (:right))
(define-gesture-name :edit          :pointer-button-press (:left :meta))
(define-gesture-name :delete        :pointer-button-press (:middle :shift))

(define-gesture-name :scroll-up     :pointer-scroll       (:wheel-up))
(define-gesture-name :scroll-down   :pointer-scroll       (:wheel-down))
(define-gesture-name :scroll-left   :pointer-scroll       (:wheel-left))
(define-gesture-name :scroll-right  :pointer-scroll       (:wheel-right))

;;; Define so we have a gesture for #\newline that we can use in
;;; *standard-activation-gestures*

(define-gesture-name :newline :keyboard (#\newline))
(define-gesture-name :newline :keyboard (#\return)  :unique nil)

(define-gesture-name :return  :keyboard (#\return))

;;; The standard delimiter

(define-gesture-name command-delimiter :keyboard (#\space))

;;; Extension: support for handling abort gestures that appears to be
;;; in real CLIM

;;; From the hyperspec, more or less

(defun invoke-condition-restart (condition)
  (let ((restarts (compute-restarts condition)))
    (loop for i from 0
          for restart in restarts
          do (format t "~&~D: ~A~%" i restart))
    (loop with n = nil
          and k = (length restarts)
          until (and (integerp n) (>= n 0) (< n k))
          do (format t "~&Option: ")
             (setf n (read))
             (fresh-line)
          finally
             #-cmu (invoke-restart (nth n restarts))
             #+cmu (funcall (conditions::restart-function (nth n restarts))))))

(defmacro catch-abort-gestures (format-args &body body)
  `(restart-case
       (handler-bind ((abort-gesture #'invoke-condition-restart))
         ,@body)
     (nil ()
       :report (lambda (stream)
                 (format stream ,@format-args))
       :test (lambda (condition)
               (typep condition 'abort-gesture))
       nil)))

;;; Known gestures
;;;
;;; Abort and accelerator gestures are special - SEIS will signal a condition
;;; whenever one of these is read. When the caller wants to handle the event,
;;; then depending on the purpose they should use these conditions.
;;;
;;; For example "possibilities gestures" should be added to the set of
;;; accelerators gestures and "completion gestures" to the set of aborts.

(macrolet ((define-standard-gesture (base-name gesture-names)
             (let ((gestures-var (symbol-concat '* base-name 's*))
                   (predicate (symbol-concat base-name '-p)))
               `(progn (defvar ,gestures-var (quote ,gesture-names))
                       (defun ,predicate (gesture)
                         (loop for gesture-name in ,gestures-var
                               when (gesture-matches-spec-p gesture gesture-name)
                                 do (return t)
                               finally (return nil)))))))
  ;; 22.2.2
  (define-standard-gesture abort-gesture         (:abort))
  (define-standard-gesture accelerator-gesture   ())
  ;; 24.2
  (define-standard-gesture activation-gesture    ())
  (define-standard-gesture delimiter-gesture     ())
  ;; 24.5
  (define-standard-gesture completion-gesture    (:complete))
  (define-standard-gesture help-gesture          (:help))
  (define-standard-gesture possibilities-gesture (:possibilities)))

(define-condition abort-gesture (condition)
  ((event :reader abort-gesture-event :initarg :event)))

(define-condition accelerator-gesture (condition)
  ((event :reader accelerator-gesture-event :initarg :event)
   (numeric-argument :reader accelerator-gesture-numeric-argument
                     :initarg :numeric-argument
                     :initform 1)))

(defvar *standard-activation-gestures* '(:newline))

(defmacro with-dynamic-gestures ((variable gestures &key override) &body body)
  ;; Preserve the evaluation order of arguments.
  (once-only (gestures override)
    `(let* ((,variable (if ,override
                           (ensure-list ,gestures)
                           (append (ensure-list ,gestures) ,variable)))            )
       ,@body)))

(defmacro with-activation-gestures ((gestures &key override) &body body)
  `(with-dynamic-gestures (*activation-gestures* ,gestures :override ,override)
     ,@body))

(defmacro with-delimiter-gestures ((gestures &key override) &body body)
  `(with-dynamic-gestures (*delimiter-gestures* ,gestures :override ,override)
     ,@body))

;;; These helper functions take the arguments of ACCEPT so that they can be used
;;; directly by ACCEPT.

(defun make-activation-gestures
    (&key (activation-gestures nil activation-gestures-p)
       (additional-activation-gestures nil additional-activations-p)
       (existing-activation-gestures *activation-gestures*)
     &allow-other-keys)
  (cond (additional-activations-p
         (append additional-activation-gestures existing-activation-gestures))
        (activation-gestures-p
         activation-gestures)
        (t (or existing-activation-gestures *standard-activation-gestures*))))

(defun make-delimiter-gestures
    (&key (delimiter-gestures nil delimiter-gestures-p)
       (additional-delimiter-gestures nil additional-delimiters-p)
       (existing-delimiter-gestures *delimiter-gestures*)
     &allow-other-keys)
  (cond (additional-delimiters-p
         (append additional-delimiter-gestures existing-delimiter-gestures))
        (delimiter-gestures-p
         delimiter-gestures)
        (t existing-delimiter-gestures)))
