(cl:defpackage #:clim-test-util
  (:use
   #:clim-lisp
   #:clim
   #:clime)

  (:export
   #:print-test-pages

   #:print-test-page-1
   #:print-test-page-2
   #:print-test-page-3
   #:print-test-page-4
   #:print-test-page-5
   #:print-test-page-6
   #:print-test-page-7
   #:print-test-page-8
   #:print-test-page-9

   #:*all-test-pages*

   #:fails

   #:compilation-signals))

(cl:in-package #:clim-test-util)

;;; Expected failures
(defvar *report-expected-fails* nil)

(defun call-as-fails (thunk)
  (if *report-expected-fails*
      (funcall thunk)
      (let ((failed nil))
        (handler-case (funcall thunk)
          (fiveam::check-failure (condition)
            (declare (ignore condition))
            (setf failed t)
            ;; A proper solution would signal expected-failure instead.
            (write-char #\e fiveam::*test-dribble*)))
        (unless failed
          (write-char #\? fiveam::*test-dribble*)
          (5am:fail "Unexpected success.")))))

(defmacro fails (&body body)
  `(call-as-fails (lambda () ,@body)))

;;; Checking compile-time conditions

(defun %compilation-signals (condition-type body)
  (multiple-value-bind (function warnings-p failure-p)
      (let* ((discard (make-broadcast-stream))
             (*trace-output* discard)
             (*error-output* discard))
        (with-compilation-unit (:override t)
          (compile nil `(lambda () ,@body))))
    (declare (ignore warnings-p))
    (5am:is-true failure-p)
    (handler-bind
        ((condition (lambda (condition)
                      (when (typep condition condition-type)
                        (5am::add-result 'it.bese.fiveam::test-passed
                                         :test-expr condition-type)
                        (return-from %compilation-signals)))))
      (funcall function)
      (5am:fail "~S failed to signal a ~S condition"
                Body condition-type))))

(defmacro compilation-signals (condition &body body)
  `(%compilation-signals ',condition ',body))
