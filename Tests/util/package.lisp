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

   #:fails))

(cl:in-package #:clim-test-util)

(defun call-as-fails (thunk)
  (let ((failed nil))
    (handler-case (funcall thunk)
      (fiveam::check-failure (condition)
        (declare (ignore condition))
        (setf failed t)
        ;; A proper solution would signal expected-failure instead.
        (write-char #\e fiveam::*test-dribble*)))
    (unless failed
      (write-char #\? fiveam::*test-dribble*)
      (5am:fail "Unexpected success."))))

(defmacro fails (&body body)
  `(call-as-fails (lambda () ,@body)))
