(cl:in-package #:clim-tests)

(def-suite* :mcclim.input-editing
  :in :mcclim)

(test input-editing.*activation-gestures*
  (is (null *activation-gestures*)))

(test input-editing.simple-parse-error
  (is (subtypep 'simple-parse-error 'parse-error))

  (finishes
    (make-condition 'simple-parse-error
                    :format-control "~A" :format-arguments (list 3)))
  (handler-case
      (simple-parse-error "foo: ~A" 3)
    (simple-parse-error (c)
      (is (search "foo: 3" (format nil "~A" c))))
    (:no-error (&rest values)
      (fail "~S returned ~S" 'simple-parse-error values))))

(test input-editing.input-not-of-required-type
  (is (subtypep 'input-not-of-required-type 'parse-error))

  (let ((c (make-condition 'input-not-of-required-type
                           :string "not an INTEGER" :type 'integer)))
    (is (search "not an INTEGER" (format nil "~A" c))))

  (signals input-not-of-required-type
    (input-not-of-required-type 3 'float)))

;;; `accept' tests

(defclass test-stream (clim:standard-extended-input-stream) ())

;;; Discard any output written by the input editor.
(defmethod climi::input-editor-format
    ((stream test-stream) format-string &rest format-args))

(test accept.smoke

  (mapc
   (lambda (case)
     (destructuring-bind (input accept-type expected-value expected-type) case
       (flet ((test-stream (stream)
                (multiple-value-bind (value type)
                    (accept accept-type :stream stream :prompt nil)
                  (is (equal expected-value value)
                      "~@<ACCEPTing ~S from a ~S with type ~S resulted ~
                       in value ~S, not ~S~@:>"
                      input (type-of stream) accept-type value expected-value)
                  (is (equal expected-value value)
                      "~@<ACCEPTing ~S from a ~S with type ~S resulted ~
                       in type ~S, not ~S~@:>"
                      input (type-of stream) accept-type type expected-type))))
         ;; Test with extended input stream.
         (let ((stream (make-instance 'test-stream)))
           (map nil (alexandria:curry #'climi::stream-append-gesture stream)
                input)
           (climi::stream-append-gesture stream #\Return)
           (test-stream stream))
         ;; Test with string stream.
         (let* ((string (format nil "~A~C" input #\Return))
                (stream (make-string-input-stream string)))
           (test-stream stream)))))
   '(("1"       integer            1         integer)
     ("1,2,3,4" (sequence integer) (1 2 3 4) (sequence integer)))))
