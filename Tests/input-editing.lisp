(in-package :clim-tests)

(assert (null *activation-gestures*))

;;; SIMPLE-PARSE-ERROR
(assert (subtypep 'simple-parse-error 'parse-error))

(make-condition 'simple-parse-error 
                :format-control "~A" :format-arguments (list 3))

(handler-case
    (simple-parse-error "foo: ~A" 3)
  (simple-parse-error (c)
    (assert (search "foo: 3" (format nil "~A" c))))
  (:no-error (&rest values)
    (error "~S returned ~S" 'simple-parse-error values)))

;;; INPUT-NOT-OF-REQUIRED-TYPE
(assert (subtypep 'input-not-of-required-type 'parse-error))

(let ((c (make-condition 'input-not-of-required-type
                         :string "not an INTEGER" :type 'integer)))
  (assert (search "not an INTEGER" (format nil "~A" c))))

(handler-case
    (input-not-of-required-type 3 'float)
  (input-not-of-required-type ())
  (:no-error (&rest values)
    (error "~S returned ~S" 'input-not-of-required-type values)))
