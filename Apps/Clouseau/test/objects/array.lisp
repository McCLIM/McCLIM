;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2019-2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Smoke test for inspecting arrays.

(cl:in-package #:clouseau.test)

(def-suite* :clouseau.objects.array
  :in :clouseau)

(test array.array-element-place.smoke
  "Smoke test for the `array-element-place' class."
  (let* ((array (make-string 3 :initial-element #\a))
         (place (make-instance 'clouseau::array-element-place
                               :parent    nil
                               :container array
                               :cell      0)))
    (is-true (supportsp place 'setf))
    (is (eql #\a (value place)))
    ;; Modify place
    (is-false (accepts-value-p place 4))
    (is-true (accepts-value-p place #\b))
    (setf (value place) #\b)
    (is (equalp "baa" array))))
