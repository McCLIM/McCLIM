;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2019-2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Utilities for tests.
;;;

(cl:in-package #:clouseau.test)

(defun object-state-class-cases (&rest cases)
  (mapc (lambda (object-and-expected)
          (destructuring-bind (object expected-class
                               &optional (place-class 'basic-place))
              object-and-expected
            (let ((place (c2mop:class-prototype (c2mop:ensure-finalized
                                                 (find-class place-class)))))
              (is (eq expected-class (object-state-class object place))))))
        cases))
