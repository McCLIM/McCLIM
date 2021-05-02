;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2020 by Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:mcclim-backend-common.test)

(in-suite :mcclim-backend-common)

(test parse-text-style*.smoke
  "Smoke test for the `parse-text-style*' function."

  (mapc (lambda (input-and-expected)
          (destructuring-bind (input &optional (expected t))
              input-and-expected
            (case expected
              ((t :no-op)
               (let ((result (climi::parse-text-style* input)))
                 (is-true (text-style-p result))
                 (is-true (climi::text-style-fully-specified-p result))
                 (when (eq expected :no-op)
                   (is (eq input result)))))
              (error
               (signals error (climi::parse-text-style* input))))))
        `(;; Invalid text style specifications
          (1                 error)
          ((nil nil)         error)
          ((nil nil nil nil) error)
          ;; Valid ones
          (nil)
          ((nil nil nil))
          (,(make-text-style nil nil nil))
          (,(make-text-style :fix nil nil))
          (,(make-text-style :fix :roman :small))
          (,(make-text-style nil nil :smaller))
          (,(make-text-style nil nil :larger))
          ;; Must be returned as-is
          (,(make-text-style :fix :roman 10) :no-op))))
