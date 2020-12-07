;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------

(cl:in-package #:clim-tests)

(def-suite* :mcclim.gestures
  :in :mcclim)

(test gestures.add-gesture-name.smoke
  "Test errors signaled by `add-gesture-name'."

  (mapc (lambda (arguments-and-expected)
          (destructuring-bind (name type gesture-spec &optional expected)
              arguments-and-expected
            (flet ((do-it ()
                     (add-gesture-name name type gesture-spec)))
              (if expected
                  (signals error (do-it))
                  (finishes (do-it))))))
        '((1    :keyboard               (#\a)                     error) ; invalid name

          (:foo :no-such-type           (#\a)                     error) ; invalid type

          (:foo :keyboard               (1)                       error) ; invalid key
          (:foo :keyboard               (:no-such-key)            error) ; invalid key
          (:foo :keyboard               (#\a :no-such-modifier)   error) ; invalid modifier
          (:foo :keyboard               (#\a))                           ; ok
          (:foo :keyboard               (#\a :meta))                     ; ok
          (:foo :keyboard               #\a)                             ; extension

          (:foo :pointer-button         (1)                       error) ; invalid button
          (:foo :pointer-button         (:no-such-button)         error) ; invalid button
          (:foo :pointer-button         (:left :no-such-modifier) error) ; invalid modifier
          (:foo :pointer-button         (:left))                         ; ok
          (:foo :pointer-button-press   (:middle))                       ; ok
          (:foo :pointer-button-release (:right))                        ; ok
          (:foo :pointer-scroll         (:wheel-up))                     ; ok
          (:foo :pointer-button         :left))))                        ; extension

(test gestures.ensure-physical-gesture.smoke
  "Smoke test for the `ensure-physical-gesture' function."

  (mapc (lambda (designator-and-expected)
          (destructuring-bind (designator expected) designator-and-expected
            (flet ((do-it ()
                     (climi::ensure-physical-gesture designator)))
              (case expected
                (error (signals error (do-it)))
                (t     (equal expected (do-it)))))))

        `(;; Physical gestures
          ((:keyboard #\x 0)         (:keyboard #\x 0))
          ;; Keys
          (#\x                       (:keyboard #\x 0))
          ((#\x :control)            (:keyboard #\x ,+control-key+))
          (:left                     (:keyboard :left 0))
          ((:left :meta)             (:keyboard :feft ,+meta-key+))
          ;; Errors
          (:no-such-key              error)
          ((:select)                 error)
          ((:no-such-key :meta)      error)
          ((:left :no-such-modifier) error))))
