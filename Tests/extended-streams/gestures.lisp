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
          (:foo :keyboard               (#\a :no-such-modifier)   error) ; invalid modifier
          (:foo :keyboard               (#\a))                           ; ok
          (:foo :keyboard               (#\a :meta))                     ; ok
          (:foo :keyboard               #\a)                             ; extension
          (:foo :keyboard               (t))                             ; extension
          (:foo :keyboard               (#\a t))                         ; extension
          (:foo :keyboard               (t t))                           ; extension

          (:foo :pointer-button         (1)                       error) ; invalid button
          (:foo :pointer-button         (:no-such-button)         error) ; invalid button
          (:foo :pointer-button         (:left :no-such-modifier) error) ; invalid modifier
          (:foo :pointer-button         (:left))                         ; ok
          (:foo :pointer-button-press   (:middle))                       ; ok
          (:foo :pointer-button-release (:right))                        ; ok
          (:foo :pointer-scroll         (:wheel-up))                     ; ok
          (:foo :pointer-button         :left)                           ; extension
          (:foo :pointer-button         (t))                             ; extension
          (:foo :pointer-button         (:left t))                       ; extension
          (:foo :pointer-button         (t t))                           ; extension
          ;;
          (:foo :pointer-motion         (t t))
          (:foo :pointer-motion         (:left t))
          (:foo :pointer-motion         ((:left :right) t))
          (:foo :pointer-motion         (:none t))
          (:foo :pointer-motion         (:none t))
          (:foo :pointer-motion         ((:left :no-button) t)    error)
          ;;
          (:foo :timer                  (:alarm))
          (:foo :timer                  (:alarm :meta)            error))))

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
          ((:left :no-such-modifier) error))))

(test gestures.event-data-matches-gesture-p.smoke
  "Smoke test for the `event-data-matches-gesture-p' function."
  (mapc (lambda (arguments-and-expected)
          (destructuring-bind (type device-name modifier-state gesture expected)
              arguments-and-expected
            (is (eq expected
                    (climi::event-data-matches-gesture-p
                     type device-name modifier-state gesture)))))
        `(;; Wildcard gesture
          (:keyboard    #\a     0       t  t) ; extension
          (:keyboard    #\a     :ignore t  t) ; extension
          (:ignore      :ignore :ignore t  t) ; extension
          ;; Wildcards in gesture elements
          (:keyboard    #\a    0              ((t          #\a   0))    t) ; extension
          (:keyboard    #\a    0              ((:keyboard  t     0))    t) ; extension
          (:keyboard    #\a    0              ((:keyboard  #\a   t))    t) ; extension
          ;; Wildcards in event data
          (:ignore      #\a     0             ((:keyboard  #\a   0))    t)
          (:keyboard    :ignore 0             ((:keyboard  #\a   0))    t)
          (:keyboard    #\a     :ignore       ((:keyboard  #\a   0))    t)
          ;; Keyboard
          (:keyboard    #\a     0             ((:keyboard  #\a   0))    t)
          (:keyboard    #\b     0             ((:keyboard  #\a   0))    nil)
          (:keyboard    #\a     ,+shift-key+  ((:keyboard  #\a   0))    nil)
          (:keyboard    :down   0             ((:keyboard  :down 0))    t)
          ;; Pointer
          (:pointer-button-press :left  0            ((:pointer-button-press :left 0)) t)
          (:pointer-button-press :right 0            ((:pointer-button-press :left 0)) nil)
          (:pointer-button-press :left  ,+shift-key+ ((:pointer-button-press :left 0)) nil)
          ;; `:pointer-button' "sub-typing"
          (:pointer-button-press :left 0             ((:pointer-button       :left 0)) t)
          ;; Multiple physical gestures
          (:keyboard #\a 0       ((:keyboard #\b 0) (:keyboard #\a 0)) t)
          (:keyboard #\a :ignore ((:keyboard #\a 0) (:keyboard #\b 0)) t)
          ;; Extended gestures
          (:pointer-motion 0 0                      ((:pointer-motion 0 0)) t)
          (:pointer-motion ,+pointer-left-button+ 0 ((:pointer-motion 0 0)) nil)
          ;; Modifier state is ignored for timer events.
          (:timer :alarm  :ignore ((:timer t        0))   t)
          (:timer :alarm  :ignore ((:timer :alarm   0))   t)
          (:timer :alarm  :ignore ((:timer :timeout 0)) nil)
          (:timer :ignore :ignore ((:timer :timeout 0))   t))))
