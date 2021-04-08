;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2001,2002 Tim Moore <moore@bricoworks.com>
;;;  (c) copyright 2019,2020 Daniel Kochmański <daniel@turtleware.eu>
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
(define-gesture-name :complete      :keyboard             (:tab))
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
(define-gesture-name :newline :keyboard (:kp-enter) :unique nil)

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
