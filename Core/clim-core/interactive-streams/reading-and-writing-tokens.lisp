;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2001 by Tim Moore (moore@bricoworks.com)
;;;  (c) copyright 2006-2008 by Troels Henriksen (athas@sigkill.dk)
;;;  (c) copyright 2022 by Daniel Kochmański (daniel@turtleware.eu)
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-internals)

;; (defmethod replace-input (stream new-input &key start end buffer-start rescan))

;; (defmethod presentation-replace-input
;;     (stream object type view
;;      &key buffer-start rescan query-identifier for-context-type))

(defun read-token (stream &key
                            (input-wait-handler *input-wait-handler*)
                            (pointer-button-press-handler
                             *pointer-button-press-handler*)
                            click-only)
  (let ((result (make-array 1
                            :adjustable t
                            :fill-pointer 0
                            :element-type 'character))
        (in-quotes nil))
    ;; The spec says that read-token ignores delimiter gestures if the
    ;; first character is #\", until it sees another.  OK... what about
    ;; other occurences of #\"?  Guess we'll just accumulate them.
    (loop for first-char = t then nil
          for gesture = (read-gesture
                         :stream stream
                         :input-wait-handler input-wait-handler
                         :pointer-button-press-handler
                         pointer-button-press-handler)
          do (cond ((or (null gesture)
                        (activation-gesture-p gesture)
                        (typep gesture 'pointer-button-event)
                        (and (not in-quotes)
                             (delimiter-gesture-p gesture)))
                    (loop-finish))
                   ((and (not click-only) (characterp gesture))
                    (if (eql gesture #\")
                        (cond (first-char
                               (setq in-quotes t))
                              (in-quotes
                               (setq in-quotes nil))
                              (t (vector-push-extend gesture result)))
                        (vector-push-extend gesture result)))
                   (t nil))
          finally (progn
                    (when gesture
                      (unread-gesture gesture :stream stream))
                    ;; Return a simple string.  XXX Would returning an
                    ;; adjustable string be so bad?
                    (return (subseq result 0))))))

(defun write-token (token stream &key acceptably)
  (let ((put-in-quotes (and acceptably (some #'delimiter-gesture-p token))))
    (when put-in-quotes
      (write-char #\" stream))
    (write-string token stream)
    (when put-in-quotes
      (write-char #\" stream))))
