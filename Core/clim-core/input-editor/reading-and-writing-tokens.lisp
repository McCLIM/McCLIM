;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2001 by Tim Moore (moore@bricoworks.com)
;;;  (c) copyright 2006-2008 by Troels Henriksen (athas@sigkill.dk)
;;;  (c) copyright 2022-2023 by Daniel Kochmański (daniel@turtleware.eu)
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-internals)

;;; Defaults for replace-input and presentation-replace-input.

(defun read-token (stream &key
                            (input-wait-handler *input-wait-handler*)
                            (pointer-button-press-handler
                             *pointer-button-press-handler*)
                            click-only)
  "Reads characters from the interactive stream STREAM until it
encounters a delimiter or activation gesture, or a pointer
gesture. Returns the accumulated string that was delimited by the
delimiter or activation gesture, leaving the delimiter unread.

If the first character of typed input is a quotation mark (#\"), then
READ-TOKEN will ignore delimiter gestures until another quotation mark
is seen. When the closing quotation mark is seen, READ-TOKEN will
proceed as above.

INPUT-WAIT-HANDLER and POINTER-BUTTON-PRESS-HANDLER are as for
STREAM-READ-GESTURE."
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
  "This function is the opposite of `read-token' given the string
token, it writes it to the interactive stream stream. If `acceptably'
is true and there are any characters in the token that are delimiter
gestures (see the macro `with-delimiter-gestures'), then `write-token'
will surround the token with quotation marks (#\").

Typically, `present' methods will use `write-token' instead of
`write-string'."
  (let ((put-in-quotes (and acceptably (some #'delimiter-gesture-p token))))
    (when put-in-quotes
      (write-char #\" stream))
    (write-string token stream)
    (when put-in-quotes
      (write-char #\" stream))))
