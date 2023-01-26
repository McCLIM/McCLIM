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
;;; 24.5 Completion

(in-package #:clim-internals)

;;; Helpers for complete-input, which is just getting too long.

(defun complete-gesture-p (gesture)
  (or (delimiter-gesture-p gesture) (activation-gesture-p gesture)))

;;; Break out rescanning case for complete-input.
;;;
;;; funky logic; we don't know if we're still rescanning until after the call
;;; to read-gesture.
(defun complete-input-rescan (stream func partial-completers so-far
                              allow-any-input)
  (when (stream-rescanning-p stream)
    (loop for gesture = (read-gesture :stream stream :timeout 0)
          while (and gesture (stream-rescanning-p stream))
          if (complete-gesture-p gesture)
            do (let (input success object nmatches)
                 (when (gesture-match gesture partial-completers)
                   (setf (values input success object nmatches)
                         (funcall func (subseq so-far 0) :complete-limited)))
                 (unless (and (numberp nmatches) (> nmatches 0))
                   ;; Not a partial match; better be a total match
                   (setf (values input success object)
                         (funcall func (subseq so-far 0) :complete))
                   (if (or success allow-any-input)
                       (progn
                         (unread-gesture gesture :stream stream)
                         (return-from complete-input-rescan
                           (values object t input)))
                       ;; This used to be an error, but no one thought
                       ;; that was a really great idea.
                       (signal 'simple-completion-error
                               :format-control "complete-input: While rescanning,~
                                               can't match ~A~A"
                               :format-arguments (list so-far  gesture)

                               :input-so-far so-far))))
          end
          do (vector-push-extend gesture so-far)
          finally (when gesture
                    (unread-gesture gesture :stream stream))))
  nil)

(defun possibilities-for-menu (possibilities)
  (loop for (display object) in possibilities
        collect `(,display :value ,object)))

(defun possibility-printer (possibility ptype stream)
  "A default function for printing a possibility. Suitable for
used as value of `:possibility-printer' in calls to
`complete-input'"
  (with-output-as-presentation (stream possibility ptype)
    (write-string (first possibility) stream)))

(define-presentation-type possibility ()
  :inherit-from t)

(define-presentation-method presentation-typep (object (type possibility))
  (declare (ignore object))
  t)

(defun print-possibilities (possibilities possibility-printer stream)
  "Write `possibitilies' to `stream', using
`possibility-printer'. `Possibilities' must be a list of
input-completion possibilities. `Stream' must be an input-editing
stream. Output will be done to its typeout."
  (with-input-editor-typeout (stream :erase t)
    (surrounding-output-with-border (stream :shape :drop-shadow :background +cornsilk1+)
      (surrounding-output-with-border (stream :shape :rectangle)
        (format-items possibilities
                      :stream stream
                      :printer #'(lambda (possibility stream)
                                   (funcall possibility-printer
                                            possibility
                                            `((possibility) :description ,(first possibility))
                                            stream)))))))

;;; Helper returns gesture (or nil if gesture shouldn't be part of the input)
;;; and completion mode, if any.

(defvar *completion-possibilities-continuation* nil)

(defun read-completion-gesture (stream
                                partial-completers
                                help-displays-possibilities)
  (flet ((possibilitiesp (gesture)
           (or (possibilities-gesture-p gesture)
               (and help-displays-possibilities
                    (help-gesture-p gesture)))))
    (let ((*completion-possibilities-continuation*
            #'(lambda ()
                (return-from read-completion-gesture
                  (values nil :possibilities)))))
      (handler-bind ((accelerator-gesture
                       #'(lambda (c)
                           (let ((gesture (accelerator-gesture-event c)))
                             (when (possibilitiesp gesture)
                               (return-from read-completion-gesture
                                 (values nil :possibilities)))))))
        (let ((gesture (read-gesture :stream stream)))
          (values gesture
                  (cond ((possibilitiesp gesture)
                         :possibilities)
                        ((gesture-match gesture partial-completers)
                         :complete-limited)
                        ((gesture-match gesture *completion-gestures*)
                         :complete-maximal)
                        ((complete-gesture-p gesture)
                         :complete)
                        (t nil))))))))

(defun complete-input (stream func &key
                                     partial-completers allow-any-input
                                     (possibility-printer #'possibility-printer)
                                     (help-displays-possibilities t))
  (let ((so-far (make-array 1 :element-type 'character :adjustable t :fill-pointer 0))
        (*accelerator-gestures* (append *help-gestures*
                                        *possibilities-gestures*
                                        *accelerator-gestures*)))
    (flet ((insert-input (input)
             (adjust-array so-far (length input)
                           :fill-pointer (length input))
             (replace so-far input)
             ;; XXX: Relies on non-specified behavior of :rescan.
             (replace-input stream input :rescan nil))
           (read-possibility (stream possibilities)
             (unwind-protect
                  (handler-case
                      (with-input-context
                          (`(completion ,possibilities) :override nil)
                          (object type event)
                          (prog1 nil (read-gesture :stream stream :peek-p t))
                        (t object))
                    (abort-gesture () nil))
               (clear-typeout stream))))
      (multiple-value-bind (object success input)
          (complete-input-rescan stream func partial-completers
                                 so-far allow-any-input)
        (when success
          (return-from complete-input (values object success input))))
      (loop
        (multiple-value-bind (gesture mode)
            (read-completion-gesture stream
                                     partial-completers
                                     help-displays-possibilities)
          (cond
            (mode
             (multiple-value-bind
                   (input success object nmatches possibilities)
                 (funcall func (subseq so-far 0) mode)
               (when (and (zerop nmatches)
                          (eq mode :complete-limited)
                          (complete-gesture-p gesture))
                 ;; Gesture is both a partial completer and a
                 ;; delimiter e.g., #\space.  If no partial match,
                 ;; try again with a total match.
                 (setf (values input success object nmatches possibilities)
                       (funcall func (subseq so-far 0) :complete))
                 (setf mode :complete))
               ;; Preserve the delimiter
               (when (and success (eq mode :complete))
                 (unread-gesture gesture :stream stream))
               ;; Get completion from menu
               (when (and (> nmatches 0) (eq mode :possibilities))
                 (print-possibilities possibilities possibility-printer stream)
                 (redraw-input-buffer stream)
                 (if-let ((possibility (read-possibility stream possibilities)))
                   (setf input (first possibility)
                         object (second possibility)
                         success t
                         nmatches 1)
                   (setf success nil
                         nmatches 0)))
               (unless (and (eq mode :complete) (not success))
                 (if (> nmatches 0)
                     (insert-input input)
                     (beep)))
               (cond ((and success (eq mode :complete))
                      (return-from complete-input
                        (values object success input)))
                     ((activation-gesture-p gesture)
                      (if allow-any-input
                          (return-from complete-input
                            (values nil t (subseq so-far 0)))
                          (error 'simple-completion-error
                                 :format-control "Input ~S does not match"
                                 :format-arguments (list so-far)
                                 :input-so-far so-far))))))
            ((null gesture) ; e.g. end-of-input if STREAM is a string stream
             (return-from complete-input (values nil nil so-far)))
            (t
             (vector-push-extend gesture so-far))))))))

;;; helper function

(defun left-prefix (string1 string2 &key (end nil))
  "Returns the common prefix of string1 and string2, up to end"
  (let* ((end1 (if end
                   (min (length string1) end)
                   nil))
         (end2 (if end
                   (min (length string2) end)
                   nil))
         (mismatch (mismatch string1 string2 :test #'char-equal
                                             :end1 end1 :end2 end2)))
    (cond (mismatch
           (subseq string1 0 mismatch))
          (end
           (subseq string1 0 end))
          (t string1))))

(defun complete-from-generator (initial-string generator delimiters &key
                                                                      (action :complete)
                                                                      (predicate (constantly t)))
  (when (eq action :possibilities)
    (return-from complete-from-generator
      (complete-from-generator-possibilities initial-string
                                             generator
                                             predicate)))
  (let ((initial-string-len (length initial-string))
        (candidate-match nil)
        (matches 0)
        (object nil)
        (identical nil)
        (identical-match nil)
        (identical-object nil)
        (actual-match nil))
    (flet ((suggester (str obj)
             (unless (funcall predicate obj)
               (return-from suggester nil))
             (let ((partial-match-end
                     (and (eq action :complete-limited)
                          (>= (length str) initial-string-len)
                          (position-if #'(lambda (c) (member c delimiters))
                                       str
                                       :start initial-string-len))))
               (when (and (eq action :complete-limited)
                          (null partial-match-end))
                 (return-from suggester nil))
               (unless partial-match-end
                 (setq partial-match-end (1- (length str))))
               (let ((mismatch-initial (mismatch initial-string str
                                                 :test #'char-equal)))
                 (cond ((and mismatch-initial
                             (>= mismatch-initial (length initial-string)))
                        (incf matches)
                        (unless candidate-match
                          (setq object obj))
                        (setf candidate-match
                              (cond (candidate-match
                                     (left-prefix candidate-match
                                                  str
                                                  :end (1+ partial-match-end)))
                                    (partial-match-end
                                     (subseq str 0 (1+ partial-match-end)))
                                    (t str))
                              actual-match str))
                       ((null mismatch-initial)
                        (incf matches)
                        ;; If there's a longer match we want to find it.
                        (if (eq action :complete-maximal)
                            (progn
                              (setf identical-match str)
                              (setf identical-object obj))
                            (progn
                              (setf candidate-match str)
                              (setf object obj)))
                        (setf identical t)))))))
      (funcall generator initial-string #'suggester)
      (let ((partial-match-before-end (and (eq action :complete-limited)
                                           (eql matches 1)
                                           (< (length candidate-match)
                                              (length actual-match)))))
        (values (or candidate-match identical-match initial-string)
                (or (and identical
                         (or (not (eq action :complete-maximal))
                             (eql matches 1)))
                    (and (eql matches 1)
                         (not partial-match-before-end)))
                (if (eq action :complete-maximal)
                    (cond ((and (eql matches 2) identical-match)
                           object)
                          ((and identical-match (eql matches 1))
                           identical-object)
                          ((eql matches 1)
                           object))
                    (and (or identical (and (eql matches 1)
                                            (not partial-match-before-end)))
                         object))
                matches
                nil)))))

;;; The possibilities action is different enough that I don't want to add to
;;; the spaghetti above...

(defun complete-from-generator-possibilities
    (initial-string generator predicate)
  (let ((possibilities nil)
        (nmatches 0)
        (initial-len (length initial-string)))
    (flet ((suggester (str obj)
             (unless (funcall predicate obj)
               (return-from suggester nil))
             (when (>= (or (mismatch initial-string str :test #'char-equal)
                           (length initial-string))
                       initial-len)
               (incf nmatches)
               (push (list str obj) possibilities))))
      (funcall generator initial-string #'suggester)
      (if (and (eql nmatches 1)
               (string-equal initial-string (caar possibilities)))
          ;; return values are as from complete-from-generator, qv.
          (values (caar possibilities)
                  t
                  (cdar possibilities)
                  nmatches
                  possibilities)
          (values initial-string nil nil nmatches (sort possibilities #'string-lessp :key #'car))))))

(defun complete-from-possibilities (initial-string completions delimiters
                                    &key (action :complete)
                                         (predicate (constantly t))
                                         (name-key #'car)
                                         (value-key #'second))
  (flet ((generator (input-string suggester)
           (declare (ignore input-string))
           (do-sequence (possibility completions)
             (funcall suggester
                      (funcall name-key possibility)
                      (funcall value-key possibility)))))
    (complete-from-generator initial-string #'generator delimiters
                             :action action :predicate predicate)))

(defun suggest (completion object)
  "Specifies one possibility for
`completing-from-suggestions'. `Completion' is a string, the
printed representation of object. `Object' is the internal
representation.

Calling this function outside of the body of
`completing-from-suggestions' is an error."
  (declare (ignore completion object))
  (error
   "SUGGEST called outside of lexical scope of COMPLETING-FROM-SUGGESTIONS" ))

(defmacro completing-from-suggestions ((stream &rest args) &body body)
  "Reads input from the input editing stream `stream', completing
over a set of possibilities generated by calls to `suggest'
within `body'. `Body' may have zero or more declarations as its
first forms.

`Completing-from-suggestions' returns three values, `object',
`success', and `string'.

The stream argument is not evaluated, and must be a symbol that
is bound to a stream. If `stream' t is (the default),
`*standard-input*' is used. `Partial-completers',
`allow-any-input', and `possibility-printer' are as for
`complete-input'.

Implementations will probably use `complete-from-generator' to
implement this."
  (when (eq stream t)
    (setq stream '*standard-input*))
  (let ((generator (gensym "GENERATOR"))
        (input-string (gensym "INPUT-STRING"))
        (suggester (gensym "SUGGESTER")))
    `(flet ((,generator (,input-string ,suggester)
              (declare (ignore ,input-string))
              (flet ((suggest (completion object)
                       (funcall ,suggester completion object)))
                ,@body)))
       ;; This sucks, but we can't use args to the macro directly because
       ;; we want the partial-delimiters argument and we need to insure its
       ;; proper evaluation order with everything else.
       (let* ((complete-input-args (list ,@args))
              (partial-completers (getf complete-input-args
                                        :partial-completers
                                        nil)))
         (apply #'complete-input
                ,stream
                #'(lambda (so-far mode)
                    (complete-from-generator so-far
                                             #',generator
                                             partial-completers
                                             :action mode))
                complete-input-args)))))
