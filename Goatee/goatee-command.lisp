;;; -*- Mode: Lisp; Package: GOATEE -*-

;;;  (c) copyright 2002 by Tim Moore (moore@bricoworks.com)
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

(in-package :goatee)

;;; dispatching commands and, eventually, defining commands

;;; A gesture table is a hash table keyed by keyboard gesture (and
;;; pointer gesture too?) name.  The value is an alist of
;;; (modifier-bits . command-name).
;;;
;;; XXX This is looking up keysym names, not gestures.  Do we care?
;;; Probably...

(defparameter *simple-area-gesture-table* (make-hash-table))
(defvar *kill-ring* (make-ring))
(defvar *last-command* nil)
(defvar *insert-extent* nil)

;; This doesn't match the emacs behavior exactly, but I think these make more
;; sense for working in lisp. +,*,and others are what's missing. 
(defparameter *word-delimiters* (concatenate 'string "() {}[]-:;,.#'`&\""
                                             '(#\newline #\linefeed #\tab #\return))
  "Characters which delimit words for the Meta-F/B/etc movement commands.")


(defun add-gesture-command-to-table (gesture-spec command-name table)
  (multiple-value-bind (gesture-name modifier-bits)
      (if (atom gesture-spec)
	  (values gesture-spec 0)
	  (values (car gesture-spec)
		  (apply #'make-modifier-state (cdr gesture-spec))))
    (push (cons modifier-bits command-name)
	  (gethash gesture-name table nil))))

(defgeneric lookup-gesture-command (gesture table))

(defmethod lookup-gesture-command :around (gesture table)
  (declare (ignore table))
  (if (activation-gesture-p gesture)
      #'insert-activation-gesture
      (call-next-method)))

(defmethod lookup-gesture-command ((gesture character) table)
  (cdr (assoc 0 (gethash gesture table nil))))

(defmethod lookup-gesture-command ((gesture key-press-event) table)
  (let ((modifier-state (logandc1 climi::+alt-key+
				  (event-modifier-state gesture))))
    #+nil
    (format *trace-output* "lookup-gesture-command: ~S ~S~%"
	    modifier-state
	    (keyboard-event-character gesture))
    (cdr (assoc modifier-state
                (gethash (or (keyboard-event-character gesture)
                             (keyboard-event-key-name gesture))
                         table nil)))))

(defmethod lookup-gesture-command (gesture table)
  (declare (ignore gesture table))
  nil)

(defvar *area*)
(defvar *buffer*)

(defvar *error-fallthrough* nil)

(defmethod execute-gesture-command (gesture (area editable-area) table)
  (let ((command (lookup-gesture-command gesture table)))
    (if command
	(let ((*area* area)
	      (*buffer* (buffer area)))
	  (block error-out
	    (handler-bind ((goatee-error #'(lambda (c)
					     (unless *error-fallthrough*
					       (print c *trace-output*)
					       (beep)
					       (return-from error-out nil)))))
	      (funcall command :input-gesture gesture)
	      (setf *last-command* command)
	      (setf (last-command area) command)))
	  (redisplay-area area)))))

;; Utilities for the word movement commands

(defun move-until (movefn test)
  (let ((line (point* *buffer*)))
    (loop do (funcall movefn)        
      until (or (not (eq line (point* *buffer*)))
                (funcall test (char-ref *buffer* (point *buffer*)))))))

(defun move-until-test-first (movefn test)
  (loop for c = (char-ref *buffer* (point *buffer*))
    until (funcall test c)
    do (funcall movefn)))

(defun insert-character (&key input-gesture &allow-other-keys)
  (insert *buffer* input-gesture))

;;; Will take numeric argument
(defun delete-character (&key &allow-other-keys)
  (delete-char *buffer*))

(defun backwards-delete-character (&key &allow-other-keys)
  (delete-char *buffer* -1))

(defun forward-character (&key &allow-other-keys)
  (setf (point* *buffer*) (forward-char* *buffer* 1)))

(defun backward-character (&key &allow-other-keys)
  (setf (point* *buffer*) (forward-char* *buffer* -1)))

;; Why must I handle both flexivector and buffer bounds errors below?
(defun forward-word (&key &allow-other-keys)
  (handler-case
      (progn
        (move-until-test-first #'forward-character #'(lambda (c) (not (find c *word-delimiters* :test #'char=))))
        (move-until-test-first #'forward-character #'(lambda (c) (find c *word-delimiters* :test #'char=))))
    (buffer-bounds-error ())
    (flexivector-bounds-error ())))

(defun backward-word (&key &allow-other-keys)
  (handler-case
      (progn
        (move-until #'backward-character #'(lambda (c) (not (find c *word-delimiters* :test #'char=))))
        (move-until #'backward-character #'(lambda (c) (find c *word-delimiters* :test #'char=)))
        (forward-character))
    (buffer-bounds-error ())
    (flexivector-bounds-error ())))

(defun backwards-delete-word (&key &allow-other-keys)
  (flet ((current-loc ()  (multiple-value-bind (line pos)
                              (point* *buffer*)
                            (make-instance 'location :line line :pos pos))))
    (let ((end-location (current-loc)))
      (backward-word)
      (kill-region *kill-ring* *buffer* (current-loc) end-location))))

(defun delete-word (&key &allow-other-keys)
  (flet ((current-loc ()  (multiple-value-bind (line pos)
                              (point* *buffer*)
                            (make-instance 'location :line line :pos pos))))
    (let ((start-location (current-loc)))
      (forward-word)
      (kill-region *kill-ring* *buffer* start-location (current-loc)))))

(defun end-line (&key &allow-other-keys)
  (setf (point* *buffer*) (end-of-line* *buffer*)))

(defun beginning-line (&key &allow-other-keys)
  (setf (point* *buffer*) (beginning-of-line* *buffer*)))

(defun insert-activation-gesture (&key input-gesture &allow-other-keys)
  (setf (point* *buffer*) (end-of-buffer* *buffer*))
  (insert *buffer* input-gesture))

(defun clear-input-buffer (&key &allow-other-keys)
  (clear-buffer *buffer*))

(defun kill-line (&key &allow-other-keys)
  (multiple-value-bind (line pos)
      (point* *buffer*)
      (let* ((last-point (line-last-point line))
	     (start-location (make-instance 'location :line line :pos pos))
	     (end-location (make-instance 'location :line line :pos last-point)))
	(kill-region *kill-ring* *buffer* start-location end-location))))

(defun cmd-yank (&key &allow-other-keys)
  (multiple-value-bind (line pos)
      (point* *buffer*)
    (setf *insert-extent* (make-instance 'extent
					   :start-line line
					   :start-pos pos
					   :end-state :open))
    (format *trace-output* "cmd-yank: ~S, ~S~%"
	    (pos (bp-start *insert-extent*))
	    (pos (bp-end *insert-extent*)))
    (yank *kill-ring* *buffer* *insert-extent*)
    (setf (slot-value *insert-extent* 'bp-end) (point *buffer*))
    (setf (end-state *insert-extent*) :closed)
    (format *trace-output* "cmd-yank: ~S, ~S~%"
	    (pos (bp-start *insert-extent*))
	    (pos (bp-end *insert-extent*)))))

(defun cmd-yank-next (&key &allow-other-keys)
  (unless (or (eq *last-command* 'cmd-yank)
	      (eq *last-command* 'cmd-yank-prev)
	      (eq *last-command* 'cmd-yank-next))
    ;; maybe do something better than an error?
    (error "Last operation was not a yank!"))
  (format *trace-output* "cmd-yank-next: ~S, ~S~%"
	  (pos (bp-start *insert-extent*))
	  (pos (bp-end *insert-extent*)))
  (yank-next *kill-ring* *buffer* *insert-extent*))

(defun cmd-yank-prev (&key &allow-other-keys)
  (unless (or (eq *last-command* 'cmd-yank)
	      (eq *last-command* 'cmd-yank-prev)
	      (eq *last-command* 'cmd-yank-next))
    ;; maybe do something better than an error?
    (error "Last operation was not a yank!"))
  (yank-prev *kill-ring* *buffer* *insert-extent*))

;; Transposing (taken from climacs)

(defun at-beginning-of-buffer-p (buffer)
  (and (first-line-p (line (point buffer)))
       (zerop (pos (point buffer)))))

(defun at-end-of-line-p (buffer)
  (multiple-value-bind (line pos) (location* (point buffer))
    (declare (ignore line))
    (multiple-value-bind (eoline eolpos) (end-of-line* buffer)
      (declare (ignore eoline))
      (= eolpos pos))))

(defun cmd-transpose-chars (&key &allow-other-keys)
  (unless (at-beginning-of-buffer-p *buffer*)
    (with-point (*buffer*)
      (when (at-end-of-line-p *buffer*)
        (backward-character))
      (let ((object (char-ref *buffer* (point *buffer*))))
        (delete-char *buffer*)
        (backward-character)
        (insert *buffer* object)))))

;; Line motion

(defun up-line (&key &allow-other-keys)
  (move-lines -1))

(defun down-line (&key &allow-other-keys)
  (move-lines 1))

(defun move-lines (n)
  (unless (goal-column-preserving-p (last-command *area*))
    (setf (goal-column *area*) (pos (point *buffer*))))
  (setf (point* *buffer*)
	(next-line *buffer* n :pos (goal-column *area*))))

(defun goal-column-preserving-p (cmd)
  (member cmd '(up-line down-line)))


(loop for i from (char-code #\space) to (char-code #\~)
      do (add-gesture-command-to-table (code-char i)
				       'insert-character
				       *simple-area-gesture-table*))
;;; people who use dead keys get to implement code for that in Goatee.
(loop for i from 160 to 255
      do (add-gesture-command-to-table (code-char i)
                                       'insert-character
                                       *simple-area-gesture-table*))

(add-gesture-command-to-table #\tab
			      'insert-character
			      *simple-area-gesture-table*)

(add-gesture-command-to-table #\newline
			      'insert-character
			      *simple-area-gesture-table*)

;; I've changed the key mapping around a bit in an attempt
;; to get things more emacs-like. --Hefner
(add-gesture-command-to-table #\Backspace
			      'backwards-delete-character
			      *simple-area-gesture-table*)

(add-gesture-command-to-table '(#\d :control)
			      'delete-character
			      *simple-area-gesture-table*)

(add-gesture-command-to-table '(#\Rubout)
			      'delete-character
			      *simple-area-gesture-table*)

(add-gesture-command-to-table '(#\f :control)
			      'forward-character
			      *simple-area-gesture-table*)
(add-gesture-command-to-table '(:right)
			      'forward-character
			      *simple-area-gesture-table*)

(add-gesture-command-to-table '(#\b :control)
			      'backward-character
			      *simple-area-gesture-table*)
(add-gesture-command-to-table '(:left)
			      'backward-character
			      *simple-area-gesture-table*)

(add-gesture-command-to-table '(#\f :meta)
			      'forward-word
			      *simple-area-gesture-table*)
(add-gesture-command-to-table '(:right :meta)
			      'forward-word
			      *simple-area-gesture-table*)
(add-gesture-command-to-table '(:right :control)
			      'forward-word
			      *simple-area-gesture-table*)

(add-gesture-command-to-table '(#\b :meta)
			      'backward-word
			      *simple-area-gesture-table*)
(add-gesture-command-to-table '(:left :meta)
			      'backward-word
			      *simple-area-gesture-table*)
(add-gesture-command-to-table '(:left :control)
			      'backward-word
			      *simple-area-gesture-table*)

(add-gesture-command-to-table '(#\backspace :meta)
			      'backwards-delete-word
			      *simple-area-gesture-table*)
(add-gesture-command-to-table '(#\backspace :control)
			      'backwards-delete-word
			      *simple-area-gesture-table*)

(add-gesture-command-to-table '(#\d :meta)
			      'delete-word
			      *simple-area-gesture-table*)
(add-gesture-command-to-table '(#\rubout :meta)
			      'delete-word
			      *simple-area-gesture-table*)
(add-gesture-command-to-table '(#\rubout :control)
			      'delete-word
			      *simple-area-gesture-table*)

(add-gesture-command-to-table '(#\a :control)
			      'beginning-line
			      *simple-area-gesture-table*)
(add-gesture-command-to-table '(:home)
			      'beginning-line
			      *simple-area-gesture-table*)

(add-gesture-command-to-table '(#\e :control)
			      'end-line
			      *simple-area-gesture-table*)
(add-gesture-command-to-table '(:end)
			      'end-line
			      *simple-area-gesture-table*)

(add-gesture-command-to-table '(#\k :control)
			      'kill-line
			      *simple-area-gesture-table*)

(add-gesture-command-to-table '(#\u :control)
			      'clear-input-buffer
			      *simple-area-gesture-table*)

(add-gesture-command-to-table '(#\p :control)
			      'up-line
			      *simple-area-gesture-table*)
(add-gesture-command-to-table '(:up)
			      'up-line
			      *simple-area-gesture-table*)

(add-gesture-command-to-table '(#\n :control)
			      'down-line
			      *simple-area-gesture-table*)
(add-gesture-command-to-table '(:down)
			      'down-line
			      *simple-area-gesture-table*)

(add-gesture-command-to-table '(#\y :control)
			      'cmd-yank
			      *simple-area-gesture-table*)

(add-gesture-command-to-table '(#\t :control)
			      'cmd-transpose-chars
			      *simple-area-gesture-table*)

#+nil
(add-gesture-command-to-table '(#\y :meta)
			      'cmd-yank-next
			      *simple-area-gesture-table*)

#+nil
(add-gesture-command-to-table '(#\y :control :meta)
			      'cmd-yank-prev
			      *simple-area-gesture-table*)
;;; Debugging fun

(defun goatee-break (&key &allow-other-keys)
  (break))

(add-gesture-command-to-table '(#\b :control :meta)
			      'goatee-break
			      *simple-area-gesture-table*)

