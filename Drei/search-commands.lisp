;;; -*- Mode: Lisp; Package: DREI-COMMANDS -*-

;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2004-2005 by
;;;           Elliott Johnson (ejohnson@fasl.info)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)

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

;;; Search commands for Drei. 

(in-package :drei-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; String search

(define-command (com-string-search :name t :command-table search-table)
    ((string 'string :prompt "String Search"))
  "Prompt for a string and search forward for it.
If found, leaves point after string. If not, leaves point where it is."
  (search-forward *current-point* string :test (case-relevant-test string)))

(define-command (com-reverse-string-search :name t :command-table search-table)
    ((string 'string :prompt "Reverse String Search"))
  "Prompt for a string and search backward for it.
If found, leaves point before string. If not, leaves point where it is."
  (search-backward *current-point* string :test (case-relevant-test string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Word search

(define-command (com-word-search :name t :command-table search-table)
    ((word 'string :prompt "Search word"))
  "Prompt for a whitespace delimited word and search forward for it.
If found, leaves point after the word. If not, leaves point where it is."
  (search-word-forward *current-point* word))

(define-command (com-reverse-word-search :name t :command-table search-table)
    ((word 'string :prompt "Search word"))
  "Prompt for a whitespace delimited word and search backward for it.
If found, leaves point before the word. If not, leaves point where it is."
  (search-word-backward *current-point* word))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Incremental search

(make-command-table 'isearch-drei-table :errorp nil)

(defun isearch-command-loop (pane forwardp)
  (let* ((point (point pane))
         (orig-offset (offset point)))
    (unless (endp (isearch-states pane))
      (setf (isearch-previous-string pane)
            (search-string (first (isearch-states pane)))))
    (setf (isearch-mode pane) t)
    (setf (isearch-states pane)
          (list (make-instance 'isearch-state
                               :search-string ""
                               :search-mark (clone-mark point)
                               :search-forward-p forwardp
                               :search-success-p t)))
    (simple-command-loop 'isearch-drei-table
                         (isearch-mode pane)
                         ((display-message "Mark saved where search started")
                          (setf (offset (mark pane)) orig-offset)
                          (setf (isearch-mode pane) nil))
                         ((display-message "Returned point to original location")
                          (setf (offset (point pane)) orig-offset)
                          (setf (isearch-mode pane) nil)
                          (signal 'abort-gesture :event *current-gesture*)))))

(defun isearch-from-mark (pane mark string forwardp)
  (let* ((point (point pane))
	 (mark2 (clone-mark mark))
	 (success (funcall (if forwardp #'search-forward #'search-backward)
			   mark2
			   string
			   :test (case-relevant-test string))))
    (when success
      (setf (offset point) (offset mark2)
	    (offset mark) (if forwardp
			      (- (offset mark2) (length string))
			      (+ (offset mark2) (length string)))))
    (display-message "~:[Failing ~;~]Isearch~:[ backward~;~]: ~A"
		     success forwardp (display-string string))
    (push (make-instance 'isearch-state
	     :search-string string
	     :search-mark mark
	     :search-forward-p forwardp
	     :search-success-p success)
	  (isearch-states pane))
    (unless success
      (beep))))

(define-command (com-isearch-forward :name t :command-table search-table) ()
  (display-message "Isearch: ")
  (isearch-command-loop *current-window* t))

(set-key 'com-isearch-forward
	 'search-table
	 '((#\s :control)))

(define-command (com-isearch-backward :name t :command-table search-table) ()
  (display-message "Isearch backward: ")
  (isearch-command-loop *current-window* nil))

(set-key 'com-isearch-backward
	 'search-table
	 '((#\r :control)))

(defun isearch-append-char (char)
  (let* ((states (isearch-states *current-window*))
         (string (concatenate 'string
                              (search-string (first states))
                              (string char)))
         (mark (clone-mark (search-mark (first states))))
         (forwardp (search-forward-p (first states))))
    (unless (or forwardp (end-of-buffer-p mark))
      (incf (offset mark)))
    (isearch-from-mark *current-window* mark string forwardp)))

(define-command (com-isearch-append-char :name t :command-table isearch-drei-table) ()
  (isearch-append-char *current-gesture*))

(define-command (com-isearch-append-newline :name t :command-table isearch-drei-table) ()
  (isearch-append-char #\Newline))

(defun isearch-append-text (movement-function)
  (let* ((states (isearch-states *current-window*))
	 (start (clone-mark *current-point*))
	 (mark (clone-mark (search-mark (first states))))
	 (forwardp (search-forward-p (first states))))
    (funcall movement-function *current-point*)
    (let* ((start-offset (offset start))
	   (point-offset (offset *current-point*))
	   (string (concatenate 'string
				(search-string (first states))
				(buffer-substring *current-buffer*
						  start-offset
						  point-offset))))
      (unless (or forwardp (end-of-buffer-p mark))
	(incf (offset mark) (- point-offset start-offset)))
      (isearch-from-mark *current-window* mark string forwardp))))

(define-command (com-isearch-append-word :name t :command-table isearch-drei-table) ()
  (isearch-append-text #'(lambda (mark) (forward-word mark *current-syntax*))))

(define-command (com-isearch-append-line :name t :command-table isearch-drei-table) ()
  (isearch-append-text #'end-of-line))

(define-command (com-isearch-append-kill :name t :command-table isearch-drei-table) ()
  (let* ((states (isearch-states *current-window*))
	 (yank (handler-case (kill-ring-yank *kill-ring*)
                 (empty-kill-ring ()
                   "")))
	 (string (concatenate 'string
			      (search-string (first states))
			      yank))
	 (mark (clone-mark (search-mark (first states))))
	 (forwardp (search-forward-p (first states))))
    (unless (or forwardp (end-of-buffer-p mark))
      (incf (offset mark) (length yank)))
    (isearch-from-mark *current-window* mark string forwardp)))

(define-command (com-isearch-delete-char :name t :command-table isearch-drei-table) ()
  (let* ((pane *current-window*))
    (cond ((null (second (isearch-states pane)))
	   (display-message "Isearch: ")
           (beep))
          (t
           (pop (isearch-states pane))
           (loop until (endp (rest (isearch-states pane)))
                 until (search-success-p (first (isearch-states pane)))
                 do (pop (isearch-states pane)))
           (let ((state (first (isearch-states pane))))
             (setf (offset (point pane))
                   (if (search-forward-p state)
                       (+ (offset (search-mark state))
                          (length (search-string state)))
                       (- (offset (search-mark state))
                          (length (search-string state)))))
	     (display-message "Isearch~:[ backward~;~]: ~A"
			      (search-forward-p state)
			      (display-string (search-string state))))))))

(define-command (com-isearch-search-forward :name t :command-table isearch-drei-table) ()
  (let* ((states (isearch-states *current-window*))
         (string (if (null (second states))
                     (isearch-previous-string *current-window*)
                     (search-string (first states))))
         (mark (clone-mark *current-point*)))
    (isearch-from-mark *current-window* mark string t)))

(define-command (com-isearch-search-backward :name t :command-table isearch-drei-table) ()
  (let* ((states (isearch-states *current-window*))
         (string (if (null (second states))
                     (isearch-previous-string *current-window*)
                     (search-string (first states))))
         (mark (clone-mark *current-point*)))
    (isearch-from-mark *current-window* mark string nil)))

(define-command (com-isearch-exit :name t :command-table isearch-drei-table) ()
  (let* ((states (isearch-states *current-window*))
	 (string (search-string (first states)))
	 (search-forward-p (search-forward-p (first states))))
    (setf (isearch-mode *current-window*) nil)
    (when (string= string "")
      (execute-frame-command *application-frame*
			     (funcall
			      *partial-command-parser*
			      (frame-command-table *application-frame*)
			      (frame-standard-input *application-frame*)
			      (if search-forward-p
				  `(com-string-search ,*unsupplied-argument-marker*)
				  `(com-reverse-string-search ,*unsupplied-argument-marker*))
			      0)))))

(defun isearch-set-key (gesture command)
  (add-command-to-command-table command 'isearch-drei-table
                                :keystroke gesture :errorp nil))

(loop for code from (char-code #\Space) to (char-code #\~)
      do (isearch-set-key (code-char code) 'com-isearch-append-char))

(isearch-set-key '(#\Newline) 'com-isearch-exit)
(isearch-set-key '(#\Backspace) 'com-isearch-delete-char)
(isearch-set-key '(#\s :control) 'com-isearch-search-forward)
(isearch-set-key '(#\r :control) 'com-isearch-search-backward)
(isearch-set-key '(#\j :control) 'com-isearch-append-newline)
(isearch-set-key '(#\w :control) 'com-isearch-append-word)
(isearch-set-key '(#\y :control) 'com-isearch-append-line)
(isearch-set-key '(#\y :meta) 'com-isearch-append-kill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Unconditional replace

(define-command (com-replace-string :name t :command-table search-table)
    ()
  "Replace all occurrences of `string' with `newstring'."
  ;; We have to do it this way if we want to refer to STRING in NEWSTRING
  (let* ((string (accept 'string :prompt "Replace String"))
	 (newstring (accept'string :prompt (format nil "Replace ~A with" string))))
    (loop with point = *current-point*
	  with length = (length string)
	  with use-region-case = (no-upper-p string)
	  for occurrences from 0
	  while (let ((offset-before (offset point)))
                  (search-forward point string :test (case-relevant-test string))
                  (/= (offset point) offset-before))
	  do (backward-object point length)
	     (replace-one-string point length newstring use-region-case)
	  finally (display-message "Replaced ~A occurrence~:P" occurrences))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Query replace

(make-command-table 'query-replace-drei-table :errorp nil)

(defun query-replace-find-next-match (state)
  (with-accessors ((string string1)
                   (buffers buffers)
                   (mark mark)) state
    (let ((offset-before (offset mark)))
      (search-forward mark string :test (case-relevant-test string))
      (/= (offset mark) offset-before))))

(define-command (com-query-replace :name t :command-table search-table) ()
  (let* ((pane *current-window*)
         (old-state (query-replace-state pane))
         (old-string1 (when old-state (string1 old-state)))
         (old-string2 (when old-state (string2 old-state)))
         (string1 (handler-case 
                      (if old-string1
                          (accept 'string
                                  :prompt "Query Replace"
                                  :default old-string1
                                  :default-type 'string)
                          (accept 'string :prompt "Query Replace"))
                    (error () (progn (beep)
                                     (display-message "Empty string")
                                     (return-from com-query-replace nil)))))
         (string2 (handler-case 
                      (if old-string2
                          (accept 'string
                                  :prompt (format nil "Replace ~A with"
                                                  string1)
                                  :default old-string2
                                  :default-type 'string)
                          (accept 'string
                                  :prompt (format nil "Replace ~A with" string1)))
                    (error () (progn (beep)
                                     (display-message "Empty string")
                                     (return-from com-query-replace nil))))))
    (setf (query-replace-state pane) (make-instance 'query-replace-state
                                                    :string1 string1
                                                    :string2 string2
                                                    :mark *current-point*))
    (when (query-replace-find-next-match (query-replace-state pane))
      (setf (query-replace-mode pane) t)
      (display-message "Replace ~A with ~A:"
                       string1 string2)
      (simple-command-loop 'query-replace-drei-table
                           (query-replace-mode pane)
                           ((setf (query-replace-mode pane) nil)
                            (display-message "Replaced ~A occurence~:P"
                                             (occurrences (query-replace-state pane))))
                           ((setf (query-replace-mode pane) nil)
                            (signal 'abort-gesture :event *current-gesture*))))))

(set-key 'com-query-replace
	 'search-table
	 '((#\% :shift :meta)))

(define-command (com-query-replace-replace :name t :command-table query-replace-drei-table) ()
  (let* ((pane *current-window*)
         (state (query-replace-state pane)))
    (with-accessors ((string1 string1)
                     (string2 string2)
                     (occurrences occurrences)) state
      (let ((string1-length (length string1)))
        (backward-object (mark state) string1-length)
        (replace-one-string (mark state)
                            string1-length
                            string2
                            (no-upper-p string1))
        (incf occurrences)
        (if (query-replace-find-next-match (query-replace-state pane))
            (display-message "Replace ~A with ~A:"
                             string1 string2)
            (setf (query-replace-mode pane) nil))))))

(define-command (com-query-replace-replace-and-quit
		 :name t
		 :command-table query-replace-drei-table)
    ()
  (let* ((pane *current-window*)
         (state (query-replace-state pane)))
    (with-accessors ((string1 string1)
                     (string2 string2)
                     (occurrences occurrences)) state
      (let ((string1-length (length string1)))
        (backward-object (mark state) string1-length)
        (replace-one-string (mark state)
                            string1-length
                            string2
                            (no-upper-p string1))
        (incf occurrences)
        (setf (query-replace-mode pane) nil)))))

(define-command (com-query-replace-replace-all
		 :name t
		 :command-table query-replace-drei-table)
    ()
  (let* ((pane *current-window*)
         (state (query-replace-state pane)))
    (with-accessors ((string1 string1)
                     (string2 string2)
                     (occurrences occurrences)) state
        (let ((string1-length (length string1)))
          (loop do (backward-object (mark state) string1-length)
               (replace-one-string (mark state)
                                   string1-length
                                   string2
                                   (no-upper-p string1))
               (incf occurrences)
               while (query-replace-find-next-match (query-replace-state pane))
               finally (setf (query-replace-mode pane) nil))))))

(define-command (com-query-replace-skip :name t :command-table query-replace-drei-table) ()
  (let* ((pane *current-window*)
         (state (query-replace-state pane)))
    (with-accessors ((string1 string1)
                     (string2 string2)) state
      (if (query-replace-find-next-match state)
          (display-message "Replace ~A with ~A:"
                           string1 string2)
          (setf (query-replace-mode pane) nil)))))

(define-command (com-query-replace-exit :name t :command-table query-replace-drei-table) ()
  (setf (query-replace-mode *current-window*) nil))

(defun query-replace-set-key (gesture command)
  (add-command-to-command-table command 'query-replace-drei-table
                                :keystroke gesture :errorp nil))

(query-replace-set-key '(#\Newline) 'com-query-replace-exit)
(query-replace-set-key '(#\Space) 'com-query-replace-replace)
(query-replace-set-key '(#\Backspace) 'com-query-replace-skip)
(query-replace-set-key '(#\Rubout) 'com-query-replace-skip)
(query-replace-set-key '(#\q) 'com-query-replace-exit)
(query-replace-set-key '(#\y) 'com-query-replace-replace)
(query-replace-set-key '(#\n) 'com-query-replace-skip)
(query-replace-set-key '(#\.) 'com-query-replace-replace-and-quit)
(query-replace-set-key '(#\!) 'com-query-replace-replace-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Regex search

(defparameter *whitespace-regex* (format nil "[~@{~A~}]+" #\Space #\Tab))

(defun normalise-minibuffer-regex (string)
  "Massages the regex STRING given to the minibuffer."
  (with-output-to-string (result)
    (loop for char across string
	  if (char= char #\Space)
	    do (princ *whitespace-regex* result)
	  else
	    do (princ char result))))

(define-command (com-regex-search-forward :name t :command-table search-table) ()
  (let ((string (accept 'string :prompt "RE search"
			:delimiter-gestures nil
			:activation-gestures
			'(:newline :return))))
    (re-search-forward *current-point* (normalise-minibuffer-regex string))))

(define-command (com-regex-search-backward :name t :command-table search-table) ()
  (let ((string (accept 'string :prompt "RE search backward"
			:delimiter-gestures nil
			:activation-gestures
			'(:newline :return))))
    (re-search-backward *current-point* (normalise-minibuffer-regex string))))

(define-command (com-how-many :name t :command-table search-table)
    ((regex 'string :prompt "How many matches for"))
  (let* ((re (normalise-minibuffer-regex regex))
	 (mark (clone-mark *current-point*))
	 (occurrences (loop for count from 0
			    while (re-search-forward mark re)
			    finally (return count))))
    (display-message "~A occurrence~:P" occurrences)))
