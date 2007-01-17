;;; -*- Mode: Lisp; Package: DREI-COMMANDS -*-

;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2004-2005 by
;;;           Elliott Johnson (ejohnson@fasl.info)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;  (c) copyright 2006 by
;;;           Taylor R. Campbell (campbell@mumble.net)
;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)

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

;;; Definitions of basic commands that are necessary for DREI to be
;;; functional at all.

(in-package :drei-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Motion commands.
;;; See information in motion.lisp
;;;
;;; Given the general motion functions FORWARD-<unit> and
;;; BACKWARD-<unit>,
;;;
;;;   (DEFINE-MOTION-COMMANDS <unit> <command-table>)
;;;
;;; defines the motion commands Forward <unit> and Backward <unit> in
;;; <command-table>.  The following keyword parameters are recognized:
;;;
;;;   :NOUN
;;;     Noun to use in the docstring:  `Move point forward by one
;;;     <noun>.'  Default is the unit name, downcased.
;;;
;;;   :PLURAL
;;;     Plural form for the prompt, `Number of <plural>', and the rest
;;;     of the docstring; e.g.:  `With a numeric argument N, move point
;;;     forward by N <plural>.'

(defmacro define-motion-commands (unit command-table &key
                                  noun
                                  plural)
  (labels ((concat (&rest strings)
             (apply #'concatenate 'STRING (mapcar #'string strings)))
           (symbol (&rest strings)
             (intern (apply #'concat strings))))
    (let ((forward (symbol "FORWARD-" unit))
          (backward (symbol "BACKWARD-" unit))
          (com-forward (symbol "COM-FORWARD-" unit))
          (com-backward (symbol "COM-BACKWARD-" unit))
          (noun (or noun (string-downcase unit)))
          (plural (or plural (concat (string-downcase unit) "s"))))
      `(PROGN
         (DEFINE-COMMAND (,com-forward :NAME T
                                       :COMMAND-TABLE ,command-table)
             ((COUNT 'INTEGER :PROMPT ,(concat "Number of " plural)))
           ,(concat "Move point forward by one " noun ".
With a numeric argument N, move point forward by N " plural ".
With a negative argument -N, move point backward by N " plural ".")
           (handler-case (,forward *current-point*
                                   (SYNTAX *current-buffer*)
                                   COUNT)
             (motion-limit-error ()
               (beep)
               (display-message ,(concat "No more " plural)))))
         (DEFINE-COMMAND (,com-backward
                          :NAME T
                          :COMMAND-TABLE ,command-table)
             ((COUNT 'INTEGER :PROMPT ,(concat "Number of " plural)))
           ,(concat "Move point backward by one " noun ".
With a numeric argument N, move point backward by N " plural ".
With a negative argument -N, move point forward by N " plural ".")
           (handler-case (,backward *current-point*
                                    (SYNTAX *current-buffer*)
                                    COUNT)
             (motion-limit-error ()
               (beep)
               (display-message ,(concat "No more " plural)))))))))

;;; Manually define some commands

(define-command (com-beginning-of-line :name t :command-table movement-table) ()
  "Move point to the beginning of the current line."
  (beginning-of-line *current-point*))

(define-command (com-end-of-line :name t :command-table movement-table) ()
  "Move point to the end of the current line."
  (end-of-line *current-point*))

;; Object movement comands - defined specially because FORWARD-OBJECT
;; and BACKWARD-OBJECT is part of the buffer protocol, not the
;; high-level motion abstraction.
(define-command (com-forward-object :name t :command-table movement-table)
    ((count 'integer :prompt "Number of objects"))
  "Move point forward by one object.
With a numeric argument N, move point forward by N objects.
With a negative argument -N, move point backward by M objects."
  (handler-case
      (forward-object *current-point*
                      count)
    (motion-limit-error nil
      (beep)
      (display-message "No more objects"))))

(define-command (com-backward-object :name t :command-table movement-table)
    ((count 'integer :prompt "number of objects"))
  "Move point backward by one object.
With a numeric argument N, move point backward by N objects.
With a negative argument -N, move point forward by N objects."
  (handler-case
      (backward-object *current-point*
                       count)
    (motion-limit-error nil
      (beep)
      (display-message "No more objects"))))

;;; Autogenerate commands
(define-motion-commands word movement-table)
(define-motion-commands line movement-table)
(define-motion-commands page movement-table)
(define-motion-commands paragraph movement-table)
(define-motion-commands sentence movement-table)

;;; Bind gestures to commands
(set-key `(com-forward-object ,*numeric-argument-marker*)
	 'movement-table
	 '((#\f :control)))

(set-key `(com-forward-object ,*numeric-argument-marker*)
	 'movement-table
	 '((#+(or mcclim building-mcclim) :right #-(or mcclim building-mcclim) :right-arrow)))

(set-key `(com-backward-object ,*numeric-argument-marker*)
	 'movement-table
	 '((#\b :control)))

(set-key `(com-backward-object ,*numeric-argument-marker*)
	 'movement-table
	 '((#+(or mcclim building-mcclim) :left #-(or mcclim building-mcclim) :left-arrow)))

(set-key `(com-forward-word ,*numeric-argument-marker*)
	 'movement-table
	 '((#\f :meta)))

(set-key `(com-forward-word ,*numeric-argument-marker*)
	 'movement-table
	 '((#+(or mcclim building-mcclim) :right #-(or mcclim building-mcclim) :right-arrow :control)))

(set-key `(com-backward-word ,*numeric-argument-marker*)
	 'movement-table
	 '((#\b :meta)))

(set-key `(com-backward-word ,*numeric-argument-marker*)
	 'movement-table
	 '((#+(or mcclim building-mcclim) :left #-(or mcclim building-mcclim) :left-arrow :control)))

(set-key `(com-forward-line ,*numeric-argument-marker*)
	 'movement-table
	 '((#\n :control)))

(set-key `(com-forward-line ,*numeric-argument-marker*)
	 'movement-table
	 '((#+(or mcclim building-mcclim) :down #-(or mcclim building-mcclim) :down-arrow)))

(set-key `(com-backward-line ,*numeric-argument-marker*)
	 'movement-table
	 '((#\p :control)))

(set-key `(com-backward-line ,*numeric-argument-marker*)
	 'movement-table
	 '((#+(or mcclim building-mcclim) :up #-(or mcclim building-mcclim) :up-arrow)))

(set-key 'com-beginning-of-line
	 'movement-table
	 '((:home)))

(set-key 'com-beginning-of-line
	 'movement-table
	 '((#\a :control)))

(set-key 'com-end-of-line
	 'movement-table
	 '((#\e :control)))

(set-key 'com-end-of-line
	 'movement-table
	 '((:end)))

(set-key `(com-forward-page ,*numeric-argument-marker*)
	 'movement-table
	 '((#\x :control) (#\])))

(set-key `(com-backward-page ,*numeric-argument-marker*)
	 'movement-table
	 '((#\x :control) (#\[)))

(set-key `(com-backward-paragraph ,*numeric-argument-marker*)
	 'movement-table
	 '((#\{ :shift :meta)))

(set-key `(com-backward-paragraph ,*numeric-argument-marker*)
	 'movement-table
	 '((#+(or mcclim building-mcclim) :up #-(or mcclim building-mcclim) :up-arrow :control)))

(set-key `(com-forward-paragraph ,*numeric-argument-marker*)
	 'movement-table
	 '((#\} :shift :meta)))

(set-key `(com-forward-paragraph ,*numeric-argument-marker*)
	 'movement-table
	 '((#+(or mcclim building-mcclim) :down #-(or mcclim building-mcclim) :down-arrow :control)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Editing commands.
;;;
;;; Commands for deleting, killing and moving stuff See information in
;;; motion.lisp
;;;
;;; A deletion command is a command named Kill <Unit>, Backward Kill
;;; <Unit>, Delete <Unit> or Backward Delete <Unit>. corresponding to
;;; the editing functions FORWARD-KILL-<unit>, BACKWARD-KILL-<unit>,
;;; FORWARD-DELETE-<unit> and BACKWARD-DELETE-<unit> respectively
;;; (note that the "forward" prefix is gone in the command name).
;;;
;;; An editing command is a command named Transpose <Unit>s.
;;;
;;; This file also holds command definitions for other functions
;;; defined in the DREI-EDITING package.

(defmacro define-deletion-commands (unit command-table &key
                                    noun
                                    plural)
  (labels ((concat (&rest strings)
             (apply #'concatenate 'STRING (mapcar #'string strings)))
           (symbol (&rest strings)
             (intern (apply #'concat strings)))
           (try-to-find (&rest strings)
             (find-symbol (apply #'concat
                                 (mapcar #'string-upcase
                                         (mapcar #'string strings))))))
    (let ((forward-kill (try-to-find "FORWARD-KILL-" unit))
          (backward-kill (try-to-find "BACKWARD-KILL-" unit))
          (forward-delete (try-to-find "FORWARD-DELETE-" unit))
          (backward-delete (try-to-find "BACKWARD-DELETE-" unit))
          (com-kill (symbol "COM-KILL-" unit))
          (com-backward-kill (symbol "COM-BACKWARD-KILL-" unit))
          (com-delete (symbol "COM-DELETE-" unit))
          (com-backward-delete (symbol "COM-BACKWARD-DELETE-" unit))
          (noun (or noun (string-downcase unit))))
      (unless (and forward-kill backward-kill forward-delete backward-delete)
        (error "The unit ~A is not known." unit))
      (let ((plural (or plural (concat (string-downcase unit) "s"))))
        `(progn

           ;; Kill Unit
           (define-command (,com-kill :name t
                                      :command-table ,command-table)
               ((count 'integer :prompt ,(concat "Number of " plural)))
             ,(concat "Kill " plural " up to the next " noun " end.
With a numeric argument, kill forward (backward if negative) 
that many " plural ".

Successive kills append to the kill ring.")
             (handler-case (,forward-kill *current-point*
                                          (syntax *current-buffer*)
                                          count
                                          (eq (command-name *previous-command*) ',com-kill))
               (motion-limit-error ()
                 (beep)
                 (display-message ,(concat "No more " plural " to kill")))))

           ;; Backward Kill Unit
           (define-command (,com-backward-kill
                            :name t
                            :command-table ,command-table)
               ((count 'integer :prompt ,(concat "Number of " plural)))
             ,(concat "Kill from point until the previous " noun " beginning.
With a numeric argument, kill backward (forward, if negative) 
that many " plural ".

Successive kills append to the kill ring.")
             (handler-case (,backward-kill *current-point*
                                           (syntax *current-buffer*)
                                           count
                                           (eq (command-name *previous-command*) ',com-backward-kill))
               (motion-limit-error ()
                 (beep)
                 (display-message ,(concat "No more " plural "to kill")))))

           ;; Delete Unit
           (define-command (,com-delete :name t :command-table ,command-table)
               ((count 'integer :prompt ,(concat "Number of " plural)))
             ,(concat "Delete from point until the next " noun " end.
With a positive numeric argument, delete that many " plural " forward.")
             (,backward-delete *current-point* (syntax *current-buffer*) count))

           ;; Backward Delete Unit
           (define-command (,com-backward-delete :name t :command-table ,command-table)
               ((count 'integer :prompt ,(concat "Number of " plural)))
             ,(concat "Delete from point until the previous " noun " beginning.
With a positive numeric argument, delete that many " plural " backward.")
             (,backward-delete *current-point* (syntax *current-buffer*) count)))))))

(defmacro define-editing-commands (unit command-table &key
                                   noun
                                   plural)
  (labels ((concat (&rest strings)
             (apply #'concatenate 'STRING (mapcar #'string strings)))
           (symbol (&rest strings)
             (intern (apply #'concat strings)))
           (try-to-find (&rest strings)
             (find-symbol (apply #'concat
                                 (mapcar #'string-upcase
                                         (mapcar #'string strings))))))
    (let* ((plural (or plural (concat (string-downcase unit) "s")))
           (upcase-plural (string-upcase plural))
           (noun (or noun (string-downcase unit)))
           (transpose (try-to-find "TRANSPOSE-" upcase-plural))
           (com-transpose (symbol "COM-TRANSPOSE-" upcase-plural)))
      (unless (and transpose)
        (error "The unit ~A is not known." unit))
      `(progn
         ;; Transpose Units
         (define-command (,com-transpose :name t :command-table ,command-table)
             ()
           ,(concat "Transpose the " plural " around point,
leaving point at the end of them.  With point in the
whitespace between words, transpose the " plural "
before and after point. With point inside a " noun ",
transpose that " noun " with the next one. With point
before the first " noun " of the buffer, transpose the
first two " plural " of the buffer.")
           (handler-case (,transpose *current-point*
                                     (syntax *current-buffer*))
             (motion-limit-error ()
               (beep)
               (display-message ,(concat "No more " plural " to transpose")))))))))

;;; Some manually defined commands

(define-command (com-transpose-objects :name t :command-table editing-table) ()
  "Transpose the objects before and after point, advancing point.
At the end of a line transpose the previous two objects without
advancing point. At the beginning of the buffer do nothing.  At
the beginning of any line other than the first effectively move
the first object of that line to the end of the previous line."
  (transpose-objects *current-point*))

(define-command (com-delete-object :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of Objects")
     (killp 'boolean :prompt "Kill?"))
  "Delete the object after point.
With a numeric argument, kill that many objects 
after (or before, if negative) point."
   (if killp
      (forward-kill-object *current-point* count)
      (forward-delete-object *current-point* count)))

(define-command (com-backward-delete-object :name t :command-table deletion-table)
    ((count 'integer :prompt "Number of Objects")
     (killp 'boolean :prompt "Kill?"))
  "Delete the object before point.
With a numeric argument, kills that many objects 
before (or after, if negative) point."
  (if killp
      (backward-kill-object *current-point* count)
      (backward-delete-object *current-point* count)))

;; We require somewhat special behavior from Kill Line, so define a
;; new function and use that to implement the Kill Line command.
(defun user-kill-line (mark &optional (count 1) (whole-lines-p nil) (concatenate-p nil))
  (let ((start (offset mark)))
    (cond ((= 0 count)
	   (beginning-of-line mark))
	  ((< count 0)
	   (loop repeat (- count)
              until (beginning-of-buffer-p mark)
              do (beginning-of-line mark)
              until (beginning-of-buffer-p mark)
              do (backward-object mark)))
	  ((or whole-lines-p (> count 1))
	   (loop repeat count
              until (end-of-buffer-p mark)
              do (end-of-line mark)
              until (end-of-buffer-p mark)
              do (forward-object mark)))
	  (t
	   (cond ((end-of-buffer-p mark) nil)
		 ((end-of-line-p mark) (forward-object mark))
		 (t (end-of-line mark)))))
    (unless (mark= mark start)
      (if concatenate-p
	  (kill-ring-concatenating-push *kill-ring*
					(region-to-sequence start mark))
	  (kill-ring-standard-push *kill-ring*
				   (region-to-sequence start mark)))
      (delete-region start mark))))

(define-command (com-kill-line :name t :command-table deletion-table)
    ((numarg 'integer :prompt "Kill how many lines?")
     (numargp 'boolean :prompt "Kill entire lines?"))
  "Kill the objects on the current line after point.
When at the end of a line, kill the #\\Newline. 
With a numeric argument of 0, kill the objects on the current line before point.
With a non-zero numeric argument, kill that many lines forward (backward, 
if negative) from point.

Successive kills append to the kill ring."
  (let* ((concatenate-p (eq (command-name *previous-command*) 'com-kill-line)))
    (user-kill-line *current-point* numarg numargp concatenate-p)))

;;; Autogenerate commands

(define-deletion-commands word deletion-table)
(define-editing-commands word editing-table)
(define-editing-commands line editing-table)
(define-deletion-commands definition deletion-table)
(define-editing-commands definition editing-table)
(define-deletion-commands paragraph deletion-table)
(define-editing-commands paragraph editing-table)

;;; Bind gestures to commands

(set-key `(com-kill-word ,*numeric-argument-marker*)
	 'deletion-table
	 '((#\d :meta)))

(set-key `(com-backward-kill-word ,*numeric-argument-marker*)
	 'deletion-table
	 '((#\Backspace :meta)))

(set-key 'com-transpose-words
	 'editing-table
	 '((#\t :meta)))

(set-key 'com-transpose-lines
	 'editing-table
	 '((#\x :control) (#\t :control)))

(set-key `(com-delete-object ,*numeric-argument-marker*
			     ,*numeric-argument-p*)
	 'deletion-table
	 '(#\Rubout))

(set-key `(com-delete-object ,*numeric-argument-marker*
			     ,*numeric-argument-p*)
	 'deletion-table
	 '((#\d :control)))

(set-key `(com-backward-delete-object ,*numeric-argument-marker*
				      ,*numeric-argument-p*)
	 'deletion-table
	 '(#\Backspace))

(set-key 'com-transpose-objects
	 'editing-table
	 '((#\t :control)))

(set-key `(com-kill-line ,*numeric-argument-marker* ,*numeric-argument-p*)
	 'deletion-table
	 '((#\k :control)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Self-insertion-commands.
;;;
;;; These are what do the basic keypress->character inserted in buffer
;;; mapping.

(define-command com-self-insert ((count 'integer))
  (loop repeat count do (insert-character *current-gesture*)))

(loop for code from (char-code #\Space) to (char-code #\~)
      do (set-key `(com-self-insert ,*numeric-argument-marker*)
	     'self-insert-table
	     (list (list (code-char code)))))

(set-key `(com-self-insert ,*numeric-argument-marker*)
	 'self-insert-table
	 '((#\Newline)))