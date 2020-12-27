;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2004-2005 Robert Strandh <strandh@labri.fr>
;;;  (c) copyright 2004-2005 Elliott Johnson <ejohnson@fasl.info>
;;;  (c) copyright 2005 Matthieu Villeneuve <matthieu.villeneuve@free.fr.
;;;  (c) copyright 2005 Aleksandar Bakic <a_bakic@yahoo.com>
;;;  (c) copyright 2006-2008 Troels Henriksen <athas@sigkill.dk>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Miscellaneous commands for Drei.

(in-package #:drei-commands)

(define-command (com-eval-expression :name t :command-table editor-table)
    ((exp 'expression :prompt "Eval")
     (insertp 'boolean :prompt "Insert?" :default nil))
  "Prompt for and evaluate a lisp expression.
With a numeric argument inserts the result at point as a string;
otherwise prints the result."
  (let* ((*package* (find-package :climacs-gui))
         (values (multiple-value-list
                  (handler-case (eval exp)
                    (error (condition) (progn (beep)
                                              (display-message "~a" condition)
                                              (return-from com-eval-expression nil))))))
         (result (format nil "~:[; No values~;~:*~{~S~^,~}~]" values)))
    (if insertp
        (insert-sequence (point) result)
        (display-message result))))

(define-command (com-count-lines-page :name t :command-table info-table) ()
  "Print the number of lines in the current page.
Also prints the number of lines before and after point (as '(b + a)')."
  (let* ((start (clone-mark (point)))
         (end (clone-mark (point))))
    (backward-page start (current-syntax) 1 nil)
    (forward-page end (current-syntax) 1 nil)
    (let ((total (number-of-lines-in-region start end))
          (before (number-of-lines-in-region start (point)))
          (after (number-of-lines-in-region (point) end)))
      (display-message "Page has ~A lines (~A + ~A)" (1+ total) before after))))

(define-command (com-count-lines-region :name t :command-table info-table) ()
  "Print the number of lines in the region.
Also prints the number of objects (as 'o character[s]')."
  (let*  ((lines (number-of-lines-in-region (point) (mark)))
          (chars (abs (- (offset (point)) (offset (mark))))))
    (display-message "Region has ~D line~:P, ~D character~:P." (1+ lines) chars)))

(set-key `(com-eval-expression ,*unsupplied-argument-marker* ,*numeric-argument-marker*)
         'editor-table
         '((#\: :meta)))

(set-key 'com-count-lines-page
         'info-table
         '((#\x :control) (#\l)))

(set-key 'com-count-lines-region
         'info-table
         '((#\= :meta)))
