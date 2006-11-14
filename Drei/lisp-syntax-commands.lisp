;;; -*- Mode: Lisp; Package: DREI-LISP-SYNTAX; -*-

;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2004-2005 by
;;;           Elliott Johnson (ejohnson@fasl.info)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)
;;;
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

;;; Commands specific to the Lisp syntax for Drei.

(in-package :drei-lisp-syntax)

;;; This command table is used when Drei runs as a pane.
(make-command-table 'pane-lisp-table
                    :errorp nil)

(defmethod additional-command-tables append ((drei drei-pane) (command-table lisp-table))
  '(pane-lisp-table))

;; Movement commands.
(drei-commands:define-motion-commands expression lisp-table)
(drei-commands:define-motion-commands definition lisp-table)
(drei-commands:define-motion-commands up lisp-table
  :noun "nesting level up"
  :plural "levels")
(drei-commands:define-motion-commands down lisp-table
  :noun "nesting level down"
  :plural "levels")
(drei-commands:define-motion-commands list lisp-table)

(drei-commands:define-editing-commands expression lisp-table)
(drei-commands:define-deletion-commands expression lisp-table)

(define-command (com-fill-paragraph :name t :command-table lisp-table) 
    ()
  "Fill paragraph at point. Will have no effect unless there is a
string at point."
  (let* ((pane *current-window*)
         (buffer (buffer pane))
         (implementation (implementation buffer))
         (syntax (syntax buffer))
         (token (form-around syntax (offset (point pane))))
         (fill-column (auto-fill-column pane))
         (tab-width (tab-space-count (stream-default-view pane))))
    (when (form-string-p token)
      (with-accessors ((offset1 start-offset) 
                       (offset2 end-offset)) token
        (fill-region (make-instance 'standard-right-sticky-mark
                                    :buffer implementation
                                    :offset offset1)
                     (make-instance 'standard-right-sticky-mark
                                    :buffer implementation
                                    :offset offset2)
                     #'(lambda (mark)
                         (syntax-line-indentation mark tab-width syntax))
                     fill-column
                     tab-width
                     syntax
                     t)))))

(define-command (com-indent-expression :name t :command-table lisp-table)
    ((count 'integer :prompt "Number of expressions"))
  (let* ((pane *current-window*)
         (point (point pane))
         (mark (clone-mark point))
         (syntax (syntax (buffer pane))))
    (if (plusp count)
        (loop repeat count do (forward-expression mark syntax))
        (loop repeat (- count) do (backward-expression mark syntax)))
    (indent-region pane (clone-mark point) mark)))

(define-command (com-lookup-arglist-for-this-symbol :command-table lisp-table)
    ()
  "Show argument list for symbol at point."
  (let* ((pane *current-window*)
         (buffer (buffer pane))
         (syntax (syntax buffer))
         (mark (point pane))
         (token (this-form mark syntax)))
    (if (and token (form-token-p token))
        (com-lookup-arglist (token-to-object syntax token))
        (display-message "Could not find symbol at point."))))

(define-command (com-lookup-arglist :name t :command-table lisp-table)
    ((symbol 'symbol :prompt "Symbol"))
  "Show argument list for a given symbol."
  (show-arglist symbol))

(define-command (com-space :command-table lisp-table)
    ()
  "Insert a space and display argument hints in the minibuffer."
  (let* ((window *current-window*)
         (mark (point window))
         (syntax (syntax (buffer window))))
    ;; It is important that the space is inserted before we look up
    ;; any symbols, but at the same time, there must not be a space
    ;; between the mark and the symbol.
    (insert-character #\Space)
    (backward-object mark)
    ;; We must update the syntax in order to reflect any changes to
    ;; the parse tree our insertion of a space character may have
    ;; done.
    (update-syntax (buffer syntax) syntax)
    (show-arglist-for-form-at-mark mark syntax)
    (forward-object mark)
    (clear-completions)))

(define-command (com-complete-symbol :name t :command-table lisp-table) ()
  "Attempt to complete the symbol at mark. If successful, move point
to end of symbol.  

If more than one completion is available, a list of
possible completions will be displayed."
  (let* ((pane *current-window*)
         (buffer (buffer pane))
         (syntax (syntax buffer))
         (mark (point pane)))
    (complete-symbol-at-mark syntax mark)))

(define-command (com-fuzzily-complete-symbol :name t :command-table lisp-table) ()
  "Attempt to fuzzily complete the abbreviation at mark.

Fuzzy completion tries to guess which symbol is abbreviated. If
the abbreviation is ambiguous, a list of possible completions
will be displayed."
  (let* ((pane *current-window*)
         (buffer (buffer pane))
         (syntax (syntax buffer))
         (mark (point pane)))
    (fuzzily-complete-symbol-at-mark syntax mark)))

(define-command (com-indent-line-and-complete-symbol :name t :command-table lisp-table) ()
  "Indents the current line and performs symbol completion.
First indents the line.  If the line was already indented,
completes the symbol.  If there's no symbol at the point, shows
the arglist for the most recently enclosed operator."
  (let* ((pane *current-window*)
         (point (point pane))
         (old-offset (offset point)))
    (indent-current-line pane point)
    (when (= old-offset
             (offset point))
      (let* ((buffer (buffer pane))
             (syntax (syntax buffer)))
        (or (complete-symbol-at-mark syntax point)
            (show-arglist-for-form-at-mark point syntax))))))

(define-presentation-to-command-translator lookup-symbol-arglist
    (symbol com-lookup-arglist lisp-table
            :gesture :describe
            :tester ((object presentation)
                     (declare (ignore object))
                     (not (eq (presentation-type presentation) 'unknown-symbol)))
            :documentation "Lookup arglist")
    (object)
  (list object))

(define-command (com-eval-region :name t :command-table pane-lisp-table)
    ()
  "Evaluate the current region."
  (let ((mark *current-mark*)
        (point *current-point*))
    (when (mark> mark point)
      (rotatef mark point))
    (eval-region mark point *current-syntax*)))

(define-command (com-eval-last-expression :name t :command-table pane-lisp-table)
    ((insertp 'boolean :prompt "Insert?"))
  "Evaluate the expression before point in the local Lisp image."
  (let ((token (form-before *current-syntax* (offset *current-point*))))
    (if token
        (with-syntax-package (*current-syntax* *current-point*)
          (let ((*read-base* (base *current-syntax*)))
            (drei-commands::com-eval-expression
             (token-to-object *current-syntax* token :read t)
             insertp)))
        (display-message "Nothing to evaluate."))))

(define-command (com-eval-defun :name t :command-table pane-lisp-table) ()
  (eval-defun *current-point* *current-syntax*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Gesture bindings

(set-key 'com-fill-paragraph
         'lisp-table
         '((#\q :meta)))

(set-key `(com-indent-expression ,*numeric-argument-marker*)
         'lisp-table
         '((#\q :meta :control)))

(set-key `(com-backward-up ,*numeric-argument-marker*)
         'lisp-table
         '((#\u :control :meta)))

(set-key `(com-forward-down ,*numeric-argument-marker*)
         'lisp-table
         '((#\d :control :meta)))

(set-key `(com-backward-expression ,*numeric-argument-marker*)
         'lisp-table
         '((#\b :control :meta)))

(set-key `(com-forward-expression ,*numeric-argument-marker*)
         'lisp-table
         '((#\f :control :meta)))

(set-key `(com-backward-definition ,*numeric-argument-marker*)
         'lisp-table
         '((#\a :control :meta)))

(set-key `(com-forward-definition ,*numeric-argument-marker*)
         'lisp-table
         '((#\e :control :meta)))

(set-key `(com-forward-list ,*numeric-argument-marker*)
         'lisp-table
         '((#\n :control :meta)))

(set-key `(com-backward-list ,*numeric-argument-marker*)
         'lisp-table
         '((#\p :control :meta)))

(set-key `(com-kill-expression ,*numeric-argument-marker*)
         'lisp-table
         '((#\k :control :meta)))

(set-key 'com-lookup-arglist-for-this-symbol
         'lisp-table
         '((#\c :control) (#\d :control) (#\a)))

(set-key 'com-space
         'lisp-table
         '((#\Space)))

(set-key 'com-complete-symbol
         'lisp-table
         '((#\Tab :meta)))

(set-key 'com-fuzzily-complete-symbol
         'lisp-table
         '((#\c :control) (#\i :meta)))

(set-key 'com-indent-line-and-complete-symbol
         'lisp-table
         '((#\Tab)))

(set-key 'drei-commands::com-newline-and-indent
         'lisp-table
         '(#\Newline))

(set-key 'com-eval-region
         'pane-lisp-table
         '((#\c :control) (#\r :control)))

(set-key `(com-eval-last-expression ,*numeric-argument-p*)
         'pane-lisp-table
         '((#\c :control) (#\e :control)))
