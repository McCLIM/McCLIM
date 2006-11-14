;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
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

;;; Package definitions for the DREI editing component.

(in-package :cl-user)

(defpackage :drei-buffer
  (:use :clim-lisp :flexichain :binseq)
  (:export #:buffer #:standard-buffer
           #:mark #:left-sticky-mark #:right-sticky-mark
           #:standard-left-sticky-mark #:standard-right-sticky-mark
           #:clone-mark
           #:no-such-offset #:offset-before-beginning #:offset-after-end
           #:invalid-motion #:motion-before-beginning #:motion-after-end
           #:size #:number-of-lines
           #:offset #:mark< #:mark<= #:mark= #:mark> #:mark>=
           #:forward-object
           #:backward-object
           #:forward-line-start #:backward-line-start
           #:forward-line-end #:backward-line-end
           #:beginning-of-buffer #:end-of-buffer
           #:beginning-of-buffer-p #:end-of-buffer-p
           #:beginning-of-line #:end-of-line
           #:beginning-of-line-p #:end-of-line-p
           #:buffer-line-number #:buffer-column-number
           #:line-number #:column-number
           #:insert-buffer-object #:insert-buffer-sequence
           #:buffer-substring
           #:insert-object #:insert-sequence
           #:delete-buffer-range #:delete-range
           #:delete-region
           #:buffer-object #:buffer-sequence
           #:object-before #:object-after #:region-to-sequence
           #:low-mark #:high-mark #:modified-p #:clear-modify
           #:binseq-buffer #:obinseq-buffer #:binseq2-buffer
           #:persistent-left-sticky-mark #:persistent-right-sticky-mark
           #:persistent-left-sticky-line-mark #:persistent-right-sticky-line-mark
           #:p-line-mark-mixin #:buffer-line-offset
           #:delegating-buffer #:implementation)
  (:documentation "An implementation of the buffer protocol. This
package is quite low-level, not syntax-aware, not CLIM-aware and
not user-oriented at all."))

(defpackage :drei-undo
  (:use :clim-lisp :drei-buffer :flexichain)
  (:export #:no-more-undo
           #:undo-tree #:standard-undo-tree
           #:undo-record #:standard-undo-record
           #:add-undo #:flip-undo-record #:undo #:redo))

(defpackage :drei-kill-ring
  (:use :clim-lisp :flexichain)
  (:export #:kill-ring
           #:empty-kill-ring
           #:kill-ring-length #:kill-ring-max-size
           #:append-next-p
           #:reset-yank-position #:rotate-yank-position #:kill-ring-yank
           #:kill-ring-standard-push #:kill-ring-concatenating-push
           #:kill-ring-reverse-concatenating-push
           #:*kill-ring*)
  (:documentation "An implementation of a kill ring."))

(defpackage :drei-base
  (:use :clim-lisp :drei-buffer :drei-kill-ring :esa-buffer :esa-utils)
  (:export #:as-region
           #:as-full-region
           #:as-offsets
           #:do-buffer-region
           #:do-buffer-region-lines
           #:previous-line #:next-line
           #:open-line
           #:delete-line
           #:extract-line
           #:lines-in-region
           #:extract-lines-in-region
           #:empty-line-p
           #:line-indentation
           #:buffer-display-column
           #:number-of-lines-in-region
           #:constituentp
           #:just-n-spaces
           #:move-to-column
           #:buffer-whitespacep
           #:buffer-region-case
           #:name-mixin #:name
           #:buffer-looking-at #:looking-at
           #:buffer-search-forward #:buffer-search-backward
           #:buffer-re-search-forward #:buffer-re-search-backward
           #:search-forward #:search-backward
           #:re-search-forward #:re-search-backward
           #:buffer-search-word-forward #:search-word-forward
           #:buffer-search-word-backward #:search-word-backward
           #:downcase-buffer-region #:downcase-region
           #:upcase-buffer-region #:upcase-region
           #:capitalize-buffer-region #:capitalize-region
           #:tabify-region #:untabify-region
           #:narrowed-mark-mixin #:narrowed-left-sticky-mark #:narrowed-right-sticky-mark
           #:make-narrowed-mark #:make-backward-narrowed-mark #:make-forward-narrowed-mark
           #:narrow-mark #:unnarrow-mark)
  (:documentation "Basic functionality built on top of the buffer
protocol. Here is where we define slightly higher level
functions, that can be directly implemented in terms of the
buffer protocol, but that are not, strictly speaking, part of
that protocol. The functions in this package are not
syntax-aware, and are thus limited in what they can do. They
percieve the buffer as little more than a sequence of
characters."))

(defpackage :drei-abbrev
  (:use :clim-lisp :clim :drei-buffer :drei-base)
  (:export #:abbrev-expander #:dictionary-abbrev-expander #:dictionary
           #:expand-abbrev #:abbrev-mixin #:possibly-expand-abbrev
           #:add-abbrev))

(defpackage :drei-syntax
  (:use :clim-lisp :clim :drei-buffer :drei-base :flexichain :esa-utils)
  (:export #:syntax #:syntaxp #:define-syntax #:*default-syntax* #:cursor-positions
           #:syntax-command-table #:use-editor-commands-p #:additional-command-tables #:define-syntax-command-table
           #:eval-option
           #:define-option-for-syntax
           #:current-attributes-for-syntax
           #:make-attribute-line
           #:syntax-from-name
           #:update-syntax #:update-syntax-for-display
           #:grammar #:grammar-rule #:add-rule
           #:parser #:initial-state
           #:advance-parse
           #:parse-tree #:start-offset #:end-offset
           #:lexer #:nb-lexemes #:lexeme #:insert-lexeme
           #:incremental-lexer #:next-lexeme
           #:delete-invalid-lexemes #:inter-lexeme-object-p
           #:skip-inter-lexeme-objects #:update-lex
           #:parse-stack-top #:target-parse-tree #:parse-state-empty-p
           #:parse-stack-next #:parse-stack-symbol
           #:parse-stack-parse-trees #:map-over-parse-trees
           #:no-such-operation #:no-expression
           #:name-for-info-pane
           #:display-syntax-name
           #:syntax-line-indentation
           #:forward-expression #:backward-expression
           #:eval-defun
           #:record-line-vertical-offset
           #:line-vertical-offset
           #:backward-paragraph #:forward-paragraph
           #:backward-sentence #:forward-sentence
           #:forward-list #:backward-list
           #:syntax-line-comment-string
           #:line-comment-region #:comment-region
           #:line-uncomment-region #:uncomment-region
           #:word-constituentp
           #:whitespacep
           #:page-delimiter
           #:paragraph-delimiter)
  (:documentation "The syntax protocol. Contains functions that
  can be used to implement higher-level operations on buffer
  contents."))

(defpackage :drei
  (:use :clim-lisp :clim :drei-buffer :drei-base :drei-abbrev
        :drei-syntax :flexichain :drei-undo :esa-buffer :esa-io :esa
        :esa-utils :drei-kill-ring)
  (:export #:drei-buffer #:needs-saving
           #:filepath #:file-saved-p #:file-write-time
           #:read-only-p #:buffer-read-only
           
           #:display-drei #:display-drei-pane #:display-drei-area #:full-redisplay
           #:offset-to-screen-position
           #:page-down #:page-up
           #:indent-tabs-mode
           #:isearch-state #:search-string #:search-mark
           #:search-forward-p #:search-success-p
           #:query-replace-state #:string1 #:string2 #:buffers #:mark #:occurrences
           #:with-undo
           #:drei-buffer
           #:drei-textual-view #:+drei-textual-view+

           ;; Signals and conditions.
           #:user-condition-mixin
           #:buffer-read-only
           #:buffer-single-line

           ;; DREI command tables.
           #:comment-table #:deletion-table #:editing-table
           #:fill-table #:indent-table #:marking-table #:case-table
           #:movement-table #:search-table #:info-table #:self-insert-table
           #:editor-table #:exclusive-gadget-table #:exclusive-input-editor-table

           ;; DREI interface stuff.
           #:drei #:drei-pane #:drei-gadget-pane #:drei-area
           #:handling-drei-conditions
           #:execute-drei-command #:display-drei-contents #:display-drei-cursor
           #:with-drei-options
           #:performing-drei-operations #:invoke-performing-drei-operations
           #:with-bound-drei-special-variables
           #:accepting-from-user #:invoke-accepting-from-user

           ;; Input-editor interface stuff.
           #:drei-input-editing-mixin #:drei-instance
           #:object #:result-type

           ;; Drei cursors.
           #:drei-cursor
           #:mark-cursor #:active #:mark
           #:active-ink #:inactive-ink #:ink

           ;; Accessors of DREI instances.
           #:buffer
           #:point
           #:top #:bot
           #:tab-space-count #:space-width #:tab-width
           #:auto-fill-mode #:auto-fill-column
           #:isearch-mode #:isearch-states #:isearch-previous-string
           #:query-replace-mode
           #:region-visible-p
           #:minibuffer
           
           #:dabbrev-expansion-mark
           #:original-prefix
           #:prefix-start-offset
           #:overwrite-mode
           #:goal-column
           #:in-focus-p

           ;; Info functions.
           #:current-point
           #:current-mark

           ;; Info variables.
           #:*current-point*
           #:*current-mark*
           #:*current-syntax*

           ;; Configuration.
           #:*foreground-color*
           #:*background-color*
           #:*show-mark*
           #:*use-tabs-for-indentation*))

(defpackage :drei-motion
  (:use :clim-lisp :drei-base :drei-buffer :drei-syntax)
  (:export #:forward-to-word-boundary #:backward-to-word-boundary
           #:define-motion-fns
           #:beep-limit-action #:revert-limit-action #:error-limit-action
           #:motion-limit-error
           #:make-diligent-motor

           ;; Lines
           #:forward-one-line
           #:backward-one-line
           #:forward-line
           #:backward-line

           ;; Words
           #:forward-one-word
           #:backward-one-word
           #:forward-word
           #:backward-word

           ;; Pages
           #:forward-one-page
           #:backward-one-page
           #:forward-page
           #:backward-page

           ;; Expressions
           #:forward-one-expression
           #:backward-one-expression
           #:forward-expression
           #:backward-expression

           ;; Definitions
           #:forward-one-definition
           #:backward-one-definition
           #:forward-definition
           #:backward-definition

           ;; Up
           #:forward-one-up
           #:backward-one-up
           #:forward-up
           #:backward-up

           ;; Down
           #:forward-one-down
           #:backward-one-down
           #:forward-down
           #:backward-down

           ;; Paragraphs
           #:forward-one-paragraph
           #:backward-one-paragraph
           #:forward-paragraph
           #:backward-paragraph

           ;; Sentences
           #:forward-one-sentence
           #:backward-one-sentence
           #:forward-sentence
           #:backward-sentence)
  (:documentation "Functions and facilities for moving a mark
around by syntactical elements. The functions in this package are
syntax-aware, and their behavior is based on the semantics
defined by the syntax of the buffer, that the mark they are
manipulating belong to. These functions are also directly used to
implement the motion commands."))

(defpackage :drei-editing
  (:use :clim-lisp :drei-base :drei-buffer
        :drei-syntax :drei-motion :drei :drei-kill-ring)
  (:export #:transpose-objects
           
           ;; Lines
           #:forward-delete-line #:backward-delete-line
           #:forward-kill-line #:backward-kill-line
           #:transpose-lines
           #:forward-delete-line-start #:backward-delete-line-start
           #:forward-kill-line-start #:backward-kill-line-start
           #:transpose-line-starts
           
           ;; Words
           #:forward-delete-word #:backward-delete-word
           #:forward-kill-word #:backward-kill-word
           #:transpose-words

           ;; Pages
           #:forward-delete-page #:backward-delete-page
           #:forward-kill-page #:backward-kill-page
           #:transpose-pages
           
           ;; Expressions
           #:forward-delete-expression #:backward-delete-expression
           #:forward-kill-expression #:backward-kill-expression
           #:transpose-expressions

           ;; Definitions
           #:forward-delete-definition #:backward-delete-definition
           #:forward-kill-definition #:backward-kill-definition
           #:transpose-definitions

           ;; Paragraphs
           #:forward-delete-paragraph #:backward-delete-paragraph
           #:forward-kill-paragraph #:backward-kill-paragraph
           #:transpose-paragraphs

           ;; Sentences
           #:forward-delete-sentence #:backward-delete-sentence
           #:forward-kill-sentence #:backward-kill-sentence
           #:transpose-sentences)
  (:documentation "Functions and facilities for changing the
buffer contents by syntactical elements. The functions in this
package are syntax-aware, and their behavior is based on the
semantics defined by the syntax of the buffer, that the mark they
are manipulating belong to. These functions are also directly
used to implement the editing commands."))

(defpackage :drei-core
  (:use :clim-lisp :drei-base :drei-buffer
        :drei-syntax :drei-motion :drei :drei-kill-ring
        :drei-editing :clim :drei-abbrev :esa :esa-buffer :esa-io
        :esa-utils :drei-undo)
  (:export #:goto-position
           #:goto-line
           #:replace-one-string
           #:possibly-fill-line
           #:back-to-indentation
           #:insert-character
           #:delete-horizontal-space
           #:indent-current-line
           #:insert-pair
           #:downcase-word #:upcase-word #:capitalize-word
           #:indent-region
           #:fill-line #:fill-region
           #:indent-line #:delete-indentation
           #:set-syntax

           #:*killed-rectangle*
           #:map-rectangle-lines
           #:extract-and-delete-rectangle-line
           #:insert-rectangle-at-mark
           #:clear-rectangle-line
           #:open-rectangle-line
           #:replace-rectangle-line
           #:insert-in-rectangle-line
           #:delete-rectangle-line-whitespace
           #:with-narrowed-buffer))

(defpackage :drei-fundamental-syntax
  (:use :clim-lisp :clim :drei-buffer :drei-base 
        :drei-syntax :flexichain :drei)
  (:export #:fundamental-syntax #:scan))

(defpackage :drei-lisp-syntax
  (:use :clim-lisp :clim :clim-extensions :drei-buffer :drei-base 
        :drei-syntax :drei-fundamental-syntax :flexichain :drei
        :drei-motion :drei-editing :esa-utils :esa :drei-core :esa-io)
  (:export #:lisp-string
           #:edit-definition)
  (:shadow clim:form))

(defpackage :drei-commands
  (:use :clim-lisp :drei-base :drei-buffer
        :drei-syntax :drei-motion :drei :drei-kill-ring
        :drei-editing :clim :drei-abbrev :esa :esa-buffer :esa-io
        :esa-utils :drei-core :drei-undo)
  (:export #:define-motion-commands
           #:define-deletion-commands
           #:define-editing-commands))
