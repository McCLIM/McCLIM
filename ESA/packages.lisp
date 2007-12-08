;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;;;  (c) copyright 2004-2006 by
;;;           Robert Strandh (strandh@labri.fr)
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

;;; Package definitions for ESA.

(defpackage :esa-utils
  (:use :clim-lisp)
  (:export #:with-gensyms
           #:once-only
           #:unlisted
           #:fully-unlisted
           #:listed
           #:list-aref
           #:letf
           #:letf*
           #:display-string
           #:object-equal
           #:object=
           #:no-upper-p
           #:case-relevant-test
           #:with-keywords-removed
           #:invoke-with-dynamic-bindings-1
           #:invoke-with-dynamic-bindings
           #:maptree
           #:subtype-compatible-p
           #:observable-mixin
           #:add-observer #:remove-observer
           #:observer-notified #:notify-observers
           #:name-mixin #:name
           #:subscriptable-name-mixin #:subscripted-name #:subscript #:subscript-generator))

(defpackage :esa
  (:use :clim-lisp :clim :esa-utils)
  (:export #:*esa-instance*
           #:buffers #:esa-current-buffer #:current-buffer
           #:windows #:esa-current-window #:current-window
           #:*previous-command*
           #:*minibuffer* #:minibuffer #:minibuffer-pane #:display-message
           #:with-minibuffer-stream
           #:esa-pane-mixin #:previous-command
           #:info-pane #:master-pane
           #:esa-frame-mixin #:recordingp #:executingp
           #:*esa-abort-gestures* #:*numeric-argument-p* #:*current-gesture* #:*command-processor*
           #:unbound-gesture-sequence #:gestures
           #:command-processor #:instant-macro-execution-mixin
           #:asynchronous-command-processor #:command-loop-command-processor
           #:overriding-handler #:directly-processing-p #:process-gesture #:process-gestures-or-command
           #:*extended-command-prompt*
           #:define-esa-top-level #:esa-top-level #:simple-command-loop
           #:convert-to-gesture #:gesture-name
           #:global-esa-table #:keyboard-macro-table
           #:help-table
	   #:help-stream
           #:set-key
           #:find-applicable-command-table
           #:esa-command-parser
           #:esa-partial-command-parser

           #:gesture-matches-gesture-name-p #:meta-digit
           #:proper-gesture-p
           #:universal-argument #:meta-minus))

(defpackage :esa-buffer
  (:use :clim-lisp :clim :esa :esa-utils)
  (:export #:frame-make-buffer-from-stream #:make-buffer-from-stream
           #:frame-save-buffer-to-stream #:save-buffer-to-stream
           #:filepath #:name #:needs-saving #:file-write-time #:file-saved-p
           #:esa-buffer-mixin
           #:frame-make-new-buffer #:make-new-buffer
           #:read-only-p))

(defpackage :esa-io
  (:use :clim-lisp :clim :esa :esa-buffer :esa-utils)
  (:export #:frame-find-file #:find-file
           #:frame-find-file-read-only #:find-file-read-only
           #:frame-set-visited-file-name #:set-visited-filename
           #:frame-save-buffer #:save-buffer
           #:frame-write-buffer #:write-buffer
           #:esa-io-table))

#-(or mcclim building-mcclim)
(defpackage :clim-extensions
  (:use :clim-lisp :clim)
  (:export
   #:+blue-violet+
   #:+dark-blue+
   #:+dark-green+
   #:+dark-violet+
   #:+gray50+
   #:+gray85+
   #:+maroon+
   #:+purple+))