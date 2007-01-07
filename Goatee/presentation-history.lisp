;;; -*- Mode: Lisp; Package: GOATEE -*-

;;;  (c) copyright 2004 by Tim Moore (moore@bricoworks.com)
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

;;; presentation (input) histories.  hack hack hack
(in-package :goatee)

(defvar *last-yank-command* nil)

;;; We story the presentation type and history here for yank-next
;;; because the accept that established the original history may not
;;; be active anymore after rescanning the input.
(defvar *last-history-type* nil)
(defvar *last-history* nil)

(defun insert-ptype-history (object type)
  (multiple-value-bind (line pos)
      (point* *buffer*)
    (multiple-value-bind (printed-rep accept-object)
	(present-acceptably-to-string object type
				      +textual-view+ ; XXX
				      type)
      ;; XXX accept-object
      (insert *buffer* printed-rep :line line :pos pos))))


(defun cmd-history-yank-next (&key &allow-other-keys)
  (let* ((accepting-type climi::*active-history-type*)
         (history (and accepting-type
                       (presentation-type-history accepting-type))))
    (when history
      (multiple-value-bind (object type)
          (climi::presentation-history-next history accepting-type)
        (when type
          (clear-buffer *buffer*)
          (insert-ptype-history object type))))))

(defun cmd-history-yank-previous (&key &allow-other-keys)
  (let* ((accepting-type climi::*active-history-type*)
         (history (and accepting-type
                       (presentation-type-history accepting-type))))
    (when history
      (multiple-value-bind (object type)
          (climi::presentation-history-previous history accepting-type)
        (when type
          (clear-buffer *buffer*)
          (insert-ptype-history object type))))))

(add-gesture-command-to-table '(#\p :meta)
                              'cmd-history-yank-previous
                              *simple-area-gesture-table*)

(add-gesture-command-to-table '(#\n :meta)
                              'cmd-history-yank-next
                              *simple-area-gesture-table*)
