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
    (setf *insert-extent* (make-instance 'extent
					 :start-line line
					 :start-pos pos))
    (multiple-value-bind (printed-rep accept-object)
	(present-acceptably-to-string object type
				      +textual-view+ ; XXX
				      type)
      (format *trace-output* "insert-ptype-history: ~S, ~S~%"
	      (pos (bp-start *insert-extent*))
	      (pos (bp-end *insert-extent*)))
      ;; XXX accept-object
      (insert *buffer* printed-rep :line line :pos pos)
      (format *trace-output* "insert-ptype-history:: ~S, ~S~%"
	      (pos (bp-start *insert-extent*))
	      (pos (bp-end *insert-extent*))))))


(defun cmd-presentation-history-yank (&key &allow-other-keys)
  (let* ((accepting-type climi::*active-history-type*)
	 (history (and accepting-type
		       (climi::presentation-type-history accepting-type))))
    (setq *last-history-type* accepting-type
	  *last-history* history)
    (when history
      (multiple-value-bind (object type)
	  (climi::presentation-history-head history accepting-type)
	(if type
	    (insert-ptype-history object type))))))

(defun cmd-presentation-history-yank-next (&key &allow-other-keys)
  (when (and *last-history-type* *last-history*)
      (multiple-value-bind (object type)
	  (climi::presentation-history-next *last-history* *last-history-type*)
	(when type
	  (delete-region *buffer*
			 (bp-start *insert-extent*)
			 (bp-end *insert-extent*))
	  (insert-ptype-history object type)))))


(defun goatee-next (&key &allow-other-keys)
  (cond ((or (eq *last-command* 'cmd-presentation-history-yank)
	     (and (eq *last-command* 'goatee-next)
		  (or (eq *last-yank-command* 'cmd-presentation-history-yank-next) 
		      (eq *last-yank-command*
			  'cmd-presentation-history-yank-prev)))) 
	 (funcall #'cmd-presentation-history-yank-next)
	 (setq *last-yank-command* 'cmd-presentation-history-yank-next))
	((or (eq *last-command* 'cmd-yank)
	     (eq *last-command* 'cmd-yank-prev)
	     (and (eq *last-command* 'goatee-next)
		  (or (eq *last-yank-command* 'cmd-yank-next) 
		      (eq *last-yank-command* 'cmd-yank-prev))))
	 (funcall #'cmd-yank-next)
	 (setq *last-yank-command* 'cmd-yank-next))
	(t (beep))))

(add-gesture-command-to-table '(#\y :control :meta)
			      'cmd-presentation-history-yank
			      *simple-area-gesture-table*)

(add-gesture-command-to-table '(#\y :meta)
			      'goatee-next
			      *simple-area-gesture-table*)
