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

(defparameter *simple-area-gesture-table* (make-hash-table))

(defun add-gesture-command-to-table (gesture-spec command-name table)
  (multiple-value-bind (gesture-name modifier-bits)
      (if (atom gesture-spec)
	  (values gesture-spec 0)
	  (values (car gesture-spec)
		  (apply #'make-modifier-state (cdr gesture-spec))))
    (push (cons modifier-bits command-name)
	  (gethash gesture-name table nil))))

(defgeneric lookup-gesture-command (gesture table))

(defmethod lookup-gesture-command ((gesture character) table)
  (cdr (assoc 0 (gethash gesture table nil))))

(defmethod lookup-gesture-command ((gesture key-press-event) table)
  (cdr (assoc (modifier-state gesture)
	      (gethash (key-name gesture) table nil))))

(defmethod lookup-gesture-command (gesture table)
  nil)

(defvar *area*)
(defvar *buffer*)

(defmethod execute-gesture-command (gesture (area editable-area) table)
  (let ((command (lookup-gesture-command gesture table)))
    (if command
	(let ((*area* area)
	      (*buffer* (buffer area)))
	  (funcall command :input-gesture gesture)
	  (redisplay-area area)))))

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

(loop for i from (char-code #\space) to (char-code #\~)
      do (add-gesture-command-to-table (code-char i)
				       'insert-character
				       *simple-area-gesture-table*))

(add-gesture-command-to-table #\backspace
			      'backwards-delete-character
			      *simple-area-gesture-table*)

(add-gesture-command-to-table '(#\f :control)
			      'forward-character
			      *simple-area-gesture-table*)

(add-gesture-command-to-table '(#\b :control)
			      'backward-character
			      *simple-area-gesture-table*)
