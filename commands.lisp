;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)

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

(in-package :CLIM-INTERNALS)

(defmacro define-command (name args &body body)
  `(defun ,name ,(loop for arg in args
		     collect (first arg))
     ,@body))

(defvar *command-parser* nil)
(defvar *command-unparser* nil)
(defvar *partial-command-parser* nil)

(defun command-name (command)
  (first command))

(defun command-arguments (command)
  (rest command))

(defun read-command (command-table
		     &key (stream *query-io*)
			  (command-parser *command-parser*)
			  (command-unparser *command-unparser*)
			  (partial-command-parser *partial-command-parser*)
			  use-keystrokes)
  (read stream))
