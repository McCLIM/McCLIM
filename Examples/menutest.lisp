;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: MENUTEST; Base: 10; Lowercase: Yes -*-

;;;  (c) copyright 2000 by 
;;;	      Robert Strandh (strandh@labri.u-bordeaux.fr)

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

(defpackage :MENUTEST (:USE :CLIM :CLIM-EXTENSIONS :CLIM-LISP))

(in-package :MENUTEST)

(defun menutest ()
  (loop for port in climi::*all-ports*
      do (destroy-port port))
  (setq climi::*all-ports* nil)
  (let ((frame (make-application-frame 'menutest)))
    (run-frame-top-level frame)
    frame))

(define-command com-file ()
  (format *error-output* "you pressed the File button~%")
  (finish-output *error-output*))

(define-command com-hello ()
  (format *error-output* "you pressed the Hello button~%")
  (finish-output *error-output*))

(define-command com-hi ()
  (format *error-output* "you pressed the Hi button~%")
  (finish-output *error-output*))

(make-command-table 'buffer-command-table
		    :errorp nil
		    :menu '(("Hello there" :command com-hello)
			    ("Hi there" :command com-hi)))

(make-command-table 'menubar-command-table
		    :errorp nil
		    :menu '(("Buffer" :menu buffer-command-table)
			    ("File" :command com-file)))

(define-application-frame menutest ()
  ()
  (:menu-bar menubar-command-table)
  (:panes
   (screen :application))
  (:layouts
   (defaults (vertically () screen)))
  #+nil
  (:top-level (menutest-frame-top-level)))

