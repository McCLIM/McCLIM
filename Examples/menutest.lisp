;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: MENUTEST; Base: 10; Lowercase: Yes -*-

;; $fiHeader: calcuator.lisp,v 1.0 22/08/200 $

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

(defpackage :MENUTEST (:USE :CLIM :CLIM-EXTENSIONS :COMMON-LISP))

(in-package :MENUTEST)

(defun menutest ()
  (loop for port in climi::*all-ports*
      do (destroy-port port))
  (setq climi::*all-ports* nil)
  (let ((frame (make-application-frame 'menutest)))
    (unless clim-sys:*multiprocessing-p*
      (run-frame-top-level frame))
    frame))

(defmethod menutest-frame-top-level
  ((frame application-frame)
   &key (command-parser 'command-line-command-parser)
   (command-unparser 'command-line-command-unparser)
   (partial-command-parser
    'command-line-read-remaining-arguments-for-partial-command)
   (prompt "Command: "))
  (declare (ignore command-parser command-unparser partial-command-parser prompt))
  (loop (event-read (climi::frame-pane frame))))
     
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
  (:panes
   (screen :text-field :value "stuff" :height 200)
   (menu-bar
    (clim-internals::make-menu-bar 'menubar-command-table)))
  (:layouts
   (defaults (vertically () menu-bar screen)))
  (:top-level (menutest-frame-top-level)))

