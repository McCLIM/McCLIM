;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

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

(in-package :CLIM-INTERNALS)

;;; Commands and presentation translators that live in the
;;; global-command-table. 

;;; Global help command

(define-command (com-help :command-table global-command-table :name "Help")
    ((kind '(completion (("Keyboard" keyboard) ("Commands" commands))
	                :value-key cadr)
	   :prompt "with"
	   :default 'keyboard
	   :display-default nil))
  (if (eq kind 'keyboard)
      (format *query-io* "Input editor commands are like Emacs.~%")
      (let ((command-table (frame-command-table *application-frame*))
	    (command-names nil))
	(map-over-command-table-names #'(lambda (name command)
					  (push (cons name command)
						command-names))
				      command-table)
	(setf command-names (sort command-names #'(lambda (a b)
						    (string-lessp (car a)
								  (car b)))))
	(loop for (nil . command) in command-names
	      do (progn
		   (fresh-line *query-io*)
		   (present command
			    `(command-name :command-table ,command-table)
			    :stream *query-io*))))))

;;; Describe command.  I don't know if this should go in the global command
;;; table, but we don't exactly have a surplus of commands yet...

(define-command (com-describe :command-table global-command-table
		 :name "Describe")
    ((obj 'expression
	  :prompt "object"
	  :gesture :describe))
  (describe obj *query-io*))

;;; Another somewhat gratuitous command...

(define-gesture-name :describe-presentation :pointer-button-press
  (:left :super))

(define-presentation-to-command-translator com-describe-presentation
    (t com-describe global-command-table
     :gesture :describe-presentation
     :tester ((presentation)
	      (not (eq presentation *null-presentation*)))
     :documentation "Describe Presentation"
     :pointer-documentation "Describe Presentation")
  (presentation)
  (list presentation))

;;; Default presentation translator; translates an object to itself.

(define-presentation-translator default-translator
    (t nil global-command-table
     :gesture :select
     :tester ((presentation context-type)
	      (presentation-subtypep (presentation-type presentation)
				     context-type))
     :tester-definitive t
     :documentation ((object presentation stream)
		     (present object (presentation-type presentation)
			      :stream stream
			      :sensitive nil)))
  (object)
  object)
