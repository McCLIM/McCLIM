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

(in-package :clim-internals)

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
     :menu nil
     :documentation ((object presentation context-type frame event window x y stream)
                     (let* ((type (presentation-type presentation))
                            (options (decode-options type))
                            (description (getf options :description)))
                       (if description
                           (if (stringp description)
                               (princ description stream)
                               (funcall description object
                                        :presentation presentation
                                        :context-type context-type
                                        :frame frame
                                        :event event
                                        :window window
                                        :x x :y y
                                        :stream stream))
                           (present object (presentation-type presentation)
                                    :stream stream
                                    :sensitive nil)))))
  (object)
  object)

(define-presentation-action presentation-menu
    (t nil global-command-table
       :documentation "Menu"
       :menu nil
       :gesture :menu
       :tester ((presentation frame window x y event)
                (find-applicable-translators presentation
                                             *input-context* ; XXX ?
                                             frame window x y
                                             :event event ; XXX ?
                                             :for-menu t
                                             :fastp t)))
  (presentation frame window x y)
  (call-presentation-menu presentation *input-context*
                          frame window x y
                          :for-menu t))

;;; Action for possibilities menu of complete-input

(define-presentation-action possibilities-menu
    (blank-area nil global-command-table
     :documentation "Possibilities menu for completion"
     :pointer-documentation "Possibilities"
     :menu nil
     :gesture :menu
     :tester (()
	      *completion-possibilities-continuation*))
  ()
  (funcall *completion-possibilities-continuation*))

;;; Turn symbols and lists into forms

(define-gesture-name :literal-expression :pointer-button-press
  (:left :meta))

(define-presentation-translator expression-to-form
    (expression form global-command-table
     :gesture :select
     :menu nil
     :priority 11)
  (object)
  (if (or (consp object) (and (symbolp object) (not (null object))))
      `',object
      object))

(define-presentation-translator expression-as-form
    (expression form global-command-table
     :gesture :literal-expression
     :menu nil
     :documentation "expression as literal"
     :tester ((object)
	      (or (symbolp object) (consp object)))
     :tester-definitive t)
  (object)
  (values object 'form))

;;; Support for accepting subforms of a form.

;;; Used to signal a read that ends a list
(define-presentation-type list-terminator ()
  :inherit-from 'form)

(defvar *sys-read* #'read)
(defvar *sys-read-preserving-whitespace* #'read-preserving-whitespace)

;;; Arguments for read
(defvar *eof-error-p* t)
(defvar *eof-value* nil)
(defvar *recursivep* nil)

;;; For passing arguments to the call to %read-list-expression.
;;; Gross, but not as gross as using presentation type options.
;;;
;;; XXX But I am using a presentation type option to choice the
;;; subform reader; what's the difference? Granted the presentation
;;;type specifier is constant.... -- moore

(defvar *dot-ok*)
(defvar *termch*)

(defun whitespacep (char)
  (or (char= char #\Space)
      (char= char #\Newline)
      (char= char #\Return)
      (char= char #\Tab)))

#+openmcl
(defvar *sys-%read-list-expression* #'ccl::%read-list-expression)

#+openmcl
(with-system-redefinition-allowed
(defun ccl::%read-list-expression (stream *dot-ok* &optional (*termch* #\)))
  (if (typep stream 'input-editing-stream)
      (progn
	;; Eat "whitespace" so it is not deleted by presentation-replace-input
	(let ((gesture (read-gesture :stream stream :timeout 0 :peek-p t)))
	  (when (and gesture
		     (or (activation-gesture-p gesture)
			 (delimiter-gesture-p gesture)
			 (and (characterp gesture)
			      (whitespacep gesture))))  
	    (read-gesture :stream stream)))
	(multiple-value-bind (object type)
	    (accept '((form) :subform-read t) :stream stream :prompt nil)
	  (values object (if (presentation-subtypep type 'list-terminator)
			     nil
			     t))))
      (funcall *sys-%read-list-expression* stream *dot-ok* *termch*)))
) 					; with-system-redefinition-allowed

(define-presentation-method accept ((type form) stream (view textual-view)
				    &key (default nil defaultp) default-type)
  (declare (ignore default defaultp default-type))
  (let* ((object nil)
	 (ptype nil))
    (if (and #-openmcl nil subform-read)
	(multiple-value-bind (val valid)
	    (funcall *sys-%read-list-expression* stream *dot-ok* *termch*)
	  (if valid
	      (setq object val)
	      (return-from accept (values nil 'list-terminator))))
	;; We don't want activation gestures like :return causing an eof
	;; while reading a form, so we turn the activation gestures into
	;; delimiter gestures.
	(with-delimiter-gestures (*activation-gestures*)
	  (with-activation-gestures (nil :override t)
	    (setq object (funcall (if preserve-whitespace
				      *sys-read-preserving-whitespace*
				      *sys-read*)
				  stream
				  *eof-error-p* *eof-value* *recursivep*)))))
    (setq ptype (presentation-type-of object))
    (unless (presentation-subtypep ptype 'form)
      (setq ptype 'form))
    (if (or subform-read auto-activate)
	(values object ptype)
	(loop for c = (read-char stream)
	      until (activation-gesture-p c)
	      finally (return (values object ptype))))))

(with-system-redefinition-allowed
(defun read (&optional (stream *standard-input*)
	     (eof-error-p t)
	     (eof-value nil)
	     (recursivep nil))
  (if (typep stream 'input-editing-stream)
      (let ((*eof-error-p* eof-error-p)
	    (*eof-value* eof-value)
	    (*recursivep* recursivep))
	(accept '((form) :auto-activate t :preserve-whitespace nil)
		:stream stream :prompt nil))
      (funcall *sys-read* stream eof-error-p eof-value recursivep)))

(defun read-preserving-whitespace (&optional (stream *standard-input*)
	     (eof-error-p t)
	     (eof-value nil)
	     (recursivep nil))
  (if (typep stream 'input-editing-stream)
      (let ((*eof-error-p* eof-error-p)
	    (*eof-value* eof-value)
	    (*recursivep* recursivep))
	(accept '((form) :auto-activate t :preserve-whitespace t)
		:stream stream :prompt nil))
      (funcall *sys-read-preserving-whitespace*
	       stream eof-error-p eof-value recursivep)))
) ; with-system-redefinition-allowed
