;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by Robert Strandh (strandh@labri.u-bordeaux.fr)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Command tables

(defgeneric command-table-name (command-table))
(defgeneric command-table-inherit-from (command-table))

;;; According to the specification, command menu items are stored as
;;; lists.  This way seems better, and I hope nothing will break.
(defclass menu-item ()
  ((name :initarg :name :reader command-menu-item-name)
   (type :initarg :type :reader command-menu-item-type)
   (value :initarg :value :reader command-menu-item-value)
   (documentation :initarg :documentation)
   (text-style :initarg :text-style :initform nil)
   (keystroke :initarg :keystroke :initform nil)))

(defmethod print-object ((item menu-item) stream)
  (print-unreadable-object (item stream :identity t :type t)
    (format stream "~S" (command-menu-item-name item))))

(defun command-menu-item-options (menu-item)
  (with-slots (documentation text-style) menu-item
    (list ':documentation documentation ':text-style text-style)))

(defclass command-table ()
  ((name :initarg :name :reader command-table-name)
   (inherit-from :initarg :inherit-from
		 :initform '()
		 :reader command-table-inherit-from)
   (commands :initarg :commands :initform (make-hash-table :test #'eq))
   (command-line-names :initform (make-hash-table :test #'equal))
   (presentation-translators :initform '())
   (keystroke-accelerators :initform (make-hash-table :test #'equal))
   (menu :initarg :menu :initform '())))

(defun command-table-p (object)
  (typep object 'command-table))

(defmethod print-object ((table command-table) stream)
  (print-unreadable-object (table stream :identity t :type t)
    (format stream "~S" (command-table-name table))))

(defclass standard-command-table (command-table)
  ())
   
(defparameter *command-tables* (make-hash-table :test #'eq))

(define-condition command-table-error (error)
  ())

(define-condition command-table-not-found (command-table-error)
  ())

(define-condition command-table-already-exists (command-table-error)
  ())

(define-condition command-not-present (command-table-error)
  ())

(define-condition command-not-accessible (command-table-error)
  ())

(define-condition command-already-present (command-table-error)
  ())

(defun find-command-table (name &key (errorp t))
  (cond ((command-table-p name) name)
	((gethash name *command-tables*))
	(errorp (error 'command-table-not-found))
	(t nil)))

; adjusted to allow anonymous command-tables for menu-bars
(defun make-command-table (name &key inherit-from menu (errorp t))
  (if (and name errorp (gethash name *command-tables*))
      (error 'command-table-already-exists)
      (let ((result (make-instance 'standard-command-table :name name
	                 :inherit-from inherit-from
	                 :menu (mapcar
		                #'(lambda (item)
			            (destructuring-bind
			             (name type value &key keystroke documentation text-style)
			             item
			             (make-instance 'menu-item
			               :name name :type type :value value
			               :documentation documentation
			               :text-style text-style
			               :keystroke keystroke)))
		                menu))))
        (when name
          (setf (gethash name *command-tables*) result))
        result)))

(make-command-table 'global-command-table)
(make-command-table 'user-command-table)

(defmacro define-command-table (name &key inherit-from menu)
  (let ((inherit (cond (inherit-from)
		       (t '(global-command-table))))
	(menu-items menu))
    `(let ((old-table (gethash ',name *command-tables* nil)))
       (if old-table
	   (with-slots (inherit-from menu) old-table
	     (setq inherit-from ',inherit
		   menu ',menu-items)
	     old-table)
           (make-command-table ',name
			     :inherit-from ',inherit
			     :menu ',menu
			     :errorp nil)))))

(defun command-name-from-symbol (symbol)
  (let ((name (symbol-name symbol)))
    (string-capitalize
     (substitute
      #\Space #\-
      (subseq name (string/= "COM-" name))))))

(defun remove-command-from-command-table (command-name
					  command-table
					  &key (errorp t))
  (let ((table (find-command-table command-table)))
    (with-slots (commands command-line-names keystroke-accelerators menu) table
      (if (and errorp (not (gethash command-name commands)))
	  (error 'command-not-present)
	  (progn 
	    (maphash #'(lambda (key value)
			 (when (eq value command-name)
			   (remhash key command-line-names)))
		     command-line-names)
	    (maphash #'(lambda (key value)
			 (when (eq value command-name)
			   (remhash key keystroke-accelerators)))
		     keystroke-accelerators)
	    (setf menu
		  (delete-if #'(lambda (item)
				 (with-slots (type value) item
				   (and (eq type ':command)
					(or (eq value command-name)
					    (and (consp value)
						 (eq (car value) command-name))))))
			     menu))
	    (remhash command-name commands))))))

(defun add-command-to-command-table (command-name
				     command-table
				     &key name menu keystroke (errorp t))
  (let ((table (find-command-table command-table)))
    (with-slots (commands command-line-names keystroke-accelerators) table
      (if (and errorp (gethash command-name commands))
	  (error 'command-already-present)
	  (progn (remove-command-from-command-table command-name command-table
						    :errorp nil)
		 (setf (gethash command-name commands) t)
		 (when name
		   (let ((command-line-name
			  (if (stringp name)
			      name
			      (command-name-from-symbol command-name))))
		     (setf (gethash command-name commands) command-line-name)
		     (setf (gethash command-line-name command-line-names)
			   command-name)))
		 (when menu
		   (let ((menu-item-name
			  (cond ((stringp menu) menu)
				((and (eq menu t) (stringp name)) name)
				((eq menu t) (command-name-from-symbol command-name))
				((consp menu) (car menu)))))
		     (apply #'add-menu-item-to-command-table
			    command-table
			    menu-item-name
			    :command command-name
			    (if (consp menu)
				(cdr menu)
				'()))))
		 (when keystroke
		   (setf (gethash keystroke keystroke-accelerators) command-name)))))))
						  

(defun apply-with-command-table-inheritance (fun command-table)
  (funcall fun command-table)
  (mapc #'(lambda (inherited-command-table)
	    (apply-with-command-table-inheritance
	     fun (find-command-table inherited-command-table)))
	(command-table-inherit-from command-table)))

(defmacro do-command-table-inheritance ((command-table-var command-table) &body body)
  `(apply-with-command-table-inheritance
    #'(lambda (,command-table-var)
	,@body)
    (find-command-table ,command-table)))

(defun map-over-command-table-commands (function command-table &key (inherited t))
  (if inherited
      (apply-with-command-table-inheritance
       #'(lambda (table)
	   (maphash #'(lambda (key val)
			(declare (ignore val))
			(funcall function key))
		    (slot-value table 'commands)))
       (find-command-table command-table))
      (maphash #'(lambda (key val)
		   (declare (ignore val))
		   (funcall function key))
	       (slot-value (find-command-table command-table) 'commands))))

(defun map-over-command-table-names (function command-table &key (inherited t))
  (if inherited
      (apply-with-command-table-inheritance
       #'(lambda (table)
	   (maphash function (slot-value table 'command-line-names)))
       (find-command-table command-table))
      (maphash function (slot-value (find-command-table command-table)
				    'command-line-names))))

(defun command-present-in-command-table-p (command-name command-table)
  (let ((table (find-command-table command-table)))
    (if (gethash command-name (slot-value table 'commands))
	table
	nil)))

(defun command-accessible-in-command-table-p (command-name command-table)
  (or (command-present-in-command-table-p command-name command-table)
      (some #'(lambda (table)
		(command-accessible-in-command-table-p
		 command-name
		 (find-command-table table)))
	    (command-table-inherit-from command-table))))

(defun find-command-from-command-line-name (name command-table &key (errorp t))
  (apply-with-command-table-inheritance
   #'(lambda (table)
       (maphash #'(lambda (key value)
		    (when (string-equal key name)
		      (return-from find-command-from-command-line-name
				   (values value table))))
		(slot-value table 'command-line-names)))
   (find-command-table command-table))
  (if errorp
      (error 'command-not-accessible)))

(defun command-line-name-for-command (command-name command-table &key (errorp t))
  (do-command-table-inheritance (table command-table)
    (let ((command-line-name (gethash command-name (slot-value table 'commands))))
      (when (stringp command-line-name)
	(return-from command-line-name-for-command command-line-name))))
  (cond ((eq errorp :create) (command-name-from-symbol command-name))
	(errorp (error 'command-not-accessible))
	(t nil)))

(defun find-menu-item (menu-name command-table &key (errorp t))
  (let* ((table (find-command-table command-table))
	 (mem (member menu-name (slot-value table 'menu)
		      :key #'command-menu-item-name :test #'string-equal)))
    (cond (mem (values (car mem) command-table))
	  (errorp (error 'command-not-accessible))
	  (t nil))))

(defun remove-menu-item-from-command-table (command-table string &key (errorp t))
  (let ((table (find-command-table command-table))
	(item (find-menu-item string command-table :errorp nil)))
    (with-slots (menu) table
      (if (and errorp (not item))
	  (error 'command-not-present)
	  (setf menu (delete string menu
			     :key #'command-menu-item-name
			     :test #'string-equal))))))

(defun add-menu-item-to-command-table (command-table
				       string type value
				       &key documentation (after :end)
				       keystroke text-style (errorp t))
  (let* ((table (find-command-table command-table))
	 (item (find-menu-item string command-table :errorp nil)))
    (with-slots (menu) table
      (cond ((and errorp item) (error 'command-already-present))
	    (t (when item
		 (remove-menu-item-from-command-table command-table string))
	       (let ((new-item (make-instance 'menu-item
				 :name string :type type :value value
				 :keystroke keystroke
				 :documentation documentation
				 :text-style text-style)))
		 (case after
		   (:start (push new-item menu))
		   ((:end nil) (setf menu (nconc menu (list new-item))))
		   (:sort (setf menu (sort (cons new-item menu)
					   #'string-lessp
					   :key #'command-menu-item-name)))
		   (t (push new-item
			    (cdr (member after menu
					 :key #'command-menu-item-name
					 :test #'string-equal))))))
	       (when keystroke
		 #+ignore(add-keystroke-to-command-table command-table keystroke
						 type value)))))))

(defun map-over-command-table-menu-items (function command-table)
  (mapc #'(lambda (item)
	    (with-slots (name keystroke) item
	      (funcall function name keystroke item)))
	(slot-value (find-command-table command-table) 'menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commands

(defparameter *command-parser-table* (make-hash-table)
  "Mapping from command names to argument parsing functions.")

(defvar *unsupplied-argument-maker* (cons nil nil))

(defvar *command-name-delimiters* '(command-delimiter))

(defvar *command-argument-delimiters* '(command-delimiter))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-argument-accept-fun (name required-args keyword-args)
    (declare (ignore keyword-args))
    (let ((stream-var (gensym "STREAM"))
	  (required-arg-names (mapcar #'car required-args)))
      (flet ((make-accept (arg-clause)
	       (let ((arg (car arg-clause))
		     (ptype (cadr arg-clause))
		     (key-args (cddr arg-clause))
		     (accept-keys '(:default :default-type :display-default
				    :prompt :documentation)))
		 `(setq ,arg (accept ,ptype
				     :stream ,stream-var
				     ,@(mapcan
					#'(lambda (key)
					    (let ((val (getf key-args
							     key
							     stream-var)))
					      (unless (eq val stream-var)
						`(,key ,val))))
					accept-keys))))))
	`(defun ,name (,stream-var)
	   (let ,(mapcar #'(lambda (arg)
			     `(,arg *unsupplied-argument-maker*))
		  required-arg-names)
	     (block activated
	       (let ((gesture (read-gesture :stream ,stream-var
					    :timeout 0
					    :peek-p t)))
		 (cond ((and gesture (activation-gesture-p gesture))
			(return-from activated nil))
		       (gesture
			(unread-gesture gesture :stream ,stream-var)))
		 ,@(mapcan #'(lambda (arg)
			       `(,(make-accept arg)
				 (setq gesture (read-gesture :stream
							     ,stream-var))
				 (when (or (null gesture)
					   (activation-gesture-p gesture))
				   (return-from activated nil))
				 (unless (delimiter-gesture-p gesture)
				   (unread-gesture gesture
						   :stream ,stream-var))))
			   required-args)))
	     (list ,@required-arg-names)))))))

;;; XXX temporarily make name default t so we can debug command line processing
;;; with some interesting examples
(defmacro define-command (name-and-options args &body body)
  (unless (listp name-and-options)
    (setq name-and-options (list name-and-options)))
  (destructuring-bind (func &key command-table (name t) menu keystroke)
      name-and-options
    (multiple-value-bind (required-args keyword-args)
	(loop for arg-tail on args
	      for (arg) = arg-tail
	      unless (eq arg '&key)
	      collect arg into required
	      finally (return (values required (cdr arg-tail))))
      (let* ((command-func-args
	      `(,@(mapcar #'car required-args)
		,@(and
		   keyword-args
		   `(&key ,@(mapcar #'(lambda (arg-clause)
					(destructuring-bind (arg-name ptype
							     &key default
							     &allow-other-keys)
					    arg-clause
					  (declare (ignore ptype))
					  `(,arg-name ,default)))
				    keyword-args)))))
	     (accept-fun-name (gentemp (format nil "~A%ACCEPTOR%"
					       (symbol-name func)
					       (symbol-package func)) )))
	
	`(progn
	  (defun ,func ,command-func-args
	    ,@body)
	  ,(if command-table
	       `(add-command-to-command-table ',func ',command-table
		 :name ,name :menu ,menu
		 :keystroke ,keystroke :errorp nil))
	  ,(make-argument-accept-fun accept-fun-name
				     required-args
				     keyword-args)
	  (setf (gethash ',func *command-parser-table*) #',accept-fun-name)
	  ',func)))))

(define-presentation-type command-name
    (&key (command-table (frame-command-table *application-frame*))))

(define-presentation-method presentation-typep (object (type command-name))
  (command-accessible-in-command-table-p object command-table))

(define-presentation-method present (object (type command-name)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (princ (command-line-name-for-command object command-table :errorp :create)
	 stream))


(define-presentation-method accept ((type command-name) stream
				    (view textual-view)
				    &key (default nil defaultp) default-type)
  (flet ((generator (string suggester)
	   (map-over-command-table-names suggester command-table)))
    (multiple-value-bind (object success string)
	(complete-input stream
			#'(lambda (so-far mode)
			    (complete-from-generator so-far
						     #'generator
						     '(#\space)
						     :action mode))
			:partial-completers '(#\space))
      (if success
	  (values object type)
	  (simple-parse-error "No command named ~S" string)))))

(defun command-line-command-parser (command-table stream)
  (let ((command-name nil)
	(command-args nil))
    (with-delimiter-gestures (*command-name-delimiters* :override t)
      (setq command-name (accept `(command-name :command-table ,command-table)
				 :stream stream :prompt nil))
      (let ((delimiter (read-gesture :stream stream :peek-p t)))
	;; Let argument parsing function see activation gestures.
	(when (and delimiter (delimiter-gesture-p delimiter))
	  (read-gesture :stream stream))))
    (with-delimiter-gestures (*command-argument-delimiters* :override t)
      (setq command-args (funcall (gethash command-name *command-parser-table*)
				  stream)))
    (cons command-name command-args)))

(defun command-line-command-unparser (command-table stream command)
  (write-string (command-line-name-for-command (car command) command-table
					       :errorp :create)
		stream)
  (with-delimiter-gestures (*command-argument-delimiters* :override t)
    (loop for arg in (cdr command)
	do (progn
	     (write-char #\space stream)
	     (write-token (present-to-string arg (presentation-type-of arg)
					     :acceptably t)
			  stream)))))

(defparameter *command-parser* #'command-line-command-parser)

(defvar *command-unparser* #'command-line-command-unparser)

(defvar *partial-command-parser* nil)

(define-presentation-type command
    (&key (command-table (frame-command-table *application-frame*))))

(define-presentation-method presentation-typep (object (type command))
  (and (consp object)
       (presentation-typep (car object)
			   `(command-name :command-table ,command-table))))

(define-presentation-method present (object (type command)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (funcall *command-unparser* command-table stream object))

(define-presentation-method accept ((type command) stream
				    (view textual-view)
				    &key (default nil defaultp) default-type)
  (let ((command (funcall *command-parser* command-table stream)))
    (cond ((and (null command) defaultp)
	   (values default default-type))
	  ((null command)
	   (simple-parse-error "Empty command"))
	  (t (values command type)))))

(defun command-name (command)
  (first command))

(defun command-arguments (command)
  (rest command))

(defun read-command (command-table
		     &key (stream *standard-input*)
			  (command-parser *command-parser*)
			  (command-unparser *command-unparser*)
			  (partial-command-parser *partial-command-parser*)
			  use-keystrokes)
  (declare (ignore use-keystrokes))
  (let ((*command-parser* command-parser)
	(*command-unparser* command-unparser)
	(*partial-command-parser* partial-command-parser))
    (handler-case
	(let ((command (accept `(command :command-table ,command-table)
			       :stream stream
			       :prompt nil)))
	  (if (member *unsupplied-argument-maker* command)
	    (progn
	      (beep)
	      (format *query-io* "~&Argument ~D not supplied.~&"
		      (position *unsupplied-argument-maker* command))
	      nil)
	    command))
      ((or simple-parse-error input-not-of-required-type)  (c)
       (beep)
       (fresh-line *query-io*)
       (princ c *query-io*)
       (terpri *query-io*)
       nil))))

;;; Global help command

(define-command (com-help :command-table global-command-table :name "Help")
    ((kind '(completion (("Keyboard" keyboard) ("Commands" commands))
	                :value-key cadr)
	   :prompt "with"
	   :default 'keyboard))
  (if (eq kind 'keyboard)
      (format *query-io* "Input editor commands are like Emacs.~%")
      (let ((command-table (frame-command-table *application-frame*))
	    (command-names nil))
	(map-over-command-table-names #'(lambda (name command)
					  (declare (ignore command))
					  (push name command-names))
				      command-table)
	(setf command-names (sort command-names #'string-lessp))
	(format *query-io* "Available commands:~%~{~A~%~}" command-names))))

