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

(in-package :clim-internals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Command tables

(defgeneric command-table-name (command-table))
(defgeneric command-table-inherit-from (command-table))

;;; Container for info about a command

(defclass command-item ()
  ((command-name :initarg :command-name :reader command-item-name
		      :initarg nil)
   (command-line-name :initarg :command-line-name :reader command-line-name)))

(defmethod print-object ((obj command-item) stream)
  (print-unreadable-object (obj stream :identity t :type t)
    (cond ((slot-boundp obj 'command-line-name)
	   (format stream "~A" (command-line-name obj)))
	  ((slot-boundp obj 'command-name)
	   (format stream "~S" (command-item-name obj)))
	  (t nil))))

;;; According to the specification, command menu items are stored as
;;; lists.  This way seems better, and I hope nothing will break.
(defclass menu-item (command-item)
  ((menu-name :reader command-menu-item-name :initarg :menu-name)
   (type :initarg :type :reader command-menu-item-type)
   (value :initarg :value :reader command-menu-item-value)
   (documentation :initarg :documentation)
   (text-style :initarg :text-style :initform nil)
   (keystroke :initarg :keystroke)))

(defmethod print-object ((item menu-item) stream)
  (print-unreadable-object (item stream :identity t :type t)
    (when (slot-boundp item 'menu-name)
      (format stream "~S" (command-menu-item-name item)))
    (when (slot-boundp item 'keystroke)
      (format stream "~:[~; ~]keystroke ~A"
	      (slot-boundp item 'menu-name)
	      (slot-value item 'keystroke)))))

(defun command-menu-item-options (menu-item)
  (with-slots (documentation text-style) menu-item
    (list ':documentation documentation ':text-style text-style)))

(define-protocol-class command-table ()
  ((name :initarg :name :reader command-table-name)
   (inherit-from :initarg :inherit-from
		 :initform '()
		 :reader command-table-inherit-from)
   (commands  :accessor commands :initarg :commands
	     :initform (make-hash-table :test #'eq))
   (command-line-names :accessor command-line-names
		       :initform (make-hash-table :test #'equal))
   (presentation-translators :reader presentation-translators
			     :initform (make-instance 'translator-table))
   (menu :initarg :menu :initform '())
   (keystroke-accelerators :initform nil)
   (keystroke-items :initform nil)))


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
			            (destructuring-bind (name type value
							 &rest args)
					item
				      (apply #'make-menu-item name type value
					     args)))
		                menu))))
        (when name
          (setf (gethash name *command-tables*) result))
        result)))

(make-command-table 'global-command-table)
(make-command-table 'user-command-table :inherit-from '(global-command-table))

(defmacro define-command-table (name &key 
				(inherit-from '(global-command-table))
				menu)
  `(let ((old-table (gethash ',name *command-tables* nil)))
     (if old-table
	 (with-slots (inherit-from menu) old-table
	   (setq inherit-from ',inherit-from
		 menu ',menu)
	   old-table)
	 (make-command-table ',name
			     :inherit-from ',inherit-from
			     :menu ',menu
			     :errorp nil))))

(defun command-name-from-symbol (symbol)
  (let ((name (symbol-name symbol)))
    (string-capitalize
     (substitute
      #\Space #\-
      (subseq name (string/= "COM-" name))))))

(defun remove-command-from-command-table (command-name
					  command-table
					  &key (errorp t))
  (let* ((table (find-command-table command-table))
	 (item (gethash command-name (commands table))))
    (if (null item)
	(when errorp
	  (error 'command-not-present))
	(progn 
	  (when (typep item 'menu-item)
	    (remove-menu-item-from-command-table table
						 (command-menu-item-name item)
						 :errorp nil)
	    
	    (when (command-item-name item)
	      (remhash (command-item-name item) (command-line-names table)))
	    (remhash command-name (commands table)))))))

(defun add-command-to-command-table (command-name
				     command-table
				     &key name menu keystroke (errorp t)
				     (menu-command (and menu
							`(,command-name))))
  
  (let ((table (find-command-table command-table))
	(name (cond ((stringp name)
		     name)
		    (name
		     (command-name-from-symbol command-name))
		    (t nil))))
    (multiple-value-bind (menu-name menu-options)
	(cond ((null menu)
	       nil)
	      ((stringp menu)
	       menu)
	      ((eq menu t)
	       name)
	      ((consp menu)
	       (values (car menu) (cdr menu))))
      (let* ((item (if menu
		       (apply #'make-menu-item
			      menu-name :command menu-command
			      :command-name command-name
			      :command-line-name name
			      `(,@(and keystroke `(:keystroke ,keystroke))
				,@menu-options))
		       (make-instance 'command-item
				      :command-name command-name
				      :command-line-name name)))
	     (after (getf menu-options :after)))
	(when (and errorp (gethash command-name (commands command-table)))
	  (error 'command-already-present))
	(remove-command-from-command-table command-name command-table
					   :errorp nil)
	(setf (gethash command-name (commands table)) item)
	(when name
	  (setf (gethash name (command-line-names table)) command-name))
	(when menu
	  (%add-menu-item table item after))))))
						  

(defun apply-with-command-table-inheritance (fun command-table)
  (funcall fun command-table)
  (mapc #'(lambda (inherited-command-table)
	    (apply-with-command-table-inheritance
	     fun (find-command-table inherited-command-table)))
	(command-table-inherit-from command-table)))

;;; do-command-table-inheritance has been shipped off to utils.lisp.

(defun map-over-command-table-commands (function command-table
					&key (inherited t))
  (let ((command-table (find-command-table command-table)))
    (flet ((map-func (table)
	     (maphash #'(lambda (key val)
			  (declare (ignore val))
			  (funcall function key))
		      (slot-value table 'commands))))
      (if inherited
	  (apply-with-command-table-inheritance #'map-func command-table)
	  (map-func command-table)))))

(defun map-over-command-table-names (function command-table &key (inherited t))
  (let ((command-table (find-command-table command-table)))
    (flet ((map-func (table)
	     (maphash function (slot-value table 'command-line-names))))
      (if inherited
	  (apply-with-command-table-inheritance #'map-func command-table)
	  (map-func command-table)))))

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

(defun command-line-name-for-command (command-name command-table
				      &key (errorp t))
  (block exit				; save typing
    (do-command-table-inheritance (table command-table)
      (let* ((command-item (gethash command-name (slot-value table 'commands)))
	     (command-line-name (and command-item
				     (command-line-name command-item))))
	(cond ((stringp command-line-name)
	       (return-from exit command-line-name))
	      ((eq errorp :create)
	       (return-from exit (command-name-from-symbol command-name)))
	      (errorp
	       (error 'command-not-accessible))
	      (t nil))))
    nil))


(defun find-menu-item (menu-name command-table &key (errorp t))
  (let* ((table (find-command-table command-table))
	 (mem (member menu-name (slot-value table 'menu)
		      :key #'command-menu-item-name :test #'string-equal)))
    (cond (mem (values (car mem) command-table))
	  (errorp (error 'command-not-accessible))
	  (t nil))))

(defun remove-menu-item-from-command-table (command-table string
					    &key (errorp t))
  (let ((table (find-command-table command-table))
	(item (find-menu-item string command-table :errorp nil)))
    (with-slots (menu) table
      (if (and errorp (not item))
	  (error 'command-not-present)
	  (setf menu (delete string menu
			     :key #'command-menu-item-name
			     :test #'string-equal))))))

(defun make-menu-item (name type value
		       &key (documentation nil documentationp)
		       (keystroke nil keystrokep)
		       (text-style nil text-style-p)
		       (command-name nil command-name-p)
		       (command-line-name nil command-line-name-p)
		       &allow-other-keys)
  ;; v-- this may be wrong, we do this to allow
  ;; text-style to contain make-text-style calls
  ;; so we use a very limited evaluator - FIXME
  (when (and (consp text-style)
	   (eq (first text-style) 'make-text-style))
    (setq text-style (apply #'make-text-style (rest text-style))))
  (apply #'make-instance 'menu-item
	 :menu-name name :type type :value value
	 `(,@(and documentationp `(:documentation ,documentation))
	   ,@(and keystrokep `(:keystroke ,keystroke))
	   ,@(and text-style-p `(:text-style ,text-style))
	   ,@(and command-name-p `(:command-name ,command-name))
	   ,@(and command-line-name-p
		  `(:command-line-name ,command-line-name)))))

(defun %add-menu-item (command-table item after)
  (with-slots (menu)
      command-table
    (case after
      (:start (push item menu))
      ((:end nil) (setf menu (nconc menu (list item))))
      (:sort (setf menu (sort (cons item menu)
			      #'string-lessp
			      :key #'command-menu-item-name)))
      (t (push item
	       (cdr (member after menu
			    :key #'command-menu-item-name
			    :test #'string-equal))))))
  (when (and (slot-boundp item 'keystroke)
	      (slot-value item 'keystroke))
    (%add-keystroke-item command-table (slot-value item 'keystroke) item nil)))


(defun add-menu-item-to-command-table (command-table
				       string type value
				       &rest args
				       &key documentation (after :end)
				       keystroke text-style (errorp t))
  (declare (ignore documentation keystroke text-style))
  (let* ((table (find-command-table command-table))
	 (old-item (find-menu-item string command-table :errorp nil)))
    (cond ((and errorp old-item)
	   (error 'command-already-present))
	  (old-item
	   (remove-menu-item-from-command-table command-table string))
	  (t nil))
    (%add-menu-item table
		    (apply #'make-menu-item string type value args)
		    after)))

(defun map-over-command-table-menu-items (function command-table)
  (mapc #'(lambda (item)
	    (with-slots (name keystroke) item
	      (funcall function name keystroke item)))
	(slot-value (find-command-table command-table) 'menu)))

(defun %add-keystroke-item (command-table gesture item errorp)
  (with-slots (keystroke-accelerators keystroke-items)
      command-table
    (let ((in-table (position gesture keystroke-accelerators :test #'equal)))
      (when errorp
	(error 'command-already-present))
      (if in-table
	  (setf (nth in-table keystroke-items) item)
	  (progn
	    (push gesture keystroke-accelerators)
	    (push item keystroke-items))))))

(defun add-keystroke-to-command-table (command-table gesture type value
				       &key documentation (errorp t))
  (let ((command-table (find-command-table command-table)))
    (%add-keystroke-item command-table gesture
			 (make-instance 'menu-item
					:type type :value value
					:keystroke gesture
					:documentation documentation)
			 errorp)))

(defun remove-keystroke-from-command-table (command-table gesture
					    &key (errorp t))
  (let ((command-table (find-command-table command-table)))
    (with-slots (keystroke-accelerators keystroke-items)
	command-table
      (let ((in-table (position gesture keystroke-accelerators)))
	(if in-table
	    (if (zerop in-table)
		(setq keystroke-accelerators (cdr keystroke-accelerators)
		      keystroke-items (cdr keystroke-items))
		(let ((accel-tail (nthcdr (1- in-table)
					  keystroke-accelerators))
		      (items-tail (nthcdr (1- in-table) keystroke-items)))
		  (setf (cdr accel-tail) (cddr accel-tail))
		  (setf (cdr items-tail) (cddr items-tail))))
	    (when errorp
	      (error 'command-not-present))))))
  nil)

(defun map-over-command-table-keystrokes (function command-table)
  (let ((command-table (find-command-table command-table)))
    (with-slots (keystroke-accelerators keystroke-items)
	command-table
      (loop for gesture in keystroke-accelerators
	    for item in keystroke-items
	    do (funcall function
			(command-menu-item-name item)
			gesture
			item)))))

(defun find-keystroke-item (gesture command-table
			    &key (test #'event-matches-gesture-name-p)
			    (errorp t))
  (let ((command-table (find-command-table command-table)))
    (loop for keystroke in (slot-value command-table 'keystroke-accelerators)
	  for item in (slot-value command-table 'keystroke-items)
	  if (funcall test gesture keystroke)
	  do (return-from find-keystroke-item (values item command-table)))
    (if errorp
	(error 'command-not-present)
	nil)))

(defun lookup-keystroke-item (gesture command-table
			      &key (test #'event-matches-gesture-name-p)
			      (errorp t))
  (let ((command-table (find-command-table command-table)))
    (multiple-value-bind (item table)
	(find-keystroke-item gesture command-table :test test :errorp nil)
      (when table
	(return-from lookup-keystroke-item (values item table)))
      (map-over-command-table-menu-items
       #'(lambda (name keystroke item)
	   (declare (ignore name keystroke))
	   (when (eq (command-menu-item-type item) :menu)
	     (multiple-value-bind (sub-item sub-command-table)
		 (lookup-keystroke-item gesture
					(command-menu-item-value item)
					:test test
					:errorp nil)
	       (when sub-command-table
		 (return-from lookup-keystroke-item
		   (values sub-item sub-command-table))))))
       command-table))
    (if errorp
	(error 'command-not-present)
	nil)))

;;; XXX The spec says that GESTURE may be a gesture name, but also that the
;;; default test is event-matches-gesture-name-p.  Uh...

(defun lookup-keystroke-command-item (gesture command-table
				      &key test (numeric-arg 1))
  (let ((item (lookup-keystroke-item
	       gesture command-table
	       :test (or test
			 #'(lambda (gesture gesture-name)
			     (or (equal gesture gesture-name)
				 (event-matches-gesture-name-p
				  gesture
				  gesture-name)))))))
    (if item
	(let* ((value (command-menu-item-value item))
	       (command (case (command-menu-item-type item)
			 (:command
			  value)
			 (:function
			  (funcall value gesture numeric-arg))
			 ;; XXX What about the :menu case?
			 (otherwise nil))))
	  (if command
	      (substitute-numeric-argument-marker command numeric-arg)
	      gesture))
	gesture)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commands

(defparameter *command-parser-table* (make-hash-table)
  "Mapping from command names to argument parsing functions.")


(defvar *unsupplied-argument-marker* (cons nil nil))

(defvar *command-name-delimiters* '(command-delimiter))

(defvar *command-argument-delimiters* '(command-delimiter))

(defvar *numeric-argument-marker* (cons nil nil))

(defun accept-form-for-argument (stream arg)
  (let ((accept-keys '(:default :default-type :display-default
		       :prompt :documentation)))
    (destructuring-bind (name ptype &rest key-args
			 &key (mentioned-default nil mentioned-default-p)
			 &allow-other-keys)
	arg
      (declare (ignore name))
      `(accept ,ptype :stream ,stream
	       ,@(loop for (key val) on key-args by #'cddr
		       when (member key accept-keys)
		       append `(,key ,val) into args
		       finally (return (if mentioned-default-p
					   `(:default ,mentioned-default
					     ,@args)
					   args)))))))

(defun make-key-acceptors (stream keyword-args key-results)
  ;; We don't use the name as a variable, and we do want a symbol in the
  ;; keyword package.
  (when (null keyword-args)
    (return-from make-key-acceptors nil))
  (setq keyword-args (mapcar #'(lambda (arg)
				 (cons (intern (symbol-name (car arg))
					       :keyword)
				       (cdr arg)))
			     keyword-args))
  (let ((key-possibilities (gensym "KEY-POSSIBILITIES"))
	(member-ptype (gensym "MEMBER-PTYPE"))
	(key-result (gensym "KEY-RESULT"))
	(val-result (gensym "VAL-RESULT")))
    `(let ((,key-possibilities nil))
       ,@(mapcar #'(lambda (key-arg)
		     (destructuring-bind (name ptype
					  &key (when t) &allow-other-keys)
			 key-arg
		       (declare (ignore ptype))
		       `(when ,when
			  (push ,name ,key-possibilities))))
		 keyword-args)
       (setq ,key-possibilities (nreverse ,key-possibilities))
       (when ,key-possibilities
	 (let ((,member-ptype (cons 'member ,key-possibilities)))
	   (loop
	     (let* ((,key-result (accept ,member-ptype
					 :stream ,stream
					 :prompt "keywords"))
		    (,val-result
		     (case ,key-result
		       ,@(mapcar
			  #'(lambda (key-arg)
			      `(,(car key-arg)
				,(accept-form-for-argument stream
							   key-arg)))
			  keyword-args))))
	       (setq ,key-results (list* ,key-result
					 ,val-result
					 ,key-results)))	     
	     (eat-delimiter-or-activator))))
       
       ,key-results)))

(defun make-argument-accept-fun (name required-args keyword-args)
  (let ((stream-var (gensym "STREAM"))
	(required-arg-names (mapcar #'car required-args))
	(key-results (gensym "KEY-RESULTS")))
    `(defun ,name (,stream-var)
       (let (,@(mapcar #'(lambda (arg)
			   `(,arg *unsupplied-argument-marker*))
		       required-arg-names)
	       (,key-results nil))
	 (block activated
	   (flet ((eat-delimiter-or-activator ()
		    (let ((gesture (read-gesture :stream ,stream-var)))
		      (when (or (null gesture)
				(activation-gesture-p gesture))
			(return-from activated nil))
		      (unless (delimiter-gesture-p gesture)
			(unread-gesture gesture
					:stream ,stream-var)))))
             (declare (ignorable (function eat-delimiter-or-activator)))
	     (let ((gesture (read-gesture :stream ,stream-var
					  :timeout 0
					  :peek-p t)))
	       (cond ((and gesture (activation-gesture-p gesture))
		      (return-from activated nil)))
	       ,@(mapcan #'(lambda (arg)
			     (copy-list
			      `((setq ,(car arg)
				 ,(accept-form-for-argument stream-var
							    arg))
				(eat-delimiter-or-activator))))
			 required-args)
	       ,(make-key-acceptors stream-var keyword-args key-results))))
	 (list* ,@required-arg-names ,key-results)))))

(defun make-command-translators (command-name command-table args)
  "Helper function to create command presentation translators for a command."
  (loop with readable-command-name = (command-name-from-symbol command-name) ; XXX or :NAME
        for arg in args
	for arg-index from 0
	append (when (getf (cddr arg) :gesture)
		 (destructuring-bind (name ptype
				      &key gesture &allow-other-keys)
		     arg
		   (let ((command-args (loop for a in args
					     for i from 0
					     if (eql i arg-index)
					       collect 'object
					     else
					       collect (getf (cddr a) :default)
					     end))
			 (translator-name (intern (format nil
							  ".~A-ARG~D."
							  command-name
							  arg-index)
						  (symbol-package name))))
		     (multiple-value-bind (gesture translator-options)
			 (if (listp gesture)
			     (values (car gesture) (cdr gesture))
			     (values gesture nil))
                       (destructuring-bind (&key (documentation
                                                  `((object stream)
                                                    (orf stream *standard-output*)
                                                    (format stream "~A "
                                                            ,readable-command-name)
                                                    (present object (presentation-type-of object) ; type?
                                                             :stream stream
                                                             :acceptably nil
                                                             :sensitive nil))
                                                  documentationp)
                                            &allow-other-keys)
                           translator-options
		       `(define-presentation-to-command-translator
			    ,translator-name
			    (,(eval ptype) ,command-name ,command-table
			     :gesture ,gesture
                             ,@(unless documentationp `(:documentation ,documentation))
			     ,@translator-options)
			  (object)
			  (list ,@command-args)))))))))

;;; XXX temporarily make name default to t so we can debug command line
;;; processing with some interesting examples
(defmacro define-command (name-and-options args &body body)
  (unless (listp name-and-options)
    (setq name-and-options (list name-and-options)))
  (destructuring-bind (func &key command-table (name t) menu keystroke)
      name-and-options
    (multiple-value-bind (required-args keyword-args)
	(loop for arg-tail on args
	      for (arg) = arg-tail
	      until (eq arg '&key)
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
		 :name ,name :menu ',menu
		 :keystroke ',keystroke :errorp nil
		 ,@(and menu
			`(:menu-command
			  (list ',func
			        ,@(make-list (length required-args)
					     :initial-element
					     '*unsupplied-argument-marker*))))))
	  ,(make-argument-accept-fun accept-fun-name
				     required-args
				     keyword-args)
	  ,(and command-table
		(make-command-translators func command-table required-args))
	  (setf (gethash ',func *command-parser-table*) #',accept-fun-name)
	  ',func)))))

;;; Note that command table inheritance is the opposite of Common Lisp
;;; subclassing / subtyping: the inheriting table defines a superset
;;; of the commands of its ancestor, so therefore it's command
;;; presentation type is a supertype of its ancestor's!
(defun command-table-inherits-from-p (command-table super-table)
  (let ((command-table (find-command-table command-table))
	(super-table (find-command-table super-table)))
    (do-command-table-inheritance (table command-table)
      (when (eq table super-table)
	(return-from command-table-inherits-from-p (values t t))))
    (values nil t)))

(define-presentation-type command-name
    (&key (command-table (frame-command-table *application-frame*)))
  :inherit-from t)

(define-presentation-method presentation-typep (object (type command-name))
  (command-accessible-in-command-table-p object command-table))

(define-presentation-method presentation-subtypep ((type command-name)
						   maybe-supertype)
  (with-presentation-type-parameters (command-name maybe-supertype)
    (let ((super-table command-table))
      (with-presentation-type-parameters (command-name type)
	(command-table-inherits-from-p super-table command-table)))))

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
					     :acceptably nil)
			  stream)))))

;;; Placeholder; yuck.

(defun command-line-read-remaining-arguments-for-partial-command
    (command-table stream partial-command start-position)
  (declare (ignore command-table stream start-position))
  partial-command)

(defparameter *command-parser* #'command-line-command-parser)

(defvar *command-unparser* #'command-line-command-unparser)

(defvar *partial-command-parser*
  #'command-line-read-remaining-arguments-for-partial-command)

(define-presentation-type command
    (&key (command-table (frame-command-table *application-frame*)))
  :inherit-from t)

(define-presentation-method presentation-typep (object (type command))
  (and (consp object)
       (presentation-typep (car object)
			   `(command-name :command-table ,command-table))))

(define-presentation-method presentation-subtypep ((type command)
						   maybe-supertype)
  (with-presentation-type-parameters (command maybe-supertype)
    (let ((super-table command-table))
      (with-presentation-type-parameters (command type)
	(command-table-inherits-from-p super-table command-table)))))

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
    #+nil
    (progn
      (format *debug-io* "~&; Command accepted: ~S.~%" command)
      (finish-output *debug-io*))
    (cond ((and (null command) defaultp)
	   (values default default-type))
	  ((null command)
	   (simple-parse-error "Empty command"))
          ((partial-command-p command)
           (funcall *partial-command-parser*
            command-table stream command
            (position *unsupplied-argument-marker* command)))
	  (t (values command type)))))

(defmacro define-presentation-to-command-translator
    (name (from-type command-name command-table &key
	   (gesture :select)
	   (tester 'default-translator-tester testerp)
	   (documentation nil documentationp)
	   (pointer-documentation (command-name-from-symbol command-name))
	   (menu t)
	   (priority 0)
	   (echo t))
     arglist
     &body body)
  (let ((command-args (gensym "COMMAND-ARGS")))
    `(define-presentation-translator ,name
	 (,from-type (command :command-table ,command-table) ,command-table
		     :gesture ,gesture
		     :tester ,tester
		     :tester-definitive t
		     ,@(and documentationp `(:documentation ,documentation))
		     :pointer-documentation ,pointer-documentation
		     :menu ,menu
		     :priority ,priority)
       ,arglist
       (let ((,command-args (progn
			      ,@body)))
	 (values (cons ',command-name ,command-args)
		 '(command :command-table ,command-table)
		 '(:echo ,echo))))))

(defun command-name (command)
  (first command))

(defun command-arguments (command)
  (rest command))

(defun partial-command-p (command)
  (member *unsupplied-argument-marker* command))

(defmacro with-command-table-keystrokes ((keystroke-var command-table)
					 &body body)
  (with-gensyms (table)
    `(let* ((,table (find-command-table ,command-table))
	    (,keystroke-var (slot-value ,table 'keystroke-accelerators)))
       ,@body)))

(defun read-command (command-table
		     &key (stream *standard-input*)
			  (command-parser *command-parser*)
			  (command-unparser *command-unparser*)
			  (partial-command-parser *partial-command-parser*)
			  use-keystrokes)
  (let ((*command-parser* command-parser)
	(*command-unparser* command-unparser)
	(*partial-command-parser* partial-command-parser))
    (if use-keystrokes
	(let ((stroke-result
	       (with-command-table-keystrokes (strokes command-table)
		 (read-command-using-keystrokes command-table
						strokes
						:stream stream))))
	  (if (consp stroke-result)
	      stroke-result
	      nil))
	(handler-case
	    (let ((command (accept `(command :command-table ,command-table)
				   :stream stream
				   :prompt nil)))
	      (if (partial-command-p command)
		  (progn
		    (beep)
		    (format *query-io* "~&Argument ~D not supplied.~&"
			    (position *unsupplied-argument-marker* command))
		    nil)
		  command))
	  ((or simple-parse-error input-not-of-required-type)  (c)
	    (beep)
	    (fresh-line *query-io*)
	    (princ c *query-io*)
	    (terpri *query-io*)
	    nil)))))

(defun read-command-using-keystrokes (command-table keystrokes
				      &key (stream *standard-input*)
				      (command-parser *command-parser*)
				      (command-unparser *command-unparser*)
				      (partial-command-parser
				       *partial-command-parser*))
  (let ((*command-parser* command-parser)
	(*command-unparser* command-unparser)
	(*partial-command-parser* partial-command-parser)
	(*accelerator-gestures* keystrokes))
    (handler-case (read-command command-table :stream stream)
      (accelerator-gesture (c)
	(lookup-keystroke-command-item (accelerator-gesture-event c)
				       command-table)))))

(defun substitute-numeric-argument-marker (command numeric-arg)
  (substitute numeric-arg *numeric-argument-marker* command))
