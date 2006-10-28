;;; -*- Syntax: Common-lisp; Package: DWIM-*-
#|
Copyright (c) 1987-1993 by BBN Systems and Technologies,
A Division of Bolt, Beranek and Newman Inc.
All rights reserved.

Permission to use, copy, modify and distribute this software and its
documentation is hereby granted without fee, provided that the above
copyright notice of BBN Systems and Technologies, this paragraph and the
one following appear in all copies and in supporting documentation, and
that the name Bolt Beranek and Newman Inc. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission. Any distribution of this software or derivative
works must comply with all applicable United States export control laws.

BBN makes no representation about the suitability of this software for any
purposes.  It is provided "AS IS", without express or implied warranties
including (but not limited to) all implied warranties of merchantability
and fitness for a particular purpose, and notwithstanding any other
provision contained herein.  In no event shall BBN be liable for any
special, indirect or consequential damages whatsoever resulting from loss
of use, data or profits, whether in an action of contract, negligence or
other tortuous action, arising out of or in connection with the use or
performance of this software, even if BBN Systems and Technologies is
advised of the possiblity of such damages.
|#

(in-package :dwim)

#+(and (not ansi-cl) (or genera lucid allegro))
(defmacro printing-random-object ((object stream . options) &body body)
  #+genera  `(si:printing-random-object (,object ,stream . ,options) ,@body)
  #+lucid   `(system:printing-random-object (,object ,stream . ,options) ,@body)
  #+allegro `(progn
	       (if *print-readably* (error "Can't print readably"))
	       (write-string "#<" ,stream)
	       (unless ,(null (member :typep options))
		 (format ,stream "~S " (class-name (class-of ,object))))
	       ,@body
	       (when ,(null (member :no-pointer options))
		 (write-char #\space ,stream)
		 (format ,stream " ~X" (excl::pointer-to-fixnum ,object)))
	       (write-string ">" ,stream)))

#+ansi-cl
(defmacro printing-random-object ((object stream &rest options)
				  &body body)
  `(print-unreadable-object (,object ,stream
			     :type ,(member :typep options)
			     :identity ,(not (member :no-pointer options)))
     ,@body))

(defmacro with-stack-list ((var &rest elements) &body body)
  #+Genera
  `(scl:with-stack-list (,var ,@elements) ,@body)
  #+allegro
  `(excl:with-stack-list (,var ,@elements) ,@body)
  #+(and (not genera) (not allegro))
  `(funcall #'(lambda (&rest ,var)
		(declare (dynamic-extent ,var))
		,@body)
	  ,@elements))

(defmacro with-stack-array ((var length &rest options) &body body)
  #+genera
  `(sys:with-stack-array (,var ,length ,@options) ,@body)
  #-genera
  `(let ((,var (make-array ,length ,@options)))
     (declare (dynamic-extent ,var))
     ,@body))

(defmacro store-conditional (place old new)
  "Atomically change the value of PLACE from OLD to NEW."
  #+genera `(si:store-conditional (scl:locf ,place) ,old ,new)
  #-genera (declare (ignore old))
  #-genera `(setf ,place ,new))

(defmacro stack-let (forms &body body)
  #+genera `(si:stack-let ,forms ,@body)
  #-genera
  (labels ((get-vars (let-vars)
	     (mapcar #'(lambda (v) (if (symbolp v) v (car v))) let-vars)))
    `(let ,forms (declare (dynamic-extent ,@(get-vars forms))) ,@body)))

#-(or openmcl-native-threads sb-thread scl)
(defmacro without-interrupts (&body body)
  #FEATURE-CASE
  ((:genera  `(scl::without-interrupts ,@body))
   (:lucid   `(lcl:with-scheduling-inhibited ,@body))
   (:allegro `(excl:without-interrupts ,@body))
   (:mcl     `(ccl:without-interrupts ,@body))))

#+(or openmcl-native-threads sb-thread scl)
(progn
  (defparameter *dwim-giant-lock* (clim-sys:make-lock "dwim giant lock"))
  (defmacro without-interrupts (&body body)
    `(clim-sys:with-lock-held (*dwim-giant-lock*)
       ,@body)))

(defmacro handler-case (form &rest clauses)
  #FEATURE-CASE
  ((:genera `(future-common-lisp:handler-case ,form ,@clauses))
   (:lucid `(lcl:handler-case ,form ,@clauses))
   (:allegro `(lisp:handler-case ,form ,@clauses))
   ((or :mcl :ansi-cl) `(COMMON-LISP:handler-case ,form ,@clauses))))

(defmacro handler-bind (bindings &body forms)
  #FEATURE-CASE
  ((:genera `(future-common-lisp:handler-bind ,bindings ,@forms))
   (:lucid `(lcl:handler-bind ,bindings ,@forms))
   (:allegro `(lisp:handler-bind ,bindings ,@forms))
   ((or :mcl :ansi-cl) `(COMMON-LISP:handler-bind ,bindings ,@forms))
   ))

(defmacro condition-case ((&rest varlist) form &rest clauses)
  #+genera (declare (zwei:indentation 1 4 2 2))
  #+genera `(scl:condition-case ,varlist ,form ,@clauses)
  #-genera
  `(let ,(cdr varlist)
     (handler-case
	 ,form
       ,@(mapcar #'(lambda (cl)
		     `(,(first cl)
		       ,(if (first varlist) (list (first varlist)))
		       ,@(cdr cl)))
	   clauses))))

(defmacro ignore-errors (&body body)
  #FEATURE-CASE
  ((:Allegro `(lisp:ignore-errors ,@body))
   ((or :mcl :ansi-cl) `(COMMON-LISP:ignore-errors ,@body))   
   (:Lucid `(lcl:ignore-errors ,@body))
   (:Xerox `(xcl:ignore-errors ,@body))
   (:Genera `(future-common-lisp:ignore-errors ,@body))))

(defmacro with-simple-restart ((name format-string &rest format-args) &body body)
  #FEATURE-CASE
  ((:allegro `(lisp:with-simple-restart (,name ,format-string ,@format-args) ,@body))
   (:lucid `(lcl:with-simple-restart (,name ,format-string ,@format-args) ,@body))
   (:genera
    `(future-common-lisp:with-simple-restart (,name ,format-string ,@format-args)
       ,@body))
   ((or :mcl :ansi-cl) `(COMMON-LISP:with-simple-restart (,name ,format-string ,@format-args) ,@body))
   ))

(defmacro restart-case (expression &body clauses)
  #FEATURE-CASE
  ((:genera `(future-common-lisp:restart-case ,expression ,@clauses))
   (:lucid `(lcl:restart-case ,expression ,@clauses))
   (:allegro `(lisp:restart-case ,expression ,@clauses))
   ((or :mcl :ansi-cl) `(COMMON-LISP:restart-case ,expression ,@clauses))))

(defun invoke-restart (restart &rest values)
  #FEATURE-CASE
  ((:genera (apply #'future-common-lisp:invoke-restart restart values))
   (:lucid (apply #'lcl:invoke-restart restart values))
   (:allegro (apply #'lisp:invoke-restart restart values))
   ((or :mcl :ansi-cl) (apply #'COMMON-LISP:invoke-restart restart values))))

(defun find-restart (name &optional condition)
  #FEATURE-CASE
  ((:genera (future-common-lisp:find-restart name condition))
   (:lucid (lcl:find-restart name condition))
   (:allegro (lisp:find-restart name condition))
   ((or :mcl :ansi-cl) (COMMON-LISP:find-restart name condition))))

(defmacro make-command-table (name &key inherit-from)
  (when (and (consp inherit-from) (eq (car inherit-from) 'quote))
    ;; dequotify
    (setq inherit-from (second inherit-from)))
  #-clim
  `(cp:make-command-table ',name :if-exists :update-options :INHERIT-FROM ',inherit-from)
  #+clim
  `(clim:define-command-table ,name :inherit-from ,inherit-from))

(defmacro define-command-table (name &key inherit-from)
  `(make-command-table ,name :inherit-from ,inherit-from))

(defun find-command-table (NAME &key (errorp t))
  #FEATURE-CASE
  ((:CLIM-0.9
    (clim:find-command-table NAME :if-does-not-exist (and errorp :error)))
   ((or :clim-2 :clim-1.0)
    (clim:find-command-table NAME :errorp errorp))
   ((not :CLIM) (cp:find-command-table name :if-does-not-exist (and errorp :error)))))

(defmacro command-table-inherit-from (NAME) 
  #FEATURE-CASE
  ((:CLIM `(clim::command-table-inherit-from ,NAME))))

(defun command-pretty-name (string)
  "COM-SHOW-FILE -> 'Show File'"
  (cond ((and (> (length string) 4)
	      (string-equal string "COM-" :end1 4))
	 (command-pretty-name (subseq string 4)))
	(t (dotimes (i (length string))
	     (if (char= (aref string i) #\-)
		 (setf (aref string i) #\space)))
	   (dotimes (i (length string))
	     (cond ((= i 0) (setf (aref string i) (char-upcase (aref string i))))
		   ((char= (aref string (1- i)) #\space)
		    (setf (aref string i) (char-upcase (aref string i))))
		   (t (setf (aref string i) (char-downcase (aref string i))))))
	   string)))

(defmacro define-command ((command-name &key (command-table :global) 
					     keystroke
					     name menu
					     (provide-output-destination-keyword t))
			  arguments &body body)
  #+clim (declare (ignore provide-output-destination-keyword))
  #+genera (declare (zwei:indentation 1 3 2 2))
  #FEATURE-CASE
  (((not :clim)
    `(cp:define-command (,command-name :command-table ,command-table :name ,name
				       :provide-output-destination-keyword
				       ,provide-output-destination-keyword)
	 ,arguments ,@body))
   (:clim-0.9
    ;; define-command doesn't work because it doesn't take care of the hidden frame
    ;; argument.  define-frame-command doesn't work unless the command table is also
    ;; the name of a frame type.  jpm.
    `(progn (ws::define-specializable-command ,command-name (,ws::*frame-parameter* t)
	      ,arguments ,@body)
	    (install-command ,command-table ',command-name ,name)))
   (:clim-1.0
    `(clim:define-command (,command-name :command-table ,(eval command-table)
					 :menu ,menu
					 :name ,(or name
						    (command-pretty-name
						     (copy-seq (string command-name)))))
	 ,arguments ,@body))   
   (clim-2
    `(clim:define-command (,command-name :command-table ,(eval command-table)
					 :keystroke ,keystroke
					 :menu ,menu
					 :name ,(or name
						    (command-pretty-name
						     (copy-seq (string command-name))))
					 #+mcclim
					 :provide-output-destination-keyword
					 #+mcclim
					 ,provide-output-destination-keyword)
	 ,arguments ,@body))))

(defun install-command (command-table command-symbol &optional command-name)
  (or command-name
      (setq command-name (command-pretty-name (copy-seq (string command-symbol)))))
  #FEATURE-CASE
  (((or :clim-1.0 :clim-2)
    (clim:add-command-to-command-table command-symbol command-table
				       :name command-name :errorp nil))
   (:clim-0.9
    (clim:add-command-to-command-table command-name
				       command-symbol command-table))
   ((not :clim)
    (let ((tab (cp:find-command-table command-table :if-does-not-exist nil)))
      (and tab (cp:command-table-delete-command-symbol tab command-symbol
						       :if-does-not-exist nil))	   
      (cp:install-command command-table command-symbol command-name)))))

(defun canonicalize-argument-list (list)
  (remove '&key list))

(defun canonicalize-command-table (command-table)
  (if (symbolp command-table)
      command-table
      (eval command-table)))

#+clim-0.9
(defun canonicalize-documentation (documentation)
  (if (stringp documentation)
      `((stream) (write-string ,documentation stream))
      documentation))

#-clim
(defun canonicalize-documentation (documentation)
  documentation)

#+(or :clim-1.0 :clim-2)
(defun canonicalize-documentation (documentation)
  documentation)


(defmacro define-presentation-to-command-translator
    (name
     (presentation-type
      &key (menu t) gesture documentation (pointer-documentation documentation)
	   command-name tester command-table)
     arguments
     &body body)
  #+genera (declare (zwei:indentation 1 2 3 1))
  (setq documentation (canonicalize-documentation documentation)
	command-table (canonicalize-command-table command-table))
  #FEATURE-CASE
  (((not :clim)
    `(dw:define-presentation-to-command-translator ,name
	 (,presentation-type :documentation ,documentation :tester ,tester
			     :gesture ,gesture :menu ,menu)
       ,arguments
       (cons ',command-name ,@body)))
   (:clim-0.9
    (let ((to-type (if command-table
		       `(command :command-table ,command-table)
		     'command)))
      `(clim:define-presentation-translator
	   ,name
	   (,presentation-type ,to-type
			       :tester ,(if tester
					    (cons (canonicalize-argument-list
						   (car tester))
						  (cdr tester)))
			       :gesture ,gesture :menu ,menu
			       :documentation ,documentation)
	 ,(canonicalize-argument-list arguments)
	 (cons ',command-name (progn ,@body)))))
   ((or :clim-1.0 :clim-2)
    `(clim:define-presentation-to-command-translator
      ,name
      (,presentation-type ,command-name ,command-table
       :tester ,(if tester
		    (cons (canonicalize-argument-list
			   (car tester))
			  (cdr tester)))
       :gesture ,gesture
       :menu ,menu
       :pointer-documentation ,pointer-documentation
       :documentation ,documentation)
      ;; Don't know who's right, but my reading of the spec suggests
      ;; that &key shouldn't be in the argument list. -- moore
      #-mcclim ,arguments
      #+mcclim ,(canonicalize-argument-list arguments)
      ,@body))))

(defmacro define-presentation-translator
    (name
     (from-type to-type
      &key (menu t) (gesture :select) command-table
	   documentation tester do-not-compose)
     arguments
     &body body)
  #+genera (declare (zwei:indentation 1 2 3 1))
  (setq documentation (canonicalize-documentation documentation)
	command-table (canonicalize-command-table command-table))
  #FEATURE-CASE
  (((not :clim)
    `(dw:define-presentation-translator ,name
	 (,from-type ,to-type :documentation ,documentation :tester ,tester
		     :gesture ,gesture :menu ,menu :do-not-compose ,do-not-compose)
       ,arguments
       ,@body))
   (:clim-0.9
    (let ((test tester))
      (when do-not-compose
	(setq test `(,(first test) (values (progn ,@(rest test)) t))))
      `(clim:define-presentation-translator
	   ,name
	   (,from-type ,to-type
		       :tester ,(if test (cons (canonicalize-argument-list (car test))
					       (cdr test)))
		       :gesture ,gesture
		       :menu ,menu
		       :documentation ,documentation)
	 ,arguments
	 ,@body)))
   
   ((or :clim-1.0 :clim-2)
    (let ((test tester))
      `(clim:define-presentation-translator
	   ,name
	   (,from-type ,to-type ,command-table
		       :tester ,(if test (cons (canonicalize-argument-list (car test))
					       (cdr test)))
		       :gesture ,gesture
		       :menu ,menu
		       :tester-definitive ,do-not-compose
		       :documentation ,documentation)
	 ,arguments
	 ,@body)))))

(defmacro define-presentation-action
    (name
     (from-type to-type &key command-table gesture tester documentation (menu t))
     arglist
     &body body)
  ;; This is similar to define-presentation-translator, except that the body of the
  ;; action is not intended to return a value, but should instead side-effect some
  ;; sort of application state.
  ;;
  ;; D. Moon says actions should be used not to side-effect the application state,
  ;; but rather to do something to the display.  It has to make sense while in the
  ;; middle of parsing a command (i.e. expand ellipsis), otherwise it should be a
  ;; presentation-to-command-translator.
  #+genera (declare (zwei:indentation 1 2 3 1))
  (setq documentation (canonicalize-documentation documentation)
	command-table (canonicalize-command-table command-table))
  #FEATURE-CASE
  (((not :clim)
    `(dw:define-presentation-action ,name
	 (,from-type ,to-type :gesture ,gesture :tester ,tester
		     :menu ,menu :documentation ,documentation)
       ,arglist
       ,@body))
   (:clim-0.9
    (progn
      (or (member 'window arglist :test #'string-equal) (push 'window arglist))
      (or (member 'gesture arglist :test #'string-equal) (push 'gesture arglist))
      ;; To prevent the body from getting evaluated in the process of testing the
      ;; applicability of the translator, the tester should return T as the second
      ;; value.
      (cond ((not tester)
	     (setq tester `((presentation) 
			    (values (ci::presentation-matches-type-p
				     presentation ',from-type)
				    t))))
	    (t (setq tester `(,(first tester) (values (progn ,@(rest tester)) t)))))
      (when (and command-table (eql to-type 'command))
	(setq to-type `(command :command-table ,command-table)))
      (let ((g (find 'gesture arglist :test #'string-equal))
	    (w (find 'window arglist :test #'string-equal)))      
	`(clim:define-presentation-translator
	     ,name
	     (,from-type ,to-type :tester ,tester :gesture ,gesture
			 :menu ,menu
			 :documentation ,documentation)
	   ,arglist
	   (when (not (eq ,g :for-menu))
	     ,@body
	     ;; pretty big hammer, but we need to get blips etc out of the input buffer.
	     (clim:stream-clear-input ,w)
	     '(ignore))))))
   ((or :clim-1.0 :clim-2)
    `(clim:define-presentation-action
	 ,name
	 (,from-type ,to-type ,command-table
		     :gesture ,gesture
		     :tester ,(if tester (cons (canonicalize-argument-list (car tester))
					       (cdr tester)))
		     :menu ,menu :documentation ,documentation)
       ,arglist
       ,@body))))

(defmacro define-presentation-type (name arglist
				    &key 
				    parser printer abbreviation-for (no-deftype t)
				    typep describer description accept-values-displayer
				    highlighter
				    INHERIT-FROM)
  (if (null no-deftype)
      (format t "Presentation type ~A, :NO-DEFTYPE option is obsolete and is ignored."
	      name))
  (if (and (consp arglist) (consp (car arglist)))
      (error "Obsolete arglist.  Use (&REST ARGS) rather than ((&REST ARGS))"))
  #FEATURE-CASE
  (((not :clim)
    (progn
      ;; DW needs extra listification of the arglist because it
      ;; distinguishes between data type arguments and formatting arguments.
      (if arglist (setq arglist (list arglist)))
      `(progn
	 #+genera (scl:record-source-file-name ',name 'define-presentation-type)
	 (dw:define-presentation-type ,name ,arglist
	   :description ,description
	   :typep ,typep
	   :describer ,describer
	   ,@(if (not abbreviation-for) `(:no-deftype t))
	   :abbreviation-for ,abbreviation-for
	   :parser ,parser
	   :printer ,printer
	   :choose-displayer ,accept-values-displayer))))
   (:clim-0.9
    `(clim:define-presentation-type ,name ,arglist
       :describer ,(or describer `((stream) (format stream ,description)))
       :abbreviation-for ,abbreviation-for
       :object-validator ,typep
       :supertype ,inherit-from ;; should really check there's only one...
       :parser ,(if parser
		    (let ((args (first parser))
			  (body (rest parser)))
		      (when (not (member '&rest args))
			(setq args (append args '(&rest rest)))
			(push (copy-list '(declare (ignore rest))) body))
		      `(,args ,@body)))
       :printer ,(if printer
		     (let ((args (first printer))
			   (body (rest printer)))
		       (when (not (member '&key args))
			 (setq args (append args '(&key))))
		       (when (not (member '&allow-other-keys args))
			 (setq args (append args '(&allow-other-keys))))
		       `(,args ,@body)))
       :accept-values-displayer ,accept-values-displayer))
   ((or :clim-1.0 :clim-2)
    `(progn
       ;; Methods automatically get lexical access to the presentation arguments.
       ;; TO DO: handle the keywords from :parser and :printer arglists.
       ,(let ((superclasses (cond (INHERIT-FROM
				   (list (eval INHERIT-FROM)))
				  ((find-class name nil)
				   (mapcar #'class-name
					   (#+mcclim
					    clim-mop:class-direct-superclasses
					    #-mcclim
					    class-direct-superclasses
					    (find-class name)))))))
	  (if superclasses
	      (setq superclasses
		(if (cdr superclasses)
		    (cons 'and superclasses)
		  (car superclasses))))
	  (if abbreviation-for
	      `(clim:define-presentation-type-abbreviation ,name ,arglist ,abbreviation-for)
	    `(clim:define-presentation-type ,name ,arglist
	       :description ,description
	       :inherit-from ,(and superclasses `',superclasses))))
       ,(when parser
	  (let ((args (canonicalize-argument-list (first parser)))
		(body (rest parser)))
	    `(clim:define-presentation-method
		 clim:accept
		 ((type ,name) stream (view clim:textual-view) &key)
	       (let ((,(first args) stream))
		 ,@body))))
       ,(when describer
	  (let ((args (canonicalize-argument-list (first describer)))
		(body (rest describer)))
	    `(clim:define-presentation-method
		 clim:describe-presentation-type
		 ((type ,name) stream plural-count)
	       (let ((,(first args) stream))
		 plural-count
		 ,@body))))
       ,(when typep
	  (let ((args (canonicalize-argument-list (first typep)))
		(body (rest typep)))
	    `(clim:define-presentation-method
		 clim:presentation-typep
		 ((,(car args) t) (type ,name))
	       ,@body)))
       ,(when printer
	  (let ((args (canonicalize-argument-list (first printer)))
		(body (rest printer)))
	    `(clim:define-presentation-method
		 clim:present
		 (object (type ,name) stream (view clim:textual-view) &key)
	       (let ((,(first args) object)
		     (,(second args) stream))
		 ,@body))))
       ,(when accept-values-displayer
	  (let* ((arglist (car accept-values-displayer))
		 (stream (first arglist))
		 (default (second arglist))
		 (query-identifier (third arglist)))
	    `(clim:define-presentation-method
		 clim:accept-present-default
		 ((type ,name)
		  ,stream
		  (view #+clim-1.0 clim:dialog-view
			#+clim-2 clim:gadget-dialog-view)
		  ,default default-supplied-p present-p ,query-identifier
		  #+(and clim-2 (not mcclim)) &key)
	       (declare (ignore default-supplied-p present-p))
	       ,@(cdr accept-values-displayer))))
       ,(when highlighter
	  (let ((args (first highlighter)))
	    `(clim:define-presentation-method
		 clim:highlight-presentation
		 ((type ,name) ,@ARGS)
	       ,@(cdr highlighter)))))))
  )

(defmacro with-output-as-presentation
    ((&key stream object (type ''expression)
	   single-box (allow-sensitive-inferiors t)
	   dont-snapshot-variables
	   record-type)
     &body body)
  dont-snapshot-variables allow-sensitive-inferiors
  #FEATURE-CASE
  (((not :clim)
    (progn
      (or record-type (setq record-type ''dw::presentation))
      `(graphics:with-output-as-graphics-presentation
	   (,stream :object ,object :type ,type :single-box ,single-box
		    :allow-sensitive-inferiors ,allow-sensitive-inferiors
		    :dont-snapshot-variables ,dont-snapshot-variables)
	 ,@body)))
   (:clim-0.9
    (progn
      (or record-type (setq record-type ''clim::presentation))
      `(clim:with-output-as-presentation
	   (:stream ,stream :object ,object :type ,type
		    ;; :allow-sensitive-inferiors ,allow-sensitive-inferiors
		    :single-box ,single-box :record-type ,record-type)
	 ,@body)))
   (:clim-1.0
    (progn
      (or record-type (setq record-type ''clim::standard-presentation))
      `(if (clim:extended-input-stream-p ,stream)
	   (clim:with-output-as-presentation
	       (:stream ,stream :object ,object :type ,type
			:allow-sensitive-inferiors ,allow-sensitive-inferiors
			:single-box ,single-box :record-type ,record-type)
	     ,@body)
	 (progn ,@body))))
   (:clim-2
    (or record-type (setq record-type ''clim::standard-presentation))
    `(clim:with-output-as-presentation
	 (,stream ,object ,type
		  :allow-sensitive-inferiors ,allow-sensitive-inferiors
		  :single-box ,single-box :record-type ,record-type)
       ,@body))))

(defmacro with-output-as-graphics-presentation
	  ((&key stream object type single-box (allow-sensitive-inferiors t)
		 dont-snapshot-variables)
	   &body body)
  stream object type single-box allow-sensitive-inferiors
  dont-snapshot-variables body
  (error "WITH-OUTPUT-AS-GRAPHICS-PRESENTATION is not supported. ~
          Use WITH-OUTPUT-AS-PRESENTATION instead"))

(defmacro with-output-truncation ((stream) &body body)
  #FEATURE-CASE
  (((not :clim)
    `(dw:with-output-truncation (,stream :horizontal t :vertical t) ,@body))
   ((or :clim-0.9 :clim-1.0)
    `(clim:with-end-of-line-action
	 (:allow ,stream)
       (clim:with-end-of-page-action (:allow ,stream) ,@body)))
   (:clim-2
    `(clim:with-end-of-line-action
      (,stream :allow)
      (clim:with-end-of-page-action (,stream :allow) ,@body)))))

(defmacro with-output-recording-enabled ((stream &optional (record-p t)) &body body)
  #FEATURE-CASE
  (((or :clim-0.9 :clim-1.0)
    `(clim:with-output-recording-options (,stream :record-p ,record-p :draw-p t) ,@body))
   ((not :clim)
    `(dw:with-output-recording-enabled (,stream ,record-p) ,@body))
   (:clim-2
    `(clim:with-output-recording-options (,stream :record ,record-p :draw t) ,@body))))

(defmacro with-output-recording-disabled ((stream) &body body)
  `(with-output-recording-enabled (,stream nil) ,@body))

(defmacro with-redisplayable-output
	  ((&key stream
		 (unique-id nil unique-id-p)
		 (id-test '#'eq)
		 (cache-value nil cache-value-p)
		 (cache-test '#'eql) copy-cache-value)
	   &body body)
  #+clim `(if (clim:extended-input-stream-p ,stream)
	      (clim:updating-output
	       (,stream ,@(if unique-id-p `(:unique-id ,unique-id))
			:id-test ,id-test
			,@(if cache-value-p `(:cache-value ,cache-value))
			:cache-test ,cache-test
			:copy-cache-value ,copy-cache-value)
	       ,@body)
	      (progn ,@body))
  #-clim `(dw:with-redisplayable-output
	      (:stream ,stream
	       :id-test ,id-test
	       ,@(if unique-id-p `(:unique-id ,unique-id))
	       :cache-test ,cache-test
	       :copy-cache-value ,copy-cache-value
	       ;; I hate behavior that depends on
	       ;; whether or not you supply the default value.
	       ,@(if cache-value-p `(:cache-value ,cache-value))) 
	    ,@body))

(defmacro with-character-face ((face &optional (stream t)) &body body)
  #FEATURE-CASE
  (((not :clim)
    `(scl:with-character-face (,face ,stream :bind-line-height t) ,@body))
   ((or :clim-0.9 :clim-1.0)
    `(clim:with-text-face (,face ,stream) ,@body))
   (:clim-2
    `(clim:with-text-face (,stream ,face) ,@body))))

(defmacro with-text-face ((face stream) &body body)
  `(with-character-face (,face ,stream) ,@body))

(defmacro with-character-style ((style &optional (stream t)) &body body)
  #FEATURE-CASE
  (((not :clim)
    `(scl:with-character-style (,style ,stream :bind-line-height t) ,@body))
   ((or :clim-0.9 :clim-1.0)
    `(clim:with-text-style (,style ,stream) ,@body))
   (:clim-2
    `(clim:with-text-style (,stream ,style) ,@body))))

(defmacro with-text-style ((style stream) &body body)
  `(with-character-style (,style ,stream :bind-line-height t) ,@body))

(defmacro with-character-size ((style &optional (stream t)) &body body)
  #FEATURE-CASE
  (((not :clim)
    `(scl:with-character-size (,style ,stream :bind-line-height t) ,@body))
   ((or :clim-0.9 :clim-1.0)
    `(clim:with-text-size (,style ,stream) ,@body))
   (:clim-2
    `(clim:with-text-size (,stream ,style) ,@body))))

(defmacro with-character-family ((family &optional (stream t)) &body body)
  #FEATURE-CASE
  (((not :clim)
    `(scl:with-character-family (,family ,stream :bind-line-height t) ,@body))
   ((or :clim-0.9 :clim-1.0)
    `(clim:with-text-family (,family ,stream) ,@body))
   (:clim-2
    `(clim:with-text-family (,stream ,family) ,@body))))

(defmacro with-frame ((symbol) &body body)
  #FEATURE-CASE
  (((not :clim)
    `(let ((,symbol (if (boundp 'dw:*program-frame*) dw:*program-frame*))) ,@body))
   (:clim-0.9
    `(clim:with-frame (,symbol) ,@body))
   ((or :clim-1.0 :clim-2)
    `(let ((,symbol (if (boundp 'clim:*application-frame*) clim:*application-frame*)))
       ,@body))))

(defmacro with-program ((symbol) &body body)
  #+clim
  `(with-frame (,symbol) ,@body)
  #-clim
  `(let ((,symbol (if (boundp 'dw:*program*) dw:*program*))) ,@body))


(defmacro accepting-values ((stream &key own-window label (abort-value :ABORT)
				    (exit-boxes ''((:exit "   OK   ")
						   (:abort "   Cancel   "))))
			    &body body)
  ;; add :exit-boxes arg
  #FEATURE-CASE
  ((:clim-0.9
    `(if (eq :abort
	  (clim:accepting-values (,stream :own-window ,own-window) 
	   ,@(if label `((format ,stream "~A~%~%" ,label)))
	   ,@body))
      ,abort-value t))
   (:clim-1.0
    `(if (eq :abort
	  (restart-case
	   (clim:accepting-values (,stream :own-window ,own-window
					   :label ,label
					   :exit-boxes ,exit-boxes
					   :resynchronize-every-pass t
					   ;; get these things to come up
					   ;; in some nonrandom location
					   :x-position 200
					   :y-position 200)
				  (setf (clim:medium-text-style ,stream)
					(parse-text-style '(:fix :roman :normal)))
				  ,@body)
	   (abort () :abort)))
      ;; If you quit using a keyboard accelerator, clim leaves the keystroke
      ;; in the input buffer (clim bug).
      ,abort-value t))
   (:clim-2
    `(if (eq :abort
	  (restart-case
	   (clim:accepting-values
	    (,stream :own-window ,own-window
		     :label ,label
		     :exit-boxes ,exit-boxes
		     :resynchronize-every-pass t
		     ;; get these things to come up
		     ;; in some nonrandom location
		     :x-position 200
		     :y-position 200
		     ,@(when (fboundp (intern "COLOR-STREAM-P" 'clim))
			 ;; Scroll bars don't work till clim 2.0.beta2.
			 `(:scroll-bars :both)))
	    ,@body)
	   (abort () :abort)))
      ;; If you quit using a keyboard accelerator, clim leaves the keystroke
      ;; in the input buffer (clim bug).
      ,abort-value t))
   ((not :clim)
    `(condition-case ()
      (progn
	(dw:accepting-values (,stream :own-window ,own-window :label ,label
				      :changed-value-overrides-default t)
			     ,@body)
	t)
      ;; catch aborts and act like clim.
      (sys:abort ,abort-value)))))

