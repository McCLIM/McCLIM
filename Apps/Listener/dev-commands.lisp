(in-package :clim-listener)

;;; (C) Copyright 2003 by Andy Hefner (hefner1@umbc.edu)

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



;;; Assorted commands intended for lisp development.


;; TODO: Split these into several command tables, and eventually
;; into seperate files. We need seperate command tables for lisp
;; development, filesystem manipulation, UNIX crap, and whatever
;; else suggests itself. Mostly to make them layout nicely in a
;; command menu, if anyone wants such a thing.

(define-command-table dev-commands)

;;; Presentation types

(define-presentation-type class () :inherit-from 'expression)
(define-presentation-type class-name () :inherit-from 'symbol)
(define-presentation-type slot-definition () :inherit-from 'expression)
(define-presentation-type function-name () :inherit-from 'symbol)
(define-presentation-type process () :inherit-from 'expression)

(define-presentation-type generic-function () :inherit-from 't)

(define-presentation-method presentation-typep
    (object (type generic-function))
  (typep object 'generic-function))

(define-presentation-type standard-generic-function ()
  :inherit-from 'generic-function)

(define-presentation-type method ()
  :inherit-from 'expression)

(define-presentation-type standard-method ()
  :inherit-from 'method)

(define-presentation-type directory-stack () :inherit-from 'expression)
(define-presentation-type bytes () :inherit-from 'integer)
(define-presentation-type lisp-memory-usage () :inherit-from 'bytes)

(define-presentation-type package () :inherit-from 'expression)
(define-presentation-method presentation-typep (object (type package))
  (typep object 'package))

(define-presentation-type package-name () :inherit-from 'string)
(define-presentation-method presentation-typep (object (type package-name))
  (find-package 'object))
    


;;; Presentation methods

(define-presentation-method present (object (type standard-method)
				     stream (view textual-view)
				     &key &allow-other-keys)
  (let ((name (clim-mop:generic-function-name
	       (clim-mop:method-generic-function object)))
	(qualifiers (clim-mop:method-qualifiers object))
	(specializers (clim-mop:method-specializers object))
	(lambda-list (clim-mop:method-lambda-list object))
	(class-of-t (find-class t)))
    (format stream "~S ~{~S ~}(" name qualifiers)
    (multiple-value-bind (required optional rest key key-present)
	(climi::parse-lambda-list lambda-list)
      (loop
	 for spec in specializers
	 for arg in required
	 for first-time = t then nil
	 do
	   (unless first-time
	     (write-char #\space stream))
	   (if (eq spec class-of-t)
	       (present arg 'symbol :stream stream)
	       (progn
		 (write-char #\( stream)
		 (present arg 'symbol :stream stream)
		 (write-char #\space  stream)
		 (with-output-as-presentation (stream spec 'class)
		   (format stream "~S" (clim-mop:class-name spec)))
                 (write-char #\) stream))))
      (when optional
	(format stream " &optional ~{~A ~^ ~}" optional))
      (when rest
	(format stream " &rest ~A" (car rest)))
      (when key-present
	(format stream " &key"))
      (loop
	   for arg in key
	   for key-arg = (cond ((symbolp arg)
				(intern (symbol-name arg) :keyword))
			       ((symbolp (car arg))
				(intern (symbol-name (car arg)) :keyword) )
			       (t (caar arg)))
	   do (format stream " ~S" key-arg))
      (write-char #\) stream))))
  

(define-presentation-method present (object (type generic-function)
                                     stream (view textual-view)
                                     &key &allow-other-keys)
  (princ (clim-mop:generic-function-name object) stream))

(define-presentation-method accept
    ((type generic-function) stream (view textual-view) &key)
  ;; generic-functions are a subclass of standard-object, so they can be
  ;; accepted as expressions!
  (let ((fn (accept 'expression
		    :stream stream
		    :view view
		    :history 'generic-function
		    :prompt nil)))
    ;;
    (when (typep fn 'generic-function)
      (return-from accept fn))
    (handler-case
	(fdefinition fn)
      (error ()
	(simple-parse-error "~S is not the name of a generic function."
			    fn)))))

(define-presentation-method present (object (type bytes)
                                     stream (view textual-view)
                                     &key &allow-other-keys)
  (if (zerop object)
      (princ "0" stream)
    (let* ((suffixes '(" bytes" " KB" " MB" " GB" " TB" " PB"))
           (x (floor (realpart (log object 1000))))
           (idx (min x (1- (length suffixes)))))
      (if (zerop idx)
          (format stream "~A bytes" object)
        (format stream "~,1F~A" (/ object (expt 1000 idx)) (nth idx suffixes))))))

;;; Presentation translators

(define-presentation-translator class-name-to-class
  (class-name class dev-commands
     :documentation ((object stream) (format stream "Class object ~A" object))
     :gesture T)
  (object)
  (find-class object))

(define-presentation-translator symbol-to-class
  (symbol class dev-commands
     :documentation ((object stream) (format stream "Class object ~A" object))
     :gesture T
     :tester ((object) (not (not (find-class object nil))))
     :tester-definitive T)
  (object)
  (find-class object))

(define-presentation-translator symbol-to-class-name
  (symbol class-name dev-commands
     :documentation ((object stream) (format stream "Class ~A" object))
     :gesture T
     :tester ((object) (not (not (find-class object nil))))
     :tester-definitive T)
  (object)
  object)

(define-presentation-translator class-to-class-name
  (class class-name dev-commands
     :documentation ((object stream) (format stream "Class of ~A" object))
     :gesture T)
  (object)
  (clim-mop:class-name object))

(define-presentation-translator symbol-to-function-name
  (symbol function-name dev-commands
     :documentation ((object stream) (format stream "Function ~A" object))
     :gesture T
     :tester ((object) (fboundp object))
     :tester-definitive T)
  (object) object)


;;; Mundane commands that don't really belong here.
;;; Arguably, these belong in the standard command table.

(define-command (com-clear-output :name "Clear Output History"
				  :command-table dev-commands
				  :provide-output-destination-keyword nil)
    ()
  (window-clear *standard-output*))

(define-command (com-exit :name "Exit"
			  :command-table dev-commands
			  :provide-output-destination-keyword nil)
    ()
  (frame-exit *application-frame*))


;;; Commands related to Lisp development
;;; ------------------------------------

(defvar *apropos-list* nil
  "The apropos command stores its output here.")

(defparameter *apropos-symbol-unbound-family* :fix)
(defparameter *apropos-symbol-unbound-face* :roman)
(defparameter *apropos-symbol-bound-family*   :fix)
(defparameter *apropos-symbol-bound-face*   :roman)

;; FIXME: Make this a present method specialzed on a view?

(defun apropos-present-symbol (symbol &optional (stream *standard-output*) show-package)
  (multiple-value-bind (style ink)
      (values
       (if (or (fboundp symbol)
               (boundp  symbol)
               (find-class symbol nil))
           (make-text-style *apropos-symbol-bound-family*
                            *apropos-symbol-unbound-face*
                            :normal)
           (make-text-style *apropos-symbol-unbound-family*
                            *apropos-symbol-bound-face*
                            :normal))
       (cond ((eql (symbol-package symbol)
                   (find-package "KEYWORD"))
              (make-rgb-color 0.46 0.0 0.0))
             ((fboundp symbol)
              (make-rgb-color 0.0 0.0 0.3))
             ((find-class symbol nil)
              (make-rgb-color 0.03 0.35 0.48))
             ((boundp symbol)
              (make-rgb-color 0.0 0.0 0.0))
             (t (make-rgb-color 0.6 0.6 0.6))))
    (with-drawing-options (stream :ink ink :text-style style)
      (with-output-as-presentation (stream symbol 'clim:symbol)
        (if show-package
            (let ((*package* (find-package :common-lisp-user)))
              (format stream "~W" symbol))
            (princ (symbol-name symbol) stream)))
      (when (boundp symbol)
        (format stream " = ")
        (with-drawing-options (stream :ink +olivedrab+ ;; XXX
                                      :text-style (make-text-style :fix :roman :small))
          (let ((object (symbol-value symbol)))
            (present object (presentation-type-of object) :stream stream)))))))

;; These are used by com-apropos to filter the list of symbols according to the domain keyword
(defgeneric apropos-applicable-p (spec symbol))

(defmethod apropos-applicable-p ((spec (eql 'symbols)) symbol) t)

(defmethod apropos-applicable-p ((spec (eql 'classes)) symbol)
  (find-class symbol nil))  

(defmethod apropos-applicable-p ((spec (eql 'functions)) symbol)
  (fboundp symbol))

(defmethod apropos-applicable-p ((spec (eql 'variables)) symbol)
  (boundp symbol))

(defmethod apropos-applicable-p ((spec (eql 'command-tables)) symbol)
  (find-command-table symbol :errorp nil))

;(defmethod apropos-applicable-p ((spec (eql 'presentation-type)) symbol)
;  (find-presentation-type-class symbol nil))

(define-command (com-apropos :name "Apropos"
			     :command-table dev-commands
			     :provide-output-destination-keyword t)
    ((string 'clim:string :prompt "string")
     &key
     (package '(or package-name package) :prompt "in package" :default nil)
     (domain '(member symbols classes functions variables command-tables) :prompt "domain" :default 'symbols))
  (let ((real-package (when package
                        (if (typep package 'package)
                            package
                            (find-package package)))))
    (when (and package (not real-package))
      (cerror "Search all packages instead" "No package specified by ~A could be found." package))
    (let ((symbols (remove-if-not (lambda (sym) (apropos-applicable-p domain sym))
                                  (apropos-list string real-package))))
      (dolist (sym symbols)
        (apropos-present-symbol sym *standard-output* T)
        (terpri))
      (setf *apropos-list* symbols)
      (note "Results have been saved to ~W~%" '*APROPOS-LIST*))))

(define-command (com-trace :name "Trace"
			   :command-table dev-commands
			   :provide-output-destination-keyword nil)
    ((fsym 'function-name :prompt "function-name"))
  (if (fboundp fsym)
      (progn 
	(eval `(trace ,fsym))
	(format T "~&Tracing ~W.~%" fsym))
    (format T "~&Function ~W is not defined.~%" fsym)))

(define-command (com-untrace :name "Untrace"
			     :command-table dev-commands
			     :provide-output-destination-keyword nil)
    ((fsym 'symbol :prompt "function name"))
  (if (fboundp fsym)
      (progn
	(eval `(untrace ,fsym))
	(format T "~&~W will no longer be traced.~%" fsym))
    (format T "~&Function ~W is not defined.~%" fsym)))


(define-command (com-load-file :name "Load File"
                               :command-table dev-commands
			       :provide-output-destination-keyword t)
  ((pathname 'pathname :prompt "pathname"))
  (load pathname))

(define-command (com-compile-file :name "Compile File"
                                  :command-table dev-commands
				  :provide-output-destination-keyword t)
  ((pathname 'pathname :prompt "pathname"))
  (compile-file pathname))

(define-command (com-compile-and-load :name "Compile and load"
                                      :command-table dev-commands
				      :provide-output-destination-keyword t)
  ((pathname 'pathname :prompt "pathname"))
  (load (compile-file pathname)))

(define-command (com-room :name "Room"
                          :command-table dev-commands
			  :provide-output-destination-keyword t)
  ()
  (room))

(define-presentation-to-command-translator mem-room-translator
  (lisp-memory-usage com-room dev-commands :gesture :select)
  ())
  

;;; CLOS introspection commands

(defun class-grapher (stream class inferior-fun)
  "Does the graphing for Show Class Superclasses and Subclasses commands"
  (let ((normal-ink +foreground-ink+)
        (arrow-ink  (make-rgb-color 0.72 0.72 0.72))	
	(text-style (make-text-style :fix :roman :normal)))
    (with-drawing-options (stream :text-style text-style)
    (format-graph-from-roots (list class)
			     #'(lambda (class stream)                                 
                                 (with-drawing-options (stream :ink normal-ink
							       :text-style text-style)
                                   (with-output-as-presentation (stream (clim-mop:class-name class) 'class-name)
                                     ; (surrounding-output-with-border (stream :shape :drop-shadow)
				       (princ (clim-mop:class-name class))))) ;)
			     inferior-fun
			     :stream stream
                             :merge-duplicates T
                             :graph-type :tree
			     :orientation :horizontal
			     :arc-drawer
			     #'(lambda (stream foo bar x1 y1 x2 y2)
				 (declare (ignore foo bar))
				 (draw-arrow* stream x1 y1 x2 y2 :ink arrow-ink))))))

(defun frob-to-class (spec)
  (if (typep spec 'class)
      spec
    (find-class spec nil)))

(define-command (com-show-class-superclasses :name "Show Class Superclasses"
                                             :command-table dev-commands
					     :provide-output-destination-keyword t)
    ((class-spec 'class-name :prompt "class"))
  (let ((class (frob-to-class class-spec)))
    (if (null class)
	(note "~A is not a defined class." class-spec)
        (class-grapher *standard-output* class #'clim-mop:class-direct-superclasses))))

(define-command (com-show-class-subclasses :name "Show Class Subclasses"
                                           :command-table dev-commands
					   :provide-output-destination-keyword t)
    ((class-spec 'class-name :prompt "class"))
  (let ((class (frob-to-class class-spec)))
    (if (not (null class))
        (class-grapher *standard-output* class #'clim-mop:class-direct-subclasses)
      (note "~A is not a defined class." class-spec))))


; Lookup direct slots from along the CPL given a class and a slot name.
; Returns them in an order parallel with the CPL.
; Need this to find readers/writers, which exist in the direct slot
; definitions, not the effective slot definitions. (ouch)
(defun direct-slot-definitions (class slot-name)
  (let ((cpl (reverse (clim-mop:class-precedence-list class)))
        (direct-slots nil))
    (dolist (foo cpl)
      (let ((dslots (clim-mop:class-direct-slots foo)))
        (dolist (slot dslots)
          (when (eq slot-name (clim-mop:slot-definition-name slot))
            (push slot direct-slots)))))
    direct-slots))

(defparameter *slot-name-ink*     +black+)
(defparameter *slot-type-ink*     +gray50+)
(defparameter *slot-initargs-ink* +red+)
(defparameter *slot-initform-ink* +goldenrod3+)
(defparameter *slot-readers-ink*  +black+)
(defparameter *slot-writers-ink*  +black+)
(defparameter *slot-documentation-ink* +turquoise4+)


(defun present-slot (slot class &key (stream *standard-output*))
  "Formats a slot definition into a table row."
  (let* ((name (clim-mop:slot-definition-name slot))
         (type (clim-mop:slot-definition-type slot))
         (initargs (clim-mop:slot-definition-initargs slot))
         (initfunc (clim-mop:slot-definition-initfunction slot))
         (initform (clim-mop:slot-definition-initform slot))
         (direct-slots (direct-slot-definitions class name))
         (readers (reduce #'append (filtermap direct-slots #'clim-mop:slot-definition-readers)))
         (writers (reduce #'append (filtermap direct-slots #'clim-mop:slot-definition-writers)))
         (documentation (first (filtermap direct-slots (lambda (x) (documentation x T)))))
         (*standard-output* stream))

  (macrolet ((with-ink ((var) &body body)
               `(with-drawing-options (T :ink ,(intern (concatenate 'string "*SLOT-" (symbol-name var) "-INK*")))
                     ,@body))
             (fcell ((var align-x &rest cell-opts) &body body)
                `(formatting-cell (T :align-x ,align-x ,@cell-opts)
                   (with-ink (,var) ,@body) )))
    
    (fcell (name :left)
     (with-output-as-presentation (T slot 'slot-definition)
       (princ name))
     (unless (eq type T)
       (fresh-line)
       (with-ink (type) (princ type))))

    (fcell (initargs :right)
      (dolist (x initargs)
        (format T "~W~%" x)))

    (fcell (initform :left)
      (if initfunc
          (format T "~W" initform)
        (note "No initform")))

    #+NIL   ; argh, shouldn't this work?
    (formatting-cell ()
      (formatting-table ()
        (formatting-column ()
          (fcell (readers :center)
                 (if readers
                     (dolist (reader readers)  (format T "~A~%" reader))
                   (note "No readers")))
          (fcell (writers :center)
                 (if writers
                     (dolist (writer writers)  (format T "~A~%" writer))
                   (note "No writers"))))))

    (formatting-cell (T :align-x :left)
      (if (not (or readers writers))
          (note "No accessors")
        (progn
          (with-ink (readers)
            (if readers (dolist (reader readers)  (format T "~A~%" reader))
              (note "No readers~%")))
          (with-ink (writers)
            (if writers (dolist (writer writers)  (format T "~A~%" writer))
              (note "No writers"))))))

    (fcell (documentation :left)
      (when documentation (with-text-family (T :serif) (princ documentation))))
)))


(defun earliest-slot-definer (slot class)
  "Returns the earliest class in the CPL of CLASS which defines SLOT."
  (let ((name (clim-mop:slot-definition-name slot)))
    (dolist (class (reverse (clim-mop:class-precedence-list class)))
      (dolist (slot-b (clim-mop:class-direct-slots class))
        (when (eq name (clim-mop:slot-definition-name slot-b))
          (return-from earliest-slot-definer class)))))
  (error "Slot ~W doesn't seem to be defined in ~W" slot class))

(defun class-sorted-slots (class)
  "Sort the slots in order of definition within the CPL, superclasses first."
  (let ((cpl (clim-mop:class-precedence-list class)))
    (sort (copy-list (clim-mop:class-slots class))
          (lambda (a b)
            (< (position (earliest-slot-definer a class) cpl)
               (position (earliest-slot-definer b class) cpl))))))

(defun print-slot-table-heading ()
  (formatting-row (T)
    (dolist (name '("Slot name" "Initargs" "Initform" "Accessors"))
      (formatting-cell (T :align-x :center)        
        (underlining (T)
          (with-text-family (T :sans-serif)
            (princ name)))))))

(defun present-slot-list (slots class)
  (formatting-table (T)
    (print-slot-table-heading)
    (dolist (slot slots)
      (formatting-row (T)
        (present-slot slot class)))))

(defun friendly-slot-allocation-type (allocation)
  (if (typep allocation 'standard-class)
      (class-name allocation)
    allocation))

(defun present-the-slots (class)  
  (let* ((slots (class-sorted-slots class))
         (instance-slots (remove-if (lambda (x) (not (eq :instance (clim-mop:slot-definition-allocation x)))) slots))
         (other-slots (set-difference slots instance-slots))
         (allocation-types (remove-duplicates (mapcar #'clim-mop:slot-definition-allocation other-slots))))
    (when other-slots
      (underlining (T) (format T "~&Instance Slots~%")))
    (present-slot-list instance-slots class)
    (dolist (alloc allocation-types)
      (underlining (T)
        (format T "~&Allocation: ~A~%" (friendly-slot-allocation-type alloc)))
      (present-slot-list (remove-if (lambda (x)
                                      (not (eq alloc (clim-mop:slot-definition-allocation x))))
                                    other-slots)
                         class))))

(define-command (com-show-class-slots :name "Show Class Slots"
				      :command-table dev-commands
				      :provide-output-destination-keyword t)
    ((class-name 'clim:symbol :prompt "class name"))
  (let ((class (find-class class-name nil)))
    (if (null class)
	(format T "~&~A is not a defined class.~%" class-name)
      (let ((slots (clim-mop:class-slots class)))
	(if (null slots)
	    (note "~%This class has no slots.~%~%")
            (progn
            ; oddly, looks much better in courier, because of all the capital letters.
;            (with-text-family (T :sans-serif)
              (invoke-as-heading
               (lambda ()
                 (format T "~&Slots for ")
                 (with-output-as-presentation (T (clim-mop:class-name class) 'class-name)
                   (princ (clim-mop:class-name class)))))
              (present-the-slots class) ))))))

(defparameter *ignorable-internal-class-names*
  '(standard-object))

(defun remove-ignorable-classes (classes)
  (remove-if (lambda (c)
               (or (member (class-name c) *ignorable-internal-class-names*)
                   (not (typep c 'standard-class))))
             classes))

(defun x-specializer-direct-generic-functions (specializer)
  #+PCL (pcl::specializer-direct-generic-functions specializer)
  #+SBCL (sb-pcl::specializer-direct-generic-functions specializer)
  #+openmcl-partial-mop
  (openmcl-mop:specializer-direct-generic-functions specializer)
  #-(or PCL SBCL openmcl-partial-mop)
  (error "Sorry, not supported in your CL implementation. See the function X-SPECIALIZER-DIRECT-GENERIC-FUNCTION if you're interested in fixing this."))

(defun class-funcs (class)
  (let ((classes (remove-ignorable-classes (copy-list (clim-mop:class-precedence-list class))))
        (gfs nil))
    (dolist (x classes)
      (setf gfs (append gfs (x-specializer-direct-generic-functions x))))
    (remove-duplicates gfs)))

(defun not-really-symbol< (a b)
  (when (and (consp a)
             (second a)
             (symbolp (second a)))
    (setf a (second a)))
  (when (and (consp b)
             (second b)
             (symbolp (second b)))
    (setf b (second b)))
  (unless (and (symbolp a) (symbolp b))
    (return-from not-really-symbol< (string< (princ-to-string a)
                                             (princ-to-string b))))
  (cond ((not (eq (symbol-package a)
                  (symbol-package b)))
         (string< (package-name (symbol-package a))
                  (package-name (symbol-package b))))
        (T (string< (symbol-name a) (symbol-name b)))))

(define-command (com-show-class-generic-functions
                 :name "Show Class Generic Functions"
                 :command-table dev-commands
		 :provide-output-destination-keyword t)
    ((class-spec 'class-name :prompt "class"))
  (let ((class (frob-to-class class-spec)))
    (if (null class)
        (note "~A is not a defined class." class-spec)
      (let ((funcs (sort (class-funcs class) (lambda (a b)
                                               (not-really-symbol< (clim-mop:generic-function-name a)
                                                        (clim-mop:generic-function-name b))))))
        (with-text-size (T :small)
          (format-items funcs :printer (lambda (item stream)
                                         (present item 'generic-function :stream stream))
                        :move-cursor T))))))

(defun method-applicable-to-args-p (method args arg-types)
  (loop
     for specializer in (clim-mop:method-specializers method)
     for arg in args
     for arg-type in arg-types
     unless (cond ((eq arg-type :wild)
		   t)
		  ((typep specializer 'clim-mop:eql-specializer)
		   (and (not (eq arg arg-type))
			    (eql arg
				 (clim-mop:eql-specializer-object
				  specializer))))
		  ((eq arg arg-type)
		   (subtypep arg-type specializer))
		  (t (typep arg specializer)))
     return nil
     finally (return t)))

(defun get-applicable-methods (gf args arg-types)
  (mapcan #'(lambda (method)
	      (when (method-applicable-to-args-p method args arg-types)
		(list method)))
	  (clim-mop:generic-function-methods gf)))

(defun sort-methods-by-args (methods arg-types)
  (let ((cpls (mapcar #'(lambda (type)
			  (if (eq type :wild)
			      nil
			      (clim-mop:class-precedence-list type)))
		      arg-types)))
    (flet ((sorter (meth1 meth2)
	     (loop
		for spec1 in (clim-mop:method-specializers meth1)
		for spec2 in (clim-mop:method-specializers meth2)
		for arg-type in arg-types
		for cpl in cpls
		for spec1-cpl = (unless (typep spec1 'clim-mop:eql-specializer)
				  (clim-mop:class-precedence-list spec1))
		for spec2-cpl = (unless (typep spec1 'clim-mop:eql-specializer)
				  (clim-mop:class-precedence-list spec2))
		do (cond ((eq spec1 spec2)) ;Keep going
			 ((eq arg-type :wild)
			  (cond ((typep spec1 'clim-mop:eql-specializer)
				 (unless (typep spec2
						'clim-mop:eql-specializer)
				   (return-from sorter t)))
				((typep spec1 'clim-mop:eql-specializer)
				 (return-from sorter nil))
				((subtypep spec1 spec2)
				 (return-from sorter t))
				((subtypep spec2 spec1)
				 (return-from sorter nil))
				;; Types are not related
				(t (let ((cpl-len1 (length spec1-cpl))
					 (cpl-len2 (length spec2-cpl)))
				     (cond ((> cpl-len1 cpl-len2)
					    (return-from sorter t))
					   ((< cpl-len1 cpl-len2)
					    (return-from sorter nil)))))))
			 ;; An actual instance
			 ((typep spec1 'clim-mop:eql-specializer)
			  (return-from sorter t))
			 ((typep spec2 'clim-mop:eql-specializer)
			  (return-from sorter nil))
			 (t (let ((pos1 (position spec1 cpl))
				  (pos2 (position spec2 cpl)))
			      (cond ((< pos1 pos2)
				     (return-from sorter t))
				    ((> pos1 pos2)
				     (return-from sorter nil))))))
		  finally (return nil))))
      (declare (dynamic-extent #'sorter))
      (sort methods #'sorter))))

(defun show-specialized-methods (gf stream methods arg-types)
  (let ((before nil)
	(after nil)
	(around nil)
	(primary nil))
    (loop
       for meth in methods
       for (qualifier) = (clim-mop:method-qualifiers meth)
       do (case qualifier
	    (:before
	     (push meth before))
	    (:after
	     (push meth after))
	    (:around
	     (push meth around))
	    (t (push meth primary))))
    (setq before (sort-methods-by-args before arg-types))
    (setq after (nreverse (sort-methods-by-args after arg-types)))
    (setq around (sort-methods-by-args around arg-types))
    (setq primary (sort-methods-by-args primary arg-types))
    (flet ((do-meths (kind methods)
	     (with-text-face (stream :italic)
	       (format stream "~A methods:~%" kind))
	     (loop
		for meth in methods
		do
		  (present meth 'standard-method :stream stream)
		  (terpri stream))))
      (do-meths "Before" before)
      (do-meths "Around" around)
      (do-meths "Primary" primary)
      (do-meths "After" after))))

(defun make-gf-specialized-ptype (gf-name)
  (let ((gf gf-name))
    (unless (typep gf 'generic-function)
      (handler-case
	  (setq gf (fdefinition gf-name))
	(error ()
	  (return-from make-gf-specialized-ptype nil))))
    (unless (typep gf 'generic-function)
      (return-from make-gf-specialized-ptype nil))
    (let ((required (climi::parse-lambda-list
		     (clim-mop::generic-function-lambda-list gf))))
      (loop
	 for arg in required
	 collect (make-presentation-type-specifier
		  '(expression)
		  :description (format nil "~A" arg))
	   into args
	 finally (return `(sequence-enumerated ,@args))))))


(define-command (com-show-generic-function
		 :name t
		 :command-table dev-commands
		 :provide-output-destination-keyword t)
    ((gf 'generic-function :prompt "a generic function")
     &key (classes 'boolean :default nil :mentioned-default t)
     (methods 'boolean :default nil :mentioned-default t)
     (specialized (make-gf-specialized-ptype gf)
		  :prompt "function arguments" :default nil
		  :display-default nil))
  (when specialized
    (setq methods t))
  (let ((doc-string (documentation gf t)))
    (with-text-face (*standard-output* :italic)
      (format *standard-output* "Lambda list:~%"))
    (format *standard-output* "~S~%" (clim-mop:generic-function-lambda-list
				      gf))
    (when doc-string
      (with-text-face (*standard-output* :italic)
	(format *standard-output* "Documentation:~%~A~&" doc-string)))
    (when classes
      (with-text-face (*standard-output* :italic)
	(format *standard-output* "Classes:~%"))
      (let ((class-list nil)
	    (meths (clim-mop:generic-function-methods gf)))
	(loop
	   for m in meths
	   do (loop for arg in (clim-mop:method-specializers m)
		 unless (typep arg 'clim-mop:eql-specializer)
		 do (pushnew arg class-list)))
	(loop
	   for class in class-list
	   do (progn
		(with-output-as-presentation (*standard-output*
					      (clim-mop:class-name class)
					      'class-name)
		  (format *standard-output*
			  "~S~%" (clim-mop:class-name class)))))))
    (when methods
      (let ((args nil)
	    (arg-types nil))
	(if (null specialized)
	    (setq args
		  (mapcar (constantly :wild)
			  (clim-mop:generic-function-argument-precedence-order
			   gf))
		  arg-types
		  args)
	    (loop
	       with arg = nil and arg-type = nil
	       for arg-for-spec in specialized
	       do (setf (values arg arg-type)
			(cond ((eq arg-for-spec '*)
			       (values :wild :wild))
			      ((and (listp arg-for-spec)
				    (eq (car arg-for-spec) 'quote))
			       (values (cadr arg-for-spec)
				       (class-of (cadr arg-for-spec))))
			      ((symbolp arg-for-spec)
			       (let ((class (find-class arg-for-spec)))
				 (values class class)))
			      ((typep arg-for-spec 'standard-class)
			       (values arg-for-spec arg-for-spec))
			      (t (values arg-for-spec
					 (class-of arg-for-spec)))))
	       collect arg into collect-args
	       collect arg-type into collect-arg-types
	       finally (setf (values args arg-types)
			     (values collect-args collect-arg-types))))
	(let ((meths (get-applicable-methods gf args arg-types)))
	  (with-text-face (*standard-output* :italic)
	    (format *standard-output* "Methods:~%"))
	  (show-specialized-methods gf *standard-output* meths arg-types))))))




;;; Filesystem Commands
;;; -------------------

(defun pathname-printing-name (pathname long-name)
  (if long-name
      (princ-to-string (namestring pathname))
    (if (directoryp pathname)
        (format nil "~A/" (first (last (pathname-directory pathname))))
      (namestring (make-pathname :name (pathname-name pathname)
                                 :type (pathname-type pathname)
                                 :version (pathname-version pathname))))))

(defun pretty-pretty-pathname (pathname stream &key (long-name T))
  (with-output-as-presentation (stream pathname 'clim:pathname)
    (let ((icon (icon-of pathname)))
      (when icon  (draw-icon stream icon :extra-spacing 3)))
    (princ (pathname-printing-name pathname long-name) stream))
  (terpri stream))

(defun sort-pathnames (list sort-by)
  list)                 ; <--- FIXME

(defun split-sort-pathnames (list group-dirs sort-by)
  (mapcar (lambda (x) (sort-pathnames x sort-by))
          (multiple-value-list
           (if (not group-dirs) (values list)
             (values (remove-if-not #'directoryp list)
                     (remove-if #'directoryp list))))))

(defun garbage-name-p (name)
  (when (> (length name) 2)
    (let ((first (elt name 0))
          (last  (elt name (1- (length name)))))
      (or (char= last #\~)
          (and (char= first #\#)
               (char= last  #\#))))))

(defun hidden-name-p (name)
  (when (> (length name) 1)
    (char= (elt name 0) #\.)))

(defun filter-garbage-pathnames (seq show-hidden hide-garbage)
  (delete-if (lambda (p)
               (let ((name (pathname-printing-name p nil)))
                 (or (and (not show-hidden) (hidden-name-p name))
                     (and hide-garbage (garbage-name-p name)))))
             seq))

;; Change to using an :ICONIC view for pathnames?

(define-command (com-show-directory :name "Show Directory"
				    :command-table dev-commands
				    :provide-output-destination-keyword t)
    ((pathname 'pathname #+nil(or 'string 'pathname) :prompt "pathname")
     &key
     #+NIL (sort-by '(member name size modify none) :default 'name)
     (show-hidden  'boolean :default nil :prompt "show hidden")
     (hide-garbage 'boolean :default T   :prompt "hide garbage")
     (show-all     'boolean :default nil :prompt "show all")
     (style '(member items list) :default 'items :prompt "listing style")
     (group-directories 'boolean :default T :prompt "group directories?")
     (full-names 'boolean :default nil :prompt "show full name?"))

  (let* ((pathname (if (wild-pathname-p pathname) ; Forgot why I did this..
                       (merge-pathnames pathname)
                     pathname))
         (dir (list-directory (gen-wild-pathname pathname))))

    (with-text-family (T :sans-serif)      
      (invoke-as-heading
        (lambda ()
          (format T "Directory contents of ")
          (present pathname)))
    
      (when (parent-directory pathname)
        (with-output-as-presentation (T (parent-directory pathname) 'clim:pathname)
          (draw-icon T (standard-icon "up-folder.xpm") :extra-spacing 3)
          (format T "Parent Directory~%")))

      (dolist (group (split-sort-pathnames dir group-directories :none #+NIL sort-by))
        (unless show-all
          (setf group (filter-garbage-pathnames group show-hidden hide-garbage)))
        (ecase style
          (items (abbreviating-format-items group :row-wise nil :x-spacing "  " :y-spacing 1
                                            :printer (lambda (x stream)
                                                       (declare (ignore stream))
                                                       (pretty-pretty-pathname x *standard-output*
                                                                               :long-name full-names)))
                 #+NIL
                 (format-items group :row-wise nil :x-spacing "  " :y-spacing 1
                               :printer (lambda (x stream)
                                          (declare (ignore stream))
                                          (pretty-pretty-pathname x *standard-output* :long-name full-names)))
                 (goatee::reposition-stream-cursor *standard-output*)                 
                 (vertical-gap T))
          (list (dolist (ent group)
                  (let ((ent (merge-pathnames ent pathname))) ; This is for CMUCL, see above. (fixme!)
                    (pretty-pretty-pathname ent *standard-output* :long-name full-names)))))))))

#+nil   ; OBSOLETE
(define-presentation-to-command-translator show-directory-translator
  (clim:pathname com-show-directory dev-commands :gesture :select
		 :pointer-documentation ((object stream)
					 (format stream "Show directory ~A" object))
                 :tester-definitive T
		 :tester ((object)
			  (directoryp object)))
  (object)
  (list object))


(define-command (com-change-directory :name "Change Directory"
                                      :command-table dev-commands)
  ((pathname 'pathname :prompt "pathname"))
  (let ((pathname (merge-pathnames pathname)))
    (cond ((not (probe-file pathname))
           (note "~A does not exist." pathname))
          ((not (directoryp pathname))
           (note "~A is not a directory." pathname))
          (T (change-directory (merge-pathnames pathname))) )))

(define-command (com-up-directory :name "Up Directory"
                                  :command-table dev-commands)
  ()
  (let ((parent (parent-directory *default-pathname-defaults*)))
    (when parent
      (change-directory parent)
      (italic (T)
        (format T "~&The current directory is now ")
        (present (truename parent))
        (terpri)))))
  
(define-gesture-name :change-directory :pointer-button-press
  (:middle))

(define-presentation-to-command-translator change-directory-translator
  (clim:pathname com-change-directory dev-commands :gesture :change-directory
		 :pointer-documentation ((object stream)  (declare (ignore object))
					 (format stream "Change to this directory"))
                 :documentation ((object stream)  (declare (ignore object))
                                 (format stream "Change to this directory"))
                 
		 :tester ((object)
			  (directoryp object)))
  (object)
  (list object))


;;; External file viewers

(defgeneric mime-type-to-command (mime-type pathname)
  (:documentation "Translates a pathname to an invocation of a CLIM command, typically according to the mime type deduced for that pathname. Returns three values: command, documentation, and pointer documentation."))

;; This pathname translator stuff is really turning into a mess.
;; What I need to do is merge mime types with presentations, and
;; rip all the rest of this shit out.

(defmethod mime-type-to-command (mime-type pathname)
  (declare (ignore mime-type pathname))
  (values nil nil))

(defmethod mime-type-to-command ((mime-type null) pathname)
  (declare (ignore #|mime-type|# pathname))
  (values nil nil))

(defmethod mime-type-to-command ((mime-type symbol) pathname)
  (mime-type-to-command (clim-mop:class-prototype (find-class mime-type nil)) pathname))

;; Move these elsewhere.

(defmethod mime-type-to-command ((mime-type text/x-lisp-source) pathname)
  (values `(com-compile-and-load ,pathname)
          "Compile and Load"
          (format nil "Compile and load ~A" pathname)))

(defmethod mime-type-to-command ((mime-type application/x-lisp-fasl) pathname)
  (values `(com-load-file ,pathname)
          "Load"
          (format nil "Load ~A" pathname)))

(defmethod mime-type-to-command ((mime-type text/x-lisp-system) pathname)
  (values `(com-load-file ,pathname)
          "Load System"
          (format nil "Load System ~A" pathname)))

;; I've taken to doing translator documentation exactly opposite of how the CLIM
;; spec seems to intend. The spec says that the pointer-documentation should be
;; short and quickly computed, and the documentation should be longer and more
;; descriptive. Personally, I like seeing the full the command with the arguments
;; in the pointer-doc window, and something short in right-button menus.
;; So.. yeah.

(defun automagic-translator (pathname)
  "Returns  values, the command translation, and a documentation string for the translation."  
  (cond ((wild-pathname-p pathname)
         (values `(com-show-directory ,pathname)
                 "Show Matching Files"
                 (format nil "Show Files Matching ~A" pathname)))
        ((not (probe-file pathname))
         (values nil nil nil))
        ((directoryp pathname)
         (values `(com-show-directory ,pathname)
                 "Show Directory"
                 (format nil "Show Directory ~A" pathname)))
        (t
         (multiple-value-bind (command doc pointer-doc)
             (find-viewspec pathname)
           (let ((mime-type (pathname-mime-type pathname)))
             (mv-or
              (when mime-type (mime-type-to-command mime-type pathname))
              (when command
                (values command doc pointer-doc))
              (when (and mime-type (subtypep mime-type 'text))
                (values `(com-edit-file ,pathname)
                        "Edit File"
                        (format nil "Edit ~A" pathname))) ))))))

(define-presentation-translator automagic-pathname-translator
  (clim:pathname clim:command dev-commands
                 :gesture :select
                 :priority 2
                 :tester ((object)
                          (automagic-translator object))
                 :documentation ((object stream)
                                 (princ (nth-value 1 (automagic-translator object)) stream))
                 :pointer-documentation ((object stream)                                         
                                         (princ (nth-value 2 (automagic-translator object)) stream)))
  (object)  
  (values
   (automagic-translator object)
   'command))


;;; The directory stack.

(defvar *directory-stack* nil)

(define-command (com-push-directory :name "Push Directory"
                                    :command-table dev-commands)
  ((pathname 'pathname :prompt "pathname"))
  (let ((pathname (merge-pathnames pathname)))
    (if (and (probe-file pathname)
             (directoryp pathname));; hrm.. need smart conversion to directories..
        (progn (push *default-pathname-defaults* *directory-stack*)
               (com-change-directory pathname))
      (italic (T)
        (fresh-line) (present (truename pathname))
        (format T " does not exist or is not a directory.~%")) )))

(defun comment-on-dir-stack ()
  (if *directory-stack*
      (progn
        (format T "~&The top of the directory stack is now ")
        (present (truename (first *directory-stack*)))
        (terpri))
    (format T "~&The directory stack is now empty.~%")))

(define-command (com-pop-direcory :name "Pop Directory"
                                  :command-table dev-commands)
  ()
  (if (null *directory-stack*)
      (note "The directory stack is empty!")
    (progn 
      (com-change-directory (pop *directory-stack*))
      (italic (T) (comment-on-dir-stack)))))

(define-command (com-drop-directory :name "Drop Directory"
                                    :command-table dev-commands)
  ()
  (italic (T)
    (if (null *directory-stack*)
        (format T "~&The directory stack is empty!~%")
      (progn
        (setf *directory-stack* (rest *directory-stack*))
        (comment-on-dir-stack)))))

(define-command (com-swap-directory :name "Swap Directory"
                                    :command-table dev-commands)
  ()
  (italic (T)
    (if (null *directory-stack*)
        (format T "~&The directory stack is empty!~%")
      (progn
        (psetf (first *directory-stack*) *default-pathname-defaults*
               *default-pathname-defaults* (first *directory-stack*))
        (comment-on-dir-stack)))))

(define-command (com-display-directory-stack :name "Display Directory Stack"
                                          :command-table dev-commands)
  ()
  (if (null *directory-stack*)
      (note "~&The directory stack is empty!~%")
    (dolist (pathname *directory-stack*)
      (fresh-line)
      (pretty-pretty-pathname pathname *standard-output*) )))

(define-presentation-to-command-translator display-dir-stack-translator
  (directory-stack com-display-directory-stack dev-commands :gesture :select)
  () ())


(define-command (com-edit-file :name "Edit File"
			       :command-table dev-commands
			       :provide-output-destination-keyword nil)
  ((pathname 'pathname  :prompt "pathname"))
  (clim-sys:make-process (lambda () (ed pathname))))

;; Leave this translator disabled for now, the automagic translator will now produce
;; com-edit-file where there is not a more specific handler for a text mime type.
#+IGNORE
(define-presentation-to-command-translator edit-file
  (clim:pathname com-edit-file dev-commands :gesture :select
		 :pointer-documentation ((object stream)
					 (format stream "Edit ~A" object))
		 :documentation ((stream) (format stream "Edit File"))
		 :tester ((object)
			  (and (not (wild-pathname-p object))
                               (probe-file object)
                               (pathname-name object))))
  (object)
  (list object))

(define-command (com-show-file :name "Show File" :command-table dev-commands
			       :provide-output-destination-keyword t)
  ((object 'pathname :prompt "pathname"))
  (show-file object))

(define-command (com-edit-definition :name "Edit Definition"
				     :command-table dev-commands
				     :provide-output-destination-keyword nil)
  ((symbol 'symbol :prompt "function-name"))
  (clim-sys:make-process (lambda () (ed symbol))))

(defun editable-definition-p (symbol)
  (fboundp symbol))

(define-presentation-to-command-translator edit-definition
  (symbol com-edit-definition dev-commands :gesture :select
	  :pointer-documentation ((object stream)
				  (format stream "Edit Definition of ~A" object))
	  :documentation ((stream) (format stream "Edit Definition"))
	  :tester ((object)
		   (editable-definition-p object)))
  (object)
  (list object))
		   

;; CLIM:OPEN-WINDOW-STREAM seems to be broken.
;; Less broken since I hacked on it, but still bad..
(defun show-file (pathname)
  (let ( #+ignore(*standard-output* (open-window-stream :scroll-bars :both)) )
    (with-open-file (in pathname)
      (loop for line = (read-line in nil)
	while line
	do (progn (princ line)
		  (terpri))))))

;; You have to seperate command arguments with commas..
;; Need to find a better way to input these.

;; McCLIM fixme: Shouldn't we be able to activate before the (args) prompt
;; since defaults are defined?
;; FIXME: Disabled input, as it usually seems to hang.
(define-command (com-run :name "Run" :command-table dev-commands
			 :provide-output-destination-keyword t)
  ((program 'string :prompt "command")
   (args '(sequence string) :default nil :prompt "args"))
  (run-program program args :wait T :input nil))

;; Replace this command with a keyword to COM-RUN.
(define-command (com-background-run :name "Background Run"
				    :command-table dev-commands
				    :provide-output-destination-keyword t)
  ((program 'string :prompt "command")
   (args '(sequence string) :default nil :prompt "args"))
  (run-program program args :wait nil :output nil :input nil))

;;; Eval

(defun hackish-present (object)
  "Hack of the day.. let McCLIM determine presentation type to use, except for lists, because the list presentation method is inappropriate for lisp return values."
  (typecase object
    (sequence (present object 'expression))
    (T (present object))))

(defun display-evalues (values)
  (with-drawing-options (T :ink +olivedrab+)
    (cond ((null values)
           (format T "No values.~%"))
          ((= 1 (length values))           
           (hackish-present (first values))
           (fresh-line))
          (T (do ((i 0 (1+ i))
                  (item values (rest item)))
                 ((null item))           
               (with-drawing-options (T :ink +limegreen+)
                 (with-text-style (T (make-text-style nil :italic :small))
                   (format T "~A  " i)))
                 (hackish-present (first item))
                 (fresh-line))))))

(defun shuffle-specials (form values)
  (setf +++ ++
        ++  +
        +   form
        /// //
        //  /
        /   values
        *** **
        **  *
        *   (first values)))

(define-command (com-eval :command-table dev-commands)
    ((form 'clim:form :prompt "form"))  
  (let ((values (multiple-value-list (eval form))))
    (fresh-line)
    (shuffle-specials form values)
    (display-evalues values)
    (fresh-line)))

(define-command (com-reload-mime-database :name "Reload Mime Database"
                                          :command-table dev-commands)
    ()
  (progn
    (load-mime-types)
    (load-mailcaps)))

;;; Some CLIM developer commands

(define-command (com-show-command-table :name t :command-table dev-commands)
    ((table 'clim:command-table :prompt "command table")
     &key
     (locally 'boolean :default nil :mentioned-default t)
     (show-commands 'boolean :default t))
  (let ((our-tables nil)
	(processed-commands (make-hash-table :test #'eq)))
    (do-command-table-inheritance (ct table)
      (let ((commands nil))
	(map-over-command-table-commands
	 #'(lambda (command)
	     (unless (gethash command processed-commands)
	       (push command commands)
	       (setf (gethash command processed-commands) t)))
	 ct
	 :inherited nil)
	(push (cons ct (sort commands
                             (lambda (x y)
                               (string-lessp (command-line-name-for-command x ct :errorp :create)
                                             (command-line-name-for-command y ct :errorp :create))))) our-tables)))
    (setq our-tables (nreverse our-tables))

    (when show-commands ;; sure, why not?
      (dolist (foo our-tables)
        (let ((ct       (car foo))
              (commands (cdr foo)))
          (invoke-as-heading
           (lambda ()
             (format T "Command table ")
             (with-output-as-presentation (t ct 'clim:command-table)
               (princ (command-table-name ct)))))
          (if commands
              (format-items commands :printer (lambda (cmd s) (present cmd 'clim:command-name :stream s))
                            :move-cursor t)
              (note "Command table is empty.~%~%") ))))))

 
;;; Various Lisp goodies

(define-presentation-type package ()
  :inherit-from t)

(define-presentation-method presentation-typep (object (type package))
  (packagep object))

(define-presentation-method present (object (type package) stream
				     (view textual-view)
				     &key)
  (princ (package-name object) stream))

(define-presentation-method accept ((type package) stream (view textual-view)
				    &key)
  (multiple-value-bind
	(object success)
      (completing-from-suggestions (stream)
	(loop
	   for p in (list-all-packages)
	   do (progn
		(suggest (package-name p) p)
		(loop
		   for n in (package-nicknames p)
		   do (suggest n p)))))
    (if success
	object
	(simple-parse-error "No package"))))

(define-command (com-set-package :name t
				 :command-table dev-commands
				 :provide-output-destination-keyword nil)
    ((p 'package))
  (setf *package* p))
