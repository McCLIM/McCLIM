;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2001,2002 by Tim Moore (moore@bricoworks.com)
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

;;; PRESENTATION class

(defclass presentation ()
  ()
  )

(defun presentationp (object)
  (typep object 'presentation))

(defvar *allow-sensitive-inferiors* t)

(defclass presentation-mixin (presentation)
  ((object :accessor presentation-object :initarg :object)
   (type :accessor presentation-type :initarg :type)
   (view :accessor presentation-view :initarg :view)
   (single-box :accessor presentation-single-box :initarg :single-box
	       :initform nil)
   (modifier :reader presentation-modifier :initarg :modifier :initform nil)
   (is-sensitive :reader is-sensitive :initarg :is-sensitive
		 :initform *allow-sensitive-inferiors*)))


(defclass standard-presentation 
    (presentation-mixin standard-sequence-output-record)
  ())

(defgeneric ptype-specializer (type)
  (:documentation "The specializer to use for this type in a presentation
method lambda list"))

;;; Metaclass for presentation types.  For presentation types not associated 
;;; with CLOS classes, objects with this metaclass are used as a proxy for the
;;; type during presentation method dispatch.  Otherwise this holds useful 
;;; info about the class.
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass presentation-type ()
    ((type-name :accessor type-name :initarg :type-name
		:documentation "The name assigned to the presentation
type, as opposed to the name constructed for the class")
     (ptype-specializer :accessor ptype-specializer :initarg :ptype-specializer
			:documentation "The symbol to use as the specializer for
this type in presentation methods")
     (parameters :accessor parameters :initarg :parameters :initform nil)
     (parameters-lambda-list :accessor parameters-lambda-list
			     :initarg :parameters-lambda-list
			     :initform nil
			     :documentation "The parameters lambda list, altered for use in destructuring bind")
     (options :accessor options :initarg :options :initform nil)
     (options-lambda-list :accessor options-lambda-list
			  :initarg :options-lambda-list
			  :initform nil
			  :documentation "The options lambda list,
altered for use in destructuring bind")
     (inherit-from :accessor inherit-from
		   :initarg :inherit-from
		   :documentation "Inherit-from form with dummys substituted")
     (inherit-from-function :accessor inherit-from-function
			    :initarg :inherit-from-function
			    :documentation "Function that returns the inherit-from form")
     (description :accessor description :initarg :description)
     (history :accessor history :initarg :history :initform nil
	      :documentation "Who knows?")
     (parameters-are-types :accessor parameters-are-types 
			   :initarg :parameters-are-types
			   :initform nil)
     (expansion-function :accessor expansion-function
			 :initarg :expansion-function
			 :documentation "A function which expands the typespec
fully, including defaulting parameters and options.")))

  (defmethod initialize-instance :after ((obj presentation-type) &key)
    (unless (slot-boundp obj 'ptype-specializer)
      (setf (slot-value obj 'ptype-specializer)
	    (make-presentation-type-name (slot-value obj 'type-name)))))

  (defclass presentation-type-class (presentation-type standard-class)
    ())

  (defmethod clim-mop:validate-superclass ((class presentation-type-class) 
					   (super standard-class))
    t)

  (defclass clos-presentation-type (presentation-type)
    ((clos-class :accessor clos-class :initarg :clos-class
		 :documentation "Holds the class object of the CLOS class of this presentation type")))

  (defmethod initialize-instance :after ((obj clos-presentation-type)
					 &key (ptype-specializer
					       nil
					       ptype-specializer-p))
	     (declare (ignore ptype-specializer))
	     (unless ptype-specializer-p
	       (setf (slot-value obj 'ptype-specializer)
		     (slot-value obj 'type-name))))

  (defun make-presentation-type-name (name)
    (intern (format nil "(presentation-type ~A::~A)"
		    (package-name (symbol-package name))
		    (symbol-name name))
	    :clim-internals))

  (defun transform-parameters-lambda-list (ll)
    "Change the destructuring  lambda list so that any optional or key variable
that has no default is supplied with '*"
    (when (atom ll)
      (return-from transform-parameters-lambda-list ll))
    (let ((state 'required))
      (loop for lambda-var in ll
	    collect
	    (cond ((member lambda-var lambda-list-keywords :test #'eq)
		   (setq state lambda-var)
		   lambda-var)
		  ((eq state '&optional)
		   (if (atom lambda-var)
		       `(,lambda-var '*)
		       (cons (transform-parameters-lambda-list (car lambda-var))
			     (or (cdr lambda-var) '('*)))))
		  ((eq state '&key)
		   (cond ((atom lambda-var)
			  `(,lambda-var '*))
			 ((atom (car lambda-var))
			  (cons (car lambda-var)
				(or (cdr lambda-var) '('*))))
			 (t (destructuring-bind
				  ((var pattern)
				   &optional (default nil default-p)
				   &rest supplied-tail)
				lambda-var
			      `((,var ,(transform-parameters-lambda-list
					pattern))
				,(if default-p
				     default
				     '*)
				,@supplied-tail)))))
		  ((member state '(required &rest &body &whole))
		   (when (eq state '&whole)
		     (setq state 'required))
		   (transform-parameters-lambda-list lambda-var))
		  (t lambda-var)))))

;;; I'm not using COLLECT because I'm superstitious about doing a
;;; LOOP-FINISH out of the collect form.  Call me paranoid.
  (defun fake-params-args (ll)
    (let ((state 'required)
	  (result nil))
      (loop for lambda-var in ll
	    do (cond ((member lambda-var lambda-list-keywords :test #'eq)
		      (setq state lambda-var)
		      (unless (eq state '&whole)
			(loop-finish)))
		     ((eq state '&whole)
		      (setq state 'required))
		     ((atom lambda-var)
		      (push (gensym (symbol-name lambda-var)) result))
		     (t (push (fake-params-args lambda-var) result)))
	    finally (return (nreverse result)))))

;;; Yet another variation on a theme...

  (defun get-all-params (ll)
    (unless ll
      (return-from get-all-params nil))
    (when (atom ll)
      (return-from get-all-params (list ll)))
    (let ((state 'required))
      (loop for arg in ll
	    append (cond ((member arg lambda-list-keywords :test #'eq)
			  (setq state arg)
			  nil)
			 ((eq state 'required)
			  (get-all-params arg))
			 ((or (eq state '&optional) (eq state '&aux))
			  (if (atom arg)
			      (list arg)
			      (get-all-params (car arg))))
			 ((eq state '&key)
			  (cond ((atom arg)
				 (list arg))
				((atom (car arg))
				 (list (car arg)))
				(t (get-all-params (cadar arg)))))
			 ((member state '(required &rest &body &whole))
			  (when (eq state '&whole)
			    (setq state 'required))
			  (get-all-params arg))
			 (t nil)))))

;;; ...And another.  Given a lambda list, return a form that replicates the
;;; structure of the argument with variables filled in.

  (defun map-over-lambda-list (function ll
			       &key (pass-lambda-list-keywords nil))
    (declare (ignore function pass-lambda-list-keywords))
    (unless ll
      (return-from map-over-lambda-list nil))
    (when (atom ll)
      (return-from map-over-lambda-list ll))
    (loop for args-tail = ll then (cdr args-tail)))

  (defun make-keyword (sym)
    (intern (symbol-name sym) :keyword))

  (defun cull-keywords (keys prop-list)
    (let ((plist (copy-list prop-list)))
      (loop for key in keys
	    do (remf plist key))
      plist))

  (defun recreate-lambda-list (ll)
    "Helper function.  Returns a form that, when evaluated inside a
DESTRUCTURING-BIND using ll, recreates the argument list with all defaults
filled in."
    (unless ll
      (return-from recreate-lambda-list nil))
    (when (atom ll)
      (return-from recreate-lambda-list ll))
    (let ((state 'required)
	  (rest-var nil)
	  (has-keys nil)
	  (keys nil)
	  (allow-other-keys nil))
      (loop for arg in ll
	    append (cond ((member arg lambda-list-keywords :test #'eq)
			  (setq state arg)
			  (when (eq arg '&key)
			    (setq has-keys t))
			  (when (eq arg '&allow-other-keys)
			    (setq allow-other-keys t))
			  nil)
			 ((eq state '&whole)
			  nil)
			 ((eq state 'required)
			  (list (recreate-lambda-list arg)))
			 ((eq state '&optional)
			  (if (atom arg)
			      (list arg)
			      (list (recreate-lambda-list (car arg)))))
			 ((or (eq state '&rest) (eq state '&body))
			  (setq rest-var arg)
			  nil)
			 ((eq state '&key)
			  (let ((key nil)
				(var nil))
			    (cond ((atom arg)
				   (setq key (make-keyword arg)
					 var arg))
				  ((atom (car arg))
				   (setq key (make-keyword (car arg))
					 var (car arg)))
				
				  (t (destructuring-bind
					   ((keyword pattern) &rest tail)
					 arg
				       (declare (ignore tail))
				       (setq key keyword
					     var (recreate-lambda-list
						  pattern)))))
			    (push key keys)
			    (list key var)))
			 (t nil))
	    into result-form
	    finally (cond ((or (not rest-var)
			       (and has-keys
				    (not allow-other-keys)))
			   (return `(list ,@result-form)))
			  ((not has-keys)
			   (return `(list* ,@result-form ,rest-var)))
			  (t (return `(list* ,@result-form
				       (cull-keywords ',(nreverse keys)
					,rest-var))))))))

  (defun transform-options-lambda-list (ll)
    "Return a legal lambda list given an options specification"
    (let ((descriptionp nil))
      (loop for spec in ll
	    collect (if (atom spec)
			(progn
			  (when (eq (make-keyword spec) :description)
			    (setq descriptionp t))
			  spec)
			(progn
			  (let ((key (if (atom (car spec))
					 (make-keyword (car spec))
					 (caar spec))))
			    (when (eq key :description)
			      (setq descriptionp t)))
			  (ldiff spec (cdddr spec))))
	    into specs
	    finally (return `(&key
			      ,@specs
			      ,@(unless descriptionp
					`(((:description ,(gensym))))))))))


;;; External function
  (defun presentation-type-name (type)
    (cond ((atom type)
	   type)
	  ((atom (car type))
	   (car type))
	  (t (caar type))))

  (defun decode-parameters (type)
    (cond ((atom type)
	   nil)
	  ((atom (car type))
	   (cdr type))
	  (t (cdar type))))

  (defun decode-options (type)
    (if (or (atom type) (atom (car type)))
	nil
	(cdr type)))

  (defun make-inherit-from-lambda (params-ll options-ll form)
    (let ((type (gensym "TYPE")))
      `(lambda (,type)
	(destructuring-bind ,params-ll (decode-parameters ,type)
	  (declare (ignorable ,@(get-all-params params-ll)))
	  (destructuring-bind ,options-ll (decode-options ,type)
	    (declare (ignorable ,@(get-all-params options-ll)))
	    ,form)))))
) ; eval-when

(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro with-presentation-type-decoded ((name
					   &optional (params nil params-p)
					             (options nil options-p))
					  type &body body)
  (let ((type-var (gensym "TYPE-VAR")))
    `(let* ((,type-var ,type)
	    (,name (presentation-type-name ,type-var))
	    ,@(and params-p `((,params (decode-parameters ,type-var))))
	    ,@(and options-p `((,options (decode-options ,type-var)))))
       ,@body)))
)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-expansion-lambda (params-ll options-ll)
    (let ((params-form (recreate-lambda-list params-ll))
	  (options-form (recreate-lambda-list options-ll))
	  (parameters (gensym))
	  (options (gensym)))
      `(lambda (typespec)
	 (with-presentation-type-decoded (name ,parameters ,options)
	   typespec
	   (make-type-spec name
	    (destructuring-bind ,params-ll
		,parameters
	      ,params-form)
	    (destructuring-bind ,options-ll
		,options
	      ,options-form)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *presentation-type-table* (make-hash-table :test #'eq)))

(defgeneric get-ptype-metaclass (type))

(defmethod get-ptype-metaclass ((type symbol))
  (let ((maybe-meta (gethash type *presentation-type-table*)))
    (if maybe-meta
	(get-ptype-metaclass maybe-meta)
	(find-class type nil))))

(defmethod get-ptype-metaclass ((type presentation-type-class))
  type)

(defmethod get-ptype-metaclass ((type clos-presentation-type))
  (clos-class type))

(defun prototype-or-error (name)
  (let ((ptype-meta (get-ptype-metaclass name)))
    (unless ptype-meta
      (error "~S is an unknown presentation type" name))
    (unless (clim-mop:class-finalized-p ptype-meta)
      (clim-mop:finalize-inheritance ptype-meta))
    (or (clim-mop:class-prototype ptype-meta)
      (error "Couldn't find a prototype for ~S" name))))
  
(defun get-ptype (name)
  (or (gethash name *presentation-type-table*)
      (let ((meta (find-class name nil)))
	(and (typep meta 'standard-class)
	     meta))))

(defgeneric presentation-ptype-supers (type)
  (:documentation "Gets a list of the presentation type objects for those
supertypes of TYPE that are presentation types"))

(defmethod presentation-ptype-supers ((type symbol))
  (let ((ptype (gethash type *presentation-type-table*)))
    (if ptype
	(presentation-ptype-supers ptype)
	nil)))

(defmethod presentation-ptype-supers ((type presentation-type-class))
  (mapcan #'(lambda (class)
	      (typecase class
		(presentation-type
		 (list class))
		(standard-class
		 (let ((clos-ptype (gethash (clim-mop:class-name class)
					    *presentation-type-table*)))
		   (if clos-ptype
		       (list clos-ptype)
		       nil)))
		(t
		 nil)))
	  (clim-mop:class-direct-superclasses type)))

(defmethod presentation-ptype-supers ((type clos-presentation-type))
  (presentation-ptype-supers (clos-class type)))

(defmethod presentation-ptype-supers ((type standard-class))
  (mapcan #'(lambda (class)
              (let ((ptype (gethash (clim-mop:class-name class)
                                    *presentation-type-table*)))
                (and ptype (list ptype))))
          (clim-mop:class-direct-superclasses type)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod ptype-specializer ((type symbol))
    (let ((ptype (gethash type *presentation-type-table*)))
      (if ptype
	  (ptype-specializer ptype)
	  (ptype-specializer (find-class type)))))

  (defmethod ptype-specializer ((type standard-class))
    (clim-mop:class-name type)))

;;; XXX This is total bullshit, but works with our patched definition of
;;; defclass in CMUCL.  I think we need to patch defclass in every
;;; implementation to record a CLOS class at compiletime.  On the other hand,
;;; I think we can assume that if a CLOS class exists at compile time, it will
;;; exist at load/run time too.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun compile-time-clos-p (name)
    (let ((meta (find-class name nil)))
      (and meta
	   (typep meta 'standard-class))))

  (defun make-default-description (name)
    "Create a description string from the type name"
    (let ((downcase-name (string-downcase name)))
      (setq downcase-name (nsubstitute-if-not #\Space
					      #'alphanumericp
					      downcase-name))
      (string-trim " " downcase-name)))

  (defun record-presentation-type (name parameters params-ll options options-ll
				   inherit-from-func description history
				   parameters-are-types
				   compile-time-p
				   supers expansion-func)
    (let* ((fake-name (make-presentation-type-name name))
	   (ptype-class-args (list :type-name name
				   :parameters parameters
				   :parameters-lambda-list params-ll
				   :options options
				   :options-lambda-list options-ll
				   :inherit-from-function inherit-from-func
				   :description description
				   :history history
				   :parameters-are-types parameters-are-types
				   :expansion-function expansion-func))
	   (ptype-meta
	    (if compile-time-p
		(apply #'make-instance
		       (if (compile-time-clos-p name)
			   'clos-presentation-type
			   'presentation-type)
		       ptype-class-args)
		(let* ((clos-meta (find-class name nil))
		       (closp (typep clos-meta 'standard-class)))
		  (if closp
		      (apply #'make-instance 'clos-presentation-type
			     :clos-class clos-meta
			     ptype-class-args)
		      (let ((directs (mapcar #'(lambda (super)
						 (or (get-ptype-metaclass super)
						     super))
					     supers)))
			(apply #'clim-mop:ensure-class fake-name
			       :name fake-name
			       :metaclass 'presentation-type-class
			       :direct-superclasses directs
			       ptype-class-args)))))))
      (setf (gethash name *presentation-type-table*) ptype-meta)
      ptype-meta))
  ); eval-when

(defgeneric massage-type-for-super (type-name super-name type-spec)
  (:documentation "translate TYPE-SPEC from that of TYPE-NAME to one
suitable for SUPER-NAME"))

;;; The default: there ain't no direct specification

(defmethod massage-type-for-super ((type-name t) (super-name t) type-spec)
  (declare (ignore type-spec))
  (values nil nil))

;;; Load-time actions for define-presentation-type
(defmacro %define-presentation-type (name parameters params-ll
				     options options-ll
				     inherit-from inherit-from-lambda
				     description history parameters-are-types)
  (declare (ignore inherit-from))
  (let* ((inherit-from-func (coerce inherit-from-lambda 'function))
	 (inherit-typespec (funcall inherit-from-func
				    (cons name (fake-params-args params-ll))))
	 (superclasses (if inherit-typespec
			   (with-presentation-type-decoded
			       (super-name super-params)
			     inherit-typespec
			     (if (eq super-name 'and)
				 (mapcar #'presentation-type-name super-params)
				 (list super-name)))
			   nil))
	 (expansion-lambda (make-expansion-lambda params-ll options-ll)))
    `(progn
       (record-presentation-type ',name ',parameters ',params-ll ',options
				   ',options-ll #',inherit-from-lambda
				   ',description ',history
				   ',parameters-are-types
				   nil ',superclasses
	                           #',expansion-lambda)
       ,@(cond ((eq (presentation-type-name inherit-typespec) 'and)
		(loop for super in superclasses
		    for i from 0
		    append (unless (or (not (atom super))
				       (eq super 'satisfies)
				       (eq super 'not))
			     `((defmethod massage-type-for-super
				   ((type-name (eql ',name))
				    (super-name (eql ',super))
				    type)
				 (values (nth ,i
					      (cdr (,inherit-from-lambda
						    type)))
					 t))))))
	       (superclasses
		`((defmethod massage-type-for-super
		      ((type-name (eql ',name))
		       (super-name (eql ',(car superclasses)))
		       type)
		    (values (,inherit-from-lambda type) t))))
	       (t nil)))))

(defmacro define-presentation-type (name parameters
				    &key options inherit-from
				    (description
				     (make-default-description name))
				    (history t)
				    parameters-are-types)
  (let* ((params-ll (transform-parameters-lambda-list parameters))
	 (options-ll (transform-options-lambda-list options))
	 (inherit-from-func (make-inherit-from-lambda params-ll
						      options-ll
						      inherit-from)))
    `(progn
       (eval-when (:compile-toplevel)
	 (record-presentation-type ',name ',parameters ',params-ll ',options
				   ',options-ll #',inherit-from-func
				   ',description ',history
				   ',parameters-are-types
				   t nil nil))
       (eval-when (:load-toplevel :execute)
	 (%define-presentation-type ,name ,parameters ,params-ll
				    ,options ,options-ll
				    ,inherit-from ,inherit-from-func
				    ,description ,history
				    ,parameters-are-types)))))


(defun presentation-type-parameters (type-name &optional env)
  (declare (ignore env))
  (let ((ptype (gethash type-name *presentation-type-table*)))
    (unless ptype
      (error "~S is not the name of a presentation type" type-name))
    (parameters ptype)))

(defun presentation-type-options (type-name &optional env)
  (declare (ignore env))
  (let ((ptype (gethash type-name *presentation-type-table*)))
    (unless ptype
      (error "~S is not the name of a presentation type" type-name))
    (options ptype)))

(defmacro with-presentation-type-parameters ((type-name type) &body body)
  (let ((ptype (gethash type-name *presentation-type-table*)))
    (unless ptype
      (error "~S is not a presentation type name." type-name))
    (let* ((params-ll (parameters-lambda-list ptype))
	   (params (gensym "PARAMS"))
	   (type-var (gensym "TYPE-VAR"))
	   (ignorable-vars (get-all-params params-ll)))
      `(let ((,type-var ,type))
	 (unless (eq ',type-name (presentation-type-name ,type-var))
	   (error "Presentation type specifier ~S does not match the name ~S"
		  ,type-var
		  ',type-name))
	 (let ((,params (decode-parameters ,type-var)))
	   (declare (ignorable ,params))
	   (destructuring-bind ,params-ll ,params
	     (declare (ignorable ,@ignorable-vars))
	     ,@body))))))

(defmacro with-presentation-type-options ((type-name type) &body body)
  (let ((ptype (gethash type-name *presentation-type-table*)))
    (unless ptype
      (error "~S is not a presentation type name." type-name))
    (let* ((options-ll (options-lambda-list ptype))
	   (options (gensym "OPTIONS"))
	   (type-var (gensym "TYPE-VAR"))
	   (ignorable-vars (get-all-params options-ll)))
      `(let ((,type-var ,type))
	 (unless (eq ',type-name (presentation-type-name ,type-var))
	   (error "Presentation type specifier ~S does not match the name ~S"
		  ,type-var
		  ',type-name))
	 (let ((,options (decode-options ,type-var)))
	   (declare (ignorable ,options))
	   (destructuring-bind ,options-ll ,options
	     (declare (ignorable ,@ignorable-vars))
	     ,@body))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *presentation-type-abbreviations* (make-hash-table :test #'eq)))

(defmacro define-presentation-type-abbreviation (name parameters
						 equivalent-type
						 &key options)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *presentation-type-abbreviations*)
	   #',(make-inherit-from-lambda
	       (transform-parameters-lambda-list parameters)
	       (transform-options-lambda-list options)
	       equivalent-type))))

(defun make-type-spec (name parameters options)
  (cond (options
	 `((,name ,@parameters) ,@options))
	(parameters
	 `(,name ,@parameters))
	(t name)))

(defun expand-presentation-type-abbreviation-1 (type &optional env)
  (flet ((expand-list (specs)
	   (loop with expand-any-p = nil
		 for spec in specs
		 collect (multiple-value-bind (expansion expanded)
			     (expand-presentation-type-abbreviation-1 spec env)
			   (setq expand-any-p (or expand-any-p expanded))
			   expansion)
		 into new-params
		 finally (return (if expand-any-p
				     (values new-params t)
				     (values specs nil))))))
    (with-presentation-type-decoded (name parms options)
      type
      (case name
	((and or sequence sequence-enumerated)
	 (multiple-value-bind (expansion expanded)
	     (expand-list parms)
	   (if expanded
	       (values (make-type-spec name expansion options) t)
	       (values type nil))))
	(t (let* ((expander (gethash name *presentation-type-abbreviations*)))
	     (flet ((copy-description (expanded-typespec)
		      (with-presentation-type-decoded (expand-name
						       expand-params
						       expand-options)
			expanded-typespec
			(let ((description (getf options :description))
			      (expand-desc (getf expand-options :description)))
			  (if (and description
				   (null expand-desc))
			      (make-type-spec expand-name
					      expand-params
					      `(:description ,description
						,@expand-options))
			      expanded-typespec)))))
	       (if expander
		 (values (copy-description (funcall expander type)) t)
		 (values type nil)))))))))


(defun expand-presentation-type-abbreviation (type &optional env)
  (let ((expand-any-p nil))
    (loop
     (multiple-value-bind (expansion expanded)
	 (expand-presentation-type-abbreviation-1 type env)
       (if expanded
	   (progn
	     (setq expand-any-p t)
	     (setq type expansion))
	   (return (values type expand-any-p)))))))

(defun make-presentation-type-specifier (name-and-params &rest options)
  (with-presentation-type-decoded (name)
    name-and-params
    (let ((ptype (gethash name *presentation-type-table*)))
      (unless ptype
	(return-from make-presentation-type-specifier name-and-params))
      (with-presentation-type-decoded (name parameters defaults)
	(funcall (expansion-function ptype) name-and-params)
	(declare (ignore name parameters))
	(loop for (key val) on options by #'cddr
	      for default = (getf defaults key)
	      unless (equal val default)
	      nconc (list key val) into needed-options
	      finally (return (if needed-options
				  `(,name-and-params ,@needed-options)
				  name-and-params)))))))



;;; Presentation methods.  The basic dispatch is performed via CLOS
;;; instances that are standins for the presentation types.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass presentation-generic-function ()
    ((generic-function-name :accessor generic-function-name
			    :initarg :generic-function-name)
     (lambda-list :accessor lambda-list :initarg :lambda-list)
     (type-key-arg :accessor type-key-arg :initarg :type-key-arg)
     (parameters-arg :accessor parameters-arg :initarg :parameters-arg
		     :initform nil)
     (options-arg :accessor options-arg :initarg :options-arg :initform nil)
     (type-arg-position :accessor type-arg-position
			:initarg :type-arg-position)))

  (defvar *presentation-gf-table* (make-hash-table :test #'eq))
) ; eval-when

;;; The hard part of presentation methods: translating the type specifier for
;;; superclasses.
;;;

(defmethod type-name ((type standard-class))
  (clim-mop:class-name type))

(defmethod expansion-function ((type standard-class))
  #'(lambda (typespec)
      (with-presentation-type-decoded (name)
	typespec
	name)))

(defmethod presentation-ptype-supers ((type standard-class))
  (mapcan #'(lambda (class)
	      (let ((ptype (gethash (clim-mop:class-name class)
				    *presentation-type-table*)))
		(and ptype (list ptype))))
	  (clim-mop:class-direct-superclasses type)))

(defun translate-specifier-for-type (type-name super-name specifier)
  (when (eq type-name super-name)
    (return-from translate-specifier-for-type (values specifier t)))
  (multiple-value-bind (translation found)
      (massage-type-for-super type-name super-name specifier)
    (when found
      (return-from translate-specifier-for-type (values translation t))))
  (loop for super in (presentation-ptype-supers type-name)
	do (multiple-value-bind (translation found)
	       (translate-specifier-for-type (type-name super)
					     super-name
					     (massage-type-for-super
					      type-name
					      (type-name super)
					      specifier))
	     (when found
	       (return-from translate-specifier-for-type (values translation
								 t)))))
  (values super-name nil))

;;; XXX can options be specified without parameters?  I think not.
(defmacro define-presentation-generic-function (generic-function-name
						presentation-function-name
						lambda-list
						&rest options)
    (let ((type-key-arg (car lambda-list))
	  (parameters-arg (cadr lambda-list))
	  (options-arg (caddr lambda-list)))
      (unless (or (eq type-key-arg 'type-key) (eq type-key-arg 'type-class))
	(error "The first argument in a presentation generic function must be
type-key or type-class"))
      (unless (eq parameters-arg 'parameters)
	(setq parameters-arg nil))
      (unless (eq options-arg 'options)
	(setq options-arg nil))
      (let* ((gf-lambda-list (cons type-key-arg
				   (cond (options-arg
					  (cdddr lambda-list))
					 (parameters-arg
					  (cddr lambda-list))
					 (t (cdr lambda-list)))))
	     ;; XXX should check that it's required
	     (type-arg-pos (position 'type gf-lambda-list)))
	(unless type-arg-pos
	  (error "type must appear as an argument in a presentation generic
function lambda list"))
	`(progn
	   (eval-when (:compile-toplevel :load-toplevel :execute)
	     (setf
	      (gethash ',presentation-function-name *presentation-gf-table*)
	      (make-instance 'presentation-generic-function
			     :generic-function-name ',generic-function-name
			     :lambda-list ',lambda-list
			     :type-key-arg ',type-key-arg
			     :parameters-arg ',parameters-arg
			     :options-arg ',options-arg
			     :type-arg-position ,type-arg-pos)))
	   (defgeneric ,generic-function-name ,gf-lambda-list ,@options)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-method-body (args)
    (loop for arglist on args
	  for (arg) = arglist
	  while (atom arg)
	  collect arg into qualifiers
	  finally (if (eq (caadr arglist) 'declare)
		      (return (values qualifiers
				      arg
				      (cadr arglist)
				      (cddr arglist)))
		      (return (values qualifiers arg nil (cdr arglist)))))))

(defmacro define-presentation-method (name &rest args)
  (when (eq name 'presentation-subtypep)
    ;; I feel so unclean!
    (return-from define-presentation-method
      `(define-subtypep-method ,@args)))
  (let ((gf (gethash name *presentation-gf-table*)))
    (unless gf
      (error "~S is not a presentation generic function" name))
    (with-accessors ((parameters-arg parameters-arg)
		     (options-arg options-arg))
      gf
      (multiple-value-bind (qualifiers lambda-list decls body)
	  (parse-method-body args)
	(let ((type-arg (nth (1- (type-arg-position gf)) lambda-list)))
	  (unless (consp type-arg)
	    (error "Type argument in presentation method must be specialized"))
	  (unless (eq (car type-arg)  'type)
	    (error "Type argument mismatch with presentation generic function
 definition"))
	  (destructuring-bind (type-var type-name) type-arg
	    (let* ((method-ll `((,(type-key-arg gf)
				 ,(ptype-specializer type-name))
				,@(copy-list lambda-list)))
		   (real-body body)
		   (massaged-type (gensym "MASSAGED-TYPE")))
	      (when options-arg
		(setq real-body
		      `((let ((,options-arg (decode-options ,massaged-type)))
			  (declare (ignorable ,options-arg))
			  (with-presentation-type-options (,type-name
							   ,massaged-type)
			    ,@real-body)))))
	      (when parameters-arg
		(setq real-body
		      `((let ((,parameters-arg (decode-parameters
						,massaged-type)))
			  (declare (ignorable ,parameters-arg))
			  (with-presentation-type-parameters (,type-name
							      ,massaged-type)
			    ,@real-body)))))
	      (when (or options-arg parameters-arg)
		(setq real-body
		      `((let ((,massaged-type (translate-specifier-for-type
					       (type-name (class-of
							   ,(type-key-arg gf)))
					       ',type-name
					       ,type-var)))
			  ,@real-body))))
	      (setf (nth (type-arg-position gf) method-ll) type-var)
	      `(defmethod ,(generic-function-name gf) ,@qualifiers ,method-ll
		 ,decls
		,@real-body))))))))


(defmacro define-default-presentation-method (name &rest args)
  (let ((gf (gethash name *presentation-gf-table*)))
    (unless gf
      (error "~S is not a presentation generic function" name))
    (multiple-value-bind (qualifiers lambda-list decls body)
	(parse-method-body args)
      `(defmethod ,(generic-function-name gf) ,@qualifiers (,(type-key-arg gf)
							    ,@lambda-list)
	 (declare (ignorable ,(type-key-arg gf))
		  ,@(cdr decls))
	 ,@body))))

(defun %funcall-presentation-generic-function (name gf type-arg-position
					       &rest args)
  (declare (ignore name))
  (let* ((type-spec (nth (1- type-arg-position) args))
	 (ptype-name (presentation-type-name type-spec)))
    (apply gf (prototype-or-error ptype-name) args)))


(defmacro funcall-presentation-generic-function (name &rest args)
  (let ((gf (gethash name *presentation-gf-table*)))
    (unless gf
      (error "~S is not a presentation generic function" name))
    `(%funcall-presentation-generic-function ',name
					     #',(generic-function-name gf)
					     ,(type-arg-position gf)
					     ,@args)))

(defmacro apply-presentation-generic-function (name &rest args)
  (let ((gf (gethash name *presentation-gf-table*)))
    (unless gf
      (error "~S is not a presentation generic function" name))
    `(apply #'%funcall-presentation-generic-function ',name
	    #',(generic-function-name gf)
	    ,(type-arg-position gf)
	    ,@args)))

(define-presentation-generic-function  %presentation-typep presentation-typep
  (type-key parameters object type))

(defun presentation-typep (object type)
  (with-presentation-type-decoded (name parameters)
    type
    (when (null parameters)
      (let ((clos-class (find-class name nil)))	; Don't error out.
	(when (and clos-class (typep clos-class 'standard-class))
	  (return-from presentation-typep (typep object name)))))
    (funcall-presentation-generic-function presentation-typep object type)))

;;; Not defined as a generic function, but what the hell.

(defgeneric presentation-type-of (object))

(defmethod presentation-type-of (object)
  (declare (ignore object))
  'expression)

(defmethod presentation-type-of ((object standard-object))
  (let* ((name (clim-mop:class-name (class-of object)))
	 (ptype-entry (gethash name *presentation-type-table*)))
    (unless ptype-entry
      (return-from presentation-type-of name))
    ;; Does the type have required parameters?  If so, we can't use it...
    (let ((parameter-ll (parameters-lambda-list ptype-entry)))
      (when (eq (car parameter-ll) '&whole)
	(setq parameter-ll (cddr parameter-ll)))
      (if (member (car parameter-ll) lambda-list-keywords)
	  name
	  (call-next-method)))))

(define-presentation-generic-function
    %map-over-presentation-type-supertypes
    map-over-presentation-type-supertypes
  (type-key function type))

;;; A bit of magic: define the method for presentation and clos types

(defmethod %map-over-presentation-type-supertypes ((type-key standard-object)
						   function
						   type)
    (let ((type-name (presentation-type-name type)))
    (funcall function type-name (funcall (expansion-function
					  (class-of type-key))
					  type))
    (loop for meta in (cdr (clim-mop:class-precedence-list (class-of
							    type-key)))
	  when (typep meta 'standard-class)
	  do (let* ((super-name (type-name meta))
		    (supertype (translate-specifier-for-type type-name
							     super-name
							     type)))
	       (funcall function super-name (funcall (expansion-function meta)
						     supertype))))
    (funcall function t t))
  nil)

(defun map-over-presentation-type-supertypes (function type)
  (funcall-presentation-generic-function map-over-presentation-type-supertypes
					 function
					 type))

(define-presentation-generic-function
    %presentation-subtypep
    presentation-subtypep
  (type-key type putative-supertype))

;;; The semantics of the presentation method presentation-subtypep are truly
;;; weird; method combination is in effect disabled.  So, the methods have to
;;; be eql methods.

(defmacro define-subtypep-method (&rest args)
  (let ((gf (gethash 'presentation-subtypep *presentation-gf-table*)))
    (multiple-value-bind (qualifiers lambda-list decls body)
	(parse-method-body args)
      (let ((type-arg (nth (1- (type-arg-position gf)) lambda-list)))
	
	(unless (consp type-arg)
	  (error "Type argument in presentation method must be specialized"))
	(unless (eq (car type-arg)  'type)
	  (error "Type argument mismatch with presentation generic function
 definition"))
	(destructuring-bind (type-var type-name) type-arg
	  (let ((method-ll `((,(type-key-arg gf)
			      (eql (prototype-or-error ',type-name)))
			     ,@(copy-list lambda-list))))
	    (setf (nth (type-arg-position gf) method-ll) type-var)
	    `(defmethod %presentation-subtypep ,@qualifiers ,method-ll
	      (declare (ignorable ,(type-key-arg gf))
		       ,@(cdr decls))
	      ,@body)))))))

(defun presentation-subtypep (type maybe-supertype)
  (when (equal type maybe-supertype)
    (return-from presentation-subtypep (values t t)))
  (let ((super-name (presentation-type-name maybe-supertype)))
    (map-over-presentation-type-supertypes
     #'(lambda (name massaged)
	 (when (eq name super-name)
	   (return-from presentation-subtypep
	     (funcall-presentation-generic-function presentation-subtypep
						    massaged
						    maybe-supertype))))
     type))
  (values nil t))

(define-default-presentation-method presentation-subtypep
    (type maybe-supertype)
  (with-presentation-type-decoded (name params)
    type
    (declare (ignore name))
    (with-presentation-type-decoded (super-name super-params)
      maybe-supertype
      (declare (ignore super-name))
      (if (equal params super-params)
	  (values t t)
	  (values nil nil)))))

(defun default-describe-presentation-type (description stream plural-count)
  (if (symbolp description)
      (setq description (make-default-description (symbol-name description))))
  (cond ((numberp plural-count)
	 (format stream "~D ~A~P" plural-count description plural-count))
	(plural-count
	 (format stream "~As" description))
	(t (format stream "~:[a~;an~] ~A"
		   (find (char description 0) "aeiouAEIOU")
		   description))))

(define-presentation-generic-function %describe-presentation-type
    describe-presentation-type
  (type-key parameters options type stream plural-count ))

(define-default-presentation-method describe-presentation-type
    (type stream plural-count)
  (with-presentation-type-decoded (name parameters options)
    type
    (declare (ignore name parameters))
    (let ((description (or (getf options :description)
			   (description (class-of type-key)))))
      (default-describe-presentation-type description
	                                  stream
	                                  plural-count))))

(defun describe-presentation-type (type
				   &optional
				   (stream *standard-output*)
				   (plural-count 1))
  (flet ((describe-it (stream)
	   (funcall-presentation-generic-function describe-presentation-type
						  type
						  stream
						  plural-count)))
    (if stream
	(describe-it stream)
	(with-output-to-string (s)
	  (describe-it s)))))

;;; XXX The spec calls out that the presentation generic function has keyword
;;; arguments acceptably and for-context-type, but the examples I've seen don't
;;; mention them at all in the methods defined for present.  So, leave them out
;;; of the generic function lambda list...
(define-presentation-generic-function %present present
    (type-key parameters options object type stream view
     &key &allow-other-keys))

(defmacro with-output-as-presentation ((stream object type
				       &rest key-args
				       &key modifier single-box
				       (allow-sensitive-inferiors t)
				       parent
				       (record-type ''standard-presentation)
				       &allow-other-keys)
				       &body body)
  (declare (ignore parent single-box modifier))
  (when (eq stream t)
    (setq stream '*standard-output*))
  (let ((output-record (gensym))
	(invoke-key-args (cull-keywords '(:record-type
					  :allow-sensitive-inferiors)
					key-args)))
    (declare (ignore invoke-key-args))
     `(flet ((continuation (,stream ,output-record)
	      (declare (ignore ,output-record))
	      (let ((*allow-sensitive-inferiors*
		     (if *allow-sensitive-inferiors*
			 ,allow-sensitive-inferiors
			 nil)))
		,@body)))
	(if (output-recording-stream-p ,stream)
	    (invoke-with-new-output-record
	     ,stream #'continuation ,record-type
	     :object ,object
	     :type (expand-presentation-type-abbreviation ,type)
	     ,@key-args)
	    (funcall #'continuation ,stream nil)))))


(defun present (object &optional (type (presentation-type-of object))
		&key
		(stream *standard-output*)
		(view (stream-default-view stream))
		modifier
		acceptably
		(for-context-type nil for-context-type-p)
		single-box
		(allow-sensitive-inferiors t)
		(sensitive t)
		(record-type 'standard-presentation))
  (let* ((real-type (expand-presentation-type-abbreviation type))
	 (context-type (if for-context-type-p
			   (expand-presentation-type-abbreviation
			    for-context-type)
			   real-type)))
    (stream-present stream object real-type
		    :view view :modifier modifier :acceptably acceptably
		    :for-context-type context-type :single-box single-box
		    :allow-sensitive-inferiors allow-sensitive-inferiors
		    :sensitive sensitive
		    :record-type record-type)))

(defgeneric stream-present (stream object type
			    &key view modifier acceptably for-context-type
			    single-box allow-sensitive-inferiors sensitive
			    record-type))

(defmethod stream-present ((stream output-recording-stream) object type
			   &key
			   (view (stream-default-view stream))
			   modifier
			   acceptably
			   (for-context-type type)
			   single-box
			   (allow-sensitive-inferiors t)
			   (sensitive t)
			   (record-type 'standard-presentation))
  (let ((*allow-sensitive-inferiors* (if *allow-sensitive-inferiors*
					 sensitive
					 nil)))
    (with-output-as-presentation (stream object type
				  :view view
				  :modifier modifier
				  :single-box single-box
				  :allow-sensitive-inferiors
				  allow-sensitive-inferiors
				  :record-type record-type)
      (funcall-presentation-generic-function
       present object type stream view
       :acceptably acceptably :for-context-type for-context-type))))

;;; Should work well enough on non-CLIM streams...
(defmethod stream-present (stream object type
			   &key
			   (view +textual-view+)
			   modifier
			   acceptably
			   (for-context-type type)
			   single-box
			   (allow-sensitive-inferiors t)
			   (sensitive t)
			   (record-type 'standard-presentation))
  (declare (ignore modifier single-box allow-sensitive-inferiors sensitive
		   record-type))
  (funcall-presentation-generic-function
   present object type stream view
   :acceptably acceptably :for-context-type for-context-type)
  nil)

(defun present-to-string (object &optional (type (presentation-type-of object))
			  &key (view +textual-view+)
			  acceptably
			  (for-context-type nil for-context-type-p)
			  (string nil stringp)
			  (index 0 indexp))
  (let* ((real-type (expand-presentation-type-abbreviation type))
	 (context-type (if for-context-type-p
			   (expand-presentation-type-abbreviation
			    for-context-type)
			   real-type)))
    (when (and stringp indexp)
      (setf (fill-pointer string) index))
    (flet ((do-present (s)
	     (stream-present s object real-type
			     :view view :acceptably acceptably
			     :for-context-type context-type)))
      (declare (dynamic-extent do-present))
      (let ((result (if stringp
			 (with-output-to-string (stream string)
			   (do-present stream))
			 (with-output-to-string (stream)
			   (do-present stream)))))
	(if stringp
	    (values string (fill-pointer string))
	    result)))))

;;; Context-dependent input
;;; An input context is a cons of a presentation type and a continuation to
;;; call to return a presentation to that input context.

(defvar *input-context* nil)

(defun input-context-type (context-entry)
  (car context-entry))


(defun input-context-wait-test (stream)
  (declare (ignore stream))
  (let* ((queue (frame-event-queue *application-frame*))
	 (event (event-queue-peek queue)))
    (when event
      (let ((sheet (event-sheet event)))
	(when (and (output-recording-stream-p stream)
		   (typep event 'pointer-event))
	  (return-from input-context-wait-test t))))
    nil))

(defun highlight-applicable-presentation (frame stream input-context
					  &optional (prefer-pointer-window t))
  (let* ((queue (frame-event-queue frame))
	 (event (event-queue-peek queue)))
    (when (and event
	       (typep event 'pointer-event)
	       (or prefer-pointer-window (eq stream (event-sheet event))))
      ;; Stream only needs to see button press events.
      (unless (typep event 'pointer-button-press-event)
	(event-queue-read queue))
      (frame-input-context-track-pointer frame
					 input-context
					 (event-sheet event)
					 event))))
  



(defun input-context-event-handler (stream)
  (highlight-applicable-presentation *application-frame*
				     stream
				     *input-context*))

(defun input-context-button-press-handler (stream button-event)
  (declare (ignore stream))
  (frame-input-context-button-press-handler *application-frame*
					    (event-sheet button-event)
					    button-event))

(defmacro with-input-context ((type &key override) 
			      (&optional (object-var (gensym))
					 (type-var (gensym))
					 event-var
					 options-var)
			      form 
			      &body pointer-cases)
  (let ((vars `(,object-var 
		,type-var
		,@(and event-var `(,event-var))
		,@(and options-var `(,options-var))))
	(return-block (gensym "RETURN-BLOCK"))
	(context-block (gensym "CONTEXT-BLOCK")))
    `(block ,return-block
       (multiple-value-bind ,vars
	   (block ,context-block
	     (let ((*input-context* 
		    (cons (cons ,type
				#'(lambda (object type event options)
				    (return-from ,context-block
				      (values object type event options))))
			  ,(if override nil '*input-context*)))
		   (*pointer-button-press-handler*
		    #'input-context-button-press-handler)
		   (*input-wait-test* #'input-context-wait-test)
		   (*input-wait-handler* #'input-context-event-handler))
	       (return-from ,return-block ,form )))
	 (cond ,@(mapcar #'(lambda (pointer-case)
			     (destructuring-bind (case-type &body case-body)
				 pointer-case
			       `((presentation-subtypep ,type-var ',case-type)
				 ,@case-body)))
			 pointer-cases))))))

(define-presentation-generic-function %accept accept
    (type-key parameters options type stream view &key))

(defvar *recursive-accept-p* nil)
(defvar *recursive-accept-1-p* nil)


(defun accept (type &rest rest-args &key
	       (stream *standard-input* streamp)
	       (view (stream-default-view stream))
	       default
	       (default-type nil default-type-p)
	       provide-default
	       insert-default
	       replace-input
	       (history nil historyp)
	       active-p
	       prompt
	       prompt-mode
	       display-default
	       query-identifier
	       activation-gestures
	       additional-activation-gestures
	       delimiter-gestures
	       additional-delimiter-gestures)
  (declare (ignore default
		   provide-default
		   insert-default
		   replace-input
		   active-p
		   prompt
		   prompt-mode
		   display-default
		   query-identifier
		   activation-gestures
		   additional-activation-gestures
		   delimiter-gestures
		   additional-delimiter-gestures))
  (let* ((real-type (expand-presentation-type-abbreviation type))
	 (real-default-type (and default-type-p
				 (expand-presentation-type-abbreviation
				  default-type)))
	 (real-history-type (and historyp
				 (expand-presentation-type-abbreviation
				  history)))
	 (new-rest-args (if (or streamp default-type-p historyp)
			    (copy-list rest-args)
			    rest-args))
	 (*recursive-accept-p* *recursive-accept-1-p*)
	 (*recursive-accept-1-p* t))
    (when streamp
      (remf new-rest-args :stream))
    (when default-type-p
      (setf (getf new-rest-args :default-type) real-default-type))
    (when historyp
      (setf (getf new-rest-args :history) real-history-type))
    (apply #'prompt-for-accept stream real-type view new-rest-args)
    (apply #'stream-accept stream real-type new-rest-args)))

(defgeneric stream-accept (stream type
			   &key
			   view
			   default
			   default-type
			   provide-default
			   insert-default
			   replace-input
			   history
			   active-p
			   prompt
			   prompt-mode
			   display-default
			   query-identifier
			   activation-gestures
			   additional-activation-gestures
			   delimiter-gestures
			   additional-delimiter-gestures))

(defmethod stream-accept ((stream standard-extended-input-stream) type
			  &rest args)
  (apply #'accept-1 stream type args))

(defun accept-1 (stream type &key
		 (view (stream-default-view stream))
		 (default nil defaultp)
		 (default-type nil default-type-p)
		 provide-default
		 insert-default
		 replace-input
		 history
		 active-p
		 prompt
		 prompt-mode
		 display-default
		 query-identifier
		 activation-gestures
		 additional-activation-gestures
		 delimiter-gestures
		 additional-delimiter-gestures)
  (declare (ignore prompt prompt-mode))
  (when (and defaultp (not default-type-p))
    (error ":default specified without :default-type"))
  (with-input-context (type)
    (object object-type event options)
    (with-input-editing (stream)
      (when defaultp
	;; If the user supplies empty input, return a default.  This is my best
	;; guess as to what "empty" means.
	(let ((initial-char (read-gesture :stream stream :peek-p t)))
	  (cond ((activation-gesture-p initial-char)
		 (read-gesture :stream stream)
		 (return-from accept-1 (values default default-type)))
		((and *recursive-accept-p* (delimiter-gesture-p initial-char))
		 (return-from accept-1 (values default default-type))))))
      (multiple-value-bind (object object-type)
	  (apply-presentation-generic-function accept
					       type
					       stream
					       view
					       `(,@(and defaultp
							`(,default))
						 ,@(and default-type-p
							`(,default-type-p))))
	(values object (or object-type type))))
    ;; A presentation was clicked on, or something
    (t
     (values object object-type))))

(defgeneric prompt-for-accept (stream type view &key))

(defmethod prompt-for-accept ((stream standard-extended-input-stream)
			      type view
			      &rest accept-args)
  (apply #'prompt-for-accept-1 stream type accept-args))

(defun prompt-for-accept-1 (stream type
			    &key
			    (default nil defaultp)
			    (default-type type)
			    (prompt t)
			    (prompt-mode :normal)
			    (display-default prompt)
			    &allow-other-keys)
  (flet ((display-using-mode (stream prompt default)
	   (ecase prompt-mode
	     (:normal
	      (if *recursive-accept-p*
		  (input-editor-format stream "(~A~@[[~A]~]) " prompt default)
		  (input-editor-format stream "~A~@[[~A]~]: " prompt default)))
	     (:raw
	      (input-editor-format stream "~A" prompt)))))
    (let ((prompt-string (if (eq prompt t)
			     (format nil "Enter ~A"
				     (describe-presentation-type type nil nil))
			     prompt))
	  (default-string (if (and defaultp display-default)
			      (present-to-string default default-type)
			      nil)))
      (cond ((null prompt)
	   nil)
	  (t
	   (display-using-mode stream prompt-string default-string))))))

;;; XXX obviously need to do something (a lot) with presentation translators
;;; when  they happen.
;;; XXX This needs to recurse beyond the top record...
(defun find-innermost-applicable-presentation
    (input-context window
     x y
     &key (frame *application-frame*) modifier-state event)
  (declare (ignore frame modifier-state event))
  (let ((top-record (stream-output-history window))
	(result nil)
	(result-size 0))
    (loop for (context-ptype . continuation) in input-context
	  do (progn
	       (map-over-output-records-containing-position
		#'(lambda (record)
		    (when (and (presentationp record)
			       (presentation-subtypep
				(presentation-type record)
				context-ptype))
		      (multiple-value-bind (min-x min-y max-x max-y)
			  (output-record-hit-detection-rectangle* record)
			(let ((size (* (- max-x min-x) (- max-y min-y))))
			  (when (or (not result)
				    (< size result-size))
			    (setq result record)
			    (setq result-size size))))))
		top-record x y)
	       (when result
		 (return-from find-innermost-applicable-presentation
		   result))))))

;;; XXX Need to search for applicable translator and call it, duh.
(defun throw-highlighted-presentation (presentation input-context event)
  (let ((ptype (presentation-type presentation)))
    (loop for (context-ptype . continuation) in input-context
	  when (presentation-subtypep ptype context-ptype)
	  do (funcall continuation (presentation-object presentation) ptype
		      event nil))))

(define-presentation-generic-function %highlight-presentation
    highlight-presentation
  (type-key parameters options type record stream state))

;;; Internal function to highlight just one presentation

(defun highlight-presentation-1 (presentation stream state)
  (funcall-presentation-generic-function highlight-presentation
					 (presentation-type presentation)
					 presentation
					 stream
					 state))

(define-default-presentation-method highlight-presentation
    (type record stream state)
  (highlight-output-record record stream state))

(defun accept-using-read (stream ptype)
  (let* ((*read-eval* nil)
	 (token (read-token stream))
	 (result (handler-case (read-from-string token)
		   (error (c)
		     (declare (ignore c))
		     (simple-parse-error "Error parsing ~S for presentation type ~S"
					 token
					 ptype)))))
    (if (presentation-typep result ptype)
	(values result ptype)
	(input-not-of-required-type result ptype))))

;;; The presentation types

(define-presentation-type t ())

(define-presentation-method presentation-typep (object (type t))
  (declare (ignore object))
  t)

(define-presentation-method present (object (type t)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (let ((*print-readably* acceptably))
    (princ object stream)))

(define-presentation-type nil ())

(define-presentation-method presentation-typep (object (type nil))
  (declare (ignore object))
  nil)

(define-presentation-type null ())

(define-presentation-method presentation-typep (object (type null))
  (eq object nil))

(define-presentation-method present (object (type null)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore object acceptably for-context-type))
  (write-string "None" stream))

(define-presentation-type boolean ())

(define-presentation-method presentation-typep (object (type boolean))
  (or (eq object t) (eq object nil)))

(define-presentation-method present (object (type boolean) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (if object
      (write-string "Yes" stream)
      (write-string "No" stream)))

(define-presentation-type symbol ())

(define-presentation-method presentation-typep (object (type symbol))
  (symbolp object))

(define-presentation-method present (object (type symbol) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (if acceptably
      (prin1 object stream)
      (princ object stream)))

(define-presentation-method accept ((type symbol) stream (view textual-view)
				    &key default default-type)
  (declare (ignore default default-type))
  (accept-using-read stream type))
  
(define-presentation-type keyword () :inherit-from 'symbol)

(define-presentation-method presentation-typep (object (type keyword))
  (keywordp object))

(define-presentation-method present (object (type keyword) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (prin1 object stream))

(defmethod presentation-type-of ((object symbol))
  (if (eq (symbol-package object) (find-package :keyword))
      'keyword
      'symbol))

(define-presentation-type number ())

(define-presentation-method presentation-typep (object (type number))
  (numberp object))

(defmethod presentation-type-of ((object number))
  'number)

(define-presentation-type complex (&optional (type 'real))
  :inherit-from 'number)

(define-presentation-method presentation-typep (object (type complex))
  (and (complexp object)
       (typep (realpart object) type)
       (typep (imagpart object) type)))

(defmethod presentation-type-of ((object complex))
  'complex)

(define-presentation-method present (object (type complex) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (present (realpart object) (presentation-type-of (realpart object))
	   :stream stream :view view :sensitive nil)
  (write-char #\Space stream)
  (present (imagpart object) (presentation-type-of (imagpart object))
	   :stream stream :view view :sensitive nil))

(define-presentation-type real (&optional low high) :options ((base 10) radix)
			  :inherit-from 'number)

(define-presentation-method presentation-typep (object (type real))
  (and (realp object)
       (or (eq low '*)
	   (<= low object))
       (or (eq high '*)
	   (<= object high))))

(defmethod presentation-type-of ((object real))
  'real)

(define-presentation-method present (object (type real) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((*print-base* base)
	(*print-radix* radix))
    (princ object stream)))

(define-presentation-method accept ((type real) stream (view textual-view)
				    &key default default-type)
  (declare (ignore default default-type))
  (let ((*read-base* base))
    (accept-using-read stream type)))

;;; Define a method that will do the comparision for all real types.  It's
;;; already determined that that the numeric class of type is a subtype of
;;;supertype.

(defun number-subtypep (low high super-low super-high)
  (if (eq low '*)
      (unless (eq super-low '*)
	(return-from number-subtypep nil))
      (unless (or (eq super-low '*) (>= low super-low))
	(return-from number-subtypep nil)))
  (if (eq high '*)
      (unless (eq super-high '*)
	(return-from number-subtypep nil))
      (unless (or (eq super-high '*) (<= high super-high))
	(return-from number-subtypep nil)))
  t)

(define-presentation-type rational (&optional low high)
  :options ((base 10) radix)
  :inherit-from `((real ,low ,high) :base ,base :radix ,radix))

(define-presentation-method presentation-typep (object (type rational))
  (and (rationalp object)
       (or (eq low '*)
	   (<= low object))
       (or (eq high '*)
	   (<= object high))))

(defmethod presentation-type-of ((object rational))
  'rational)

(define-presentation-method present (object (type rational) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((*print-base* base)
	(*print-radix* radix))
    (princ object stream)))

(define-presentation-type integer (&optional low high)
  :options ((base 10) radix)
  :inherit-from `((rational ,low ,high) :base ,base :radix ,radix))

(define-presentation-method presentation-typep (object (type integer))
  (and (integerp object)
       (or (eq' low '*)
	   (<= low object))
       (or (eq' high '*)
	   (<= object high))))

(defmethod presentation-type-of ((object integer))
  'integer)

(define-presentation-method present (object (type integer) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((*print-base* base)
	(*print-radix* radix))
    (princ object stream)))

(define-presentation-type ratio (&optional low high)
  :options ((base 10) radix)
  :inherit-from `((rational ,low ,high) :base ,base :radix ,radix))

(define-presentation-method presentation-typep (object (type ratio))
  (and (not (integerp object))
       (rationalp object)
       (or (eq low '*)
	   (<= low object))
       (or (eq high '*)
	   (<= object high))))

(defmethod presentation-type-of ((object ratio))
  'ratio)

(define-presentation-method present (object (type ratio) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((*print-base* base)
	(*print-radix* radix))
    (princ object stream)))

(define-presentation-type float (&optional low high)
  :options ((base 10) radix)
  :inherit-from `((real ,low ,high) :base ,base :radix ,radix))

(define-presentation-method presentation-typep (object (type float))
  (and (floatp object)
       (or (eq low '*)
	   (<= low object))
       (or (eq high '*)
	   (<= object high))))

(defmethod presentation-type-of ((object float))
  'float)

(define-presentation-method present (object (type float) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((*print-base* base)
	(*print-radix* radix))
    (princ object stream)))

(macrolet ((frob (num-type)
	     `(define-presentation-method presentation-subtypep ((type
								  ,num-type)
								 maybe-supertype)
	        (with-presentation-type-parameters (,num-type maybe-supertype)
		  (let ((super-low low)
			(super-high high))
		    (with-presentation-type-parameters (,num-type type)
		      (values (number-subtypep low high super-low super-high)
			      t)))))))
  (frob real)
  (frob rational)
  (frob ratio)
  (frob float))

(define-presentation-type character ())

(define-presentation-method presentation-typep (object (type character))
  (characterp object))

(defmethod presentation-type-of ((object character))
  'character)

(define-presentation-method present (object (type character) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (princ object stream))

(define-presentation-type string (&optional length))

(define-presentation-method presentation-typep (object (type string))
  (and (stringp object)
       (or (eq length '*) (eql (length object) length))))

(define-presentation-method presentation-subtypep ((type string)
						   maybe-supertype)
  (with-presentation-type-parameters (string maybe-supertype)
    (let ((super-length length))
      (with-presentation-type-parameters (string type)
	(values (or (eq super-length '*)
		    (eql length super-length))
		t)))))

(defmethod presentation-type-of ((object string))
  (if (or (adjustable-array-p object)
	  (array-has-fill-pointer-p object))
      'string
      `(string ,(length object))))

(define-presentation-method present (object (type string) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (princ object stream))

(define-presentation-method accept ((type string) stream (view textual-view)
				    &key default default-type)
  (declare (ignore default default-type))
  (let ((result (read-token stream)))
    (if (numberp length)
	(if (eql length (length result))
	    (values result type)
	    (input-not-of-required-type result type))
	(values result type))))

(define-presentation-type pathname ()
  :options ((default-version :newest) default-type (merge-default t)))

(define-presentation-method presentation-typep (object (type pathname))
  (pathnamep object))

(defmethod presentation-type-of ((object pathname))
  'pathname)

(define-presentation-method present (object (type pathname) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (princ object stream))

(defgeneric default-completion-name-key (item))

(defmethod default-completion-name-key ((item string))
  item)

(defmethod default-completion-name-key ((item null))
  "NIL")

(defmethod default-completion-name-key ((item cons))
  (string (car item)))

(defmethod default-completion-name-key ((item symbol))
  (string-capitalize (symbol-name item)))

(defmethod default-completion-name-key (item)
  (princ-to-string item))

;;; Someone decided to patch defconstant, so we need the eval-when :P
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +completion-options+
    '((name-key 'default-completion-name-key)
      documentation-key
      (partial-completers '(#\Space)))))

(define-presentation-type completion (sequence
				      &key (test 'eql) (value-key 'identity))
  :options #.+completion-options+)

(define-presentation-method presentation-typep (object (type completion))
  (map nil #'(lambda (obj)
	       (when (funcall test object (funcall value-key obj))
		 (return-from %presentation-typep t)))
       sequence)
  nil)

;;; Useful for subtype comparisons for several of the "member" style types

(defun sequence-subset-p (seq1 test1 value-key1 seq2 test2 value-key2)
  (let ((test-fun (if (eq test1 test2)
		      test1
		      ;; The object has to pass both type's equality test
		      #'(lambda (obj1 obj2)
			  (and (funcall test1 obj1 obj2)
			       (funcall test2 obj1 obj2))))))
    (map nil #'(lambda (type-obj)
		 (unless (find (funcall value-key1 type-obj)
			       seq2
			       :test test-fun :key value-key2)
		   (return-from sequence-subset-p nil)))
	 seq1)
    t))

(define-presentation-method presentation-subtypep ((type completion)
						   maybe-supertype)
  (with-presentation-type-parameters (completion maybe-supertype)
    (let ((super-sequence sequence)
	  (super-test test)
	  (super-value-key value-key))
      (with-presentation-type-parameters (completion type)
	(values (sequence-subset-p sequence test value-key
				   super-sequence super-test super-value-key)
		t)))))

(define-presentation-method present (object (type completion) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (present object (presentation-type-of object)
	   :stream stream :view view))

(define-presentation-type-abbreviation member (&rest elements)
  (make-presentation-type-specifier `(completion ,elements)
				    :name-key name-key
				    :documentation-key documentation-key
				    :partial-completers partial-completers)
  :options #.+completion-options+)

(define-presentation-type-abbreviation member-sequence (sequence
							&key (test 'eql testp))
  (make-presentation-type-specifier
   `(completion ,sequence ,@(and testp `(:test ,test)))
   :name-key name-key
   :documentation-key documentation-key
   :partial-completers partial-completers)
  :options #.+completion-options+)

(defun member-alist-value-key (element)
  (cond ((atom element)
	 element)
	((atom (cdr element))
	 (cdr element))
	((null (cddr element))
	 (cadr element))
	(t (getf (cdr element) :value))))

(defun member-alist-doc-key (element)
  (if (and (consp element) (consp (cdr element)) (consp (cddr element)))
      (getf (cdr element) :documentation)))

(define-presentation-type-abbreviation member-alist (alist
						     &key (test 'eql testp))
  (make-presentation-type-specifier
   `(completion ,alist ,@(and testp `(:test ,test))
		:value-key member-alist-value-key)
   :name-key name-key
   :documentation-key documentation-key
   :partial-completers partial-completers)
  :options ((name-key 'default-completion-name-key)
	    (documentation-key 'member-alist-doc-key)
	    (partial-completers '(#\Space))))

(define-presentation-type subset-completion (sequence
					     &key (test 'eql)
					     (value-key 'identity))
  :options ((name-key 'default-completion-name-key)
	    documentation-key
	    (partial-completers '(#\Space))
	    (separator #\,)
	    (echo-space t)))

(define-presentation-method presentation-typep (object
						(type subset-completion))
  (map nil #'(lambda (obj)
	       (unless (find obj sequence :test test :key value-key)
		 (return-from %presentation-typep nil)))
       object)
  t)

(define-presentation-method presentation-subtypep ((type subset-completion)
						   maybe-supertype)
  (with-presentation-type-parameters (subset-completion maybe-supertype)
    (let ((super-sequence sequence)
	  (super-test test)
	  (super-value-key value-key))
      (with-presentation-type-parameters (subset-completion type)
	(values (sequence-subset-p sequence test value-key
				   super-sequence super-test super-value-key)
		t)))))

(define-presentation-method present ((object list) (type subset-completion)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for tail on object
	for (obj) = tail
	do (progn
	     (present obj (presentation-type-of object)
			:stream stream :view view
			:acceptably acceptably
			:sensitive nil)
	     (when (cdr tail)
	       (if acceptably
		   (princ separator stream)
		   (terpri stream))))))

(define-presentation-method present ((object vector) (type subset-completion)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for i from 0 below (length object)
	for obj = (aref object i)
	do (progn
	     (present obj (presentation-type-of object)
			:stream stream :view view
			:acceptably acceptably
			:sensitive nil)
	     (when (< i (1- (length object)))
	       (if acceptably
		   (princ separator stream)
		   (terpri stream))))))

;;; XXX is it a typo in the spec that subset, subset-sequence and subset-alist
;;; have the same options as completion, and not subset-completion?

(define-presentation-type-abbreviation subset (&rest elements)
  (make-presentation-type-specifier `(subset-completion ,elements)
				    :name-key name-key
				    :documentation-key documentation-key
				    :partial-completers partial-completers)
  :options #.+completion-options+)

(define-presentation-type-abbreviation subset-sequence (sequence
							&key (test 'eql testp))
(make-presentation-type-specifier
     `(subset-completion ,sequence ,@(and testp `(:test ,test)))
     :name-key name-key
     :documentation-key documentation-key
     :partial-completers partial-completers)
  :options #.+completion-options+)

(define-presentation-type-abbreviation subset-alist (alist
						     &key (test 'eql testp))
  (make-presentation-type-specifier
   `(subset-completion ,@(and testp `(:test ,test))
                       :value-key member-alist-value-key)
   :name-key name-key
   :documentation-key documentation-key
   :partial-completers partial-completers)
  :options ((name-key 'default-completion-name-key)
	    (documentation-key 'member-alist-doc-key)
	    (partial-completers '(#\Space))))

(define-presentation-type sequence (type)
  :options ((separator #\,) (echo-space t))
  :parameters-are-types t)

(define-presentation-method presentation-typep (object (type sequence))
  ;; XXX TYPE here is the sequence element type, not the whole type specifier
  (unless (or (listp object) (vectorp object))
    (return-from %presentation-typep nil))
  (let ((real-type (expand-presentation-type-abbreviation type)))
    (map nil #'(lambda (obj)
		 (unless (presentation-typep obj real-type)
		   (return-from %presentation-typep nil)))
	 object)
    t))

(define-presentation-method presentation-subtypep ((type sequence)
						   maybe-supertype)
  (with-presentation-type-parameters (sequence type)
    ;; now TYPE is bound to the parameter TYPE
    (let ((real-type (expand-presentation-type-abbreviation type)))
      (with-presentation-type-parameters (sequence maybe-supertype)
	(let ((real-super-type (expand-presentation-type-abbreviation type)))
	  (presentation-subtypep real-type real-super-type))))))

(defmethod presentation-type-of ((object cons))
  '(sequence t))

;;; Do something interesting with the array-element-type
(defmethod presentation-type-of ((object vector))
  '(sequence t))

(define-presentation-method present ((object list) (type sequence)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for tail on object
	for (obj) = tail
	do (progn
	     (present obj type		; i.e., the type parameter
			:stream stream :view view
			:acceptably acceptably
			:sensitive nil)
	     (when (cdr tail)
	       (if acceptably
		   (princ separator stream)
		   (terpri stream))))))

(define-presentation-method present ((object vector) (type sequence)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for i from 0 below (length object)
	for obj = (aref object i)
	do (progn
	     (present obj type		; i.e., the type parameter
			:stream stream :view view
			:acceptably acceptably
			:sensitive nil)
	     (when (< i (1- (length object)))
	       (if acceptably
		   (princ separator stream)
		   (terpri stream))))))

(define-presentation-type sequence-enumerated (&rest types)
  :options ((separator #\,) (echo-space t))
  :parameters-are-types t)

(define-presentation-method presentation-typep (object
						(type sequence-enumerated))
  (unless (or (listp object) (vectorp object))
    (return-from %presentation-typep nil))
  (map nil #'(lambda (obj type)
	       (let ((real-type (expand-presentation-type-abbreviation type)))
		 (unless (presentation-typep obj real-type)
		   (return-from %presentation-typep nil))))
       object
       types)
  t)

(define-presentation-method presentation-subtypep ((type sequence-enumerated)
						   maybe-supertype)
  (with-presentation-type-parameters (sequence-enumerated maybe-supertype)
    (let ((supertypes types))
      (with-presentation-type-parameters (sequence-enumerated type)
	(unless (eql (length supertypes) (length types))
	  (return-from %presentation-subtypep (values nil t)))
	(map nil
	     #'(lambda (element-type element-supertype)
		 (let ((real-type (expand-presentation-type-abbreviation
				   element-type))
		       (real-supertype (expand-presentation-type-abbreviation
					element-supertype)))
		   (multiple-value-bind (subtypep determined)
		       (presentation-subtypep real-type real-supertype)
		     (cond ((not determined)
			    (return-from %presentation-subtypep
			      (values nil nil)))
			   ((not subtypep)
			    (return-from %presentation-subtypep
			      (values nil t)))))))
	     types
	     supertypes)
	(values t t)))))

(define-presentation-method present ((object list) (type sequence-enumerated)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for tail on object
	for (obj) = tail
	for obj-type in types
	do (progn
	     (present obj obj-type
			:stream stream :view view
			:acceptably acceptably
			:sensitive nil)
	     (when (cdr tail)
	       (if acceptably
		   (princ separator stream)
		   (terpri stream))))))

(define-presentation-method present ((object vector) (type sequence-enumerated)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for i from 0 below (length object)
	for obj = (aref object i)
	for obj-type in types
	do (progn
	     (present obj obj-type
			:stream stream :view view
			:acceptably acceptably
			:sensitive nil)
	     (when (< i (1- (length object)))
	       (if acceptably
		   (princ separator stream)
		   (terpri stream))))))

(define-presentation-type or (&rest types)
  :parameters-are-types t)

(define-presentation-method presentation-typep (object (type or))
  (loop for type in types
	for real-type = (expand-presentation-type-abbreviation type)
	do (when (presentation-typep object real-type)
	     (return-from %presentation-typep t)))
  nil)

(define-presentation-method present (object (type or)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (loop for or-type in types
	for expanded-type = (expand-presentation-type-abbreviation or-type)
	do (when (presentation-typep object expanded-type)
	     (present object expanded-type
		      :stream stream :view view
		      :acceptably acceptably
		      :for-context-type for-context-type)
	     (loop-finish))))

(define-presentation-type and (&rest types)
  :parameters-are-types t)

(define-presentation-method presentation-typep (object (type and))
  (loop for type in types
	for real-type = (expand-presentation-type-abbreviation type)
	do (with-presentation-type-decoded (name parameters)
	     real-type
	     (cond ((eq name 'satisfies)
		    (unless (funcall (car parameters) object)
		      (return-from %presentation-typep nil)))
		   ((eq name 'not)
		    (unless (not (presentation-typep object (car parameters)))
		      (return-from %presentation-typep nil)))
		   (t (unless (presentation-typep object real-type)
			(return-from %presentation-typep nil))))))
  t)

(define-presentation-method present (object (type and)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (present object (expand-presentation-type-abbreviation (car types))
	   :stream stream :view view
	   :acceptably acceptably
	   :for-context-type for-context-type))

(define-presentation-type-abbreviation token-or-type (tokens type)
  `(or (member-alist ,tokens) ,type))

(define-presentation-type-abbreviation null-or-type (type)
  `(or null ,type))

(define-presentation-type-abbreviation type-or-string (type)
  `(or ,type string))

(define-presentation-type expression ())

(define-presentation-method presentation-typep (object (type expression))
  (declare (ignore object))
  t)

(define-presentation-method present (object (type expression)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (let ((*print-readably* acceptably))
    (princ object stream)))

(define-presentation-method accept ((type expression) stream
				    (view textual-view)
				    &key default default-type)
  (declare (ignore default default-type))
  (accept-using-read stream type))

(define-presentation-type form ()
  :inherit-from `expression)

