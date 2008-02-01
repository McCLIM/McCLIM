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

;;; Implementation of the presentation type system, presentation generic
;;; functions and methods, presentation translators, finding an applicable
;;;presentation.

(in-package :clim-internals)

;;; PRESENTATION class

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

(defvar *print-presentation-verbose* nil)

(defmethod print-object ((self standard-presentation) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (with-bounding-rectangle* (x1 y1 x2 y2)
	self
      (format stream "~D:~D,~D:~D ~S" x1 x2 y1 y2 (presentation-type self))
      (when *print-presentation-verbose*
	(format stream " ~S" (presentation-object self))))))

(defmacro with-output-as-presentation ((stream object type
					       &rest key-args
					       &key modifier single-box
					       (allow-sensitive-inferiors t)
					       parent
					       (record-type
						''standard-presentation)
					       &allow-other-keys)
				       &body body)
  (declare (ignore parent single-box modifier))
  (setq stream (stream-designator-symbol stream '*standard-output*))
  (multiple-value-bind (decls with-body)
      (get-body-declarations body)
    (with-gensyms (record-arg continuation)
      (with-keywords-removed (key-args (:record-type
					:allow-sensitive-inferiors))
	`(flet ((,continuation ()
		  ,@decls
		  ,@with-body))
	   (declare (dynamic-extent #'continuation))
	   (if (and (output-recording-stream-p ,stream)
		    *allow-sensitive-inferiors*)
	       (with-new-output-record
		   (,stream ,record-type ,record-arg
			    :object ,object
			    :type (expand-presentation-type-abbreviation
				   ,type)
			    ,@key-args)
		 (let ((*allow-sensitive-inferiors*
			,allow-sensitive-inferiors))
		   (,continuation)))
	       (,continuation)))))))

(defgeneric ptype-specializer (type)
  (:documentation "The specializer to use for this type in a presentation
method lambda list"))

;;; Metaclass for presentation types.  For presentation types not associated
;;; with CLOS classes, objects with this metaclass are used as a proxy for the
;;; type during presentation method dispatch. We don't prevent these
;;; presentation proxies from being subclasses of standard-object,
;;; but in presentation method dispatch we remove methods on standard
;;; object for presentation types that are not CLOS classes. The
;;; presentation type metaclass for T is the builtin object T instead
;;; of a presentation-type-class; in this way we can avoid weirdness
;;; with T being a subtype of standard-object!
;;;

(defclass presentation-type ()
  ((type-name :accessor type-name :initarg :type-name
	      :documentation "The name assigned to the presentation
type, as opposed to the name constructed for the class")
   (ptype-specializer :accessor ptype-specializer :initarg :ptype-specializer)
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

(defmethod history ((ptype standard-class))
  "Default for CLOS types that are not defined explicitly as
presentation types."
  t)

(defvar *builtin-t-class* (find-class t))

;;;Methods for the T presentation type
(defmethod type-name ((class (eql *builtin-t-class*)))
  t)

(defmethod ptype-specializer ((class (eql *builtin-t-class*)))
  t)

(defmethod parameters ((class (eql *builtin-t-class*)))
  nil)

(defmethod parameters-lambda-list ((class (eql *builtin-t-class*)))
  nil)

(defmethod options ((class (eql *builtin-t-class*)))
  nil)

(defmethod options-lambda-list ((class (eql *builtin-t-class*)))
  nil)

(defmethod inherit-from ((class (eql *builtin-t-class*)))
  nil)

(defmethod inherit-from-function ((class (eql *builtin-t-class*)))
  #'(lambda (type)
      (declare (ignore type))
      nil))

(defmethod description ((class (eql *builtin-t-class*)))
  "The supertype of all presentation types.")

(defmethod history ((class (eql *builtin-t-class*)))
  t)

(defmethod parameters-are-types ((class (eql *builtin-t-class*)))
  nil)

(defmethod expansion-function ((class (eql *builtin-t-class*)))
  #'(lambda (type)
      (declare (ignore type))
      t))

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

(defun fake-params-args (ll)
  (let ((state 'required))
    (flet ((do-arg (lambda-var)
	     (let ((var-name (symbol-name lambda-var)))
	       (cond ((or (eq state 'required) (eq state '&optional))
		      (list (gensym var-name)))
		     ((eq state '&key)
		      `(,(intern var-name :keyword) ,(gensym var-name)))
		     (t nil)))))
      (loop for lambda-var in ll
	 append (cond ((member lambda-var lambda-list-keywords :test #'eq)
		       (setq state lambda-var)
		       nil)
		      ((eq state '&whole)
		       (setq state 'required)
		       nil)
		      ((atom lambda-var)
		       (do-arg lambda-var))
		      ((consp lambda-var)
		       (let ((var (car lambda-var)))
			 (do-arg (if (and (eq state '&key) (consp var))
				     (car var)
				     var)))))))))

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
(declaim (inline presentation-type-name))

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

(defvar *presentation-type-table* (make-hash-table :test #'eq))

(setf (gethash t *presentation-type-table*) (find-class t))

(defgeneric get-ptype-metaclass (type))

(defmethod get-ptype-metaclass ((type symbol))
  (let ((maybe-meta (gethash type *presentation-type-table*)))
    (if maybe-meta
	(get-ptype-metaclass maybe-meta)
	(let ((system-meta (find-class type nil)))
	  (and (typep system-meta 'standard-class)
	       system-meta)))))

(defmethod get-ptype-metaclass ((type presentation-type-class))
  type)

(defmethod get-ptype-metaclass ((type clos-presentation-type))
  (clos-class type))

(defmethod get-ptype-metaclass ((type (eql *builtin-t-class*)))
  type)

(defmethod get-ptype-metaclass ((type class))
  type)

(defmethod get-ptype-metaclass (type)
  (error "~A is not the name of a presentation type" type))

;;; external functions
(defun find-presentation-type-class (name &optional (errorp t) environment)
  (declare (ignore environment))
  (let ((metaclass (get-ptype-metaclass name)))
    (cond (metaclass
	   metaclass)
	  (errorp
	   (error "~S is not the name of a presentation type" name))
	  (t nil))))

(defun class-presentation-type-name (class &optional environment)
  (declare (ignore environment))
  (cond ((typep class 'presentation-type)
	 (type-name class))
	(t (class-name class))))

;;; For the presentation class T, the stand in object must not be a
;;; standard-object. So... why not the symbol T?

(defvar *class-t-prototype* t)

(defun prototype-or-error (name)
  (let ((ptype-meta (get-ptype-metaclass name)))
    (unless ptype-meta
      (error "~S is an unknown presentation type" name))
    (when (eq ptype-meta *builtin-t-class*)
      (return-from prototype-or-error *class-t-prototype*))
    (unless (clim-mop:class-finalized-p ptype-meta)
      (clim-mop:finalize-inheritance ptype-meta))
    (or (clim-mop:class-prototype ptype-meta)
      (error "Couldn't find a prototype for ~S" name))))

(defun safe-cpl (class)
  (unless (clim-mop:class-finalized-p class)
    (clim-mop:finalize-inheritance class))
  (clim-mop:class-precedence-list class))
  
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
		 (let ((clos-ptype (gethash (class-name class)
					    *presentation-type-table*)))
		   (if clos-ptype
		       (list clos-ptype)
		       nil)))
		(t
		 nil)))
	  (clim-mop:class-direct-superclasses type)))

(defmethod presentation-ptype-supers ((type clos-presentation-type))
  (presentation-ptype-supers (clos-class type)))

;;; External function

(defun presentation-type-direct-supertypes (type)
  (with-presentation-type-decoded (name)
      type
    (let ((supers (presentation-ptype-supers name)))
      (mapcar #'class-presentation-type-name supers))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod ptype-specializer ((type symbol))
    (let ((ptype (gethash type *presentation-type-table*)))
      (cond (ptype
	     (ptype-specializer ptype))
	    ((find-class type nil)
	     (ptype-specializer (find-class type)))
	    ;; Assume it's a forward referenced CLOS class.
	    (t type))))

  (defmethod ptype-specializer ((type standard-class))
    (class-name type)))

;;; We need to patch defclass in every implementation to record a CLOS
;;; class at compiletime.  On the other hand, I think we can assume
;;; that if a CLOS class exists at compile time, it will exist at
;;; load/run time too.
(eval-when (:compile-toplevel :load-toplevel :execute)
  #-(or excl cmu sbcl openmcl)
  (defun compile-time-clos-p (name)
    (let ((meta (find-class name nil)))
      (and meta
	   (typep meta 'standard-class))))

  #+(or excl cmu sbcl openmcl)
  (defun compile-time-clos-p (name)
    (let ((metaclass (find-class name nil)))
      (or (and metaclass
	     (typep metaclass 'standard-class))
	(clim-lisp-patch::compile-time-clos-class-p name))))

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
		      (let ((directs (mapcan
				      #'(lambda (super)
					  (if (eq super t)
					      nil
					      (list (or (get-ptype-metaclass
							 super)
							super))))
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

;;; These are used by the presentation method MOP code, but are
;;; actually defined in presentation-defs.lisp after the forms for these
;;; types are executed.

(defvar *ptype-t-class*)

(defvar *ptype-expression-class*)

(defvar *ptype-form-class*)

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
  (let ((ptype (get-ptype type-name)))
    (unless (or ptype (compile-time-clos-p type-name))
      (warn "~S is not a presentation type name." type-name))
    (if (typep ptype 'presentation-type)
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
		,@body))))
	`(let ()
	   ,@body))))


(defmacro with-presentation-type-options ((type-name type) &body body)
  (let ((ptype (get-ptype type-name)))
    (unless (or ptype (compile-time-clos-p type-name))
      (warn "~S is not a presentation type name." type-name))
    (if (typep ptype 'presentation-type)
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
		,@body))))
	`(let ()
	   ,@body))))

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

;;; Presentation methods.
;;;
;;; The basic dispatch is performed via CLOS instances that are standins
;;; for the presentation types.  There are a couple of complications to
;;; this simple plan.  First, methods on the presentation type class
;;;STANDARD-OBJECT -- if there are any -- should not be applicable to
;;;presentation types that are not CLOS classes, even though STANDARD-OBJECT
;;;is in the class precedence list of the standin object. Our own methods on
;;;COMPUTE-APPLICABLE-METHODS-USING-CLASSES and COMPUTE-APPLICABLE-METHODS
;;;remove methods specialized on standard-object.
;;;
;;; The second major complication is the whole raison d'etre of presentation
;;; type methods: type parameters and options are massaged so that
;;; applicable methods written on the supertype of a presentation type get
;;; parameters and options in the expected form.  "Real" CLIM apparently
;;; does this massaging in the body of the effective method and passes the
;;; massaged parameters as an argument into the method.  We do it with a
;;; function call within the body of the method.  This is potentially more
;;; expensive, but caching should help that.  Our method has the huge
;;; advantage of working with any method combination.

(defclass presentation-gf-info ()
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

(defclass presentation-generic-function (standard-generic-function)
  ()
  (:metaclass clim-mop:funcallable-standard-class))

(defvar *standard-object-class* (find-class 'standard-object))

#-scl
(defmethod clim-mop:compute-applicable-methods-using-classes :around
    ((gf presentation-generic-function) classes)
  (multiple-value-bind (methods success)
      (call-next-method)
    (let ((ptype-class (car classes)))
      (if (or (null success)
	      (not (typep ptype-class 'presentation-type-class)))
	  (values methods success)
	  (values (remove-if #'(lambda (method)
				 (eq (car (clim-mop:method-specializers
					   method))
				     *standard-object-class*))
			     methods)
		  t)))))

#+scl
(defmethod clim-mop:compute-applicable-methods-using-classes :around
    ((gf presentation-generic-function) classes)
  (multiple-value-bind (methods success non-class-positions)
      (call-next-method)
    (let ((ptype-class (car classes)))
      (if (or (null success)
	      (not (typep ptype-class 'presentation-type-class)))
	  (values methods non-class-positions non-class-positions)
	  (values (remove-if #'(lambda (method)
				 (eq (car (clim-mop:method-specializers
					   method))
				     *standard-object-class*))
			     methods)
		  t
		  non-class-positions)))))

(defun method-applicable (method arguments)
  (loop for arg in arguments
	for specializer in (clim-mop:method-specializers method)
	always (cond ((typep specializer 'clim-mop:eql-specializer)
		      (eql arg (clim-mop:eql-specializer-object specializer)))
		     ((typep arg specializer)
		      t)
		     ((and (not (typep (class-of arg)
				       'presentation-type-class))
			   (or (eq specializer *ptype-form-class*)
			       (eq specializer *ptype-expression-class*)))
		      t)
		     (t nil))))

(defmethod compute-applicable-methods :around
    ((gf presentation-generic-function) arguments)
  (let ((methods (call-next-method)))
    (if (typep (class-of (car arguments)) 'presentation-type-class)
	(remove-if #'(lambda (method)
		       (eq (car (clim-mop:method-specializers method))
			   *standard-object-class*))
		   methods)
	methods)))

;;; The hard part of presentation methods: translating the type specifier for
;;; superclasses.
;;;

(defmethod type-name ((type standard-class))
  (class-name type))

(defmethod expansion-function ((type standard-class))
  #'(lambda (typespec)
      (with-presentation-type-decoded (name)
	typespec
	name)))

(defmethod presentation-ptype-supers ((type standard-class))
  (mapcan #'(lambda (class)
	      (let ((ptype (gethash (class-name class)
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
	      (make-instance 'presentation-gf-info
			     :generic-function-name ',generic-function-name
			     :lambda-list ',lambda-list
			     :type-key-arg ',type-key-arg
			     :parameters-arg ',parameters-arg
			     :options-arg ',options-arg
			     :type-arg-position ,type-arg-pos)))
	   (defgeneric ,generic-function-name ,gf-lambda-list
	     (:generic-function-class presentation-generic-function)
	     ,@options)))))

(defun parse-method-body (args)
  (loop for arglist on args
     for (arg) = arglist
     while (atom arg)
     collect arg into qualifiers
     finally (if (and (consp arglist)
		      (consp (cdr arglist))
		      (consp (cadr arglist))
		      (eq (caadr arglist) 'declare))
		 (return (values qualifiers arg (cadr arglist) (cddr arglist)))
		 (return (values qualifiers arg nil (cdr arglist))))))

(defun type-name-from-type-key (type-key)
  (if (symbolp type-key)
      't
      (type-name (class-of type-key))))

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
					       (type-name-from-type-key
						,(type-key-arg gf))
					       ',type-name
					       ,type-var)))
			  ,@real-body))))
	      (setf (nth (type-arg-position gf) method-ll) type-var)
	      `(defmethod ,(generic-function-name gf) ,@qualifiers ,method-ll
		 ,@(when decls
		     (list decls))
		 (block ,name
		   ,@real-body)))))))))



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
	 (block ,name
	   ,@body)))))

;;; Somewhat obsolete, but keep it around for apply-presentation-generic-function.
(defun %funcall-presentation-generic-function (name gf type-arg-position
					       &rest args)
  (declare (ignore name))
  (let* ((type-spec (nth (1- type-arg-position) args))
	 (ptype-name (presentation-type-name type-spec)))
    (apply gf (prototype-or-error ptype-name) args)))

;;; I wonder if this pattern for preserving order of evaluation is
;;; has a more general use...

(defmacro funcall-presentation-generic-function (name &rest args)
  (let ((gf (gethash name *presentation-gf-table*)))
    (unless gf
      (error "~S is not a presentation generic function" name))
    (let* ((rebound-args (loop for arg in args
                            unless (symbolp arg)
                            collect (list (gensym "ARG") arg)))
           (gf-name (generic-function-name gf))
           (type-spec-var (nth (1- (type-arg-position gf)) args)))
      `(let ,rebound-args
         (,gf-name (prototype-or-error (presentation-type-name
                                        ,(or (first (find type-spec-var rebound-args :key #'second))
                                             type-spec-var)))
                   ,@(mapcar #'(lambda (arg)
                                 ;; Order of evaluation doesn't matter
                                 ;; for symbols, and this shuts up
                                 ;; warnings about arguments in a
                                 ;; keyword position not being
                                 ;; constant. By the way, why do we
                                 ;; care about order of evaluation
                                 ;; here? -trh
                                 (or (first (find arg rebound-args :key #'second))
                                     arg)) args))))))

(defmacro apply-presentation-generic-function (name &rest args)
  (let ((gf (gethash name *presentation-gf-table*)))
    (unless gf
      (error "~S is not a presentation generic function" name))
    `(apply #'%funcall-presentation-generic-function ',name
	    #',(generic-function-name gf)
	    ,(type-arg-position gf)
	    ,@args)))

;;; 23.7.1 Defining Presentation Translators

(defclass presentation-translator ()
  ((name :reader name :initarg :name)   
   (from-type :reader from-type :initarg :from-type)
   (to-type :reader to-type :initarg :to-type)
   (gesture :reader gesture :initarg :gesture)
   (tester :reader tester :initarg :tester)
   (tester-definitive :reader tester-definitive :initarg :tester-definitive)
   (documentation :reader translator-documentation :initarg :documentation)
   (pointer-documentation :reader pointer-documentation
			  :initarg :pointer-documentation)
   (menu :reader menu :initarg :menu)
   (priority :reader priority :initarg :priority :initform 0)
   (translator-function :reader translator-function
			:initarg :translator-function)))

(defmethod initialize-instance :after ((obj presentation-translator)
				       &key &allow-other-keys)
  (unless (slot-boundp obj 'pointer-documentation)
    (setf (slot-value obj 'pointer-documentation)
	  (translator-documentation obj))))

(defmethod print-object ((obj presentation-translator) stream)
  (print-unreadable-object (obj stream :identity t)
    (format stream "Translator ~S from ~S to ~S"
	    (name obj) (from-type obj) (to-type obj))))

(defclass presentation-action (presentation-translator)
  ())

(defmethod initialize-instance :after ((obj presentation-action)
				       &key &allow-other-keys)
  (setf (slot-value obj 'tester-definitive) t))

(defmethod print-object ((obj presentation-action) stream)
  (print-unreadable-object (obj stream :identity t)
    (format stream "Action from ~S to ~S" (from-type obj) (to-type obj))))

;;; This lives in a command table

(defvar *current-translator-cache-generation* 0
  "This is incremented whenever presentation translators are defined,
and used to ensure that presentation-translators-caches are up to date.")

(defclass translator-table ()
  ((translators :accessor translators :initarg :translators
		:initform (make-hash-table :test #'eq)
		:documentation "Translators keyed by name.")
   (simple-type-translators :accessor simple-type-translators
			    :initarg :simple-type-translators
			    :initform (make-hash-table :test #'eq)
			    :documentation "Holds transators with a simple
  from-type (i.e. doesn't contain \"or\" or \"and\").")
   (translator-cache-generation :accessor translator-cache-generation :initform 0)
   (presentation-translators-cache
    :writer (setf presentation-translators-cache)
    :initform (make-hash-table :test #'equal))))

(defun invalidate-translator-caches ()
  (incf *current-translator-cache-generation*))

(defmethod presentation-translators-cache ((table translator-table))
  (with-slots ((cache presentation-translators-cache)
	       (generation translator-cache-generation))
      table    
    (unless (or (= generation *current-translator-cache-generation*)
		(zerop (hash-table-size cache)))            
      (clrhash cache))
    (setf generation *current-translator-cache-generation*)
    cache))
      
      

;;; Returns function lambda list, ignore forms
(defun make-translator-ll (translator-args)
  (let ((object-arg (find "object" translator-args :test #'string-equal))
	(ignore-form nil))
    (if object-arg
	(setq translator-args (remove "object" translator-args
				      :test #'string-equal))
	(progn
	  (setq object-arg (gensym "OBJECT-ARG"))
	  (setq ignore-form `(declare (ignore ,object-arg)))))
    (values `(,object-arg &key ,@translator-args &allow-other-keys)
	    ignore-form)))

(defun default-translator-tester (object-arg &key &allow-other-keys)
  (declare (ignore object-arg))
  t)

(defun add-translator (table translator)
  ;; Remove old one.
  (with-accessors ((translators translators)
		   (simple-type-translators simple-type-translators))
      table
    (let ((old (gethash (name translator) translators)))
      (when old
	(setf (gethash (presentation-type-name (from-type old))
		       simple-type-translators)
	      (remove old
		      (gethash (presentation-type-name (from-type old))
			       simple-type-translators))))
      (invalidate-translator-caches)
      (setf (gethash (name translator) translators) translator)
      (push translator
	    (gethash (from-type translator) simple-type-translators))
      translator)))

(defun make-translator-fun (args body)
  (multiple-value-bind (ll ignore)
      (make-translator-ll args)
    `(lambda ,ll
       ,@(and ignore (list ignore))
       ,@body)))

(defun make-documentation-fun (doc-arg)
  (cond ((and doc-arg (symbolp doc-arg))
	 doc-arg)
	((consp doc-arg)
	 (make-translator-fun (car doc-arg) (cdr doc-arg)))
	((stringp doc-arg)
	 `(lambda (object &key stream &allow-other-keys)
	    (declare (ignore object))
	    (write-string ,doc-arg stream)))
	((null doc-arg)
	 `(lambda (object &key presentation stream &allow-other-keys)
	    (present object (presentation-type presentation)
		     :stream stream :sensitive nil)))
	(t (error "Can't handle doc-arg ~S" doc-arg))))

(defmacro define-presentation-translator
    (name (from-type to-type command-table &rest translator-options &key
	   (gesture :select)
	   (tester 'default-translator-tester testerp)
	   (tester-definitive (if testerp nil t))
	   (documentation nil documentationp)
	   (pointer-documentation nil pointer-documentation-p)
	   (menu t)
	   (priority 0)
	   (translator-class 'presentation-translator)
	   &allow-other-keys)
     arglist
     &body body)
  ;; null tester should be the same as no tester
  (unless tester
    (setq tester 'default-translator-tester)
    (setq tester-definitive t))
  (let* ((real-from-type (expand-presentation-type-abbreviation from-type))
	 (real-to-type (expand-presentation-type-abbreviation to-type)))
    (with-keywords-removed (translator-options
			    (:gesture :tester :tester-definitive :documentation
			     :pointer-documentation :menu :priority
			     :translator-class))
      `(add-translator (presentation-translators (find-command-table
						  ',command-table))
		       (make-instance
			',translator-class
			:name ',name
			:from-type ',real-from-type
			:to-type ',real-to-type
			:gesture ,(if (eq gesture t)
				      t
				      `(gethash ',gesture *gesture-names*))
			:tester ,(if (symbolp tester)
				     `',tester
				     `#',(make-translator-fun (car tester)
							      (cdr tester)))
			:tester-definitive ',tester-definitive
			:documentation #',(make-documentation-fun
					   (if documentationp
					       documentation
					       (command-name-from-symbol
						name)))
			,@(when pointer-documentation-p
				`(:pointer-documentation
				  #',(make-documentation-fun
				      pointer-documentation)))
			:menu ',menu
			:priority ,priority
			:translator-function #',(make-translator-fun arglist
								     body)
			,@translator-options)))))

(defmacro define-presentation-action
    (name (from-type to-type command-table &key
	   (gesture :select)
	   (tester 'default-translator-tester)
	   (documentation nil documentationp)
	   (pointer-documentation nil pointer-documentation-p)
	   (menu t)
	   (priority 0))
     arglist
     &body body)
  (let* ((real-from-type (expand-presentation-type-abbreviation from-type))
	 (real-to-type (expand-presentation-type-abbreviation to-type)))
    `(add-translator (presentation-translators (find-command-table
						',command-table))
      (make-instance
       'presentation-action
       :name ',name
       :from-type ',real-from-type
       :to-type ',real-to-type
       :gesture ,(if (eq gesture t)
		     t
		     `(gethash ',gesture *gesture-names*))
       :tester ,(if (symbolp tester)
		    `',tester
		    `#',(make-translator-fun (car tester)
					     (cdr tester)))
       :tester-definitive t
       :documentation #',(make-documentation-fun (if documentationp
						     documentation
						     (command-name-from-symbol
						      name)))
       ,@(when pointer-documentation-p
	       `(:pointer-documentation
		 #',(make-documentation-fun pointer-documentation)))
       :menu ',menu
       :priority ,priority
       :translator-function #',(make-translator-fun arglist
						    body)))))

;;; define-presentation-to-command-translator is in commands.lisp

;;; 23.7.2 Presentation Translator Functions

;;; Used by map-over-presentation-type-supertypes as well

(defun map-over-ptype-superclasses (function type)
  (let* ((type-name (presentation-type-name type))
	 (type-meta (get-ptype-metaclass type-name))
	 (type-is-ptype (typep type-meta 'presentation-type-class)))
    (unless type-meta
      (return-from map-over-ptype-superclasses nil))
    (loop
       for super-meta in (safe-cpl type-meta)
       ;; structure classes?
       when (and (or (typep super-meta 'standard-class)
		     (eq super-meta *builtin-t-class*))
		 (not (and type-is-ptype
			   (eq super-meta *standard-object-class*))))
       do (funcall function super-meta))))

;;; This is to implement the requirement on presentation translators
;;; for doing subtype calculations without reference to type
;;; parameters.  We are generous in that we return T when we are
;;; unsure, to give translator testers a chance to accept or reject
;;; the translator.  This is essentially 
;;;   (multiple-value-bind (yesp surep)
;;;       (presentation-subtypep maybe-subtype type)
;;;     (or yesp (not surep)))
;;; except faster.
(defun stupid-subtypep (maybe-subtype type)
  "Return t if maybe-subtype is a presentation subtype of type, regardless of
  parameters."
  (when (or (eq maybe-subtype nil) (eq type t))
    (return-from stupid-subtypep t))
  (when (eql maybe-subtype type)
    (return-from stupid-subtypep t))
  (let ((maybe-subtype-name (presentation-type-name maybe-subtype))
	(type-name (presentation-type-name type)))
    (cond
      ;; see DEFUN PRESENTATION-SUBTYPEP for some caveats
      ((eq maybe-subtype-name 'or)
       (let ((or-types (decode-parameters maybe-subtype)))
         (every (lambda (x) (stupid-subtypep x type)) or-types)))
      ((eq type-name 'and)
       (stupid-subtypep maybe-subtype (car (decode-parameters type))))
      ((eq type-name 'or)
       (let ((or-types (decode-parameters type)))
         (some (lambda (x) (stupid-subtypep maybe-subtype x)) or-types)))
      ((eq maybe-subtype-name 'and)
       ;; this clause is actually not conservative, but probably in a
       ;; way that no-one will complain about too much.  Basically, we
       ;; will only return T if the first type in the AND (which is
       ;; treated specially by CLIM) is subtypep the maybe-supertype
       (stupid-subtypep (car (decode-parameters maybe-subtype)) type))
      (t
       (let ((subtype-meta (get-ptype-metaclass maybe-subtype-name))
             (type-meta (get-ptype-metaclass type-name)))
         (unless (and subtype-meta type-meta)
           (return-from stupid-subtypep nil))
         (map-over-ptype-superclasses #'(lambda (super)
                                          (when (eq type-meta super)
                                            (return-from stupid-subtypep t)))
                                      maybe-subtype-name)
         nil)))))

(defun find-presentation-translators (from-type to-type command-table)
  (let* ((command-table (find-command-table command-table))	 
	 (from-name (presentation-type-name from-type))
	 (to-name (presentation-type-name to-type))
	 (cached-translators (gethash (cons from-name to-name)
				      (presentation-translators-cache
				       (presentation-translators
					command-table)))))
    (when cached-translators
      (return-from find-presentation-translators cached-translators))
    (let ((translator-vector (make-array 8 :adjustable t :fill-pointer 0))
	  (table-counter 0))
      (do-command-table-inheritance (table command-table)
	(let ((translator-map (simple-type-translators
			       (presentation-translators table))))
	  (flet ((get-translators (type)
		   (let ((translators (gethash type translator-map)))
		     (loop for translator in translators
			   if (stupid-subtypep (to-type translator)
					       to-type)
			   do (vector-push-extend (cons translator
							table-counter)
						  translator-vector)))))
	    (map-over-ptype-superclasses #'(lambda (super)
					     (get-translators (type-name
							       super)))
					 from-name)))
	(incf table-counter))
      (let ((from-super-names nil))
	(map-over-ptype-superclasses #'(lambda (super)
					 (push (type-name super)
					       from-super-names))
				     from-name)
	(setq from-super-names (nreverse from-super-names))
	;; The Spec mentions "high order priority" and "low order priority"
	;; without saying what that is!  Fortunately, the Franz CLIM user guide
	;; says that high order priority is (floor priority 10), low order
 	;; priority is (mod priority 10.) That's pretty wacked...
	(flet ((translator-lessp (a b)
		 (destructuring-bind (translator-a . table-num-a)
		     a
		   (destructuring-bind (translator-b . table-num-b)
		       b
		     (multiple-value-bind (hi-a low-a)
			 (floor (priority translator-a))
		       (multiple-value-bind (hi-b low-b)
			   (floor (priority translator-b))
			 ;; High order priority
			 (cond ((> hi-a hi-b)
				(return-from translator-lessp t))
			       ((< hi-a hi-b)
				(return-from translator-lessp nil)))
			 ;; more specific
			 (let ((a-precedence (position 
					      (presentation-type-name
					       (from-type translator-a))
					      from-super-names))
			       (b-precedence (position 
					      (presentation-type-name
					       (from-type translator-b))
					      from-super-names)))
			   (cond ((< a-precedence b-precedence)
				  (return-from translator-lessp t))
				 ((> a-precedence b-precedence)
				  (return-from translator-lessp nil))))
			 ;; Low order priority
			 (cond ((> low-a low-b)
				(return-from translator-lessp t))
			       ((< low-a low-b)
				(return-from translator-lessp nil)))))
		     ;; Command table inheritance
		     (< table-num-a table-num-b)))))
	  ;; Add translators to their caches.
	  (setf (gethash (cons from-name to-name)
			 (presentation-translators-cache
			  (presentation-translators command-table)))
                (remove-duplicates
                 (map 'list
                      #'car
                      (sort translator-vector #'translator-lessp)))))))))

(defgeneric call-presentation-translator
    (translator presentation context-type frame event window x y))

(defmethod call-presentation-translator
    ((translator presentation-translator) presentation context-type
     frame event window x y)
  ;; Let the translator return an explict ptype of nil to, in effect, abort the
  ;; presentation throw.
  (multiple-value-call
      #'(lambda (object &optional (ptype context-type) options)
	  (values object ptype options))
    (funcall (translator-function translator)
	     (presentation-object presentation)
	     :presentation presentation
	     :context-type context-type
	     :frame frame
	     :event event
	     :window window
	     :x x
	     :y y)))

(defmethod call-presentation-translator
    ((translator presentation-action) presentation context-type
     frame event window x y)
  (funcall (translator-function translator)
	   (presentation-object presentation)
	   :presentation presentation
	   :context-type context-type
	   :frame frame
	   :event event
	   :window window
	   :x x
	   :y y)
  (values nil nil nil))

(defun document-presentation-translator (translator
					 presentation
					 context-type
					 frame
					 event
					 window
					 x y
					 &key (stream *standard-output*)
					 (documentation-type :normal))
  (funcall (if (eq documentation-type :normal)
	       (translator-documentation translator)
	       (pointer-documentation translator))
	   (presentation-object presentation)
	   :presentation presentation
	   :context-type context-type
	   :frame frame
	   :event event
	   :window window
	   :x x
	   :y y
	   :stream stream))

;;; :button is a pointer button state, for performing matches where we want to
;;; restrict the match to certain gestures but don't have a real event.

(defun test-presentation-translator
    (translator presentation context-type frame window x y
     &key event (modifier-state 0) for-menu button)
  (flet ((match-gesture (gesture event modifier-state)
	   (let ((modifiers (if event
				(event-modifier-state event)
				modifier-state)))
	     (or (eq gesture t)
		 for-menu
		 (loop for g in gesture
		       thereis (and (eql modifiers (caddr g))
				    (or (and button (eql button (cadr g)))
					(and (null button)
					     (or (null event)
						 (eql (pointer-event-button
						       event)
						      (cadr g)))))))))))
    (let* ((from-type (from-type translator)))
      (unless (match-gesture (gesture translator) event modifier-state)
	(return-from test-presentation-translator nil))
      (unless (or (null (decode-parameters from-type))
		  (presentation-typep (presentation-object presentation)
				      from-type))
	(return-from test-presentation-translator nil))
      (unless (or (null (tester translator))
		  (funcall (tester translator)
			   (presentation-object presentation)
			   :presentation presentation
			   :context-type context-type
			   :frame frame
			   :window window
			   :x x
			   :y y
			   :event event))
	(return-from test-presentation-translator nil))
      (unless (or (tester-definitive translator)
		  (null (decode-parameters context-type))
		  (presentation-typep (call-presentation-translator
				       translator
				       presentation
				       context-type
				       frame
				       event
				       window
				       x y)
				      context-type))
	(return-from test-presentation-translator nil))))
  t)

;;; presentation-contains-position moved to presentation-defs.lisp

(defun map-over-presentations-containing-position (func record x y)
  "maps recursively over all presentations in record, including record."
  (map-over-output-records-containing-position
   #'(lambda (child)
       (when (output-record-p child)	; ? What else could it be? --moore
	 (map-over-presentations-containing-position func child x y))
       #+nil
       (when (presentationp child)
	 (funcall func child)))
   record
   x y)
  (when (and (presentationp record)
	     (presentation-contains-position record x y))
    (funcall func record)))

(defvar *null-presentation*)

(defun map-applicable-translators (func
				   presentation input-context frame window x y
				   &key event (modifier-state 0)
					for-menu
					button)
  (flet ((process-presentation (context context-ptype presentation)
	   (let ((maybe-translators
		  (find-presentation-translators (presentation-type 
						  presentation)
						 context-ptype
						 (frame-command-table
						  frame))))
	     (loop for translator in maybe-translators
		 when (and (or (not for-menu) (eql for-menu (menu translator)))
			   (test-presentation-translator translator
							 presentation
							 context-ptype
							 frame
							 window
							 x y
							 :event event
							 :modifier-state
							 modifier-state
							 :for-menu for-menu
							 :button button))
		 do (funcall func translator presentation context)))))
    (if (and (presentationp presentation)
	     (presentation-subtypep (presentation-type presentation) 
				    'blank-area))
	(loop for context in input-context
	      for (context-ptype) = context
	      do (process-presentation context context-ptype presentation))
	(loop for context in input-context
	      for (context-ptype) = context
	      do (map-over-presentations-containing-position
		  #'(lambda (p)
		      (process-presentation context context-ptype p))
		  presentation
		  x y)))))

(defun window-modifier-state (window)
  "Provides default modifier state for presentation translator functions."
  (let ((pointer (port-pointer (port window))))
    (pointer-modifier-state pointer)))

(defun find-applicable-translators
    (presentation input-context frame window x y
     &key event (modifier-state (window-modifier-state window)) for-menu fastp)
  (let ((results nil))
    (flet ((fast-func (translator presentation context)
	     (declare (ignore translator presentation context))
	     (return-from find-applicable-translators t))
	   (slow-func (translator presentation context)
	     (push (list translator presentation (input-context-type context))
		   results)))
      (map-applicable-translators (if fastp #'fast-func #'slow-func)
				  presentation input-context frame window x y
				  :event event
				  :modifier-state modifier-state
				  :for-menu for-menu)
      (nreverse results))))

(defun presentation-matches-context-type
    (presentation context-type frame window x y &key event (modifier-state 0))
  (let* ((ptype (expand-presentation-type-abbreviation (presentation-type
						       presentation)))
	 (ctype (expand-presentation-type-abbreviation context-type))
	 (translators (find-presentation-translators ptype
						     ctype
						     (frame-command-table
						      frame))))
    (loop for translator in translators
	  if (test-presentation-translator translator
					   presentation
					   ctype
					   frame
					   window
					   x y
					   :event event
					   :modifier-state modifier-state)
	  do (return-from presentation-matches-context-type t))
    nil))

;;; 23.7.3 Finding Applicable Presentations

(defun find-innermost-presentation-match
    (input-context top-record frame window x y event modifier-state button)
  "Helper function that implements the \"innermost-smallest\" input-context
  presentation matching algorithm.  Returns presentation, translator, and
  matching input context."
  (let ((result nil)
	(result-translator nil)
	(result-context nil)
	(result-size nil))
    (map-applicable-translators
     #'(lambda (translator presentation context)
         (if (and result-context (not (eq result-context context)))
	     ;; Return inner presentation
	     (return-from find-innermost-presentation-match
	       (values result result-translator result-context))
	     (multiple-value-bind (min-x min-y max-x max-y)
		 (output-record-hit-detection-rectangle* presentation)
	       (let ((size (* (- max-x min-x) (- max-y min-y))))
		 (when (or (not result) (< size result-size))
		   (setq result presentation)
		   (setq result-translator translator)
		   (setq result-context context)
		   (setq result-size size))))))
     top-record
     input-context
     frame
     window
     x y
     :event event
     :modifier-state modifier-state
     :button button)
    (when result
      (return-from find-innermost-presentation-match
	(values result result-translator result-context)))
    (map-applicable-translators
     #'(lambda (translator presentation context)
	 (return-from find-innermost-presentation-match
	   (values presentation translator context)))
     *null-presentation*
     input-context
     frame
     window
     x y
     :event event
     :modifier-state modifier-state
     :button button))
  nil)

(defun find-innermost-applicable-presentation
    (input-context window x y
     &key (frame *application-frame*)
     (modifier-state (window-modifier-state window))
     event)
  (values (find-innermost-presentation-match input-context
                                             (stream-output-history window)
                                             frame
                                             window
                                             x y
                                             event
                                             modifier-state
					     nil)))

(defun find-innermost-presentation-context
    (input-context window x y
     &key (top-record (stream-output-history window))
     (frame *application-frame*)
     event
     (modifier-state (window-modifier-state window))
     button)
  (find-innermost-presentation-match input-context
				     top-record
				     frame
				     window
				     x y
				     event
				     modifier-state
				     button))

(defun throw-highlighted-presentation (presentation input-context event)
  (let ((x (pointer-event-x event))
	(y (pointer-event-y event))
	(window (event-sheet event)))
    (multiple-value-bind (p translator context)
	(find-innermost-presentation-match input-context
					   presentation
					   *application-frame*
					   (event-sheet event)
					   x y
					   event
					   0
					   nil)
      (when p
	(multiple-value-bind (object ptype options)
	    (call-presentation-translator translator
					  p
					  (input-context-type context)
					  *application-frame*
					  event
					  window
					  x y)
	  (when ptype
	    (funcall (cdr context) object ptype event options)))))))

(defvar *input-context*)

(defun throw-object-ptype (object type
			   &key (input-context *input-context*) sheet)
  "Throw an object and presentation type within input-context without
a presentation"
  (throw-highlighted-presentation
                          (make-instance 'standard-presentation
                                         :object object :type type
					 :single-box t)
			  input-context
                          (make-instance 'pointer-button-press-event
                                         :sheet sheet
                                         :x 0 :y 0
                                         :modifier-state 0
                                         :button +pointer-left-button+)))

(defstruct presentation-translator-menu-item
  translator
  presentation
  context)

(defun call-presentation-menu
    (presentation input-context frame window x y
     &key (for-menu t) label)
  (let (items)
    (map-applicable-translators
     #'(lambda (translator presentation context)
	 (push
          `(,(make-presentation-translator-menu-item :translator translator
                                                     :presentation presentation
                                                     :context context)
             :documentation ,(with-output-to-string (stream)
                               (document-presentation-translator
                                translator
                                presentation
                                input-context
                                frame nil window x y
                                :stream stream)))
          items))
     presentation input-context frame window x y :for-menu for-menu)
    (when items
      (setq items (nreverse items))
      (multiple-value-bind (item object event)
          (menu-choose items
                       :label label
                       :associated-window window
                       :printer #'(lambda (item stream)
                                    (let ((object (first item)))
                                     (document-presentation-translator
                                      (presentation-translator-menu-item-translator object)
                                      (presentation-translator-menu-item-presentation object)
                                      (presentation-translator-menu-item-context object)
                                      frame nil window x y
                                      :stream stream)))
                       :label label
                       :pointer-documentation *pointer-documentation-output*)
        (declare (ignore object))
        (when item
          (multiple-value-bind (object ptype options)
              (call-presentation-translator (presentation-translator-menu-item-translator item)
                                            (presentation-translator-menu-item-presentation item)
                                            (presentation-translator-menu-item-context item)
                                            frame
                                            event
                                            window
                                            x y)
            (when ptype
              (funcall (cdr (presentation-translator-menu-item-context item)) object ptype event options))))))))

#+nil
(defmethod highlight-output-record ((record standard-presentation)
				    stream state)
  (map-over-output-records
   (lambda (child)
     (highlight-output-record child stream state))
   record))

;;; Context-dependent input
;;; An input context is a cons of a presentation type and a continuation to
;;; call to return a presentation to that input context.

(defvar *input-context* nil)

(defun input-context-type (context-entry)
  (car context-entry))

;;; Many presentation functions, internal and external, take an input
;;; context as an argument, but they really only need to look at one
;;; presentation type.
(defun make-fake-input-context (ptype)
  (list (cons (expand-presentation-type-abbreviation ptype)
	      #'(lambda (object type event options)
		  (declare (ignore event options))
		  (error "Fake input context called with object ~S type ~S. ~
                          This shouldn't happen!"
			 object type)))))

(defun input-context-wait-test (stream)
  (let* ((queue (stream-input-buffer stream))
	 (event (event-queue-peek queue)))
    (when event
      (let ((sheet (event-sheet event)))
	(when (and (output-recording-stream-p sheet)
		   (or (typep event 'pointer-event)
		       (typep event 'keyboard-event))
		   (not (gadgetp sheet)))
	  (return-from input-context-wait-test t))))
    nil))

(defun highlight-applicable-presentation (frame stream input-context
					  &optional (prefer-pointer-window t))
  (let* ((queue (stream-input-buffer stream))
	 (event (event-queue-peek queue)))
    (when (and event
	       (or (and (typep event 'pointer-event)
			(or prefer-pointer-window 
			    (eq stream (event-sheet event))))
		   (typep event 'keyboard-event)))
      ;; Stream only needs to see button press events.
      ;; XXX Need to think about this more.  Should any pointer events be
      ;; passed through?  If there's no presentation, maybe?
      (unless (typep event 'keyboard-event)
	(event-queue-read queue))
      (progn
	(frame-input-context-track-pointer frame
					   input-context
					   (event-sheet event)
					   event)
	(when (typep event 'pointer-button-press-event)
	  (funcall *pointer-button-press-handler* stream event)))
      #+nil
      (if (and (typep event 'pointer-motion-event)
	       (pointer-event-button event))
	  (frame-drag frame input-context (event-sheet event) event)
	  ))))

(defun input-context-event-handler (stream)
  (highlight-applicable-presentation *application-frame*
				     stream
				     *input-context*))

(defun input-context-button-press-handler (stream button-event)
  (declare (ignore stream))
  (frame-input-context-button-press-handler *application-frame*
					    (event-sheet button-event)
					    button-event))

(defun highlight-current-presentation (frame input-context)
  (let ((event (synthesize-pointer-motion-event (port-pointer
						 (port
						  *application-frame*)))))
    (when event
      (frame-input-context-track-pointer frame
                                         input-context
                                         (event-sheet event)
                                         event))))

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
		    (cons (cons (expand-presentation-type-abbreviation ,type)
                                #'(lambda (object type event options)
				    (return-from ,context-block
				      (values object type event options))))
			  ,(if override nil '*input-context*)))
		   (*pointer-button-press-handler*
		    #'input-context-button-press-handler)
		   (*input-wait-test* #'input-context-wait-test)
		   (*input-wait-handler* #'input-context-event-handler))
	       (return-from ,return-block ,form )))
         (declare (ignorable ,@vars))
	 (highlight-current-presentation *application-frame* *input-context*)
	 (cond ,@(mapcar #'(lambda (pointer-case)
			     (destructuring-bind (case-type &body case-body)
				 pointer-case
			       `((presentation-subtypep ,type-var ',case-type)
				 ,@case-body)))
			 pointer-cases))))))
