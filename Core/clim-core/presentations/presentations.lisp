;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998-2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2001-2002 by Tim Moore (moore@bricoworks.com)

;;; Implementation of the presentation type system and finding an applicable
;;; presentation.

(in-package #:clim-internals)

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

(defvar *null-presentation*)
(defvar *print-presentation-verbose* nil)

(defmethod initialize-instance :after ((instance presentation-mixin) &key)
  (unless (presentation-type instance)
    (error "A presentation can't have a type NIL.")))

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
           (declare (dynamic-extent #',continuation))
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric ptype-specializer (type)
    (:documentation "The specializer to use for this type in a presentation
method lambda list")))

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

(defmethod c2mop:validate-superclass ((class presentation-type-class)
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

;;; The presentation type specifier may be in one of these forms:
;;;
;;;     name
;;;    (name &rest params)
;;;   ((name &rest params) &rest options)
;;;
;;; GET-PTYPE-METACLASS accepts either the presentation type or its
;;; specifier and returns the presentation type metaclass.
(defgeneric get-ptype-metaclass (type))

(defmethod get-ptype-metaclass ((type symbol))
  (if-let ((maybe-meta (gethash type *presentation-type-table*)))
    (get-ptype-metaclass maybe-meta)
    (let ((system-meta (find-class type nil)))
      (and (typep system-meta 'standard-class)
           system-meta))))

(defmethod get-ptype-metaclass ((type presentation-type-class))
  type)

(defmethod get-ptype-metaclass ((type clos-presentation-type))
  (clos-class type))

(defmethod get-ptype-metaclass ((type (eql *builtin-t-class*)))
  type)

(defmethod get-ptype-metaclass ((type class))
  type)

(defmethod get-ptype-metaclass ((type cons))
  (let ((ptype-name (first type)))
    (if (atom ptype-name)
        (get-ptype-metaclass ptype-name)
        (let ((ptype-name (first ptype-name)))
          (if (atom ptype-name)
              (get-ptype-metaclass ptype-name)
              (call-next-method))))))

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
    (unless (c2mop:class-finalized-p ptype-meta)
      (c2mop:finalize-inheritance ptype-meta))
    (or (c2mop:class-prototype ptype-meta)
        (error "Couldn't find a prototype for ~S" name))))

(defun safe-cpl (class)
  (unless (c2mop:class-finalized-p class)
    (c2mop:finalize-inheritance class))
  (c2mop:class-precedence-list class))

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
          (c2mop:class-direct-superclasses type)))

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
  #-(or excl cmu sbcl openmcl ecl clasp)
  (defun compile-time-clos-p (name)
    (let ((meta (find-class name nil)))
      (and meta
           (typep meta 'standard-class))))

  #+(or excl cmu sbcl openmcl ecl clasp)
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
                         (apply #'c2mop:ensure-class fake-name
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

(defvar *input-context*)

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
  (alexandria:when-let* ((port-pointer (port-pointer (port *application-frame*)))
                         (event (synthesize-pointer-motion-event port-pointer))
                         (sheet (event-sheet event)))
    (frame-input-context-track-pointer frame input-context sheet event)))

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
