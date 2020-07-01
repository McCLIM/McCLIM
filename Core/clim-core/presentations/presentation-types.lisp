;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998-2000 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2001-2002 by Tim Moore <moore@bricoworks.com>
;;;  (c) copyright 2020 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Defining presentation types and abbreviations.
;;;

(in-package #:clim-internals)

(defvar *presentation-type-table* (make-hash-table :test #'eq))

(defun find-presentation-type (name &optional (errorp t))
  (or (gethash name *presentation-type-table*)
      (when errorp
        (error "~S is not the name of a presentation type" name))))

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
  (let ((type (presentation-type instance))
        (object (presentation-object instance)))
    (cond ((null type)
           (error "A presentation can't have a type NIL."))
          ;; Enforcing the object to be presentation-typep to its
          ;; presentation type is (imo) the right thing to do and
          ;; avoids multiple problems down the road when used with
          ;; translators and in presentation methods. That said
          ;; currently we work hard on improving the presentation type
          ;; inheritance and we don't want to introduce too many
          ;; breaking changes at once. -- jd 2020-07-07
          #+ (or)
          ((not (presentation-typep object type))
           (error "The presentation object ~s isn't of the ~
                   presentation type ~s." object type)))))

(defmethod print-object ((self standard-presentation) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (with-bounding-rectangle* (x1 y1 x2 y2)
        self
      (format stream "~D:~D,~D:~D ~S" x1 x2 y1 y2 (presentation-type self))
      (when *print-presentation-verbose*
        (format stream " ~S" (presentation-object self))))))

(defgeneric ptype-specializer (type)
  (:documentation "The specializer to use for this type in a presentation
method lambda list"))

(defmethod ptype-specializer ((type symbol))
  (if-let ((ptype (or (find-presentation-type type nil)
                      (find-class type nil))))
    (ptype-specializer ptype)
    ;; Assume it's a forward referenced CLOS class.
    type))

(defmethod ptype-specializer ((type standard-class))
  (class-name type))

(defmethod ptype-specializer ((type cons))
  (if (typep type '(cons (eql eql)
                    (cons t null)))
      type
      (error "~s is not a valid presentation method specializer." type)))

(defmacro destructure-type-arg
    ((type-var type-spec type-name) type-arg &body body)
  `(destructuring-bind (,type-var ,type-spec) ,type-arg
     (let ((,type-var ,type-var)
           (,type-spec ,type-spec)
           (,type-name (if (atom ,type-spec)
                           ,type-spec
                           (presentation-type-of (second ,type-spec)))))
       ,@body)))

;;; Metaclass for presentation types.  For presentation types not associated
;;; with CLOS classes, objects with this metaclass are used as a proxy for the
;;; type during presentation method dispatch. We don't prevent these
;;; presentation proxies from being subclasses of standard-object,
;;; but in presentation method dispatch we remove methods on standard
;;; object for presentation types that are not CLOS classes. The
;;; presentation type metaclass for T is the builtin object T instead
;;; of a presentation-type-class; in this way we can avoid weirdness
;;; with T being a subtype of standard-object!

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
  (if-let ((maybe-meta (find-presentation-type type nil)))
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
  (or (find-presentation-type name nil)
      (let ((meta (find-class name nil)))
        (and (typep meta 'standard-class)
             meta))))

(defgeneric presentation-ptype-supers (type)
  (:documentation "Gets a list of the presentation type objects for those
supertypes of TYPE that are presentation types"))

(defmethod presentation-ptype-supers ((type (eql *builtin-t-class*)))
  nil)

(defmethod presentation-ptype-supers ((type symbol))
  (if-let ((ptype (find-presentation-type type nil)))
    (presentation-ptype-supers ptype)
    nil))

(defmethod presentation-ptype-supers ((type presentation-type-class))
  (mapcan #'(lambda (class)
              (typecase class
                (presentation-type
                 (list class))
                (standard-class
                 (if-let ((clos-ptype (find-presentation-type (class-name class) nil)))
                   (list clos-ptype)
                   nil))
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

;;; We need to patch defclass in every implementation to record a CLOS
;;; class at compile time. On the other hand, I think we can assume
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
      (string-trim " " downcase-name))))

(defun record-presentation-type (name
                                 parameters params-ll
                                 options options-ll
                                 description history
                                 parameters-are-types
                                 compile-time-p
                                 supers expansion-func)
  (let* ((fake-name (make-presentation-type-name name))
         (ptype-class-args (list :type-name name
                                 :parameters parameters
                                 :parameters-lambda-list params-ll
                                 :options options
                                 :options-lambda-list options-ll
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
               (let ((clos-meta (find-class name nil)))
                 (if-let ((closp (typep clos-meta 'standard-class)))
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
                                     inherit-from-lambda
                                     description history parameters-are-types)
  (let* ((inherit-typespec (funcall (coerce inherit-from-lambda 'function)
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
                                 ',options-ll
                                 ',description ',history
                                 ',parameters-are-types
                                 nil ',superclasses
                                 (function ,expansion-lambda))
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
         (record-presentation-type ',name ',parameters ',params-ll
                                   ',options ',options-ll
                                   ',description ',history
                                   ',parameters-are-types
                                   t nil nil))
       (eval-when (:load-toplevel :execute)
         (%define-presentation-type ,name ,parameters ,params-ll
                                    ,options ,options-ll
                                    ,inherit-from-func
                                    ,description ,history
                                    ,parameters-are-types)))))

(defun presentation-type-parameters (type-name &optional env)
  (declare (ignore env))
  (parameters (find-presentation-type type-name)))

(defun presentation-type-options (type-name &optional env)
  (declare (ignore env))
  (options (find-presentation-type type-name)))

;;; XXX specification states, that the type-name must be a
;;; presentation type specifier. Should we error otherwise?
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
               (destructuring-bind ,params-ll ,params
                 (declare (ignorable ,@ignorable-vars))
                 ,@body))))
        `(let ()
           ,@body))))

;;; XXX specification states, that the TYPE-NAME must be a
;;; presentation type specifier. Should we error otherwise?
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
    (if-let ((ptype (find-presentation-type name nil)))
      (with-presentation-type-decoded (name parameters defaults)
          (funcall (expansion-function ptype) name-and-params)
        (declare (ignore name parameters))
        (loop for (key val) on options by #'cddr
              for default = (getf defaults key)
              unless (equal val default)
                nconc (list key val) into needed-options
              finally (return (if needed-options
                                  `(,name-and-params ,@needed-options)
                                  name-and-params))))
      name-and-params)))

;;; This function is used to determine the CLIM presentation name from
;;; the Common Lisp object.
;;;
;;; Not defined as a generic function, but what the hell.
(defgeneric presentation-type-of (object))

(defmethod presentation-type-of (object)
  (declare (ignore object))
  'expression)

(flet ((get-ptype-from-class-of (object)
         (let ((name (class-name (class-of object))))
           (when-let ((ptype (find-presentation-type name nil)))
             ;; Does the type have required parameters?
             ;; If so, we can't use it...
             (let ((parameter-ll (parameters-lambda-list ptype)))
               (values name
                       (if (eq (car parameter-ll) '&whole)
                           (cddr parameter-ll)
                           parameter-ll)))))))

  (defmethod presentation-type-of ((object standard-object))
    (multiple-value-bind (name lambda-list)
        (get-ptype-from-class-of object)
      (cond ((and name
                  (or (null lambda-list)
                      (member (first lambda-list) lambda-list-keywords)))
             name)
            (name
             'standard-object)
            (t (let* ((class (class-of object))
                      (class-name (class-name class)))
                 (or class-name class))))))

  (defmethod presentation-type-of ((object structure-object))
    (multiple-value-bind (name lambda-list)
        (get-ptype-from-class-of object)
      (if (and name
               (or (null lambda-list)
                   (member lambda-list lambda-list-keywords)))
          name
          (call-next-method)))))

(defmethod presentation-type-of ((object symbol))
  (if (eq (symbol-package object) (find-package :keyword))
      'keyword
      'symbol))

(defmethod presentation-type-of ((object number))
  'number)

(defmethod presentation-type-of ((object complex))
  'complex)

(defmethod presentation-type-of ((object real))
  'real)

(defmethod presentation-type-of ((object rational))
  'rational)

(defmethod presentation-type-of ((object integer))
  'integer)

(defmethod presentation-type-of ((object ratio))
  'ratio)

(defmethod presentation-type-of ((object float))
  'float)

(defmethod presentation-type-of ((object character))
  'character)

;;; `(string ,length) would be more specific, but is not "likely to be useful
;;; to the programmer."
(defmethod presentation-type-of ((object string))
  'string)

(defmethod presentation-type-of ((object pathname))
  'pathname)

(defmethod presentation-type-of ((object cons))
  '(sequence t))

;;; Do something interesting with the array-element-type
(defmethod presentation-type-of ((object vector))
  '(sequence t))
