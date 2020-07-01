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
;;; Implementation of the presentation generic functions and methods.
;;;

(in-package #:clim-internals)

;;; Presentation methods.
;;;
;;; The basic dispatch is performed via CLOS instances that are standins for
;;; the presentation types.  There are a couple of complications to this
;;; simple plan.  First, methods on the presentation type class
;;; STANDARD-OBJECT -- if there are any -- should not be applicable to
;;; presentation types that are not CLOS classes, even though STANDARD-OBJECT
;;; is in the class precedence list of the standin object. Our own methods on
;;; COMPUTE-APPLICABLE-METHODS-USING-CLASSES and COMPUTE-APPLICABLE-METHODS
;;; remove methods specialized on standard-object.
;;;
;;; The second major complication is the whole raison d'etre of presentation
;;; type methods: type parameters and options are massaged so that applicable
;;; methods written on the supertype of a presentation type get parameters and
;;; options in the expected form.  "Real" CLIM apparently does this massaging
;;; in the body of the effective method and passes the massaged parameters as
;;; an argument into the method.  We do it with a function call within the
;;; body of the method.  This is potentially more expensive, but caching
;;; should help that.  Our method has the huge advantage of working with any
;;; method combination.

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

(defun find-presentation-gf (name &optional (errorp t))
  (or (gethash name *presentation-gf-table*)
      (when errorp
        (error "~S is not the name of a presentation generic function" name))))

(defclass presentation-generic-function (standard-generic-function)
  ()
  (:metaclass c2mop:funcallable-standard-class))

(defvar *standard-object-class* (find-class 'standard-object))

#-scl
(defmethod c2mop:compute-applicable-methods-using-classes :around
    ((gf presentation-generic-function) classes)
  (multiple-value-bind (methods success)
      (call-next-method)
    (let ((ptype-class (car classes)))
      (if (or (null success)
              (not (typep ptype-class 'presentation-type-class)))
          (values methods success)
          (values (remove-if #'(lambda (method)
                                 (eq (car (c2mop:method-specializers
                                           method))
                                     *standard-object-class*))
                             methods)
                  t)))))

#+scl
(defmethod c2mop:compute-applicable-methods-using-classes :around
    ((gf presentation-generic-function) classes)
  (multiple-value-bind (methods success non-class-positions)
      (call-next-method)
    (let ((ptype-class (car classes)))
      (if (or (null success)
              (not (typep ptype-class 'presentation-type-class)))
          (values methods non-class-positions non-class-positions)
          (values (remove-if #'(lambda (method)
                                 (eq (car (c2mop:method-specializers
                                           method))
                                     *standard-object-class*))
                             methods)
                  t
                  non-class-positions)))))

(defmethod compute-applicable-methods :around
    ((gf presentation-generic-function) arguments)
  (let ((methods (call-next-method)))
    (if (typep (class-of (car arguments)) 'presentation-type-class)
        (remove-if #'(lambda (method)
                       (eq (car (c2mop:method-specializers method))
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
              (let ((ptype (find-presentation-type (class-name class) nil)))
                (and ptype (list ptype))))
          (c2mop:class-direct-superclasses type)))

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
  (destructuring-bind (type-key-arg &optional parameters-arg options-arg &rest rest)
      lambda-list
    (with-current-source-form (lambda-list)
      ;; What is a difference between TYPE-KEY and TYPE-CLASS?
      ;;
      ;; In McCLIM there is no difference but CLIM-TOS makes a
      ;; distinction during the generic function dispatch:
      ;;
      ;; - TYPE-KEY   - methods are inherited
      ;; - TYPE-CLASS - methods are not inherited
      ;;
      ;; When methods are not inherited and the presentation type
      ;; class doesn't have a method specialized on it, the default
      ;; method of a generic function is used instead of the method
      ;; specialized on its supertype. -- jd 2020-06-30
      (unless (member type-key-arg '(type-key type-class))
        (error "The first argument in a presentation generic function ~
                must be type-key or type-class"))
      (unless (eq parameters-arg 'parameters)
        (setq parameters-arg nil))
      (unless (eq options-arg 'options)
        (setq options-arg nil))
      (let* ((gf-lambda-list (cons type-key-arg
                                   (cond (options-arg
                                          rest)
                                         (parameters-arg
                                          (cddr lambda-list))
                                         (t
                                          (cdr lambda-list)))))
             ;; XXX should check that it's required
             (type-arg-pos (position 'type gf-lambda-list)))
        (unless type-arg-pos
          (error "type must appear as an argument in a presentation ~
                  generic function lambda list"))
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
             ,@options))))))

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
  (multiple-value-bind (qualifiers lambda-list decls body)
      (parse-method-body args)
    (let* ((gf (find-presentation-gf name))
           (par-arg (parameters-arg gf))
           (opt-arg (options-arg gf))
           (real-body body)
           (massaged-type (gensym "MASSAGED-TYPE"))
           (type-arg-pos (type-arg-position gf))
           (type-arg (nth (1- type-arg-pos) lambda-list))
           (type-key-arg (type-key-arg gf)))
      (with-current-source-form (type-arg)
        (unless (consp type-arg)
          (error "Type argument in presentation method must be ~
                 specialized"))
        (unless (eq (car type-arg) 'type)
          (error "Type argument mismatch with presentation generic ~
                 function definition")))
      (destructure-type-arg (type-var type-spec type-name) type-arg
        ;; The semantics of the presentation method
        ;; PRESENTATION-SUBTYPEP are truly weird; method combination is
        ;; in effect disabled.  So, the methods have to be eql methods.
        (let ((method-ll `((,type-key-arg
                            ,(if (eq name 'presentation-subtypep)
                                 `(eql (prototype-or-error ',type-name))
                                 (ptype-specializer type-spec)))
                           ,@(copy-list lambda-list))))
          ;; In reality the presentation type is specialized on the
          ;; method's first argument. Replace the (TYPE-VAR TYPE-SPEC)
          ;; argument with the unspecialized argument TYPE-VAR.
          (setf (nth type-arg-pos method-ll) type-var)
          (when-let ((ptype (find-presentation-type type-name nil)))
            (let (opt-vars par-vars)
              (when opt-arg
                (let ((options-ll (options-lambda-list ptype)))
                  (setf opt-vars (get-all-params options-ll))
                  (setf real-body
                        `((let ((,opt-arg (decode-options ,massaged-type)))
                            (destructuring-bind ,options-ll ,opt-arg
                              (declare (ignorable ,@opt-vars))
                              ,@real-body))))))
              (when par-arg
                (let ((params-ll (parameters-lambda-list ptype)))
                  (setf par-vars (get-all-params params-ll))
                  (setf real-body
                        `((let ((,par-arg (decode-parameters ,massaged-type)))
                            (destructuring-bind ,params-ll ,par-arg
                              (declare (ignorable ,@par-vars))
                              ,@real-body))))))
              (when (or par-arg opt-arg)
                (let* ((mth-vars (get-all-params lambda-list))
                       (par-mth  (intersection   mth-vars par-vars))
                       (mth/par  (set-difference mth-vars par-vars))
                       (opt-mth  (intersection   mth/par  opt-vars))
                       (opt-par  (intersection   opt-vars par-vars)))
                  (when par-mth
                    (alexandria:simple-style-warning
                     "Method arguments ~s are shadowed by the ~
                     presentation type parameters." par-mth))
                  (when opt-mth
                    (alexandria:simple-style-warning
                     "Method arguments ~s are shadowed by the ~
                     presentation type options." opt-mth))
                  (when opt-par
                    (alexandria:simple-style-warning
                     "Presentation type parameters ~s are shadowed by ~
                     the presentation type options." opt-par)))
                (setf real-body
                      `((let ((,massaged-type (translate-specifier-for-type
                                               (type-name-from-type-key
                                                ,type-key-arg)
                                               ',type-name
                                               ,type-var)))
                          ,@real-body))))))
          `(defmethod ,(generic-function-name gf) ,@qualifiers ,method-ll
             ,@(when decls
                 (list decls))
             (block ,name
               ,@real-body)))))))

(defmacro define-default-presentation-method (name &rest args)
  (let ((gf (find-presentation-gf name)))
    (multiple-value-bind (qualifiers lambda-list decls body)
        (parse-method-body args)
      `(defmethod ,(generic-function-name gf) ,@qualifiers (,(type-key-arg gf)
                                                            ,@lambda-list)
         (declare (ignorable ,(type-key-arg gf))
                  ,@(cdr decls))
         (block ,name
           ,@body)))))

;;; Somewhat obsolete, but keep it around for
;;; apply-presentation-generic-function.
(defun %funcall-presentation-generic-function (name gf type-arg-position
                                               &rest args)
  (declare (ignore name))
  (let* ((type-spec (nth (1- type-arg-position) args))
         (ptype-name (presentation-type-name type-spec)))
    (apply gf (prototype-or-error ptype-name) args)))

;;; I wonder if this pattern for preserving order of evaluation is
;;; has a more general use...

(defmacro funcall-presentation-generic-function (name &rest args)
  (let ((gf (find-presentation-gf name)))
    (let* ((rebound-args (loop for arg in args
                               unless (symbolp arg)
                                 collect (list (gensym "ARG") arg)))
           (gf-name (generic-function-name gf))
           (type-spec-var (nth (1- (type-arg-position gf)) args)))
      `(let ,rebound-args
         (,gf-name (prototype-or-error
                    (presentation-type-name
                     ,(or (first (find type-spec-var rebound-args
                                       :key #'second))
                          type-spec-var)))
                   ,@(mapcar #'(lambda (arg)
                                 ;; Order of evaluation doesn't matter
                                 ;; for symbols, and this shuts up
                                 ;; warnings about arguments in a
                                 ;; keyword position not being
                                 ;; constant. By the way, why do we
                                 ;; care about order of evaluation
                                 ;; here? -trh
                                 (or (first (find arg rebound-args
                                                  :key #'second))
                                     arg))
                             args))))))

(defmacro apply-presentation-generic-function (name &rest args)
  (let ((gf (find-presentation-gf name)))
    `(apply #'%funcall-presentation-generic-function ',name
            #',(generic-function-name gf)
            ,(type-arg-position gf)
            ,@args)))
