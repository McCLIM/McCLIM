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
  (when (eq name 'presentation-subtypep)
    ;; I feel so unclean!
    (return-from define-presentation-method
      `(define-subtypep-method ,@args)))
  (let ((gf (find-presentation-gf name)))
    (with-accessors ((parameters-arg parameters-arg)
                     (options-arg options-arg))
        gf
      (multiple-value-bind (qualifiers lambda-list decls body)
          (parse-method-body args)
        (let ((type-arg (nth (1- (type-arg-position gf)) lambda-list)))
          (with-current-source-form (type-arg)
            (unless (consp type-arg)
              (error "Type argument in presentation method must be ~
                      specialized"))
            (unless (eq (car type-arg)  'type)
              (error "Type argument mismatch with presentation generic ~
                      function definition")))
          (destructure-type-arg (type-var type-spec type-name) type-arg
            (let* ((method-ll `((,(type-key-arg gf)
                                 ,(ptype-specializer type-spec))
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
