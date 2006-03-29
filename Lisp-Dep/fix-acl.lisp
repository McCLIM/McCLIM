;;; -*- Mode: Lisp; Package: User -*-

(in-package :common-lisp-user)

;;; Needed to keep ACL from issuing warnings about toplevel (shadow ...) forms
(setq comp:*cltl1-compile-file-toplevel-compatibility-p* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :loop)
  (require :mop))

(defpackage :clim-mop
    (:use :clos :common-lisp)
    (:export #:accessor-method-slot-definition
	     #:add-dependent
	     #:add-direct-method
	     #:add-direct-subclass
	     #:add-method
	     #:allocate-instance
	     #:built-in-class
	     #:class
	     #:class-default-initargs
	     #:class-direct-default-initargs
	     #:class-direct-slots
	     #:class-direct-subclasses
	     #:class-direct-superclasses
	     #:class-finalized-p
	     #:class-name
	     #:class-precedence-list
	     #:class-prototype
	     #:class-slots
	     #:compute-applicable-methods
	     #:compute-applicable-methods-using-classes
	     #:compute-class-precedence-list
	     #:compute-default-initargs
	     #:compute-discriminating-function
	     #:compute-effective-method
	     #:compute-effective-slot-definition
	     #:compute-slots
	     #:direct-slot-definition
	     #:direct-slot-definition-class
	     #:effective-slot-definition
	     #:effective-slot-definition-class
	     #:ensure-class
	     #:ensure-class-using-class
	     #:ensure-generic-function
	     #:ensure-generic-function-using-class
	     #:eql-specializer
	     #:eql-specializer-object
	     #:extract-lambda-list
	     #:extract-specializer-names
	     #:finalize-inheritance
	     #:find-method-combination
	     #:forward-referenced-class
	     #:funcallable-standard-class
	     #:funcallable-standard-instance-access
	     #:funcallable-standard-object
	     #:function
	     #:generic-function
	     #:generic-function-argument-precedence-order
	     #:generic-function-declarations
	     #:generic-function-lambda-list
	     #:generic-function-method-class
	     #:generic-function-method-combination
	     #:generic-function-methods
	     #:generic-function-name
	     #:intern-eql-specializer
	     #:make-instance
	     #:make-method-lambda
	     #:map-dependents
	     #:metaobject
	     #:method
	     #:method-combination
	     #:method-function
	     #:method-generic-function
	     #:method-lambda-list
	     #:method-qualifiers
	     #:method-specializers
	     #:reader-method-class
	     #:remove-dependent
	     #:remove-direct-method
	     #:remove-direct-subclass
	     #:remove-method
	     #:set-funcallable-instance-function
	     #:slot-boundp-using-class
	     #:slot-definition
	     #:slot-definition-allocation
	     #:slot-definition-initargs
	     #:slot-definition-initform
	     #:slot-definition-initfunction
	     #:slot-definition-location
	     #:slot-definition-name
	     #:slot-definition-readers
	     #:slot-definition-type
	     #:slot-definition-writers
	     #:slot-makunbound-using-class
	     #:slot-value-using-class
	     #:specializer
	     #:specializer-direct-generic-functions
	     #:specializer-direct-methods
	     #:standard-accessor-method
	     #:standard-class
	     #:standard-direct-slot-definition
	     #:standard-effective-slot-definition
	     #:standard-generic-function
	     #:standard-instance-access
	     #:standard-method
	     #:standard-object
	     #:standard-reader-method
	     #:standard-slot-definition
	     #:standard-writer-method
	     #:update-dependent
	     #:validate-superclass
	     #:writer-method-class))

;;;(eval-when (:compile-toplevel :load-toplevel :execute)
;;;  (do-external-symbols (sym :clos)
;;;	(export sym :clim-mop)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(clim-lisp-patch::defclass)
          :clim-lisp-patch))

(defvar clim-lisp-patch::*compile-time-clos-names* (make-hash-table))

(defun clim-lisp-patch::compile-time-clos-class-p (name)
  (gethash name clim-lisp-patch::*compile-time-clos-names* nil))

(defmacro clim-lisp-patch:defclass (name &rest args)
  `(progn
     (eval-when (:compile-toplevel)
       (setf (gethash ',name clim-lisp-patch::*compile-time-clos-names*) t))
     (cl:defclass ,name ,@args)))

#+nil
(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defvar clim-lisp-patch::*inline-functions* nil))

  (defmacro clim-lisp-patch:declaim (&rest args)
    (dolist (arg args)
      (cond ((and (consp arg) (eq (car arg) 'inline))
             (dolist (k (cdr arg))
               (pushnew k clim-lisp-patch::*inline-functions*)))))
    `(declaim ,@args) )

  (defmacro clim-lisp-patch:defun (fun args &body body)
    (cond ((member fun clim-lisp-patch::*inline-functions*)
           (cond ((and (consp fun) (eq (car fun) 'setf))
                  (let ((fnam (intern (concatenate 'string
                                        "(SETF " (symbol-name (cadr fun)) ")")
                                      (symbol-package (cadr fun)))))
                    `(progn
                       (defsetf ,(cadr fun) (&rest ap) (new-value)
                         (list* ',fnam new-value ap))
                       (defun ,fnam ,args .,body)
                       (define-compiler-macro ,fnam (&rest .args.)
                         (cons '(lambda ,args .,body)
                               .args.)))))
                 (t
                  `(progn
                     (defun ,fun ,args .,body)
                     (define-compiler-macro ,fun (&rest .args.)
                       (cons '(lambda ,args .,body)
                             .args.))))))
          (t
           `(defun ,fun ,args ,@body)))) )



