;;; -*- Mode: Lisp; Package: User -*-

(in-package :common-lisp-user)

;;; Needed to keep ACL from issuing warnings about toplevel (shadow ...) forms
(setq comp:*cltl1-compile-file-toplevel-compatibility-p* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :loop)
  (require :mop))

(defpackage :clim-mop
    (:use :clos :common-lisp)
    (:export "ACCESSOR-METHOD-SLOT-DEFINITION"
	     "ADD-DEPENDENT"
	     "ADD-DIRECT-METHOD"
	     "ADD-DIRECT-SUBCLASS"
	     "ADD-METHOD"
	     "ALLOCATE-INSTANCE"
	     "BUILT-IN-CLASS"
	     "CLASS"
	     "CLASS-DEFAULT-INITARGS"
	     "CLASS-DIRECT-DEFAULT-INITARGS"
	     "CLASS-DIRECT-SLOTS"
	     "CLASS-DIRECT-SUBCLASSES"
	     "CLASS-DIRECT-SUPERCLASSES"
	     "CLASS-FINALIZED-P"
	     "CLASS-NAME"
	     "CLASS-PRECEDENCE-LIST"
	     "CLASS-PROTOTYPE"
	     "CLASS-SLOTS"
	     "COMPUTE-APPLICABLE-METHODS"
	     "COMPUTE-APPLICABLE-METHODS-USING-CLASSES"
	     "COMPUTE-CLASS-PRECEDENCE-LIST"
	     "COMPUTE-DEFAULT-INITARGS"
	     "COMPUTE-DISCRIMINATING-FUNCTION"
	     "COMPUTE-EFFECTIVE-METHOD"
	     "COMPUTE-EFFECTIVE-SLOT-DEFINITION"
	     "COMPUTE-SLOTS"
	     "DIRECT-SLOT-DEFINITION"
	     "DIRECT-SLOT-DEFINITION-CLASS"
	     "EFFECTIVE-SLOT-DEFINITION"
	     "EFFECTIVE-SLOT-DEFINITION-CLASS"
	     "ENSURE-CLASS"
	     "ENSURE-CLASS-USING-CLASS"
	     "ENSURE-GENERIC-FUNCTION"
	     "ENSURE-GENERIC-FUNCTION-USING-CLASS"
	     "EQL-SPECIALIZER"
	     "EQL-SPECIALIZER-OBJECT"
	     "EXTRACT-LAMBDA-LIST"
	     "EXTRACT-SPECIALIZER-NAMES"
	     "FINALIZE-INHERITANCE"
	     "FIND-METHOD-COMBINATION"
	     "FORWARD-REFERENCED-CLASS"
	     "FUNCALLABLE-STANDARD-CLASS"
	     "FUNCALLABLE-STANDARD-INSTANCE-ACCESS"
	     "FUNCALLABLE-STANDARD-OBJECT"
	     "FUNCTION"
	     "GENERIC-FUNCTION"
	     "GENERIC-FUNCTION-ARGUMENT-PRECEDENCE-ORDER"
	     "GENERIC-FUNCTION-DECLARATIONS"
	     "GENERIC-FUNCTION-LAMBDA-LIST"
	     "GENERIC-FUNCTION-METHOD-CLASS"
	     "GENERIC-FUNCTION-METHOD-COMBINATION"
	     "GENERIC-FUNCTION-METHODS"
	     "GENERIC-FUNCTION-NAME"
	     "INTERN-EQL-SPECIALIZER"
	     "MAKE-INSTANCE"
	     "MAKE-METHOD-LAMBDA"
	     "MAP-DEPENDENTS"
	     "METAOBJECT"
	     "METHOD"
	     "METHOD-COMBINATION"
	     "METHOD-FUNCTION"
	     "METHOD-GENERIC-FUNCTION"
	     "METHOD-LAMBDA-LIST"
	     "METHOD-QUALIFIERS"
	     "METHOD-SPECIALIZERS"
	     "READER-METHOD-CLASS"
	     "REMOVE-DEPENDENT"
	     "REMOVE-DIRECT-METHOD"
	     "REMOVE-DIRECT-SUBCLASS"
	     "REMOVE-METHOD"
	     "SET-FUNCALLABLE-INSTANCE-FUNCTION"
	     "SLOT-BOUNDP-USING-CLASS"
	     "SLOT-DEFINITION"
	     "SLOT-DEFINITION-ALLOCATION"
	     "SLOT-DEFINITION-INITARGS"
	     "SLOT-DEFINITION-INITFORM"
	     "SLOT-DEFINITION-INITFUNCTION"
	     "SLOT-DEFINITION-LOCATION"
	     "SLOT-DEFINITION-NAME"
	     "SLOT-DEFINITION-READERS"
	     "SLOT-DEFINITION-TYPE"
	     "SLOT-DEFINITION-WRITERS"
	     "SLOT-MAKUNBOUND-USING-CLASS"
	     "SLOT-VALUE-USING-CLASS"
	     "SPECIALIZER"
	     "SPECIALIZER-DIRECT-GENERIC-FUNCTIONS"
	     "SPECIALIZER-DIRECT-METHODS"
	     "STANDARD-ACCESSOR-METHOD"
	     "STANDARD-CLASS"
	     "STANDARD-DIRECT-SLOT-DEFINITION"
	     "STANDARD-EFFECTIVE-SLOT-DEFINITION"
	     "STANDARD-GENERIC-FUNCTION"
	     "STANDARD-INSTANCE-ACCESS"
	     "STANDARD-METHOD"
	     "STANDARD-OBJECT"
	     "STANDARD-READER-METHOD"
	     "STANDARD-SLOT-DEFINITION"
	     "STANDARD-WRITER-METHOD"
	     "UPDATE-DEPENDENT"
	     "VALIDATE-SUPERCLASS"
	     "WRITER-METHOD-CLASS"))

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
