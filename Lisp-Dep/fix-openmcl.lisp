;;; -*- Mode: Lisp; Package: User -*-

;;;  (c) copyright 2002 by John Wiseman (jjwiseman@yahoo.com)
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

(export 'ccl::stream-finish-output :ccl)

(defpackage :clim-mop
  (:use :common-lisp)
  (:import-from :ccl #:class-prototype #:class-precedence-list
		#:class-direct-superclasses #:generic-function-methods
		#:method-specializers #:compute-applicable-methods
		#:funcallable-standard-class
		#:class-name
		
		#:class-direct-subclasses)
  (:export #:validate-superclass #:class-finalized-p
	   #:finalize-inheritance ccl::class-prototype
	   ccl::class-precedence-list ccl::class-direct-superclasses
	   ccl::generic-function-methods ccl::method-specializers
	   ccl::compute-applicable-methods
	   ccl::funcallable-standard-class
	   #:slot-definition-name
	   #:eql-specializer
	   #:eql-specializer-object
	   #:intern-eql-specializer
	   ccl::ensure-class
	   #:compute-applicable-methods-using-classes
	   #:extract-specializer-names #:extract-lambda-list
	   #:class-slots
	   ccl::class-name
	   #:class-direct-slots
	   #:slot-definition-type
	   #:slot-definition-allocation
	   #:slot-definition-initargs
	   #:slot-definition-initfunction
	   #:slot-definition-initform
	   #:slot-definition-readers
	   #:slot-definition-writers
	   #:generic-function-name
	   #:class-direct-subclasses))


(in-package :clim-mop)


(defmethod validate-superclass (a b)
  T)

(defmethod class-finalized-p (c)
  T)

(defmethod finalize-inheritance (c)
  (values))

;; MCL's eql-specializer-objects are just lists whose car is eql.

(defun eql-specializer-p (spec)
  (and (consp spec) (eq (car spec) 'eql)))

(deftype eql-specializer () '(satisfies eql-specializer-p))

(defun eql-specializer-object (spec)
  (cadr spec))

;;; Pretty bogus, but should suit our purposes, whatever they are.

(defparameter *eql-specializer-hash* (make-hash-table))

(defun intern-eql-specializer (object)
  (let ((eql-object (gethash object *eql-specializer-hash* nil)))
    (unless eql-object
      (setq eql-object (cons 'eql object))
      (setf (gethash object *eql-specializer-hash*) eql-object))
    eql-object))

(defun ensure-class (name &rest all-keys &key name metaclass direct-superclasses
			  &allow-other-keys)
  (let ((metaclass-options (copy-list all-keys)))
    (remf metaclass-options :name)
    (remf metaclass-options :metaclass)
    (remf metaclass-options :direct-superclasses)
    (ccl::%defclass name direct-superclasses '() '() nil '()
		    metaclass :metaclass-opts metaclass-options)))

(defun extract-specializer-names (lambda-list)
  (loop for var in lambda-list
	until (member var lambda-list-keywords :test #'eq)
	collect (if (consp var)
		    (cadr var)
		    t)))

(defun extract-lambda-list (lambda-list)
  (loop for tail on lambda-list
	for (var) = tail
	until (member var lambda-list-keywords :test #'eq)
	collect (if (consp var)
		    (car var)
		    var)
	into required
	finally (return (nconc required tail))))

;;; Fake slot definition metaobject protocol, for describe and
;;; listener

(defclass slot-definition ()
  ((slot-class :reader slot-class :initarg :slot-class)
   (allocation :reader slot-definition-allocation :initarg :allocation)
   (ccl-slot :reader ccl-slot :initarg :ccl-slot)))

(defclass direct-slot-definition (slot-definition)
  ())

(defclass effective-slot-definition (slot-definition)
  ())

(defmethod class-slots ((class standard-class))
  (nconc (mapcar #'(lambda (slot)
		     (make-instance 'effective-slot-definition
				    :slot-class class
				    :allocation :instance
				    :ccl-slot slot))
		 (ccl::class-instance-slots class))
	 (mapcar #'(lambda (slot)
		     (make-instance 'effective-slot-definition
				    :slot-class class
				    :allocation :class
				    :ccl-slot slot))
		 (ccl::class-class-slots class))))

(defmethod class-slots ((class standard-class))
  (nconc (mapcar #'(lambda (slot)
		     (make-instance 'direct-slot-definition
				    :slot-class class
				    :allocation :instance
				    :ccl-slot slot))
		 (ccl::class-direct-instance-slots class))
	 (mapcar #'(lambda (slot)
		     (make-instance 'direct-slot-definition
				    :slot-class class
				    :allocation :class
				    :ccl-slot slot))
		 (ccl::class-direct-class-slots class))))

(defmethod slot-definition-name ((slot-definition slot-definition))
  (ccl:slot-definition-name (ccl-slot slot-definition)))
  
(defmethod slot-definition-type ((slot-definition slot-definition))
  (ccl:slot-definition-type (ccl-slot slot-definition)))

(defmethod slot-definition-initargs ((slot-definition slot-definition))
  (ccl:slot-definition-initargs (ccl-slot slot-definition)))

(defmethod slot-definition-initfunction ((slot-definition slot-definition))
  (ccl:slot-definition-initfunction (ccl-slot slot-definition)))

(defmethod slot-definition-initform ((slot-definition slot-definition))
  (ccl:slot-definition-initform (ccl-slot slot-definition)))

(defmethod slot-definition-readers ((slot direct-slot-definition))
  (ccl:slot-readers (slot-class slot) (slot-definition-name slot)))

(defmethod slot-definition-writers ((slot direct-slot-definition))
  (ccl:slot-writers (slot-class slot) (slot-definition-name slot)))

(defmethod generic-function-name (gf)
  (ccl::function-name gf))

(defpackage :clim-lisp-patch
  (:use)
  (:export #:defclass))

(defvar clim-lisp-patch::*compile-time-clos-names* (make-hash-table))
 
(defun clim-lisp-patch::compile-time-clos-class-p (name)
  (gethash name clim-lisp-patch::*compile-time-clos-names* nil))

(defmacro clim-lisp-patch:defclass (name &rest args)
  `(progn
     (eval-when (:compile-toplevel)
       (setf (gethash ',name clim-lisp-patch::*compile-time-clos-names*) t))
     (cl:defclass ,name ,@args)))
  
(in-package :ccl)

(let ((*warn-if-redefine-kernel* NIL))
(defun %defclass (class-name superclasses instance-slotds class-slotds doc
                  default-initargs metaclass
                  &key primary-p metaclass-opts)
  (if (null superclasses)
    (setq superclasses (list 'standard-object))
    (setq superclasses (copy-list superclasses)))
  (if (null metaclass)
    (setq metaclass *standard-class-class*))
  (if (symbolp metaclass) (setq metaclass (find-class metaclass)))
  (unless (subclassp metaclass *std-class-class*)
    (error "~s is not a subclass of ~s" metaclass *std-class-class*))
  (let* ((old-class (find-class class-name nil))
         (class (or old-class
                    (let* ((c (if (or (eq metaclass *standard-class-class*)
                                      (eq metaclass *funcallable-standard-class-class*))
                                (%cons-standard-class class-name (%class-own-wrapper metaclass))
                                (apply #'make-instance metaclass :name class-name metaclass-opts))))
                        (setf (%class-ctype c) (make-class-ctype c))
                        c))))
    (when (eq class *standard-object-class*)
      (error "Cannot redefine ~S" class))
    (unless (eq (class-of class) metaclass)
      (cerror (format nil "(~s '~s '~s)" 'change-class class metaclass)
              "~s is not an instance of ~s" class metaclass)
      (change-class class metaclass))   ; (s)he asked for it.
    (setf (find-class class-name) class)
    (labels ((obsolete (class)
               (dolist (sub (%class-subclasses class)) (obsolete sub))
               ;Need to save old class info in wrapper for obsolete instance access...
               (setf (%class-cpl class) nil)
               (make-instances-obsolete class)))
      (without-interrupts
       (obsolete class)
       (dolist (sup (%class-local-supers class))
         (if (typep sup 'class)          ; might be a symbol from earlier forward ref
           (setf (%class-subclasses sup) (nremove class (%class-subclasses sup)))))
       (setf (%class-local-supers class) superclasses)
       (setf (%class-local-instance-slotds class) instance-slotds)
       (setf (%old-class-local-shared-slotds class)
             (%class-local-shared-slotds class))
       (setf (%class-local-shared-slotds class) class-slotds)
       (setf (%class-local-default-initargs class) default-initargs)))
    (setf (%class-primary-p class) primary-p)
    (when doc (setf (documentation class 'type) doc))
    (record-source-file class-name 'class)
    (initialize-class class t)
    class))


)


