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

(in-package :cl-user)

(export 'ccl::stream-finish-output :ccl)

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
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (cl:defclass ,name ,@args))))

	 
  
#-openmcl-partial-mop
(in-package :ccl)
#-openmcl-partial-mop
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
		  (change-class class metaclass)) ; (s)he asked for it.
		(setf (find-class class-name) class)
		(labels ((obsolete (class)
						   (dolist (sub (%class-subclasses class)) (obsolete sub))
										;Need to save old class info in wrapper for obsolete instance access...
						   (setf (%class-cpl class) nil)
						   (make-instances-obsolete class)))
		  (without-interrupts
		   (obsolete class)
		   (dolist (sup (%class-local-supers class))
			 (if (typep sup 'class)	; might be a symbol from earlier forward ref
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
		class)))

;;; Fake compute-applicable-methods-using-classes, for the Show
;;; Applicable Methods command in the listener.

#+openmcl-partial-mop
(in-package :ccl)
#+openmcl-partial-mop
(progn
  (defgeneric compute-applicable-methods-using-classes (gf args))
  (defmethod compute-applicable-methods-using-classes
      ((gf standard-generic-function) args)
    (let* ((methods (%gf-methods gf))
	   (args-length (length args))
	   (bits (inner-lfun-bits gf))
	   arg-count res)
      (when methods
	(setq arg-count (length (%method-specializers (car methods))))
	(unless (<= arg-count args-length)
	  (error "Too few args to ~s" gf))
	(unless (or (logbitp $lfbits-rest-bit bits)
		    (logbitp $lfbits-restv-bit bits)
		    (logbitp $lfbits-keys-bit bits)
		    (<= args-length 
			(+ (ldb $lfbits-numreq bits) (ldb $lfbits-numopt bits))))
	  (error "Too many args to ~s" gf))
	(let ((cpls (make-list arg-count)))
	  (declare (dynamic-extent cpls))
	  (do* ((args-tail args (cdr args-tail))
		(cpls-tail cpls (cdr cpls-tail)))
	       ((null cpls-tail))
	    (setf (car cpls-tail)
		  (%class-precedence-list (car args-tail))))
	  (dolist (m methods)
	    (when (find-if #'(lambda (spec)
			       (typep spec 'eql-specializer))
			   (%method-specializers m))
	      (return-from compute-applicable-methods-using-classes
		(values nil nil)))
	    (if (%method-applicable-p m args cpls)
		(push m res)))
	  (values (sort-methods res cpls (%gf-precedence-list gf)) t))))))
