;;; -*- Mode: Lisp; Package: COMMON-LISP -*-

;;;  (c) copyright 2002 by Michael McDonald (mikemac@mikemac.com)

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

(in-package :COMMON-LISP)

#+excl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (excl:package-definition-lock (find-package :common-lisp)) nil))

#+openmcl
(defmacro with-system-redefinition-allowed (&body body)
  `(let ((ccl::*warn-if-redefine-kernel* nil))
     ,@body))

#-openmcl
(defmacro with-system-redefinition-allowed (&body body)
  `(progn
     ,@body))

(with-system-redefinition-allowed
  (defun describe (thing &optional stream)
    (if (null stream)
	(setq stream *standard-output*)
	(if  (eq stream t)
	     (setq stream *terminal-io*)))
    (describe-object thing stream)
    (values))
)

(defgeneric describe-object (thing stream))

;;; For these methods, stream should be of type
;;; (or EXTENDED-OUTPUT-STREAM OUTPUT-RECORDING-STREAM)
;;; but CLIM-STREAM-PANE is used instead.

(with-system-redefinition-allowed
  (defmethod describe-object ((thing t) stream)
    (let ((*print-array* nil))
      (clim:present thing (clim:presentation-type-of thing)
		    :acceptably t :stream stream)
      (format stream " is of type ")
      (clim:present (type-of thing) (clim:presentation-type-of (type-of thing))
		    :stream stream)
      (terpri stream)))
)

(defmethod describe-object ((thing symbol) stream)
  (clim:present thing (clim:presentation-type-of thing)
		:stream stream)
  (format stream " is of type ")
  (clim:present (type-of thing) (clim:presentation-type-of (type-of thing))
		:stream stream)
  (terpri stream)
  (cond
   ((not (boundp thing))
    (format stream "   it is unbound~%"))
   (t
    (format stream "   it has a value of ")
    (clim:present (symbol-value thing) (clim:presentation-type-of (symbol-value thing))
		  :stream stream)
    (terpri)))
  (format stream "   it is in the ")
  (clim:present (symbol-package thing) (clim:presentation-type-of (symbol-package thing))
		:stream stream)
  (format stream " package~%")
  (when (fboundp thing)
    (format stream "   it has a function definition of ~S~%" (symbol-function thing))
    (format stream "      which has the argument list ")
    #+excl (clim:present (excl:arglist (symbol-function thing))
			 (clim:presentation-type-of (excl:arglist (symbol-function thing)))
			 :stream stream)
    #+cmu (clim:present (kernel:%function-arglist (symbol-function thing))
			  (clim:presentation-type-of (kernel:%function-arglist (symbol-function thing)))
			  :stream stream)
    (terpri))
  (format stream "   it has a property list of ~S~%" (symbol-plist thing)))

(defmethod describe-object ((thing number) stream)
  (clim:present thing (clim:presentation-type-of thing)
		:stream stream)
  (format stream " is a number of type ")
  (clim:present (type-of thing) (clim:presentation-type-of (type-of thing))
		:stream stream)
  (terpri stream))

(defmethod describe-object ((thing string) stream)
  (clim:present thing (clim:presentation-type-of thing)
		:stream stream)
  (format stream " is of type ")
  (clim:present (type-of thing) (clim:presentation-type-of (type-of thing))
		:stream stream)
  (format stream " with a length of ")
  (clim:present (length thing) 'clim:integer
		:stream stream)
  (terpri stream))

(defmethod describe-object ((thing package) stream)
  (clim:present thing (clim:presentation-type-of thing)
		:stream stream)
  (format stream " is a package named ")
  (clim:present (package-name thing) (clim:presentation-type-of (package-name thing))
		:stream stream)
  (terpri stream)
  (format stream "   it has the nicknames of ")
  (clim:present (package-nicknames thing) 'clim:expression
		:stream stream)
  (terpri)
  (format stream "   it uses these packages: ")
  (clim:present (package-use-list thing) 'clim:expression
		:stream stream)
  (terpri)
  (format stream "   it is used by the packages: ")
  (clim:present (package-used-by-list thing) 'clim:expression
		:stream stream)
  (terpri))

(defmethod describe-object ((thing structure-object) stream)
  (clim:present thing (clim:presentation-type-of thing)
		:stream stream)
  (format stream " is a structure of type ")
  (clim:present (type-of thing) (clim:presentation-type-of (type-of thing))
		:stream stream)
  (terpri stream)
  (format stream "   it has the following slots:~%")
  (let* ((slots (clim-mop:class-slots (class-of thing)))
	 (width (loop for slot in slots
		      maximizing (length (symbol-name (clim-mop:slot-definition-name slot))))))
    (loop for slot in slots
	  do (cond
	      ((slot-boundp thing (clim-mop:slot-definition-name slot))
	       (format stream "      ~v@A: " width
		       (clim-mop:slot-definition-name slot))
	       (clim:present (slot-value thing (clim-mop:slot-definition-name slot))
			     'clim:expression
			     :stream stream)
	       (terpri stream))
	      (t
	       (format stream "      ~v@A: <unbound>~%" width
		       (clim-mop:slot-definition-name slot)))))))

(defmethod describe-object ((thing standard-object) stream)
  (clim:present thing (clim:presentation-type-of thing)
		:stream stream)
  (format stream " is an instance of type ")
  (clim:present (type-of thing) (clim:presentation-type-of (type-of thing))
		:stream stream)
  (terpri stream)
  (format stream "   it has the following slots:~%")
  (let* ((slots (clim-mop:class-slots (class-of thing)))
	 (width (loop for slot in slots
		      maximizing (length (symbol-name (clim-mop:slot-definition-name slot))))))
    (loop for slot in slots
	  do (cond
	      ((slot-boundp thing (clim-mop:slot-definition-name slot))
	       (format stream "      ~v@A: " width
		       (clim-mop:slot-definition-name slot))
	       (clim:present (slot-value thing (clim-mop:slot-definition-name slot))
			     'clim:expression
			     :stream stream)
	       (terpri stream))
	      (t
	       (format stream "      ~v@A: <unbound>~%" width
		       (clim-mop:slot-definition-name slot)))))))

#+excl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (excl:package-definition-lock (find-package :common-lisp)) t))

