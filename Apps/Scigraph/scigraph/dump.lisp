;;; -*- Syntax: Common-lisp; Package: TOOL -*-
#|
Copyright (c) 1987-1993 by BBN Systems and Technologies,
A Division of Bolt, Beranek and Newman Inc.
All rights reserved.

Permission to use, copy, modify and distribute this software and its
documentation is hereby granted without fee, provided that the above
copyright notice of BBN Systems and Technologies, this paragraph and the
one following appear in all copies and in supporting documentation, and
that the name Bolt Beranek and Newman Inc. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission. Any distribution of this software or derivative
works must comply with all applicable United States export control laws.

BBN makes no representation about the suitability of this software for any
purposes.  It is provided "AS IS", without express or implied warranties
including (but not limited to) all implied warranties of merchantability
and fitness for a particular purpose, and notwithstanding any other
provision contained herein.  In no event shall BBN be liable for any
special, indirect or consequential damages whatsoever resulting from loss
of use, data or profits, whether in an action of contract, negligence or
other tortuous action, arising out of or in connection with the use or
performance of this software, even if BBN Systems and Technologies is
advised of the possiblity of such damages.
|#

(in-package :tool)

(eval-when (compile load eval)
  (export `(dump-forms with-slot-dumping dump-set-slot dump-slot
		       dump-slots final-dump dumpable-mixin)
	  'tool))

(defclass dumpable-mixin
	  ()
    ()
  (:documentation
    "Provides method for doing DUMP of an object.
     Each mixin should provide an DUMP-FORMS and FINAL-DUMP-CLASS method 
     to copy its slots appropriately."))

(defgeneric final-dump (SELF)
  (:method-combination progn))

(defmethod final-dump progn ((SELF dumpable-mixin))
  self)

(defgeneric dump-forms (SELF)
  (:method-combination append)
  (:documentation "Defined for each component class of an object with mixin DUPLABLE-MIXIN.
It should setup its slots as appropriate."))

(defmethod dump-forms append ((SELF dumpable-mixin))
  nil)

(defmethod make-load-form ((SELF dumpable-mixin) &optional env)
  (declare (ignore env))
  (values `(make-instance ',(class-name (class-of SELF)))
	  `(progn
	     ,@(dump-forms SELF)
	     (final-dump ,SELF))))

;;; KRA: These could go into the macrolet below.
;;; KRA: It would be better if the form prouced by dump-forms was very compact
;;; KRA: for example, it wouldn't mention the object for each slot or
;;; KRA: the slot names each time an object is dumped.
;;; KRA: There are 2 reasons to be compact, consing while dumping, and 
;;; KRA: size of binary file.
;;; KRA: So, dump-forms could produce something like:
;;; KRA: (dump-load object (list slot-value-1 slot-value-2 ...))
;;; Things to make using DUMP-FORMS easier.
(defmacro dump-set-slot-1 (DUMP-OBJECT SLOT-NAME VALUE)
  ``(setf (slot-value ,,DUMP-OBJECT ',',SLOT-NAME) ',,VALUE))

(defmacro dump-slot-1  (DUMP-OBJECT SLOT-NAME)
  `(dump-set-slot-1 ,DUMP-OBJECT ,SLOT-NAME (slot-value ,DUMP-OBJECT ',SLOT-NAME)))

(defmacro with-slot-dumping ((&optional (DUMP-OBJECT 'SELF)) &body BODY)
  `(let ((..FORMS-LIST.. nil))
     (macrolet ((dump-set-slot (SLOT-NAME VALUE)
		  `(push (dump-set-slot-1 ,',DUMP-OBJECT ,SLOT-NAME ,`,VALUE)
			 ..FORMS-LIST..))
		(dump-slot (SLOT-NAME)
		  `(push (dump-slot-1 ,',DUMP-OBJECT ,SLOT-NAME)
			 ..FORMS-LIST..)))
       (macrolet ((dump-slots (&rest SLOT-NAMES)
		    `(progn
		       ,@(loop for SLOT-NAME in SLOT-NAMES
			       collecting `(dump-slot ,SLOT-NAME)))))
	 ,@BODY))
     (reverse ..FORMS-LIST..)))

;;; JPM.  Until the vendors catch up with the spec, implement here
;;; some of the dumpers we need for common lisp things.


