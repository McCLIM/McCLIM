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

;;;
;;; KRA: This needs more documentation
;;;

(eval-when (compile load eval)
  (export '(duplicate-set
	    duplicate-slots
	    duplicator-methods
	    )
	  'tool
	  ))

(defun duplicate-class-forms-copy (SYMBOL CLASS-NAME SLOT-FORMS)
  (let ((WITH-SLOTS-SLOTS nil))
    (let ((SLOTS-FORMS (loop for (FORM-KIND FIRST-FORM . RST) in SLOT-FORMS
			     collecting (case FORM-KIND
					  (duplicate-set
					    (push FIRST-FORM WITH-SLOTS-SLOTS)
					    `(copy-set-slot ,FIRST-FORM ,@RST))
					  (duplicate-slots
					    `(copy-slots ,FIRST-FORM ,@RST))
					  (otherwise
					    (error "Unknown DUPLICATE Form-kind: ~S"
						   FORM-KIND))))))
      `(defmethod copy-inner-class progn ((,SYMBOL ,CLASS-NAME)
					  ..COPY-OBJECT.. ..COPY-HTABLE..)
	 (,@(if WITH-SLOTS-SLOTS
		`(with-slots ,WITH-SLOTS-SLOTS ,symbol)
	       '(progn))
	  (with-slot-copying (..COPY-OBJECT.. ..COPY-HTABLE..)
	    ,@SLOTS-FORMS))))))

(defun duplicate-class-forms-dump (SYMBOL CLASS-NAME SLOT-FORMS)
  (let ((WITH-SLOTS-SLOTS nil))
    (let ((SLOTS-FORMS (loop for (FORM-KIND FIRST-FORM . RST) in SLOT-FORMS
			     collecting (case FORM-KIND
					  (duplicate-set
					    (push FIRST-FORM WITH-SLOTS-SLOTS)
					    `(dump-set-slot ,FIRST-FORM ,@RST))
					  (duplicate-slots					       
					    `(dump-slots ,FIRST-FORM ,@RST))
					  (otherwise
					    (error "Unknown DUPLICATE Form-kind: ~S" FORM-KIND))))))
      `(defmethod dump-forms append ((,SYMBOL ,CLASS-NAME))
	 (,@(if WITH-SLOTS-SLOTS
		`(with-slots ,WITH-SLOTS-SLOTS ,SYMBOL)
	       '(progn))
	  (with-slot-dumping ()
	    ,@SLOTS-FORMS))))))

(defun duplicate-class-forms-final-duplicate
       (SYMBOL CLASS-NAME FINAL-DUPLICATE-FORMS COPY-FORMS? DUMP-FORMS?)
  (when FINAL-DUPLICATE-FORMS
    (list `(defmethod final-duplicate-class progn ((,SYMBOL ,CLASS-NAME))
	     ,@FINAL-DUPLICATE-FORMS)
	  (and COPY-FORMS?
	       `(defmethod copy-final-class progn ((,SYMBOL ,CLASS-NAME))
		  (final-duplicate-class ,SYMBOL)))
	  (and DUMP-FORMS?
	       `(defmethod final-dump progn ((,SYMBOL ,CLASS-NAME))
		  (final-duplicate-class ,SYMBOL))))))

;;;
;;; 
;;;
(defvar *DUPLICATE-CLASS-FORMS-COPY-FORMS?* t)
(defvar *DUPLICATE-CLASS-FORMS-DUMP-FORMS?* t)
(defmacro duplicator-methods
	  ((CLASS-NAME
	     &key
	     (symbol 'self)
	     (COPY-FORMS? *DUPLICATE-CLASS-FORMS-COPY-FORMS?*)
	     (DUMP-FORMS? *DUPLICATE-CLASS-FORMS-DUMP-FORMS?*))
	   SLOT-FORMS &optional FINAL-DUPLICATE-FORMS)
  #+lispm
  (declare (zwei:indentation 2 1))
  `(progn
     ,(and COPY-FORMS?
	   (duplicate-class-forms-copy symbol CLASS-NAME SLOT-FORMS))
     ,(and DUMP-FORMS?
	   (duplicate-class-forms-dump symbol CLASS-NAME SLOT-FORMS))
     ,@(duplicate-class-forms-final-duplicate
	 symbol CLASS-NAME FINAL-DUPLICATE-FORMS COPY-FORMS? DUMP-FORMS?)))


