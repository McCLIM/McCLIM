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
  (export 'with-stack-list-copy 'tool)
  (export '(with-slot-copying copy-slot copy-set-slot copy-slots) 'tool)
  (export '(copyable-mixin copy-inner-class) 'tool)
  (export '(copy-self copy-inner) 'tool)
  (export '(copy-top-level copy) 'tool))

;;; COPY-TOP-LEVEL:
;;; Copy objects with aribtrarily complex substructure.
;;; Objects are kept track of in a HashTable, so only make one copy of each.
;;; Things which are EQ in the original (i.e. objects, sublists, etc.) come out
;;; EQ in the corresponding places in the copy.

(let ((copy-htable (make-hash-table)))
  (defmethod copy-top-level (ORIGINAL-THING)
    (clrhash COPY-HTABLE)
    (copy ORIGINAL-THING COPY-HTABLE)))

(defgeneric copy (SELF COPY-HTABLE)
  (:documentation "Returns a fullfledged copy of SELF, set-up and ready to go."))

;;;********************************************************************************
;;; Some simple cases.
;;; Copies these objects are always eq to the original and have no 
;;; internal structure.  -->  So just use the objects use themselves.
;;; (I.e. no need to worry about caching them).
;;;**********************************************************************NLC21NOV90
(defmethod copy ((ORINGINAL-SYMBOL symbol) COPY-HTABLE)
  (declare (ignore COPY-HTABLE))
  ORINGINAL-SYMBOL)

(defmethod copy ((ORINGINAL-NUMBER number) COPY-HTABLE)
  (declare (ignore COPY-HTABLE))
  ORINGINAL-NUMBER)

;;;********************************************************************************
;;; The hairier, default case. 
;;; In general,
;;;  1] Objects can have internal structure (and, for instance, circular 
;;;     refrences) and/or
;;;  2] Copies are not eql to the original.
;;;
;;; The basic idea here is to only make one copy of the ORIGINAL-THING and 
;;; store it in the HashTable for future use.  In this way, the Copied 
;;; Object has the same "eq-connectedness" that the original had.
;;;**********************************************************************NLC21NOV90

(defgeneric copy-self (SELF)
  (:documentation "Return a new, empty version of SELF"))

(defgeneric copy-inner (SELF COPY-OBJECT COPY-HTABLE)
  (:documentation
    "Copy the relevant portions of SELF into COPY-OBJECT.
     OK if it calls COPY on sub-objects."))

(defgeneric copy-final (SELF)
  (:documentation "Last pass to make sure everything is in place."))

;;;NLC21NOV90 - The call to COPY-INNER has to be inside this guy, else it
;;; will loose if ORIGINAL-THING contains a pointer to itself.
;;;
;;; So, in short there are three steps (if I've not already been Copied):
;;;  1] Create a new, empty copy  (using COPY-SELF).
;;;  2] Shove it in the HashTable.
;;;  3] Setup its internal structure, as needed (using COPY-INNER).
(defmethod copy (ORIGINAL-THING COPY-HTABLE)
  (multiple-value-bind (VALUE FOUND?) (gethash ORIGINAL-THING COPY-HTABLE)
    (or (and FOUND? VALUE)
	(let ((COPY-THING (copy-self ORIGINAL-THING)))
	  (setf (gethash ORIGINAL-THING COPY-HTABLE) COPY-THING)
	  (copy-inner ORIGINAL-THING COPY-THING COPY-HTABLE)
	  (copy-final ORIGINAL-THING)
	  COPY-THING))))

(defmethod copy-self (SELF)
  (error "Don't know how to copy ~A" self))

(defmethod copy-self ((ORIGINAL string))
  (subseq ORIGINAL 0 (length ORIGINAL)))

(defmethod copy-self ((original array))
  (subseq original 0 (length original)))

(defmethod copy-inner (SELF COPY-OBJECT COPY-HTABLE)
  "Default is to do nothing."
  (declare (ignore SELF COPY-OBJECT COPY-HTABLE))
  nil)

(defmethod copy-final ((self t))
  "Default is to do nothing."
  nil)

;;;********************************************************************************
;;; Lists
;;;**********************************************************************NLC14DEC90
;;; The Old, Boring, Common-Lisp compatible Way.
#-lispm
(defmethod copy-self ((ORIGINAL-LIST list))
  (and ORIGINAL-LIST (cons nil nil)))

(defmethod copy-inner ((ORIGINAL-LIST list) COPY-LIST COPY-HTABLE)
  ;; This handles circular lists, but is slower and isn't cdr coded.
  (unless (null ORIGINAL-LIST)
    (setf (car COPY-LIST) (copy (car ORIGINAL-LIST) COPY-HTABLE))
    (setf (cdr COPY-LIST) (copy (cdr ORIGINAL-LIST) COPY-HTABLE))))

;;;NLC15DEC90 - New, improved copy on lists.
;;; This isn't as "elegant" as the above, but it preserves Cdr-coding.
#+lispm
(defmethod copy ((ORIGINAL-LIST list) COPY-HTABLE)
  (and ORIGINAL-LIST
       (multiple-value-bind (VALUE FOUND?) (gethash ORIGINAL-LIST COPY-HTABLE)
	 (if FOUND?
	     VALUE
	    (let (COPY-LIST)
	      (multiple-value-bind (NUM-CDR-NEXT LAST-CONTIG-PART)
		  (si:contiguous-list-info ORIGINAL-LIST)
		(if (eq LAST-CONTIG-PART ORIGINAL-LIST)
		    ;;; THIS CONS (AT LEAST) ISN'T CDR-CODED.
		    ;;; SO MAKE A COPY OF THIS CONS, AND CACHE IT
		    ;;; THEN COPY BOTH SIDES OF THE ITS SUB-TREE.
		    (setf COPY-LIST (cons nil nil)
			  (gethash ORIGINAL-LIST COPY-HTABLE) COPY-LIST
			  (car COPY-LIST) (copy (car ORIGINAL-LIST) COPY-HTABLE)
			  (cdr COPY-LIST) (copy (cdr ORIGINAL-LIST) COPY-HTABLE))

		   ;;; ELSE, THE LIST STARTS WITH (AT LEAST SOME) CDR-CODING.
		   ;;; SO GET A CDR-CODED COPY OF THE SAME LENGTH
		   ;;; AND CDR DOWN IT, COPYING EACH ELEMENT.
		   ;;; NOTE THAT WE NEED TO CACHE EACH CONS.
		   (prog ((COPY-PNTR (setq COPY-LIST (make-list (1+ NUM-CDR-NEXT)))))
		      LOOP-TAG
			 (setf (gethash ORIGINAL-LIST COPY-HTABLE) COPY-PNTR
			       (car COPY-PNTR) (copy (car ORIGINAL-LIST) COPY-HTABLE))
			 (unless (eq LAST-CONTIG-PART ORIGINAL-LIST)
			   (setq COPY-PNTR (cdr COPY-PNTR)
				 ORIGINAL-LIST (cdr ORIGINAL-LIST))
			   (go LOOP-TAG))

			 ;;; FINALLY, MAKE SURE THE LAST CDR IS HANDLED RIGHT.
			 (and (cdr ORIGINAL-LIST)
			      (setf (cdr COPY-PNTR)
				    (copy (cdr ORIGINAL-LIST) COPY-HTABLE))))))
	      COPY-LIST))))
  )

;;;********************************************************************************
;;; Copy-able Class objects.
;;;**********************************************************************NLC21NOV90

(defclass copyable-mixin
	  ()
    ()
  (:documentation
    "Provides method for doing COPY that creates a copy on an object.
     Each mixin should provide an COPY-INNER-CLASS method to copy its
     slots appropriately."))

(defmethod copy-self ((SELF copyable-mixin))
  (make-instance (class-of SELF)))

(defgeneric copy-inner-class (SELF COPY-OBJECT COPY-HTABLE)
  (:method-combination progn)
  (:documentation
    "Defined for each component class of an object with mixin COPYABLE-MIXIN.
     It should setup its slots as appropriate.
     This needs to be a seperate method (from COPY-INNER) because it has
     to be done with a PROGN Method-Combination."))

(defmethod copy-inner-class progn ((ORIGINAL-OBJECT copyable-mixin) COPY-LIST COPY-HTABLE)
  (declare (ignore COPY-LIST COPY-HTABLE))
  nil)

(defmethod copy-inner ((ORIGINAL-OBJECT copyable-mixin) COPY-LIST COPY-HTABLE)
  (copy-inner-class ORIGINAL-OBJECT COPY-LIST COPY-HTABLE))

(defgeneric copy-final-class (SELF)
  (:method-combination progn)
  (:documentation
    "Defined for each component class of an object with mixin COPYABLE-MIXIN.
     It should setup its slots as appropriate.
     This needs to be a seperate method (from COPY-FINAL) because it has
     to be done with a PROGN Method-Combination."))

(defmethod copy-final-class progn ((ORIGINAL-OBJECT copyable-mixin))
  nil)

(defmethod copy-final ((ORIGINAL-OBJECT copyable-mixin))
  (copy-final-class ORIGINAL-OBJECT))

;;;********************************************************************************
;;; Things to make using COPY-INNER-CLASS easier.
;;;**********************************************************************NLC06DEC90

;;; KRA: these 2 macros should be macrolet inside with-slot-copying. 
(defmacro copy-set-slot-1 (COPY-OBJECT SLOT-NAME VALUE)
  `(setf (slot-value ,COPY-OBJECT ,SLOT-NAME)
	 ,VALUE))

(defmacro copy-slot-1 (COPY-OBJECT SLOT-NAME ORIGINAL-OBJECT COPY-HTABLE)
  `(copy-set-slot-1 ,COPY-OBJECT ,SLOT-NAME (copy (slot-value ,ORIGINAL-OBJECT ,SLOT-NAME)
						  ,COPY-HTABLE)))

;;; (copy-set-slot (SLOT-NAME VALUE)
;;;   Set the contents of SLOT-NAME in COPY-OBJECT to VALUE.
;;; (copy-slot (SLOT-NAME) ...
;;;   Set the contents of SLOT-NAME in COPY-OBJECT to be a copyicate of the
;;;   contents of the same slot in ORIGINAL-OBJECT.
(defmacro with-slot-copying
	  ((COPY-OBJECT COPY-HTABLE &optional (ORIGINAL-OBJECT 'SELF)) &body BODY)
  `(macrolet ((copy-slot (SLOT-NAME)
		`(copy-slot-1 ,',COPY-OBJECT ',SLOT-NAME ,',ORIGINAL-OBJECT ,',COPY-HTABLE))
	      (copy-set-slot (SLOT-NAME VALUE)
		`(copy-set-slot-1 ,',COPY-OBJECT ',SLOT-NAME ,VALUE)))
     (macrolet ((copy-slots (&rest SLOT-NAMES)
		  `(progn
		     ,@(loop for SLOT-NAME in SLOT-NAMES
			     collecting `(copy-slot ,SLOT-NAME)))))
       ,@BODY)))

;;;
;;;
;;;
(defmacro WITH-STACK-LIST-COPY ((variable list) &body body)
  "Like `((let ((,variable (copy-list ,list))) ,@body) 
   except that the copy is consed on the stack."
  #+Genera
  `(let ((.n. (length ,list))
	 (.l. ,list))
     (flet ((.body. (&rest ,variable)
	      ,@body))
       (sys:%start-function-call #'.body. return .n. nil)
       (do () ((null .l.)) (sys:%push (pop .l.)))
       (sys:%finish-function-call #'.body. return .n. nil)))
  #-Genera
  `(let ((,variable (copy-list ,list))) ,@body))
