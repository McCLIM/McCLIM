;;; -*- Syntax: Common-lisp; Package: DWIM -*-
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

(in-package :dwim)

;;;***********************************************************
;;; #FEATURE-CASE( ... ) :: The FEATURE-CASE MACRO.
;;; A CASE-like reader-macro that dispatches on Features.
;;;
;;;(defun sheet-parent (sheet)
;;;  #FEATURE-CASE
;;;  (((not :clim)              (send sheet :superior))
;;;   (:clim-0.9                (clim:sheet-parent sheet))
;;;   ((or :clim-1.0 :clim-2)   (clim:window-parent sheet))))
;;;
;;; This is nearly equivalent to:
;;;
;;;(defun sheet-parent (sheet)
;;;  #-clim (send sheet :superior)
;;;  #+clim-0.9 (clim:sheet-parent sheet)
;;;  #+(or :clim-1.0 :clim-2) (clim:window-parent sheet))
;;;
;;; These two examples differ in that feature-case provides an implicit
;;; otherwise clause that will generate a useful error message if no
;;; feature matches the current environment, something like:
;;;
;;; ERROR:  Sorry, this portion of the program is only supported for:
;;; (not :clim)
;;; :clim-0.9
;;; (or :clim-1.0 :clim-2)
;;;
;;; An ARG to FEATURE-CASE (e.g. #1FEATURE-CASE) suppresses the OTHERWISE/T clause.
;;; Of course, you are always free to rewrite the clause yourself.
;;;
;;; NOTE: This uses *read-suppress* just like #+ and #- so that illegal syntax
;;;       is not a problem unless the feature matches the current environment.
;;;*****************************************************************

(defun feature-case-error (features)
  (error
   "Sorry, this portion of this program is currently
    supported only for the following:~{~%~S ~}" features))

(defun stringify (thing)
  (typecase thing
    (string thing)
    (symbol (string thing))
    (t (princ-to-string thing))))

(defun feature-p (feature)
  (if (atom feature)
      (not (null (member (stringify feature) *features*
			 :key #'stringify :test #'equalp)))
      (ecase (car feature)
	(not (not (feature-p (second feature))))
	(and (every #'feature-p (cdr feature)))
	(or  (some  #'feature-p (cdr feature))))))

(defun feature-case-macro-function (STREAM CHAR ARG)
  (cond
    (*read-suppress*
     (read stream)
     (read stream))
    (t (let ((object (read stream)))
	 (unless (and (symbolp object)
		      (equalp (string object) "EATURE-CASE"))
	   (error "Unknown #~C Macro: ~A~A" char char object)))
       (let* ((features nil))
	 (flet ((make-default-clause ()
		  (if (not arg) `(feature-case-error ',features)))
		(skip-whitespace ()
		  (loop
		   (let ((char (peek-char nil stream)))
		     (cond ((member char '(#\Space #\Tab #\Return
					   #\Newline))
			    (read-char stream))
			   ((eql char #\;)
			    (read-line stream))
			   (t (return)))))))
	   (skip-whitespace)
	   (let ((char (read-char stream)))
	     (or (eql char #\()
		 (error "Illegal #FEATURE-CASE syntax (unexpected character ~C)" char)))
	   (loop
	    (skip-whitespace)
	    (let ((char (read-char stream)))
	      (cond ((eql char #\)) (return (make-default-clause)))
		    ((eql char #\())
		    (t (error "Illegal #FEATURE-CASE syntax (unexpected character ~C)" char))))
	    (let ((feature (read stream)))
	      (cond ((feature-p feature)
		     (return
		       (let ((forms (read-delimited-list #\) stream)))
			 ;; consume any remaining clauses before returning:
			 (let ((*READ-SUPPRESS* t))
			   (read-delimited-list #\) STREAM))
			 (if (cdr forms) `(progn ,@forms) (car forms)))))
		    (t (push feature features)
		       ;; Consume the rest of this clause and throw it away:
		       (let ((*READ-SUPPRESS* t))
			 (read-delimited-list #\) STREAM)))))))))))

;;; This used to be (eval-when (compile load eval) ...), but
;;; compile-time definition of the read macro is obviously incorrect.
(progn
  (set-dispatch-macro-character #\# #\F 'feature-case-macro-function)
  (set-dispatch-macro-character #\# #\f 'feature-case-macro-function)
  )

(defun not-done ()
  (error "This operation not completed for this platform/system"))

