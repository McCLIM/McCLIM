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

;;;*****************
;;; Lisp Extensions
;;;*****************

#-(and)
(unless (fboundp 'ignore)
  ;; Define IGNORE to be like our old friend from Genera.
  ;; This practice is frowned upon because IGNORE is in the
  ;; common lisp package (it is a declaration) and changing
  ;; anything about those symbols is frowned upon.  So we
  ;; should learn to live without this old friend some day.
  #FEATURE-CASE
  ((:allegro
    (excl:without-package-locks 
     (setf (symbol-function 'ignore)
       #'(lambda (&rest args)
	   (declare (ignore args) (dynamic-extent args))
	   nil))))
   ((not allegro)
    (unless (fboundp 'ignore)
      (setf (symbol-function 'ignore)
	#'(lambda (&rest args)
	    (declare (ignore args) (dynamic-extent args))
	    nil))))))

(defmacro with-rem-keywords ((new-list list keywords-to-remove) &body body)
  `(let ((,new-list (with-rem-keywords-internal ,list ,keywords-to-remove)))
    ,@body))

(defun with-rem-keywords-internal (keyword-list keywords-to-remove)
  ;; Remove leading keywords.
  (loop (unless (member (car keyword-list) keywords-to-remove)
	  (return))
	(setf keyword-list (cddr keyword-list)))
  (when keyword-list
    (do* ((kwl keyword-list cons2)	
	  (cons1 (cdr kwl) (cdr kwl))
	  (cons2 (cdr cons1) (cdr cons1)))
	 ((null cons2) keyword-list)
      (when (member (car cons2) keywords-to-remove)
	(setf (cdr cons1) (cddr cons2))
	(setf cons2 kwl)))))

(defun rem-keywords (list keywords-to-remove)
  (with-rem-keywords (new-list list keywords-to-remove)
    (copy-list new-list)))

;;;Still need Genera
(eval-when (compile eval load)
  #+lucid (import '(#-clim-1.0 lcl:*load-pathname* lcl:*source-pathname*))
  #+allegro (import 'excl:*source-pathname*))

#1feature-case
((:lucid 
  (eval-when (compile eval load)
    ;;Just use the existing Lucid definition
    (import 'lcl:working-directory)))
 (:allegro
  (defun working-directory ()
    (excl:current-directory))
  (defsetf working-directory excl:chdir)))
  


;;; **************************
;;; UNIX Environmental Support
;;; **************************
  
						
(defun getenv (string)
  "Get the value of the environment variable named STRING."
  (assert (stringp string))
  #FEATURE-CASE
  ((:lucid (lucid::environment-variable string))
   (:allegro (system:getenv string))
   (:genera (let ((symbol (intern string :scl)))
	      (and (boundp symbol) (symbol-value symbol))))
   (:openmcl (ccl::getenv string))
   (:sbcl (sb-ext:posix-getenv string))
   (:scl (cdr (assoc string ext:*environment-list* :test #'string=)))
   ))

#+allegro
;;>> Allegro 4.2 supports SYSTEM:GETENV.  How do I set an environment variable?
;;>> I expected a SETF method or a SETENV function.
;;Franz sez: Well here's one way of doing it: foreign call putenv from libc.a.
(progn
  (load  "" :unreferenced-lib-names
	 `(,(ff:convert-to-lang "putenv")))
  (ff:defforeign 'putenv :arguments '(integer)))

#+sbcl
(sb-alien:define-alien-routine ("putenv" putenv) sb-alien:int
  (name sb-alien:c-string))

(defsetf getenv (string) (new-value)
  #FEATURE-CASE
  ((:allegro `(putenv (ff:string-to-char* (format nil "~A=~A" ,string ,new-value))))
   (:lucid `(setf (lcl:environment-variable ,string) ,(princ-to-string new-value)))
   (:genera `(setf (symbol-value ,(intern string :scl)) ,new-value))
   (:openmcl `(ccl::setenv string new-value))
   (:sbcl `(putenv (format nil "~A=~A" ,string ,new-value)))))

(defun run-shell-command (command &rest args &key input output error-output (wait t)
						  arguments
						  if-input-does-not-exist 
						  if-output-exists
						  if-error-output-exists)
  "Runs a shell command.  See documentation for Allegro CL version of this for return
   value details.  Command can include the arguments, or they can be additionally be
   specified by the :ARGUMENTS keyword arg."
  (declare (ignore input output error-output if-input-does-not-exist
		   if-output-exists if-error-output-exists #-lucid wait))
  (assert (listp arguments)) ;;Should be a list of strings
  (let ((command-with-arguments (format nil "~A~{ ~A~}" command arguments)))
    #FEATURE-CASE
    ((:allegro
      (with-rem-keywords (args1 args '(:arguments))
	(apply #'excl:run-shell-command command-with-arguments args1)))
     (:lucid
      (let* ((end-of-command-pos 
	      (position #\space command-with-arguments :test #'char=))
	     (command-only 
	      (if end-of-command-pos
		  (subseq command-with-arguments 0 end-of-command-pos)
		command-with-arguments))
	     (real-arguments 
	      (string-trim " "
			   (subseq command-with-arguments end-of-command-pos))))
	(with-rem-keywords (args1 args '(:arguments))
	  (multiple-value-bind (stream1 stream2 exit-status process-id)
	      (apply #'lcl:run-program command-only :arguments real-arguments args1)
	    (if wait
		exit-status
	      (values stream1 stream2 process-id))))))
     ((and :mcl (not :openmcl))
      (with-rem-keywords (args1 args '(:arguments))
	(apply #'ccl:run-fred-command command-with-arguments args1)))
     ((or :openmcl :sbcl)
      (with-rem-keywords (args1 args '(:arguments))
	(apply #+sbcl #'sb-ext:run-program
	       #+openmcl #'ccl:run-program
	       command arguments args1)))
     (:genera (not-done)))))
	      

(defun current-process ()
  #FEATURE-CASE
  ((:genera process::*current-process*)
   (:allegro mp::*current-process*)
   (:lucid nil)
   (:clim-1.0 clim-utils::*current-process*)
   (:clim-2 (clim-sys:current-process))))

(defun process-wait (whostate predicate)
  #FEATURE-CASE
  ((genera (scl:process-wait whostate predicate))
   (lucid (lcl:process-wait whostate predicate))
   (allegro (mp:process-wait whostate predicate))
   (clim-1.0 (clim-utils:process-wait whostate predicate))
   (clim-2   (clim-sys:process-wait whostate predicate))))

(defun process-run-function (name-or-keywords function &rest args)
  (let* ((new-args (copy-list args)) ; in case of stack-allocation
	 (predicate
	  (if args #'(lambda () (apply function new-args)) function)))
    #FEATURE-CASE
    ((:ALLEGRO
      (funcall #'mp:process-run-function name-or-keywords predicate))
     (:GENERA
      (funcall #'scl:process-run-function name-or-keywords predicate))
     (:LUCID
      (flet ((lucid-procees-run-function-hack (NAME-OR-KEYWORDS
					       &rest FNCT-LIST)
	       (let ((FNCT-NAME (first FNCT-LIST))
		     (FNCT-ARGS (copy-list (cdr FNCT-LIST))))
		 (if (consp NAME-OR-KEYWORDS)
		     (apply #'lcl::make-process
			    :function FNCT-NAME
			    :args FNCT-ARGS
			    NAME-OR-KEYWORDS)
		   (lcl::make-process
		    :name NAME-OR-KEYWORDS
		    :function FNCT-NAME
		    :args FNCT-ARGS)))))
	(apply #'lucid-procees-run-function-hack
	       name-or-keywords function args)))
     ((and :MCL (not :openmcl))
      ;; No multiprocessing.  Fake it.
      (funcall predicate))
     (:CLIM-1.0
      (clim-utils::make-process predicate :name name-or-keywords))
     ((and :clim-2 :cmu (not :x86))
      ;; This is a hack for CMUCL
      (funcall predicate))
     (:CLIM-2
      ;; The Spec says that make-process takes a keyword arg... -- moore
      (CLIM-SYS:make-process predicate #+mcclim :name name-or-keywords)))))

(defun activate-process (p)
  #FEATURE-CASE
  ((:lucid (lcl::activate-process p))
   (:allegro (mp:process-enable p))
   (:clim-2 (clim-sys:enable-process p))))

(defun deactivate-process (p)
  #FEATURE-CASE
  ((:lucid (lcl::deactivate-process p))
   (:allegro (mp:process-disable p))
   (:clim-2 (clim-sys:disable-process p))))

(defun process-interrupt (p function &rest args)
  #FEATURE-CASE
  ((:allegro (apply #'mp:process-interrupt p function args))))

(defun kill-process (p)
  #FEATURE-CASE
  ((:genera (process:kill p))
   (:clim-0.9 (ci::destroy-process p))
   (:clim-1.0 (clim-utils:destroy-process p))
   (:clim-2 (clim-sys:destroy-process p))))

(defmacro with-process-lock ((lock) &body body)
  "Grant current process exclusive access to some resource.  Wait for access if necessary."
  #+allegro
  `(progn
     (or ,lock (setf ,lock (mp:make-process-lock)))
     (mp:with-process-lock (,lock) ,@body))
  #+lucid
  `(lucid::with-process-lock (,lock) ,@body)
  #+clim-2
  `(clim-sys:with-lock-held (,lock)
     ,@body)
  #+genera
  (let ((me (gensym)))
    `(let ((,me scl:*current-process*))
       (if (eq ,lock ,me) (error "Lock already locked by this process."))
       (unwind-protect
	   (if (or (si:store-conditional (scl:locf ,lock) nil ,me)
		   (and (process::safe-to-process-wait-p scl:*current-process*)
			(scl:process-wait "Lock"
			  #'(lambda (locative)
			      (declare (sys:downward-function))
			      (si:store-conditional locative nil ,me))
			  (scl:locf ,lock))))
	       (when (eq ,lock ,me) ,@body))
	 (si:store-conditional (scl:locf ,lock) ,me nil)))))

;;;Loop unrolling can increase the performance of big loops by 10 to 30% if the body
;;;  is fast relative to the price of an iteration.  Here is a portable version of
;;;  DOTIMES that unrolls its body.  The argument BLOCKING must be an integer; the
;;;  compiler unrolls the loop BLOCKING number of times.  A good number to use is 8.
;;;  Avoid choosing a really big integer because your compiled code will be huge.

(defmacro dotimes-unrolled ((var init countform blocking &optional resultform) &body body)
  (unless (integerp blocking)
    (error "To unroll this loop, ~S must be an integer." blocking))
  `(let ((,var ,init))
     (dotimes (ignore (floor ,countform ,blocking))
       ,@(let ((result nil))
	   (setq body (append body `((incf ,var))))
	   (dotimes (ignore blocking)
	     (setq result (nconc (copy-list body) result)))
	   result))
     (dotimes (ignore (mod ,countform ,blocking) ,resultform)
       ,@body)))				

#+test
(defun roll-test (n)
  (let ((number 2.1))
    (multiple-value-bind (val time)
	(the-time
	  (dotimes (i n)
	    (* number number)))
      (print val)
      (print time))
    (multiple-value-bind (val time)
	(the-time
	  (dotimes-unrolled (i 0 n 20)
	    (* number number)))
      (print val)
      (print time))
    ))

;;; Zetalisp function.
(defmethod instancep ((object t)) nil)
(defmethod instancep ((object standard-object)) t)

(defun type-specifier-p (object)
  "Determine if OBJECT is a valid type specifier"
  ;; A somewhat consful implementation, but entirely portable.
  (let ((test #'(lambda (x) (typep 't x))))
    (when (or (symbolp object) (listp object))
      (multiple-value-bind (v errorp) (ignore-errors (funcall test object))
	(declare (ignore v))
	(not errorp)))))

(defun file-type-for-binaries ()
  #FEATURE-CASE
  ((:genera si:*default-binary-file-type*)
   ((or :allegro :sbcl)
    #.(if (fboundp 'compile-file-pathname)
	  (pathname-type (compile-file-pathname "foo"))
	  "fasl"))
   (:scl (pathname-type (compile-file-pathname "foo")))
   (:lucid (car lcl:*load-binary-pathname-types*))
   (:mcl #.(pathname-type ccl:*.fasl-pathname*))
   ))

(defun file-type-for-sources ()
  #FEATURE-CASE
  ((:genera "LISP")
   (:unix "lisp")
   (:mcl  "lisp")
   ))

;;;************
;;; DUMPER
;;;************

;;; Here is the simplest possible dumper that worries about the EQ-ness of shared
;;; objects.  Objects that satisfy INSTANCEP are put into a hash table.  References
;;; to such objects are replaced with corresponding GETHASH forms:
;;;
;;;      `(progn (setq frog #<FROG 123>) (setq reptile #<FROG 123>))
;;;
;;;           becomes something like
;;;
;;;      (let ((*dump-table* (make-hash-table)))
;;;         (setf (gethash 0 *dump-table*) (make-instance 'frog))
;;;         (progn (setq frog (gethash 0 *dump-table*)))
;;;                (setq reptile (gethash 0 *dump-table*))))
;;;
;;; Thus the dumper augments the forms being dumped with appropriate calls to
;;; MAKE-HASH-TABLE and GETHASH.  Thus any Common Lisp reader is sufficient
;;; to parse this ASCII file.
;;;
;;; KRA has pointed out that when you load via (EVAL (READ)), the form being read
;;; can be as large as the object being recreated.  After the EVAL, the form is
;;; thrown away, which may be somewhat difficult for the GC to swallow if it is
;;; large.  A better dumper would be more considerate of the GC.
;;;
;;; Some day, this dumper should probably be extended to worry about structure-sharing
;;; of all structures, particularly lists and instances of STRUCTURE-OBJECT.  

(defvar *dump-table* (make-hash-table) "Hash table used by the dumper.")
(defvar *dump-index* 0 "Counter used by the dumper.")

(defmacro writing-dump-file ((stream-var file) &body body)
  `(let ((*dump-table* (make-hash-table))
	 (*dump-index* 0))
     (with-open-file (,stream-var ,file :direction :output :if-exists :supersede)
       ,@body)))

(defun enter-table (object)
  (setf (gethash object *dump-table*) t)
  (incf *dump-index*))

(defun finish-enter-table (object index)
  (setf (gethash object *dump-table*) (1- index)))

(defun dump-form-to-eval (form stream)
  "Dump a form directly to the stream."
  (print form stream))

(defun dump-instance (object stream)
  "Use MAKE-LOAD-FORM to dump this object to the stream."
  (multiple-value-bind (maker initializer) (make-load-form object)
    (let ((index (enter-table object))
	  (symbol (intern "*DUMP-TABLE*")))
      (dump-form-to-file `(setf (gethash ,(1- index) ,symbol) ,maker) stream)
      (finish-enter-table object index)
      (when initializer (dump-form-to-file initializer stream)))))

(defun dump-form-to-file (form stream)
  "Dump a form that may refer to instances."
  (labels ((tree-search (tree predicate)
	     (if (atom tree)
		 (if (funcall predicate tree)
		     (return-from tree-search t)
		   nil)
	       (dolist (element tree)
		 (if (tree-search element predicate)
		     (return-from tree-search t)))))
	   (need-full-dump (object)
	     (or (instancep object)
		 (and (arrayp object) (not (stringp object)))
		 (hash-table-p object )))
	   (traverse (form)
	     (let (index)
	       (cond ((need-full-dump form)
		      (cond ((setq index (gethash form *dump-table*))
			     (if (not (numberp index))
				 (error "Circular dependency encountered.")
			       `(gethash ,index ,(intern "*DUMP-TABLE*"))))
			    (t (dump-instance form stream)
			       (traverse form))))
		     ((atom form) form)
		     ((not (eq (first form) 'quote))
		      ;; The normal case, an unquoted form.
		      (mapcar #'traverse form))
		     ((tree-search form #'need-full-dump)
		      ;; KRA has pointed out that LIST can take no more than 512
		      ;; arguments in Lucid.  That means the following will break
		      ;; if the list is a long one.
		      (if (atom (second form))
			  (traverse (second form)) ; quoted instance
			(traverse ; list contains instances
			 `(list ,@(mapcar #'(lambda (x) `(quote ,x))
					  (second form)))))) 
		     (t form)))))
    (dump-form-to-eval (traverse form) stream)))

(defun dump-objects-to-file (file objects
			     &optional (package #+ansi-cl :common-lisp-user
						#-ansi-cl :user))
  "Use the MAKE-LOAD-FORM protocol to dump a list of objects to the specified file."
  (or (and (symbolp package) (find-package package))
      (error "Package ~A does not exist" package))
  (setq file (namestring (pathname file)))
  (let ((*package* (find-package package)))
    (writing-dump-file (stream file)
      (format stream ";-*- Package: ~A; Syntax: Common-Lisp -*-" (package-name *package*))
      (format stream "~%(lisp::in-package ~S)" (package-name *package*))
      (format stream "~%(let ((~S (make-hash-table)))"
	      (intern "*DUMP-TABLE*"))
      (dolist (object objects)
	(dump-instance object stream))
      (format stream "~%)"))))



