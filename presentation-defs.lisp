;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2001,2002 by Tim Moore (moore@bricoworks.com)
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

;;; Definitions for the standard presentation types and generic functions

(in-package :clim-internals)

;;; The presentation type for T is the built-in type T. The correspondence is
;;; established by hand in presentations.lisp.
;(define-presentation-type t ())

;;; auto-activate is described in the Franz user guide; it controls whether an
;;; accepting an expression returns immediately after typing the closing
;;; delimiter -- a la Genera et Mac Lisp -- or if an activation gesture is
;;; required. 
;;; preserve-whitespace controls whether the accept method uses read or
;;; read-preserving-whitespace. This is used in our redefinitions of read and
;;; read-preserving-whitespace that accept forms.

(define-presentation-type expression ()
  :options (auto-activate (preserve-whitespace t) (subform-read nil))
  :inherit-from t)


(define-presentation-type form ()
  :options (auto-activate (preserve-whitespace t) (subform-read nil))
  :inherit-from `((expression) :auto-activate ,auto-activate
		  :preserve-whitespace ,preserve-whitespace
		  :subform-read ,subform-read ))

;;; Actual definitions of presentation methods and types.  They've
;;; been separated from the macro and presentation class definitions and
;;; helper functions in order to avoid putting all of presentations.lisp
;;; inside a (eval-when (compile) ...).

(define-presentation-generic-function %presentation-typep presentation-typep
  (type-key parameters object type))

(define-default-presentation-method presentation-typep (object type)
  (declare (ignore object type))
  nil)

(defun presentation-typep (object type)
  (with-presentation-type-decoded (name parameters)
    type
    (when (null parameters)
      (let ((clos-class (find-class name nil)))	; Don't error out.
	(when (and clos-class (typep clos-class 'standard-class))
	  (return-from presentation-typep (typep object name)))))
    (funcall-presentation-generic-function presentation-typep object type)))

;;; Not defined as a generic function, but what the hell.

(defgeneric presentation-type-of (object))

(defmethod presentation-type-of (object)
  (declare (ignore object))
  'expression)

(defun get-ptype-from-class-of (object)
  (let* ((name (class-name (class-of object)))
	 (ptype-entry (gethash name *presentation-type-table*)))
    (unless ptype-entry
      (return-from get-ptype-from-class-of nil))
    ;; Does the type have required parameters?  If so, we can't use it...
    (let ((parameter-ll (parameters-lambda-list ptype-entry)))
      (values name 
              (if (eq (car parameter-ll) '&whole)
                  (cddr parameter-ll)
                  parameter-ll)))))

(defmethod presentation-type-of ((object standard-object))
  (multiple-value-bind (name lambda-list)
      (get-ptype-from-class-of object)
    (cond ((and name
		(or (null lambda-list)
		    (member (first lambda-list) lambda-list-keywords)))
	   name)
	  (name
	   'standard-object)
	  (t (let* ((class (class-of object))
		    (class-name (class-name class)))
	       (or class-name class))))))

(defmethod presentation-type-of ((object structure-object))
  (multiple-value-bind (name lambda-list)
      (get-ptype-from-class-of object)
    (if (or (null lambda-list)
            (member lambda-list lambda-list-keywords))
        name
        (call-next-method))))

(define-presentation-generic-function
    %map-over-presentation-type-supertypes
    map-over-presentation-type-supertypes
  (type-key function type))

;;; Define the method for presentation and clos types
(define-default-presentation-method map-over-presentation-type-supertypes
    (function type)
  (let ((type-name (presentation-type-name type)))
    (map-over-ptype-superclasses
     #'(lambda (super)
	 (let ((super-name (type-name super)))
	   (funcall function
		    super-name
		    (funcall (expansion-function super)
			     (translate-specifier-for-type type-name
							   super-name
							   type)))))
     type-name)))

(defun map-over-presentation-type-supertypes (function type)
  (funcall-presentation-generic-function map-over-presentation-type-supertypes
					 function
					 type))

(define-presentation-generic-function
    %presentation-subtypep
    presentation-subtypep
  (type-key type putative-supertype))

;;; The semantics of the presentation method presentation-subtypep are truly
;;; weird; method combination is in effect disabled.  So, the methods have to
;;; be eql methods.

(defmacro define-subtypep-method (&rest args)
  (let ((gf (gethash 'presentation-subtypep *presentation-gf-table*)))
    (multiple-value-bind (qualifiers lambda-list decls body)
	(parse-method-body args)
      (let ((type-arg (nth (1- (type-arg-position gf)) lambda-list)))
	
	(unless (consp type-arg)
	  (error "Type argument in presentation method must be specialized"))
	(unless (eq (car type-arg)  'type)
	  (error "Type argument mismatch with presentation generic function
 definition"))
	(destructuring-bind (type-var type-name) type-arg
	  (let ((method-ll `((,(type-key-arg gf)
			      (eql (prototype-or-error ',type-name)))
			     ,@(copy-list lambda-list))))
	    (setf (nth (type-arg-position gf) method-ll) type-var)
	    `(defmethod %presentation-subtypep ,@qualifiers ,method-ll
	       (declare (ignorable ,(type-key-arg gf))
		        ,@(cdr decls))
	       (block presentation-subtypep
		 ,@body))))))))

(defun presentation-subtypep (type maybe-supertype)
  (when (equal type maybe-supertype)
    (return-from presentation-subtypep (values t t)))
  (with-presentation-type-decoded (super-name super-parameters)
    maybe-supertype
    (when (eq super-name 'or)
      (loop for or-type in super-parameters
	    when (presentation-subtypep type or-type)
	    do (return-from presentation-subtypep (values t t))
	    finally (return-from presentation-subtypep (values nil t))))
    (map-over-presentation-type-supertypes
     #'(lambda (name massaged)
	 (when (eq name super-name)
	   (return-from presentation-subtypep
	     (funcall-presentation-generic-function presentation-subtypep
						    massaged
						    maybe-supertype))))
     type))
  (values nil t))

(define-default-presentation-method presentation-subtypep
    (type maybe-supertype)
  (with-presentation-type-decoded (name params)
    type
    (declare (ignore name))
    (with-presentation-type-decoded (super-name super-params)
      maybe-supertype
      (declare (ignore super-name))
      (if (equal params super-params)
	  (values t t)
	  (values nil nil)))))

(defun default-describe-presentation-type (description stream plural-count)
  (if (symbolp description)
      (setq description (make-default-description (symbol-name description))))
  (cond ((eql 1 plural-count)
	 (format stream "~:[a~;an~] ~A"
		   (find (char description 0) "aeiouAEIOU")
		   description))
	((numberp plural-count)
	 (format stream "~D ~A~P" plural-count description plural-count))
	(plural-count
	 (format stream "~As" description))
	(t (write-string description stream))))

(define-presentation-generic-function %describe-presentation-type
    describe-presentation-type
  (type-key parameters options type stream plural-count ))

;;; Support for the default method on describe-presentation-type: if a CLOS
;;; class has been defined as a presentation type, get description out of the
;;; presentation type.

(defmethod description ((class standard-class))
  (let* ((name (class-name class))
	 (ptype-entry (gethash name *presentation-type-table*)))
    (if ptype-entry
	(description ptype-entry)
	(make-default-description name))))

(define-default-presentation-method describe-presentation-type
    (type stream plural-count)
  (with-presentation-type-decoded (name parameters options)
    type
    (declare (ignore name parameters))
    (let ((description (or (getf options :description)
			   (description (class-of type-key)))))
      (default-describe-presentation-type description
	                                  stream
	                                  plural-count))))

(defun describe-presentation-type (type
				   &optional
				   (stream *standard-output*)
				   (plural-count 1))
  (flet ((describe-it (stream)
	   (funcall-presentation-generic-function describe-presentation-type
						  type
						  stream
						  plural-count)))
    (if stream
	(describe-it stream)
	(with-output-to-string (s)
	  (describe-it s)))))

(define-presentation-generic-function %presentation-default-processor
    presentation-default-processor
    (type-key parameters default type &key default-type))

(define-default-presentation-method presentation-default-processor
    (default type &key (default-type nil default-type-p))
  (values default (if default-type-p
		      default-type
		      type)))

;;; XXX The spec calls out that the presentation generic function has keyword
;;; arguments acceptably and for-context-type, but the examples I've seen don't
;;; mention them at all in the methods defined for present.  So, leave them out
;;; of the generic function lambda list...
(define-presentation-generic-function %present present
    (type-key parameters options object type stream view
     &key &allow-other-keys))

(defmacro with-output-as-presentation ((stream object type
					       &rest key-args
					       &key modifier single-box
					       (allow-sensitive-inferiors t)
					       parent
					       (record-type
						''standard-presentation)
					       &allow-other-keys)
				       &body body)
  (declare (ignore parent single-box modifier))
  (setq stream (stream-designator-symbol stream '*standard-output*))
  (multiple-value-bind (decls with-body)
      (get-body-declarations body)
    (with-gensyms (record-arg continuation)
      (with-keywords-removed (key-args (:record-type
					:allow-sensitive-inferiors))
	`(flet ((,continuation ()
		  ,@decls
		  ,@with-body))
	   (declare (dynamic-extent #'continuation))
	   (if (and (output-recording-stream-p ,stream)
		    *allow-sensitive-inferiors*)
	       (with-new-output-record
		   (,stream ,record-type ,record-arg
			    :object ,object
			    :type (expand-presentation-type-abbreviation
				   ,type)
			    ,@key-args)
		 (let ((*allow-sensitive-inferiors*
			,allow-sensitive-inferiors))
		   (,continuation)))
	       (,continuation)))))))


(defun present (object &optional (type (presentation-type-of object))
		&key
		(stream *standard-output*)
		(view (stream-default-view stream))
		modifier
		acceptably
		(for-context-type type)
		single-box
		(allow-sensitive-inferiors t)
		(sensitive t)
		(record-type 'standard-presentation))
  (let* ((real-type (expand-presentation-type-abbreviation type))
	 (context-type (if (eq for-context-type type)
			   real-type
			   (expand-presentation-type-abbreviation
			    for-context-type))))
    (stream-present stream object real-type
		    :view view :modifier modifier :acceptably acceptably
		    :for-context-type context-type :single-box single-box
		    :allow-sensitive-inferiors allow-sensitive-inferiors
		    :sensitive sensitive
		    :record-type record-type)))

(defgeneric stream-present (stream object type
			    &key view modifier acceptably for-context-type
			    single-box allow-sensitive-inferiors sensitive
			    record-type))

(defmethod stream-present ((stream output-recording-stream) object type
			   &key
			   (view (stream-default-view stream))
			   modifier
			   acceptably
			   (for-context-type type)
			   single-box
			   (allow-sensitive-inferiors t)
			   (sensitive t)
			   (record-type 'standard-presentation))
  ;; *allow-sensitive-inferiors* controls whether or not
  ;; with-output-as-presentation will emit a presentation
  (let ((*allow-sensitive-inferiors* (and *allow-sensitive-inferiors*
					  sensitive)))
    (with-output-as-presentation (stream object type
				  :view view
				  :modifier modifier
				  :single-box single-box
				  :allow-sensitive-inferiors
				  allow-sensitive-inferiors
				  :record-type record-type)
      (funcall-presentation-generic-function
       present object type stream view
       :acceptably acceptably :for-context-type for-context-type))))

;;; Should work well enough on non-CLIM streams...
(defmethod stream-present (stream object type
			   &key
			   (view +textual-view+)
			   modifier
			   acceptably
			   (for-context-type type)
			   single-box
			   (allow-sensitive-inferiors t)
			   (sensitive t)
			   (record-type 'standard-presentation))
  (declare (ignore modifier single-box allow-sensitive-inferiors sensitive
		   record-type))
  (funcall-presentation-generic-function
   present object type stream view
   :acceptably acceptably :for-context-type for-context-type)
  nil)

(defun present-to-string (object &optional (type (presentation-type-of object))
			  &key (view +textual-view+)
			  acceptably
			  (for-context-type type)
			  (string nil stringp)
			  (index 0 indexp))
  (let* ((real-type (expand-presentation-type-abbreviation type))
	 (context-type (if (eq for-context-type type)
			   real-type
			   (expand-presentation-type-abbreviation
			    for-context-type))))
    (when (and stringp indexp)
      (setf (fill-pointer string) index))
    (flet ((do-present (s)
	     (stream-present s object real-type
			     :view view :acceptably acceptably
			     :for-context-type context-type)))
      (declare (dynamic-extent #'do-present))
      (let ((result (if stringp
			 (with-output-to-string (stream string)
			   (do-present stream))
			 (with-output-to-string (stream)
			   (do-present stream)))))
	(if stringp
	    (values string (fill-pointer string))
	    result)))))

;;; I believe this obsolete... --moore
(defmethod presentation-replace-input
    ((stream input-editing-stream) object type view
     &key (buffer-start nil buffer-start-supplied-p)
     (rescan nil rescan-supplied-p)
     query-identifier
     (for-context-type type))
  (declare (ignore query-identifier))
  (let ((result (present-to-string object type
				   :view view :acceptably nil
				   :for-context-type for-context-type)))
    (apply #'replace-input stream result `(,@(and buffer-start-supplied-p
						  `(:buffer-start
						    ,buffer-start))
					   ,@(and rescan-supplied-p
						  `(:rescan ,rescan))))))

;;; Presentation histories.

;;; This allocates a hash table even if the frame doesn't support
;;; histories!  I'm over it already. -- moore
(defclass presentation-history-mixin ()
  ((presentation-history :accessor frame-presentation-history
			 :initform (make-hash-table :test #'eq)))
  (:documentation "Mixin class for frames that implements presentation
type histories"))

(define-presentation-generic-function %presentation-type-history
    presentation-type-history
  (type-key parameters type))

;;; This function should exist for convenience even if not mentioned in the
;;; spec.

(defun presentation-type-history (type)
  (funcall-presentation-generic-function presentation-type-history type))

(defclass presentation-history-ring (goatee::ring)
  ())

(define-default-presentation-method presentation-type-history (type)
  (if (and *application-frame*
	   (frame-maintain-presentation-histories *application-frame*))
      (with-presentation-type-decoded (name)
	type
	(let* ((ptype (get-ptype-metaclass name))
	       (history (history ptype)))
	  (case history
	    ((t)
	     (let* ((history-table (frame-presentation-history
				    *application-frame*))
		    (history-object (gethash name history-table)))
	       (unless history-object
		 (setf history-object
		       (make-instance 'presentation-history-ring)
		       (gethash name history-table)
		       history-object))
	       history-object))
	    (nil
	     nil)
	    (otherwise
	     (funcall-presentation-generic-function presentation-type-history
						    type)))))))

;;; Not in the spec, but I think this is necessary (or at any rate, the easiest
;;; way) to control whether or not to use histories in a given context.
(define-presentation-generic-function %presentation-type-history-for-stream
    presentation-type-history-for-stream
  (type-key parameters type stream)
  (:documentation "Returns a type history or nil for a presentation TYPE and
STREAM. This is used by McCLIM to decide whether or not to use histories in a
given situation. A primary method specialized on just the type should
call-next-method to get the \"real\" answer based on the stream type."))

(define-default-presentation-method presentation-type-history-for-stream
    (type stream)
  (declare (ignore stream))
  nil)

;;; method for clim-stream-pane in panes.lisp

(define-presentation-method presentation-type-history-for-stream
    ((type t) (stream input-editing-stream))
  (if (not (stream-rescanning-p stream))
      (funcall-presentation-generic-function presentation-type-history type)
      nil))

(defun presentation-history-insert (history object ptype)
  (goatee::ring-obj-insert (cons object ptype) history))

(defun presentation-history-head (history ptype)
  (loop
   for cell = (goatee::dbl-head history) then (goatee::next cell)
   for (object . object-ptype) = (and cell (goatee::contents cell))
   while cell
   if (presentation-subtypep object-ptype ptype)
   return (values object object-ptype)
   finally (return (values nil nil))))

(defun presentation-history-next (history ptype)
  (let ((first-object (goatee::forward history)))
    (loop
     for first-time = t then nil
     for cell = first-object then (goatee::forward history)
     for (object . object-ptype) = (goatee::contents cell)
     while (or first-time (not (eq first-object cell)))
     if (presentation-subtypep object-ptype ptype)
       return (values object object-ptype)
     end
     finally (return (values nil nil)))))

(defmacro with-object-on-history ((history object ptype) &body body)
  `(goatee::with-object-on-ring ((cons ,object ,ptype) ,history)
     ,@body))

(defun presentation-history-add (history object ptype)
  "Add OBJECT and PTYPE to the HISTORY unless they are already at the head of
 HISTORY"
  (let* ((cell (goatee::dbl-head history))
	 (contents (and cell (goatee::contents cell))))
    (unless (and cell
		 (eql object (car contents))
		 (equal ptype (cdr contents)))
      (presentation-history-insert history object ptype))))

;;; Context-dependent input
;;; An input context is a cons of a presentation type and a continuation to
;;; call to return a presentation to that input context.

(defvar *input-context* nil)

(defun input-context-type (context-entry)
  (car context-entry))

;;; Many presentation functions, internal and external, take an input
;;; context as an argument, but they really only need to look at one
;;; presentation type.
(defun make-fake-input-context (ptype)
  (list (cons (expand-presentation-type-abbreviation ptype)
	      #'(lambda (object type event options)
		  (declare (ignore event options))
		  (error "Fake input context called with object ~S type ~S. ~
                          This shouldn't happen!"
			 object type)))))

(defun input-context-wait-test (stream)
  (let* ((queue (stream-input-buffer stream))
	 (event (event-queue-peek queue)))
    (when event
      (let ((sheet (event-sheet event)))
	(when (and (output-recording-stream-p sheet)
		   (or (typep event 'pointer-event)
		       (typep event 'keyboard-event))
		   (not (gadgetp sheet)))
	  (return-from input-context-wait-test t))))
    nil))

(defun highlight-applicable-presentation (frame stream input-context
					  &optional (prefer-pointer-window t))
  (let* ((queue (stream-input-buffer stream))
	 (event (event-queue-peek queue)))
    (when (and event
	       (or (and (typep event 'pointer-event)
			(or prefer-pointer-window 
			    (eq stream (event-sheet event))))
		   (typep event 'keyboard-event)))
      ;; Stream only needs to see button press events.
      ;; XXX Need to think about this more.  Should any pointer events be
      ;; passed through?  If there's no presentation, maybe?
      (unless (typep event 'keyboard-event)
	(event-queue-read queue))
      (progn
	(frame-input-context-track-pointer frame
					   input-context
					   (event-sheet event)
					   event)
	(when (typep event 'pointer-button-press-event)
	  (funcall *pointer-button-press-handler* stream event)))
      #+nil
      (if (and (typep event 'pointer-motion-event)
	       (pointer-event-button event))
	  (frame-drag frame input-context (event-sheet event) event)
	  ))))

(defun input-context-event-handler (stream)
  (highlight-applicable-presentation *application-frame*
				     stream
				     *input-context*))

(defun input-context-button-press-handler (stream button-event)
  (declare (ignore stream))
  (frame-input-context-button-press-handler *application-frame*
					    (event-sheet button-event)
					    button-event))

(defun highlight-current-presentation (frame input-context)
  (let ((event (synthesize-pointer-motion-event (port-pointer
						 (port
						  *application-frame*)))))
    (when event
      (frame-input-context-track-pointer *application-frame*
				       *input-context*
				       (event-sheet event)
				       event))))

(defmacro with-input-context ((type &key override)
			      (&optional (object-var (gensym))
					 (type-var (gensym))
					 event-var
					 options-var)
			      form
			      &body pointer-cases)
  (let ((vars `(,object-var
		,type-var
		,@(and event-var `(,event-var))
		,@(and options-var `(,options-var))))
	(return-block (gensym "RETURN-BLOCK"))
	(context-block (gensym "CONTEXT-BLOCK")))
    `(block ,return-block
       (multiple-value-bind ,vars
	   (block ,context-block
	     (let ((*input-context*
		    (cons (cons (expand-presentation-type-abbreviation ,type)
                                #'(lambda (object type event options)
				    (return-from ,context-block
				      (values object type event options))))
			  ,(if override nil '*input-context*)))
		   (*pointer-button-press-handler*
		    #'input-context-button-press-handler)
		   (*input-wait-test* #'input-context-wait-test)
		   (*input-wait-handler* #'input-context-event-handler))
	       (return-from ,return-block ,form )))
         (declare (ignorable ,@vars))
	 (highlight-current-presentation *application-frame* *input-context*)
	 (cond ,@(mapcar #'(lambda (pointer-case)
			     (destructuring-bind (case-type &body case-body)
				 pointer-case
			       `((presentation-subtypep ,type-var ',case-type)
				 ,@case-body)))
			 pointer-cases))))))

(define-presentation-generic-function %accept accept
    (type-key parameters options type stream view &key))

(defvar *recursive-accept-p* nil)
(defvar *recursive-accept-1-p* nil)
(defvar *active-history-type* nil)

;;; The spec says "default-type most be a presentation type specifier", but the
;;; examples we have imply that default-type is optional, so we'll be liberal
;;; in what we accept.

(defun accept (type &rest rest-args &key
	       (stream *standard-input*)
	       (view nil viewp)
	       (default nil defaultp)
	       (default-type nil default-type-p)
	       provide-default insert-default replace-input
	       (history nil historyp)	; true default supplied below
	       active-p			; Don't think this will be used
	       prompt prompt-mode display-default
	       query-identifier
	       activation-gestures additional-activation-gestures
	       delimiter-gestures additional-delimiter-gestures)
  (declare (ignore insert-default replace-input active-p prompt prompt-mode
		   display-default query-identifier
		   activation-gestures additional-activation-gestures
		   delimiter-gestures additional-delimiter-gestures))
  (handler-bind ((abort-gesture (lambda (condition)
                                  (signal condition) ;; to give outer handlers a chance to say "I know how to handle this"
                                  (abort condition))))
    (let* ((real-type (expand-presentation-type-abbreviation type))
           (real-default-type (cond (default-type-p
                                     (expand-presentation-type-abbreviation
                                      default-type))
                                    ((or defaultp provide-default)
                                     real-type)
                                    (t nil)))
           (real-history-type (cond ((null historyp) real-type)
                                    ((null history) nil)
                                    (t (expand-presentation-type-abbreviation
                                        history))))
           (*recursive-accept-p* *recursive-accept-1-p*)
           (*recursive-accept-1-p* t))
      (with-keywords-removed (rest-args (:stream))
        (when (or default-type-p defaultp)
          (setf rest-args
                (list* :default-type real-default-type rest-args)))
        (when historyp
          (setf rest-args (list* :history real-history-type rest-args)))
        (cond ((and viewp (symbolp view))
               (setf rest-args
                     (list* :view (funcall #'make-instance view) rest-args)))
              ((consp view)
               (setf rest-args
                     (list* :view (apply #'make-instance view) rest-args))))
        ;; Presentation type history interaction. According to the spec,
        ;; if provide-default is true, we take the default from the
        ;; presentation history. In addition, we'll implement the Genera
        ;; behavior of temporarily putting the default on the history
        ;; stack so the user can conveniently suck it in.
        (flet ((do-accept (args)
                 (apply #'stream-accept stream real-type args))
               (get-history ()
                 (when real-history-type
                   (funcall-presentation-generic-function
                    presentation-type-history-for-stream
                    real-history-type stream))))
          (let* ((default-from-history (and (not defaultp) provide-default))
                 (history (get-history))
                 (results
                  (multiple-value-list 
                   (if history
                       (let ((*active-history-type* real-history-type))
                         (cond (defaultp
                                (with-object-on-history
                                    (history default real-default-type)
                                  (do-accept rest-args)))
                               (default-from-history
                                (multiple-value-bind
                                      (history-default history-type)
                                    (presentation-history-head history
                                                               real-default-type)
                                  (do-accept (if history-type
                                                 (list* :default history-default
                                                        :default-type history-type
                                                        rest-args)
                                                 rest-args))))
                               (t (do-accept rest-args))))
                       (do-accept rest-args))))
                 (results-history (get-history)))
            (when results-history
              (presentation-history-add results-history
                                        (car results)
                                        (cadr results)))
            (values-list results)))))))

(defgeneric stream-accept (stream type
			   &key
			   view
			   default
			   default-type
			   provide-default
			   insert-default
			   replace-input
			   history
			   active-p
			   prompt
			   prompt-mode
			   display-default
			   query-identifier
			   activation-gestures
			   additional-activation-gestures
			   delimiter-gestures
			   additional-delimiter-gestures))

(defmethod stream-accept ((stream standard-extended-input-stream) type
			  &rest args
			  &key (view (stream-default-view stream))
			  &allow-other-keys)
  (apply #'prompt-for-accept stream type view args)
  (apply #'accept-1 stream type args))

(defmethod stream-accept ((stream standard-input-editing-stream) type
			  &rest args
			  &key (view (stream-default-view stream))
			  &allow-other-keys)
  (apply #'prompt-for-accept stream type view args)
  (apply #'accept-1 stream type args))

(defmethod stream-accept ((stream #.*string-input-stream-class*) type
			  &key (view (stream-default-view stream))
			  (default nil defaultp)
			  (default-type nil default-type-p)
			  (activation-gestures nil activationsp)
			  (additional-activation-gestures
			   nil additional-activations-p)
			  (delimiter-gestures nil delimitersp)
			  (additional-delimiter-gestures
			   nil additional-delimiters-p)
			  &allow-other-keys)
  (with-activation-gestures ((if additional-activations-p
				 additional-activation-gestures
				 activation-gestures)
			     :override activationsp)
    (with-delimiter-gestures ((if additional-delimiters-p
				  additional-delimiter-gestures
				  delimiter-gestures)
			      :override delimitersp)
      (multiple-value-bind (object object-type)
	  (apply-presentation-generic-function
	   accept
	   type stream view
	   `(,@(and defaultp `(:default ,default))
	     ,@(and default-type-p `(:default-type ,default-type))))
	(values object (or object-type type))))))

(defun accept-1 (stream type &key
		 (view (stream-default-view stream))
		 (default nil defaultp)
		 (default-type nil default-type-p)
		 provide-default
		 insert-default
		 (replace-input t)
		 history
		 active-p
		 prompt
		 prompt-mode
		 display-default
		 query-identifier
		 (activation-gestures nil activationsp)
		 (additional-activation-gestures nil additional-activations-p)
		 (delimiter-gestures nil delimitersp)
		 (additional-delimiter-gestures nil  additional-delimiters-p))
  (declare (ignore provide-default history active-p
		   prompt prompt-mode
		   display-default query-identifier))
  (when (and defaultp (not default-type-p))
    (error ":default specified without :default-type"))
  (when (and activationsp additional-activations-p)
    (error "only one of :activation-gestures or ~
            :additional-activation-gestures may be passed to accept."))
  (unless (or activationsp additional-activations-p *activation-gestures*)
    (setq activation-gestures *standard-activation-gestures*))
  (let ((sensitizer-object nil)
	(sensitizer-type nil))
    (with-input-editing
	(stream
	 :input-sensitizer #'(lambda (stream cont)
			       (with-output-as-presentation
				   (stream sensitizer-object sensitizer-type)
				 (declare (ignore stream))
				 (funcall cont))))
      (with-input-position (stream)	; support for calls to replace-input
        (when insert-default
          ;; Insert the default value to the input stream. It should
          ;; become fully keyboard-editable.
          (presentation-replace-input stream
                                       default
                                       default-type
                                       view))
	(setf (values sensitizer-object sensitizer-type)
	      (with-input-context (type)
		  (object object-type event options)
		(with-activation-gestures ((if additional-activations-p
					       additional-activation-gestures
					       activation-gestures)
					   :override activationsp)
		  (with-delimiter-gestures ((if additional-delimiters-p
						additional-delimiter-gestures
						delimiter-gestures)
					    :override delimitersp)
		    (let ((accept-results nil))
		      (handle-empty-input (stream)
			  (setq accept-results
				(multiple-value-list
				 (if defaultp
                                     (funcall-presentation-generic-function
                                      accept type stream view
                                      :default default
                                      :default-type default-type)
				     (funcall-presentation-generic-function
				      accept type stream view))))
			;; User entered activation or delimeter
			;; gesture without any input.
			(if defaultp
			    (progn
			      (presentation-replace-input
			       stream default default-type view :rescan nil))
			    (simple-parse-error
			     "Empty input for type ~S with no supplied default"
			     type))
			(setq accept-results (list default default-type)))
		      ;; Eat trailing activation gesture
		      ;; XXX what about pointer gestures?
		      ;; XXX and delimiter gestures?
		      (unless *recursive-accept-p*
			(let ((ag (read-char-no-hang stream nil stream t)))
			  (unless (or (null ag) (eq ag stream))
			    (unless (activation-gesture-p ag)
			      (unread-char ag stream)))))
		      (values (car accept-results) (if (cdr accept-results)
						       (cadr accept-results)
						       type)))))
		;; A presentation was clicked on, or something
		(t
		 (when (and replace-input
			    (getf options :echo t)
			    (not (stream-rescanning-p stream)))
		   (presentation-replace-input stream object object-type view
					       :rescan nil))
		 (values object object-type))))
	;; Just to make it clear that we're returning values
	(values sensitizer-object sensitizer-type)))))

(defgeneric prompt-for-accept (stream type view &rest accept-args &key))

(defmethod prompt-for-accept ((stream t)
			      type view
			      &rest accept-args
			      &key &allow-other-keys)
  (declare (ignore view))
  (apply #'prompt-for-accept-1 stream type accept-args))

(defun prompt-for-accept-1 (stream type
			    &key
			    (default nil defaultp)
			    (default-type type)
                            (insert-default nil)
			    (prompt t)
			    (prompt-mode :normal)
			    (display-default prompt)
			    &allow-other-keys)
  (flet ((display-using-mode (stream prompt default)
	   (ecase prompt-mode
	     (:normal
	      (if *recursive-accept-p*
		  (input-editor-format stream "(~A~@[[~A]~]) " prompt default)
		  (input-editor-format stream "~A~@[[~A]~]: " prompt default)))
	     (:raw
	      (input-editor-format stream "~A" prompt)))))
    (let ((prompt-string (if (eq prompt t)
			     (format nil "~:[Enter ~;~]~A"
				     *recursive-accept-p*
				     (describe-presentation-type type nil nil))
			     prompt))
          ;; Don't display the default in the prompt if it is to be
          ;; inserted into the input stream.
	  (default-string (and defaultp
				(not insert-default)
				display-default
				(present-to-string default default-type))))
      (cond ((null prompt)
	   nil)
	  (t
	   (display-using-mode stream prompt-string default-string))))))

(defmethod prompt-for-accept ((stream #.*string-input-stream-class*)
			      type view
			      &rest other-args
			      &key &allow-other-keys)
			      
  (declare (ignore type view other-args))
  nil)

;;; XXX This needs work! It needs to do everything that accept does for
;;; expanding ptypes and setting up recursive call processing
(defun accept-from-string (type string
			   &rest args
			   &key view
			   (default nil defaultp)
			   (default-type nil default-type-p)
			   activation-gestures additional-activation-gestures
			   delimiter-gestures additional-delimiter-gestures
			   (start 0)
			   (end (length string)))
  (declare (ignore view activation-gestures
		   additional-activation-gestures 
		   delimiter-gestures additional-delimiter-gestures))
  (with-activation-gestures ((if additional-activations-p
				 additional-activation-gestures
				 activation-gestures)
			     :override activationsp)
    (with-delimiter-gestures ((if additional-delimiters-p
				  additional-delimiter-gestures
				  delimiter-gestures)
			      :override delimitersp)))
  (when (or (zerop (- end start))
	    (let ((maybe-end))))
    (if defaultp
	(return-from accept-from-string (values default
						(if default-type-p
						    default-type
						    type)
						0))
	(simple-parse-error "Empty string")))
  (let ((index 0))
    (multiple-value-bind (val ptype)
	(with-input-from-string (stream string :start start :end end
				 :index index)
	  (with-keywords-removed (args (:start :end))
	    (apply #'stream-accept stream type :view +textual-view+ args)))
      (values val ptype index))))

(define-presentation-generic-function %presentation-refined-position-test
    presentation-refined-position-test
  (type-key parameters options type record x y))

(define-default-presentation-method presentation-refined-position-test
    (type record x y)
  (declare (ignore type))
  ;;; output-record-hit-detection-rectangle* has already been called
  (let ((single-box (presentation-single-box record)))
    (if (or (eq single-box t) (eq single-box :position))
	t
	(labels ((tester (record)
		   (typecase record
		     (displayed-output-record
		      (return-from presentation-refined-position-test t))
		     (compound-output-record
		      (map-over-output-records-containing-position
		       #'tester record x y))
		     (t nil))))
	  (tester record)
	  nil))))

(defun presentation-contains-position (record x y)
  (let ((single-box (presentation-single-box record)))
    (multiple-value-bind (min-x min-y max-x max-y)
	(output-record-hit-detection-rectangle* record)
      (if (and (<= min-x x max-x) (<= min-y y max-y))
	  (if (or (null single-box) (eq single-box :highlighting))
	      (funcall-presentation-generic-function
	       presentation-refined-position-test
	       (presentation-type record) record x y)
	      t)
	  nil))))

(define-presentation-generic-function %highlight-presentation
    highlight-presentation
  (type-key parameters options type record stream state))

;;; Internal function to highlight just one presentation

(defun highlight-presentation-1 (presentation stream state)
  (with-output-recording-options (stream :record nil)
    (funcall-presentation-generic-function highlight-presentation
					   (presentation-type presentation)
					   presentation
					   stream
					   state)))

(define-default-presentation-method highlight-presentation
    (type record stream state)
  (declare (ignore type))
  (if (or (eq (presentation-single-box record) t)
	  (eq (presentation-single-box record) :highlighting))
      (highlight-output-record-rectangle record stream state)
      (labels ((highlighter (record)
		 (typecase record
		   (displayed-output-record
		    (highlight-output-record record stream state))
		   (compound-output-record
		    (map-over-output-records #'highlighter record))
		   (t nil))))
	(highlighter record))))


(define-default-presentation-method present
    (object type stream (view textual-view) &key acceptably for-context-type)
  (declare (ignore for-context-type type))
  (if acceptably
      (let ((*print-readably* t))
	(prin1 object stream))
      (princ object stream)))


(defun accept-using-read (stream ptype &key ((:read-eval *read-eval*) nil))
  (let* ((token (read-token stream)))
    (let ((result (handler-case (read-from-string token)
		    (error (c)
		      (declare (ignore c))
		      (simple-parse-error "Error parsing ~S for presentation type ~S"
					  token
					  ptype)))))
      (if (presentation-typep result ptype)
	  (values result ptype)
	  (input-not-of-required-type result ptype)))))

(defun accept-using-completion (type stream func
				&rest complete-with-input-key-args)
  "A wrapper around complete-with-input that returns the presentation type with
  the completed object."
  (multiple-value-bind (object success input)
      (apply #'complete-input stream func complete-with-input-key-args)
    (if success
	(values object type)
	(simple-parse-error "Error parsing ~S for presentation type ~S"
			    input
			    type))))
  
;;; When no accept method has been defined for a type, allow some kind of
;;; input.  The accept can be satisfied with pointer input, of course, and this
;;; allows the clever user a way to input the type at the keyboard, using #. or
;;; some other printed representation.
;;;
;;; XXX Once we "go live" we probably want to disable this, probably with a
;;; beep and warning that input must be clicked on.

(define-default-presentation-method accept
    (type stream (view textual-view) &key default default-type)
  (declare (ignore default default-type))
  (accept-using-read stream type :read-eval t))

;;; The presentation types

(define-presentation-method presentation-typep (object (type t))
  (declare (ignore object))
  t)

(define-presentation-method present (object (type t) 
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (let ((*print-readably* acceptably))
    (princ object stream)))

(define-presentation-type nil ())

(define-presentation-method presentation-typep (object (type nil))
  (declare (ignore object))
  nil)

(define-presentation-type null ()
  :inherit-from t)

(define-presentation-method presentation-typep (object (type null))
  (eq object nil))

(define-presentation-method present (object (type null)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore object acceptably for-context-type))
  (write-string "None" stream))

(define-presentation-method accept ((type null) stream (view textual-view)
				    &key)
  (values (completing-from-suggestions (stream)
	    (suggest "None" nil)
	    (suggest "" nil))))

(define-presentation-type boolean ()
  :inherit-from t)

(define-presentation-method presentation-typep (object (type boolean))
  (or (eq object t) (eq object nil)))

(define-presentation-method present (object (type boolean) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (if object
      (write-string "Yes" stream)
      (write-string "No" stream)))

(define-presentation-method accept ((type boolean) stream (view textual-view)
				    &key)
  (accept-using-completion 'boolean
			   stream
			   #'(lambda (input-string mode)
			       (complete-from-possibilities
				input-string
				'(("yes" t) ("no" nil))
				nil
				:action mode))))

(define-presentation-type symbol ()
  :inherit-from 't)

(define-presentation-method presentation-typep (object (type symbol))
  (symbolp object))

(define-presentation-method present (object (type symbol) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (if acceptably
      (prin1 object stream)
      (princ object stream)))

(define-presentation-method accept ((type symbol) stream (view textual-view)
				    &key)
  (accept-using-read stream type))
  
(define-presentation-type keyword () :inherit-from 'symbol)

(define-presentation-method presentation-typep (object (type keyword))
  (keywordp object))

(define-presentation-method present (object (type keyword) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (prin1 object stream))

(defmethod presentation-type-of ((object symbol))
  (if (eq (symbol-package object) (find-package :keyword))
      'keyword
      'symbol))

(define-presentation-type blank-area ()
  :inherit-from t)

(define-presentation-method highlight-presentation ((type blank-area)
						    record
						    stream
						    state)
  (declare (ignore record stream state))
  nil)

;;; Do other slots of this have to be bound in order for this to be useful?
;;; Guess we'll see.
(defparameter *null-presentation* (make-instance 'standard-presentation
						 :object nil
						 :type 'blank-area
						 :view +textual-view+))

(define-presentation-type number ()
  :inherit-from 't)

(define-presentation-method presentation-typep (object (type number))
  (numberp object))

(defmethod presentation-type-of ((object number))
  'number)

(define-presentation-type complex (&optional (type 'real))
  :inherit-from 'number)

(define-presentation-method presentation-typep (object (type complex))
  (and (complexp object)
       (typep (realpart object) type)
       (typep (imagpart object) type)))

(define-presentation-method presentation-subtypep ((type complex)
						   maybe-supertype)
  (with-presentation-type-parameters (complex type)
    (let ((component-type type))	;i.e., the parameter named "type"
      (with-presentation-type-parameters (complex maybe-supertype)
	(let ((super-component-type type))
	  (presentation-subtypep component-type super-component-type))))))


(defmethod presentation-type-of ((object complex))
  'complex)

(define-presentation-method present (object (type complex) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (present (realpart object) (presentation-type-of (realpart object))
	   :stream stream :view view :sensitive nil)
  (write-char #\Space stream)
  (present (imagpart object) (presentation-type-of (imagpart object))
	   :stream stream :view view :sensitive nil))

(define-presentation-type real (&optional low high) :options ((base 10) radix)
			  :inherit-from 'number)

(define-presentation-method presentation-typep (object (type real))
  (and (realp object)
       (or (eq low '*)
	   (<= low object))
       (or (eq high '*)
	   (<= object high))))

(defmethod presentation-type-of ((object real))
  'real)

(define-presentation-method present (object (type real) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((*print-base* base)
	(*print-radix* radix))
    (princ object stream)))

(define-presentation-method accept ((type real) stream (view textual-view)
				    &key)
  (let ((*read-base* base))
    (accept-using-read stream type)))

;;; Define a method that will do the comparision for all real types.  It's
;;; already determined that that the numeric class of type is a subtype of
;;;supertype.

(defun number-subtypep (low high super-low super-high)
  (if (eq low '*)
      (unless (eq super-low '*)
	(return-from number-subtypep nil))
      (unless (or (eq super-low '*) (>= low super-low))
	(return-from number-subtypep nil)))
  (if (eq high '*)
      (unless (eq super-high '*)
	(return-from number-subtypep nil))
      (unless (or (eq super-high '*) (<= high super-high))
	(return-from number-subtypep nil)))
  t)

(define-presentation-type rational (&optional low high)
  :options ((base 10) radix)
  :inherit-from `((real ,low ,high) :base ,base :radix ,radix))

(define-presentation-method presentation-typep (object (type rational))
  (and (rationalp object)
       (or (eq low '*)
	   (<= low object))
       (or (eq high '*)
	   (<= object high))))

(defmethod presentation-type-of ((object rational))
  'rational)

(define-presentation-method present (object (type rational) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((*print-base* base)
	(*print-radix* radix))
    (princ object stream)))

(define-presentation-type integer (&optional low high)
  :options ((base 10) radix)
  :inherit-from `((rational ,low ,high) :base ,base :radix ,radix))

(define-presentation-method presentation-typep (object (type integer))
  (and (integerp object)
       (or (eq low '*)
	   (<= low object))
       (or (eq high '*)
	   (<= object high))))

(defmethod presentation-type-of ((object integer))
  'integer)

(define-presentation-method present (object (type integer) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((*print-base* base)
	(*print-radix* radix))
    (princ object stream)))

(define-presentation-type ratio (&optional low high)
  :options ((base 10) radix)
  :inherit-from `((rational ,low ,high) :base ,base :radix ,radix))

(define-presentation-method presentation-typep (object (type ratio))
  (and (not (integerp object))
       (rationalp object)
       (or (eq low '*)
	   (<= low object))
       (or (eq high '*)
	   (<= object high))))

(defmethod presentation-type-of ((object ratio))
  'ratio)

(define-presentation-method present (object (type ratio) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((*print-base* base)
	(*print-radix* radix))
    (princ object stream)))

(define-presentation-type float (&optional low high)
  :options ((base 10) radix)
  :inherit-from `((real ,low ,high) :base ,base :radix ,radix))

(define-presentation-method presentation-typep (object (type float))
  (and (floatp object)
       (or (eq low '*)
	   (<= low object))
       (or (eq high '*)
	   (<= object high))))

(defmethod presentation-type-of ((object float))
  'float)

(define-presentation-method present (object (type float) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((*print-base* base)
	(*print-radix* radix))
    (princ object stream)))

(macrolet ((frob (num-type)
	     `(define-presentation-method presentation-subtypep ((type
								  ,num-type)
								 maybe-supertype)
	        (with-presentation-type-parameters (,num-type maybe-supertype)
		  (let ((super-low low)
			(super-high high))
		    (with-presentation-type-parameters (,num-type type)
		      (values (number-subtypep low high super-low super-high)
			      t)))))))
  (frob real)
  (frob rational)
  (frob ratio)
  (frob float))

(define-presentation-type character ()
  :inherit-from 't)

(define-presentation-method presentation-typep (object (type character))
  (characterp object))

(defmethod presentation-type-of ((object character))
  'character)

(define-presentation-method present (object (type character) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (princ object stream))

(define-presentation-type string (&optional length)
  :inherit-from 't)

(define-presentation-method presentation-typep (object (type string))
  (and (stringp object)
       (or (eq length '*) (eql (length object) length))))

(define-presentation-method presentation-subtypep ((type string)
						   maybe-supertype)
  (with-presentation-type-parameters (string maybe-supertype)
    (let ((super-length length))
      (with-presentation-type-parameters (string type)
	(values (or (eq super-length '*)
		    (eql length super-length))
		t)))))

;;; `(string ,length) would be more specific, but is not "likely to be useful
;;; to the programmer."

(defmethod presentation-type-of ((object string))
  'string)

(define-presentation-method present (object (type string) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (princ object stream))

(define-presentation-method accept ((type string) stream (view textual-view)
				    &key (default nil defaultp)
				    (default-type type))
  (let ((result (read-token stream)))
    (cond ((numberp length)
	   (if (eql length (length result))
	       (values result type)
	       (input-not-of-required-type result type)))
	  ((and (zerop (length result)) defaultp)
	   (values default default-type))
	  (t (values result type)))))

(define-presentation-type pathname ()
  :options ((default-version :newest) default-type (merge-default t))
  :inherit-from 't)

(define-presentation-method presentation-typep (object (type pathname))
  (pathnamep object))

(defmethod presentation-type-of ((object pathname))
  'pathname)

(define-presentation-method present (object (type pathname) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (princ object stream))

(define-presentation-method accept 
    ((type pathname) stream (view textual-view)
     &key (default *default-pathname-defaults*))
  (let* ((namestring (read-token stream))
	 (path (parse-namestring namestring)))
    (if merge-default
	(merge-pathnames 
	 path 
	 (merge-pathnames (make-pathname :type default-type
					 :version default-version)
			  default))
	path)))

(defgeneric default-completion-name-key (item))

(defmethod default-completion-name-key ((item string))
  item)

(defmethod default-completion-name-key ((item null))
  "NIL")

(defmethod default-completion-name-key ((item cons))
  (string (car item)))

(defmethod default-completion-name-key ((item symbol))
  (string-capitalize (symbol-name item)))

(defmethod default-completion-name-key (item)
  (princ-to-string item))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +completion-options+
    '((name-key 'default-completion-name-key)
      documentation-key
      (partial-completers '(#\Space)))))

(define-presentation-type completion (sequence
				      &key (test 'eql) (value-key 'identity))
  :options #.+completion-options+
  :inherit-from t)

(define-presentation-method presentation-typep (object (type completion))
  (map nil #'(lambda (obj)
	       (when (funcall test object (funcall value-key obj))
		 (return-from presentation-typep t)))
       sequence)
  nil)

;;; Useful for subtype comparisons for several of the "member" style types

(defun sequence-subset-p (seq1 test1 value-key1 seq2 test2 value-key2)
  (let ((test-fun (if (eq test1 test2)
		      test1
		      ;; The object has to pass both type's equality test
		      #'(lambda (obj1 obj2)
			  (and (funcall test1 obj1 obj2)
			       (funcall test2 obj1 obj2))))))
    (map nil #'(lambda (type-obj)
		 (unless (find (funcall value-key1 type-obj)
			       seq2
			       :test test-fun :key value-key2)
		   (return-from sequence-subset-p nil)))
	 seq1)
    t))

(define-presentation-method presentation-subtypep ((type completion)
						   maybe-supertype)
  (with-presentation-type-parameters (completion maybe-supertype)
    (let ((super-sequence sequence)
	  (super-test test)
	  (super-value-key value-key))
      (with-presentation-type-parameters (completion type)
	(values (sequence-subset-p sequence test value-key
				   super-sequence super-test super-value-key)
		t)))))

(define-presentation-method present (object (type completion) stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((obj-pos (position object sequence :test test :key value-key)))
    (if obj-pos
	(write-string (funcall name-key (elt sequence obj-pos)) stream)
	;; Should define a condition type here.
	(error "~S is not of presentation type ~S" object type))))

(define-presentation-method accept ((type completion)
				    stream
				    (view textual-view)
				    &key)
  (accept-using-completion (make-presentation-type-specifier
			    `(completion ,@parameters)
			    options)
			   stream
			   #'(lambda (input-string mode)
			       (complete-from-possibilities
				input-string
				sequence
				partial-completers
				:action mode
				:name-key name-key
				:value-key value-key))
			   :partial-completers partial-completers))

(define-presentation-type-abbreviation member (&rest elements)
  (make-presentation-type-specifier `(completion ,elements)
				    :name-key name-key
				    :documentation-key documentation-key
				    :partial-completers partial-completers)
  :options #.+completion-options+)

(define-presentation-type-abbreviation member-sequence (sequence
							&key (test 'eql testp))
  (make-presentation-type-specifier
   `(completion ,sequence ,@(and testp `(:test ,test)))
   :name-key name-key
   :documentation-key documentation-key
   :partial-completers partial-completers)
  :options #.+completion-options+)

(defun member-alist-value-key (element)
  (cond ((atom element)
	 element)
	((atom (cdr element))
	 (cdr element))
	((null (cddr element))
	 (cadr element))
	(t (getf (cdr element) :value))))

(defun member-alist-doc-key (element)
  (if (and (consp element) (consp (cdr element)) (consp (cddr element)))
      (getf (cdr element) :documentation)))

(define-presentation-type-abbreviation member-alist (alist
						     &key (test 'eql testp))
  (make-presentation-type-specifier
   `(completion ,alist ,@(and testp `(:test ,test))
		:value-key member-alist-value-key)
   :name-key name-key
   :documentation-key documentation-key
   :partial-completers partial-completers)
  :options ((name-key 'default-completion-name-key)
	    (documentation-key 'member-alist-doc-key)
	    (partial-completers '(#\Space))))

(define-presentation-type subset-completion (sequence
					     &key (test 'eql)
					     (value-key 'identity))
  :options ((name-key 'default-completion-name-key)
	    documentation-key
	    (partial-completers '(#\Space))
	    (separator #\,)
	    (echo-space t))
  :inherit-from t)

(define-presentation-method presentation-typep (object
						(type subset-completion))
  (map nil #'(lambda (obj)
	       (unless (find obj sequence :test test :key value-key)
		 (return-from presentation-typep nil)))
       object)
  t)

(define-presentation-method presentation-subtypep ((type subset-completion)
						   maybe-supertype)
  (with-presentation-type-parameters (subset-completion maybe-supertype)
    (let ((super-sequence sequence)
	  (super-test test)
	  (super-value-key value-key))
      (with-presentation-type-parameters (subset-completion type)
	(values (sequence-subset-p sequence test value-key
				   super-sequence super-test super-value-key)
		t)))))

(define-presentation-method present ((object list) (type subset-completion)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for tail on object
	for (obj) = tail
	do (progn
	     (present obj (presentation-type-of object)
			:stream stream :view view
			:acceptably acceptably
			:sensitive nil)
	     (when (cdr tail)
	       (if acceptably
		   (princ separator stream)
		   (terpri stream))))))

(define-presentation-method present ((object vector) (type subset-completion)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for i from 0 below (length object)
	for obj = (aref object i)
	do (progn
	     (present obj (presentation-type-of object)
			:stream stream :view view
			:acceptably acceptably
			:sensitive nil)
	     (when (< i (1- (length object)))
	       (if acceptably
		   (princ separator stream)
		   (terpri stream))))))

;;; XXX is it a typo in the spec that subset, subset-sequence and subset-alist
;;; have the same options as completion, and not subset-completion?

(define-presentation-type-abbreviation subset (&rest elements)
  (make-presentation-type-specifier `(subset-completion ,elements)
				    :name-key name-key
				    :documentation-key documentation-key
				    :partial-completers partial-completers)
  :options #.+completion-options+)

(define-presentation-type-abbreviation subset-sequence (sequence
							&key (test 'eql testp))
  (make-presentation-type-specifier
   `(subset-completion ,sequence ,@(and testp `(:test ,test)))
   :name-key name-key
   :documentation-key documentation-key
   :partial-completers partial-completers)
  :options #.+completion-options+)

(define-presentation-type-abbreviation subset-alist (alist
						     &key (test 'eql testp))
  (make-presentation-type-specifier
   `(subset-completion ,@(and testp `(:test ,test))
                       :value-key member-alist-value-key)
   :name-key name-key
   :documentation-key documentation-key
   :partial-completers partial-completers)
  :options ((name-key 'default-completion-name-key)
	    (documentation-key 'member-alist-doc-key)
	    (partial-completers '(#\Space))))

(define-presentation-type sequence (type)
  :options ((separator #\,) (echo-space t))
  :inherit-from 't
  :parameters-are-types t)

(define-presentation-method presentation-typep (object (type sequence))
  ;; XXX TYPE here is the sequence element type, not the whole type specifier
  (unless (or (listp object) (vectorp object))
    (return-from presentation-typep nil))
  (let ((real-type (expand-presentation-type-abbreviation type)))
    (map nil #'(lambda (obj)
		 (unless (presentation-typep obj real-type)
		   (return-from presentation-typep nil)))
	 object)
    t))

(define-presentation-method presentation-subtypep ((type sequence)
						   maybe-supertype)
  (with-presentation-type-parameters (sequence type)
    ;; now TYPE is bound to the parameter TYPE
    (let ((real-type (expand-presentation-type-abbreviation type)))
      (with-presentation-type-parameters (sequence maybe-supertype)
	(let ((real-super-type (expand-presentation-type-abbreviation type)))
	  (presentation-subtypep real-type real-super-type))))))

(defmethod presentation-type-of ((object cons))
  '(sequence t))

;;; Do something interesting with the array-element-type
(defmethod presentation-type-of ((object vector))
  '(sequence t))

(define-presentation-method present ((object list) (type sequence)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for tail on object
	for (obj) = tail
	do (progn
	     (present obj type		; i.e., the type parameter
			:stream stream :view view
			:acceptably acceptably
			:sensitive nil)
	     (when (cdr tail)
	       (write-char separator stream)))))

(define-presentation-method present ((object vector) (type sequence)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for i from 0 below (length object)
	for obj = (aref object i)
	do (progn
	     (present obj type		; i.e., the type parameter
			:stream stream :view view
			:acceptably acceptably
			:sensitive nil)
	     (when (< i (1- (length object)))
	       (write-char separator stream)))))


(define-presentation-method accept ((type sequence)
				    stream
				    (view textual-view)
				    &key)
  (loop
     with separators = (list separator)
     for element = (accept type		; i.e., the type parameter
			   :stream stream
			   :view view
			   :prompt nil
			   :additional-delimiter-gestures separators)
     collect element
     do (progn
	  (when (not (eql (peek-char nil stream nil nil) separator))
	    (loop-finish))
	  (read-char stream)
	  (when echo-space
	    ;; Make the space a noise string
	    (input-editor-format stream " ")))))


(define-presentation-type sequence-enumerated (&rest types)
  :options ((separator #\,) (echo-space t))
  :inherit-from 't
  :parameters-are-types t)

(define-presentation-method presentation-typep (object
						(type sequence-enumerated))
  (unless (or (listp object) (vectorp object))
    (return-from presentation-typep nil))
  (map nil #'(lambda (obj type)
	       (let ((real-type (expand-presentation-type-abbreviation type)))
		 (unless (presentation-typep obj real-type)
		   (return-from presentation-typep nil))))
       object
       types)
  t)

(define-presentation-method presentation-subtypep ((type sequence-enumerated)
						   maybe-supertype)
  (with-presentation-type-parameters (sequence-enumerated maybe-supertype)
    (let ((supertypes types))
      (with-presentation-type-parameters (sequence-enumerated type)
	(unless (eql (length supertypes) (length types))
	  (return-from presentation-subtypep (values nil t)))
	(map nil
	     #'(lambda (element-type element-supertype)
		 (let ((real-type (expand-presentation-type-abbreviation
				   element-type))
		       (real-supertype (expand-presentation-type-abbreviation
					element-supertype)))
		   (multiple-value-bind (subtypep determined)
		       (presentation-subtypep real-type real-supertype)
		     (cond ((not determined)
			    (return-from presentation-subtypep
			      (values nil nil)))
			   ((not subtypep)
			    (return-from presentation-subtypep
			      (values nil t)))))))
	     types
	     supertypes)
	(values t t)))))

(define-presentation-method present ((object list) (type sequence-enumerated)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for tail on object
	for (obj) = tail
	for obj-type in types
	do (progn
	     (present obj obj-type
			:stream stream :view view
			:acceptably acceptably
			:sensitive nil)
	     (when (cdr tail)
	       (if acceptably
		   (princ separator stream)
		   (terpri stream))))))

(define-presentation-method present ((object vector) (type sequence-enumerated)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (loop for i from 0 below (length object)
	for obj = (aref object i)
	for obj-type in types
	do (progn
	     (present obj obj-type
			:stream stream :view view
			:acceptably acceptably
			:sensitive nil)
	     (when (< i (1- (length object)))
	       (if acceptably
		   (princ separator stream)
		   (terpri stream))))))

(define-presentation-method accept ((type sequence-enumerated)
				    stream
				    (view textual-view)
				    &key)
  (loop
     with element = nil and element-type = nil
       and separators = (list separator)
     for type-tail on types
     for (this-type) = type-tail
     do (setf (values element element-type)
	      (accept this-type
		      :stream stream
		      :view view
		      :prompt t
		      :display-default nil
		      :additional-delimiter-gestures separators))
     collect element into sequence-val
     do (progn
	  (when (not (eql (peek-char nil stream nil nil) separator))
	    (loop-finish))
	  (read-char stream)
	  (when echo-space
	    ;; Make the space a noise string
	    (input-editor-format stream " ")))
     finally (if (cdr type-tail)
		 (simple-parse-error "Input ~S too short for ~S."
				     sequence-val
				     types)
		 (return sequence-val))))

(define-presentation-type or (&rest types)
  :inherit-from t
  :parameters-are-types t)

(define-presentation-method presentation-typep (object (type or))
  (loop for type in types
	for real-type = (expand-presentation-type-abbreviation type)
	do (when (presentation-typep object real-type)
	     (return-from presentation-typep t)))
  nil)

(define-presentation-method present (object (type or)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (loop for or-type in types
	for expanded-type = (expand-presentation-type-abbreviation or-type)
	do (when (presentation-typep object expanded-type)
	     (present object expanded-type
		      :stream stream :view view
		      :acceptably acceptably
		      :for-context-type for-context-type)
	     (loop-finish))))

(define-presentation-method accept ((type or)
				    (stream input-editing-stream)
				    (view textual-view)
				    &key)
  (let ((scan-begin (stream-scan-pointer stream)))
    (loop for or-type in types
	  do (handler-case (return (accept or-type
					   :stream stream
					   :view view
					   :prompt nil))
	       (parse-error () (setf (stream-scan-pointer stream) scan-begin)))
	  finally (simple-parse-error "Input type is not one of ~S" types))))

;;; What does and inherit from?  Maybe we'll punt on that for the moment.
;;; Unless it inherits from its arguments...

(define-presentation-type and (&rest types)
  :parameters-are-types t)

(define-presentation-method presentation-typep (object (type and))
  (loop for type in types
	for real-type = (expand-presentation-type-abbreviation type)
	do (with-presentation-type-decoded (name parameters)
	     real-type
	     (cond ((eq name 'satisfies)
		    (unless (funcall (car parameters) object)
		      (return-from presentation-typep nil)))
		   ((eq name 'not)
		    (unless (not (presentation-typep object (car parameters)))
		      (return-from presentation-typep nil)))
		   (t (unless (presentation-typep object real-type)
			(return-from presentation-typep nil))))))
  t)

(define-presentation-method present (object (type and)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (present object (expand-presentation-type-abbreviation (car types))
	   :stream stream :view view
	   :acceptably acceptably
	   :for-context-type for-context-type))

(define-presentation-type-abbreviation token-or-type (tokens type)
  `(or (member-alist ,tokens) ,type))

(define-presentation-type-abbreviation null-or-type (type)
  `(or null ,type))

(define-presentation-type-abbreviation type-or-string (type)
  `(or ,type string))

(define-presentation-method presentation-typep (object (type expression))
  (declare (ignore object))
  t)

(define-presentation-method present (object (type expression)
				     stream
				     (view textual-view)
				     &key acceptably for-context-type)
  (declare (ignore for-context-type))
  (let ((*print-readably* acceptably))
    (prin1 object stream)))

(define-presentation-generic-function %accept-present-default
    accept-present-default
  (type-key parameters options type
   stream view default default-supplied-p present-p query-identifier))

;;; All the expression and form reading stuff is in builtin-commands.lisp

;;; drag-n-drop fun

(defclass drag-n-drop-translator (presentation-translator)
  ((feedback :reader feedback :initarg :feedback)
   (highlighting :reader highlighting :initarg :highlighting)))

(defvar *dragged-object* nil
  "Bound to the object dragged in a drag-and-drop context")

;;; According to the Franz User's guide, the destination object is
;;; available in the tester, documentation, and translator function
;;; as destination-object. Therefore OBJECT is the dragged object. In
;;; our scheme the tester function, translator function etc. is
;;; really called on the destination object. So, we do a little
;;; shuffling of arguments here.

(defmethod initialize-instance :after ((obj drag-n-drop-translator)
				       &key tester documentation
				       pointer-documentation
				       translator-function)
  (flet ((make-adapter (func)
	   (lambda (object &rest args)
	     (apply func *dragged-object* :destination-object object args))))
    (setf (slot-value obj 'tester) (make-adapter tester))
    (setf (slot-value obj 'documentation) (make-adapter documentation))
    (when pointer-documentation
      (setf (slot-value obj 'pointer-documentation)
	    (make-adapter pointer-documentation)))
    (setf (slot-value obj 'translator-function)
	  (make-adapter translator-function))))

(define-presentation-type drag-over (over context)
  :inherit-from t
  :parameters-are-types t)

(define-presentation-method presentation-subtypep ((type drag-over)
						   maybe-supertype)
  (with-presentation-type-parameters (drag-over type)
    (let ((subtype-over under)
	  (subtype-context context))
      (with-presentation-type-parameters (drag-over maybe-supertype)
	(and (presentation-subtypep subtype-over over)
	     (presentation-subtypep subtype-over over))))))

(defmacro define-drag-and-drop-translator
   (name (from-type to-type destination-type command-table
           &rest args &key
           (gesture :select)
           (tester 'default-translator-tester)
           documentation
           (pointer-documentation nil pointer-documentation-p)
           (menu t)
           (priority 0)
           (feedback 'frame-drag-and-drop-feedback)
           (highlighting 'frame-drag-and-drop-highlighting))
     arglist
     &body body)
  (declare (ignore tester documentation pointer-documentation pointer-documentation-p
                   menu priority))
  (let* ((real-from-type (expand-presentation-type-abbreviation from-type))
	 (real-dest-type (expand-presentation-type-abbreviation
			  destination-type))
	 (real-to-type (expand-presentation-type-abbreviation to-type))
	 (action-name (gensym (format nil "~S-~S-DRAG-ACTION"
				      from-type to-type))))
    (with-keywords-removed (args (:feedback :highlighting))
      `(progn
	 (define-presentation-translator ,name
	     (,real-dest-type (drag-over ,real-from-type ,real-to-type)
	      ,@args
	      :feedback #',feedback :highlighting #',highlighting
	      :translator-class drag-n-drop-translator)
	   ,arglist
	   ,@body)
	 (define-presentation-action ,action-name
	     (,from-type ,to-type ,command-table :gesture ,gesture
			 :priority -1)
	   (object presentation context-type frame event window x y)
	   (frame-drag ',name ',command-table object presentation context-type
		       frame event window x y))))))



