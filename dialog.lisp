;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2003 by Tim Moore (moore@bricoworks.com)
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

#| Random notes:

An accepting-values stream diverts the calls to accept into calling
accept-present-default, as described in the spec.  The output record
produced by accept-present-default, as well as the current value of
that query, arguments that were passed to accept, etc. are stored in a
query object. The stream stores all the query objects for this
invocation of accepting-values.

The query output records are presentations with command translators
defined that directly change their value (stored in the query object)
or select them for further user input, like the default text input.

After the initial output records are drawn, invoke-accepting-values
blocks accepting commands.  When a query's value is changed, the body
of the call to accepting-values is run, with all the values returned
by calls to accept coming from the query objects.

|#

(in-package :clim-internals)

(defclass query ()
  ((query-identifier :accessor query-identifier :initarg :query-identifier)
   (ptype :accessor ptype :initarg :ptype)
   (view :accessor view :initarg :view)
   (default :accessor default :initarg :default :initform nil)
   (value :accessor value :initarg :value :initform nil)
   (changedp :accessor changedp :initform nil)
   (record :accessor record :initarg :record)
   (activation-gestures :accessor activation-gestures
			:initform *activation-gestures*
			:documentation "Binding of *activation-gestures* on
entry to this accept") 
   (delimeter-gestures :accessor delimiter-gestures
		       :initform *delimiter-gestures*
		       :documentation "Binding of *delimeter-gestures* on entry
to this accept")
   (accept-arguments :accessor accept-arguments :initarg :accept-arguments)
   (accept-condition :accessor accept-condition :initarg :accept-condition
		     :initform nil
		     :documentation "Condition signalled, if any, during
accept of this query")))

(defclass accepting-values-record (standard-updating-output-record)
  ())

(defclass accepting-values-stream (standard-encapsulating-stream)
  ((queries :accessor queries :initform nil)
   (selected-query :accessor selected-query :initform nil)))

(defmethod stream-default-view ((stream accepting-values-stream))
  +textual-dialog-view+)

(define-condition av-exit (condition)
  ())

;;; The accepting-values state machine is controlled by commands. Each
;;; action (e.g., "select a text field") terminates 

(define-command-table accepting-values)	; :inherit-from nil???

(defvar *default-command* '(accepting-values-default-command))

;;; The fields of the query have presentation type query.  Fields that
;;; are "selectable", like the default text editor field, have type
;;; selectable-query.  The presentation object is the query
;;; identifier.

(define-presentation-type query () :inherit-from t)

(define-presentation-type selectable-query () :inherit-from 'query)

(define-presentation-type exit-button () :inherit-from t)

(define-presentation-type abort-button () :inherit-from t)

(defvar *accepting-values-stream* nil)

(defmacro accepting-values
    ((&optional (stream t)
      &rest args
      &key own-window exit-boxes initially-select-query-identifier
           modify-initial-query resynchronize-every-pass resize-frame
           align-prompts label scroll-bars
           x-position y-position width height command-table frame-class)
     &body body)
  (declare (ignorable own-window exit-boxes initially-select-query-identifier
            modify-initial-query resynchronize-every-pass resize-frame
            align-prompts label scroll-bars
            x-position y-position width height command-table frame-class))
  (when (eq stream 't)
    (setq stream '*standard-input*))
  (check-type stream symbol)
  (with-gensyms (accepting-values-continuation)
    `(flet ((,accepting-values-continuation (,stream)
              ,@body))
       (invoke-accepting-values ,stream
                                #',accepting-values-continuation
                                ,@args))))

(defun invoke-accepting-values
    (stream body
     &key own-window exit-boxes initially-select-query-identifier
     modify-initial-query resynchronize-every-pass resize-frame
     align-prompts label scroll-bars
     x-position y-position width height
     (command-table 'accepting-values)
     (frame-class 'accept-values))
  (let* ((*accepting-values-stream* (make-instance 'accepting-values-stream
						  :stream stream))
	 (arecord (updating-output (stream
				    :record-type 'accepting-values-record)
		    (funcall body *accepting-values-stream*)
		    (display-exit-boxes *application-frame*
					stream
					(stream-default-view
					 *accepting-values-stream*))))
	 (first-time t)
	 (current-command *default-command*))
    (letf (((frame-command-table *application-frame*)
	    (find-command-table command-table)))
      (unwind-protect
	   (handler-case
	       (loop
		(if first-time
		    (setq first-time nil)
		    (when resynchronize-every-pass
		      (redisplay arecord stream)))
		(with-input-context
		    ('(command :command-table accepting-values))
		  (object)
		  (progn
		    (apply (command-name current-command)
			   (command-arguments current-command))
		    ;; If current command returns without throwing a
		    ;; command, go back to the default command
		    (setq current-command *default-command*))
		  (t (setq current-command object)))
		(redisplay arecord stream))
	     (av-exit ()
	       (finalize-query-records *accepting-values-stream*)
	       (redisplay arecord stream)))
	(erase-output-record arecord stream)))))

(defgeneric display-exit-boxes (frame stream view))

(defmethod display-exit-boxes (frame stream (view textual-dialog-view))
  (declare (ignore frame))
  (updating-output (stream :unique-id 'buttons :cache-value t)
    (fresh-line stream)
    (with-output-as-presentation
	(stream nil 'exit-button)
      (format stream "Exit"))
    (write-char #\space stream)
    (with-output-as-presentation
	(stream nil 'abort-button)
      (format stream "Abort"))
    (terpri stream)))

(defmethod stream-accept ((stream accepting-values-stream) type
			  &rest rest-args
			  &key
			  (view (stream-default-view stream))
			  (default nil default-supplied-p)
			  default-type
			  provide-default
			  insert-default
			  replace-input
			  history
			  active-p
			  prompt
			  prompt-mode
			  display-default
			  (query-identifier prompt)
			  activation-gestures
			  additional-activation-gestures
			  delimiter-gestures
			  additional-delimiter-gestures)
  (declare (ignore activation-gestures additional-activation-gestures
		   delimiter-gestures additional-delimiter-gestures))
  (apply #'prompt-for-accept stream type view rest-args)
  (let ((query (find query-identifier (queries stream)
		     :key #'query-identifier :test #'equal)))
    (unless query
      (setq query (make-instance 'query
				 :query-identifier query-identifier
				 :ptype type
				 :view view
				 :default default
				 :value default))
      (setf (queries stream) (nconc (queries stream) (list query))))
    (setf (accept-arguments query) rest-args)
    ;; If the program changes the default, that becomes the value.
    (unless (equal default (default query)) 
      (setf (default query) default)
      (setf (value query) default))
    (let ((query-record (funcall-presentation-generic-function
			 accept-present-default
			 type (encapsulating-stream-stream stream) view
			 (value query)
			 default-supplied-p
			 nil query-identifier)))
      (setf (record query) query-record)
      (when (accept-condition query)
	(signal (accept-condition query)))
      (multiple-value-prog1
	  (values (value query) (ptype query) (changedp query))
	(setf (default query) default)
	(setf (ptype query) type)
	(setf (changedp query) nil)))))

(defmethod prompt-for-accept ((stream accepting-values-stream)
			      type view
			      &rest args)
  (declare (ignore view))
  (apply #'prompt-for-accept-1 stream type :display-default nil args))

(define-command (com-query-exit :command-table accepting-values :name nil)
    ()
  (signal 'av-exit))

(define-command (com-query-abort :command-table accepting-values :name nil)
    ()
  (and (find-restart 'abort)
       (invoke-restart 'abort)))

(define-command (com-change-query :command-table accepting-values :name nil)
    ((query-identifier t)
     (value t))
  (when *accepting-values-stream*
    (let ((query (find query-identifier (queries *accepting-values-stream*)
		       :key #'query-identifier :test #'equal)))
      (when query
	(setf (value query) value)
	(setf (changedp query) t)))))

(defgeneric select-query (stream query record)
  (:documentation "Does whatever is needed for input (e.g., calls accept) when
a query is selected for input." ))

(defgeneric deselect-query (stream query record)
  (:documentation "Deselect a query field: turn the cursor off, turn off
highlighting, etc." ))

(define-command (com-select-query :command-table accepting-values :name nil)
    ((query-identifier t))
  (when *accepting-values-stream*
    (with-accessors ((selected-query selected-query))
	*accepting-values-stream*
      (let* ((query-list (member query-identifier
				 (queries *accepting-values-stream*)
				 :key #'query-identifier :test #'equal))
	     (query (car query-list)))
	(when selected-query
	  (unless (equal query-identifier
			 (query-identifier selected-query)) 
	    (deselect-query *accepting-values-stream*
			    selected-query
			    (record selected-query))))
	(when query
	  (setf selected-query query)
	  (select-query *accepting-values-stream* query (record query))
	  (if (cdr query-list)
	      (throw-object-ptype (query-identifier (cadr query-list))
				  'selectable-query)
	      (throw-object-ptype '(com-deselect-query)
				  '(command :command-table accepting-values))))))))

(define-command (com-deselect-query :command-table accepting-values :name nil)
    ()
  (when *accepting-values-stream*
    (with-accessors ((selected-query selected-query))
	*accepting-values-stream*
      (when selected-query
	(deselect-query *accepting-values-stream*
			selected-query
			(record selected-query))
	(setf selected-query nil)))))

(defclass av-text-record (standard-updating-output-record)
  ((editing-stream :accessor editing-stream)
   (snapshot :accessor snapshot :initarg :snapshot :initform nil
	     :documentation "A copy of the stream buffer before accept
is called. Used to determine if any editing has been done by user")))

(define-default-presentation-method accept-present-default
    (type stream (view textual-dialog-view) default default-supplied-p
     present-p query-identifier)
  (let* ((editing-stream nil)
	 (record (updating-output (stream :unique-id query-identifier
				   :cache-value query-identifier
				   :record-type 'av-text-record)
		   (with-output-as-presentation
		       (stream query-identifier 'selectable-query)
		     (setq editing-stream
			   (make-instance 'standard-input-editing-stream
					  :stream stream
					  :cursor-visibility nil
					  :background-ink +grey90+))))))
    (when editing-stream
      (setf (editing-stream record) editing-stream))
    record))

(defun av-do-accept (query record)
  (let ((estream (editing-stream record))
	(ptype (ptype query))
	(view (view query)))
    (setf (values (value query) (ptype query)) ; Hmm, should ptype be set here?
	  (input-editing-rescan-loop
	   estream
	   #'(lambda (s)
	       (accept ptype :stream s :view view :prompt nil))))))


;;; The desired 
(defmethod select-query (stream query (record av-text-record))
  (declare (ignore stream))
  (let ((estream (editing-stream record))
	(ptype (ptype query))
	(view (view query))
	(accept-args (accept-arguments query)))
    (declare (ignore ptype view))	;for now
    (let* ((*activation-gestures* (apply #'make-activation-gestures
					 :existing-activation-gestures
					 (activation-gestures query)
					 accept-args))
	   
	   (*delimiter-gestures* (apply #'make-delimiter-gestures
					 :existing-delimiter-args
					 (delimiter-gestures query)
					 accept-args)))
      (with-accessors ((stream-activated stream-activated)
		       (stream-input-buffer stream-input-buffer))
	estream
	;; "deactivate" editing stream if user has previously activated it.
	(when stream-activated
	  (setf stream-activated nil)
	  (when (activation-gesture-p (aref stream-input-buffer
					    (1- (fill-pointer
						 stream-input-buffer))))
	    (replace-input estream ""
			   :buffer-start (1- (fill-pointer
					      stream-input-buffer))
			   :rescan t)))
	(setf (cursor-visibility estream) t)
	(setf (snapshot record) (copy-seq stream-input-buffer))
	(handler-case
	    (av-do-accept query record)
	  (condition (c)
	    (setf (accept-condition query) c)))))))


(defmethod deselect-query (stream query (record av-text-record))
  (let ((estream (editing-stream record)))
    (setf (cursor-visibility estream) nil)))

(defgeneric finalize-query-record (query record)
  (:documentation "Do any cleanup on a query before the accepting-values body
is run for the last time"))

(defmethod finalize-query-record (query record)
  nil)

;;; If the user edits a text field, selects another text field and
;;; then exits from accepting-values without activating the first
;;; field, the values returned would be some previous value or default
;;; for the field, not what's on the screen.  That would be completely
;;; bogus.  So, if a field has been edited but not activated, activate
;;; it now.  Unfortunately that's a bit hairy.

(defmethod finalize-query-record (query (record av-text-record))
  (let ((estream (editing-stream record)))
    (when (and (not (stream-activated estream))
	       (snapshot record)
	       (not (equal (snapshot record)
			   (stream-input-buffer estream))))
      (let* ((activation-gestures (apply #'make-activation-gestures
					 :existing-activation-gestures
					 (activation-gestures query)
					 (accept-arguments query)))
	     (gesture (car activation-gestures)))
	(when gesture
	  (let ((c (character-gesture-name gesture)))
	    (replace-input estream (string c)
			   :buffer-start (fill-pointer (stream-input-buffer
							estream))
			   :rescan nil)
	    (setf (stream-activated estream) t)
	    (reset-scan-pointer estream)
	    (av-do-accept query record)))))))

(defun finalize-query-records (av-stream)
  (loop for query in (queries av-stream)
	do (finalize-query-record query (record query))))


(define-presentation-to-command-translator com-select-field
    (selectable-query com-select-query accepting-values
     :gesture :select
     :documentation "Select field for input"
     :pointer-documentation "Select field for input"
     :echo nil
     :tester ((object)
	      (let ((selected (selected-query *accepting-values-stream*)))
		(or (null selected)
		    (not (eq (query-identifier selected) object))))))
  (object)
  `(,object))

(define-presentation-to-command-translator com-exit-button
    (exit-button com-query-exit accepting-values
     :gesture :select
     :documentation "Exit dialog"
     :pointer-documentation "Exit dialog"
     :echo nil)
  ()
  ())

(define-presentation-to-command-translator com-abort-button
    (abort-button com-query-abort accepting-values
     :gesture :select
     :documentation "Abort dialog"
     :pointer-documentation "Abort dialog"
     :echo nil)
  ()
  ())

(defun accepting-values-default-command ()
  (loop
   (read-gesture :stream *accepting-values-stream*)))
