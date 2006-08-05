;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2001 by 
;;;           Tim Moore (moore@bricoworks.com)

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

(in-package :clim-internals)

(defvar *activation-gestures* nil)
(defvar *standard-activation-gestures* '(:newline :return))

(defvar *delimiter-gestures* nil)

(defgeneric interactive-stream-p (stream)
  (:method (stream)
    (cl:interactive-stream-p stream)))

(defclass empty-input-mixin ()
  ()
  (:documentation "A mixin class used for detecting empty input"))

(defclass standard-input-editing-stream (empty-input-mixin
					 goatee:goatee-input-editing-mixin
					 input-editing-stream
					 standard-encapsulating-stream)
  ((buffer :reader stream-input-buffer
	   :initform (make-array 16 :adjustable t :fill-pointer 0))
   (insertion-pointer :accessor stream-insertion-pointer :initform 0)
   (scan-pointer :accessor stream-scan-pointer :initform 0)
   (rescan-queued :accessor rescan-queued :initform nil)
   (rescanning-p :reader stream-rescanning-p :initform nil)
   (activation-gesture :accessor activation-gesture :initform nil)))

;;; Markers for noise strings in the input buffer.

(defclass noise-string-property ()
  ())

(defclass noise-string-start-property (noise-string-property)
  ())

(defparameter *noise-string* (make-instance 'noise-string-property))

(defparameter *noise-string-start*
  (make-instance 'noise-string-start-property))

;;; Have to reexamine how many of the keyword arguments to stream-read-gesture
;;; should really be passed to the encapsulated stream.
;;;
;;; OK, now I know :)  They should all be passed, except for peek-p.
;;; However, the loop that calls stream-read-gesture on the
;;; encapsulated stream needs to return null if we see a :timeout or :eof.
;;;
;;; Activation gesture handling has been moved out of stream-process-gesture to
;;; stream-read-gesture and stream-unread-gesture. This allows a gesture to be
;;; read in while it is not an activation gesture, unread, and then read again
;;; as an activation gesture. This kind of game seems to be needed for reading
;;; forms properly. -- moore
#-(and)
(defmethod stream-read-gesture ((stream standard-input-editing-stream)
				&rest rest-args &key peek-p
				&allow-other-keys)
  (with-keywords-removed (rest-args (:peek-p))
    (rescan-if-necessary stream)
    (with-slots (buffer insertion-pointer scan-pointer)
	stream
      (loop
       (cond ((< scan-pointer insertion-pointer)
	      (let ((gesture (aref buffer scan-pointer)))
		(cond ((typep gesture 'noise-string-property)
		       (incf scan-pointer))
		      ;; XXX What about if peek-p is true?
		      ;; I'm thinking that accept should look for accept
		      ;; results explicitly. -- moore
		      ((and (not peek-p)
			    (typep gesture 'goatee::accept-result-extent))
		       (incf scan-pointer)
		       (throw-object-ptype (goatee::object gesture)
					   (goatee::result-type gesture)))
		      (t (unless peek-p
			   (when (activation-gesture-p gesture)
			     (setf (stream-activated stream) t))
			   (incf scan-pointer))
			 (return-from stream-read-gesture gesture)))))
	     ;; If activated, insertion pointer is at fill pointer XXX get rid
	     ;; of this
	     #+(or)
	     ((stream-activated stream)
	      (return-from stream-read-gesture (values nil :eof)))
	     (t (when (eql scan-pointer (fill-pointer buffer))
		  (setf (slot-value stream 'rescanning-p) nil))
		(loop for result = (multiple-value-bind (gesture type)
				       (apply #'stream-read-gesture
					      (encapsulating-stream-stream
					       stream)
					      rest-args)
				     (when (null gesture)
				       (return-from stream-read-gesture
					 (values gesture type)))
				     (stream-process-gesture stream
							     gesture
							     type))
		      until result)))))))

(defmethod stream-read-gesture ((stream standard-input-editing-stream)
				&rest rest-args &key peek-p
				&allow-other-keys)
  (with-keywords-removed (rest-args (:peek-p))
    (rescan-if-necessary stream)
    (with-slots (buffer insertion-pointer scan-pointer activation-gesture)
	stream
      (loop
	 (loop
	    while (< scan-pointer insertion-pointer)
	    do (let ((gesture (aref buffer scan-pointer)))
		 ;; Skip noise strings.
		 ;; XXX We should skip accept results too; I think that they
		 ;; should be consumed by ACCEPT-1. That's not happening yet.
		 (cond ((characterp gesture)
			(unless peek-p
			 (incf scan-pointer))
			(return-from stream-read-gesture gesture))
		       ((and (not peek-p)
			     (typep gesture 'goatee::accept-result-extent))
			(incf scan-pointer)
			(throw-object-ptype (goatee::object gesture)
					    (goatee::result-type gesture)))
		       (t (incf scan-pointer)))))
	 ;; The scan pointer should not be greater than the insertion pointer
	 ;; because the code that set the insertion pointer should have queued
	 ;; a rescan.
	 (when (> scan-pointer insertion-pointer)
	   (warn "scan-pointer ~S > insertion-pointer ~S; shouldn't happen"
		 scan-pointer insertion-pointer)
	   (immediate-rescan stream))
	 (when activation-gesture
	   (return-from stream-read-gesture
	     (prog1
		 activation-gesture
	       (unless peek-p
		 (setf activation-gesture nil)))))
	 (setf (slot-value stream 'rescanning-p) nil)
	 ;; In McCLIM stream-process-gesture is responsible for inserting
	 ;; characters into the buffer, changing the insertion pointer and
	 ;; possibly setting up the activation-gesture slot.
	 (loop
	    with gesture and type 
	    do (setf (values gesture type)
		     (apply #'stream-read-gesture
			    (encapsulating-stream-stream stream) rest-args))
	    when (null gesture)
	      do (return-from stream-read-gesture (values gesture type))
	    when (stream-process-gesture stream gesture type)
	      do (loop-finish))))))

#-(and)
(defmethod stream-unread-gesture ((stream standard-input-editing-stream)
				  gesture)
  (declare (ignore gesture))
  (when (> (stream-scan-pointer stream) 0)
    (setf (stream-activated stream) nil)
    (decf (stream-scan-pointer stream))))

(defmethod stream-unread-gesture ((stream standard-input-editing-stream)
				  gesture)
  (with-slots (buffer scan-pointer activation-gesture)
      stream
    (when (> scan-pointer 0)
      (if (and (eql scan-pointer (fill-pointer buffer))
	       (activation-gesture-p gesture))
	  (setf activation-gesture gesture)
	  (decf scan-pointer)))))

(defgeneric activate-stream (stream gesture)
  (:documentation "Cause the input editing stream STREAM to be activated with
  GESTURE"))

(defmethod activate-stream ((stream standard-input-editing-stream) gesture)
  (setf (activation-gesture stream) gesture)
  (setf (stream-insertion-pointer stream)
	(fill-pointer (stream-input-buffer stream)))
  (goatee::set-editing-stream-insertion-pointer
   stream
   (stream-insertion-pointer stream)))

;;; The editing functions of stream-process-gesture are performed by the
;;; primary method on goatee-input-editing-mixin
;;;

;;; These helper functions take the arguments of ACCEPT so that they
;;; can be used directly by ACCEPT.

(defun make-activation-gestures
    (&key (activation-gestures nil activation-gestures-p)
     (additional-activation-gestures nil additional-activations-p)
     (existing-activation-gestures *activation-gestures*)
     &allow-other-keys)
  (cond (additional-activations-p
	 (append additional-activation-gestures existing-activation-gestures))
	(activation-gestures-p
	 activation-gestures)
	(t (or existing-activation-gestures
	       *standard-activation-gestures*))))

(defun make-delimiter-gestures
    (&key (delimiter-gestures nil delimiter-gestures-p)
     (additional-delimiter-gestures nil additional-delimiters-p)
     (existing-delimiter-gestures *delimiter-gestures*)
     &allow-other-keys)
  (cond (additional-delimiters-p
	 (append additional-delimiter-gestures existing-delimiter-gestures))
	(delimiter-gestures-p
	 delimiter-gestures)
	(t existing-delimiter-gestures)))

(defmacro with-activation-gestures ((gestures &key override) &body body)
  ;; XXX Guess this implies that gestures need to be defined at compile time.
  ;; Sigh.
  (let ((gesture-form (if (and (symbolp gestures)
			       (gethash gestures *gesture-names*))
			  `(list ',gestures)
			  gestures))
	(gestures (gensym))
	(override-var (gensym)))
    `(let* ((,gestures ,gesture-form)	;Preserve evaluation order of arguments
	    (,override-var ,override)
	    (*activation-gestures* (make-activation-gestures
				    (if ,override-var
					:activation-gestures
					:additional-activation-gestures)
				    ,gestures)))
       ,@body)))

(defmacro with-delimiter-gestures ((gestures &key override) &body body)
  ;; XXX Guess this implies that gestures need to be defined at compile time.
  ;; Sigh.
  (let ((gesture-form (if (and (symbolp gestures)
			       (gethash gestures *gesture-names*))
			  `(list ',gestures)
			  gestures))
	(gestures (gensym))
	(override-var (gensym)))
    `(let* ((,gestures ,gesture-form)	;Preserve evaluation order of arguments
	    (,override-var ,override)
	    (*delimiter-gestures* (make-delimiter-gestures
				   (if ,override-var
				       :delimiter-gestures
				       :additional-delimiter-gestures)
				   ,gestures)))
       ,@body)))

(defun activation-gesture-p (gesture)
  (loop for gesture-name in *activation-gestures*
	when (gesture-matches-spec-p gesture gesture-name)
	do (return t)
	finally (return nil)))

(defun delimiter-gesture-p (gesture)
  (loop for gesture-name in *delimiter-gestures*
	when (gesture-matches-spec-p gesture gesture-name)
	do (return t)
	finally (return nil)))

(defmacro with-input-editing ((&optional (stream t)
			       &rest args
			       &key input-sensitizer (initial-contents "")
			       (class ''standard-input-editing-stream))
			      &body body)
  (setq stream (stream-designator-symbol stream '*standard-input*))
  (with-keywords-removed (args (:input-sensitizer :initial-contents :class))
    `(invoke-with-input-editing ,stream
				#'(lambda (,stream) ,@body)
				,input-sensitizer ,initial-contents ,class
				,@args)))

(define-condition rescan-condition (condition)
  ())

(defgeneric finalize (editing-stream input-sensitizer)
  (:documentation "Do any cleanup on an editing stream, like turning off the
  cursor, etc."))

(defgeneric invoke-with-input-editing
    (stream continuation input-sensitizer initial-contents class))

(defmethod invoke-with-input-editing :around ((stream extended-output-stream)
					      continuation
					      input-sensitizer
					      initial-contents
					      class)
  (declare (ignore continuation input-sensitizer initial-contents class))
  (letf (((cursor-visibility (stream-text-cursor stream)) nil))
    (call-next-method)))

(defmethod invoke-with-input-editing ((stream extended-input-stream)
				      continuation
				      input-sensitizer
				      initial-contents
				      class)
  (let ((editing-stream (make-instance class
				       :stream stream
				       :initial-contents initial-contents)))
    (goatee::redisplay-area (goatee::area editing-stream))
    (unwind-protect
	 (loop
	  (block rescan
	    (handler-bind ((rescan-condition #'(lambda (c)
						 (declare (ignore c))
						 (reset-scan-pointer
						  editing-stream)
						 (return-from rescan nil))))
	      (return-from invoke-with-input-editing
		(funcall continuation editing-stream)))))
      (finalize editing-stream input-sensitizer))))

(defun input-editing-rescan-loop (editing-stream continuation)
  (loop
   (block rescan
     (handler-bind ((rescan-condition #'(lambda (c)
					  (declare (ignore c))
					  (reset-scan-pointer editing-stream)
					  (return-from rescan nil))))
       (return-from input-editing-rescan-loop
	 (funcall continuation editing-stream))))))

(defmethod invoke-with-input-editing
    (stream continuation input-sensitizer initial-contents class)
  (declare (ignore input-sensitizer initial-contents class))
  (funcall continuation stream))

(defmethod reset-scan-pointer ((stream standard-input-editing-stream)
			       &optional (scan-pointer 0))
  (setf (stream-scan-pointer stream) scan-pointer)
  (setf (slot-value stream 'rescanning-p) t))

(defmethod immediate-rescan ((stream standard-input-editing-stream))
  (signal 'rescan-condition))

(defmethod queue-rescan ((stream standard-input-editing-stream))
  (setf (rescan-queued stream) t))

(defmethod rescan-if-necessary ((stream standard-input-editing-stream)
				&optional inhibit-activation)
  (declare (ignore inhibit-activation))
  (when (rescan-queued stream)
    (setf (rescan-queued stream) nil)
    (immediate-rescan stream)))

(defmethod input-editor-format ((stream t) format-string &rest format-args)
  (unless (and (typep stream '#.*string-input-stream-class*)
	       (input-stream-p stream))
    (apply #'format stream format-string format-args)))

(defun make-room (buffer pos n)
  (let ((fill (fill-pointer buffer)))
    (when (> (+ fill n)
	     (array-dimension buffer 0))
      (adjust-array buffer (list (+ fill n))))
    (incf (fill-pointer buffer) n)
    (replace buffer buffer :start1 (+ pos n) :start2 pos :end2 fill)))

;;; Defaults for replace-input and presentation-replace-input.

(defvar *current-input-stream* nil)
(defvar *current-input-position* 0)

(defmacro with-input-position ((stream) &body body)
  (let ((stream-var (gensym "STREAM")))
    `(let* ((,stream-var ,stream)
	    (*current-input-stream* (and (typep ,stream-var
						'input-editing-stream)
					 ,stream-var))
	    (*current-input-position* (and *current-input-stream*
					   (stream-scan-pointer ,stream-var))))
       ,@body)))

(defun read-token (stream &key
		   (input-wait-handler *input-wait-handler*)
		   (pointer-button-press-handler
		    *pointer-button-press-handler*)
		   click-only)
  (declare (ignore click-only))		;XXX For now
  (let ((result (make-array 1
			    :adjustable t
			    :fill-pointer 0
			    :element-type 'character))
	(in-quotes nil))
    ;; The spec says that read-token ignores delimiter gestures if the
    ;; first character is #\", until it sees another.  OK... what about
    ;; other occurences of #\"?  Guess we'll just accumulate them.
    (loop for first-char = t then nil
	  for gesture = (read-gesture
			 :stream stream
			 :input-wait-handler input-wait-handler
			 :pointer-button-press-handler
			 pointer-button-press-handler)
	  do (cond ((or (null gesture)
			(activation-gesture-p gesture)
			(typep gesture 'pointer-button-event)
			(and (not in-quotes)
			     (delimiter-gesture-p gesture)))
		    (loop-finish))
		   ((characterp gesture)
		    (if (eql gesture #\")
			(cond (first-char
			       (setq in-quotes t))
			      (in-quotes
			       (setq in-quotes nil))
			      (t (vector-push-extend gesture result)))
			(vector-push-extend gesture result)))
		   (t nil))
	  finally (progn
		    (when gesture
		      (unread-gesture gesture :stream stream))
		    ;; Return a simple string.  XXX Would returning an
		    ;; adjustable string be so bad?
		    (return (subseq result 0))))))

(defun write-token (token stream &key acceptably)
  (let ((put-in-quotes (and acceptably (some #'delimiter-gesture-p token))))
    (when put-in-quotes
      (write-char #\" stream))
    (write-string token stream)
    (when put-in-quotes
      (write-char #\" stream))))

;;; Signalling Errors Inside present (sic)

(define-condition simple-parse-error (simple-condition parse-error)
  ())

(defun simple-parse-error (format-string &rest format-args)
  (error 'simple-parse-error
	 :format-control format-string :format-arguments format-args))

(define-condition input-not-of-required-type (parse-error)
  ((string :reader not-required-type-string :initarg :string)
   (type :reader not-required-type-type :initarg :type))
  (:report (lambda (condition stream)
	     (format stream "Input ~S is not of required type ~S"
		     (not-required-type-string condition)
		     (not-required-type-type condition)))))

(defun input-not-of-required-type (object type)
  (error 'input-not-of-required-type :string object :type type))

;;; 24.5 Completion

(defvar *completion-gestures* '(:complete))
(defvar *help-gestures* '(:help))
(defvar *possibilities-gestures* '(:possibilities))

(define-condition simple-completion-error (simple-parse-error)
  ((input-so-far :reader completion-error-input-so-far
		 :initarg :input-so-far)))

;;; wrapper around event-matches-gesture-name-p to match against characters too.

(defgeneric gesture-matches-spec-p (gesture spec)
  (:documentation "Match a gesture against a gesture name or character."))

(defmethod gesture-matches-spec-p (gesture (spec symbol))
  (event-matches-gesture-name-p gesture spec))

(defmethod gesture-matches-spec-p ((gesture character) (spec character))
  (char-equal gesture spec))

(defmethod gesture-matches-spec-p (gesture spec)
  (declare (ignore gesture spec))
  nil)

(defun gesture-match (gesture list)
  "Returns t if gesture matches any gesture spec in list."
  (some #'(lambda (name)
	    (gesture-matches-spec-p gesture name))
	list))

;;; Helpers for complete-input, which is just getting too long.

(defun complete-gesture-p (gesture)
  (or (delimiter-gesture-p gesture) (activation-gesture-p gesture)))

;;; Break out rescanning case for complete-input.
;;;
;;; funky logic; we don't know if we're still rescanning until after the call
;;; to read-gesture. 
(defun complete-input-rescan (stream func partial-completers so-far
			      allow-any-input)
  (when (stream-rescanning-p stream)
    (loop for gesture = (read-gesture :stream stream :timeout 0)
	  while (and gesture (stream-rescanning-p stream))
	  if (complete-gesture-p gesture)
	    do (let (input success object nmatches)
		 (when (gesture-match gesture partial-completers)
		   (setf (values input success object nmatches)
			 (funcall func (subseq so-far 0) :complete-limited)))
                 (unless (and (numberp nmatches) (> nmatches 0))
                   ;; Not a partial match; better be a total match
                   (setf (values input success object)
                         (funcall func (subseq so-far 0) :complete))
                   (if (or success allow-any-input)
                       (progn
                         (unread-gesture gesture :stream stream)
                         (return-from complete-input-rescan
                           (values object t input)))
		       (error 'simple-completion-error
                            :format-control "complete-input: While rescanning,~
                                             can't match ~A~A"
                            :format-arguments (list so-far  gesture)
			    
                            :input-so-far so-far))))
	  end
	  do (vector-push-extend gesture so-far)
	  finally (when gesture
		    (unread-gesture gesture :stream stream))))
  nil)

(defun possibilities-for-menu (possibilities)
  (loop for p in possibilities
	for (display . object) = p
	if (listp object)
	  collect `(,display :value ,object)
	else
	  collect p))

;;; Helper returns gesture (or nil if gesture shouldn't be part of the input)
;;; and completion mode, if any.

(defvar *completion-possibilities-continuation* nil)

(defun read-completion-gesture (stream
				partial-completers
				help-displays-possibilities)
  (flet ((possibilitiesp (gesture)
	   (or (gesture-match gesture *possibilities-gestures*)
	       (and help-displays-possibilities
		    (gesture-match gesture *help-gestures*)))))
    (let ((*completion-possibilities-continuation*
	   #'(lambda ()
	       (return-from read-completion-gesture
		 (values nil :possibilities)))))
      (handler-bind ((accelerator-gesture
		      #'(lambda (c)
			  (let ((gesture (accelerator-gesture-event c)))
			    (when (possibilitiesp gesture)
				(return-from read-completion-gesture
				  (values nil :possibilities)))))))
	(let ((gesture (read-gesture :stream stream)))
	  (values gesture
		  (cond ((possibilitiesp gesture)
			 :possibilities)
			((gesture-match gesture partial-completers)
			 :complete-limited)
			((gesture-match gesture *completion-gestures*)
			 :complete-maximal)
			((complete-gesture-p gesture)
			 :complete)
			(t nil))))))))

(defparameter *trace-complete-input* nil)

(defun complete-input (stream func &key
		       partial-completers allow-any-input possibility-printer
		       (help-displays-possibilities t))
  (declare (ignore possibility-printer))
  (let ((so-far (make-array 1 :element-type 'character :adjustable t
			    :fill-pointer 0))
	(*accelerator-gestures* (append *help-gestures*
					*possibilities-gestures*
					*accelerator-gestures*)))
    (with-input-position (stream)
      (flet ((insert-input (input)
	       (adjust-array so-far (length input)
			     :fill-pointer (length input))
	       (replace so-far input)
	       (replace-input stream input :rescan nil)))
 	(multiple-value-bind (object success input)
	    (complete-input-rescan stream func partial-completers
				   so-far allow-any-input)
	  (when success
	    (return-from complete-input (values object success input))))
	(loop
	 (multiple-value-bind (gesture mode)
	     (read-completion-gesture stream
				      partial-completers
				      help-displays-possibilities)
	   (if mode
	       (multiple-value-bind
		     (input success object nmatches possibilities)
		   (funcall func (subseq so-far 0) mode)
		 (when (and (zerop nmatches)
			    (eq mode :complete-limited)
			    (complete-gesture-p gesture))
		   ;; Gesture is both a partial completer and a
		   ;; delimiter e.g., #\space.  If no partial match,
		   ;; try again with a total match.
		   (setf (values input success object nmatches possibilities)
			 (funcall func (subseq so-far 0) :complete))
		   (setf mode :complete))
		 ;; Preserve the delimiter
		 (when (and success (eq mode :complete))
		   (unread-gesture gesture :stream stream))
		 ;; Get completion from menu
		 (when *trace-complete-input*
		   (format *trace-output* "nmatches = ~A, mode = ~A~%"
			   nmatches mode))
		 (when (and (> nmatches 0) (eq mode :possibilities))
		   (multiple-value-bind (menu-object item event)
		       (menu-choose (possibilities-for-menu possibilities)
                                    :label "Possibilities"
                                    :n-columns 1)
		     (declare (ignore event))
		     (if item
			 (progn
			   (setf (values input success object nmatches)
				 (values (car item) t menu-object 1)))
			 (setf success nil
			       nmatches 0))))
		 (unless (and (eq mode :complete) (not success))
		   (if (> nmatches 0)
		       (insert-input input)
		       (beep)))
                 (cond ((and success (eq mode :complete))
			(return-from complete-input
			  (values object success input)))
		       ((activation-gesture-p gesture)
			(if allow-any-input
			    (return-from complete-input
			      (values nil t (subseq so-far 0)))
			    (error 'simple-completion-error
				   :format-control "Input ~S does not match"
				   :format-arguments (list so-far)
				   :input-so-far so-far)))))
	       (vector-push-extend gesture so-far))))))))


;;; helper function

(defun left-prefix (string1 string2 &key (end nil))
  "Returns the common prefix of string1 and string2, up to end"
  (let* ((end1 (if end
		   (min (length string1) end)
		   nil))
	 (end2 (if end
		   (min (length string2) end)
		   nil))
	 (mismatch (mismatch string1 string2 :test #'char-equal
			     :end1 end1 :end2 end2)))
    (cond (mismatch
	   (subseq string1 0 mismatch))
	  (end
	   (subseq string1 0 end))
	  (t string1))))

(defun complete-from-generator (initial-string generator delimiters &key
				(action :complete)
				(predicate (constantly t)))
  (when (eq action :possibilities)
    (return-from complete-from-generator
      (complete-from-generator-possibilities initial-string
					     generator
					     predicate)))
  (let ((initial-string-len (length initial-string))
	(candidate-match nil)
	(matches 0)
	(object nil)
	(identical nil)
	(identical-match nil)
	(identical-object nil)
	(actual-match nil))
    (flet ((suggester (str obj)
	     (unless (funcall predicate obj)
	       (return-from suggester nil))
	     (let ((partial-match-end
		    (and (eq action :complete-limited)
			 (>= (length str) initial-string-len)
			 (position-if #'(lambda (c) (member c delimiters))
				      str
				      :start initial-string-len))))
	       (when (and (eq action :complete-limited)
			  (null partial-match-end))
		 (return-from suggester nil))
	       (unless partial-match-end
		 (setq partial-match-end (1- (length str))))
	       (let ((mismatch-initial (mismatch initial-string str
						 :test #'char-equal)))
		 (cond ((and mismatch-initial
			     (>= mismatch-initial (length initial-string)))
			(incf matches)
			(unless candidate-match
			  (setq object obj))
			(setf candidate-match
			      (cond (candidate-match
				     (left-prefix candidate-match
						  str
						  :end (1+ partial-match-end)))
				    (partial-match-end
				     (subseq str 0 (1+ partial-match-end)))
				    (t str))
			      actual-match str))
		       ((null mismatch-initial)
			(incf matches)
			;; If there's a longer match we want to find it.
			(if (eq action :complete-maximal)
			    (progn
			      (setf identical-match str)
			      (setf identical-object obj))
			    (progn
			      (setf candidate-match str)
			      (setf object obj)))
			(setf identical t)))))))
      (funcall generator initial-string #'suggester)
      (let ((partial-match-before-end (and (eq action :complete-limited)
					   (eql matches 1)
					   (< (length candidate-match)
					      (length actual-match)))))
	(values (or candidate-match identical-match initial-string)
		(or (and identical
			 (or (not (eq action :complete-maximal))
			     (eql matches 1)))
		    (and (eql matches 1)
			 (not partial-match-before-end)))
		(if (eq action :complete-maximal)
		    (cond ((and (eql matches 2) identical-match)
			   object)
			  ((and identical-match (eql matches 1))
			   identical-object)
			  ((eql matches 1)
			   object))
		    (and (or identical (and (eql matches 1)
					    (not partial-match-before-end)))
			 object))
		matches
		nil)))))

;;; The possibilities action is different enough that I don't want to add to
;;; the spaghetti above...

(defun complete-from-generator-possibilities
    (initial-string generator predicate)
  (let ((possibilities nil)
	(nmatches 0)
	(initial-len (length initial-string)))
    (flet ((suggester (str obj)	     
	     (unless (funcall predicate obj)
	       (return-from suggester nil))
	     (when (>= (or (mismatch initial-string str :test #'char-equal)
			   (length initial-string))
		       initial-len)
	       (incf nmatches)
	       (push (cons str obj) possibilities))))
      (funcall generator initial-string #'suggester)
      (if (and (eql nmatches 1)
	       (string-equal initial-string (caar possibilities)))
	  ;; return values are as from complete-from-generator, qv.
	  (values (caar possibilities)
		  t
		  (cdar possibilities)
		  nmatches
		  possibilities)
	  (values initial-string nil nil nmatches (sort possibilities #'string-lessp :key #'car))))))

(defun complete-from-possibilities (initial-string completions delimiters &key
				    (action :complete)
				    (predicate (constantly t))
				    (name-key #'car)
				    (value-key #'cadr))
  (flet ((generator (input-string suggester)
	   (declare (ignore input-string))
	   (do-sequence (possibility completions)
	     (funcall suggester
		      (funcall name-key possibility)
		      (funcall value-key possibility)))))
    (complete-from-generator initial-string #'generator delimiters
			     :action action
			     :predicate predicate)))

(defun suggest (completion object)
  (declare (ignore completion object))
  (error
   "SUGGEST called outside of lexical scope of COMPLETING-FROM-SUGGESTIONS" ))

(defmacro completing-from-suggestions ((stream &rest args) &body body)
  (when (eq stream t)
    (setq stream '*standard-input*))
  (let ((generator (gensym "GENERATOR"))
	(input-string (gensym "INPUT-STRING"))
	(suggester (gensym "SUGGESTER")))
     `(flet ((,generator (,input-string ,suggester)
	      (declare (ignore ,input-string))
	      (flet ((suggest (completion object)
		       (funcall ,suggester completion object)))
		,@body)))
       ;; This sucks, but we can't use args to the macro directly because
       ;; we want the partial-delimiters argument and we need to insure its
       ;; proper evaluation order with everything else.
       (let* ((complete-input-args (list ,@args))
	      (partial-completers (getf complete-input-args
					:partial-completers
					nil)))
	 (apply #'complete-input
		,stream
		#'(lambda (so-far mode)
		    (complete-from-generator so-far
					     #',generator
					     partial-completers
					     :action mode))
		complete-input-args)))))

;;; Infrasructure for detecting empty input, thus allowing accept-1
;;; to supply a default.

(defmacro handle-empty-input ((stream) input-form &body handler-forms)
  "Establishes a context on `stream' (a `standard-input-editing-stream') in
  which empty input entered in `input-form' may transfer control to
  `handler-forms'. Empty input is assumed when a simple-parse-error is
  signalled and there is a delimeter gesture or activation gesture in the
  stream at the position where `input-form' began its input. The gesture that
  caused the transfer remains to be read in `stream'. Control is transferred to
  the outermost `handle-empty-input' form that is empty.

  Note: noise strings in the buffer, such as the prompts of recursive calls to
  `accept', cause input to not be empty. However, the prompt generated by
  `accept' is generally not part of its own empty input context."
  (with-gensyms (input-cont handler-cont)
    `(flet ((,input-cont ()
	      ,input-form)
	    (,handler-cont ()
	      ,@handler-forms))
       (declare (dynamic-extent #',input-cont #',handler-cont))
       (invoke-handle-empty-input ,stream #',input-cont #',handler-cont))))

(define-condition empty-input-condition (simple-condition)
  ((stream :reader empty-input-condition-stream :initarg :stream)))

;;; The code that signalled the error might have consumed the gesture, or
;;; not.
;;; XXX Actually, it would be a violation of the `accept' protocol to consume
;;; the gesture, but who knows what random accept methods are doing.
(defun empty-input-p
    (stream begin-scan-pointer activation-gestures delimiter-gestures)
  (let ((scan-pointer (stream-scan-pointer stream))
	(fill-pointer (fill-pointer (stream-input-buffer stream))))
    ;; activated?
    (cond ((and (eql begin-scan-pointer scan-pointer)
		(eql scan-pointer fill-pointer))
	   t)
	  ((or (eql begin-scan-pointer scan-pointer)
	       (eql begin-scan-pointer (1- scan-pointer)))
	   (let ((gesture (aref (stream-input-buffer stream)
				begin-scan-pointer)))
	     (and (characterp gesture)
		  (or (gesture-match gesture activation-gestures)
		      (gesture-match gesture delimiter-gestures)))))
	  (t nil))))

;;; The control flow in here might be a bit confusing. The handler catches
;;; parse errors from accept forms and checks if the input stream is empty. If
;;; so, it resignals an empty-input-condition to see if an outer call to
;;; accept is empty and wishes to handle this situation. We don't resignal the
;;; parse error itself because it might get handled by a handler on ERROR in an
;;; accept method or in user code, which would screw up the default mechanism.
;;;
;;; If the situation is not handled in the innermost empty input handler,
;;; either directly or as a result of resignalling, then it won't be handled
;;; by any of the outer handlers as the stack unwinds, because EMPTY-INPUT-P
;;; will return nil.
(defun invoke-handle-empty-input
    (stream input-continuation handler-continuation)
  (unless (input-editing-stream-p stream)
    (return-from invoke-handle-empty-input (funcall input-continuation)))
  (let ((begin-scan-pointer (stream-scan-pointer stream))
	(activation-gestures *activation-gestures*)
	(delimiter-gestures *delimiter-gestures*))
    (block empty-input
      (handler-bind (((or simple-parse-error empty-input-condition)
		      #'(lambda (c)
			  (when (empty-input-p stream
					       begin-scan-pointer
					       activation-gestures
					       delimiter-gestures)
			    (if (typep c 'empty-input-condition)
				(signal c)
				(signal 'empty-input-condition :stream stream))
			    ;; No one else wants to handle it, so we will
			    (return-from empty-input nil)))))
	(return-from invoke-handle-empty-input (funcall input-continuation))))
    (funcall handler-continuation)))


