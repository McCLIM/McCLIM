;; Gilbert Baumann's hacks for using the CMUCL debugger within a CLIM stream.

(in-package :climi)

;; a patch
(defmethod stream-listen ((stream standard-extended-input-stream))
  (with-encapsulating-stream (estream stream)
    (loop for char = (read-gesture-or-reason estream :timeout 0 :peek-p t)
	  do (if (read-result-p char)
		 (loop-finish)
		 (stream-read-gesture estream)) ; consume pointer gesture
	  finally (return (characterp char)))))

;; a patch, not sure about this one as constructing an event seems wrong.
(defmethod stream-unread-gesture ((stream standard-extended-input-stream)
				  gesture)
  (with-encapsulating-stream (estream stream)
    (repush-gesture (if (characterp gesture)
                        (make-instance 'key-press-event
                                       :modifier-state 0
                                       :key-name gesture
                                       :key-character gesture
                                       :sheet estream
                                       :x 0 :y 0 :graft-x 0 :graft-y 0)
                        gesture)
                    (stream-input-buffer estream))))

;; get rid of moore's debugging messages ...

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
	    (complete-input-rescan stream func partial-completers so-far)
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
		 (when (and (> nmatches 0) (eq mode :possibilities))
		   (multiple-value-bind (menu-object item event)
		       (menu-choose (possibilities-for-menu possibilities))
		     (declare (ignore event))
		     (if item
			 (progn
			   (setf (values input success object nmatches)
				 (values (car item) t menu-object 1)))
			 (setf success nil
			       nmatches 0))))
		 (if (> nmatches 0)
		     (insert-input input)
		     (beep))
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

(in-package :goatee)

(defmethod lookup-gesture-command ((gesture key-press-event) table)
  (let ((modifier-state (logandc1 climi::+alt-key+
                                  (event-modifier-state gesture))))

    (cdr (assoc modifier-state
                (gethash (keyboard-event-key-name gesture) table nil)))))

(in-package "DEBUG")

(defun internal-debug ()
  (let ((*in-the-debugger* t)
	(*read-suppress* nil))
    (unless (typep *debug-condition* 'step-condition)
      (clear-input *debug-io*)
      (format *debug-io* "~2&Debug  (type H for help)~2%"))
     (debug-loop) ))
    ;; (mp:without-scheduling (debug-loop))))

(defun invoke-debugger (condition)
  "The CMU Common Lisp debugger.  Type h for help."
  (when *debugger-hook*
    (let ((hook *debugger-hook*)
	  (*debugger-hook* nil))
      (funcall hook condition hook)))
  (unix:unix-sigsetmask 0)
  (let* ((*debug-condition* condition)
	 (*debug-restarts* (compute-restarts condition))
	 (*standard-input* *debug-io*)  ;in case of setq
	 (*standard-output* *debug-io*) ;''  ''  ''  ''
	 (*error-output* *debug-io*)
	 ;; Rebind some printer control variables.
	 (kernel:*current-level* 0)
	 (*print-readably* nil)
	 (*read-eval* t))
    (let ((*debugger-hook*
           (lambda (cond hook)
             (let ((*debugger-hook* nil)
                   (*debug-io* sys:*tty*))
               (invoke-debugger cond)))))
      (real-invoke-debugger condition))))

(defun debug-prompt ()
  (let ((*standard-output* *debug-io*))
    (progn
      (terpri)
      (prin1 (di:frame-number *current-frame*))
      (dotimes (i *debug-command-level*) (princ "]"))
      (princ " ")
      (force-output))))

(defparameter *debug-prompt* #'debug-prompt
  "This is a function of no arguments that prints the debugger prompt
   on *debug-io*.")

(in-package "LISP")

(defun get-stream-command (stream)
  "This takes a stream and waits for text or a command to appear on it.  If
   text appears before a command, this returns nil, and otherwise it returns
   a command."
  (let ((cmdp nil #+NIL (funcall (lisp-stream-misc stream) stream :get-command)))
    (cond (cmdp)
	  ((listen stream)
	   nil)
	  (t
	   ;; This waits for input and returns nil when it arrives.
	   (unread-char (read-char stream) stream)))))
