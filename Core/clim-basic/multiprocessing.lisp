;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-INTERNALS; -*-
;;;   Title: CLIM-2, Chapter 32.2 Multi-processing (BT)
;;; Created: 2016-01-27
;;;  Author: Daniel Kochmański <daniel@turtleware.eu>, Bo Yao <ailisp@sina.com>
;;; License: LGPL-2.1
;;; ---------------------------------------------------------------------------
;;; (c) copyright 2016 by Daniel Kochmański


;;;; Multiprocessing non-portable stuff (implemented using
;;;; bordeaux-threads where feasible). BT has also limited support for
;;;; single-threaded implementations. Supersedes MP-NIL backend.

(in-package :clim-internals)


;;; B.2 Multi-processing
;;;
;;; Most Lisp implementations provide some form of
;;; multi-processing. CLIM provides a pset of functions that implement
;;; a uniform interface to the multi-processing functionality.

(defconstant *multiprocessing-p* bt:*supports-threads-p*
  "The value of *multiprocessing-p* is t if the current Lisp
environment supports multi-processing, otherwise it is nil.")

(eval-when (:load-toplevel :compile-toplevel :execute)
  (pushnew :clim-mp *features*))

(define-condition multiprocessing-condition (error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (format stream (message condition)))))

(defstruct (process (:constructor %make-process)
		    (:conc-name %process-))
  (name nil)
  (function nil)
  (thread nil)
  (state :running)
  (wait-on nil)
  (timeout nil)
  (lock (make-recursive-lock "property-lock"))
  (interrupt-allow-p t))

(defvar *all-process-lock* (make-recursive-lock "all-process-lock"))
(defvar *all-processes* (make-hash-table))

(defvar *current-process* nil)

(let ((current-thread (bt:current-thread)))
  (setf *current-process* (%make-process :name (bt:thread-name current-thread)
					 :thread current-thread))
  (setf (gethash *current-process* *all-processes*) t))

(defun make-process (function &key name)
  "Creates a process named name. The new process will evaluate the function function. On systems that do not support multi-processing, make-process will signal an error."
  (%signal-error-if-mp-not-support)
  (let* ((process (%make-process :name (or name "anonymous process")))
	 (func #'(lambda () (let ((*current-process* process))
			      (funcall function)))))
    (setf (%process-thread process) 
	  (bt:make-thread func :name (%process-name process))
	  (%process-function process)
	  func)
    (with-recursive-lock-held (*all-process-lock*)
      (setf (gethash process *all-processes*) t))
    process))

(defun %signal-error-if-current-process (process)
  (when (eql process (current-process))
    (error 'multiprocessing-condition
	   :message "cannot destroy current process")))

(defun %signal-error-if-mp-not-support ()
  (unless *multiprocessing-p*
    (error 'multiprocessing-condition
	   :message "multiprocessing is not supported")))

(defun destroy-process (process)
  "Terminates the process process. process is an object returned by make-process."
  (%signal-error-if-current-process process)
  (with-recursive-lock-held ((%process-lock process))
    (bt:destroy-thread (%process-thread process)))
  (with-recursive-lock-held (*all-process-lock*)
    (remhash process *all-processes*)))

(defun current-process ()
  "Returns the currently running process, which will be the same kind of object as would be returned by make-process."
  *current-process*)

(defun all-processes ()
  "Returns a sequence of all of the processes."
  (let (all-proc)
   (with-recursive-lock-held (*all-process-lock*)
     (alexandria:maphash-keys #'(lambda (key) (push key all-proc))
			      *all-processes*)
     all-proc)))

(defun processp (object)
  "Returns t if object is a process, otherwise returns nil."
  (typep object 'process))

(defun process-name (process)
  "Returns the name of the process. The format varys depending on the platform."
  (with-recursive-lock-held ((%process-lock process))
    (%process-name process)))

(defun process-state (process)
  "Returns the state of the process. The format varys depending on the platform."
  (with-recursive-lock-held ((%process-lock process))
   (%process-state process)))

(defun process-whostate (process)
  "Returns the whostate of the process. The format varys depending on the platform."
  (with-recursive-lock-held ((%process-lock process))
   (case (%process-state process)
     (:waiting (aif (%process-timeout process)
		    (format nil "waiting on: ~s, timeout: ~s" (%process-wait-on process) it)
		    (format nil "waiting on: ~s" (%process-wait-on process) it)))
     (otherwise (format nil "~(~a~)" (%process-state process))))))

(defun process-wait (reason predicate)
  "Causes the current process to wait until predicate returns true. reason is a   
  \"reason\" for waiting, usually a string. On systems that do not support         
  multi-processing, process-wait will loop until predicate returns true."
  (%process-wait-with-timeout reason nil predicate))

(defun process-wait-with-timeout (reason timeout predicate)
  "Causes the current process to wait until either predicate returns true, or the number of seconds specified by timeout has elapsed. reason is a \"reason\" for waiting, usually a string. On systems that do not support multi-processing, process-wait-with-timeout will loop until predicate returns true or the timeout has elapsed."
  (%process-wait-with-timeout reason timeout predicate))

(defun %%set-current-process-wait (reason timeout)
  (with-recursive-lock-held ((%process-lock (current-process)))
   (setf (%process-state (current-process)) :waiting
	 (%process-wait-on (current-process)) reason
	 (%process-timeout (current-process)) timeout)))

(defun %%set-current-process-run ()
  (with-recursive-lock-held ((%process-lock (current-process)))
    (setf (%process-state (current-process)) :running
	  (%process-wait-on (current-process)) nil
	  (%process-timeout (current-process)) nil)))

(defun %%make-pred-process-func (timeout predicate wait-cv wait-lock)
  (if timeout
      #'(lambda ()
	  (let ((end-time (+ (get-universal-time))))
	    (loop (when (or (funcall predicate)
			    (>= (get-universal-time) end-time))
		    (with-recursive-lock-held (wait-lock)
		      (bt:condition-notify wait-cv))
		    (return))
	       (process-yield))))
      #'(lambda ()
	  (loop (when (funcall predicate)
		  (with-recursive-lock-held (wait-lock)
		    (bt:condition-notify wait-cv))
		  (return))
	     (process-yield)))) )

(defun %process-wait-with-timeout (reason timeout predicate)
   (let ((wait-lock (make-recursive-lock))
	 (wait-cv (bt:make-condition-variable)))
     (%%set-current-process-wait reason timeout)
     (with-recursive-lock-held (wait-lock)
       (let ((pred-proc (make-process (%%make-pred-process-func timeout predicate wait-cv wait-lock))))
	 (bt:condition-wait wait-cv wait-lock)
	 (destroy-process pred-proc)))
     (%%set-current-process-run)))

(defun process-yield ()
  "Allows other processes to run. On systems that do not support multi-processing, this does nothing."
  (bt:thread-yield))

(defun process-interrupt (process function)
  "Interrupts the process process and causes it to evaluate the function function. On systems that do not support multi-processing, this is equivalent to funcall'ing function."
  (with-recursive-lock-held ((%process-lock process))
    (when (%process-interrupt-allow-p process)
      (bt:interrupt-thread (%process-thread process) function))))

(defun disable-process (process)
  "Disables the process process from becoming runnable until it is enabled again."
  (with-recursive-lock-held ((%process-lock process))
    (when (%process-interrupt-allow-p process)
      (bt:destroy-thread (%process-thread process))
      (setf (%process-state process) :disabled))))

(defun enable-process (process)
  "Enable the process process to become runnable again after it has been disabled."
  (with-recursive-lock-held ((%process-lock process))
    (when (eql (%process-state process) :disabled)
      (setf (%process-state process) :running
	    (%process-thread process) (bt:make-thread (%process-function process)
						     :name (%process-name process))))))

(defun restart-process (process)
  "Restarts the process process by \"unwinding\" it to its initial state, and reinvoking its top-level function."
  (with-recursive-lock-held ((%process-lock process))
    (when (%process-interrupt-allow-p process)
      (bt:destroy-thread (%process-thread process))
      (setf (%process-thread process) (bt:make-thread (%process-function process)
						     :name (%process-name process))))))

(defmacro without-scheduling (&body body)
  "Evaluates body in a context that is guaranteed to be free from interruption by other processes. On systems that do not support multi-processing, without-scheduling is equivalent to progn."
  (if *multiprocessing-p*
      `(progn
	 (with-recursive-lock-held ((%process-lock (current-process)))
	  (setf (%process-interrupt-allow-p (current-process)) nil))
	 (unwind-protect (progn
			   ,@body)
	   (with-recursive-lock-held ((%process-lock (current-process)))
	     (setf (%process-interrupt-allow-p (current-process)) t))))
      `(progn
	 ,@body)))

(defun atomic-incf (reference)
  "Increments the fixnum value referred to by reference as a single, atomic operation."
  (without-scheduling
      (incf reference)))

(defun atomic-decf (reference)
  "Decrements the fixnum value referred to by reference as a single, atomic operation."
  (without-scheduling
      (decf reference)))



;;; B.3 Locks

(defun make-lock (&optional name)
  "Creates a lock whose name is name. On systems that do not support locking, this will return a new list of one element, nil."
  (if *multiprocessing-p*
      (bt:make-lock name)
      (cons nil nil)))

(defmacro with-lock-held ((place &optional state) &body body)
  "Evaluates body with the lock named by place. place is a reference to a lock created by make-lock.
On systems that do not support locking, with-lock-held is equivalent to progn."
  (declare (ignore state))
  (if *multiprocessing-p*
      `(bt:with-lock-held (,place) ,@body)
      `(progn ,@body)))

(defun make-recursive-lock (&optional name)
  "Creates a recursive lock whose name is name. On systems that do not support locking, this will return a new list of one element, nil. A recursive lock differs from an ordinary lock in that a process that already holds the recursive lock can call with-recursive-lock-held on the same lock without blocking."
  (if *multiprocessing-p*
      (bt:make-recursive-lock name)
      (cons nil nil)))

(defmacro with-recursive-lock-held ((place &optional state) &body body)
  "Evaluates body with the recursive lock named by place. place is a reference to a recursive lock created by make-recursive-lock. 
On systems that do not support locking, with-recursive-lock-held is equivalent to progn."
  (declare (ignore state))
  (if *multiprocessing-p*
      `(bt:with-recursive-lock-held (,place) ,@body)
      `(progn ,@body)))



;;; O.0 Conditionals
;;; 
;;; These functions aren't in the CLIM specification, but *things*
;;; depend on them (apparenly they are useful)

(defun make-condition-variable ()
  (bt:make-condition-variable))

(defun condition-wait (cv lock &optional timeout)
  (bt:condition-wait cv lock :timeout timeout))

(defun condition-notify (cv)
  (bt:condition-notify cv))
