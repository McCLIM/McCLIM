;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-DEMO; Base: 10; Lowercase: Yes -*-
;; $fiHeader: process-browser.lisp,v 1.7 1993/07/27 01:46:02 colin Exp $

(in-package :clim-demo)

(define-application-frame process-browser ()
    ((timer :accessor process-browser-timer :initform nil)
     (delay :initarg :delay :accessor process-browser-delay))
  (:panes
    (processes :application
	       :incremental-redisplay '(t :check-overlapping nil)
	       :display-function 'display-processes
	       :width :compute :height :compute))
  (:layouts
    (default processes))
  (:default-initargs :delay 5))

(defmethod read-frame-command :around ((frame process-browser) &key)
  (with-accessors ((timer process-browser-timer)) frame 
    (unwind-protect
	(progn
	  (install-process-browser-timer frame)
	  (call-next-method))
      (when timer (clim-utils:delete-timer timer)))))

(defun install-process-browser-timer (frame)
  (with-accessors ((timer process-browser-timer)
		   (delay process-browser-delay)) frame 
    (when timer (clim-utils:delete-timer timer))
    (setq timer (clim-internals::make-command-timer
		  frame '(com-update-process-browser)
		  :delay delay))))

(define-process-browser-command (com-change-delay-browser :menu t)
    ()
  (let ((frame *application-frame*))
    (with-accessors ((timer process-browser-timer)
		     (delay process-browser-delay)) frame
      (accepting-values (*query-io* :own-window t)
	(setf delay (accept '(integer 1 *) 
			    :prompt "Enter new delay"
			    :default delay
			    :stream *query-io*))
	(terpri *query-io*)))))

(define-process-browser-command com-update-process-browser ()
  ;;--- We have to do this because of the asynchronous updates
  (unhighlight-highlighted-presentation *standard-output*))

(define-process-browser-command (com-refresh-process-browser :menu t) ()
  (window-clear *standard-output*))

(define-process-browser-command (com-quit-process-browser :menu t) ()
  (frame-exit *application-frame*))

#+:composer-v2.0
(define-process-browser-command com-inspect-process
    ((process 'mp::process :gesture :select))
  (wt::winspect process))

(defun display-processes (frame pane &key &allow-other-keys)
  (declare (ignore frame))
  (with-text-size (t :small)
    (let ((*standard-output* pane))
      (formatting-table ()
	(with-text-face (t :bold)
	  (updating-output (t :unique-id 'headings)
	    (formatting-row ()
	      (formatting-cell () (write-string "P"))
	      (formatting-cell () (write-string "Dis"))
	      (formatting-cell () (write-string "Sec"))
	      (formatting-cell () (write-string "dSec"))
	      (formatting-cell () (write-string "Priority"))
	      (formatting-cell () (write-string "State"))
	      (formatting-cell () (write-string "Name,Whostate,Arrest")))))
	;;--- snarfed from toplevel.cl
	(let* ((processes (clim-sys:all-processes))
	       (processes
		 (sort (clim-sys:without-scheduling	;assure consistent data
			 (mapcar #'(lambda (p)
				     (let* ((times  (mp::process-times-resumed p))
					    (dtimes (- times (mp::process-times-resumed-1 p)))
					    (msec   (mp::process-cpu-msec-used p))
					    (dmsec  (- msec (mp::process-cpu-msec-used-1 p))))
				       (prog1 (list* dtimes msec dmsec p)
					      (setf (mp::process-times-resumed-1 p) times
						    (mp::process-cpu-msec-used-1 p) msec))))
				 processes))
		       #'>= :key #'caddr)))
	  (dolist (p processes)
	    (destructuring-bind (times-resumed msec-used msec-used-d . process) p
	      (let ((profilep 
		      (let ((stack-group (mp::process-stack-group process)))
			(and stack-group
			     (mp::profile-stack-group-p stack-group)))))
		(updating-output (t :unique-id process 
				    :cache-test #'equal
				    :cache-value 
				    (list p
					  profilep
					  (mp::process-active-p process)
					  (mp::process-runnable-p process)
					  (mp::process-wait-function process)
					  (clim-sys:process-name process)
					  (clim-sys:process-whostate process)
					  (mp::process-arrest-reasons process)))
		  (formatting-row ()
		    (with-output-as-presentation (t process 'mp::process
						  :single-box t)
		      (updating-output (t :cache-value profilep)
			(formatting-cell ()
			  (princ profilep)))
		      (updating-output (t :cache-value times-resumed)
			(formatting-cell ()
			  (princ times-resumed)))
		      (updating-output (t :cache-value msec-used)
			(formatting-cell ()
			  (princ (round msec-used 1000))))
		      (updating-output (t :cache-value msec-used-d)
			(formatting-cell ()
			  (princ (/ msec-used-d 1000.0))))
		      (updating-output (t :cache-value (mp::process-priority process))
			(formatting-cell ()
			  (princ (mp::process-priority process))))
		      (let ((state
			      (cond ((not (mp::process-active-p process)) "inactive")
				    ((mp::process-runnable-p process) "runnable")
				    ((mp::process-wait-function process) "waiting ")
				    (t "   ?    "))))
			(updating-output (t :cache-value state :cache-test #'equal)
			  (formatting-cell ()
			    (princ state))))
		      (updating-output (t :cache-value (clim-sys:process-name process)
					  :cache-test #'equal)
			(formatting-cell ()
			  (with-text-face (t :bold)
			    (princ (clim-sys:process-name process)))))
		      (updating-output (t :cache-value (clim-sys:process-whostate process))
			(formatting-cell ()
			  (princ (clim-sys:process-whostate process))))
		      (updating-output (t :cache-value (mp::process-arrest-reasons process)
					  :cache-test #'equal)
			(formatting-cell ()
			  (when (mp::process-arrest-reasons process)
			    (princ (mp::process-arrest-reasons process))))))))))))))))



(define-demo "Process Browser" process-browser)
