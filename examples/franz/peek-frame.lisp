;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-USER; Base: 10; Lowercase: Yes -*-

;; $fiHeader: peek-frame.lisp,v 1.5 1993/07/27 01:45:54 colin Exp $

(in-package :clim-user)

#||
			 RESTRICTED RIGHTS LEGEND
				    
 Use, duplication, or disclosure by the Government is subject to
 restrictions as set forth in subdivision (b)(3)(ii) of the Rights in
 Technical Data and Computer Software Clause at 52.227-7013 of the DOD
 FAR Supplement.
				    
		     BBN Systems and Technologies,
			     a division of
		      Bolt Beranek and Newman Inc.
			   10 Moulton Street
			  Cambridge, MA 02138
			      617-873-3000
				    
      Copyright 1990, 1991, 1992 by BBN Systems and Technologies, 
      a division of Bolt Beranek and Newman Inc., all rights reserved.

||#


#||

(PEEK) creates a peek frame for watching and manipulating lisp process activity
and optionally OS process activity.  

The current command items are:

Update: Immediately update the display pane.

Redisplay: Completely redisplay the display pane if something ugly happens to it.

Options: Popup dialog of display options.

Faster: Half the interval between screen updates.

Slower: Double the interval between screen updates.

Pause: Toggle the paused/running state of the display.

Authors: Ken Anderson (KAnderson@bbn.com), Jeff Morrill (JMorrill@bbn.com),
with initial help from Dennis Doughty (doughty@daffy-duck.hq.ileaf.com).
Revised and stripped down by Scott McKay (SWM@Symbolics.COM) for CLIM 2.0.

||#


;; Define a simple "peek-like" application.  It has one display area
;; which incrementally redisplays every timeout seconds.
(define-application-frame peek-frame ()
    ((display-pane)
     (mode :initform :process)			;for extensibility
     (show-gc-p :initform t)
     (show-lisp-processes-p :initform t)
     (show-OS-processes-p :initform nil)
     (OS-command :initarg :OS-command :initform '("/bin/ps"))
     (timeout :initform 1)
     (timeout-growth-factor :initform 2.0)
     (paused-p :initform nil))
  (:command-definer define-peek-command)
  (:panes
    (display :application
	     :incremental-redisplay t
	     :display-function 'display-peek-status
	     :display-time :command-loop
	     :text-cursor nil
	     :end-of-page-action :allow
	     :end-of-line-action :allow))
  (:layouts (default display))
  (:pointer-documentation t))

(defmacro with-peek-frame ((symbol) &body body)
  `(with-application-frame (,symbol) ,@body))

(defmacro with-peek-frame-slots ((&rest symbols) &body body)
  (let ((frame '#:frame))
    `(with-peek-frame (,frame)
       (with-slots ,symbols ,frame
	 ,@body))))

;; We might reuse an existing frame.  Reinitialize it.
(defmethod run-frame-top-level :before ((frame peek-frame) &key)
  (initialize-peek-frame frame))

(defmethod initialize-peek-frame ((frame peek-frame))
  (setf (slot-value frame 'display-pane) (get-frame-pane frame 'display)))

;; Here's the redisplay function that will be incrementally redisplayed.
(defmethod display-peek-status ((frame peek-frame) stream)
  (display-peek-status-1 frame stream (slot-value frame 'mode)))

;; Here it is again, defined as a multi-method so we can easily define
;; new "modes" of display (as in "Processes", "File System", etc.)
(defmethod display-peek-status-1 ((frame peek-frame) stream (mode (eql ':process)))
  (macrolet ((cell (stream item)
	       `(formatting-cell (,stream)
		  (format ,stream "~A" ,item))))
    (with-slots (timeout paused-p show-gc-p show-lisp-processes-p
		 show-OS-processes-p OS-command) frame
      (let ((count 0))
	;; Show the time
	(let ((ut (get-universal-time)))
	  (updating-output (stream :unique-id (incf count)
				   :cache-value ut)
	    (multiple-value-bind (sec min hrs)
		(decode-universal-time ut)
	      (format stream "~2D:~2,'0D:~2,'0D" hrs min sec)))
	  (let ((cache-value (if paused-p "Paused" timeout)))
	    (updating-output (stream :unique-id 'timeout
				     :cache-value cache-value)
	      (format stream " [~A]" cache-value))
	    (terpri stream))))
      (when show-lisp-processes-p
	(formatting-table (stream)
	  ;; Headings
	  (let ((id "Process label"))
	    (updating-output (stream :unique-id id :cache-value id)
	      (with-text-face (stream :italic)
		(formatting-row (stream)
		  (cell stream "Process")
		  (cell stream "State")
		  (cell stream "Activity")))))
	  (let ((list (clim-sys:all-processes)))
	    (setq list (remove-if #'(lambda (x)
				      (or (eq x (clim-sys:current-process))
					  #+lucid (eq x system:*idle-process*)))
				  list))
	    (unless list
	      (formatting-row (stream)
		(cell stream "No processes")))
	    (dolist (p list)
	      (let ((name (clim-sys:process-name p))
		    (state (clim-sys:process-whostate p))
		    (activity (clim-sys:process-state p)))
		(unless (or (eq p (clim-sys:current-process))	 ;Ignore this process
			    #+lucid (eq p system:*idle-process*)); and idle process
		  (formatting-row (stream)
		    (with-output-as-presentation
			(stream p 'process :single-box t)
		      (cell stream name)
		      (cell stream state)
		      (cell stream (string-capitalize (string activity)))))))))))
      (when show-gc-p
	(fresh-line stream)
	(check-gc stream)
	(fresh-line stream))
      (when show-OS-processes-p
	(fresh-line stream)
	(check-OS stream OS-command)))))

(defun check-gc (stream)
  ;; What you might want to say about the GC is very system-dependent.
  #+lucid
  (macrolet ((cell (stream item)
	       `(formatting-cell (,stream)
		  (format ,stream "~A" ,item))))
    (let ((id "EGC label"))
      (updating-output (stream :unique-id id :cache-value id)
	(with-text-face (stream :italic)
	  (format stream "EGC Levels "))))
    (formatting-item-list (stream :n-rows 1)
      (let ((uid 0))
	(dolist (level (lcl:egc-state))
	  (let ((value (round (* (car level) 100)
			      (+ (car level) (cdr level)))))
	    (updating-output (stream :unique-id uid :cache-value value)
	      (formatting-cell (stream)
		(format stream "~2D" value)))))))
    (terpri stream)
    (multiple-value-bind (used before-gc no-gc) (lcl::gc-size)
      (formatting-table
	  (stream)
	(let ((id "GC label"))
	  (updating-output
	      (stream :unique-id id :cache-value id)
	    (with-text-face (stream :italic)
	      (formatting-row (stream)
		(cell stream "Kwords used")
		(cell stream "Available Before GC")
		(cell stream "Available if GC disabled")))))
	(formatting-row (stream)
	  (cell stream (floor used 4000))
	  (cell stream (floor before-gc 4000))
	  (cell stream (floor no-gc 4000))))))
  #-lucid
  (format stream "~%I don't know nothin' about this GC."))

(defmethod read-frame-command ((frame peek-frame) &key (stream *standard-input*))
  (let ((timeout (slot-value frame 'timeout))	;in seconds
	(paused (slot-value frame 'paused-p))
	object)
    (when (setq object
		(with-input-context ('command) (ob)
		     (read-gesture :stream stream
				   :peek-p t
				   :timeout (and (not paused) timeout))
		   (t ob)))
      (read-gesture :stream stream :timeout 0)
      (when (consp object)
	(execute-frame-command frame object))
      nil)))


;;; Process presentation type
(define-presentation-type process ())

(define-presentation-action operate-on-process
    (process command peek-frame
     :documentation "Operate on process"
     :menu nil				;this doesn't go into any menu
     :gesture :select)
    (presentation frame window x y)
  (call-presentation-menu presentation *input-context*
			  frame window x y
			  :for-menu t
			  :gesture :select))

(define-peek-command (com-activate-process)
    ((thing 'process :gesture nil))
  (and (clim-sys:processp thing)
       (clim-sys:enable-process thing)))

(define-peek-command (com-deactivate-process)
    ((thing 'process :gesture nil))
  (and (clim-sys:processp thing)
       #+lucid (not (eq thing system:*idle-process*))
       (clim-sys:disable-process thing)))

(define-peek-command (com-destroy-process)
    ((thing 'process :gesture nil))
  (and (clim-sys:processp thing)
       #+lucid (not (eq thing lcl::*initial-process*))
       (clim-sys:destroy-process thing)))

(define-peek-command (com-restart-process)
    ((thing 'process :gesture nil))
  (and (clim-sys:processp thing)
       #+lucid (not (eq thing lcl::*initial-process*))
       (clim-sys:restart-process thing)))

(define-peek-command (com-options :menu "Options") ()
  (with-peek-frame-slots 
      (display-pane show-gc-p show-lisp-processes-p show-OS-processes-p)
    (let ((gc show-gc-p)
	  (lisp show-lisp-processes-p)
	  (OS show-OS-processes-p)
	  (stream display-pane)
	  (framem (frame-manager *application-frame*)))
      (clim-utils:letf-globally (((frame-manager-dialog-view framem) +gadget-dialog-view+))
	(accepting-values (stream :own-window t :label "Peek Options")
	  (setq gc (accept 'boolean :stream stream
			   :default gc :prompt "Show GC"))
	  (terpri stream)
	  (setq lisp (accept 'boolean :stream stream
			     :default lisp
			     :prompt "Show LISP processes"))
	  (terpri stream)
	  (setq OS (accept 'boolean :stream stream
			   :default OS
			   :prompt "Show OS processes"))
	  (terpri stream))
	;; We won't get here if the user aborted out of the AVV
	(setf show-gc-p gc)
	(setf show-lisp-processes-p lisp)
	(setf show-OS-processes-p OS)))))

(define-peek-command (com-faster :menu "Faster") ()
  (with-peek-frame-slots (timeout timeout-growth-factor)
    (setf timeout (/ timeout timeout-growth-factor))))

(define-peek-command (com-slower :menu "Slower") ()
  (with-peek-frame-slots (timeout timeout-growth-factor)
    (setf timeout (* timeout timeout-growth-factor))))

(define-peek-command (com-pause :menu "Pause") ()
  (with-peek-frame-slots (paused-p)
    (setf paused-p (not paused-p))))

(define-peek-command (com-redisplay :menu "Redisplay") ()
  (redisplay-frame-panes *application-frame* :force-p t))

(define-peek-command (com-exit-peek :menu "Exit") ()
  (frame-exit *application-frame*))

;; Here is a sample UNIX program that shows running processes
;; /bin/ps -aux | awk ' \
;;    {if ($3 != "0.0") print $0}'

(defun check-OS (stream program)
  (fresh-line stream)
  #+(or lucid allegro)
  (let ((pipe ()))
    (unwind-protect
	(progn
	  (setq pipe
		#+lucid
		(lcl::run-program (first program)
				  :arguments (rest program)
				  :output :stream :wait nil)
		#+allegro
		(excl:run-shell-command (first program)
					:output :stream :wait nil))
	  (loop
	    (let ((line (read-line pipe nil nil)))
	      (if (null line)
		  (return)
		  (updating-output (stream :unique-id line
					   :id-test #'equal
					   :cache-value line
					   :cache-test #'string=)
		    (format stream "~A" line)
		    (terpri stream))))))	;darn CLIM can't do "~%"
      (close pipe))))



(clim-demo:define-demo "Peek" peek-frame :width 700 :height 450)
