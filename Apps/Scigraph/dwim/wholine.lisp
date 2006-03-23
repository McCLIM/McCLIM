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

(eval-when (compile load eval)
  (export '(status-pane status-line set-status-line mouse-documentation-pane 
	    *include-machine-name-in-status-line-p*
	    *frame-for-status-line* *time-type*
	    initialize-status-line make-status-line refresh-status-line
	    noting-progress note-progress)
	  'dwim))

;;; The status line is a small pane associated with a frame which provides
;;; status information, such as:
;;;  1. time of day
;;;  2. user name
;;;  3. process state, usually one of "User Input", "Run", "GC", or "Error!".
;;;  4. progress notes
;;;
;;; To use this code, you must:
;;;  a. Provide a method STATUS-PANE that takes your frame as an argument
;;;      and that returns some pane of your frame.
;;;      Such a pane needs a redisplay function.  REFRESH-STATUS-LINE is
;;;      defined below for this purpose.
;;;  b. Provide a method STATUS-LINE which returns an instance of the
;;;      status-line structure.  The best way to do this is to provide
;;;      a slot on your frame whose :initform is (MAKE-STATUS-LINE stream) and
;;;      whose :accessor is STATUS-LINE.  (The stream argument to MAKE-STATUS-LINE
;;;      should be the status pane.)
;;;  c. Initialize the status line (and for clim-0.9, 
;;;     bind *frame-for-status-line*):
;;;
;;;      (defmethod dart-frame-top-level ((frame dart-frame))
;;;         (initialize-status-line)
;;;         (let ((*frame-for-status-line* frame))
;;;           (loop
;;;      	(lcl:with-simple-restart
;;;                   (dart-top-level "Abort to DART Top Level")
;;;       	  (clim:clim-top-level frame)))))
;;;
;;;
;;;
;;; If you modify this code, be careful.  These things should happen with
;;; little or no overhead.  In addition, this code must be error free; an
;;; error while advising the GC or debugger could cause lisp to terminate
;;; itself rather hastily.

;;; default width of a field;
(defconstant *status-width* 150)

;;; leftmost positions of each field, as a percentage of pane width:
(defconstant time-left .05)
(defconstant username-left .25)
(defconstant process-left .4)
(defconstant progress-left .6)

(defparameter *include-machine-name-in-status-line-p* nil)

(defun whoami ()
  #FEATURE-CASE
  ((:unix 
    (let ((host-string (and *include-machine-name-in-status-line-p*
			    (let ((raw-host-string (getenv "HOST")))
			      (cond ((null raw-host-string) nil)
				    ((let ((dot-pos (position #\. raw-host-string)))
				       (if dot-pos
					   (subseq (the string raw-host-string) 0 dot-pos)
					 raw-host-string)))))))
	  (user-string (getenv "USER")))
      (if host-string
	  (concatenate 'string user-string "@" host-string)
	user-string)))
    (:lispm (let ((me si:*user*))
	      (and me (scl:send me :lispm-name))))))


;;; frequently used strings:
(defconstant empty-string " ")
(defparameter run-string    "Please Wait")
(defparameter input-string  "Ready")
(defparameter error-string  "Unexpected Condition")
(defparameter GC-string     "Reclaiming Memory")
(defparameter expand-string "Expanding Memory")

(defclass status-line ()
    ((stream :initform nil :initarg :stream :accessor status-line-stream)
     (time :initform empty-string :accessor status-line-time)
     (ptime :initform nil :accessor status-line-ptime)
     (username :initform (whoami) :accessor status-line-username)
     (pusername :initform nil :accessor status-line-pusername)
     (process :initform run-string :accessor status-line-process)
     (pprocess :initform nil :accessor status-line-pprocess)
     (progress :initform empty-string :accessor status-line-progress)
     (pprogress :initform nil :accessor status-line-pprogress)
     (thermometer :initform 0 :accessor status-line-thermometer)))

(defun make-status-line (stream) (make-instance 'status-line :stream stream))

(defvar *frame-for-status-line* nil
  "Used for progress notes, but only for clim 0.9.")

(defun frame-for-status-line ()
  #FEATURE-CASE
  ((:clim-0.9 *frame-for-status-line*)
   ((or :clim-1.0 :clim-2)
    (and (boundp 'clim:*application-frame*) clim:*application-frame*))
   ((not :clim) (and (boundp 'dw:*program-frame*) dw:*program-frame*))))

(defmethod status-pane ((any t))
  #+clim nil
  #-clim (if (typep any 'dw::program-frame)
	     (status-pane (send any :program))))

(defmethod status-line ((any t))
  #+clim nil
  #-clim (if (typep any 'dw::program-frame)
	     (status-line (send any :program))))

(defmethod mouse-documentation-pane ((any t))
  #+clim nil
  #-clim (if (typep any 'dw::program-frame)
	     (mouse-documentation-pane (send any :program))))

(defvar *status-line-sheet-lock* nil)

(defmacro sheet-lock ((window) &body body)
  "Get a lock on this window, or wait for it."
  (declare (ignore window))
  ;; Yes, yes, I know I should have a different lock for each status
  ;; pane.  But after all, there is almost always only one of them,
  ;; and the time spent updating a status line is very small indeed.
  `(with-process-lock (*status-line-sheet-lock*) ,@body))

(defmethod draw-status-element
	   ((status-line status-line) (element-name t) string x y stream)
  (draw-string string x y :stream stream))

(defmethod process-status-element
	   ((status-line status-line)
	    field-name presentation-name string column
	    &optional (record-p t) (status-width *status-width*))
  (let* ((stream (status-line-stream status-line))
	 presentation)
    (when (and stream status-line)
      (multiple-value-bind (left top right bottom) (stream-viewport stream)
	(setq column (truncate (* column (- right left))))
	(sheet-lock (stream)
	  (setf (slot-value status-line field-name) string)
	  (setq presentation (slot-value status-line presentation-name))
	  (setf (slot-value status-line presentation-name) nil)
	  (if (and record-p presentation)
	      (erase-graphics-presentation presentation :stream stream))
	  (let* ((minx column)
		 (maxx (+ column status-width))
		 (miny top)
		 (fudge-factor 15)
		 (maxy bottom))
	    (with-output-recording-disabled (stream)
	      (draw-rectangle minx maxx maxy miny :stream stream
			      :filled t :alu %erase))
	    (if record-p
		(setf (slot-value status-line presentation-name)
		      (with-output-as-presentation (:stream stream
						    :object string
						    :type 'string)
			(draw-status-element status-line field-name string
					     minx (+ miny fudge-factor) stream)))
		(with-output-recording-disabled (stream)
		  (draw-status-element status-line field-name string
				       minx (+ miny fudge-factor) stream)))
	    (force-output stream)		
	    ))))))

(defmethod set-status-line ((frame t) (field (eql 'status-line-time)) string
			    &optional (record-p t))
  (let* ((status-line (status-line frame)))
    (when status-line 
      (process-status-element status-line 'time 'ptime
			      string time-left record-p))))

(defmethod set-status-line ((frame t) (field (eql 'status-line-username)) string
			    &optional (record-p t))
  (let* ((status-line (status-line frame)))
    (when status-line 
      (process-status-element status-line 'username 'pusername
			      string username-left record-p))))

(defmethod set-status-line ((frame t) (field (eql 'status-line-process)) string
			    &optional (record-p t))
  (let* ((status-line (status-line frame)))
    (when status-line 
      (process-status-element status-line 'process 'pprocess
			      string process-left record-p))))

(defmethod set-status-line ((frame t) (field (eql 'status-line-progress)) string
			    &optional (record-p t))
  (let* ((status-line (status-line frame)))
    (when status-line
      (process-status-element status-line 'progress 'pprogress
			      string progress-left record-p 400))))

(defmacro with-status-line ((string field &optional
				    (frame '(frame-for-status-line))
				    (record-p t))
			    &body body)
  (let ((old (gensym)) (f (gensym)) (status-line (gensym)))
    `(let* ((,f ,frame)
	    (,status-line (and ,f (status-line ,f)))
	    (,old (and ,status-line (funcall ,field ,status-line))))
      (unwind-protect
	   (progn (or (not ,f) (set-status-line ,f ,field ,string ,record-p))
		  ,@body)
	(or (not ,f) (set-status-line ,f ,field ,old ,record-p))))))

(defmacro with-process-state ((string) &body body)
  `(with-status-line (,string 'status-line-process) ,@body))

(defun set-all-status-lines (string field &key how-many (record-p t))
  "Find all frames and notify them with this string"
  ;; HOW-MANY can be used to limit the number of frames notified.
  ;; This is used to limit the amount of consing.
  (let ((count 0))
    (for-each-frame (frame)
       (when (and (status-pane frame)
		  (or (not how-many) (< count how-many)))
	 (set-status-line frame field string record-p)
	 (incf count)))))

(defmethod refresh-status-line (frame pane)
  "Redisplay a pane that is a status line."
  (declare (ignore pane))
  (let* ((line (status-line frame)))
    (when line
      (dolist (field '(status-line-time
		       status-line-username
		       status-line-process
		       status-line-progress))
	(set-status-line frame field (funcall field line))))))

(defun advise (name old-name new-function)
  (unless (fboundp old-name)
    (setf (symbol-function old-name) (symbol-function name))
    (setf (symbol-function name) new-function)
    name))

(defun unadvise (name old-name)
  (when (fboundp old-name)
    (setf (symbol-function name) (symbol-function old-name))
    (fmakunbound old-name)
    name))

(defun string-for-process-whostate (process)
  (let ((whostate
	 #+allegro (mp:process-whostate process)
	 #+lucid (lcl::process-whostate process)
	 #+genera (scl:send-if-handles process :whostate)))
    (if (and whostate (search "Input" whostate :test #'equalp))
	input-string run-string)))

(defun repair-unbelievable-status-lines ()
  ;; Sometimes the status line does not get reset properly,
  ;; particularly after a GC event.  So this
  ;; function is used by the clock process to repair mistakes.
  (for-each-frame (frame)
    (when (status-pane frame)
      (set-status-line
	frame
	'status-line-process
	(let* ((p (frame-top-level-process frame)))
	  (if p (string-for-process-whostate p) "no process"))))))

(defvar *time-type* :normal
  "user-customizable feature")

(defvar *months*
  (vector nil "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
	  "Sep" "Oct" "Nov" "Dec"))

(defun integer-string (integer)
  "Stringify an integer (without using the pretty printer)."
  ;; We seem to be having some wierd lucid bug involving the
  ;; pretty printer, so don't pretty print.
  (let* ((posint (abs integer))
	 (order (if (zerop integer) 1
		    (+ (truncate (log posint 10))
		       (if (minusp integer) 2 1))))
	 (string (make-string order :initial-element #\0))
	 digit)
    (when (minusp integer)
      (setf (elt string 0) #\-))
    (loop
	(if (zerop posint) (return))
      (multiple-value-setq (posint digit) (truncate posint 10))
      (decf order)
      (setf (elt string order) (code-char (+ digit 48))))
    string))

(defun time-string (&optional (type *time-type*))
  ;; This used to be done with ~D format directives, but there
  ;; seems to be some kind of lucid bug that I can't identify.
  (multiple-value-bind (sec min hour day month) (get-decoded-time)
    (declare (ignore sec))
    (ecase type
      ((:normal :12-hour)
       (let ((morning (< hour 12)))
	 (setq hour (cond ((> hour 12) (- hour 12))
			  ((= hour 0) 12)
			  (t hour)))
	 (concatenate 'string
		      (aref *months* month)
		      " "
		      (integer-string day)
		      " "
		      (integer-string hour)
		      ":"
		      (if (<= min 9) "0" "")
		      (integer-string min)
		      (if morning "am" "pm"))))
      ((:military :24-hour)
       (concatenate 'string
		    (integer-string day)
		    " "
		    (aref *months* month)
		    " "
		    (integer-string hour)
		    (if (<= min 9) ":0" ":")
		    (integer-string min))))))

(defvar *clock-quantum* 20
  "Seconds between clock ticks.")

(defun clock-top-level (&optional (quantum *clock-quantum*))
  "What the clock process actually does."
  (loop
    (ignore-errors
     ;; Ignore them because they arise unavoidably at times when
     ;; the number of frames is changing and when somebody bypasses
     ;; my simple window lock.  You might miss one clock cycle but
     ;; the next one will (probably) work fine.
     (set-all-status-lines (time-string) 'status-line-time)
     #+lucid (repair-unbelievable-status-lines)
     (sleep quantum))))

(let ((clock-process nil))
  (defun start-clock ()
    (or clock-process
	(setq clock-process
	      (process-run-function "Clock Process" 'clock-top-level))))
  (defun clock () clock-process)
  (defun stop-clock ()
    (let ((process clock-process))
      (when process
	(kill-process process)
	(setq clock-process nil))))
  )

;;;
;;; The application must initialize this facility explicitly at run time
;;; by calling the function (INITIALIZE-STATUS-LINE).  Do not initialize
;;; at load time, that is too early.
;;;

(defun realize-username (&optional (string (whoami)))
  "Update all status lines with the current username."
  (set-all-status-lines string 'status-line-username))

(defun initialize-status-line ()
  "Do this once at run time to get everything started."
  #+lucid (setq lucid::*gc-silence* #'gc-notify-users)
  (advise-debugger)
  (advise-read-frame)
  (advise-menus)
  (start-clock)
  (realize-username))

(defun halt-status-line ()
  "Undo the side effects of INITIALIZE-STATUS-LINE."
  #+lucid (setq lucid::*gc-silence* nil)
  (unadvise-debugger)
  (unadvise-read-frame)
  (stop-clock))

(defmacro noting-progress ((string) &body body)
  "Place STRING in the right side of the current status pane."
  `(let ((frame (frame-for-status-line)))
     (if (and frame (status-line frame))
	 (with-status-line (,string 'status-line-progress frame)
	   (unwind-protect (progn ,@body)
	     (note-progress 0.0 1.0 frame)))
         #-genera
	 (progn ,@body)
	 #+genera
	 (tv:noting-progress (,string) ,@body))))

(defun note-progress (numerator &optional
		      (denominator 1.0)
		      (frame (frame-for-status-line)))
  "Move the status line progress thermometer."
  (let ((status-line (and frame (status-line frame))))
    (if status-line
	(when (not (eq (status-line-progress status-line) empty-string))
	  (let ((stream (status-line-stream status-line))
		(old-therm (status-line-thermometer status-line))
		(new-therm (max 0.0 (min (float (/ numerator denominator)) 1.0))))
	    (when (and stream (not (eql old-therm new-therm)))
	      (setf (status-line-thermometer status-line) new-therm)
	      (multiple-value-bind (left top right) (stream-viewport stream)
		(let* ((column (truncate (* progress-left (- right left))))
		       (x column)
		       (y (+ top 20))
		       (width (- right left)))
		  (with-output-recording-disabled (stream)
		    (when (< new-therm old-therm)
		      (draw-line x y
				 (+ x (* old-therm (- width x))) y
				 :stream stream :alu %erase))
		    (when (and (plusp new-therm)
			       (> new-therm old-therm))
		      (draw-line (+ x (* old-therm (- width x))) y
				 (+ x (* new-therm (- width x))) y
				 :stream stream :alu %draw)
		      ;; KRA 09JUL93: JM had this f-o commented out.  However,
		      ;; it lets user see actual progress.  If this is too
		      ;; slow we should be smarter about drawing fewer lines.
		      (force-output stream))))))))
	#+genera
	(if (boundp 'tv:*current-progress-note*)
	    (tv:note-progress numerator denominator)))))

;;;
;;; Modify the underlying system.
;;;

#+clim-0.9
(defmethod graft-children ((graft clim-shared::graft))
  ;; Hack to provide optimized access.
  (slot-value graft 'silica::children))

#+clim-0.9
(defmethod clim:read-frame-command :around ((frame t) stream)
	   (with-process-state (input-string) (call-next-method)))

#+clim-0.9
(defmethod ci::accept-values-top-level :around ((frame t) &rest args)
	   (with-process-state (input-string) (call-next-method)))

#+clim-0.9
(defmethod clim:execute-frame-command :around ((frame t) command &optional run)
	   (with-process-state (run-string) (call-next-method)))

(defun advise-menus ()
  "Modify menus so that the process state is 'Ready'"
  #+clim-0.9
  (advise 'ci::menu-choose-from-drawer
	  'old-menu-choose-from-drawer
	  #'(lambda (&rest arguments)
	      (with-process-state (input-string)
		(apply 'old-menu-choose-from-drawer arguments))))
  #+(or clim-1.0 clim-2)
  (advise 'clim::menu-choose-from-drawer
	  'old-menu-choose-from-drawer
	  #'(lambda (&rest arguments)
	      (with-process-state (input-string)
		(apply 'old-menu-choose-from-drawer arguments)))))

#+(or clim-1.0 clim-2)
(defmethod clim:read-frame-command :around ((frame t) &key stream)
  (declare (ignore stream))
  (with-process-state (input-string) (call-next-method)))

#+(or clim-1.0 clim-2)
(defmethod clim:execute-frame-command :around ((frame t) command)
  (declare (ignore command))
  (with-process-state (run-string) (call-next-method)))

#-clim
(scl:defwhopper (dw::program-command-evaluator dw::program) ()
  (let ((e (or (scl:continue-whopper)
	       #'(lambda (program command arguments)
		   (declare (ignore program))
		   (apply command arguments)))))
    #'(lambda (program command arguments)
	(if (status-line program)
	    (with-process-state (run-string)
	      (funcall e program command arguments))
	    (funcall e program command arguments)))))

#-clim
(scl:defwhopper (tv:who-line-screen-mouse-documentation-update-internal
		  tv:generic-who-line-screen-mixin) ()
  (let ((old-doc (send (tv:get-who-line-field :mouse-documentation scl:self)
		       :who-line-item-state)))
    (prog1
      (scl:continue-whopper)
      (let ((doc (send (tv:get-who-line-field :mouse-documentation scl:self)
		       :who-line-item-state)))
	(when (not (eq old-doc doc))
	  (for-each-frame (frame)
	    (let ((pane (mouse-documentation-pane frame)))
	      (when pane
		(window-clear pane)
		(and doc (write-string doc pane))))))))))

(defun advise-read-frame ()
  #+clim-1.0
  (advise 'clim::accept-values-1
	  'old-accept-values-1
	  #'(lambda (&rest arguments)
	      (with-process-state (input-string)
		(apply 'old-accept-values-1 arguments))))
  #+clim-2
  (advise 'clim-internals::invoke-accepting-values
	  'old-invoke-accepting-values
	  #'(lambda (&rest arguments)
	      (with-process-state (input-string)
		(apply 'old-invoke-accepting-values arguments))))
  #-clim
  (advise 'dw::read-program-command
	  'old-read-program-command
	  #'(lambda (&rest arguments)
	      (let ((program (car arguments)))
		(if (or (status-line program)
			(and (typep program 'dw:accept-values)
			     (not (dw::program-frame program))))
		    (with-process-state (input-string)
		      (apply 'old-read-program-command arguments))
		  (apply 'old-read-program-command arguments))))))

(defun unadvise-read-frame ()
  #+clim-1.0
  (unadvise 'clim::accept-values-1 'old-accept-values-1)
  #+clim-2
  (unadvise 'clim-internals::invoke-accepting-values
	    'old-invoke-accepting-values)
  #-clim
  (unadvise 'dw::read-program-command 'old-read-program-command))

#+lucid
(defun gc-notify-users (when)
  "Because *gc-silence* procedures are called when normal memory allocation
   is impossible, an executing function that is bound to *gc-silence* should
   not use more than the amount of storage that is reserved by the value of
   the keyword :reserved-dynamic.  This value defaults to 1024.
   (WE SHOULD SET IT TO 10000.)

   In addition, because *gc-silence* procedures are called when scheduling
   is inhibited, such procedures should not try to acquire process locks...
   for example, writing to a window uses locks.
   (WE ACQUIRE THEM ANYWAY BY BINDING LUCID::*POTENTIAL-DEADLOCK-ACTION*)"

  (let ((LUCID::*POTENTIAL-DEADLOCK-ACTION* :IGNORE))
    (case when
      (:before
	(format *terminal-io* "~%;;;GC~%")
	(set-all-status-lines GC-string 'status-line-process
			      :how-many 1 :record-p nil))
      (:dynamic-expansion
	(format *terminal-io* "~%;;;Dynamic Expansion~%")
	(set-all-status-lines expand-string 'status-line-process
			      :how-many 1 :record-p nil))
      (:reserved-expansion
	(format *terminal-io* "~%;;;Reserved Expansion~%")
	(set-all-status-lines expand-string 'status-line-process
			      :how-many 1 :record-p nil))
      (otherwise
	;; We don't really know what state those processes are in,
	;; so take a guess.  Ideally, we should store those states
	;; away and restore them when GC is done.       
	(set-all-status-lines run-string 'status-line-process
			      :how-many 1 :record-p nil)))))

#+debug
(defun tester ()
  (lucid::with-scheduling-inhibited
      (time
       ;; Conses somewhere between 1100 and 1350 bytes.
       ;; Dont know why the variation exists.
       (gc-notify-users :before))))

#+somewhere-else
(lcl:change-memory-management :reserved-dynamic 10000)

(defvar *panic* nil			; dont panic yet
  "Dynamically bound to prevent recursively entering the debugger.")

(defvar *debugger-name*
	#+lucid 'lcl::invoke-debugger
	#+allegro 'excl::internal-invoke-debugger
	#-(or lucid allegro) nil)

(defun unadvise-debugger ()
  (unadvise *debugger-name* 'old-invoke-debugger))

(defun advise-debugger ()
  "Modify debugger behavior such that if we fall into the debugger,
   all our applications find out via the process status string."

  (unadvise-debugger)

  (when *debugger-name*
    (advise
     *debugger-name*
     'old-invoke-debugger
     #'(lambda (&rest arguments)
	 (if *panic*
	     (apply #'old-invoke-debugger arguments)
	   (let* ((*panic* t)
		  (*standard-input* #+lucid lcl::*initial-io*
				    #-lucid *terminal-io*)
		  (*standard-output* *standard-input*)
		  (*query-io* *standard-input*)
		  (*terminal-io* *standard-input*)
		  (*trace-output* *standard-input*))
	     (unwind-protect
		 (progn
		   (ignore-errors
		    (set-all-status-lines error-string 'status-line-process))
		   (apply #'old-invoke-debugger arguments))
	       (ignore-errors
		(set-all-status-lines run-string 'status-line-process)
		))))))))

